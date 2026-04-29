# Image Input Support Implementation Plan

## Goal

Add first-class image input support to Ellama when the selected `llm`
provider advertises `image-input`.

Support must work in two paths:

- Interactive chat sessions, where a user attaches or references image files
  and the next chat request sends those images as `llm-media`.
- Tool use, where the model can call `read_file` on an image and Ellama sends
  that image to the provider on the follow-up tool-result request instead of
  returning only textual file bytes.

## Current State

- The installed `llm` library supports image input through:
  - `llm-media` with `:mime-type` and unibyte `:data`.
  - `llm-multipart` containing strings and `llm-media`.
  - `llm-make-chat-prompt` and `llm-chat-prompt-append-response`, whose
    content can be a string or `llm-multipart`.
  - `llm-capabilities`, where `image-input` indicates provider support.
- `ellama-stream` currently builds prompts from plain strings:
  - New sessions use `llm-make-chat-prompt`.
  - Existing sessions append user input with
    `llm-chat-prompt-append-response`.
- `ellama-chat` displays only text in the chat buffer.
- `ellama-context` extracts file and buffer context as text.
- `ellama-tools-read-file-tool` currently reads every file as text and returns
  JSON-encoded text.
- Tool results in `llm-provider-utils` are normalized into provider tool
  result content. The generic OpenAI path formats tool result content as text,
  so returning an `llm-media` directly from a tool is not enough.
- Session persistence writes `ellama-session` with `prin1-to-string`. Storing
  raw image bytes inside prompt history would make session files large and
  brittle.

## Design

### 1. Add media helpers in `ellama.el`

Introduce helpers that are independent from UI and tools:

- `ellama-image-file-extensions`
  - Default: `("png" "jpg" "jpeg" "webp" "gif")`.
  - SVG is out of scope for the initial implementation.
- `ellama--image-file-p`
  - Detect supported image files by extension and existence.
- `ellama--image-mime-type`
  - Map file extension to MIME type.
- `ellama--file-to-llm-media`
  - Use `insert-file-contents-literally` in a unibyte temp buffer.
  - Return `(make-llm-media :mime-type ... :data ...)`.
- `ellama--provider-supports-image-input-p`
  - Check `(memq 'image-input (llm-capabilities provider))`.
  - Wrap in `condition-case` and return nil on capability probe errors.
- `ellama--make-multipart`
  - Given text and image descriptors, return either the original text or
    `(llm-make-multipart text media...)`.

Rationale: centralizing this avoids duplicating binary reading and capability
logic in chat, context, and tools.

### 2. Add request-level image arguments

Extend public request functions without breaking existing callers:

- Add `:images FILES` to `ellama-stream`.
- Add `:images FILES` to `ellama-chat`.
- Optionally add `:image FILE` as a convenience alias normalized to a list.

Implementation points:

- In `ellama-chat`, pass image arguments through to `ellama-stream`.
- In `ellama-stream`, build `prompt-content` before prompt construction:
  - Start with `prompt-with-ctx`.
  - If images are present and provider supports `image-input`, wrap with
    `llm-multipart`.
  - If images are present and provider does not support `image-input`, signal a
    user-facing error before mutating the session prompt.
- When an existing session is used, append `prompt-content` rather than
  `prompt-with-ctx`.
- When a new session prompt is created, pass `prompt-content` to
  `llm-make-chat-prompt`.

### 3. Add interactive commands

Add user-visible entry points:

- `ellama-chat-with-image`
  - Prompt for one image file and a text prompt.
  - Call `ellama-chat` with `:images`.
- `ellama-chat-with-images`
  - Prompt for multiple image files. Use `read-file-name` repeatedly or
    `completing-read-multiple` over image files in the current directory.
- Optional context command:
  - `ellama-context-add-image` adds an image context element that is consumed by
    the next request.

Defaults and configuration:

- Image context added interactively should be ephemeral by default, so images
  are sent with the next request and then removed from context.
- Add `ellama-image-context-default-scope` with default `ephemeral`.
  Supported values:
  - `ephemeral`: attach image context only to the next request.
  - `persistent`: keep image context until the user removes or resets it.

Keymap/transient integration:

- Add a key under the existing ask/context area after the implementation is
  stable.
- If transient menus list context commands, add image context there as well.

Chat buffer display:

- Keep the transcript textual. Insert links such as
  `[[file:/path/to/image.png][image.png]]` in org buffers and markdown links in
  markdown buffers.
- Do not inline binary data into chat buffers.

### 4. Add image context elements

Extend `ellama-context.el`:

- Add `ellama-context-element-image-file` with slots:
  - `name` absolute file name.
  - optional `description` or display label.
- `ellama-context-element-display`
  - Return the file basename.
- `ellama-context-element-format`
  - Return an org or markdown link for visible transcript/context display.
- Add a new generic or helper to extract media:
  - `ellama-context-element-media`
  - Default method returns nil.
  - Image file method returns a list of image file names or `llm-media`.

Update prompt building:

- Keep `ellama-context-prompt-with-context` text-only for existing behavior.
- Add `ellama-context-media` or `ellama-context-request-media` to collect image
  context separately.
- In `ellama-stream`, combine explicit `:images` with image context media.
- Consume ephemeral image context after it has been attached to a request.

Rationale: text context should remain text; media should travel through
`llm-multipart` so providers receive real image bytes.

### 5. Extend `read_file` for image tool calls

Add image-aware behavior to `ellama-tools-read-file-tool`:

- Keep existing behavior for text files.
- For image files:
  - Validate read access with `ellama-tools--tool-check-file-access`.
  - Validate provider/session support for `image-input`.
  - Register the image as pending media for the current Ellama session.
  - Return a compact textual JSON result, for example:
    `"Image file queued for model input: /abs/path/image.png (image/png, N bytes)."`

Read modes:

- Add optional tool argument `mode`.
- Add `ellama-tools-read-file-default-mode`, default `auto`.
- Supported values:
  - `auto`: detect image files by extension/MIME mapping. Text files continue
    through the current text path; image files are queued as model media.
  - `text`: force text reading. This is useful for unusual files, generated
    text with image-like extensions, or when the model specifically needs raw
    file contents. If the file is binary or cannot be decoded safely, return a
    clear textual error instead of binary output.
  - `image`: force image handling. If the file is not a supported image type,
    or the provider lacks `image-input`, return a clear textual error.

The tool argument should override `ellama-tools-read-file-default-mode` for a
single call. The defcustom lets users choose a conservative default globally.

Pending media bridge:

- Add session extra key, for example `:pending-tool-media`.
- Add helper `ellama--session-add-pending-tool-media`.
- A pending item should store file path, MIME type, byte size, and the eventual
  `llm-media` or enough data to rebuild it.
- On the next tool-result follow-up request, merge pending media into the last
  user/tool-result interaction by appending a new user interaction or by
  wrapping the next request content as multipart.

Preferred implementation:

- Let `read_file` queue file metadata and return text.
- In the retry loop inside `ellama--response-handler`, before calling
  `llm-chat-async` or `llm-chat-streaming` again after tool execution, call a
  helper such as `ellama--prompt-attach-pending-tool-media`.
- That helper appends a user interaction like:
  - text: `"Images read by tools are attached for visual analysis."`
  - media parts: the queued image media.
- Clear `:pending-tool-media` only after it has been attached.

Rationale: current generic tool result serialization is text-only. Adding a
separate multipart user interaction keeps provider compatibility and avoids
patching `llm-provider-utils`.

### 6. Capability and error behavior

Rules:

- If no images are present, behavior must remain unchanged.
- If images are present but provider lacks `image-input`, fail before sending
  the request.
- For tool calls, `read_file` should return a textual error if the current
  provider/session cannot accept images, so the model can choose another action.
- If file type is unknown or unsupported, use existing text reading behavior
  only when the file is safely decodable as text. Otherwise return a clear
  unsupported/binary file message.
- SVG is unsupported in the initial implementation. If `mode` is `image` and
  the file is SVG, return an explicit unsupported image type error.
- Keep DLP output scanning on textual tool messages. Do not scan binary image
  bytes with text DLP rules unless a later DLP image policy is added.

### 7. Session Persistence

Avoid persisting raw image bytes in session files by default.

Plan:

- Store image references in session `extra`, not raw media, when possible.
- Before saving, sanitize prompt interactions by replacing `llm-media` parts
  with lightweight placeholders:
  - path if available,
  - MIME type,
  - byte length.
- On session load, do not automatically reload old image bytes into history.
  Keep visible transcript links and continue text-only unless the user attaches
  the image again.

Follow-up option:

- Add `ellama-session-persist-media` defcustom later if full multimodal session
  replay is required.

### 8. Auto-Compaction

Current compaction already detects `llm-multipart` and formats non-string parts
with `%S`. Improve this before shipping:

- Render media as placeholders such as
  `[image: image/png, 12345 bytes, /path/to/file.png]`.
- Do not pass image bytes to the summarization provider.
- Ensure token counting does not try to count raw binary data.

### 9. Tests

Add focused ERT tests:

- `ellama--image-file-p` recognizes supported extensions.
- `ellama--file-to-llm-media` returns unibyte data and correct MIME type.
- `ellama-stream` builds `llm-multipart` for new sessions with `:images`.
- `ellama-stream` appends `llm-multipart` to existing sessions with `:images`.
- `ellama-stream` errors before prompt mutation when provider lacks
  `image-input`.
- `ellama-chat` passes `:images` through to `ellama-stream`.
- `ellama-tools-read-file-tool` keeps text behavior unchanged.
- `ellama-tools-read-file-tool` queues image media and returns compact text for
  image files.
- Pending tool media is attached before the follow-up LLM request and cleared
  after attachment.
- Session save sanitizes media placeholders and does not write raw image data.
- Compaction renders media placeholders instead of raw media structs.

Use tiny generated binary fixtures in temp files inside tests. Do not add large
image assets to the repo.

### 10. Implementation Order

1. Add image detection, MIME, media construction, and capability helpers.
2. Add `:images` support in `ellama-stream` and pass-through in `ellama-chat`.
3. Add tests for direct interactive/request image support.
4. Add chat display links and interactive image commands.
5. Add ephemeral-by-default image context element and context media collection.
6. Extend `read_file` with configurable `auto`/`text`/`image` modes and queue
   pending media.
7. Attach pending tool media in the tool-call retry path.
8. Add tests for image `read_file` tool calls.
9. Add session save/load media sanitization.
10. Adjust compaction rendering for media placeholders.
11. Run checks:
    - `make build`
    - `make test`
    - `make check-compile-warnings`
    - `make checkdocs`

## Open Questions

- Should session files support opt-in media persistence for users who want full
  multimodal replay?
