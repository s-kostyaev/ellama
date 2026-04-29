# Image Input

Ellama can send image files to providers that advertise the `image-input`
capability through the `llm` library. Image data is sent as `llm-media` inside
`llm-multipart` prompt content.

SVG is not supported in the initial implementation.

## Supported files

The default supported extensions are:

- `png`
- `jpg`
- `jpeg`
- `webp`
- `gif`

Customize `ellama-image-file-extensions` to change the extension allow-list.
The current MIME mapping supports the defaults above.

## Interactive chat

Use `ellama-chat-with-image` to ask about one image.

Use `ellama-chat-with-images` to ask about multiple images.

Programmatic calls can pass image files directly:

```elisp
(ellama-chat "Describe this screenshot." nil
             :images '("/path/to/screenshot.png"))
```

`ellama-stream` accepts the same `:images` argument:

```elisp
(ellama-stream "What is shown here?"
               :images '("/path/to/image.png"))
```

The singular `:image` argument is also accepted as a convenience alias.

## Image context

Use `ellama-context-add-image` to add an image to context.

Image context is ephemeral by default. The image is attached to the next request
and then removed from ephemeral context, matching `ellama-context-ephemeral`.

Customize `ellama-image-context-default-scope`:

- `ephemeral`: attach image context to the next request only.
- `persistent`: keep image context until it is removed or context is reset.

The transient context menu also includes `Add Image`.

## Tool calls

The built-in `read_file` tool supports image files through a configurable read
mode. The tool keeps returning a compact text result to the model, but Ellama
queues the image bytes and attaches them to the next provider follow-up request.

Customize `ellama-tools-read-file-default-mode`:

- `auto`: default. Text files are read as text; supported image files are queued
  as image input.
- `text`: force text reading. If a file appears binary, Ellama returns a clear
  text error instead of binary output.
- `image`: force image handling. Unsupported image types, SVG files, missing
  sessions, or providers without `image-input` return a clear text error.

The model can override the default for a single tool call by passing the
optional `mode` argument:

```json
{"file_name": "/path/to/image.png", "mode": "image"}
```

## Persistence

Ellama does not store raw image bytes in saved session files. Multipart image
parts are converted to placeholders before session persistence and context
compaction. This avoids large session files and accidental binary data in
history.

Reloading an old session will not resend previous image bytes. Attach the image
again when visual context is needed.
