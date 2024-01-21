# Ellama

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/ellama-badge.svg)](https://melpa.org/#/ellama)
[![MELPA Stable](https://stable.melpa.org/packages/ellama-badge.svg)](https://stable.melpa.org/#/ellama)
[![GNU ELPA](https://elpa.gnu.org/packages/ellama.svg)](https://elpa.gnu.org/packages/ellama.html)

Ellama is a tool for interacting with large language models from
Emacs. It allows you to ask questions and receive responses from the
LLMs. Ellama can perform various tasks such as translation, code
review, summarization, enhancing grammar/spelling or wording and
more through the Emacs interface. Ellama natively supports streaming
output, making it effortless to use with your preferred text editor.

## Installation

Just `M-x` `package-install`<kbd>Enter</kbd> `ellama` <kbd>Enter</kbd>.
By default it uses [ollama](https://github.com/jmorganca/ollama)
provider and [zephyr](https://ollama.ai/library/zephyr) model. If you
ok with it, you need to install
[ollama](https://github.com/jmorganca/ollama) and pull
[zephyr](https://ollama.ai/library/zephyr). You can use `ellama` with
other model or other llm provider. In that case you should customize
ellama configuration like this:

``` emacs-lisp
(use-package ellama
  :init
  (setopt ellama-language "German")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "mistral:7b-instruct-v0.2-q6_K" :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
		  '(("zephyr" . (make-llm-ollama
						 :chat-model "zephyr:7b-beta-q6_K"
						 :embedding-model "zephyr:7b-beta-q6_K"))
			("mistral" . (make-llm-ollama
						  :chat-model "mistral:7b-instruct-v0.2-q6_K"
						  :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
			("mixtral" . (make-llm-ollama
						  :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
						  :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k")))))
```

## Commands

### ellama-chat

Ask Ellama about something by entering a prompt in an interactive
buffer and continue conversation.
![ellama-chat](imgs/ellama-ask.gif)

### ellama-ask-about

Ask Ellama about a selected region or the current buffer.
![ellama-ask-about](imgs/ellama-ask-about.gif)

### ellama-ask-selection

Send selected region or current buffer to ellama chat.

### ellama-ask-line

Send current line to ellama chat.

### ellama-complete

Complete text in current buffer with ellama.

### ellama-translate

Ask Ellama to translate a selected region or word at the point.
![ellama-translate](imgs/ellama-translate.gif)

### ellama-define-word

Find the definition of the current word using Ellama.
![ellama-define-word](imgs/ellama-define-word.gif)

### ellama-summarize

Summarize a selected region or the current buffer using Ellama.
![ellama-summarize](imgs/ellama-summarize.gif)

### ellama-make-flash-cards
Create flashcards in org-mode headline format, which could later be used with packages such as [org-anki](https://github.com/eyeinsky/org-anki).

### ellama-code-review

Review code in a selected region or the current buffer using Ellama.
![ellama-code-review](imgs/ellama-code-review.gif)

### ellama-change

Change text in a selected region or the current buffer according to a
provided change.

### ellama-make-list

Create a markdown list from the active region or the current buffer using Ellama.

### ellama-make-table

Create a markdown table from the active region or the current buffer using Ellama.

### ellama-summarize-webpage

Summarize a webpage fetched from a URL using Ellama.

### ellama-provider-select

Select ellama provider.

### ellama-code-complete

Complete selected code or code in the current buffer according to a
provided change using Ellama.

### ellama-code-add

Add new code according to a description, generating it with a provided
context from the selected region or the current buffer using Ellama.

### ellama-code-edit

Change selected code or code in the current buffer according to a
provided change using Ellama.

### ellama-code-improve

Change selected code or code in the current buffer according to a
provided change using Ellama.

### ellama-improve-wording

Enhance the wording in the currently selected region or buffer using Ellama.

### ellama-improve-grammar
Enhance the grammar and spelling in the currently selected region or
buffer using Ellama.
![ellama-improve-grammar](imgs/ellama-enhance-grammar-spelling.gif)

### ellama-improve-conciseness

Make the text of the currently selected region or buffer concise and
simple using Ellama.

### ellama-make-format

Render the currently selected text or the text in the current buffer
as a specified format using Ellama.

### ellama-load-session

Load ellama session from file.

### ellama-session-remove

Remove ellama session.

### ellama-session-switch

Change current active session.

### ellama-session-rename

Rename current ellama session.

## Keymap

Here is a table of keybindings and their associated functions in
Ellama, using the `C-c e` prefix:

| Keymap | Function                   | Description                        |
|--------|----------------------------|------------------------------------|
| "c c"  | ellama-code-complete       | Code complete                      |
| "c a"  | ellama-code-add            | Code add                           |
| "c e"  | ellama-code-edit           | Code edit                          |
| "c i"  | ellama-code-improve        | Code improve                       |
| "c r"  | ellama-code-review         | Code review                        |
| "s s"  | ellama-summarize           | Summarize                          |
| "s w"  | ellama-summarize-webpage   | Summarize webpage                  |
| "i w"  | ellama-improve-wording     | Improve wording                    |
| "i g"  | ellama-improve-grammar     | Improve grammar and spelling       |
| "i c"  | ellama-improve-conciseness | Improve conciseness                |
| "m l"  | ellama-make-list           | Make list                          |
| "m t"  | ellama-make-table          | Make table                         |
| "m f"  | ellama-make-format         | Make format                        |
| "a a"  | ellama-ask-about           | Ask about                          |
| "a i"  | ellama-chat                | Chat (ask interactively)           |
| "a l"  | ellama-ask-line            | Ask about current line             |
| "a s"  | ellama-ask-selection       | Ask about selection                |
| "t t"  | ellama-translate           | Text translate                     |
| "t c"  | ellama-complete            | Text complete                      |
| "d w"  | ellama-define-word         | Define word                        |
| "p s"  | ellama-provider-select     | Provider select                    |

## Configuration

The following variables can be customized for the Ellama client:

- `ellama-enable-keymap`: Enable the Ellama keymap.
- `ellama-keymap-prefix`: The keymap prefix for Ellama.
- `ellama-user-nick`: The user nick in logs.
- `ellama-assistant-nick`: The assistant nick in logs.
- `ellama-language`: The language for Ollama translation. Default
language is english.
- `ellama-provider`: llm provider for ellama. Default provider is
`ollama` with [zephyr](https://ollama.ai/library/zephyr) model.
There are many supported providers: `ollama`, `open ai`, `vertex`,
`GPT4All`. For more information see [llm
documentation](https://elpa.gnu.org/packages/llm.html)
- `ellama-providers`: association list of model llm providers with
  name as key.
- `ellama-spinner-type`: Spinner type for ellama. Default type is
`progress-bar`.
- `ellama-ollama-binary`: Path to ollama binary.
- `ellama-auto-scroll`: If enabled ellama buffer will scroll
  automatically during generation. Disabled by default.
- `ellama-fill-paragraphs`: Option to customize ellama paragraphs
  filling behaviour.
- `ellama-name-prompt-words-count`: Count of words in prompt to
  generate name.
- Prompt templates for every command.
- `ellama-chat-done-callback`: Callback that will be called on ellama
chat response generation done. It should be a function with single
argument generated text string.
- `ellama-nick-prefix`: User and assistant nick prefix in logs.
- `ellama-session-file-extension`: File extension for saving ellama
  session. Default value "org".
- `ellama-sessions-directory`: Directory for saved ellama sessions.
- `ellama-instant-mode`: Major mode for ellama instant commands. Org
  mode by default.
- `ellama-long-lines-length`: Long lines length for fill paragraph call.
Too low value can break generated code by splitting long comment
lines. Default value 100.

## Acknowledgments

Thanks [Jeffrey Morgan](https://github.com/jmorganca) for excellent
project [ollama](https://github.com/jmorganca/ollama). This project
cannot exist without it.

Thanks [zweifisch](https://github.com/zweifisch) - I got some ideas
from [ollama.el](https://github.com/zweifisch/ollama) what ollama
client in Emacs can do.

Thanks [Dr. David A. Kunz](https://github.com/David-Kunz) - I got more
ideas from [gen.nvim](https://github.com/David-Kunz/gen.nvim).

Thanks [Andrew Hyatt](https://github.com/ahyatt) for `llm` library.
Without it only `ollama` would be supported.

# Contributions

If you are interested in creating a provider, please send a pull
request, or open a bug. This library is part of GNU ELPA, so any major
provider that we include in this module needs to be written by someone
with FSF papers. However, you can always write a module and put it on
a different package archive, such as MELPA.
