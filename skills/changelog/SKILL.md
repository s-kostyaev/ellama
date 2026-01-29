---
name: changelog
description: Use this skill to generate changelog.
---

# Generating changelog

Call shell_command tool with "git log --reverse main..HEAD" argument. Based on
the output write short changelog in org-mode list format. Use "~tildas~" quoting
instead of "`backticks`" quoting. Do not add any anknowledgements. Every
changelog element should be ended with full stop. Changelog shouldn't be too
short or too long, use detailed description for major changes and concise
description for minor changes.

Call shell_command tools with "git tag -l --points-at=main" argument to see
previously released version. Based on this information and minority/majority of
the changes you can fill version variable. If you are not sure, ask the user
using ask_user tool with your variants of the version.

Write it to ./NEWS.org using prepend_file tool
with header:

* Version {version}

After header should be changelog content. Content should ends with single
newline.
Example:
 ```text
* Version {version}
- Some change description. Some additional information.
- Major change detailed description. This change improves ~ellama-something~.
  Some additional information.
- Third change description.
- Some fix in ~ellama-example-command~ description.
```
