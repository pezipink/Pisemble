# Pisemble Opcodes

This VS Code extension colours AArch64 mnemonics inside Pisemble `{ }` blocks. It is a TextMate grammar injected into the Magic Racket grammar (`source.racket`), so it works alongside the [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket) extension and its language server.

Magic Racket's language server can only colour symbols it resolves as functions or variables. Mnemonics inside `{ }` are quoted into instruction data, so the server never tags them. This extension covers that gap at the grammar level.

## Install

Copy this folder into your VS Code extensions directory, replacing any earlier copy, then reload the window:

```powershell
$dest = "$env:USERPROFILE\.vscode\extensions\pisemble.pisemble-opcodes-0.1.0"
Remove-Item -Recurse -Force $dest -ErrorAction SilentlyContinue
Copy-Item -Recurse tools\pisemble-opcodes $dest
```

Run "Developer: Reload Window" from the command palette. To confirm the scopes apply, run "Developer: Inspect Editor Tokens and Scopes" with the cursor on a token:

| Token | Example | Scope | Default colour |
| ----- | ------- | ----- | -------------- |
| Mnemonic | `mov`, `cbnz` | `keyword.other.opcode.pisemble` | `#C586C0` bold |
| Label definition | `:fill` | `entity.name.label.definition.pisemble` | `#4EC9B0` italic |
| Label reference | `fill-`, `done+`, `name:` | `entity.name.label.reference.pisemble` | `#4EC9B0` italic |
| Alignment | `/=` | `keyword.operator.alignment.pisemble` | `#D7BA7D` bold |

Strings and comments are consumed before label matching, so text such as `"exception:"` inside a string is not coloured.

## Changing the colour

The default colours are set in `package.json` under `configurationDefaults`. Settings you add under `editor.tokenColorCustomizations` in your own `settings.json` replace that whole object, so copy the `textMateRules` entries there if you want to keep them alongside your other rules.

## Keeping the opcode list in sync

The regex in `syntaxes/pisemble-opcodes.tmLanguage.json` is a hand-maintained copy of the `opcode` syntax class in `expander.rkt`. When you add or remove an opcode there, update the `opcode` pattern in the grammar to match.
