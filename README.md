<p align="center">
<img src="https://github.com/user-attachments/assets/8c160cf3-6675-4ca3-b598-037dcb0d2692" width="450">
</p>

## Brind joy to coding!

<div align="center">
  <p>
    <a href="#installation">Installation</a> •
    <a href="#completed-features">Features</a> •
    <a href="#usage">Usage</a> •
    <a href="#key-commands-and-bindings">Commands</a> •
    <a href="#reviewing-changes">Reviews</a> •
    <a href="#common-workflows">Workflows</a> •
    <a href="#normal-edit-flow">Edit Flow</a> •
    <a href="#notes">Notes</a> •
    <a href="#faq">FAQ</a>
  </p>
</div>

# Demo

https://github.com/user-attachments/assets/e0704424-d155-449f-9e85-51b6d915b648

# Installation
#### Using Quelpa

   ```emacs-lisp
(use-package relysium
  :quelpa (relysium :fetcher github
                    :repo "bluzky/relysium"
                    :branch "main"
                    :files ("*.el"))
  :hook (prog-mode . relysium-prog-mode))
   ```

#### Using Straight

```emacs-lisp
(straight-use-package
 '(relysium :type git
            :host github
            :repo "bluzky/relysium"
            :branch "main"
            :files ("*.el")))

(add-hook 'prog-mode-hook 'relysium-prog-mode)
```

#### Relysium depends on `gptel`, so you have to install and configure `gptel` before using.

# Completed Features

1. **Code generation from comments**
   - Fully implemented via `relysium-generate-from-comments`
   - Uses a clear pattern of "AI:" comment markers

2. **AI-powered code completion at cursor**
   - Implemented through `relysium-complete-cursor.el`
   - Context-aware with buffer understanding, extends code at current cursor position

3. **Code explanation capabilities**
   - Strong implementation via `relysium-ask.el`
   - Allows selecting code regions for specific questions

4. **In-chat code editing**
   - Core functionality via `relysium-edit.el`
   - Supports both region-based and cursor-position editing

5. **Suggestion system**
   - Whole-file analysis and improvement via `relysium-suggest.el`
   - User-provided instructions for tailored suggestions

# Normal edit flow

1. Mark code region or point cursor to line you want to make change
2. `Ctrl-enter` to set direction. `Enter` to submit request to LLM
3. On changes suggestion transient menu
  - `Enter` to accept
  - `x` to discard all suggestions
  - `r` discard all suggestions and query again on same region


# Usage

<div align="center">
  <p>
    <a href="#key-commands-and-bindings">Commands</a> •
    <a href="#reviewing-changes-with-the-transient-menu">Reviewing Changes</a> •
    <a href="#common-workflows">Workflows</a> •
    <a href="#editing-selected-code">Editing</a> •
    <a href="#getting-code-completion">Completion</a> •
    <a href="#generating-code-from-comments">Generation</a> •
    <a href="#getting-code-suggestions">Suggestions</a> •
    <a href="#asking-questions-about-code">Questions</a>
  </p>
</div>

Relysium provides several ways to interact with AI models to improve your coding workflow. Here's how to use each feature:

## Key Commands and Bindings

| Function                         | Key Binding  | Description                                                  |
|----------------------------------|--------------|--------------------------------------------------------------|
| `relysium-edit-dwim`             | `C-<return>` | Smart contextual editing: edits selected region or completes at cursor |
| `relysium-ask`                   | `C-c a`      | Ask questions about selected code                             |
| `relysium-suggest`               | `C-c e s`    | Get improvement suggestions for the entire buffer             |
| `relysium-generate-from-comments`| `C-c e g`    | Generate code based on `AI:` comments                        |
| `relysium-complete-cursor`       | `C-c e p`    | Complete code at the current cursor position                  |
| `relysium-buffer-toggle-window`  | `C-c e t`    | Toggle the Relysium chat window                               |
| `relysium-buffer-clear`          | `C-c e c`    | Clear the Relysium buffer                                     |
| `relysium-buffer-add-context`    | `C-c e b`    | Add region or buffer content to the Relysium buffer           |
| `relysium-buffer-switch-to-chat` | `C-c e w`    | Switch to the chat buffer                                     |
| `relysium-toggle-debug-mode`     | `C-c e D`    | Toggle debug mode, showing debug logs                         |
| `relysium-debug-log`             | `C-c e d`    | View the debug buffer                                         |
| `relysium-transient-menu`        | `C-c e m`    | Show the Relysium action menu                                 |

## Reviewing Changes with the Transient Menu

When AI suggests changes, a transient menu will appear with these options:

| Action       | Key      | Description                                           |
|--------------|----------|-------------------------------------------------------|
| Next         | `n`      | Navigate to the next change                           |
| Prev         | `p`      | Navigate to the previous change                       |
| Accept       | `a`      | Accept the current change and move to the next        |
| Reject       | `d`      | Reject the current change and move to the next        |
| Accept all   | `RET`    | Accept all changes and exit                           |
| Discard all  | `x`      | Discard all changes and exit                          |
| Retry        | `r`      | Discard changes and retry with modified query         |
| Quit         | `q`      | Exit the transient menu                               |

## Common Workflows

### Editing Selected Code
1. Select the code region you want to modify
2. Press `C-<return>` (or `M-x relysium-edit`)
3. Enter your instructions for how the code should be changed
4. Review and accept/reject the suggested changes using the transient menu

### Getting Code Completion
1. Position your cursor where you want code to be inserted
2. Press `C-<return>` (or `M-x relysium-complete-cursor`)
3. Enter a description of what code you want to generate
4. Review and accept/reject the suggested code

### Generating Code From Comments
1. Add comments starting with `AI:` followed by your instructions
   ```python
   # AI: Implement a function to calculate the factorial of a number
   ```
2. Press `C-c e g` (or `M-x relysium-generate-from-comments`)
3. The AI will replace these comments with actual code implementations
4. Review and accept/reject the implementations

### Getting Code Suggestions
1. Press `C-c e s` (or `M-x relysium-suggest`)
2. Enter any specific instructions for the AI to consider
3. The AI will analyze your entire file and suggest improvements
4. Review and accept/reject the changes

### Asking Questions About Code
1. Select the code region you want to ask about
2. Press `C-c a` (or `M-x relysium-ask`)
3. Enter your question about the selected code
4. View the AI's response in the chat window


# Notes

- **Backend:** Relysium uses [gptel](https://github.com/karthink/gptel) as a backend. Ensure you have configured `gptel` appropriately.
- **Rate Limits:** If using Claude, sending the entire buffer might hit rate limits. It's better to send only a region.
- **Models:** Claude 3-5 Sonnet seems to generate better code suggestions for this use case, though you can use other models supported by `gptel`.


# FAQ

**1. Why don't you contribute to original package?**
This is my customization to fit my workflow. And it is a huge code changes. So I don't want to bother @lanceberge, because maybe the original version fit his workflow


** Special thanks to @lanceberge for your original work on this package, and Claude AI which help me to actualize my ideas**
(Because I'm not good at Elisp :D)

**Enjoy seamless code suggestions and management with Relysium!**
