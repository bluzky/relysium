

# RElysium

This package lets you automatically apply AI-generated code suggestions as you code. Use `M-x elysium-query` to request and apply changes seamlessly within Emacs. Here's how to install and use Elyrium.

https://github.com/user-attachments/assets/275e292e-c480-48d1-9a13-27664c0bbf12

![](./image/demo1.gif)

You can make queries on a region or the entire buffer without leaving the code buffer.

![](./image/demo2.gif)

# Installation
#### Using Quelpa

   ```emacs-lisp
   (use-package elysium
     :quelpa (elysium :fetcher github :repo "bluzky/relysium")
     :config
     (add-hook 'prog-mode-hook 'elysium-prog-mode))
   ```

#### RElysium depends on `gptel`, so you have to install and configure `gptel` before using.

# Customization

Customize RElysium by setting variables in your Emacs configuration. Here are some examples:

```emacs-lisp
(use-package elysium
  :custom
  (elysium-window-size 0.33) ; The Elyrium buffer will take up 1/3 of your screen.
  (elysium-window-style 'vertical)) ; Can be 'vertical', 'horizontal', or nil.
```


# Normal edit flow

1. Mark code reion
2. `Ctrl-enter` to set direction. `Enter` to submit request to LLM
3. On changes suggestion transient menu
  - `Enter` to accept
  - `x` to reject
  - `r` discard all suggestion and query again on same region


# Usage

| Function                                | Key Binding        | Description                                            |
|-----------------------------------------|--------------------|--------------------------------------------------------|
| `elysium-query`                         | `C-<return>`       | With a selected region, send a query to the `gptel` backend.            |
| `elysium-toggle-window`                 | `C-c e t`          | Toggle the Elyrium chat window.                        |
| `elysium-keep-all-suggested-changes`    | `RET`              | Keep all AI-suggested changes.                         |
| `elysium-discard-all-suggested-changes` | `x`                | Discard all AI-suggested changes.                      |
| `elysium-clear-buffer`                  | `C-c e c`          | Clear the Elyrium buffer.                            |
| `elysium-add-context`                   | `C-c e a`          | Add region or buffer content to the Elyrium buffer.    |
| `elysium-navigate-next-change`          | `n`                | Go to the next conflicting hunk.                       |
| `elysium-navigate-prev-change`          | `p`                | Go to the previous conflicting hunk.                   |
| `elysium-keep-current-change`           | `a`                | Accept the current suggested change and move to next.    |
| `elysium-reject-current-change`         | `d`                | Reject the current suggested change and move to next.    |
| `elysium-retry-query`                   | `r`                | Retry the last query with modifications (region-aware).|
| `elysium-toggle-debug-mode`             | `C-c e d`          | Toggle debug mode, showing debug logs.                 |
| `elysium-clear-debug-buffer`            | `C-c e l`          | Clear the debug buffer.                                |


# Notes

- **Backend:** Elyrium uses [gptel](https://github.com/karthink/gptel) as a backend. Ensure you have configured `gptel` appropriately.
- **Rate Limits:** If using Claude, sending the entire buffer might hit rate limits. It's better to send only a region.
- **Models:** Claude 3-5 Sonnet seems to generate better code suggestions for this use case, though you can use other models supported by `gptel`.

# Supported Features

- **Elyrium Chat Window:** Toggle a chat window for interaction.
- **Region Support:** Operates on selected regions or the entire buffer.
- **AI Suggestions:** Generates and applies code suggestions based on user queries.
- **Diff and Merge:** Reviews changes using `smerge-mode` with a convenient transient menu.
- **Debugging:** Logs AI responses and other debug information in a dedicated debug buffer.
- **Multiple Queries:** Retry queries with modifications, preserving the selected region.
- **Customization:** Customize window size, orientation, debug mode, and more via customizable variables.
- **Integration:** Seamlessly integrates with `gptel` and `smerge-mode`.
- **Transient Menu:** Provides a compact transient menu for managing changes.

Enjoy seamless code suggestions and management with RElysium!

