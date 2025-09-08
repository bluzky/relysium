# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Relysium is an Emacs package that provides AI-powered code assistance through a tight integration with gptel. It enables seamless code editing, completion, generation, and suggestion workflows directly within Emacs buffers using git merge-style conflict resolution.

## Architecture

### Core Components

- **relysium-core.el**: Central request-response flow that standardizes LLM interactions across all commands
- **relysium.el**: Main entry point with keybindings and the primary `relysium-edit-dwim` command
- **relysium-buffer.el**: Chat buffer management for AI conversations
- **relysium-patch.el**: Applies code changes using git merge-style conflict markers
- **relysium-extraction.el**: Extracts code blocks and suggestions from LLM responses
- **relysium-context.el**: Gathers file and region context for LLM requests

### Command Modules

Each command module follows a consistent pattern:
- **relysium-edit.el**: Edits selected code regions based on user instructions
- **relysium-complete.el**: Completes code at cursor position
- **relysium-generate.el**: Generates code from "AI:" comments
- **relysium-ask.el**: Asks questions about code (chat-only, no edits)
- **relysium-suggest.el**: Analyzes entire buffer and suggests improvements

### Utility Modules

- **simple-template.el**: Template engine supporting variables, conditionals, loops
- **simple-diff.el**: Diff algorithm for generating git merge-style conflicts
- **relysium-prompt-template.el**: Prompt templates and formatting configuration

## Development Commands

### Testing
- Run tests: `M-x ert RET ^simple-diff-test- RET` (for simple-diff tests)
- Run tests: `M-x ert RET ^simple-template-test- RET` (for simple-template tests)
- Interactive testing: `M-x simple-diff-run-tests` or `M-x simple-template-run-tests`

### Dependencies
- **gptel**: Required backend for LLM communication
- **smerge-mode**: Used for reviewing and accepting/rejecting changes
- **transient**: Provides the interactive change review menu

## Key Workflows

### Code Change Process
1. User invokes command (edit, complete, suggest, etc.)
2. `relysium-context-gather` collects relevant context (buffer, region, cursor position)
3. Command module builds appropriate prompt using template system
4. `relysium-core-request` sends request to LLM via gptel
5. Response is processed by command-specific handler
6. Changes are applied using `relysium-patch-apply` with conflict markers
7. User reviews changes with smerge-mode and transient menu

### Template System
The template engine in `simple-template.el` supports:
- Variable substitution: `${variable}`
- Expression evaluation: `${=expression}`
- Conditionals: `{{if condition}}...{{else}}...{{endif}}`
- Loops: `{{for item in collection}}...{{endfor}}`
- Comments: `{{! comment }}`

### Response Processing
- **Code blocks**: Extracted and applied as replacements
- **Suggestions**: XML format with start_row/end_row attributes for precise placement
- **Chat responses**: Displayed in chat buffer without code changes

## File Organization

- Core functionality in `relysium-*.el` files
- Utility modules: `simple-*.el`
- Tests in `tests/` directory
- Main entry point: `relysium.el`

## Key Variables and Settings

- `relysium-prompt-template-multi-suggestion-format`: Defines expected LLM response format for suggestions
- `relysium--last-query`: Stores last user query for retry functionality
- Buffer-local variables track region context across async operations

## Integration Points

- **gptel integration**: All LLM communication flows through gptel's request system
- **smerge-mode**: Used for conflict resolution after applying changes
- **transient**: Provides interactive menus for reviewing changes
- **ert**: Test framework for utility modules