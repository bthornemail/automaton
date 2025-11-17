---
id: voice-ide
type: canvasl-template
dimension: 2

adjacency:
  edges:
    - voice-input
    - ide-command
    - action-execution
    - voice-feedback
  orientation: [1, 1, 1, -1]

speech:
  input:
    lang: en-US
    continuous: true
    interimResults: true
    keywords:
      - save
      - build
      - test
      - run
      - debug
      - format
      - search
      - find
      - replace
      - commit
      - push
      - pull
      - terminal
      - close
      - open
      - file
      - editor
      - sidebar
      - panel
      - next
      - previous
      - comment
      - uncomment
      - indent
      - outdent
      - copy
      - paste
      - cut
      - undo
      - redo
      - select
      - goto
      - definition
      - references
      - rename
      - refactor
      - navigate
      - back
      - forward
  output:
    voice: Google US English
    rate: 1.2
    pitch: 1.0

macros:
  # File Operations
  - keyword: save
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+S"
    type: [ide_command, save_file]

  - keyword: open
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+O"
    type: [ide_command, open_file]

  - keyword: close
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+W"
    type: [ide_command, close_file]

  # Build and Run
  - keyword: build
    api: clipboard
    method: writeText
    params:
      text: "npm run build"
    type: [ide_command, build_project]

  - keyword: test
    api: clipboard
    method: writeText
    params:
      text: "npm test"
    type: [ide_command, run_tests]

  - keyword: run
    api: clipboard
    method: writeText
    params:
      text: "npm start"
    type: [ide_command, run_dev]

  # Navigation
  - keyword: goto
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+G"
    type: [ide_command, goto_line]

  - keyword: definition
    api: clipboard
    method: writeText
    params:
      text: "F12"
    type: [ide_command, goto_definition]

  - keyword: references
    api: clipboard
    method: writeText
    params:
      text: "Shift+F12"
    type: [ide_command, find_references]

  - keyword: back
    api: clipboard
    method: writeText
    params:
      text: "Alt+Left"
    type: [ide_command, navigate_back]

  - keyword: forward
    api: clipboard
    method: writeText
    params:
      text: "Alt+Right"
    type: [ide_command, navigate_forward]

  # Search and Replace
  - keyword: search
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+F"
    type: [ide_command, search]

  - keyword: find
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+Shift+F"
    type: [ide_command, find_in_files]

  - keyword: replace
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+H"
    type: [ide_command, replace]

  # Editing
  - keyword: comment
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+/"
    type: [ide_command, toggle_comment]

  - keyword: format
    api: clipboard
    method: writeText
    params:
      text: "Shift+Alt+F"
    type: [ide_command, format_document]

  - keyword: indent
    api: clipboard
    method: writeText
    params:
      text: "Tab"
    type: [ide_command, indent_line]

  - keyword: outdent
    api: clipboard
    method: writeText
    params:
      text: "Shift+Tab"
    type: [ide_command, outdent_line]

  - keyword: rename
    api: clipboard
    method: writeText
    params:
      text: "F2"
    type: [ide_command, rename_symbol]

  # Selection and Clipboard
  - keyword: copy
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+C"
    type: [ide_command, copy]

  - keyword: paste
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+V"
    type: [ide_command, paste]

  - keyword: cut
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+X"
    type: [ide_command, cut]

  - keyword: undo
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+Z"
    type: [ide_command, undo]

  - keyword: redo
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+Y"
    type: [ide_command, redo]

  - keyword: select
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+A"
    type: [ide_command, select_all]

  # Version Control
  - keyword: commit
    api: clipboard
    method: writeText
    params:
      text: "git commit"
    type: [ide_command, git_commit]

  - keyword: push
    api: clipboard
    method: writeText
    params:
      text: "git push"
    type: [ide_command, git_push]

  - keyword: pull
    api: clipboard
    method: writeText
    params:
      text: "git pull"
    type: [ide_command, git_pull]

  # UI
  - keyword: terminal
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+`"
    type: [ide_command, toggle_terminal]

  - keyword: sidebar
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+B"
    type: [ide_command, toggle_sidebar]

  - keyword: panel
    api: clipboard
    method: writeText
    params:
      text: "Ctrl+J"
    type: [ide_command, toggle_panel]

  # Debug
  - keyword: debug
    api: clipboard
    method: writeText
    params:
      text: "F5"
    type: [ide_command, start_debug]

validates:
  homology: true
  byzantine: false
  accessibility: true

features:
  version: 1.0.0
  category: voice-controlled-ide
  description: Voice commands for IDE operations
  ide_compatibility:
    - vscode
    - cursor
    - atom
    - sublime
    - webstorm
---

# Voice-Controlled IDE Commands

Voice commands for common IDE operations. Say keywords to execute IDE actions.

## Available Commands

### File Operations
- **save** - Save current file (Ctrl+S)
- **open** - Open file (Ctrl+O)
- **close** - Close current file (Ctrl+W)

### Build & Run
- **build** - Build project (npm run build)
- **test** - Run tests (npm test)
- **run** - Start development server (npm start)
- **debug** - Start debugging (F5)

### Navigation
- **goto** - Go to line (Ctrl+G)
- **definition** - Go to definition (F12)
- **references** - Find all references (Shift+F12)
- **back** - Navigate back (Alt+Left)
- **forward** - Navigate forward (Alt+Right)

### Search & Replace
- **search** - Find in file (Ctrl+F)
- **find** - Find in all files (Ctrl+Shift+F)
- **replace** - Find and replace (Ctrl+H)

### Editing
- **comment** - Toggle comment (Ctrl+/)
- **format** - Format document (Shift+Alt+F)
- **indent** - Indent line (Tab)
- **outdent** - Outdent line (Shift+Tab)
- **rename** - Rename symbol (F2)

### Clipboard
- **copy** - Copy (Ctrl+C)
- **paste** - Paste (Ctrl+V)
- **cut** - Cut (Ctrl+X)
- **undo** - Undo (Ctrl+Z)
- **redo** - Redo (Ctrl+Y)
- **select** - Select all (Ctrl+A)

### Version Control
- **commit** - Git commit
- **push** - Git push
- **pull** - Git pull

### UI Panels
- **terminal** - Toggle terminal (Ctrl+`)
- **sidebar** - Toggle sidebar (Ctrl+B)
- **panel** - Toggle bottom panel (Ctrl+J)

## Usage

1. Start the voice IDE controller
2. Say any keyword from the list above
3. The corresponding command is copied to clipboard
4. Paste (Ctrl+V) in your terminal or IDE

## Homological Structure

- **C₀**: 38 IDE command keywords (vertices)
- **C₁**: Keyword → IDE action mappings (edges)
- **C₂**: This template document (face)
- **C₃**: Command execution triples (solids)
- **C₄**: Command history and execution contexts (hypervolumes)

## IDE Compatibility

Works with:
- VS Code / Cursor
- Atom
- Sublime Text
- WebStorm / JetBrains IDEs
- Any IDE that supports standard keyboard shortcuts

## Extension Ideas

Add custom keywords for your workflow:
- Project-specific build commands
- Custom git aliases
- Deployment scripts
- Test suites
- Code generation templates
