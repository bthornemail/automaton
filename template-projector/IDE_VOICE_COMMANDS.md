# IDE Voice Commands - Quick Reference

Control your development environment with voice commands powered by CANVASL.

## 🚀 Quick Start

```bash
npm run dev:ide
```

Open http://localhost:5173/voice-ide-demo.html and click **"Start Listening"**

## 📋 Command Reference

### File Operations
| Keyword | Action | Shortcut |
|---------|--------|----------|
| **save** | Save current file | Ctrl+S |
| **open** | Open file dialog | Ctrl+O |
| **close** | Close current file | Ctrl+W |

### Build & Run
| Keyword | Action | Command |
|---------|--------|---------|
| **build** | Build project | `npm run build` |
| **test** | Run tests | `npm test` |
| **run** | Start dev server | `npm start` |
| **debug** | Start debugging | F5 |

### Navigation
| Keyword | Action | Shortcut |
|---------|--------|----------|
| **goto** | Go to line | Ctrl+G |
| **definition** | Go to definition | F12 |
| **references** | Find all references | Shift+F12 |
| **back** | Navigate back | Alt+Left |
| **forward** | Navigate forward | Alt+Right |

### Search & Replace
| Keyword | Action | Shortcut |
|---------|--------|----------|
| **search** | Find in file | Ctrl+F |
| **find** | Find in all files | Ctrl+Shift+F |
| **replace** | Find and replace | Ctrl+H |

### Editing
| Keyword | Action | Shortcut |
|---------|--------|----------|
| **comment** | Toggle comment | Ctrl+/ |
| **format** | Format document | Shift+Alt+F |
| **indent** | Indent line | Tab |
| **outdent** | Outdent line | Shift+Tab |
| **rename** | Rename symbol | F2 |

### Clipboard Operations
| Keyword | Action | Shortcut |
|---------|--------|----------|
| **copy** | Copy | Ctrl+C |
| **paste** | Paste | Ctrl+V |
| **cut** | Cut | Ctrl+X |
| **undo** | Undo | Ctrl+Z |
| **redo** | Redo | Ctrl+Y |
| **select** | Select all | Ctrl+A |

### Version Control (Git)
| Keyword | Action | Command |
|---------|--------|---------|
| **commit** | Git commit | `git commit` |
| **push** | Git push | `git push` |
| **pull** | Git pull | `git pull` |

### UI Panels
| Keyword | Action | Shortcut |
|---------|--------|----------|
| **terminal** | Toggle terminal | Ctrl+` |
| **sidebar** | Toggle sidebar | Ctrl+B |
| **panel** | Toggle bottom panel | Ctrl+J |

## 🎯 How It Works

1. **Say a keyword** - e.g., "save", "build", "test"
2. **Command copied to clipboard** - Automatically copied for you
3. **Paste in terminal/IDE** - Press Ctrl+V to execute

## 💡 Usage Tips

### For Commands
When you say a build/test/git command, the full command is copied to your clipboard:
- Say "build" → `npm run build` copied
- Say "test" → `npm test` copied
- Say "commit" → `git commit` copied

Paste in your terminal with Ctrl+V or Cmd+V

### For Shortcuts
When you say an IDE shortcut, the keyboard combination is copied:
- Say "save" → `Ctrl+S` copied
- Say "format" → `Shift+Alt+F` copied

You can reference the shortcut or use it in automation scripts.

### Voice Recognition Tips
- **Speak clearly** at normal pace
- **Use exact keywords** from the table above
- **Watch the status indicator** - green dot means listening
- **Check execution history** to confirm detection

## 🔧 Customization

### Add Your Own Commands

Edit `templates/voice-ide.md`:

```yaml
macros:
  - keyword: deploy
    api: terminal
    method: executeCommand
    params:
      command: "npm run deploy"
    type: [terminal, deploy]
```

### Project-Specific Shortcuts

Create a template for your project:

```yaml
macros:
  - keyword: migrate
    api: terminal
    method: executeCommand
    params:
      command: "npm run db:migrate"
    type: [terminal, migrate]

  - keyword: seed
    api: terminal
    method: executeCommand
    params:
      command: "npm run db:seed"
    type: [terminal, seed]
```

## 🎨 IDE Compatibility

Works with:
- ✅ **VS Code** / Cursor
- ✅ **JetBrains** IDEs (WebStorm, IntelliJ, etc.)
- ✅ **Sublime Text**
- ✅ **Atom**
- ✅ **Vim** / Neovim (with mappings)
- ✅ Any IDE supporting standard shortcuts

## 📊 Statistics

The UI shows real-time metrics:
- **Keywords**: Number of available voice commands (C₀ cells)
- **Executions**: Total commands executed (C₄ cells)
- **Euler χ**: Topological invariant of the chain complex

## 🔬 Technical Details

### Chain Complex Structure
```
C₀ (vertices)     = IDE command keywords (38 commands)
C₁ (edges)        = Keyword → action mappings
C₂ (faces)        = Template document
C₃ (solids)       = Command execution triples
C₄ (hypervolumes) = Execution history
```

### Homological Validation
- Boundary operators: ∂ ∘ ∂ = 0
- Betti numbers: β₀ = keywords, β₄ = executions
- Euler characteristic: χ = Σ(-1)ⁿβₙ

## 🐛 Troubleshooting

### Voice Not Detected
- Grant microphone permission
- Use Chrome/Edge (best support)
- Speak clearly and distinctly

### Commands Not Working
- Check clipboard permission
- Verify template is loaded
- Look at execution history for errors

### Shortcuts Wrong for My IDE
- Edit `templates/voice-ide.md`
- Customize keyboard shortcuts
- Add IDE-specific mappings

## 🎓 Advanced Usage

### Combine with Shell Aliases

```bash
# ~/.bashrc or ~/.zshrc
alias vb='npm run build'
alias vt='npm test'
alias vc='git commit'
```

Say "build" → clipboard has `npm run build` → paste in terminal

### Automation Scripts

```javascript
import { IDECommandMacro } from './src/speech/ide-macros.js';

const deployMacro = new IDECommandMacro('deploy', 'deploy', {
  text: './scripts/deploy.sh'
});

await deployMacro.execute(); // Copies to clipboard
```

### Custom Voice Workflows

Create multi-step workflows:

```yaml
macros:
  - keyword: shipit
    api: terminal
    method: executeCommand
    params:
      command: "npm test && npm run build && git push"
    type: [terminal, workflow]
```

## 📚 Related Documentation

- **API Reference**: `src/speech/README.md`
- **IDE Macros**: `src/speech/ide-macros.ts`
- **Template Syntax**: `templates/voice-ide.md`
- **General Voice**: `QUICKSTART_VOICE.md`

## 🚀 Next Steps

1. **Try the demo**: `npm run dev:ide`
2. **Customize commands**: Edit `templates/voice-ide.md`
3. **Add project commands**: Create project-specific templates
4. **Share templates**: Export and version control your voice commands

---

**Voice control your entire development workflow!** 🎤💻
