# CANVASL Voice Control - Quick Start Guide

Get started with voice-controlled web applications in under 5 minutes!

## Installation

No installation needed! The template-projector already includes all dependencies.

## Running the Demo

### Option 1: Interactive Demo (Recommended)

```bash
npm run dev:voice
```

This opens the full-featured voice control demo with UI at http://localhost:5173/voice-demo.html

### Option 2: Integration Test

```bash
npm run test:voice
```

This opens a test page that validates all components at http://localhost:4173/test/voice-integration-test.html

## First Steps

1. **Grant Permissions**
   - Allow microphone access when prompted
   - Allow notifications when prompted

2. **Start Listening**
   - Click the "Start Listening" button
   - You'll hear: "Voice application started. Say a keyword to begin."

3. **Say a Keyword**
   - Try: "location" → Gets your GPS coordinates
   - Try: "notify" → Sends a browser notification
   - Try: "save" → Saves to IndexedDB
   - Try: "copy" → Copies to clipboard
   - Try: "battery" → Checks battery level

4. **Watch the Results**
   - The app speaks the results back to you
   - Execution history updates in real-time
   - Chain complex metrics show topological state

## Available Commands

| Keyword | Action | API Used |
|---------|--------|----------|
| `location` | Get GPS coordinates | Geolocation API |
| `notify` | Send notification | Notification API |
| `save` | Store data | IndexedDB |
| `copy` | Copy to clipboard | Clipboard API |
| `battery` | Check battery | Battery Status API |

## Understanding the Output

### Execution History
Shows recent voice commands with timestamps:
```
10:23:45 - location
10:23:52 - notify
10:24:01 - save
```

### Chain Complex State
Shows topological metrics:
```
Betti numbers: [5, 4, 1, 5, 12]
Euler characteristic: -5
Executions: 12
```

**What do these mean?**
- **β₀ = 5**: Number of connected components (keywords)
- **β₁ = 4**: Number of independent cycles (API connections)
- **β₂ = 1**: Number of 2D voids (template document)
- **β₃ = 5**: Number of 3D cavities (interface triples)
- **β₄ = 12**: Number of 4D voids (execution contexts)
- **χ = -5**: Euler characteristic (global topological invariant)

## Creating Your Own Template

### 1. Create a Template File

Create `templates/my-voice-app.md`:

```yaml
---
id: my-voice-app
type: canvasl-template

speech:
  input:
    lang: en-US
    continuous: true
    keywords: [hello, goodbye]
  output:
    voice: Google US English
    rate: 1.0

macros:
  - keyword: hello
    api: notifications
    method: showNotification
    params:
      title: Hello!
      body: You said hello
    type: [web_api, notifications]

validates:
  homology: true
  accessibility: true
---

# My Voice App

Say "hello" to get a greeting notification!
```

### 2. Load Your Template

Edit `voice-demo.html` to load your template:

```javascript
// Change this line:
controller.loadTemplate('/templates/my-voice-app.md');
```

### 3. Run It

```bash
npm run dev:voice
```

## Programmatic Usage

### Basic Example

```javascript
import { CANVASLVoiceApp } from './src/speech/index.js';

// Define template (or load from file)
const template = {
  id: 'my-app',
  type: 'node',
  dimension: 2,
  frontmatter: {
    type: 'canvasl-template',
    adjacency: {
      edges: ['input', 'output'],
      orientation: [1, -1]
    },
    speech: {
      input: {
        lang: 'en-US',
        continuous: true,
        interimResults: false,
        keywords: ['test']
      },
      output: {
        voice: 'Google US English',
        rate: 1.0,
        pitch: 1.0
      }
    },
    macros: [{
      keyword: 'test',
      api: 'geolocation',
      method: 'getCurrentPosition',
      params: {},
      type: ['web_api', 'geolocation']
    }],
    validates: {
      homology: true,
      byzantine: false,
      accessibility: true
    },
    features: {}
  },
  body: '# Test App'
};

// Create and start app
const app = new CANVASLVoiceApp(template);
app.start();

// Get state
const summary = app.getSummary();
console.log('Betti numbers:', summary.betti);
console.log('Executions:', summary.executionCount);

// Stop app
app.stop();
```

### Using the UI Controller

```javascript
import { initializeVoiceUI } from './src/speech/ui-integration.js';

// Initialize with container element
const controller = initializeVoiceUI('my-container');

// Load template
controller.loadTemplate('/templates/my-voice-app.md');
```

## Troubleshooting

### No Microphone Permission
- Chrome: Settings → Privacy → Microphone
- Check that your site has microphone access

### Speech Recognition Not Working
- Currently works best in Chrome/Edge
- Safari has limited support
- Firefox doesn't support Web Speech Recognition

### Notifications Not Appearing
- Grant notification permission when prompted
- Check browser notification settings
- Some browsers block notifications in incognito mode

### Keywords Not Detected
- Speak clearly and at normal speed
- Ensure keywords match exactly (case-insensitive)
- Check that continuous mode is enabled
- Look at browser console for transcript output

## Next Steps

1. **Explore the Code**
   - Read `src/speech/README.md` for API docs
   - Study `templates/voice-demo.md` for template syntax
   - Check `SPEECH_INTEGRATION.md` for architecture

2. **Add More Macros**
   - Implement custom Web API wrappers
   - Create new resolution functors
   - Extend the macro registry

3. **Advanced Features**
   - Federation with peer automata
   - Persistent state with IndexedDB
   - Custom homological validators
   - NLP integration for complex commands

4. **Integration**
   - Combine with semantic slides projector
   - Add to existing web applications
   - Build autonomous voice agents

## Resources

- **Demo**: http://localhost:5173/voice-demo.html
- **Test**: http://localhost:4173/test/voice-integration-test.html
- **Docs**: `src/speech/README.md`
- **Examples**: `templates/voice-demo.md`
- **Architecture**: `SPEECH_INTEGRATION.md`
- **API Reference**: MDN Web Docs - Web Speech API

## Support

If you encounter issues:
1. Check browser console for errors
2. Verify browser compatibility (Chrome/Edge recommended)
3. Ensure all permissions are granted
4. Review the integration test results

## Mathematical Background (Optional)

This system is based on sheaf-theoretic CANVASL formalization:

- **Chain Complexes**: C₀ → C₁ → C₂ → C₃ → C₄
- **Boundary Operators**: ∂ₙ: Cₙ → Cₙ₋₁ with ∂ ∘ ∂ = 0
- **Homology Groups**: Hₙ = ker(∂ₙ) / im(∂ₙ₊₁)
- **Betti Numbers**: βₙ = rank(Hₙ)
- **Presheaf of Keywords**: K: τᵒᵖ → Set
- **Gluing Condition**: Ȟ¹(X, K) = 0 for consistency

See `docs/34-Speech-Input-Output/` for complete mathematical treatment.

---

**Ready to build voice-controlled applications with topological guarantees!** 🎤🔬
