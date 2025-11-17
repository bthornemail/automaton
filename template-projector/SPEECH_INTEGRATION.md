# CANVASL Speech Input/Output Integration

Complete implementation of the speech input/output system from `docs/34-Speech-Input-Output/` into the template-projector project.

## Summary

Successfully integrated a full-featured voice-controlled web application framework based on sheaf-theoretic CANVASL formalization with chain complexes and homological validation.

## What Was Implemented

### 1. Core Type System (`src/speech/types.ts`)

Complete TypeScript types for:
- **Chain Complex**: C₀ through C₄ cells with boundary operators
- **CANVASL Templates**: Frontmatter structure with speech config and macros
- **Resolution Functors**: Abstract base class for type-based resolution
- **Sheaf Sections**: Keyword configurations over automata domains
- **Validation Results**: Homological and accessibility checks

### 2. Web Speech API Handlers (`src/speech/speech-handlers.ts`)

- **SpeechRecognitionHandler**: Keyword detection from voice input
  - Continuous listening mode
  - Configurable language and keywords
  - Error handling and auto-restart

- **SpeechSynthesisHandler**: Text-to-speech output
  - Voice selection
  - Rate and pitch control
  - Promise-based API

- **Utility Functions**: Browser support detection, voice enumeration

### 3. W3C API Macro System (`src/speech/web-api-macros.ts`)

Implemented 7 macro types:
1. **GeolocationMacro**: GPS coordinates via `navigator.geolocation`
2. **NotificationMacro**: Browser notifications via `Notification` API
3. **ClipboardMacro**: Clipboard access via `navigator.clipboard`
4. **StorageMacro**: IndexedDB persistence
5. **MediaMacro**: Camera/microphone via `navigator.mediaDevices`
6. **BatteryMacro**: Battery status via `navigator.getBattery()`
7. **VibrationMacro**: Haptic feedback via `navigator.vibrate()`

Plus:
- **MacroRegistry**: Resolution functor for Web APIs
- **ResolutionRegistry**: Global schema → functor mapping
- **createMacro**: Factory function for macro instantiation

### 4. Template Compiler (`src/speech/template-compiler.ts`)

- **TemplateCompiler**: Compiles YAML templates to executable apps
  - Builds chain complex from template structure
  - Generates speech handlers
  - Creates macro executors
  - Validates homology and accessibility

- **HomologyComputer**: Topological validation
  - Boundary matrix computation
  - Homology group calculation
  - Betti number extraction
  - Row reduction for linear algebra

- **Helper Functions**: `computeAllBetti`, `eulerCharacteristic`

### 5. Voice App Runtime (`src/speech/canvasl-voice-app.ts`)

- **CANVASLVoiceApp**: Main application runtime
  - Speech recognition integration
  - Keyword → macro execution
  - Result formatting and speech output
  - C₄ execution history tracking
  - State export as JSONL
  - Homological summary computation

- **CANVASLAppManager**: Multi-app orchestration
  - Create and register apps
  - Start/stop lifecycle management
  - Summary aggregation

### 6. UI Integration (`src/speech/ui-integration.ts`)

- **VoiceAppUIController**: Browser UI for voice apps
  - Template loading from files
  - Simple YAML parser
  - Start/stop controls
  - Real-time status indicators
  - Keyword list display
  - Execution history timeline
  - Chain complex state visualization
  - Browser support warnings

- **initializeVoiceUI**: One-line initialization function

### 7. Demo Application (`voice-demo.html`)

Complete interactive demo with:
- Modern, responsive UI design
- Gradient background and glassmorphism
- Real-time status indicators with pulse animation
- Execution history tracking
- Chain complex metrics display
- Usage instructions and keyword reference
- Homological structure explanation

### 8. Example Template (`templates/voice-demo.md`)

Full CANVASL template demonstrating:
- Speech input/output configuration
- 5 macro definitions (location, notify, save, copy, battery)
- Adjacency structure for ∂₂ boundary
- Homological and accessibility validation
- Comprehensive documentation

### 9. Documentation

- `src/speech/README.md`: Complete API reference and usage guide
- `src/speech/index.ts`: Clean module exports
- Inline JSDoc comments throughout

### 10. Package Integration

Updated `package.json` with:
- `npm run dev:voice`: Development server for voice demo
- `npm run preview:voice`: Production preview of voice demo
- Additional keywords: voice-control, web-speech-api, chain-complexes, homology

## File Structure

```
template-projector/
├── src/speech/
│   ├── types.ts                  # Core type definitions
│   ├── speech-handlers.ts        # Web Speech API wrappers
│   ├── web-api-macros.ts        # W3C API macro implementations
│   ├── template-compiler.ts      # Template → app compilation
│   ├── canvasl-voice-app.ts     # Voice app runtime
│   ├── ui-integration.ts         # Browser UI controller
│   ├── index.ts                  # Module exports
│   └── README.md                 # Documentation
├── templates/
│   └── voice-demo.md             # Example template
├── voice-demo.html               # Interactive demo page
└── package.json                  # Updated with voice scripts
```

## Key Features

### Sheaf-Theoretic Formalization

- **Keywords as C₀ cells**: Vertices in the chain complex
- **API connections as C₁ cells**: Edges with typed resolution
- **Templates as C₂ cells**: Documents with boundary operators
- **Macros as C₃ cells**: Interface triples
- **Executions as C₄ cells**: Evolution contexts

### Homological Validation

- Boundary consistency: ∂ ∘ ∂ = 0
- Betti number computation: β₀ through β₄
- Euler characteristic: χ = Σ(-1)ⁿβₙ
- Homology group analysis: Hₙ = ker(∂ₙ) / im(∂ₙ₊₁)

### Type-Based Resolution

- **Resolution functors**: Res_S: τᵒᵖ → Set
- **Restriction maps**: ρ_{U,V} for domain projections
- **Schema registry**: web_api, file, memory, peer, http

### Accessibility Compliance

- WCAG 2.1 language specification validation
- Speech rate range checking (0.5-2.0)
- Voice output configuration
- Error messages and fallbacks

## Usage Examples

### Basic Usage

```typescript
import { CANVASLVoiceApp } from './src/speech/index.js';

// Load template
const template = await loadTemplate('/templates/voice-demo.md');

// Create and start app
const app = new CANVASLVoiceApp(template);
app.start();

// Monitor state
const summary = app.getSummary();
console.log('Betti numbers:', summary.betti);
console.log('Executions:', summary.executionCount);
```

### With UI Controller

```typescript
import { initializeVoiceUI } from './src/speech/ui-integration.js';

// Initialize UI
const controller = initializeVoiceUI('voice-app-container');

// Auto-load template
controller.loadTemplate('/templates/voice-demo.md');
```

### Custom Template

```yaml
---
id: custom-app
type: canvasl-template

speech:
  input:
    lang: en-US
    keywords: [weather, news]
  output:
    voice: Google US English
    rate: 1.0

macros:
  - keyword: weather
    api: geolocation
    method: getCurrentPosition
    params: {}
    type: [web_api, geolocation]
---

# Custom Voice App
```

## Running the Demo

1. **Development Mode**:
   ```bash
   npm run dev:voice
   ```

2. **Open Browser**: Navigate to http://localhost:5173/voice-demo.html

3. **Grant Permissions**: Allow microphone and notifications

4. **Start Listening**: Click "Start Listening" button

5. **Say Keywords**: Try "location", "notify", "save", "copy", or "battery"

6. **Watch Updates**: See execution history and chain complex state update

## Mathematical Foundations

Based on complete sheaf-theoretic formalization from:
- Discrete 11-point topology for M-theory correspondence
- Presheaf of keywords with gluing conditions
- Čech cohomology: Ȟ¹(X, K) = 0 for consistency
- Byzantine fault tolerance via homological invariants
- E₈×E₈ heterotic string correspondence

## Browser Compatibility

- ✅ Chrome/Edge: Full support (Web Speech API)
- ✅ Safari: Partial support (recognition may be limited)
- ⚠️ Firefox: Speech synthesis only (no recognition)
- ❌ IE: Not supported

## Next Steps

Potential extensions:
1. **Federation**: Multi-automaton peer-to-peer synchronization
2. **Persistence**: Save/load app state from IndexedDB
3. **NLP**: Natural language processing for complex commands
4. **WebGL**: 3D visualization of chain complex topology
5. **MCP Server**: Model Context Protocol integration for LLM control

## References

- `docs/34-Speech-Input-Output/01-canvasl-macros.md`: Sheaf formalization
- `docs/34-Speech-Input-Output/02-templates-macros.md`: Template system
- `docs/34-Speech-Input-Output/03-federated-autonomous-system.md`: Autonomous agents
- MDN Web Docs: Web Speech API, W3C APIs
- Template-Projector: Existing semantic slides infrastructure

## Integration Complete ✅

All components from `docs/34-Speech-Input-Output/` have been successfully implemented and integrated into the template-projector project with:
- Full TypeScript type safety
- Homological validation
- Browser UI
- Interactive demo
- Comprehensive documentation
