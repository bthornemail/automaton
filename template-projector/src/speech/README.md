# CANVASL Speech Input/Output System

A complete implementation of voice-controlled web applications using the CANVASL (Canvas Language) framework with sheaf-theoretic formalization.

## Overview

This system provides:

- **Voice Recognition**: Web Speech API integration for keyword detection
- **Voice Synthesis**: Text-to-speech output for results
- **W3C API Macros**: Type-safe wrappers for Web APIs (Geolocation, Notifications, Clipboard, etc.)
- **Homological Validation**: Chain complex verification ensuring topological consistency
- **Template-Based**: Declarative YAML/Markdown templates that compile to executable apps

## Architecture

### Chain Complex Structure

The system represents voice-controlled apps as cells in a graded chain complex:

- **C₀ (0-cells)**: Keywords (vertices)
- **C₁ (1-cells)**: Edges connecting keywords to APIs
- **C₂ (2-cells)**: Template documents (faces)
- **C₃ (3-cells)**: Interface triples for macros (solids)
- **C₄ (4-cells)**: Execution contexts and history (hypervolumes)

Boundary operators (∂₁, ∂₂, ∂₃, ∂₄) ensure ∂ ∘ ∂ = 0, maintaining homological consistency.

### Resolution Functors

Type-based keyword resolution through functors:

```typescript
Res_S: τᵒᵖ → Set
```

where `S` is a schema (e.g., "web_api", "geolocation") and τ is the topology of automata.

## Quick Start

### 1. Create a Template

Create a YAML/Markdown template in `templates/`:

```yaml
---
id: my-voice-app
type: canvasl-template

speech:
  input:
    lang: en-US
    continuous: true
    keywords: [location, notify]
  output:
    voice: Google US English
    rate: 1.0

macros:
  - keyword: location
    api: geolocation
    method: getCurrentPosition
    params: { enableHighAccuracy: true }
    type: [web_api, geolocation]

validates:
  homology: true
  accessibility: true
---

# My Voice App

Say "location" to get GPS coordinates.
```

### 2. Load and Run

```typescript
import { CANVASLVoiceApp } from './src/speech/index.js';

// Parse template (from file or string)
const template = parseTemplate(templateContent);

// Create app
const app = new CANVASLVoiceApp(template);

// Start listening
app.start();
```

### 3. Use the Demo UI

```bash
npm run dev:voice
```

Open http://localhost:5173/voice-demo.html

## API Reference

### CANVASLVoiceApp

Main voice application runtime.

```typescript
class CANVASLVoiceApp {
  constructor(template: CANVASLTemplate);
  start(): void;
  stop(): void;
  executeKeyword(keyword: string): Promise<any>;
  getSummary(): Summary;
  exportState(): string;
}
```

### TemplateCompiler

Compiles templates to executable apps with validation.

```typescript
class TemplateCompiler {
  compile(template: CANVASLTemplate): CompiledVoiceApp;
}
```

### MacroRegistry

Manages Web API macros with resolution.

```typescript
class MacroRegistry extends ResolutionFunctor {
  register(macro: WebAPIMacro): void;
  resolve(U: Set<string>, identifier: string, keyword: string): Promise<any>;
}
```

## Available Macros

### Geolocation
```typescript
keyword: "location"
api: "geolocation"
```
Returns GPS coordinates via `navigator.geolocation`.

### Notifications
```typescript
keyword: "notify"
api: "notifications"
```
Sends browser notification via `Notification` API.

### Clipboard
```typescript
keyword: "copy"
api: "clipboard"
```
Copies text to clipboard via `navigator.clipboard`.

### Storage (IndexedDB)
```typescript
keyword: "save"
api: "indexeddb"
```
Stores data in IndexedDB.

### Media Devices
```typescript
keyword: "camera"
api: "mediadevices"
```
Access camera/microphone via `navigator.mediaDevices`.

### Battery Status
```typescript
keyword: "battery"
api: "battery"
```
Get battery level (if available).

## Homological Validation

The compiler validates templates using:

1. **Boundary Consistency**: Ensures ∂ₙ maps reference valid (n-1)-cells
2. **Orientation Matching**: Edge orientations match in ∂₂ maps
3. **Homology Groups**: Computes Betti numbers βₙ = rank(Hₙ)
4. **Euler Characteristic**: χ = Σ(-1)ⁿβₙ

Example:
```typescript
const summary = app.getSummary();
// {
//   betti: [5, 4, 1, 5, 12],  // β₀ through β₄
//   euler: -5,                // χ
//   executionCount: 12        // |C₄|
// }
```

## Browser Support

Requires:
- **Web Speech API** (Chrome, Edge, Safari)
- **Modern JavaScript** (ES2020+)
- **W3C APIs** (Geolocation, Notifications, etc.)

Check support:
```typescript
import { isSpeechRecognitionSupported, isSpeechSynthesisSupported } from './speech-handlers.js';

if (!isSpeechRecognitionSupported()) {
  console.warn('Speech recognition not supported');
}
```

## Mathematical Foundations

Based on the sheaf-theoretic formalization in:
- `docs/34-Speech-Input-Output/01-canvasl-macros.md`
- `docs/34-Speech-Input-Output/02-templates-macros.md`
- `docs/34-Speech-Input-Output/03-federated-autonomous-system.md`

Key concepts:
- **Presheaf of keywords**: K: τᵒᵖ → Set
- **Sheaf condition**: Gluing compatible local sections
- **Čech cohomology**: Ȟ¹(X, K) = 0 ensures consistency
- **Byzantine consensus**: Topological fault tolerance

## Examples

See:
- `templates/voice-demo.md` - Complete demo template
- `voice-demo.html` - Interactive UI demo
- `docs/34-Speech-Input-Output/` - Full documentation

## License

MIT
