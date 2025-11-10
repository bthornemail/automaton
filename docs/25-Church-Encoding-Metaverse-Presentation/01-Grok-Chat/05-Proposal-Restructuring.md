---
id: proposal-restructuring
title: "Rewritten CanvasL Media Slides Proposal: Plugin-Based Architecture"
level: intermediate
type: specification
tags: [plugin-architecture, browser-side, extensibility, meta-log-integration, projector-system]
keywords: [plugin-architecture, browser-side-implementation, extensible-plugins, meta-log-integration, projector-system, base-plugin]
prerequisites: [targeting-2d-canvas-context, meta-log-canvas-slide-templates]
enables: [canvasl-semantic-slides-project, macro-examples]
related: [targeting-2d-canvas-context, meta-log-canvas-slide-templates, macro-examples]
readingTime: 35
difficulty: 4
blackboard:
  status: active
  assignedAgent: "OpenCode-Integration-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [meta-log-db, plugin-system]
  watchers: ["6D-Intelligence-Agent"]
---

# 🎥 Rewritten CanvasL Media Slides Proposal: Plugin-Based Architecture with Browser-Side Focus

## 📋 Executive Summary

**Goal**: Restructure the CanvasL Media Slides system into a modular, extensible plugin architecture, emphasizing browser-side environments (HTML5, JS, Web Workers, Canvas APIs). The core "projector" will serve as the primary implementation of the `templates/plugin` base, allowing users to create extendable plugins for custom rendering, interactions, and Meta-Log integrations. This enables dynamic, interactive slides that leverage CanvasL descriptions and the Meta-Log framework (ProLog, DataLog, R5RS) for self-evolving presentations.

**Key Changes from Original**:
- **Folder Organization**: Align with specified structure (`templates/plugin`, `templates/projector`, `templates/slides`, `templates/cards`, `templates/documents`).
- **Extensibility**: Plugins are first-class; users can extend the projector by creating new plugins that hook into Meta-Log for logic (e.g., querying blackboard, executing R5RS).
- **Browser-Side Focus**: All rendering via native Canvas (static/offscreen), Web Workers for performance, no external libs like Three.js. Fallbacks for older browsers; emphasis on offline-capable, embeddable demos.
- **Meta-Log Integration**: Plugins can directly invoke Meta-Log (e.g., for dynamic content generation via R5RS evals or ProLog queries), making slides "living" and evolvable in-browser.

**Delivery**: A self-contained browser app (viewer.html) that loads plugins dynamically. Plugins are JS modules with CanvasL hooks. Public sharing via GitHub; users fork/extend plugins for custom decks.

**Benefits**:
- **Extensibility**: Developers create plugins (e.g., custom renderers) that integrate Meta-Log without forking the core.
- **Browser Optimization**: Lightweight (no heavy deps), worker-based for smooth animations, static exports for sharing.
- **Dynamic & Evolvable**: Meta-Log in plugins allows slides to query/evolve in real-time (e.g., user input triggers DataLog materialization).

**Timeline**: Prototype restructure in 1 week; plugin examples in 2 weeks; full Meta-Log browser integration in 3 weeks.

**🎯 System Flow**: Load Plugin → Parse CanvasL (Meta-Log) → Render (Static/Offscreen) → Interact/Evolve → Export/Share.

## 📊 System Structure

Organized under `templates/` for modularity. The `projector` is the core plugin implementation, bootstrapping others. Focus on browser-side: JS modules, ES6 imports, Web Workers for off-main-thread tasks (e.g., Meta-Log evals).

- **templates/plugin**: Base templates for creating extendable plugins.
  - Contains abstract classes/interfaces for plugins (e.g., `BasePlugin.js` with hooks for init, render, evolve).
  - Example: Plugin manifest (JSON) for Meta-Log integration points (e.g., "hooks": ["r5rs-eval", "prolog-query"]).

- **templates/projector**: Core implementation of the plugin system (extends `templates/plugin`).
  - Acts as the "holding" projector: Loads/coordinates other plugins, handles viewer lifecycle.
  - Includes browser-side Meta-Log shim (lightweight JS impl of ProLog/DataLog/R5RS for in-browser execution).
  - Files: `Projector.js` (main class), `MetaLogBridge.js` (browser-safe Meta-Log adapter).

- **templates/slides**: Slide-specific templates and examples.
  - CanvasL-based slide defs (e.g., `basic-slide.canvasl.jsonl` with directives for rendering modes).
  - Extensible via plugins (e.g., a plugin adds custom slide types like "quantum-visualizer").

- **templates/cards**: Modular card components for reusable content (e.g., info cards, hotspots).
  - CanvasL snippets for cards (e.g., `agent-card.canvasl.jsonl` with R5RS for dynamic text).
  - Plugins can extend cards with Meta-Log (e.g., query blackboard for real-time data).

- **templates/documents**: Full deck/document templates and docs.
  - Master decks (e.g., `demo-deck.canvasl.jsonl` referencing slides/cards).
  - Documentation: How-to guides for creating plugins, integrating Meta-Log (e.g., "Extend the Projector.md").

**Overall Repo Structure** (Browser-Focused):
```
template-projector/
├── src/
│   ├── plugin/
│   │   ├── BasePlugin.js          # Abstract plugin class with Meta-Log hooks
│   │   ├── plugin-manifest.json   # Template for plugin config (e.g., hooks, deps)
│   │   └── example-plugin.js      # Sample: "AnimationPlugin" with worker integration
│   ├── projector/
│   │   ├── Projector.js           # Core: Loads plugins, renders decks
│   │   ├── MetaLogBridge.js       # Browser Meta-Log: JS impl for R5RS/ProLog/DataLog
│   │   └── ProjectorPlugin.js     # Projector as a plugin (self-extends BasePlugin)
│   ├── macros/
│   │   ├── RSR5.canvasl
│   │   ├── Prolog.canvasl
│   │   └── Datalog.canvasl
│   │   └── Metalog.canvasl
│   │   └── Automaton.canvasl
│   │   └── Template.canvasl
│   │   └── Projector.canvasl
├── templates/
│   ├── slides/
│   │   ├── basic-slide.canvasl.jsonl  # Template slide with static/offscreen directive
│   │   └── dimensional-slide.canvasl.jsonl  # Example with Meta-Log query
│   ├── cards/
│   │   ├── info-card.canvasl.jsonl    # Reusable card with R5RS dynamic content
│   │   └── hotspot-card.js        # JS wrapper for interactive cards
│   └── documents/
│       ├── demo-deck.canvasl.jsonl    # Full deck template
│       ├── ExtendPlugins.md       # Guide: Creating plugins with Meta-Log
│       └── MetaLogIntegration.md  # How plugins use Meta-Log in browser
├── viewer.html                    # Entry point: Browser viewer
├── assets/                        # (Unchanged: canvasl, static, animated, audio)
├── js/                            # Shared utils (e.g., offscreen-worker.js)
├── css/                           # (Unchanged)
└── README.md                      # Setup: "Build extendable plugins for browser slides"
```

## 📈 Slide-by-Slide Breakdown (Updated for Plugin Extensibility)

The demo deck ("Exploring Church Encoding Dimensions") remains similar, but now each slide/card is plugin-extensible. E.g., a user-made "QuantumPlugin" could override Slide 4's rendering with custom Meta-Log logic.

- **Loading Flow**: Projector loads plugins dynamically (e.g., via `import()`), registers Meta-Log hooks, then parses slides/cards.

**Example Slide (templates/slides/dimensional-slide.canvasl.jsonl)**:
```json
{"id": "slide-4", "dimension": "3D-7D", "type": "datalog-rule", "rule": "progression(D1, D2) :- successor(D1, D2).", "plugin": "AnimationPlugin", "directive": "@render", "mode": "offscreen"}
```
- Plugins handle rendering: BasePlugin provides defaults; custom plugins override with Meta-Log (e.g., R5RS for animation math).

## 🎨 Technical Specification

### Plugin Extensibility
- **BasePlugin (templates/plugin/BasePlugin.js)**:
```javascript
class BasePlugin {
  constructor(config) {
    this.config = config; // From manifest: hooks, Meta-Log integrations
    this.metaLog = new MetaLogBridge(); // Browser Meta-Log instance
  }

  init() { /* Hook: Setup worker if needed */ }

  render(data, mode) {
    // Default: Static canvas draw
    // Override in child plugins
    if (mode === 'offscreen') {
      // Post to worker with Meta-Log data (e.g., R5RS result)
    }
  }

  evolve(slide) {
    // Use Meta-Log: E.g., ProLog query for changes, then modify CanvasL
    this.metaLog.prologQuery('evolve_rule(X).').then(result => {
      // Update slide, validate SHACL
    });
  }

  hook(type, args) {
    // Meta-Log integration: E.g., if (type === 'r5rs') return this.metaLog.evalR5RS(args);
  }
}
```

- **Projector as Plugin (templates/projector/ProjectorPlugin.js)**:
```javascript
class ProjectorPlugin extends BasePlugin {
  loadPlugins(pluginPaths) {
    // Dynamic import: e.g., import('./example-plugin.js').then(plugin => this.register(plugin));
  }

  renderDeck(deckPath) {
    // Parse CanvasL, delegate to plugins based on slide "plugin" key
    // Use workers for browser perf
  }
}
```

### Browser-Side Meta-Log Integration
- **MetaLogBridge (templates/projector/MetaLogBridge.js)**: Lightweight JS port/emulation of Meta-Log.
  - R5RS: Use a Scheme interpreter like BiwaScheme (or custom minimal impl).
  - ProLog/DataLog: Simple JS unification/fixpoint engines (e.g., based on public libs like logic.js, but keep lightweight).
  - SHACL: Basic validator for CanvasL consistency.
- Plugins call `metaLog.hook('prolog', query)` for in-browser logic without server.

### Rendering Updates
- Stick to static/offscreen as per request.
- **Offscreen Worker**: Handles Meta-Log evals if compute-heavy (e.g., R5RS simulations for animations).
- Fallback: Main-thread for non-worker browsers.

## 🔧 Development & Integration Plan

1. **Restructure Folders**: Migrate existing code to templates/*; make projector extend plugin.
2. **Plugin Examples**: Create 2-3 sample plugins (e.g., StaticRendererPlugin, MetaLogQueryPlugin).
3. **Browser Testing**: Ensure works in Chrome/Firefox/Safari; offline via Service Workers.
4. **Docs in templates/documents**: Tutorials for "Building a Custom Plugin with Meta-Log".
5. **Public Demo**: Host on GitHub Pages; include extendable deck for users to fork.

This rewritten plan makes the system highly extensible, browser-centric, and Meta-Log-powered. Plugins empower users to innovate (e.g., a plugin for AI-generated slides via Meta-Log). Ready for feedback or implementation details!