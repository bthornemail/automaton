---
id: targeting-2d-canvas-context
title: "Targeting 2D Canvas Context"
level: intermediate
type: technical-note
tags: [canvas, offscreen-canvas, web-workers, 2d-rendering, performance, browser-optimization]
keywords: [offscreen-canvas, web-workers, 2d-rendering, performance-optimization, browser-compatibility, static-canvas]
prerequisites: [meta-log-canvas-slide-templates, feedback-on-proposal]
enables: [canvasl-semantic-slides-project, proposal-restructuring]
related: [meta-log-canvas-slide-templates, proposal-restructuring]
readingTime: 20
difficulty: 3
blackboard:
  status: active
  assignedAgent: "Visualization-Agent"
  lastUpdate: "2025-01-07"
  dependencies: [canvas-api]
  watchers: ["6D-Intelligence-Agent"]
---

Yes, it's feasible to shift from Three.js (which relies on WebGL for 3D rendering) to a combination of static canvases (for simple, non-interactive views) and OffscreenCanvas (for dynamic, off-main-thread rendering) while leveraging web workers for better performance and thread isolation. This approach simplifies the demo presentation by reducing dependencies on heavy 3D libraries like Three.js, making it lighter, more compatible across browsers, and easier to share publicly (e.g., via GitHub Pages or static hosting). It aligns well with the CanvasL protocol's emphasis on declarative, evolvable content, as we can execute R5RS expressions or queries in workers to generate canvas output dynamically.

### Why This Switch Makes Sense
- **Three.js Overhead**: Three.js is great for complex 3D (e.g., dimensional topology visualizations), but it's overkill for many slides. Raw Canvas 2D/WebGL via OffscreenCanvas can handle 2D graphics, simple animations, and even basic 3D if needed (though we'd stick to 2D for simplicity unless specified).
- **Static Mode**: Use standard `<canvas>` for quick, non-animated renders (e.g., SVG-like outputs exported as PNG/SVG for sharing).
- **OffscreenCanvas + Web Workers**: If web workers are available (which they are in all modern browsers as of 2025, per MDN), transfer rendering to a worker thread. This prevents main-thread blocking, improves FPS for animations, and allows parallel execution (e.g., one worker per slide or hotspot).
- **Browser Support**: Web workers (since ~2010) and OffscreenCanvas (widely supported since 2023) are available in Chrome, Firefox, Safari, and Edge. We can feature-detect them with `if ('OffscreenCanvas' in window && 'Worker' in window)` and fallback to main-thread canvas if not.
- **Public Sharing Benefits**: Smaller bundle size (no Three.js ~500KB), better mobile performance, and easier embedding (e.g., in blogs, wikis, or as a standalone HTML file). Outputs can be exported as static assets for non-interactive demos.
- **Integration with CanvasL/Meta-Log**: CanvasL definitions can specify rendering mode ("static" or "offscreen"). Workers can execute Meta-Log logic (e.g., R5RS evals, ProLog queries) via postMessage, keeping the main thread responsive.

Potential trade-offs:
- Lose some 3D ease (e.g., no built-in camera/ lighting from Three.js), but we can emulate simple effects with raw WebGL if needed.
- Workers can't directly access DOM, so rendering results (e.g., ImageBitmap) must be transferred back to the main thread for display.

### Updated Proposal: CanvasL Media Slides with Static/Offscreen Rendering
I'll adapt the previous proposal to replace Three.js with this new approach. The system remains dynamic and interactive, but rendering is now worker-based where possible.

#### System Structure Changes
- **Rendering Modes**:
  - **Static**: Main-thread `<canvas>` for quick draws; export to Blob (PNG/SVG) via `toDataURL()` or `convertToBlob()`.
  - **Animated/Offscreen**: If workers available, transfer to OffscreenCanvas in a worker; render 2D animations (e.g., particle systems) or simple WebGL; transfer ImageBitmap back for display.
- **Fallback**: If no workers/OffscreenCanvas, use main-thread canvas with requestAnimationFrame.
- **Dependencies**: Remove Three.js; use native Canvas APIs. Bundle size drops significantly.

#### Technical Specification Updates

**Asset Organization** (Same as before, but add worker scripts):
```
slides-system/
├── viewer.html
├── assets/  # (unchanged)
├── js/
│   ├── meta-log-engine.js  # (unchanged)
│   ├── canvasl-parser.js   # (unchanged)
│   ├── renderer.js         # Updated for static/offscreen
│   ├── interactions.js     # (unchanged)
│   ├── automaton-bridge.js # (unchanged)
│   └── offscreen-worker.js # New: Worker script for rendering
└── css/  # (unchanged)
```

**Updated Renderer (renderer.js)**:
```javascript
class Renderer {
  constructor(canvasId) {
    this.canvas = document.getElementById(canvasId);
    this.ctx = this.canvas.getContext('2d'); // Default 2D for static
    this.worker = null;
    this.supportsOffscreen = 'OffscreenCanvas' in window && 'Worker' in window;
    if (this.supportsOffscreen) {
      this.initWorker();
    }
  }

  initWorker() {
    this.worker = new Worker('js/offscreen-worker.js');
    this.worker.onmessage = (e) => {
      if (e.data.type === 'bitmap') {
        // Display transferred ImageBitmap
        this.ctx.transferFromImageBitmap(e.data.bitmap);
      } else if (e.data.type === 'staticBlob') {
        // For static export: Create img from Blob
        const img = new Image();
        img.src = URL.createObjectURL(e.data.blob);
        img.onload = () => {
          this.ctx.drawImage(img, 0, 0);
        };
      }
    };
  }

  render(data, mode = 'animated') {
    if (mode === 'static' || !this.supportsOffscreen) {
      // Main-thread static render (2D example)
      this.ctx.fillStyle = '#00ffff'; // Accent color
      this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);
      this.ctx.fillStyle = '#0a0a0a';
      this.ctx.font = '20px Inter';
      this.ctx.fillText(data.text || 'Static Render from CanvasL', 10, 50);
      // Export as static asset if needed
      this.canvas.toBlob(blob => {
        // Share or save blob (e.g., download link)
      });
    } else {
      // Offscreen animated: Transfer to worker
      const offscreen = this.canvas.transferControlToOffscreen();
      this.worker.postMessage({
        type: 'init',
        canvas: offscreen,
        data: data  // Pass CanvasL exec result
      }, [offscreen]);
      // Worker handles animation loop, sends bitmaps
    }
  }
}
```

**New Worker Script (offscreen-worker.js)**:
```javascript
let offscreen;
let gl; // For WebGL if needed; else use '2d'

onmessage = (e) => {
  if (e.data.type === 'init') {
    offscreen = e.data.canvas;
    // Use 2D context for simplicity (or 'webgl' for advanced)
    const ctx = offscreen.getContext('2d'); // Or 'webgl'
    // Example animation loop
    function animate() {
      // Dynamic render based on data (e.g., particles from R5RS sim)
      ctx.clearRect(0, 0, offscreen.width, offscreen.height);
      ctx.fillStyle = '#FF6B6B'; // Dim-0D color
      ctx.fillRect(Math.random() * offscreen.width, Math.random() * offscreen.height, 50, 50);
      // Transfer frame
      const bitmap = offscreen.transferToImageBitmap();
      postMessage({ type: 'bitmap', bitmap }, [bitmap]);
      requestAnimationFrame(animate);
    }
    animate();
  }
};
```

**CanvasL Integration Example** (slides-master.canvasl.jsonl):
Add directives for rendering mode:
```json
{"id": "slide-1", "dimension": "0D", "type": "r5rs-call", "function": "r5rs:church-zero", "directive": "@render", "mode": "offscreen", "worker": true}
```

#### Next Steps for Demo
1. **Test Support**: Add feature detection in viewer.html.
2. **Build Demo Deck**: Use the slide breakdown from before; render static for simple slides, offscreen for interactive ones.
3. **Public Sharing**: Host on GitHub; export static PNGs per slide for non-browser views (e.g., via worker blobs).
4. **Performance**: Workers offload rendering, ideal for public demos on varied devices.

This updated system maintains the project's self-referential magic while being more accessible. If you provide a specific slide or code to adapt, I can refine further!