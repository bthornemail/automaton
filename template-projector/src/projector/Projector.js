/**
 * Projector - Core engine for CanvasL Semantic Slides
 * 
 * Loads plugins, parses CanvasL files, executes Meta-Log operations,
 * and coordinates rendering of semantic slides.
 */

import { BasePlugin } from '../plugin/BasePlugin.js';
import { MetaLogBridge } from './MetaLogBridge.js';
import { MacroExpander } from './MacroExpander.js';
import { IncludeLoader } from './IncludeLoader.js';
import { CanvasLExecutor } from './CanvasLExecutor.js';
import { ErrorHandler } from '../utils/ErrorHandler.js';
import { AgentCoordinator } from '../agents/AgentCoordinator.js';

export class Projector extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'Projector',
      version: '0.1.0',
      hooks: ['r5rs', 'prolog', 'datalog', 'sparql'],
      ...config
    });
    
    this.plugins = new Map();
    this.metaLog = new MetaLogBridge();
    this.macroExpander = new MacroExpander();
    this.includeLoader = new IncludeLoader();
    this.executor = new CanvasLExecutor(this.metaLog);
    this.errorHandler = new ErrorHandler();
    this.slides = [];
    this.currentSlide = null;
    
    // Initialize agent coordinator for slide population
    this.agentCoordinator = new AgentCoordinator(
      config.kernelUrl || '/automaton-kernel.jsonl',
      config.contentIndexUrl || '/content-index.jsonl'
    );
    
    // Set error handler for federation
    this.metaLog.setErrorHandler(this.errorHandler);
    
    // Register default recovery strategies
    this.setupErrorRecovery();
  }

  /**
   * Setup error recovery strategies
   */
  setupErrorRecovery() {
    // Network error recovery
    this.errorHandler.registerRecoveryStrategy('network', async (errorInfo, context) => {
      if (context.retry) {
        const maxRetries = 3;
        const baseDelay = 1000;
        
        for (let i = 0; i < maxRetries; i++) {
          await new Promise(resolve => setTimeout(resolve, baseDelay * Math.pow(2, i)));
          try {
            return await context.retry();
          } catch (retryError) {
            if (i === maxRetries - 1) throw retryError;
          }
        }
      }
      throw new Error('Network recovery failed');
    });

    // Rate limit recovery
    this.errorHandler.registerRecoveryStrategy('ratelimit', async (errorInfo, context) => {
      await new Promise(resolve => setTimeout(resolve, 5000));
      if (context.retry) {
        return await context.retry();
      }
      throw new Error('Rate limit recovery failed');
    });
  }

  /**
   * Initialize projector
   */
  async onInit() {
    try {
      // Initialize Meta-Log bridge (includes meta-log-db initialization)
      await this.metaLog.init();
      
      // Initialize R5RS fallback
      await this.metaLog.evalR5RS('(define projector-version "0.1.0")');
      
      // Initialize agent coordinator
      await this.agentCoordinator.init();
      
      // Load built-in plugins
      await this.loadBuiltInPlugins();
    } catch (error) {
      const recovery = await this.errorHandler.handle(error, {
        context: 'projector_init'
      });
      
      if (!recovery.recovered) {
        throw error;
      }
    }
  }

  /**
   * Load built-in plugins
   */
  async loadBuiltInPlugins() {
    // Load DBpedia plugin
    try {
      const { DBpediaPlugin } = await import('../plugin/dbpedia-plugin.js');
      const dbpediaPlugin = new DBpediaPlugin();
      await this.registerPlugin(dbpediaPlugin);
    } catch (error) {
      console.warn('Failed to load DBpedia plugin:', error);
    }
  }

  /**
   * Register a plugin
   * @param {BasePlugin} plugin - Plugin instance
   */
  async registerPlugin(plugin) {
    if (!(plugin instanceof BasePlugin)) {
      throw new Error('Plugin must extend BasePlugin');
    }
    
    plugin.validate();
    
    // Initialize plugin with Meta-Log bridge
    await plugin.init(this.metaLog);
    
    this.plugins.set(plugin.name, plugin);
    
    console.log(`Plugin registered: ${plugin.name} v${plugin.version}`);
  }

  /**
   * Load plugin dynamically
   * @param {string} pluginPath - Path to plugin module
   */
  async loadPlugin(pluginPath) {
    try {
      const module = await import(pluginPath);
      const PluginClass = module.default || module[Object.keys(module)[0]];
      
      if (!PluginClass) {
        throw new Error(`No default export found in ${pluginPath}`);
      }
      
      const plugin = new PluginClass();
      await this.registerPlugin(plugin);
      
      return plugin;
    } catch (error) {
      throw new Error(`Failed to load plugin ${pluginPath}: ${error.message}`);
    }
  }

  /**
   * Parse CanvasL file
   * @param {string} content - CanvasL JSONL content
   * @param {string} sourceUrl - Source URL for @include resolution
   * @returns {Promise<Array>} Parsed objects with @include expanded
   */
  async parseCanvasL(content, sourceUrl = '') {
    // Set base path for include resolution
    if (sourceUrl) {
      const basePath = sourceUrl.substring(0, sourceUrl.lastIndexOf('/') + 1);
      this.includeLoader.setBasePath(basePath);
    }
    
    // Parse content (handles @include directives)
    const objects = this.includeLoader.parseCanvasL(content, sourceUrl);
    
    // Expand @include directives (this loads included files)
    const expanded = await this.includeLoader.expandIncludes(objects, sourceUrl);
    
    const slidesFound = expanded.filter(o => o.type === 'slide');
    console.log(`Parsed ${expanded.length} objects from ${sourceUrl}`);
    console.log(`Slides found: ${slidesFound.length}`, slidesFound.map(s => ({ id: s.id, dimension: s.dimension || 'none' })));
    
    // Check for include errors
    if (this.includeLoader.loadErrors && this.includeLoader.loadErrors.length > 0) {
      console.warn(`Include errors in ${sourceUrl}:`, this.includeLoader.loadErrors);
    }
    
    return expanded;
  }

  /**
   * Expand CanvasL macros
   * @param {Array} objects - Parsed CanvasL objects
   * @returns {Promise<Array>} Expanded objects
   */
  async expandMacros(objects) {
    // Load macros from objects
    this.macroExpander.loadMacros(objects);
    
    // Expand all macros
    return this.macroExpander.expandAll(objects);
  }

  /**
   * Execute CanvasL objects
   * @param {Array} objects - CanvasL objects
   * @returns {Promise<Object>} Execution result
   */
  async execute(objects) {
    return await this.executor.executeAll(objects);
  }

  /**
   * Load and render a deck
   * @param {string} deckPath - Path to deck CanvasL file
   * @returns {Promise<Object>} Rendered deck
   */
  async loadDeck(deckPath) {
    try {
      const response = await fetch(deckPath);
      if (!response.ok) {
        throw new Error(`Failed to fetch ${deckPath}: ${response.statusText}`);
      }
      
      const content = await response.text();
      
      // Parse CanvasL (includes @include expansion)
      const objects = await this.parseCanvasL(content, deckPath);
      
      // Check for include errors
      if (this.includeLoader.loadErrors && this.includeLoader.loadErrors.length > 0) {
        console.warn('Some include files failed to load:', this.includeLoader.loadErrors);
      }
      
      // Expand macros
      const expanded = await this.expandMacros(objects);
      
      // Execute
      const result = await this.execute(expanded);
      
      // Append slides instead of replacing (allows multiple deck loads)
      if (result.slides && result.slides.length > 0) {
        this.slides = this.slides || [];
        this.slides.push(...result.slides);
      } else {
        this.slides = result.slides || this.slides || [];
      }
      
      // Log slide loading details
      console.log(`Loaded ${this.slides.length} slides from deck`);
      if (result.errors && result.errors.length > 0) {
        console.warn('Execution errors:', result.errors);
      }
      
      // Don't automatically populate - agents work in background
      // Population happens on-demand via re-render button or background processing
      
      return result;
    } catch (error) {
      const recovery = await this.errorHandler.handle(error, {
        context: 'load_deck',
        deckPath,
        retry: () => this.loadDeck(deckPath)
      });
      
      if (recovery.recovered) {
        return recovery.recovery;
      }
      
      throw new Error(`Failed to load deck ${deckPath}: ${error.message}`);
    }
  }

  /**
   * Render a slide
   * @param {Object} slide - Slide definition
   * @param {string} mode - Rendering mode
   * @returns {Promise<Object>} Rendered slide
   */
  async renderSlide(slide, mode = 'static') {
    this.currentSlide = slide;
    
    // Find appropriate plugin for rendering
    const pluginName = slide.plugin || 'default';
    const plugin = this.plugins.get(pluginName);
    
    if (plugin) {
      return await plugin.render(slide, mode);
    } else {
      // Default rendering
      return slide;
    }
  }

  /**
   * Render current slide to canvas (with scrolling support)
   * @param {HTMLCanvasElement} canvas - Canvas element
   */
  renderToCanvas(canvas) {
    if (!canvas) {
      console.error('Canvas element not found');
      return;
    }
    
    try {
      this._renderToCanvasInternal(canvas);
    } catch (error) {
      console.error('Error rendering slide to canvas:', error);
      // Render error message
      const ctx = canvas.getContext('2d');
      ctx.fillStyle = '#ff4444';
      ctx.font = '24px Inter, sans-serif';
      ctx.textAlign = 'center';
      ctx.fillText('Error rendering slide', canvas.width / 2, canvas.height / 2 - 30);
      ctx.fillStyle = '#888';
      ctx.font = '16px Inter, sans-serif';
      ctx.fillText(error.message, canvas.width / 2, canvas.height / 2);
      if (this.currentSlide) {
        ctx.fillText(`Slide: ${this.currentSlide.id || 'unnamed'}`, canvas.width / 2, canvas.height / 2 + 30);
      }
    }
  }
  
  /**
   * Internal rendering method
   * @param {HTMLCanvasElement} canvas - Canvas element
   */
  _renderToCanvasInternal(canvas, isRecursive = false) {

    const ctx = canvas.getContext('2d');
    if (!ctx) {
      console.error('Could not get 2D context');
      return;
    }

    // Set canvas width to viewport width
    const viewportWidth = window.innerWidth;
    canvas.width = viewportWidth;
    
    // Set initial canvas height (will be adjusted after rendering)
    const padding = 60;
    const maxWidth = viewportWidth - padding * 2;
    
    // Calculate estimated content height first (skip if recursive to avoid infinite loop)
    let estimatedHeight = window.innerHeight;
    if (!isRecursive && this.currentSlide) {
      // For large content, estimate height more conservatively
      if (this.currentSlide.content && this.currentSlide.content.length > 10000) {
        // Estimate: ~30px per line, max 500 lines = 15000px, plus title/UI components (~500px)
        // Cap at reasonable maximum to avoid excessive canvas heights
        estimatedHeight = Math.max(window.innerHeight, 25000); // Cap at 25000px initially
        console.log(`Large content detected (${this.currentSlide.content.length} chars), estimating height: ${estimatedHeight}px`);
      } else {
        const calculatedHeight = this.calculateSlideHeight(this.currentSlide, maxWidth, padding);
        estimatedHeight = Math.max(window.innerHeight, calculatedHeight);
      }
    } else if (isRecursive) {
      // In recursive call, use current canvas height (already set correctly)
      estimatedHeight = canvas.height;
      console.log(`Recursive render: using existing canvas height ${estimatedHeight}px`);
    }
    
    // Set canvas height to estimated height (will be adjusted if needed)
    // NOTE: Setting canvas.height clears the canvas, so we do this before clearing
    if (!isRecursive) {
      canvas.height = estimatedHeight;
      console.log(`Initial canvas height set to ${estimatedHeight}px`);
    } else {
      // In recursive call, canvas height is already set, don't change it
      console.log(`Recursive render: keeping canvas height at ${canvas.height}px`);
    }

    // Clear canvas
    ctx.fillStyle = '#0a0a0a';
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    if (!this.currentSlide && this.slides.length > 0) {
      this.currentSlide = this.slides[0];
    }

    if (!this.currentSlide) {
      // No slide to render
      ctx.fillStyle = '#f0f0f0';
      ctx.font = '24px Inter, sans-serif';
      ctx.textAlign = 'center';
      ctx.fillText('No slides loaded', canvas.width / 2, canvas.height / 2);
      return;
    }

    // Render slide content
    const slide = this.currentSlide;
    const startY = padding + 80;
    let y = startY;

    // Debug: log slide structure
    console.log('Rendering slide:', {
      id: slide.id,
      title: slide.title,
      dimension: slide.dimension,
      hasContent: !!slide.content,
      contentLength: slide.content ? slide.content.length : 0,
      hasText: !!slide.text,
      hasDescription: !!slide.description,
      populated: slide._populated,
      populatedBy: slide._populatedBy,
      uiComponents: slide.uiComponents ? slide.uiComponents.length : 0
    });
    
    // If slide has no content at all, show a message (but still render title)
    const hasAnyContent = slide.content || slide.text || slide.description || 
                         (slide.uiComponents && slide.uiComponents.length > 0);
    
    if (!hasAnyContent) {
      // Still render title even if no content
      const title = slide.title || slide.name || slide.id || 'Untitled Slide';
      ctx.fillStyle = '#00ffff';
      ctx.font = 'bold 48px Inter, sans-serif';
      ctx.textAlign = 'left';
      ctx.fillText(title, padding, y);
      y += 100;
      
      ctx.fillStyle = '#ffaa00';
      ctx.font = '20px Inter, sans-serif';
      ctx.textAlign = 'center';
      ctx.fillText('Slide loaded but no content available', canvas.width / 2, y + 50);
      ctx.fillStyle = '#888';
      ctx.font = '16px Inter, sans-serif';
      ctx.fillText(`Slide ID: ${slide.id || 'unnamed'}`, canvas.width / 2, y + 80);
      ctx.fillText(`Dimension: ${slide.dimension || 'none'}`, canvas.width / 2, y + 110);
      ctx.fillText('Click ⚡ to populate with agent content', canvas.width / 2, y + 150);
      return;
    }

    // Title (check multiple possible properties)
    const title = slide.title || slide.name || slide.id || 'Untitled Slide';
    
    // Show subtitle if available
    if (slide.subtitle) {
      ctx.fillStyle = '#00aaff';
      ctx.font = 'bold 36px Inter, sans-serif';
      ctx.textAlign = 'left';
      ctx.fillText(slide.subtitle, padding, y);
      y += 50;
    }
    ctx.fillStyle = '#00ffff';
    ctx.font = 'bold 48px Inter, sans-serif';
    ctx.textAlign = 'left';
    ctx.fillText(title, padding, y);
    y += 80;

    // Render UI components (images, diagrams, quotes) first
    if (slide.uiComponents && Array.isArray(slide.uiComponents)) {
      for (let i = 0; i < slide.uiComponents.length; i++) {
        const component = slide.uiComponents[i];
        if (component.type === 'image') {
          // Render image placeholder (images will be loaded asynchronously)
          const imgHeight = 200;
          const imgWidth = Math.min(maxWidth, 400);
          
          // Draw clickable image area
          const imageX = padding;
          const imageY = y;
          
          // Draw image placeholder
          ctx.fillStyle = '#333';
          ctx.fillRect(imageX, imageY, imgWidth, imgHeight);
          
          // Draw image border
          ctx.strokeStyle = '#00aaff';
          ctx.lineWidth = 2;
          ctx.strokeRect(imageX, imageY, imgWidth, imgHeight);
          
          // Draw image label
          ctx.fillStyle = '#00aaff';
          ctx.font = '14px Inter, sans-serif';
          ctx.fillText(component.alt || component.title || 'Image', imageX + 10, imageY + 20);
          
          // Store component click area for interaction layer
          if (!slide._componentAreas) slide._componentAreas = [];
          slide._componentAreas.push({
            type: 'image',
            component: component,
            index: i,
            x: imageX,
            y: imageY,
            width: imgWidth,
            height: imgHeight
          });
          
          // Load and draw actual image
          const img = new Image();
          img.crossOrigin = 'anonymous';
          img.onload = () => {
            // Redraw canvas with image
            const ctx2 = canvas.getContext('2d');
            const aspectRatio = img.width / img.height;
            const displayWidth = Math.min(imgWidth, maxWidth);
            const displayHeight = displayWidth / aspectRatio;
            ctx2.drawImage(img, imageX, imageY, displayWidth, displayHeight);
          };
          img.onerror = () => {
            // Draw error message
            const ctx2 = canvas.getContext('2d');
            ctx2.fillStyle = '#ff4444';
            ctx2.font = '14px Inter, sans-serif';
            ctx2.fillText('Failed to load image', imageX + 10, imageY + imgHeight / 2);
          };
          img.src = component.url;
          
          y += imgHeight + 20;
        } else if (component.type === 'diagram') {
          // Render diagram placeholder
          const diagramHeight = 250;
          const diagramWidth = maxWidth;
          const diagramX = padding;
          const diagramY = y;
          
          ctx.fillStyle = '#1a1a2e';
          ctx.fillRect(diagramX, diagramY, diagramWidth, diagramHeight);
          
          ctx.strokeStyle = '#00ffaa';
          ctx.lineWidth = 2;
          ctx.strokeRect(diagramX, diagramY, diagramWidth, diagramHeight);
          
          ctx.fillStyle = '#00ffaa';
          ctx.font = '16px Inter, sans-serif';
          ctx.fillText(`Diagram (${component.format.toUpperCase()})`, diagramX + 10, diagramY + 25);
          
          // Render diagram content as text (could be enhanced with actual diagram rendering)
          ctx.fillStyle = '#aaffaa';
          ctx.font = '12px monospace';
          const diagramLines = component.content.split('\n').slice(0, 10);
          let diagramTextY = diagramY + 50;
          for (const line of diagramLines) {
            if (diagramTextY > diagramY + diagramHeight - 20) break;
            ctx.fillText(line.substring(0, 80), diagramX + 10, diagramTextY);
            diagramTextY += 18;
          }
          
          // Store component click area
          if (!slide._componentAreas) slide._componentAreas = [];
          slide._componentAreas.push({
            type: 'diagram',
            component: component,
            index: i,
            x: diagramX,
            y: diagramY,
            width: diagramWidth,
            height: diagramHeight
          });
          
          y += diagramHeight + 20;
        } else if (component.type === 'quote') {
          // Render quote with special styling
          const quotePadding = 20;
          const quoteWidth = maxWidth - quotePadding * 2;
          const quoteX = padding + quotePadding;
          const quoteY = y;
          const quoteHeight = 100;
          
          // Quote background
          ctx.fillStyle = '#1a1a2e';
          ctx.fillRect(quoteX, quoteY, quoteWidth, quoteHeight);
          
          // Quote border (left side)
          ctx.fillStyle = '#00ffff';
          ctx.fillRect(quoteX, quoteY, 4, quoteHeight);
          
          // Store component click area
          if (!slide._componentAreas) slide._componentAreas = [];
          slide._componentAreas.push({
            type: 'quote',
            component: component,
            index: i,
            x: quoteX,
            y: quoteY,
            width: quoteWidth,
            height: quoteHeight
          });
          
          // Quote text
          ctx.fillStyle = '#ffffaa';
          ctx.font = 'italic 22px Inter, sans-serif';
          ctx.textAlign = 'left';
          
          const quoteText = `"${component.text}"`;
          const words = quoteText.split(' ');
          let quoteLine = '';
          let quoteTextY = quoteY + 35;
          
          for (const word of words) {
            const testLine = quoteLine + (quoteLine ? ' ' : '') + word;
            const metrics = ctx.measureText(testLine);
            if (metrics.width > quoteWidth - 20 && quoteLine) {
              ctx.fillText(quoteLine, quoteX + 15, quoteTextY);
              quoteTextY += 28;
              quoteLine = word;
            } else {
              quoteLine = testLine;
            }
          }
          if (quoteLine) {
            ctx.fillText(quoteLine, quoteX + 15, quoteTextY);
          }
          
          // Quote author if available
          let authorY = quoteTextY + 15;
          if (component.author) {
            authorY += 15;
            ctx.fillStyle = '#888';
            ctx.font = '16px Inter, sans-serif';
            ctx.fillText(`— ${component.author}`, quoteX + 15, authorY);
          }
          
          y += 120;
        }
      }
    }
    
    // Content (check multiple possible properties)
    const content = slide.content || slide.text || slide.description || '';
    console.log(`Rendering content for slide ${slide.id}:`, {
      hasContent: !!content,
      contentLength: content ? content.length : 0,
      hasUIComponents: slide.uiComponents && slide.uiComponents.length > 0,
      currentY: y,
      canvasHeight: canvas.height,
      viewportHeight: window.innerHeight
    });
    
    // Debug: Draw a visible marker at current Y position
    ctx.strokeStyle = '#00ff00';
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(0, y);
    ctx.lineTo(canvas.width, y);
    ctx.stroke();
    ctx.fillStyle = '#00ff00';
    ctx.font = '12px monospace';
    ctx.fillText(`Content starts here (y=${y})`, padding, y - 5);
    
    if (content) {
      ctx.fillStyle = '#f0f0f0';
      ctx.font = '20px Inter, sans-serif';
      ctx.textAlign = 'left';
      
      // Handle very large content (truncate for performance)
      const contentStr = String(content);
      const maxContentLength = 50000; // Limit to 50k chars for rendering performance
      let contentToRender = contentStr;
      let truncated = false;
      
      if (contentStr.length > maxContentLength) {
        contentToRender = contentStr.substring(0, maxContentLength) + '\n\n[... Content truncated for performance ...]';
        truncated = true;
        console.warn(`Content too long (${contentStr.length} chars), truncating to ${maxContentLength} chars`);
      }
      
      const lines = contentToRender.split('\n');
      let lineCount = 0;
      const maxLines = 500; // Limit lines for performance
      let renderedAnyContent = false;
      
      console.log(`Rendering ${lines.length} lines of content (max ${maxLines}) for slide ${slide.id}`);
      
      for (const line of lines) {
        if (lineCount >= maxLines) {
          ctx.fillStyle = '#ffaa00';
          ctx.font = '18px Inter, sans-serif';
          ctx.fillText(`[... ${lines.length - maxLines} more lines truncated ...]`, padding, y);
          y += 30;
          break;
        }
        
        if (line.trim()) {
          // Skip markdown images and blockquotes (already rendered as components)
          if (line.match(/^!\[.*\]\(.*\)/) || line.match(/^>\s+/)) {
            continue;
          }
          
          // Word wrap
          const words = line.split(' ');
          let currentLine = '';
          for (const word of words) {
            const testLine = currentLine + (currentLine ? ' ' : '') + word;
            const metrics = ctx.measureText(testLine);
            if (metrics.width > maxWidth && currentLine) {
              ctx.fillText(currentLine, padding, y);
              y += 30;
              lineCount++;
              renderedAnyContent = true;
              currentLine = word;
              
              // Check if we've exceeded max lines
              if (lineCount >= maxLines) break;
            } else {
              currentLine = testLine;
            }
          }
          if (currentLine && lineCount < maxLines) {
            ctx.fillText(currentLine, padding, y);
            y += 30;
            lineCount++;
            renderedAnyContent = true;
          }
        } else {
          y += 20; // Empty line
        }
        
        // Safety check - don't render beyond canvas height
        if (y > canvas.height - 100) {
          ctx.fillStyle = '#ffaa00';
          ctx.font = '18px Inter, sans-serif';
          ctx.fillText('[... Content continues but truncated for display ...]', padding, y);
          renderedAnyContent = true;
          break;
        }
      }
      
      if (!renderedAnyContent && contentToRender.trim()) {
        console.warn(`Content exists (${contentToRender.length} chars) but nothing was rendered for slide ${slide.id}`);
        // Try rendering first line as fallback
        const firstLine = lines.find(l => l.trim());
        if (firstLine) {
          ctx.fillText(firstLine.substring(0, 100) + '...', padding, y);
          renderedAnyContent = true;
        }
      }
      
      console.log(`Rendered ${lineCount} lines for slide ${slide.id}, renderedAnyContent: ${renderedAnyContent}, final Y: ${y}`);
      
      // Debug: Draw a visible marker at final Y position
      ctx.strokeStyle = '#ff00ff';
      ctx.lineWidth = 2;
      ctx.beginPath();
      ctx.moveTo(0, y);
      ctx.lineTo(canvas.width, y);
      ctx.stroke();
      ctx.fillStyle = '#ff00ff';
      ctx.font = '12px monospace';
      ctx.fillText(`Content ends here (y=${y})`, padding, y + 15);
      
      if (truncated) {
        ctx.fillStyle = '#ffaa00';
        ctx.font = '16px Inter, sans-serif';
        ctx.fillText(`Note: Content truncated (${contentStr.length} chars total)`, padding, y + 40);
        y += 30;
      }
    } else {
      console.warn(`No content to render for slide ${slide.id}`);
    }

    // Render slide ID and dimension if available
    if (slide.id || slide.dimension) {
      y += 20;
      ctx.fillStyle = '#888';
      ctx.font = '14px Inter, sans-serif';
      const info = [slide.id, slide.dimension].filter(Boolean).join(' • ');
      if (info) {
        ctx.fillText(info, padding, y);
      }
    }
    
    // Update canvas height if content exceeded initial height (only on first pass, not recursive)
    const finalY = y;
    if (!isRecursive && finalY > canvas.height - 50) {
      const newHeight = Math.max(canvas.height, finalY + 100);
      console.log(`Canvas height needs adjustment: current=${canvas.height}, needed=${newHeight}, finalY=${finalY}`);
      
      // IMPORTANT: Setting canvas.height clears the canvas and resets context!
      // Adjust canvas height first
      canvas.height = newHeight;
      
      // Re-render everything from scratch with correct height
      // This is a recursive call to ensure proper rendering
      console.log(`Re-rendering slide ${slide.id} with adjusted height ${newHeight}px`);
      this._renderToCanvasInternal(canvas, true);
      return; // Exit early since we've re-rendered
    }
    
    console.log(`Final render complete: slide=${slide.id}, finalY=${y}, canvasHeight=${canvas.height}`);
  }

  /**
   * Calculate slide height for scrolling support
   * @param {Object} slide - Slide object
   * @param {number} maxWidth - Maximum content width
   * @param {number} padding - Padding value
   * @returns {number} Calculated height
   */
  calculateSlideHeight(slide, maxWidth, padding) {
    let height = padding + 80; // Start Y
    
    // Title height
    height += 80;
    if (slide.subtitle) height += 50;
    
    // UI components height
    if (slide.uiComponents && Array.isArray(slide.uiComponents)) {
      for (const component of slide.uiComponents) {
        if (component.type === 'image') {
          height += 200 + 20; // Image height + spacing
        } else if (component.type === 'diagram') {
          height += 250 + 20; // Diagram height + spacing
        } else if (component.type === 'quote') {
          height += 120; // Quote height
        }
      }
    }
    
    // Content height (estimate)
    const content = slide.content || slide.text || slide.description || '';
    if (content) {
      const contentStr = String(content);
      const lines = contentStr.split('\n');
      
      // Cap at 500 lines (same as rendering limit) to avoid excessive height calculations
      const maxLinesToEstimate = 500;
      const linesToProcess = lines.slice(0, maxLinesToEstimate);
      const isTruncated = lines.length > maxLinesToEstimate;
      
      for (const line of linesToProcess) {
        if (line.trim() && !line.match(/^!\[.*\]\(.*\)/) && !line.match(/^>\s+/)) {
          // Estimate line height based on word wrapping
          const words = line.split(' ');
          let lineCount = 1;
          let currentWidth = 0;
          const ctx = document.createElement('canvas').getContext('2d');
          ctx.font = '20px Inter, sans-serif';
          
          for (const word of words) {
            const wordWidth = ctx.measureText(word + ' ').width;
            if (currentWidth + wordWidth > maxWidth && currentWidth > 0) {
              lineCount++;
              currentWidth = wordWidth;
            } else {
              currentWidth += wordWidth;
            }
          }
          height += lineCount * 30;
        } else {
          height += 20; // Empty line or skipped line
        }
      }
      
      // Add note about truncation if content was truncated
      if (isTruncated) {
        height += 50; // Space for truncation message
      }
    }
    
    // Footer height
    height += 40;
    
    return height;
  }

  /**
   * Go to next slide
   */
  nextSlide() {
    if (this.slides.length === 0) {
      console.warn('No slides available');
      return null;
    }
    
    const currentIndex = this.slides.findIndex(s => 
      s.id === this.currentSlide?.id || s === this.currentSlide
    );
    
    if (currentIndex < 0) {
      // Current slide not found, go to first slide
      console.warn('Current slide not found in slides array, going to first slide');
      this.currentSlide = this.slides[0];
      return this.currentSlide;
    }
    
    const nextIndex = (currentIndex + 1) % this.slides.length;
    this.currentSlide = this.slides[nextIndex];
    
    console.log(`Next slide: ${this.currentSlide.id} (${this.currentSlide.dimension || 'no dimension'})`);
    return this.currentSlide;
  }

  /**
   * Go to previous slide
   */
  prevSlide() {
    if (this.slides.length === 0) {
      console.warn('No slides available');
      return null;
    }
    
    const currentIndex = this.slides.findIndex(s => 
      s.id === this.currentSlide?.id || s === this.currentSlide
    );
    
    if (currentIndex < 0) {
      // Current slide not found, go to last slide
      console.warn('Current slide not found in slides array, going to last slide');
      this.currentSlide = this.slides[this.slides.length - 1];
      return this.currentSlide;
    }
    
    const prevIndex = (currentIndex - 1 + this.slides.length) % this.slides.length;
    this.currentSlide = this.slides[prevIndex];
    
    console.log(`Previous slide: ${this.currentSlide.id} (${this.currentSlide.dimension || 'no dimension'})`);
    return this.currentSlide;
  }
  
  /**
   * Go to slide by index
   */
  goToSlide(index) {
    if (index < 0 || index >= this.slides.length) {
      console.warn(`Invalid slide index: ${index}, total slides: ${this.slides.length}`);
      return null;
    }
    this.currentSlide = this.slides[index];
    console.log(`Went to slide ${index}: ${this.currentSlide.id}`);
    return this.currentSlide;
  }
  
  /**
   * Go to slide by ID
   */
  goToSlideById(id) {
    const slide = this.slides.find(s => s.id === id);
    if (!slide) {
      console.warn(`Slide not found: ${id}`);
      return null;
    }
    this.currentSlide = slide;
    console.log(`Went to slide: ${id}`);
    return this.currentSlide;
  }

  /**
   * Get current slide index
   */
  getCurrentSlideIndex() {
    return this.slides.indexOf(this.currentSlide) + 1;
  }

  /**
   * Populate all slides with agent content (automatic)
   * @param {boolean} force - Force re-population even if already populated
   * @returns {Promise<Array>} Populated slides
   */
  async populateSlides(force = false) {
    if (!this.agentCoordinator) {
      console.warn('AgentCoordinator not initialized');
      return this.slides;
    }

    try {
      // Ensure coordinator is initialized
      if (!this.agentCoordinator.initialized) {
        await this.agentCoordinator.init();
      }

      // Populate all slides (re-populate if force is true or if slides aren't populated)
      const needsPopulation = force || this.slides.some(s => !s._populated);
      if (needsPopulation) {
        this.slides = await this.agentCoordinator.populateAll(this.slides);
        
        // Update current slide if it exists
        if (this.currentSlide) {
          const currentIndex = this.slides.findIndex(s => 
            s.id === this.currentSlide.id || s === this.currentSlide
          );
          if (currentIndex >= 0) {
            this.currentSlide = this.slides[currentIndex];
          }
        }

        console.log(`Populated ${this.slides.length} slides with agent content`);
      }
      
      return this.slides;
    } catch (error) {
      console.error('Failed to populate slides:', error);
      return this.slides; // Return unmodified slides on error
    }
  }

  /**
   * Start infinite resolving - continuously evolve slides as content becomes available
   * @param {number} intervalMs - Resolve interval in milliseconds (default: 5000)
   * @deprecated Use background population instead - this is disabled for performance
   */
  startInfiniteResolving(intervalMs = 5000) {
    // Disabled - agents work in background, suggestions only on query/keyword click
    console.log('Infinite resolving disabled - use background population instead');
  }

  /**
   * Start auto-revolving slides (automatically advance to next slide)
   * @param {number} intervalMs - Revolve interval in milliseconds (default: 10000)
   * @deprecated Disabled - slides only change on user request
   */
  startAutoRevolving(intervalMs = 10000) {
    // Disabled - slides only change when user clicks prev/next buttons
    console.log('Auto-revolving disabled - slides only change on user request');
  }

  /**
   * Stop auto-revolving slides
   */
  stopAutoRevolving() {
    if (this.revolveInterval) {
      clearInterval(this.revolveInterval);
      this.revolveInterval = null;
      console.log('Stopped auto-revolving slides');
    }
  }

  /**
   * Stop infinite resolving
   */
  stopInfiniteResolving() {
    if (this.resolveInterval) {
      clearInterval(this.resolveInterval);
      this.resolveInterval = null;
      console.log('Stopped infinite resolving');
    }
  }

  /**
   * Populate a single slide on-demand
   * @param {string|Object} slideIdOrSlide - Slide ID or slide object
   * @param {boolean} force - Force re-population even if already populated
   * @returns {Promise<Object|null>} Populated slide or null if not found
   */
  async populateSlide(slideIdOrSlide, force = false) {
    if (!this.agentCoordinator) {
      console.warn('AgentCoordinator not initialized');
      return null;
    }

    try {
      // Ensure coordinator is initialized
      if (!this.agentCoordinator.initialized) {
        await this.agentCoordinator.init();
      }
      
      // Find slide if ID provided
      let slide = null;
      if (typeof slideIdOrSlide === 'string') {
        slide = this.slides.find(s => s.id === slideIdOrSlide);
      } else {
        slide = slideIdOrSlide;
      }
      
      if (!slide) {
        console.warn('Slide not found:', slideIdOrSlide);
        return null;
      }
      
      // Check if already populated (unless force)
      if (!force && slide._populated) {
        return slide;
      }
      
      // Populate slide via agent coordinator
      const populatedSlide = await this.agentCoordinator.populateSlide(slide);
      
      // Update slide in slides array
      const slideIndex = this.slides.findIndex(s => s.id === populatedSlide.id);
      if (slideIndex >= 0) {
        this.slides[slideIndex] = populatedSlide;
      }
      
      // Update current slide if it's the one being populated
      if (this.currentSlide && (this.currentSlide.id === populatedSlide.id || this.currentSlide === slide)) {
        this.currentSlide = populatedSlide;
      }
      
      return populatedSlide;
    } catch (error) {
      console.error('Failed to populate slide:', error);
      return null;
    }
  }

  /**
   * Get plugin by name
   * @param {string} name - Plugin name
   * @returns {BasePlugin|null} Plugin instance
   */
  getPlugin(name) {
    return this.plugins.get(name) || null;
  }

  /**
   * Get all registered plugins
   * @returns {Array} Plugin manifests
   */
  getPlugins() {
    return Array.from(this.plugins.values()).map(p => p.getManifest());
  }
}
