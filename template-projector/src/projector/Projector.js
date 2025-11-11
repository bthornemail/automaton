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
    
    console.log(`Parsed ${expanded.length} objects from ${sourceUrl}`);
    console.log('Slides found:', expanded.filter(o => o.type === 'slide').map(s => s.id || 'unnamed'));
    
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
      
      // Expand macros
      const expanded = await this.expandMacros(objects);
      
      // Execute
      const result = await this.execute(expanded);
      
      this.slides = result.slides;
      
      // Automatically populate slides with agent content
      if (this.slides.length > 0) {
        await this.populateSlides();
      }
      
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
   * Render current slide to canvas
   * @param {HTMLCanvasElement} canvas - Canvas element
   */
  renderToCanvas(canvas) {
    if (!canvas) {
      console.error('Canvas element not found');
      return;
    }

    const ctx = canvas.getContext('2d');
    if (!ctx) {
      console.error('Could not get 2D context');
      return;
    }

    // Set canvas size
    canvas.width = canvas.offsetWidth;
    canvas.height = canvas.offsetHeight;

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
    const padding = 60;
    const maxWidth = canvas.width - padding * 2;
    const startY = padding + 80;
    let y = startY;

    // Debug: log slide structure
    console.log('Rendering slide:', {
      id: slide.id,
      title: slide.title,
      hasContent: !!slide.content,
      contentLength: slide.content ? slide.content.length : 0,
      populated: slide._populated,
      populatedBy: slide._populatedBy
    });

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

    // Content (check multiple possible properties)
    const content = slide.content || slide.text || slide.description || '';
    if (content) {
      ctx.fillStyle = '#f0f0f0';
      ctx.font = '20px Inter, sans-serif';
      ctx.textAlign = 'left';
      
      const lines = String(content).split('\n');
      for (const line of lines) {
        if (line.trim()) {
          // Word wrap
          const words = line.split(' ');
          let currentLine = '';
          for (const word of words) {
            const testLine = currentLine + (currentLine ? ' ' : '') + word;
            const metrics = ctx.measureText(testLine);
            if (metrics.width > maxWidth && currentLine) {
              ctx.fillText(currentLine, padding, y);
              y += 30;
              currentLine = word;
            } else {
              currentLine = testLine;
            }
          }
          if (currentLine) {
            ctx.fillText(currentLine, padding, y);
            y += 30;
          }
        } else {
          y += 20; // Empty line
        }
      }
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
  }

  /**
   * Go to next slide
   */
  nextSlide() {
    if (this.slides.length === 0) return;
    const currentIndex = this.slides.indexOf(this.currentSlide);
    const nextIndex = (currentIndex + 1) % this.slides.length;
    this.currentSlide = this.slides[nextIndex];
    return this.currentSlide;
  }

  /**
   * Go to previous slide
   */
  prevSlide() {
    if (this.slides.length === 0) return;
    const currentIndex = this.slides.indexOf(this.currentSlide);
    const prevIndex = (currentIndex - 1 + this.slides.length) % this.slides.length;
    this.currentSlide = this.slides[prevIndex];
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
   * @returns {Promise<Array>} Populated slides
   */
  async populateSlides() {
    if (!this.agentCoordinator) {
      console.warn('AgentCoordinator not initialized');
      return this.slides;
    }

    try {
      // Ensure coordinator is initialized
      if (!this.agentCoordinator.initialized) {
        await this.agentCoordinator.init();
      }

      // Populate all slides
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
      return this.slides;
    } catch (error) {
      console.error('Failed to populate slides:', error);
      return this.slides; // Return unmodified slides on error
    }
  }

  /**
   * Populate a single slide on-demand
   * @param {string|Object} slideIdOrSlide - Slide ID or slide object
   * @returns {Promise<Object|null>} Populated slide or null if not found
   */
  async populateSlide(slideIdOrSlide) {
    if (!this.agentCoordinator) {
      console.warn('AgentCoordinator not initialized');
      return null;
    }

    try {
      // Ensure coordinator is initialized
      if (!this.agentCoordinator.initialized) {
        await this.agentCoordinator.init();
      }

      // Find slide
      let slide = null;
      if (typeof slideIdOrSlide === 'string') {
        slide = this.slides.find(s => s.id === slideIdOrSlide);
      } else {
        slide = slideIdOrSlide;
      }

      if (!slide) {
        console.warn(`Slide not found: ${slideIdOrSlide}`);
        return null;
      }

      // Populate slide
      const populated = await this.agentCoordinator.populateSlide(slide);
      
      // Update in slides array
      const index = this.slides.findIndex(s => 
        s.id === slide.id || s === slide
      );
      if (index >= 0) {
        this.slides[index] = populated;
        
        // Update current slide if it's the one being populated
        if (this.currentSlide === slide || this.currentSlide?.id === slide.id) {
          this.currentSlide = populated;
        }
      }

      return populated;
    } catch (error) {
      console.error(`Failed to populate slide ${slideIdOrSlide}:`, error);
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
