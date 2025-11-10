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
    
    // Expand @include directives
    const expanded = await this.includeLoader.expandIncludes(objects, sourceUrl);
    
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
