/**
 * BasePlugin - Abstract base class for all CanvasL Semantic Slides plugins
 * 
 * Provides hooks for Meta-Log integration, rendering, and evolution.
 * All plugins extend this class to integrate with the projector system.
 */

export class BasePlugin {
  constructor(config = {}) {
    this.config = config;
    this.name = config.name || 'UnnamedPlugin';
    this.version = config.version || '0.1.0';
    this.hooks = config.hooks || [];
    this.metaLog = null; // Will be injected by Projector
    this.initialized = false;
  }

  /**
   * Initialize the plugin
   * Called by Projector after MetaLogBridge is available
   */
  async init(metaLogBridge) {
    if (this.initialized) {
      throw new Error(`Plugin ${this.name} already initialized`);
    }
    
    this.metaLog = metaLogBridge;
    this.initialized = true;
    
    // Call plugin-specific initialization
    if (typeof this.onInit === 'function') {
      await this.onInit();
    }
  }

  /**
   * Render data using the plugin
   * @param {Object} data - Data to render (from CanvasL execution)
   * @param {string} mode - Rendering mode: 'static' | 'offscreen' | 'animated'
   * @returns {Promise<Object>} Rendering result
   */
  async render(data, mode = 'static') {
    if (!this.initialized) {
      throw new Error(`Plugin ${this.name} not initialized`);
    }
    
    // Default implementation: return data as-is
    // Plugins should override this method
    return data;
  }

  /**
   * Evolve/modify slide based on interactions
   * @param {Object} slide - Slide definition
   * @returns {Promise<Object>} Evolved slide
   */
  async evolve(slide) {
    if (!this.initialized) {
      throw new Error(`Plugin ${this.name} not initialized`);
    }
    
    // Default implementation: return slide unchanged
    // Plugins can override to implement self-modification
    return slide;
  }

  /**
   * Handle Meta-Log hook calls
   * @param {string} type - Hook type (e.g., 'r5rs', 'prolog', 'datalog', 'sparql')
   * @param {*} args - Hook arguments
   * @returns {Promise<*>} Hook result
   */
  async hook(type, args) {
    if (!this.initialized || !this.metaLog) {
      throw new Error(`Plugin ${this.name} Meta-Log not available`);
    }
    
    switch (type) {
      case 'r5rs':
        return await this.metaLog.evalR5RS(args.expression, args.context);
      
      case 'prolog':
        return await this.metaLog.prologQuery(args.query, args.facts);
      
      case 'datalog':
        return await this.metaLog.datalogQuery(args.program, args.goal);
      
      case 'sparql':
        return await this.metaLog.sparqlQuery(args.query, args.endpoint);
      
      default:
        throw new Error(`Unknown hook type: ${type}`);
    }
  }

  /**
   * Validate plugin configuration
   * @returns {boolean} True if valid
   */
  validate() {
    if (!this.name) {
      throw new Error('Plugin must have a name');
    }
    return true;
  }

  /**
   * Get plugin manifest
   * @returns {Object} Plugin manifest
   */
  getManifest() {
    return {
      name: this.name,
      version: this.version,
      hooks: this.hooks,
      config: this.config
    };
  }
}
