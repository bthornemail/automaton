/**
 * BasePlugin - Abstract base class for all CanvasL Semantic Slides plugins
 * 
 * Provides hooks for Meta-Log integration, rendering, and evolution.
 * All plugins extend this class to integrate with the projector system.
 */

import { MetaLogBridge } from './MetaLogBridge';

export interface PluginConfig {
  name?: string;
  version?: string;
  hooks?: string[];
  [key: string]: any;
}

export class BasePlugin {
  protected config: PluginConfig;
  protected name: string;
  protected version: string;
  protected hooks: string[];
  protected metaLog: MetaLogBridge | null;
  protected initialized: boolean;

  constructor(config: PluginConfig = {}) {
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
  async init(metaLogBridge: MetaLogBridge): Promise<void> {
    if (this.initialized) {
      throw new Error(`Plugin ${this.name} already initialized`);
    }
    
    this.metaLog = metaLogBridge;
    this.initialized = true;
    
    // Call plugin-specific initialization
    if (typeof (this as any).onInit === 'function') {
      await (this as any).onInit();
    }
  }

  /**
   * Render data using the plugin
   */
  async render(data: any, mode: string = 'static'): Promise<any> {
    if (!this.initialized) {
      throw new Error(`Plugin ${this.name} not initialized`);
    }
    
    // Default implementation: return data as-is
    // Plugins should override this method
    return data;
  }

  /**
   * Evolve/modify slide based on interactions
   */
  async evolve(slide: any): Promise<any> {
    if (!this.initialized) {
      throw new Error(`Plugin ${this.name} not initialized`);
    }
    
    // Default implementation: return slide unchanged
    // Plugins can override to implement self-modification
    return slide;
  }

  /**
   * Handle Meta-Log hook calls
   */
  async hook(type: string, args: any): Promise<any> {
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
   */
  validate(): boolean {
    if (!this.name) {
      throw new Error('Plugin must have a name');
    }
    return true;
  }

  /**
   * Get plugin manifest
   */
  getManifest(): PluginConfig {
    return {
      name: this.name,
      version: this.version,
      hooks: this.hooks,
      config: this.config
    };
  }
}

