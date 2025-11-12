/**
 * Projector - Core engine for CanvasL Semantic Slides
 * 
 * Loads plugins, parses CanvasL files, executes Meta-Log operations,
 * and coordinates rendering of semantic slides.
 */

import { BasePlugin } from './BasePlugin';
import { MetaLogBridge } from './MetaLogBridge';
import { MacroExpander } from './MacroExpander';
import { IncludeLoader } from './IncludeLoader';
import { CanvasLExecutor, ExecutionResult } from './CanvasLExecutor';
import { ErrorHandler } from './ErrorHandler';
import { TopicSlideGenerator } from './TopicSlideGenerator';

export interface ProjectorConfig {
  name?: string;
  version?: string;
  hooks?: string[];
  kernelUrl?: string;
  contentIndexUrl?: string;
  [key: string]: any;
}

export interface Slide {
  id?: string;
  type?: string;
  title?: string;
  dimension?: string;
  description?: string;
  content?: string;
  text?: string;
  subtitle?: string;
  uiComponents?: any[];
  _populated?: boolean;
  _populatedBy?: string;
  _componentAreas?: any[];
  [key: string]: any;
}

export class Projector extends BasePlugin {
  private plugins: Map<string, BasePlugin>;
  private metaLog: MetaLogBridge;
  private macroExpander: MacroExpander;
  private includeLoader: IncludeLoader;
  private executor: CanvasLExecutor;
  private errorHandler: ErrorHandler;
  private slides: Slide[];
  private currentSlide: Slide | null;
  private agentCoordinator: any; // AgentCoordinator - will be imported later
  private revolveInterval: NodeJS.Timeout | null;
  private resolveInterval: NodeJS.Timeout | null;

  constructor(config: ProjectorConfig = {}) {
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
    this.revolveInterval = null;
    this.resolveInterval = null;
    
    // Initialize agent coordinator for slide population
    // Will be set up after agent system migration
    this.agentCoordinator = null;
    
    // Set error handler for federation
    this.metaLog.setErrorHandler(this.errorHandler);
    
    // Register default recovery strategies
    this.setupErrorRecovery();
  }

  /**
   * Setup error recovery strategies
   */
  private setupErrorRecovery(): void {
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
  async onInit(): Promise<void> {
    try {
      // Initialize Meta-Log bridge (includes meta-log-db initialization)
      await this.metaLog.init();
      
      // Initialize R5RS fallback
      await this.metaLog.evalR5RS('(define projector-version "0.1.0")');
      
      // Initialize agent coordinator (will be set up after migration)
      if (this.agentCoordinator) {
        await this.agentCoordinator.init();
      }
      
      // Load built-in plugins
      await this.loadBuiltInPlugins();
    } catch (error: any) {
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
  private async loadBuiltInPlugins(): Promise<void> {
    // Load DBpedia plugin
    try {
      // Dynamic import will be handled when plugin system is migrated
      // const { DBpediaPlugin } = await import('../plugin/dbpedia-plugin.js');
      // const dbpediaPlugin = new DBpediaPlugin();
      // await this.registerPlugin(dbpediaPlugin);
    } catch (error) {
      console.warn('Failed to load DBpedia plugin:', error);
    }
  }

  /**
   * Register a plugin
   */
  async registerPlugin(plugin: BasePlugin): Promise<void> {
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
   */
  async loadPlugin(pluginPath: string): Promise<BasePlugin> {
    try {
      const module = await import(pluginPath);
      const PluginClass = module.default || module[Object.keys(module)[0]];
      
      if (!PluginClass) {
        throw new Error(`No default export found in ${pluginPath}`);
      }
      
      const plugin = new PluginClass();
      await this.registerPlugin(plugin);
      
      return plugin;
    } catch (error: any) {
      throw new Error(`Failed to load plugin ${pluginPath}: ${error.message}`);
    }
  }

  /**
   * Parse CanvasL file
   */
  async parseCanvasL(content: string, sourceUrl: string = ''): Promise<any[]> {
    // Set base path for include resolution
    if (sourceUrl) {
      const basePath = sourceUrl.substring(0, sourceUrl.lastIndexOf('/') + 1);
      this.includeLoader.setBasePath(basePath);
    }
    
    // Parse content (handles @include directives)
    const objects = this.includeLoader.parseCanvasL(content, sourceUrl);
    
    // Expand @include directives (this loads included files)
    const expanded = await this.includeLoader.expandIncludes(objects, sourceUrl);
    
    const slidesFound = expanded.filter((o: any) => o.type === 'slide');
    console.log(`Parsed ${expanded.length} objects from ${sourceUrl}`);
    console.log(`Slides found: ${slidesFound.length}`, slidesFound.map((s: any) => ({ id: s.id, dimension: s.dimension || 'none' })));
    
    // Check for include errors
    if (this.includeLoader.loadErrors && this.includeLoader.loadErrors.length > 0) {
      console.warn(`Include errors in ${sourceUrl}:`, this.includeLoader.loadErrors);
    }
    
    return expanded;
  }

  /**
   * Expand CanvasL macros
   */
  async expandMacros(objects: any[]): Promise<any[]> {
    // Load macros from objects
    this.macroExpander.loadMacros(objects);
    
    // Expand all macros
    return this.macroExpander.expandAll(objects);
  }

  /**
   * Execute CanvasL objects
   */
  async execute(objects: any[]): Promise<ExecutionResult> {
    return await this.executor.executeAll(objects);
  }

  /**
   * Load and render a deck
   */
  async loadDeck(deckPath: string): Promise<ExecutionResult> {
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
      
      return result;
    } catch (error: any) {
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
   */
  async renderSlide(slide: Slide, mode: string = 'static'): Promise<Slide> {
    this.currentSlide = slide;
    
    // Find appropriate plugin for rendering
    const pluginName = (slide as any).plugin || 'default';
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
   */
  renderToCanvas(canvas: HTMLCanvasElement): void {
    if (!canvas) {
      console.error('Canvas element not found');
      return;
    }
    
    try {
      this._renderToCanvasInternal(canvas);
    } catch (error: any) {
      console.error('Error rendering slide to canvas:', error);
      // Render error message
      const ctx = canvas.getContext('2d');
      if (ctx) {
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
  }
  
  /**
   * Internal rendering method
   */
  private _renderToCanvasInternal(canvas: HTMLCanvasElement, isRecursive: boolean = false): void {
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

    // Title (check multiple possible properties)
    const title = slide.title || (slide as any).name || slide.id || 'Untitled Slide';
    
    // Show subtitle if available
    if ((slide as any).subtitle) {
      ctx.fillStyle = '#00aaff';
      ctx.font = 'bold 36px Inter, sans-serif';
      ctx.textAlign = 'left';
      ctx.fillText((slide as any).subtitle, padding, y);
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
          }
        } else {
          y += 20; // Empty line
        }
        
        // Safety check - don't render beyond canvas height
        if (y > canvas.height - 100) {
          ctx.fillStyle = '#ffaa00';
          ctx.font = '18px Inter, sans-serif';
          ctx.fillText('[... Content continues but truncated for display ...]', padding, y);
          break;
        }
      }
      
      if (truncated) {
        ctx.fillStyle = '#ffaa00';
        ctx.font = '16px Inter, sans-serif';
        ctx.fillText(`Note: Content truncated (${contentStr.length} chars total)`, padding, y + 40);
        y += 30;
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
   */
  private calculateSlideHeight(slide: Slide, maxWidth: number, padding: number): number {
    let height = padding + 80; // Start Y
    
    // Title height
    height += 80;
    if ((slide as any).subtitle) height += 50;
    
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
      
      for (const line of linesToProcess) {
        if (line.trim() && !line.match(/^!\[.*\]\(.*\)/) && !line.match(/^>\s+/)) {
          // Estimate line height based on word wrapping
          const words = line.split(' ');
          let lineCount = 1;
          let currentWidth = 0;
          const ctx = document.createElement('canvas').getContext('2d');
          if (ctx) {
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
          }
        } else {
          height += 20; // Empty line or skipped line
        }
      }
    }
    
    // Footer height
    height += 40;
    
    return height;
  }

  /**
   * Go to next slide
   */
  nextSlide(): Slide | null {
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
  prevSlide(): Slide | null {
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
  goToSlide(index: number): Slide | null {
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
  goToSlideById(id: string): Slide | null {
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
  getCurrentSlideIndex(): number {
    return this.slides.indexOf(this.currentSlide!) + 1;
  }

  /**
   * Populate all slides with agent content (automatic)
   */
  async populateSlides(force: boolean = false): Promise<Slide[]> {
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
            s.id === this.currentSlide!.id || s === this.currentSlide
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
   * Populate a single slide on-demand
   */
  async populateSlide(slideIdOrSlide: string | Slide, force: boolean = false): Promise<Slide | null> {
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
      let slide: Slide | null = null;
      if (typeof slideIdOrSlide === 'string') {
        slide = this.slides.find(s => s.id === slideIdOrSlide) || null;
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
   */
  getPlugin(name: string): BasePlugin | null {
    return this.plugins.get(name) || null;
  }

  /**
   * Get all registered plugins
   */
  getPlugins(): PluginConfig[] {
    return Array.from(this.plugins.values()).map(p => p.getManifest());
  }

  /**
   * Set agent coordinator (called after agent system migration)
   */
  setAgentCoordinator(coordinator: any): void {
    this.agentCoordinator = coordinator;
  }

  /**
   * Get slides
   */
  getSlides(): Slide[] {
    return this.slides;
  }

  /**
   * Get current slide
   */
  getCurrentSlide(): Slide | null {
    return this.currentSlide;
  }
}

