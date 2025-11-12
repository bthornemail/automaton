/**
 * AgentCoordinator - Routes slide population requests to appropriate dimensional agents
 * 
 * Coordinates multi-agent operations and caches kernel data for performance
 */

import { ContentLoader } from './ContentLoader';
import { DimensionalAgent, Slide } from './DimensionalAgent';

export class AgentCoordinator {
  private contentLoader: ContentLoader;
  private agents: Map<string, DimensionalAgent>;
  private contentData: any[] | null;
  public initialized: boolean;

  constructor(kernelUrl: string = '/automaton-kernel.jsonl', contentIndexUrl: string = '/content-index.jsonl') {
    this.contentLoader = new ContentLoader(kernelUrl, contentIndexUrl);
    this.agents = new Map();
    this.contentData = null;
    this.initialized = false;
  }

  /**
   * Initialize coordinator and load kernel
   */
  async init(): Promise<void> {
    if (this.initialized) {
      return;
    }

    try {
      // Load all content sources (kernel + frontmatter)
      await this.contentLoader.loadAll();
      this.contentData = this.contentLoader.getAllEntries();
      
      // Initialize specific dimensional agents (0D-7D)
      // For now, use generic DimensionalAgent for all dimensions
      // Individual agent classes can be added later
      for (let dim = 0; dim <= 7; dim++) {
        const dimension = `${dim}D`;
        const agent = new DimensionalAgent(dimension, this.contentLoader);
        this.agents.set(dimension, agent);
      }

      this.initialized = true;
      console.log(`AgentCoordinator initialized with ${this.agents.size} agents`);
    } catch (error) {
      console.error('Failed to initialize AgentCoordinator:', error);
      throw error;
    }
  }

  /**
   * Get agent for a dimension
   */
  getAgent(dimension: string): DimensionalAgent | null {
    if (!this.initialized) {
      throw new Error('AgentCoordinator not initialized. Call init() first.');
    }

    if (!dimension) {
      console.warn('getAgent called with empty dimension, defaulting to 0D');
      dimension = '0D';
    }

    // Normalize dimension
    let dim = String(dimension).trim().toUpperCase();
    dim = dim.replace(/D+$/i, '') + 'D';
    
    // Ensure it's a valid dimension format (0D-7D)
    if (!/^[0-7]D$/.test(dim)) {
      console.warn(`Invalid dimension format: ${dimension}, defaulting to 0D`);
      dim = '0D';
    }
    
    const agent = this.agents.get(dim);
    if (!agent) {
      console.warn(`No agent found for dimension ${dim}, available agents:`, Array.from(this.agents.keys()));
    }
    
    return agent || null;
  }

  /**
   * Populate a single slide
   */
  async populateSlide(slide: Slide): Promise<Slide> {
    if (!this.initialized) {
      await this.init();
    }

    // Get slide dimension (default to "0D" if not specified)
    let dimension = slide.dimension || '0D';
    
    // Normalize dimension
    dimension = String(dimension).trim().toUpperCase().replace(/D+$/i, '') + 'D';
    
    console.log(`[AgentCoordinator] Populating slide ${slide.id || 'unnamed'} with dimension ${dimension}`);
    
    // Get appropriate agent
    const agent = this.getAgent(dimension);
    if (!agent) {
      console.error(`No agent found for dimension ${dimension}, slide: ${slide.id}`);
      
      // Try to use 0D agent as fallback
      const defaultAgent = this.getAgent('0D');
      if (!defaultAgent) {
        console.error('No default 0D agent available, returning unmodified slide');
        return slide;
      }
      console.warn(`Using 0D agent as fallback for slide ${slide.id}`);
      return await defaultAgent.populateSlide(slide, this.contentData);
    }

    console.log(`[AgentCoordinator] Using agent: ${agent.name} for slide ${slide.id}`);
    
    // Populate slide using agent
    const populatedSlide = await agent.populateSlide(slide, this.contentData);
    
    console.log(`[AgentCoordinator] Slide ${slide.id} populated by ${agent.name}, has content: ${!!populatedSlide.content}`);
    
    return populatedSlide;
  }

  /**
   * Populate all slides in a deck
   */
  async populateAll(slides: Slide[]): Promise<Slide[]> {
    if (!this.initialized) {
      await this.init();
    }

    if (!Array.isArray(slides) || slides.length === 0) {
      return slides;
    }

    console.log(`Populating ${slides.length} slides with agents...`);

    // Group slides by dimension for batch processing
    const slidesByDimension = new Map<string, Slide[]>();
    for (const slide of slides) {
      const dimension = slide.dimension || '0D';
      if (!slidesByDimension.has(dimension)) {
        slidesByDimension.set(dimension, []);
      }
      slidesByDimension.get(dimension)!.push(slide);
    }

    // Populate slides by dimension
    const populatedSlides: Slide[] = [];
    for (const [dimension, dimensionSlides] of slidesByDimension) {
      const agent = this.getAgent(dimension);
      if (!agent) {
        console.warn(`No agent for dimension ${dimension}, skipping ${dimensionSlides.length} slides`);
        populatedSlides.push(...dimensionSlides);
        continue;
      }

      // Populate each slide in this dimension
      for (const slide of dimensionSlides) {
        try {
          const populated = await agent.populateSlide(slide, this.contentData);
          populatedSlides.push(populated);
        } catch (error) {
          console.error(`Failed to populate slide ${slide.id || 'unnamed'}:`, error);
          populatedSlides.push(slide); // Add unmodified slide on error
        }
      }
    }

    console.log(`Populated ${populatedSlides.length} slides`);
    return populatedSlides;
  }

  /**
   * Get population status
   */
  getPopulationStatus(slides: Slide[]): { total: number; populated: number; unpopulated: number; percentage: string } {
    if (!Array.isArray(slides)) {
      return { total: 0, populated: 0, unpopulated: 0, percentage: '0' };
    }

    const populated = slides.filter(s => s._populated === true).length;
    const unpopulated = slides.length - populated;

    return {
      total: slides.length,
      populated,
      unpopulated,
      percentage: slides.length > 0 ? (populated / slides.length * 100).toFixed(1) : '0'
    };
  }

  /**
   * Clear cache and reload all content
   */
  async reloadContent(): Promise<void> {
    await this.contentLoader.reload();
    this.contentData = this.contentLoader.getAllEntries();
    this.initialized = false;
    await this.init();
  }

  /**
   * Set kernel URL
   */
  setKernelUrl(url: string): void {
    (this.contentLoader as any).kernelLoader.setKernelUrl(url);
    this.initialized = false; // Force re-initialization
  }

  /**
   * Set content index URL
   */
  setContentIndexUrl(url: string): void {
    (this.contentLoader as any).frontmatterLoader.setContentIndexUrl(url);
    this.initialized = false; // Force re-initialization
  }
}

