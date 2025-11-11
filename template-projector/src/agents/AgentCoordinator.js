/**
 * AgentCoordinator - Routes slide population requests to appropriate dimensional agents
 * 
 * Coordinates multi-agent operations and caches kernel data for performance
 */

import { ContentLoader } from './ContentLoader.js';
import { DimensionalAgent } from './DimensionalAgent.js';
import { TopologyAgent0D } from './agents/0D-TopologyAgent.js';
import { TemporalAgent1D } from './agents/1D-TemporalAgent.js';
import { StructuralAgent2D } from './agents/2D-StructuralAgent.js';
import { AlgebraicAgent3D } from './agents/3D-AlgebraicAgent.js';
import { NetworkAgent4D } from './agents/4D-NetworkAgent.js';
import { ConsensusAgent5D } from './agents/5D-ConsensusAgent.js';
import { IntelligenceAgent6D } from './agents/6D-IntelligenceAgent.js';
import { QuantumAgent7D } from './agents/7D-QuantumAgent.js';

export class AgentCoordinator {
  constructor(kernelUrl = '/automaton-kernel.jsonl', contentIndexUrl = '/content-index.jsonl') {
    this.contentLoader = new ContentLoader(kernelUrl, contentIndexUrl);
    this.agents = new Map();
    this.contentData = null;
    this.initialized = false;
  }

  /**
   * Initialize coordinator and load kernel
   */
  async init() {
    if (this.initialized) {
      return;
    }

    try {
      // Load all content sources (kernel + frontmatter)
      await this.contentLoader.loadAll();
      this.contentData = this.contentLoader.getAllEntries();
      
      // Initialize specific dimensional agents (0D-7D)
      const agentClasses = [
        TopologyAgent0D,
        TemporalAgent1D,
        StructuralAgent2D,
        AlgebraicAgent3D,
        NetworkAgent4D,
        ConsensusAgent5D,
        IntelligenceAgent6D,
        QuantumAgent7D
      ];
      
      for (let dim = 0; dim <= 7; dim++) {
        const dimension = `${dim}D`;
        const AgentClass = agentClasses[dim];
        if (AgentClass) {
          const agent = new AgentClass(this.contentLoader);
          this.agents.set(dimension, agent);
        } else {
          // Fallback to generic agent
          const agent = new DimensionalAgent(dimension, this.contentLoader);
          this.agents.set(dimension, agent);
        }
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
   * @param {string} dimension - Dimension string (e.g., "0D")
   * @returns {DimensionalAgent|null} Agent instance or null
   */
  getAgent(dimension) {
    if (!this.initialized) {
      throw new Error('AgentCoordinator not initialized. Call init() first.');
    }

    // Normalize dimension
    const dim = dimension.replace(/D$/, '') + 'D';
    return this.agents.get(dim) || null;
  }

  /**
   * Populate a single slide
   * @param {Object} slide - Slide object to populate
   * @returns {Promise<Object>} Populated slide
   */
  async populateSlide(slide) {
    if (!this.initialized) {
      await this.init();
    }

    // Get slide dimension (default to "0D" if not specified)
    const dimension = slide.dimension || '0D';
    
    // Get appropriate agent
    const agent = this.getAgent(dimension);
    if (!agent) {
      console.warn(`No agent found for dimension ${dimension}, using 0D agent`);
      const defaultAgent = this.getAgent('0D');
      if (!defaultAgent) {
        return slide; // Return unmodified if no agents available
      }
      return await defaultAgent.populateSlide(slide, this.contentData);
    }

    // Populate slide using agent
    return await agent.populateSlide(slide, this.contentData);
  }

  /**
   * Populate all slides in a deck
   * @param {Array} slides - Array of slide objects
   * @returns {Promise<Array>} Array of populated slides
   */
  async populateAll(slides) {
    if (!this.initialized) {
      await this.init();
    }

    if (!Array.isArray(slides) || slides.length === 0) {
      return slides;
    }

    console.log(`Populating ${slides.length} slides with agents...`);

    // Group slides by dimension for batch processing
    const slidesByDimension = new Map();
    for (const slide of slides) {
      const dimension = slide.dimension || '0D';
      if (!slidesByDimension.has(dimension)) {
        slidesByDimension.set(dimension, []);
      }
      slidesByDimension.get(dimension).push(slide);
    }

    // Populate slides by dimension
    const populatedSlides = [];
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
   * @param {Array} slides - Array of slides to check
   * @returns {Object} Status object
   */
  getPopulationStatus(slides) {
    if (!Array.isArray(slides)) {
      return { total: 0, populated: 0, unpopulated: 0 };
    }

    const populated = slides.filter(s => s._populated === true).length;
    const unpopulated = slides.length - populated;

    return {
      total: slides.length,
      populated,
      unpopulated,
      percentage: slides.length > 0 ? (populated / slides.length * 100).toFixed(1) : 0
    };
  }

  /**
   * Clear cache and reload all content
   */
  async reloadContent() {
    await this.contentLoader.reload();
    this.contentData = this.contentLoader.getAllEntries();
    this.initialized = false;
    await this.init();
  }

  /**
   * Set kernel URL
   * @param {string} url - New kernel URL
   */
  setKernelUrl(url) {
    this.contentLoader.kernelLoader.setKernelUrl(url);
    this.initialized = false; // Force re-initialization
  }

  /**
   * Set content index URL
   * @param {string} url - New content index URL
   */
  setContentIndexUrl(url) {
    this.contentLoader.frontmatterLoader.setContentIndexUrl(url);
    this.initialized = false; // Force re-initialization
  }
}

