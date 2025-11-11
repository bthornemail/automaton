/**
 * KernelLoader - Loads and parses automaton-kernel.jsonl
 * 
 * Provides methods to query kernel data by dimension, type, and relationships
 */

export class KernelLoader {
  constructor() {
    this.kernelData = [];
    this.loaded = false;
    this.kernelUrl = '/automaton-kernel.jsonl'; // Default path, can be configured
  }

  /**
   * Set kernel URL
   * @param {string} url - URL to automaton-kernel.jsonl
   */
  setKernelUrl(url) {
    this.kernelUrl = url;
    this.loaded = false; // Force reload on next access
  }

  /**
   * Load and parse kernel JSONL file
   * @param {string} url - Optional URL override
   * @returns {Promise<Array>} Parsed kernel objects
   */
  async loadKernel(url = null) {
    if (this.loaded && !url) {
      return this.kernelData;
    }

    const kernelUrl = url || this.kernelUrl;
    
    try {
      const response = await fetch(kernelUrl);
      if (!response.ok) {
        throw new Error(`Failed to load kernel: ${response.statusText}`);
      }

      const content = await response.text();
      const lines = content.split('\n').filter(line => line.trim());
      
      this.kernelData = [];
      for (const line of lines) {
        if (line.trim() && line.trim().startsWith('{')) {
          try {
            const obj = JSON.parse(line);
            this.kernelData.push(obj);
          } catch (error) {
            console.warn(`Failed to parse kernel line: ${error.message}`);
          }
        }
      }

      this.loaded = true;
      console.log(`Loaded ${this.kernelData.length} entries from kernel`);
      return this.kernelData;
    } catch (error) {
      console.error(`Failed to load kernel from ${kernelUrl}:`, error);
      throw error;
    }
  }

  /**
   * Find entries by dimension
   * @param {string} dimension - Dimension string (e.g., "0D", "1D")
   * @returns {Array} Matching kernel entries
   */
  findByDimension(dimension) {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    // Normalize dimension (remove "D" if present, add it back)
    const dim = dimension.replace(/D$/, '') + 'D';
    
    return this.kernelData.filter(entry => {
      // Check if entry ID contains dimension
      if (entry.id && entry.id.startsWith(dim)) {
        return true;
      }
      
      // Check if entry has dimension property
      if (entry.dimension === dim || entry.dimension === dimension) {
        return true;
      }
      
      // Check dimensionalLevel property
      if (entry.dimensionalLevel !== undefined) {
        const entryDim = `${entry.dimensionalLevel}D`;
        return entryDim === dim || entryDim === dimension;
      }
      
      return false;
    });
  }

  /**
   * Find entries by type
   * @param {string} type - Entry type (e.g., "topology", "system", "automaton", "vertical", "horizontal")
   * @returns {Array} Matching kernel entries
   */
  findByType(type) {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    return this.kernelData.filter(entry => {
      // Direct type match
      if (entry.type === type) {
        return true;
      }
      
      // Check if ID contains type
      if (entry.id && entry.id.includes(type)) {
        return true;
      }
      
      return false;
    });
  }

  /**
   * Get relationships for an entry
   * @param {string} entryId - Entry ID to find relationships for
   * @returns {Object} Relationships object with vertical, horizontal, and transitions arrays
   */
  getRelationships(entryId) {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    const relationships = {
      vertical: [],
      horizontal: [],
      transitions: [],
      incoming: [],
      outgoing: []
    };

    for (const entry of this.kernelData) {
      // Vertical edges (fromNode → toNode)
      if (entry.type === 'vertical') {
        if (entry.fromNode === entryId) {
          relationships.vertical.push(entry);
          relationships.outgoing.push(entry);
        }
        if (entry.toNode === entryId) {
          relationships.incoming.push(entry);
        }
      }
      
      // Horizontal edges (topology ↔ system)
      if (entry.type === 'horizontal') {
        if (entry.fromNode === entryId || entry.toNode === entryId) {
          relationships.horizontal.push(entry);
        }
      }
      
      // Transitions (automaton state transitions)
      if (entry.type === 'transition') {
        if (entry.from === entryId || entry.to === entryId) {
          relationships.transitions.push(entry);
        }
      }
    }

    return relationships;
  }

  /**
   * Find entry by ID
   * @param {string} id - Entry ID
   * @returns {Object|null} Matching entry or null
   */
  findById(id) {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    return this.kernelData.find(entry => entry.id === id) || null;
  }

  /**
   * Get all entries
   * @returns {Array} All kernel entries
   */
  getAllEntries() {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    return this.kernelData;
  }

  /**
   * Clear cached data
   */
  clearCache() {
    this.kernelData = [];
    this.loaded = false;
  }
}

