/**
 * KernelLoader - Loads and parses automaton-kernel.jsonl
 * 
 * Provides methods to query kernel data by dimension, type, and relationships
 */

export interface KernelEntry {
  id?: string;
  type?: string;
  dimension?: string;
  dimensionalLevel?: number;
  fromNode?: string;
  toNode?: string;
  from?: string;
  to?: string;
  [key: string]: any;
}

export interface Relationships {
  vertical: KernelEntry[];
  horizontal: KernelEntry[];
  transitions: KernelEntry[];
  incoming: KernelEntry[];
  outgoing: KernelEntry[];
}

export class KernelLoader {
  private kernelData: KernelEntry[];
  private loaded: boolean;
  private kernelUrl: string;

  constructor() {
    this.kernelData = [];
    this.loaded = false;
    this.kernelUrl = '/automaton-kernel.jsonl'; // Default path, can be configured
  }

  /**
   * Set kernel URL
   */
  setKernelUrl(url: string): void {
    this.kernelUrl = url;
    this.loaded = false; // Force reload on next access
  }

  /**
   * Load and parse kernel JSONL file
   */
  async loadKernel(url: string | null = null): Promise<KernelEntry[]> {
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
          } catch (error: any) {
            console.warn(`Failed to parse kernel line: ${error.message}`);
          }
        }
      }

      this.loaded = true;
      console.log(`Loaded ${this.kernelData.length} entries from kernel`);
      return this.kernelData;
    } catch (error: any) {
      console.error(`Failed to load kernel from ${kernelUrl}:`, error);
      throw error;
    }
  }

  /**
   * Find entries by dimension
   */
  findByDimension(dimension: string): KernelEntry[] {
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
   */
  findByType(type: string): KernelEntry[] {
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
   */
  getRelationships(entryId: string): Relationships {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    const relationships: Relationships = {
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
   */
  findById(id: string): KernelEntry | null {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    return this.kernelData.find(entry => entry.id === id) || null;
  }

  /**
   * Get all entries
   */
  getAllEntries(): KernelEntry[] {
    if (!this.loaded) {
      throw new Error('Kernel not loaded. Call loadKernel() first.');
    }

    return this.kernelData;
  }

  /**
   * Clear cached data
   */
  clearCache(): void {
    this.kernelData = [];
    this.loaded = false;
  }
}

