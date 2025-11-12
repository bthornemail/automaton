/**
 * ContentLoader - Unified content loader that delegates to specialized loaders
 * 
 * Provides unified query interface across kernel and frontmatter sources
 */

import { KernelLoader } from './KernelLoader';
import { FrontmatterLoader } from './FrontmatterLoader';

export class ContentLoader {
  private kernelLoader: KernelLoader;
  private frontmatterLoader: FrontmatterLoader;
  private loaded: boolean;

  constructor(kernelUrl: string = '/automaton-kernel.jsonl', contentIndexUrl: string = '/content-index.jsonl') {
    this.kernelLoader = new KernelLoader();
    this.kernelLoader.setKernelUrl(kernelUrl);
    
    this.frontmatterLoader = new FrontmatterLoader();
    this.frontmatterLoader.setContentIndexUrl(contentIndexUrl);
    
    this.loaded = false;
  }

  /**
   * Load all content sources
   */
  async loadAll(): Promise<void> {
    if (this.loaded) {
      return;
    }

    try {
      // Load kernel
      await this.kernelLoader.loadKernel();
      
      // Load content index (may fail if not built yet)
      try {
        await this.frontmatterLoader.loadContentIndex();
      } catch (error: any) {
        console.warn('Content index not available, using kernel only:', error.message);
      }

      this.loaded = true;
      console.log('ContentLoader: All sources loaded');
    } catch (error) {
      console.error('Failed to load content sources:', error);
      throw error;
    }
  }

  /**
   * Find entries by dimension across all sources
   */
  findByDimension(dimension: string): any[] {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const results: any[] = [];
    
    // Get from kernel
    const kernelEntries = this.kernelLoader.findByDimension(dimension);
    results.push(...kernelEntries);
    
    // Get from frontmatter
    try {
      const frontmatterDocs = this.frontmatterLoader.findByDimension(dimension);
      results.push(...frontmatterDocs);
    } catch (error: any) {
      // Frontmatter loader may not be loaded
      console.warn('FrontmatterLoader not available:', error.message);
    }

    return results;
  }

  /**
   * Find entries by tag across all sources
   */
  findByTag(tag: string): any[] {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const results: any[] = [];
    
    // Get from frontmatter (kernel doesn't have tags)
    try {
      const frontmatterDocs = this.frontmatterLoader.findByTag(tag);
      results.push(...frontmatterDocs);
    } catch (error: any) {
      console.warn('FrontmatterLoader not available:', error.message);
    }

    return results;
  }

  /**
   * Find entries by keyword across all sources
   */
  findByKeyword(keyword: string): any[] {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const results: any[] = [];
    
    // Get from frontmatter (kernel doesn't have keywords)
    try {
      const frontmatterDocs = this.frontmatterLoader.findByKeyword(keyword);
      results.push(...frontmatterDocs);
    } catch (error: any) {
      console.warn('FrontmatterLoader not available:', error.message);
    }

    return results;
  }

  /**
   * Find entries by relationship type
   */
  findByRelationship(relType: string, fromId: string): any[] {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const results: any[] = [];
    
    // Get from frontmatter
    try {
      const relationships = this.frontmatterLoader.getRelationships(fromId);
      const relatedIds = (relationships as any)[relType] || [];
      
      for (const relatedId of relatedIds) {
        const doc = this.frontmatterLoader.findById(relatedId);
        if (doc) {
          results.push(doc);
        }
      }
    } catch (error: any) {
      console.warn('FrontmatterLoader not available:', error.message);
    }

    return results;
  }

  /**
   * Get relationships for an entry
   */
  getRelationships(entryId: string): any {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    // Try kernel first
    try {
      const kernelRels = this.kernelLoader.getRelationships(entryId);
      if (kernelRels && (kernelRels.vertical.length > 0 || kernelRels.horizontal.length > 0)) {
        return kernelRels;
      }
    } catch (error) {
      // Entry not in kernel, try frontmatter
    }

    // Try frontmatter
    try {
      const frontmatterRels = this.frontmatterLoader.getRelationships(entryId);
      if (frontmatterRels && (frontmatterRels.prerequisites.length > 0 || frontmatterRels.enables.length > 0 || frontmatterRels.related.length > 0)) {
        return {
          prerequisites: frontmatterRels.prerequisites.map((id: string) => ({ fromNode: entryId, toNode: id })),
          enables: frontmatterRels.enables.map((id: string) => ({ fromNode: entryId, toNode: id })),
          related: frontmatterRels.related.map((id: string) => ({ fromNode: entryId, toNode: id })),
          vertical: [],
          horizontal: []
        };
      }
    } catch (error) {
      // Entry not found
    }

    return {
      prerequisites: [],
      enables: [],
      related: [],
      vertical: [],
      horizontal: []
    };
  }

  /**
   * Get RDF triples for an entry
   */
  getRDFTriples(entryId: string): any[] {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const triples: any[] = [];
    
    // Get from frontmatter
    try {
      const frontmatterTriples = this.frontmatterLoader.getRDFTriples(entryId);
      triples.push(...frontmatterTriples);
    } catch (error: any) {
      console.warn('FrontmatterLoader not available:', error.message);
    }

    return triples;
  }

  /**
   * Find entry by ID across all sources
   */
  findById(id: string): any | null {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    // Try kernel first
    const kernelEntry = this.kernelLoader.findById(id);
    if (kernelEntry) {
      return kernelEntry;
    }

    // Try frontmatter
    try {
      const frontmatterDoc = this.frontmatterLoader.findById(id);
      if (frontmatterDoc) {
        return frontmatterDoc;
      }
    } catch (error) {
      // Not found
    }

    return null;
  }

  /**
   * Get all entries from all sources
   */
  getAllEntries(): any[] {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const results: any[] = [];
    
    // Get from kernel
    results.push(...this.kernelLoader.getAllEntries());
    
    // Get from frontmatter
    try {
      results.push(...this.frontmatterLoader.getAllDocuments());
    } catch (error: any) {
      console.warn('FrontmatterLoader not available:', error.message);
    }

    return results;
  }

  /**
   * Get knowledge graph structure
   */
  getKnowledgeGraph(): any {
    if (!this.loaded) {
      throw new Error('ContentLoader not loaded. Call loadAll() first.');
    }

    const kernelEntries = this.kernelLoader.getAllEntries();
    
    let frontmatterGraph: any = null;
    try {
      frontmatterGraph = this.frontmatterLoader.getKnowledgeGraph();
    } catch (error) {
      frontmatterGraph = { nodes: [], edges: [], triples: [] };
    }

    return {
      kernel: {
        entries: kernelEntries,
        count: kernelEntries.length
      },
      frontmatter: frontmatterGraph,
      total: kernelEntries.length + (frontmatterGraph.nodes?.length || 0)
    };
  }

  /**
   * Clear cache and reload
   */
  async reload(): Promise<void> {
    this.kernelLoader.clearCache();
    this.frontmatterLoader.clearCache();
    this.loaded = false;
    await this.loadAll();
  }
}

