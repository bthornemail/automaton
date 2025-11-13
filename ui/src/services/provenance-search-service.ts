/**
 * Provenance Search Service
 * 
 * Provides search and filtering capabilities for provenance chains.
 * Supports searching by pattern, dimension, agent, and advanced queries.
 */

import { ProvenanceChain, ProvenanceNode, ProvenanceEdge } from './provenance-slide-service';

export interface SearchQuery {
  pattern?: string;
  dimension?: string;
  agentId?: string;
  nodeType?: ProvenanceNode['type'];
  edgeType?: ProvenanceEdge['type'];
  file?: string;
  customQuery?: (node: ProvenanceNode | ProvenanceEdge) => boolean;
}

export interface FilterPreset {
  id: string;
  name: string;
  query: SearchQuery;
  createdAt: number;
  shared?: boolean;
}

export class ProvenanceSearchService {
  private presets: Map<string, FilterPreset> = new Map();

  /**
   * Search provenance chain by query.
   * 
   * Searches a provenance chain using the provided query criteria.
   * Returns matching nodes and edges.
   * 
   * @param {ProvenanceChain} chain - Provenance chain to search
   * @param {SearchQuery} query - Search query criteria
   * @returns {{ nodes: ProvenanceNode[]; edges: ProvenanceEdge[] }} Matching nodes and edges
   * 
   * @example
   * ```typescript
   * const results = service.searchChain(chain, {
   *   pattern: 'identity',
   *   dimension: '0D'
   * });
   * console.log(`Found ${results.nodes.length} nodes`);
   * ```
   */
  searchChain(
    chain: ProvenanceChain,
    query: SearchQuery
  ): { nodes: ProvenanceNode[]; edges: ProvenanceEdge[] } {
    const matchingNodes: ProvenanceNode[] = [];
    const matchingEdges: ProvenanceEdge[] = [];

    // Search nodes
    for (const node of chain.nodes) {
      if (this.matchesNode(node, query)) {
        matchingNodes.push(node);
      }
    }

    // Search edges
    for (const edge of chain.edges) {
      if (this.matchesEdge(edge, query, chain)) {
        matchingEdges.push(edge);
      }
    }

    return { nodes: matchingNodes, edges: matchingEdges };
  }

  /**
   * Filter nodes by query.
   * 
   * Filters nodes from a provenance chain based on query criteria.
   * 
   * @param {ProvenanceNode[]} nodes - Nodes to filter
   * @param {SearchQuery} query - Filter query
   * @returns {ProvenanceNode[]} Filtered nodes
   */
  filterNodes(nodes: ProvenanceNode[], query: SearchQuery): ProvenanceNode[] {
    return nodes.filter(node => this.matchesNode(node, query));
  }

  /**
   * Filter edges by query.
   * 
   * Filters edges from a provenance chain based on query criteria.
   * 
   * @param {ProvenanceEdge[]} edges - Edges to filter
   * @param {ProvenanceChain} chain - Full chain (needed for node lookups)
   * @param {SearchQuery} query - Filter query
   * @returns {ProvenanceEdge[]} Filtered edges
   */
  filterEdges(
    edges: ProvenanceEdge[],
    chain: ProvenanceChain,
    query: SearchQuery
  ): ProvenanceEdge[] {
    return edges.filter(edge => this.matchesEdge(edge, query, chain));
  }

  /**
   * Check if node matches query.
   */
  private matchesNode(node: ProvenanceNode, query: SearchQuery): boolean {
    // Custom query function
    if (query.customQuery) {
      return query.customQuery(node);
    }

    // Pattern match
    if (query.pattern && node.metadata.pattern !== query.pattern) {
      return false;
    }

    // Dimension match
    if (query.dimension && node.metadata.dimension !== query.dimension) {
      return false;
    }

    // Agent ID match
    if (query.agentId && node.metadata.agentId !== query.agentId) {
      return false;
    }

    // Node type match
    if (query.nodeType && node.type !== query.nodeType) {
      return false;
    }

    // File match
    if (query.file && node.metadata.file !== query.file) {
      return false;
    }

    return true;
  }

  /**
   * Check if edge matches query.
   */
  private matchesEdge(
    edge: ProvenanceEdge,
    query: SearchQuery,
    chain: ProvenanceChain
  ): boolean {
    // Custom query function
    if (query.customQuery) {
      return query.customQuery(edge);
    }

    // Edge type match
    if (query.edgeType && edge.type !== query.edgeType) {
      return false;
    }

    // If query has node-specific criteria, check connected nodes
    if (query.pattern || query.dimension || query.agentId || query.nodeType) {
      const fromNode = chain.nodes.find(n => n.id === edge.from);
      const toNode = chain.nodes.find(n => n.id === edge.to);

      // Check if either connected node matches
      const fromMatches = fromNode ? this.matchesNode(fromNode, query) : false;
      const toMatches = toNode ? this.matchesNode(toNode, query) : false;

      return fromMatches || toMatches;
    }

    return true;
  }

  /**
   * Save filter preset.
   * 
   * Saves a search query as a reusable preset.
   * 
   * @param {string} name - Preset name
   * @param {SearchQuery} query - Search query to save
   * @param {boolean} [shared] - Whether preset is shared
   * @returns {string} Preset ID
   * 
   * @example
   * ```typescript
   * const presetId = service.savePreset('0D Identity Nodes', {
   *   pattern: 'identity',
   *   dimension: '0D'
   * });
   * ```
   */
  savePreset(name: string, query: SearchQuery, shared: boolean = false): string {
    const presetId = `preset-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    
    const preset: FilterPreset = {
      id: presetId,
      name,
      query,
      createdAt: Date.now(),
      shared
    };

    this.presets.set(presetId, preset);
    this.savePresetsToStorage();

    return presetId;
  }

  /**
   * Get filter preset by ID.
   * 
   * @param {string} presetId - Preset ID
   * @returns {FilterPreset | undefined} Preset or undefined if not found
   */
  getPreset(presetId: string): FilterPreset | undefined {
    return this.presets.get(presetId);
  }

  /**
   * Get all filter presets.
   * 
   * @param {boolean} [sharedOnly] - Return only shared presets
   * @returns {FilterPreset[]} Array of presets
   */
  getAllPresets(sharedOnly: boolean = false): FilterPreset[] {
    const allPresets = Array.from(this.presets.values());
    if (sharedOnly) {
      return allPresets.filter(p => p.shared);
    }
    return allPresets;
  }

  /**
   * Delete filter preset.
   * 
   * @param {string} presetId - Preset ID to delete
   * @returns {boolean} True if preset was deleted
   */
  deletePreset(presetId: string): boolean {
    const deleted = this.presets.delete(presetId);
    if (deleted) {
      this.savePresetsToStorage();
    }
    return deleted;
  }

  /**
   * Export preset as shareable format.
   * 
   * @param {string} presetId - Preset ID to export
   * @returns {string} JSON string representation
   */
  exportPreset(presetId: string): string {
    const preset = this.presets.get(presetId);
    if (!preset) {
      throw new Error(`Preset ${presetId} not found`);
    }

    // Remove ID and timestamps for sharing
    const shareable = {
      name: preset.name,
      query: preset.query,
      shared: preset.shared
    };

    return JSON.stringify(shareable, null, 2);
  }

  /**
   * Import preset from shareable format.
   * 
   * @param {string} presetJson - JSON string representation
   * @returns {string} Preset ID
   */
  importPreset(presetJson: string): string {
    const shareable = JSON.parse(presetJson);
    return this.savePreset(shareable.name, shareable.query, shareable.shared || false);
  }

  /**
   * Build advanced query from multiple criteria.
   * 
   * Combines multiple search criteria into a single query with
   * AND/OR logic support.
   * 
   * @param {SearchQuery[]} queries - Array of queries to combine
   * @param {'AND' | 'OR'} logic - Logic operator (default: 'AND')
   * @returns {SearchQuery} Combined query
   * 
   * @example
   * ```typescript
   * const combined = service.buildAdvancedQuery([
   *   { dimension: '0D' },
   *   { pattern: 'identity' }
   * ], 'AND');
   * ```
   */
  buildAdvancedQuery(
    queries: SearchQuery[],
    logic: 'AND' | 'OR' = 'AND'
  ): SearchQuery {
    if (queries.length === 0) {
      return {};
    }

    if (queries.length === 1) {
      return queries[0];
    }

    // Combine queries with custom query function
    return {
      customQuery: (item: ProvenanceNode | ProvenanceEdge) => {
        if (logic === 'AND') {
          return queries.every(q => {
            if ('type' in item) {
              return this.matchesNode(item as ProvenanceNode, q);
            } else {
              // For edges, we'd need the full chain, so this is simplified
              return true;
            }
          });
        } else {
          return queries.some(q => {
            if ('type' in item) {
              return this.matchesNode(item as ProvenanceNode, q);
            } else {
              return true;
            }
          });
        }
      }
    };
  }

  /**
   * Save presets to localStorage.
   */
  private savePresetsToStorage(): void {
    try {
      const presetsArray = Array.from(this.presets.values());
      localStorage.setItem('provenance-filter-presets', JSON.stringify(presetsArray));
    } catch (error) {
      console.warn('Failed to save presets to localStorage:', error);
    }
  }

  /**
   * Load presets from localStorage.
   */
  loadPresetsFromStorage(): void {
    try {
      const stored = localStorage.getItem('provenance-filter-presets');
      if (stored) {
        const presetsArray = JSON.parse(stored) as FilterPreset[];
        this.presets.clear();
        for (const preset of presetsArray) {
          this.presets.set(preset.id, preset);
        }
      }
    } catch (error) {
      console.warn('Failed to load presets from localStorage:', error);
    }
  }

  /**
   * Initialize service (load presets from storage).
   */
  init(): void {
    this.loadPresetsFromStorage();
  }
}

// Export singleton instance
export const provenanceSearchService = new ProvenanceSearchService();

// Initialize on load
if (typeof window !== 'undefined') {
  provenanceSearchService.init();
}

