/**
 * FrontmatterLoader - Loads and queries pre-processed frontmatter content
 * 
 * Loads content-index.jsonl and provides query methods for dimension,
 * tag, level, type, and relationship queries.
 */

export class FrontmatterLoader {
  constructor() {
    this.contentData = [];
    this.documents = new Map();
    this.relationships = [];
    this.rdfTriples = [];
    this.loaded = false;
    this.contentIndexUrl = '/content-index.jsonl';
  }

  /**
   * Set content index URL
   * @param {string} url - URL to content-index.jsonl
   */
  setContentIndexUrl(url) {
    this.contentIndexUrl = url;
    this.loaded = false;
  }

  /**
   * Load and parse content index JSONL file
   * @param {string} url - Optional URL override
   * @returns {Promise<Array>} Parsed content entries
   */
  async loadContentIndex(url = null) {
    if (this.loaded && !url) {
      return this.contentData;
    }

    const indexUrl = url || this.contentIndexUrl;
    
    try {
      const response = await fetch(indexUrl);
      if (!response.ok) {
        throw new Error(`Failed to load content index: ${response.statusText}`);
      }

      const content = await response.text();
      const lines = content.split('\n').filter(line => line.trim());
      
      this.contentData = [];
      this.documents = new Map();
      this.relationships = [];
      this.rdfTriples = [];
      
      for (const line of lines) {
        if (line.trim() && line.trim().startsWith('{')) {
          try {
            const obj = JSON.parse(line);
            this.contentData.push(obj);
            
            // Index by type
            if (obj.type === 'document') {
              this.documents.set(obj.id, obj);
            } else if (obj.type === 'relationship') {
              this.relationships.push(obj);
            } else if (obj.type === 'rdf-triple') {
              this.rdfTriples.push(obj);
            }
          } catch (error) {
            console.warn(`Failed to parse content index line: ${error.message}`);
          }
        }
      }

      this.loaded = true;
      console.log(`Loaded ${this.documents.size} documents, ${this.relationships.length} relationships, ${this.rdfTriples.length} RDF triples from content index`);
      return this.contentData;
    } catch (error) {
      console.error(`Failed to load content index from ${indexUrl}:`, error);
      throw error;
    }
  }

  /**
   * Find documents by dimension
   * @param {string} dimension - Dimension string (e.g., "0D")
   * @returns {Array} Matching document entries
   */
  findByDimension(dimension) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    const dim = dimension.replace(/D$/, '') + 'D';
    
    return Array.from(this.documents.values()).filter(doc => {
      return doc.dimension === dim || doc.dimension === dimension;
    });
  }

  /**
   * Find documents by level
   * @param {string} level - Level string (e.g., "foundational", "practical")
   * @returns {Array} Matching document entries
   */
  findByLevel(level) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => doc.level === level);
  }

  /**
   * Find documents by type
   * @param {string} type - Type string (e.g., "concept", "guide")
   * @returns {Array} Matching document entries
   */
  findByType(type) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => doc.docType === type);
  }

  /**
   * Find documents by tag
   * @param {string} tag - Tag string
   * @returns {Array} Matching document entries
   */
  findByTag(tag) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => {
      return doc.tags && doc.tags.includes(tag);
    });
  }

  /**
   * Find documents by keyword
   * @param {string} keyword - Keyword string
   * @returns {Array} Matching document entries
   */
  findByKeyword(keyword) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => {
      return doc.keywords && doc.keywords.includes(keyword);
    });
  }

  /**
   * Get relationships for a document
   * @param {string} docId - Document ID
   * @returns {Object} Relationships object
   */
  getRelationships(docId) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    const doc = this.documents.get(docId);
    if (!doc || !doc.relationships) {
      return {
        prerequisites: [],
        enables: [],
        related: []
      };
    }

    return {
      prerequisites: doc.relationships.prerequisites || [],
      enables: doc.relationships.enables || [],
      related: doc.relationships.related || []
    };
  }

  /**
   * Get RDF triples for a document
   * @param {string} docId - Document ID
   * @returns {Array} RDF triple entries
   */
  getRDFTriples(docId) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return this.rdfTriples.filter(triple => {
      return triple.subject === `#${docId}` || triple.object === `#${docId}`;
    });
  }

  /**
   * Find document by ID
   * @param {string} id - Document ID
   * @returns {Object|null} Document entry or null
   */
  findById(id) {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return this.documents.get(id) || null;
  }

  /**
   * Get all documents
   * @returns {Array} All document entries
   */
  getAllDocuments() {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values());
  }

  /**
   * Get knowledge graph structure
   * @returns {Object} Knowledge graph with nodes and edges
   */
  getKnowledgeGraph() {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return {
      nodes: Array.from(this.documents.values()),
      edges: this.relationships,
      triples: this.rdfTriples
    };
  }

  /**
   * Clear cached data
   */
  clearCache() {
    this.contentData = [];
    this.documents.clear();
    this.relationships = [];
    this.rdfTriples = [];
    this.loaded = false;
  }
}

