/**
 * FrontmatterLoader - Loads and queries pre-processed frontmatter content
 * 
 * Loads content-index.jsonl and provides query methods for dimension,
 * tag, level, type, and relationship queries.
 */

export interface DocumentEntry {
  id: string;
  type?: string;
  dimension?: string;
  level?: string;
  docType?: string;
  title?: string;
  description?: string;
  tags?: string[];
  keywords?: string[];
  frontmatter?: any;
  body?: string;
  relationships?: {
    prerequisites?: string[];
    enables?: string[];
    related?: string[];
  };
  [key: string]: any;
}

export interface RelationshipEntry {
  type: string;
  from?: string;
  to?: string;
  [key: string]: any;
}

export interface RDFTripleEntry {
  type: string;
  subject: string;
  predicate: string;
  object: string;
  [key: string]: any;
}

export interface KnowledgeGraph {
  nodes: DocumentEntry[];
  edges: RelationshipEntry[];
  triples: RDFTripleEntry[];
}

export class FrontmatterLoader {
  private contentData: any[];
  private documents: Map<string, DocumentEntry>;
  private relationships: RelationshipEntry[];
  private rdfTriples: RDFTripleEntry[];
  private loaded: boolean;
  private contentIndexUrl: string;

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
   */
  setContentIndexUrl(url: string): void {
    this.contentIndexUrl = url;
    this.loaded = false;
  }

  /**
   * Load and parse content index JSONL file
   */
  async loadContentIndex(url: string | null = null): Promise<any[]> {
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
          } catch (error: any) {
            console.warn(`Failed to parse content index line: ${error.message}`);
          }
        }
      }

      this.loaded = true;
      console.log(`Loaded ${this.documents.size} documents, ${this.relationships.length} relationships, ${this.rdfTriples.length} RDF triples from content index`);
      return this.contentData;
    } catch (error: any) {
      console.error(`Failed to load content index from ${indexUrl}:`, error);
      throw error;
    }
  }

  /**
   * Find documents by dimension
   */
  findByDimension(dimension: string): DocumentEntry[] {
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
   */
  findByLevel(level: string): DocumentEntry[] {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => doc.level === level);
  }

  /**
   * Find documents by type
   */
  findByType(type: string): DocumentEntry[] {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => doc.docType === type);
  }

  /**
   * Find documents by tag
   */
  findByTag(tag: string): DocumentEntry[] {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => {
      return doc.tags && doc.tags.includes(tag);
    });
  }

  /**
   * Find documents by keyword
   */
  findByKeyword(keyword: string): DocumentEntry[] {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values()).filter(doc => {
      return doc.keywords && doc.keywords.includes(keyword);
    });
  }

  /**
   * Get relationships for a document
   */
  getRelationships(docId: string): { prerequisites: string[]; enables: string[]; related: string[] } {
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
   */
  getRDFTriples(docId: string): RDFTripleEntry[] {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return this.rdfTriples.filter(triple => {
      return triple.subject === `#${docId}` || triple.object === `#${docId}`;
    });
  }

  /**
   * Find document by ID
   */
  findById(id: string): DocumentEntry | null {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return this.documents.get(id) || null;
  }

  /**
   * Get all documents
   */
  getAllDocuments(): DocumentEntry[] {
    if (!this.loaded) {
      throw new Error('Content index not loaded. Call loadContentIndex() first.');
    }

    return Array.from(this.documents.values());
  }

  /**
   * Get knowledge graph structure
   */
  getKnowledgeGraph(): KnowledgeGraph {
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
  clearCache(): void {
    this.contentData = [];
    this.documents.clear();
    this.relationships = [];
    this.rdfTriples = [];
    this.loaded = false;
  }
}

