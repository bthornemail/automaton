/**
 * DBpediaPlugin - DBpedia federation and property mapping plugin
 * 
 * Provides SPARQL queries to DBpedia endpoint, property mapping,
 * caching, and error handling for semantic slide enrichment.
 */

import { BasePlugin } from './BasePlugin.js';

/**
 * Error types for DBpedia plugin
 */
export const DBpediaErrorTypes = {
  NETWORK: 'network',
  PARSE: 'parse',
  NOT_FOUND: 'notfound',
  RATE_LIMIT: 'ratelimit',
  INVALID_ID: 'invalid_id',
  QUERY_FAILED: 'query_failed'
};

/**
 * DBpedia-specific error class
 */
export class DBpediaError extends Error {
  constructor(type, message, context = {}) {
    super(message);
    this.name = 'DBpediaError';
    this.type = type;
    this.context = context;
  }
}

export class DBpediaPlugin extends BasePlugin {
  constructor(config = {}) {
    super({
      name: 'DBpedia',
      version: '0.1.0',
      hooks: ['sparql'],
      endpoint: config.endpoint || 'https://dbpedia.org/sparql',
      cacheTTL: config.cacheTTL || 900000, // 15 minutes
      ...config
    });
    
    this.cache = new Map();
    this.propertyMap = {
      abstract: 'dbo:abstract',
      thumbnail: 'dbo:thumbnail',
      birthDate: 'dbo:birthDate',
      deathDate: 'dbo:deathDate',
      influencedBy: 'dbo:influencedBy',
      influenced: 'dbo:influenced',
      almaMater: 'dbo:almaMater',
      areaTotal: 'dbo:areaTotal',
      populationTotal: 'dbo:populationTotal'
    };
  }

  /**
   * Initialize DBpedia plugin
   */
  async onInit() {
    console.log(`DBpediaPlugin initialized with endpoint: ${this.config.endpoint}`);
  }

  /**
   * Query DBpedia for a specific property
   * @param {string} dbpediaId - DBpedia resource ID (e.g., "Albert_Einstein")
   * @param {string} property - Property to query (e.g., "abstract", "thumbnail")
   * @param {string} targetPredicate - Target predicate for result (e.g., "ui:summary")
   * @returns {Promise<Object>} Query result with triples
   */
  async queryProperty(dbpediaId, property, targetPredicate = 'ui:summary') {
    const propertyPath = this.propertyMap[property] || property;
    const cacheKey = `dbpedia:${dbpediaId}:${property}`;
    
    // Check cache
    if (this.cache.has(cacheKey)) {
      const cached = this.cache.get(cacheKey);
      if (Date.now() - cached.timestamp < this.config.cacheTTL) {
        return cached.data;
      }
    }
    
    // Build SPARQL query
    const query = `
      PREFIX dbr: <http://dbpedia.org/resource/>
      PREFIX dbo: <http://dbpedia.org/ontology/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      
      SELECT ?value WHERE {
        dbr:${dbpediaId} ${propertyPath} ?value .
        ${property === 'abstract' ? 'FILTER(LANG(?value) = "en")' : ''}
      }
      LIMIT 1
    `;
    
    try {
      // Validate DBpedia ID
      if (!dbpediaId || typeof dbpediaId !== 'string') {
        throw new DBpediaError(DBpediaErrorTypes.INVALID_ID, 
          `Invalid DBpedia ID: ${dbpediaId}`, { dbpediaId, property });
      }

      const result = await this.hook('sparql', {
        query,
        endpoint: this.config.endpoint
      });
      
      // Check for SPARQL errors
      if (result.error) {
        throw new DBpediaError(DBpediaErrorTypes.QUERY_FAILED,
          `SPARQL query failed: ${result.error}`, { dbpediaId, property, query });
      }
      
      const data = {
        triples: [],
        value: null
      };
      
      if (result.results && result.results.bindings && result.results.bindings.length > 0) {
        const binding = result.results.bindings[0];
        const value = binding.value?.value || binding.value;
        
        data.value = value;
        data.triples.push({
          subject: `dbr:${dbpediaId}`,
          predicate: targetPredicate,
          object: value
        });
      }
      
      // Cache result (even if empty, to avoid repeated queries)
      this.cache.set(cacheKey, {
        data,
        timestamp: Date.now()
      });
      
      return data;
    } catch (error) {
      // Enhanced error handling
      if (error instanceof DBpediaError) {
        throw error;
      }

      // Classify error
      const errorMessage = error.message || String(error);
      let errorType = DBpediaErrorTypes.NETWORK;
      
      if (errorMessage.includes('404') || errorMessage.includes('not found')) {
        errorType = DBpediaErrorTypes.NOT_FOUND;
      } else if (errorMessage.includes('429') || errorMessage.includes('rate limit')) {
        errorType = DBpediaErrorTypes.RATE_LIMIT;
      } else if (errorMessage.includes('parse') || errorMessage.includes('JSON')) {
        errorType = DBpediaErrorTypes.PARSE;
      }

      const dbpediaError = new DBpediaError(errorType,
        `DBpedia query failed for ${dbpediaId}.${property}: ${errorMessage}`,
        { dbpediaId, property, originalError: error });

      // Emit error event if available
      if (this.onError) {
        this.onError(dbpediaError);
      }

      throw dbpediaError;
    }
  }

  /**
   * Query DBpedia abstract
   * @param {string} dbpediaId - DBpedia resource ID
   * @returns {Promise<Object>} Abstract query result
   */
  async queryAbstract(dbpediaId) {
    return await this.queryProperty(dbpediaId, 'abstract', 'ui:summary');
  }

  /**
   * Query DBpedia thumbnail
   * @param {string} dbpediaId - DBpedia resource ID
   * @returns {Promise<Object>} Thumbnail query result
   */
  async queryThumbnail(dbpediaId) {
    return await this.queryProperty(dbpediaId, 'thumbnail', 'ui:image');
  }

  /**
   * Query DBpedia birth date
   * @param {string} dbpediaId - DBpedia resource ID
   * @returns {Promise<Object>} Birth date query result
   */
  async queryBirthDate(dbpediaId) {
    return await this.queryProperty(dbpediaId, 'birthDate', 'ui:born');
  }

  /**
   * Query DBpedia related entities
   * @param {string} dbpediaId - DBpedia resource ID
   * @param {string} relation - Relation property (e.g., "influencedBy")
   * @returns {Promise<Object>} Related entities query result
   */
  async queryRelated(dbpediaId, relation = 'influencedBy') {
    const propertyPath = this.propertyMap[relation] || `dbo:${relation}`;
    const cacheKey = `dbpedia:${dbpediaId}:related:${relation}`;
    
    // Check cache
    if (this.cache.has(cacheKey)) {
      const cached = this.cache.get(cacheKey);
      if (Date.now() - cached.timestamp < this.config.cacheTTL) {
        return cached.data;
      }
    }
    
    const query = `
      PREFIX dbr: <http://dbpedia.org/resource/>
      PREFIX dbo: <http://dbpedia.org/ontology/>
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      
      SELECT ?related ?label WHERE {
        dbr:${dbpediaId} ${propertyPath} ?related .
        OPTIONAL {
          ?related rdfs:label ?label .
          FILTER(LANG(?label) = "en")
        }
      }
      LIMIT 10
    `;
    
    try {
      const result = await this.hook('sparql', {
        query,
        endpoint: this.config.endpoint
      });
      
      const data = {
        triples: [],
        related: []
      };
      
      if (result.results && result.results.bindings) {
        for (const binding of result.results.bindings) {
          const related = binding.related?.value || binding.related;
          const label = binding.label?.value || related.split('/').pop().replace(/_/g, ' ');
          
          data.related.push({ uri: related, label });
          data.triples.push({
            subject: `dbr:${dbpediaId}`,
            predicate: 'ui:related',
            object: related
          });
        }
      }
      
      // Cache result
      this.cache.set(cacheKey, {
        data,
        timestamp: Date.now()
      });
      
      return data;
    } catch (error) {
      console.error(`DBpedia related query failed for ${dbpediaId}.${relation}:`, error);
      return {
        triples: [],
        related: [],
        error: error.message
      };
    }
  }

  /**
   * Enrich slide with DBpedia data
   * @param {Object} slide - Slide definition
   * @param {string} dbpediaId - DBpedia resource ID
   * @returns {Promise<Object>} Enriched slide
   */
  async enrichSlide(slide, dbpediaId) {
    const enriched = { ...slide };
    
    // Query abstract
    const abstract = await this.queryAbstract(dbpediaId);
    if (abstract.value) {
      enriched.summary = abstract.value;
      enriched.triples = enriched.triples || [];
      enriched.triples.push(...abstract.triples);
    }
    
    // Query thumbnail
    const thumbnail = await this.queryThumbnail(dbpediaId);
    if (thumbnail.value) {
      enriched.image = thumbnail.value;
      enriched.triples = enriched.triples || [];
      enriched.triples.push(...thumbnail.triples);
    }
    
    // Query birth date
    const birthDate = await this.queryBirthDate(dbpediaId);
    if (birthDate.value) {
      enriched.born = birthDate.value;
      enriched.triples = enriched.triples || [];
      enriched.triples.push(...birthDate.triples);
    }
    
    // Query related entities
    const influencedBy = await this.queryRelated(dbpediaId, 'influencedBy');
    if (influencedBy.related.length > 0) {
      enriched.influencedBy = influencedBy.related;
      enriched.triples = enriched.triples || [];
      enriched.triples.push(...influencedBy.triples);
    }
    
    return enriched;
  }

  /**
   * Render slide with DBpedia enrichment
   * @param {Object} slide - Slide definition
   * @param {string} mode - Rendering mode
   * @returns {Promise<Object>} Rendered slide
   */
  async render(slide, mode = 'static') {
    // If slide has dbpediaId, enrich it
    if (slide.dbpediaId) {
      return await this.enrichSlide(slide, slide.dbpediaId);
    }
    
    return slide;
  }

  /**
   * Clear cache
   */
  clearCache() {
    this.cache.clear();
  }

  /**
   * Get cache statistics
   * @returns {Object} Cache stats
   */
  getCacheStats() {
    return {
      size: this.cache.size,
      entries: Array.from(this.cache.keys())
    };
  }
}
