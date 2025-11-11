/**
 * CanvasLExecutor - Executes CanvasL objects with Meta-Log integration
 * 
 * Handles execution of different CanvasL object types:
 * - rdf-triple: Add to RDF store
 * - r5rs-call: Execute R5RS function
 * - sparql-construct: Execute SPARQL CONSTRUCT query
 * - prolog-query: Execute ProLog query
 * - datalog-query: Execute DataLog query
 * - shacl-validate: Validate with SHACL
 */

export class CanvasLExecutor {
  constructor(metaLogBridge) {
    this.metaLog = metaLogBridge;
  }

  /**
   * Execute a CanvasL object
   * @param {Object} obj - CanvasL object
   * @returns {Promise<*>} Execution result
   */
  async execute(obj) {
    switch (obj.type) {
      case 'rdf-triple':
        return await this.executeRdfTriple(obj);
      
      case 'r5rs-call':
        return await this.executeR5RSCall(obj);
      
      case 'sparql-construct':
        return await this.executeSparqlConstruct(obj);
      
      case 'prolog-query':
        return await this.executePrologQuery(obj);
      
      case 'datalog-query':
        return await this.executeDatalogQuery(obj);
      
      case 'shacl-validate':
        return await this.executeShaclValidate(obj);
      
      case 'slide':
        return obj; // Return slide as-is
      
      default:
        console.warn(`Unknown CanvasL object type: ${obj.type}`);
        return obj;
    }
  }

  /**
   * Execute RDF triple
   * @param {Object} obj - RDF triple object
   */
  async executeRdfTriple(obj) {
    this.metaLog.addTriple(obj.subject, obj.predicate, obj.object);
    return { type: 'rdf-triple', subject: obj.subject, predicate: obj.predicate, object: obj.object };
  }

  /**
   * Execute R5RS function call
   * @param {Object} obj - R5RS call object
   */
  async executeR5RSCall(obj) {
    const result = await this.metaLog.evalR5RS(obj.expression, obj.context || {});
    return { type: 'r5rs-result', result };
  }

  /**
   * Execute SPARQL CONSTRUCT query
   * @param {Object} obj - SPARQL construct object
   */
  async executeSparqlConstruct(obj) {
    const query = typeof obj.query === 'string' ? obj.query : obj.query.template;
    const endpoint = obj.endpoint || null;
    
    const result = await this.metaLog.sparqlQuery(query, endpoint);
    
    // If CONSTRUCT query, add triples to store
    if (result.results && result.results.bindings) {
      for (const binding of result.results.bindings) {
        // Extract triples from CONSTRUCT result
        // This is simplified - full implementation would parse CONSTRUCT format
      }
    }
    
    return { type: 'sparql-result', result };
  }

  /**
   * Execute ProLog query
   * @param {Object} obj - ProLog query object
   */
  async executePrologQuery(obj) {
    const query = obj.query || obj.goal;
    const facts = obj.facts || [];
    
    const result = await this.metaLog.prologQuery(query, facts);
    return { type: 'prolog-result', result };
  }

  /**
   * Execute DataLog query
   * @param {Object} obj - DataLog query object
   */
  async executeDatalogQuery(obj) {
    const goal = obj.goal || obj.query;
    const program = obj.program || null;
    
    const result = await this.metaLog.datalogQuery(goal, program);
    return { type: 'datalog-result', result };
  }

  /**
   * Execute SHACL validation
   * @param {Object} obj - SHACL validate object
   */
  async executeShaclValidate(obj) {
    const shapes = obj.shapes || obj.shape;
    const focus = obj.focus || null;
    
    const result = await this.metaLog.shaclValidate(shapes, focus);
    return { type: 'shacl-result', result };
  }

  /**
   * Execute multiple objects
   * @param {Array} objects - CanvasL objects
   * @returns {Promise<Object>} Execution results
   */
  async executeAll(objects) {
    const results = {
      triples: [],
      slides: [],
      objects: new Map(), // Store all objects by ID
      errors: []
    };

    for (const obj of objects) {
      try {
        // Skip @include directives (should be expanded already, but check anyway)
        if (obj['@include'] || obj.type === '@include') {
          continue;
        }
        
        // Skip version directives
        if (obj['@version']) {
          continue;
        }
        
        // Skip macro definitions (they're used for expansion, not execution)
        if (obj.type === 'macro' && obj.call) {
          continue;
        }

        // Skip macros (already expanded)
        if (obj.type === 'macro') {
          continue;
        }

        // Handle slide objects directly - don't execute them, just collect
        if (obj.type === 'slide') {
          // Ensure slide has required properties
          if (!obj.id) {
            console.warn('Slide object missing id:', obj);
          }
          if (!obj.dimension) {
            console.warn(`Slide ${obj.id || 'unnamed'} missing dimension, defaulting to 0D`);
            obj.dimension = '0D';
          }
          
          results.slides.push(obj);
          if (obj.id) {
            results.objects.set(obj.id, obj);
          }
          console.log(`Found slide: ${obj.id} (dimension: ${obj.dimension})`);
          continue;
        }

        const result = await this.execute(obj);
        
        // Store object by ID if it has one
        if (obj.id) {
          results.objects.set(obj.id, result);
        }
        
        if (result.type === 'rdf-triple') {
          results.triples.push(result);
        } else if (result.type === 'slide' || obj.type === 'slide') {
          // Collect slides (use original obj if result doesn't have slide properties)
          const slide = result.type === 'slide' ? result : obj;
          results.slides.push(slide);
        } else {
          results[obj.id || 'results'] = result;
        }
      } catch (error) {
        results.errors.push({
          object: obj,
          error: error.message
        });
      }
    }

    console.log(`CanvasLExecutor: Found ${results.slides.length} slides, ${results.objects.size} objects`);
    console.log(`All slide IDs found:`, results.slides.map(s => ({ id: s.id, dimension: s.dimension })));

    // If deck object exists, resolve slide references
    const deckObj = Array.from(results.objects.values()).find(o => o.type === 'deck' || o.id?.includes('deck'));
    if (deckObj && deckObj.slides && Array.isArray(deckObj.slides)) {
      console.log(`Deck found with ${deckObj.slides.length} slide references:`, deckObj.slides);
      console.log(`Available slides before resolution:`, results.slides.map(s => s.id));
      
      // Resolve slide IDs to actual slide objects
      const resolvedSlides = [];
      const missingSlides = [];
      
      for (const slideId of deckObj.slides) {
        // Try multiple ways to find the slide
        let slide = results.slides.find(s => s.id === slideId);
        
        // If not found, try case-insensitive match
        if (!slide) {
          slide = results.slides.find(s => s.id && s.id.toLowerCase() === slideId.toLowerCase());
        }
        
        // If still not found, try in objects map
        if (!slide) {
          slide = results.objects.get(slideId);
        }
        
        // Case-insensitive lookup in objects
        if (!slide) {
          for (const [key, value] of results.objects.entries()) {
            if (key.toLowerCase() === slideId.toLowerCase() && value.type === 'slide') {
              slide = value;
              break;
            }
          }
        }
        
        if (slide && slide.type === 'slide') {
          // Ensure slide has all required properties
          if (!slide.dimension) {
            console.warn(`Slide ${slideId} missing dimension, defaulting to 0D`);
            slide.dimension = '0D';
          }
          resolvedSlides.push(slide);
          console.log(`✓ Resolved slide: ${slideId} (dimension: ${slide.dimension})`);
        } else {
          missingSlides.push(slideId);
          console.error(`✗ Slide "${slideId}" not found. Available slide IDs:`, results.slides.map(s => s.id));
          console.error(`  Also checked objects map:`, Array.from(results.objects.keys()));
        }
      }
      
      if (missingSlides.length > 0) {
        console.error(`Failed to resolve ${missingSlides.length} slides:`, missingSlides);
        // Don't fail completely - use what we have
        console.warn(`Using ${resolvedSlides.length} resolved slides, ${missingSlides.length} missing`);
      }
      
      console.log(`Resolved ${resolvedSlides.length}/${deckObj.slides.length} slides from deck`);
      
      if (resolvedSlides.length > 0) {
        results.slides = resolvedSlides;
        // Store missing slides info for debugging
        if (missingSlides.length > 0) {
          results.missingSlides = missingSlides;
        }
      } else {
        console.warn('No slides resolved from deck, using all collected slides');
      }
    } else {
      console.log('No deck object found, using all collected slides');
    }

    console.log(`Final slides array: ${results.slides.length} slides`);
    return results;
  }
}
