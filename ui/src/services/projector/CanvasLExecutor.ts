/**
 * CanvasLExecutor - Executes CanvasL objects with Meta-Log integration
 * 
 * Uses CanvasLMetaverseBrowser from meta-log-db for unified execution.
 * Handles execution of different CanvasL object types:
 * - rdf-triple: Add to RDF store
 * - r5rs-call: Execute R5RS function
 * - sparql-construct: Execute SPARQL CONSTRUCT query
 * - prolog-query: Execute ProLog query
 * - datalog-query: Execute DataLog query
 * - shacl-validate: Validate with SHACL
 */

import { MetaLogBridge } from './MetaLogBridge';

export interface ExecutionResult {
  triples?: any[];
  slides?: any[];
  objects?: Map<string, any>;
  errors?: Array<{ object: any; error: string }>;
  missingSlides?: string[];
  [key: string]: any;
}

export class CanvasLExecutor {
  private metaLog: MetaLogBridge;
  private browser: any; // CanvasLMetaverseBrowser

  constructor(metaLogBridge: MetaLogBridge) {
    this.metaLog = metaLogBridge;
    // Access browser from bridge
    this.browser = metaLogBridge.adapter;
  }

  /**
   * Execute a CanvasL object
   * Uses unified CanvasLMetaverseBrowser.executeCanvasLObject if available
   */
  async execute(obj: any): Promise<any> {
    // Use unified browser execution if available
    if (this.browser && typeof this.browser.executeCanvasLObject === 'function') {
      try {
        const result = await this.browser.executeCanvasLObject(obj);
        // Convert unified result format to legacy format
        if (result.type === 'r5rs-result' || result.type === 'prolog-result' || 
            result.type === 'datalog-result' || result.type === 'sparql-result' || 
            result.type === 'shacl-result') {
          return { type: result.type, result: result.result };
        }
        return result.result || result;
      } catch (error) {
        console.warn('Unified execution failed, falling back to legacy:', error);
        // Fall through to legacy implementation
      }
    }

    // Legacy implementation (fallback)
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
   */
  private async executeRdfTriple(obj: any): Promise<any> {
    this.metaLog.addTriple(obj.subject, obj.predicate, obj.object);
    return { type: 'rdf-triple', subject: obj.subject, predicate: obj.predicate, object: obj.object };
  }

  /**
   * Execute R5RS function call
   */
  private async executeR5RSCall(obj: any): Promise<any> {
    const result = await this.metaLog.evalR5RS(obj.expression, obj.context || {});
    return { type: 'r5rs-result', result };
  }

  /**
   * Execute SPARQL CONSTRUCT query
   */
  private async executeSparqlConstruct(obj: any): Promise<any> {
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
   */
  private async executePrologQuery(obj: any): Promise<any> {
    const query = obj.query || obj.goal;
    const facts = obj.facts || [];
    
    const result = await this.metaLog.prologQuery(query, facts);
    return { type: 'prolog-result', result };
  }

  /**
   * Execute DataLog query
   */
  private async executeDatalogQuery(obj: any): Promise<any> {
    const goal = obj.goal || obj.query;
    const program = obj.program || null;
    
    const result = await this.metaLog.datalogQuery(goal, program);
    return { type: 'datalog-result', result };
  }

  /**
   * Execute SHACL validation
   */
  private async executeShaclValidate(obj: any): Promise<any> {
    const shapes = obj.shapes || obj.shape;
    const focus = obj.focus || null;
    
    const result = await this.metaLog.shaclValidate(shapes, focus);
    return { type: 'shacl-result', result };
  }

  /**
   * Execute multiple objects
   * Uses unified CanvasLMetaverseBrowser.executeCanvasLObjects if available
   */
  async executeAll(objects: any[]): Promise<ExecutionResult> {
    // Use unified browser execution if available
    if (this.browser && typeof this.browser.executeCanvasLObjects === 'function') {
      try {
        return await this.browser.executeCanvasLObjects(objects);
      } catch (error) {
        console.warn('Unified batch execution failed, falling back to legacy:', error);
        // Fall through to legacy implementation
      }
    }

    // Legacy implementation (fallback)
    const results: ExecutionResult = {
      triples: [],
      slides: [],
      objects: new Map(), // Store all objects by ID
      errors: []
    };

    for (const obj of objects) {
      try {
        // Skip @include directives (should be expanded already, but check anyway)
        if ((obj as any)['@include'] || obj.type === '@include') {
          continue;
        }
        
        // Skip version directives
        if ((obj as any)['@version']) {
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
          
          results.slides!.push(obj);
          if (obj.id) {
            results.objects!.set(obj.id, obj);
          }
          console.log(`Found slide: ${obj.id} (dimension: ${obj.dimension})`);
          continue;
        }

        const result = await this.execute(obj);
        
        // Store object by ID if it has one
        if (obj.id) {
          results.objects!.set(obj.id, result);
        }
        
        if (result.type === 'rdf-triple') {
          results.triples!.push(result);
        } else if (result.type === 'slide' || obj.type === 'slide') {
          // Collect slides (use original obj if result doesn't have slide properties)
          const slide = result.type === 'slide' ? result : obj;
          results.slides!.push(slide);
        } else {
          (results as any)[obj.id || 'results'] = result;
        }
      } catch (error: any) {
        results.errors!.push({
          object: obj,
          error: error.message
        });
      }
    }

    console.log(`CanvasLExecutor: Found ${results.slides!.length} slides, ${results.objects!.size} objects`);
    console.log(`All slide IDs found:`, results.slides!.map((s: any) => ({ id: s.id, dimension: s.dimension })));

    // If deck object exists, resolve slide references
    const deckObj = Array.from(results.objects!.values()).find((o: any) => o.type === 'deck' || o.id?.includes('deck'));
    if (deckObj && deckObj.slides && Array.isArray(deckObj.slides)) {
      console.log(`Deck found with ${deckObj.slides.length} slide references:`, deckObj.slides);
      console.log(`Available slides before resolution:`, results.slides!.map((s: any) => s.id));
      
      // Resolve slide IDs to actual slide objects
      const resolvedSlides: any[] = [];
      const missingSlides: string[] = [];
      
      for (const slideId of deckObj.slides) {
        // Try multiple ways to find the slide
        let slide = results.slides!.find((s: any) => s.id === slideId);
        
        // If not found, try case-insensitive match
        if (!slide) {
          slide = results.slides!.find((s: any) => s.id && s.id.toLowerCase() === slideId.toLowerCase());
        }
        
        // If still not found, try in objects map
        if (!slide) {
          slide = results.objects!.get(slideId);
        }
        
        // Case-insensitive lookup in objects
        if (!slide) {
          for (const [key, value] of results.objects!.entries()) {
            if (key.toLowerCase() === slideId.toLowerCase() && (value as any).type === 'slide') {
              slide = value;
              break;
            }
          }
        }
        
        if (slide && (slide as any).type === 'slide') {
          // Ensure slide has all required properties
          if (!(slide as any).dimension) {
            console.warn(`Slide ${slideId} missing dimension, defaulting to 0D`);
            (slide as any).dimension = '0D';
          }
          resolvedSlides.push(slide);
          console.log(`✓ Resolved slide: ${slideId} (dimension: ${(slide as any).dimension})`);
        } else {
          missingSlides.push(slideId);
          console.error(`✗ Slide "${slideId}" not found. Available slide IDs:`, results.slides!.map((s: any) => s.id));
          console.error(`  Also checked objects map:`, Array.from(results.objects!.keys()));
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

    console.log(`Final slides array: ${results.slides!.length} slides`);
    return results;
  }
}

