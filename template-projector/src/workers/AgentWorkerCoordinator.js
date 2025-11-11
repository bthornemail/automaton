/**
 * AgentWorkerCoordinator - Manages Web Workers for agent operations
 * 
 * Coordinates off-canvas agent processing and suggestion generation
 */

export class AgentWorkerCoordinator {
  constructor() {
    this.workers = new Map(); // Map<dimension, Worker>
    this.suggestionCallbacks = new Map(); // Map<requestId, callback>
    this.requestId = 0;
  }

  /**
   * Get or create worker for dimension
   */
  getWorker(dimension) {
    if (!this.workers.has(dimension)) {
      const worker = new Worker(
        new URL('./agent-worker.js', import.meta.url),
        { type: 'module' }
      );
      
      worker.onmessage = (e) => this.handleWorkerMessage(e, dimension);
      worker.onerror = (error) => {
        console.error(`Worker error (${dimension}):`, error);
      };
      
      this.workers.set(dimension, worker);
    }
    
    return this.workers.get(dimension);
  }

  /**
   * Handle messages from workers
   */
  handleWorkerMessage(e, dimension) {
    const { type, payload, error } = e.data;
    
    if (error) {
      console.error(`Worker error (${dimension}):`, error);
      return;
    }
    
    switch (type) {
      case 'POPULATE_SLIDE_RESULT':
        this.handlePopulateResult(payload);
        break;
      
      case 'SUGGESTIONS_RESULT':
        this.handleSuggestionsResult(payload);
        break;
      
      case 'ANALYSIS_RESULT':
        this.handleAnalysisResult(payload);
        break;
      
      default:
        console.warn(`Unknown worker message type: ${type}`);
    }
  }

  /**
   * Handle populate slide result
   */
  handlePopulateResult(payload) {
    const { slide, suggestions } = payload;
    
    // Emit event for slide population
    this.emitEvent('slidePopulated', { slide, suggestions });
  }

  /**
   * Handle suggestions result
   */
  handleSuggestionsResult(payload) {
    const { suggestions, component, action } = payload;
    
    // Emit event for suggestions
    this.emitEvent('suggestionsGenerated', { suggestions, component, action });
  }

  /**
   * Handle analysis result
   */
  handleAnalysisResult(payload) {
    const { analysis, component } = payload;
    
    // Emit event for analysis
    this.emitEvent('componentAnalyzed', { analysis, component });
  }

  /**
   * Populate slide using worker
   */
  async populateSlide(slide, contentData, dimension) {
    return new Promise((resolve, reject) => {
      const worker = this.getWorker(dimension);
      const requestId = ++this.requestId;
      
      // Store callback
      this.suggestionCallbacks.set(requestId, { resolve, reject, type: 'populate' });
      
      // Set up one-time listener for result
      const listener = (e) => {
        if (e.data.type === 'POPULATE_SLIDE_RESULT') {
          worker.removeEventListener('message', listener);
          const { slide: populatedSlide, suggestions } = e.data.payload;
          resolve({ slide: populatedSlide, suggestions });
        }
      };
      worker.addEventListener('message', listener);
      
      // Send populate request
      worker.postMessage({
        type: 'POPULATE_SLIDE',
        payload: { slide, contentData, dimension }
      });
      
      // Timeout after 10 seconds
      setTimeout(() => {
        worker.removeEventListener('message', listener);
        this.suggestionCallbacks.delete(requestId);
        reject(new Error('Populate request timeout'));
      }, 10000);
    });
  }

  /**
   * Generate suggestions for component click
   */
  async generateSuggestions(slide, component, action) {
    return new Promise((resolve, reject) => {
      const dimension = slide.dimension || '0D';
      const worker = this.getWorker(dimension);
      const requestId = ++this.requestId;
      
      // Set up one-time listener
      const listener = (e) => {
        if (e.data.type === 'SUGGESTIONS_RESULT') {
          worker.removeEventListener('message', listener);
          resolve(e.data.payload.suggestions);
        }
      };
      worker.addEventListener('message', listener);
      
      // Send suggestion request
      worker.postMessage({
        type: 'GENERATE_SUGGESTIONS',
        payload: { slide, component, action }
      });
      
      // Timeout after 5 seconds
      setTimeout(() => {
        worker.removeEventListener('message', listener);
        reject(new Error('Suggestion request timeout'));
      }, 5000);
    });
  }

  /**
   * Analyze component
   */
  async analyzeComponent(component, slide, contentData) {
    return new Promise((resolve, reject) => {
      const dimension = slide.dimension || '0D';
      const worker = this.getWorker(dimension);
      
      // Set up one-time listener
      const listener = (e) => {
        if (e.data.type === 'ANALYSIS_RESULT') {
          worker.removeEventListener('message', listener);
          resolve(e.data.payload.analysis);
        }
      };
      worker.addEventListener('message', listener);
      
      // Send analysis request
      worker.postMessage({
        type: 'ANALYZE_COMPONENT',
        payload: { component, slide, contentData }
      });
      
      // Timeout after 5 seconds
      setTimeout(() => {
        worker.removeEventListener('message', listener);
        reject(new Error('Analysis request timeout'));
      }, 5000);
    });
  }

  /**
   * Emit custom event
   */
  emitEvent(eventName, data) {
    const event = new CustomEvent(`agentWorker:${eventName}`, { detail: data });
    window.dispatchEvent(event);
  }

  /**
   * Terminate all workers
   */
  terminate() {
    for (const [dimension, worker] of this.workers) {
      worker.terminate();
      console.log(`Terminated worker for ${dimension}`);
    }
    this.workers.clear();
    this.suggestionCallbacks.clear();
  }
}

