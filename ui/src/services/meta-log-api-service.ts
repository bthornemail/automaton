/**
 * Meta-Log API Service
 * Provides browser-native access to Meta-Log database functionality
 * Uses MetaLogDbBrowser for ProLog, DataLog, and SPARQL queries
 * Falls back to API if browser implementation is not available
 */

import { apiService } from './api';
import { getMetaLogBrowserAdapter } from './meta-log-browser-adapter';

export interface MetaLogApiService {
  prologQuery(query: string, canvasFile?: string): Promise<any>;
  datalogQuery(query: string, program?: any, canvasFile?: string): Promise<any>;
  sparqlQuery(query: string, canvasFile?: string): Promise<any>;
  loadCanvas(canvasFile: string): Promise<void>;
  isAvailable(): boolean;
}

class MetaLogApiServiceImpl implements MetaLogApiService {
  private available: boolean = false;
  private browserAdapter = getMetaLogBrowserAdapter();
  private useBrowser: boolean = true; // Prefer browser implementation

  constructor() {
    // Initialize browser adapter
    this.initializeBrowser();
    // Check if Meta-Log API is available as fallback
    this.checkApiAvailability();
  }

  private async initializeBrowser(): Promise<void> {
    try {
      await this.browserAdapter.init();
      this.useBrowser = true;
      console.log('✓ Meta-Log browser implementation initialized');
    } catch (error) {
      console.warn('Meta-Log browser implementation not available, will use API fallback:', error);
      this.useBrowser = false;
    }
  }

  private async checkApiAvailability(): Promise<void> {
    try {
      // Try to ping the API endpoint
      const response = await fetch('/api/meta-log/health', { method: 'HEAD' });
      this.available = response.ok;
    } catch {
      this.available = false;
    }
  }

  isAvailable(): boolean {
    return this.useBrowser || this.available;
  }

  async loadCanvas(canvasFile: string): Promise<void> {
    // Try browser implementation first
    if (this.useBrowser) {
      try {
        const url = canvasFile.startsWith('http://') || canvasFile.startsWith('https://') || canvasFile.startsWith('/')
          ? canvasFile
          : `/jsonl/${canvasFile}`;
        await this.browserAdapter.loadCanvas(canvasFile, url);
        return;
      } catch (error) {
        console.warn('Failed to load canvas via browser implementation, trying API...', error);
      }
    }

    // Fallback to API
    if (this.available) {
      try {
        await apiService.post('/api/meta-log/load', { canvasFile });
      } catch (error) {
        console.warn('Failed to load canvas via Meta-Log API:', error);
      }
    }
  }

  async prologQuery(query: string, canvasFile?: string): Promise<any> {
    // Try browser implementation first
    if (this.useBrowser) {
      try {
        // Load canvas if provided
        if (canvasFile) {
          await this.loadCanvas(canvasFile);
        }
        return await this.browserAdapter.prologQuery(query);
      } catch (error) {
        console.warn('Meta-Log Prolog query via browser failed, trying API...', error);
      }
    }

    // Fallback to API
    if (!this.available) {
      throw new Error('Meta-Log not available (browser or API)');
    }

    try {
      const response = await apiService.post('/api/meta-log/prolog', {
        query,
        canvasFile
      });
      return response.data;
    } catch (error) {
      console.error('Meta-Log Prolog query failed:', error);
      throw error;
    }
  }

  async datalogQuery(query: string, program?: any, canvasFile?: string): Promise<any> {
    // Try browser implementation first
    if (this.useBrowser) {
      try {
        // Load canvas if provided
        if (canvasFile) {
          await this.loadCanvas(canvasFile);
        }
        return await this.browserAdapter.datalogQuery(query, program);
      } catch (error) {
        console.warn('Meta-Log DataLog query via browser failed, trying API...', error);
      }
    }

    // Fallback to API
    if (!this.available) {
      throw new Error('Meta-Log not available (browser or API)');
    }

    try {
      const response = await apiService.post('/api/meta-log/datalog', {
        query,
        program,
        canvasFile
      });
      return response.data;
    } catch (error) {
      console.error('Meta-Log DataLog query failed:', error);
      throw error;
    }
  }

  async sparqlQuery(query: string, canvasFile?: string): Promise<any> {
    // Try browser implementation first
    if (this.useBrowser) {
      try {
        // Load canvas if provided
        if (canvasFile) {
          await this.loadCanvas(canvasFile);
        }
        return await this.browserAdapter.sparqlQuery(query);
      } catch (error) {
        console.warn('Meta-Log SPARQL query via browser failed, trying API...', error);
      }
    }

    // Fallback to API
    if (!this.available) {
      throw new Error('Meta-Log not available (browser or API)');
    }

    try {
      const response = await apiService.post('/api/meta-log/sparql', {
        query,
        canvasFile
      });
      return response.data;
    } catch (error) {
      console.error('Meta-Log SPARQL query failed:', error);
      throw error;
    }
  }
}

export const metaLogApiService: MetaLogApiService = new MetaLogApiServiceImpl();
