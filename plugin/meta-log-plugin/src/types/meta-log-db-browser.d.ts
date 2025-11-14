/**
 * Type declarations for meta-log-db/browser module
 * This module may not have type definitions available at compile time
 */

declare module 'meta-log-db/browser' {
  export interface CanvasLBrowserConfig {
    enableProlog?: boolean;
    enableDatalog?: boolean;
    enableRdf?: boolean;
    enableShacl?: boolean;
    indexedDBName?: string;
  }

  export class CanvasLMetaverseBrowser {
    constructor(config?: CanvasLBrowserConfig);
    init(): Promise<void>;
    loadCanvas(canvasPath: string, url?: string): Promise<void>;
    [key: string]: any;
  }
}

