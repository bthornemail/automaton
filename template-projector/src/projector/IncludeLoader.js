/**
 * IncludeLoader - Handles @include directive for CanvasL files
 * 
 * Loads and includes CanvasL files recursively, handling circular dependencies
 * and caching loaded files.
 */

export class IncludeLoader {
  constructor() {
    this.loadedFiles = new Map();
    this.loadingFiles = new Set();
    this.basePath = '';
  }

  /**
   * Set base path for resolving relative includes
   * @param {string} basePath - Base path (e.g., '/templates/')
   */
  setBasePath(basePath) {
    this.basePath = basePath;
  }

  /**
   * Resolve include path
   * @param {string} includePath - Include path from @include directive
   * @param {string} currentPath - Current file path
   * @returns {string} Resolved path
   */
  resolvePath(includePath, currentPath = '') {
    // If absolute URL, use as-is
    if (includePath.startsWith('http://') || includePath.startsWith('https://')) {
      return includePath;
    }

    // If relative path, resolve relative to current file or base path
    if (includePath.startsWith('./') || includePath.startsWith('../')) {
      if (currentPath) {
        const currentDir = currentPath.substring(0, currentPath.lastIndexOf('/') + 1);
        return currentDir + includePath;
      }
      return this.basePath + includePath;
    }

    // Otherwise, resolve relative to base path
    return this.basePath + includePath;
  }

  /**
   * Load and parse CanvasL file
   * @param {string} url - File URL
   * @returns {Promise<Array>} Parsed objects
   */
  async loadFile(url) {
    // Check if already loaded
    if (this.loadedFiles.has(url)) {
      return this.loadedFiles.get(url);
    }

    // Check for circular dependency
    if (this.loadingFiles.has(url)) {
      console.warn(`Circular dependency detected: ${url}`);
      return [];
    }

    this.loadingFiles.add(url);

    try {
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error(`Failed to load ${url}: ${response.statusText}`);
      }

      const content = await response.text();
      const objects = this.parseCanvasL(content, url);
      
      // Process @include directives
      const expanded = await this.expandIncludes(objects, url);
      
      // Cache loaded file
      this.loadedFiles.set(url, expanded);
      this.loadingFiles.delete(url);
      
      return expanded;
    } catch (error) {
      this.loadingFiles.delete(url);
      console.error(`Failed to load include file ${url}:`, error);
      throw error;
    }
  }

  /**
   * Parse CanvasL content
   * @param {string} content - File content
   * @param {string} sourceUrl - Source URL for error reporting
   * @returns {Array} Parsed objects
   */
  parseCanvasL(content, sourceUrl = '') {
    const lines = content.split('\n');
    const objects = [];
    let currentDirective = null;

    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      
      // Handle directives
      if (line.startsWith('@')) {
        const match = line.match(/^@(\w+)\s*(.*)$/);
        if (match) {
          const directive = match[1];
          const value = match[2].trim();
          
          if (directive === 'include') {
            objects.push({
              type: '@include',
              path: value,
              _sourceLine: i + 1,
              _sourceUrl: sourceUrl
            });
          } else {
            objects.push({
              type: `@${directive}`,
              value: value || true,
              _sourceLine: i + 1,
              _sourceUrl: sourceUrl
            });
          }
        }
        continue;
      }

      // Parse JSONL lines
      if (line && line.startsWith('{')) {
        try {
          const obj = JSON.parse(line);
          obj._sourceLine = i + 1;
          obj._sourceUrl = sourceUrl;
          objects.push(obj);
        } catch (error) {
          console.warn(`Failed to parse line ${i + 1} in ${sourceUrl}: ${error.message}`);
        }
      }
    }

    return objects;
  }

  /**
   * Expand @include directives
   * @param {Array} objects - Parsed objects
   * @param {string} currentUrl - Current file URL
   * @returns {Promise<Array>} Expanded objects
   */
  async expandIncludes(objects, currentUrl = '') {
    const expanded = [];

    for (const obj of objects) {
      if (obj.type === '@include') {
        // Resolve include path
        const includePath = this.resolvePath(obj.path, currentUrl);
        
        try {
          // Load included file
          const includedObjects = await this.loadFile(includePath);
          
          // Add included objects
          expanded.push(...includedObjects);
        } catch (error) {
          console.error(`Failed to include ${obj.path} from ${currentUrl}:`, error);
          // Continue with other objects
        }
      } else {
        expanded.push(obj);
      }
    }

    return expanded;
  }

  /**
   * Clear cache
   */
  clearCache() {
    this.loadedFiles.clear();
    this.loadingFiles.clear();
  }

  /**
   * Get cache statistics
   * @returns {Object} Cache stats
   */
  getCacheStats() {
    return {
      loadedFiles: this.loadedFiles.size,
      cachedUrls: Array.from(this.loadedFiles.keys())
    };
  }
}
