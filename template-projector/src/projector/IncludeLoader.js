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
    this.loadErrors = []; // Track failed loads
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

    // If path starts with /, it's absolute from root
    if (includePath.startsWith('/')) {
      return includePath;
    }

    // If relative path (./ or ../), resolve relative to current file
    if (includePath.startsWith('./') || includePath.startsWith('../')) {
      if (currentPath) {
        const currentDir = currentPath.substring(0, currentPath.lastIndexOf('/') + 1);
        return currentDir + includePath;
      }
      return includePath; // If no current path, use as-is
    }

    // Otherwise, treat as relative to project root
    // Paths like "templates/slides/..." should be resolved from root, not from basePath
    // This prevents double-pathing like "templates/documents/templates/slides/..."
    return includePath;
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
        throw new Error(`Failed to load ${url}: ${response.statusText} (${response.status})`);
      }

      const content = await response.text();
      if (!content || content.trim().length === 0) {
        console.warn(`Empty file loaded: ${url}`);
        this.loadingFiles.delete(url);
        return [];
      }

      const objects = this.parseCanvasL(content, url);
      
      // Process @include directives recursively
      const expanded = await this.expandIncludes(objects, url);
      
      // Cache loaded file
      this.loadedFiles.set(url, expanded);
      this.loadingFiles.delete(url);
      
      return expanded;
    } catch (error) {
      this.loadingFiles.delete(url);
      console.error(`Failed to load include file ${url}:`, error);
      // Store error for debugging
      if (!this.loadErrors) this.loadErrors = [];
      this.loadErrors.push({ url, error: error.message });
      // Don't throw - return empty array so other includes can still work
      return [];
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

      // Parse JSONL lines (handle multi-line JSON objects)
      if (line && line.startsWith('{')) {
        try {
          // Try parsing the line as-is first
          const obj = JSON.parse(line);
          obj._sourceLine = i + 1;
          obj._sourceUrl = sourceUrl;
          objects.push(obj);
        } catch (error) {
          // If parsing fails, try accumulating lines until we have valid JSON
          // This handles multi-line JSON objects
          let jsonStr = line;
          let braceCount = (jsonStr.match(/{/g) || []).length - (jsonStr.match(/}/g) || []).length;
          let j = i + 1;
          
          // Accumulate lines until braces are balanced
          while (braceCount > 0 && j < lines.length) {
            jsonStr += '\n' + lines[j];
            braceCount += (lines[j].match(/{/g) || []).length - (lines[j].match(/}/g) || []).length;
            j++;
          }
          
          try {
            const obj = JSON.parse(jsonStr);
            obj._sourceLine = i + 1;
            obj._sourceUrl = sourceUrl;
            objects.push(obj);
            // Skip the lines we already processed
            i = j - 1;
          } catch (nestedError) {
            // If still fails, log warning and continue
            console.warn(`Failed to parse line ${i + 1} in ${sourceUrl}: ${error.message}`);
          }
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
      // Handle @include directive (check both type and property)
      if (obj.type === '@include' || obj['@include']) {
        // Get include path
        const includePath = obj.path || obj['@include'];
        if (!includePath) {
          console.warn('@include directive missing path:', obj);
          continue;
        }
        
        // Resolve include path
        const resolvedPath = this.resolvePath(includePath, currentUrl);
        
        console.log(`Expanding @include: ${includePath} -> ${resolvedPath}`);
        
        try {
          // Load included file
          const includedObjects = await this.loadFile(resolvedPath);
          
          if (includedObjects.length === 0) {
            console.warn(`No objects loaded from ${resolvedPath} - file may be empty or failed to parse`);
          } else {
            console.log(`Loaded ${includedObjects.length} objects from ${resolvedPath}`);
          }
          
          // Add included objects
          expanded.push(...includedObjects);
        } catch (error) {
          console.error(`Failed to include ${includePath} from ${currentUrl}:`, error);
          // Store error for debugging
          if (!this.loadErrors) this.loadErrors = [];
          this.loadErrors.push({ url: resolvedPath, error: error.message, from: currentUrl });
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
