/**
 * Browser JSONL Parser
 * 
 * Browser-specific JSONL parser using BrowserFileIO instead of Node.js fs
 */

import { BrowserFileIO } from '../io.js';
import { Fact, Canvas, RdfTriple } from '../../types/index.js';
import { decryptFileContent } from '../crypto/storage-encryption.js';

export interface BrowserJsonlParserConfig {
  enableEncryption?: boolean;
  mnemonic?: string;
  encryptionPurpose?: 'local' | 'published' | 'contributor' | 'ephemeral';
  fileIO?: BrowserFileIO;
}

/**
 * Browser JSONL Parser
 */
export class BrowserJsonlParser {
  private facts: Fact[] = [];
  private canvas: Canvas | null = null;
  private fileIO: BrowserFileIO;
  private enableEncryption: boolean;
  private mnemonic?: string;
  private encryptionPurpose: 'local' | 'published' | 'contributor' | 'ephemeral';

  constructor(config: BrowserJsonlParserConfig = {}) {
    this.fileIO = config.fileIO || new BrowserFileIO();
    this.enableEncryption = config.enableEncryption || false;
    this.mnemonic = config.mnemonic;
    this.encryptionPurpose = config.encryptionPurpose || 'local';
  }

  /**
   * Initialize file I/O
   */
  async init(): Promise<void> {
    await this.fileIO.init();
  }

  /**
   * Parse JSONL file from URL or IndexedDB
   */
  async parse(filePath: string, url?: string): Promise<Canvas> {
    let content: string;

    try {
      // Load file content
      content = await this.fileIO.loadFile(filePath, url);

      // Decrypt if encryption is enabled
      if (this.enableEncryption && this.mnemonic) {
        try {
          content = await decryptFileContent(content, this.mnemonic, this.encryptionPurpose);
        } catch (error) {
          // If decryption fails, assume content is not encrypted
          console.warn('Decryption failed, assuming unencrypted content:', error);
        }
      }
    } catch (error) {
      throw new Error(`Failed to load file ${filePath}: ${error}`);
    }

    // Parse JSONL content
    const lines = content.split('\n').filter(line => line.trim());
    const objects: any[] = [];

    for (const line of lines) {
      if (line.trim()) {
        try {
          const obj = JSON.parse(line);
          objects.push(obj);
        } catch (error) {
          console.warn(`Failed to parse line: ${line}`, error);
        }
      }
    }

    this.canvas = this.organizeCanvas(objects);
    return this.canvas;
  }

  /**
   * Parse CanvasL file (with extensions)
   */
  async parseCanvasL(filePath: string, url?: string): Promise<Canvas> {
    let content: string;

    try {
      // Load file content
      content = await this.fileIO.loadFile(filePath, url);

      // Decrypt if encryption is enabled
      if (this.enableEncryption && this.mnemonic) {
        try {
          content = await decryptFileContent(content, this.mnemonic, this.encryptionPurpose);
        } catch (error) {
          // If decryption fails, assume content is not encrypted
          console.warn('Decryption failed, assuming unencrypted content:', error);
        }
      }
    } catch (error) {
      throw new Error(`Failed to load CanvasL file ${filePath}: ${error}`);
    }

    // Parse CanvasL content
    const lines = content.split('\n');
    const objects: any[] = [];
    let currentDirective: string | null = null;

    for (const line of lines) {
      const trimmed = line.trim();
      
      // Handle directives
      if (trimmed.startsWith('@')) {
        const match = trimmed.match(/^@(\w+)\s*(.*)$/);
        if (match) {
          currentDirective = match[1];
          if (match[2]) {
            objects.push({ type: `@${currentDirective}`, value: match[2] });
          }
        }
        continue;
      }

      // Parse JSONL lines
      if (trimmed && trimmed.startsWith('{')) {
        try {
          const obj = JSON.parse(trimmed);
          if (currentDirective) {
            obj._directive = `@${currentDirective}`;
          }
          objects.push(obj);
        } catch (error) {
          console.warn(`Failed to parse CanvasL line: ${trimmed}`, error);
        }
      }
    }

    this.canvas = this.organizeCanvas(objects);
    return this.canvas;
  }

  /**
   * Organize parsed objects into canvas structure
   */
  private organizeCanvas(objects: any[]): Canvas {
    const canvas: Canvas = {
      nodes: [],
      edges: [],
    };

    for (const obj of objects) {
      if (obj.type === 'node' || obj.id) {
        canvas.nodes = canvas.nodes || [];
        canvas.nodes.push(obj);
      } else if (obj.type === 'edge' || obj.fromNode || obj.toNode) {
        canvas.edges = canvas.edges || [];
        canvas.edges.push(obj);
      } else {
        // Store other objects
        const key = obj.type || 'other';
        if (!canvas[key]) {
          canvas[key] = [];
        }
        (canvas[key] as any[]).push(obj);
      }
    }

    return canvas;
  }

  /**
   * Extract facts from canvas
   */
  extractFacts(canvas?: Canvas): Fact[] {
    const targetCanvas = canvas || this.canvas;
    if (!targetCanvas) {
      return [];
    }

    this.facts = [];

    // Extract node facts
    if (targetCanvas.nodes) {
      for (const node of targetCanvas.nodes) {
        this.facts.push({
          predicate: 'node',
          args: [node.id, node.type || 'unknown', node.x || 0, node.y || 0, node.text || '']
        });

        // Extract node properties as facts
        for (const [key, value] of Object.entries(node)) {
          if (!['id', 'type', 'x', 'y', 'text'].includes(key)) {
            this.facts.push({
              predicate: `node_${key}`,
              args: [node.id, value]
            });
          }
        }
      }
    }

    // Extract edge facts
    if (targetCanvas.edges) {
      for (const edge of targetCanvas.edges) {
        this.facts.push({
          predicate: 'edge',
          args: [edge.id, edge.type || 'unknown', edge.fromNode, edge.toNode]
        });

        // Extract vertical/horizontal relationships
        if (edge.type?.startsWith('v:')) {
          this.facts.push({
            predicate: 'vertical',
            args: [edge.id, edge.fromNode, edge.toNode]
          });
        } else if (edge.type?.startsWith('h:')) {
          this.facts.push({
            predicate: 'horizontal',
            args: [edge.id, edge.fromNode, edge.toNode]
          });
        }
      }
    }

    return this.facts;
  }

  /**
   * Convert facts to RDF triples
   */
  toRdf(facts?: Fact[]): RdfTriple[] {
    const targetFacts = facts || this.facts;
    const triples: RdfTriple[] = [];

    for (const fact of targetFacts) {
      if (fact.predicate === 'node' && fact.args.length >= 2) {
        const [id, type] = fact.args;
        triples.push({
          subject: `<http://example.org/node/${id}>`,
          predicate: '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
          object: `<http://example.org/type/${type}>`
        });
      } else if (fact.predicate === 'edge' && fact.args.length >= 4) {
        const [id, type, fromNode, toNode] = fact.args;
        triples.push({
          subject: `<http://example.org/edge/${id}>`,
          predicate: '<http://example.org/predicate/fromNode>',
          object: `<http://example.org/node/${fromNode}>`
        });
        triples.push({
          subject: `<http://example.org/edge/${id}>`,
          predicate: '<http://example.org/predicate/toNode>',
          object: `<http://example.org/node/${toNode}>`
        });
        triples.push({
          subject: `<http://example.org/edge/${id}>`,
          predicate: '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
          object: `<http://example.org/type/${type}>`
        });
      }
    }

    return triples;
  }

  /**
   * Get extracted facts
   */
  getFacts(): Fact[] {
    return [...this.facts];
  }

  /**
   * Clear facts
   */
  clear(): void {
    this.facts = [];
    this.canvas = null;
  }
}

