/**
 * Provenance Export Service
 * 
 * Provides export functionality for provenance chains in multiple formats:
 * - JSON/JSONL
 * - GraphML
 * - DOT (Graphviz)
 * - PNG/SVG images
 */

import { ProvenanceChain, ProvenanceNode, ProvenanceEdge } from './provenance-slide-service';

export type ExportFormat = 'json' | 'jsonl' | 'graphml' | 'dot' | 'png' | 'svg';

export interface ExportOptions {
  format: ExportFormat;
  filename?: string;
  includeMetadata?: boolean;
  imageOptions?: {
    width?: number;
    height?: number;
    backgroundColor?: string;
  };
}

export class ProvenanceExportService {
  /**
   * Export provenance chain to specified format.
   * 
   * Exports a provenance chain to the requested format and triggers download.
   * Supports JSON, JSONL, GraphML, DOT, PNG, and SVG formats.
   * 
   * @param {ProvenanceChain} chain - Provenance chain to export
   * @param {ExportOptions} options - Export options
   * @returns {Promise<void>} Promise that resolves when export completes
   * 
   * @example
   * ```typescript
   * await service.exportChain(chain, {
   *   format: 'json',
   *   filename: 'provenance-chain.json'
   * });
   * ```
   */
  async exportChain(chain: ProvenanceChain, options: ExportOptions): Promise<void> {
    const filename = options.filename || this.generateFilename(options.format);
    let content: string | Blob;
    let mimeType: string;

    switch (options.format) {
      case 'json':
        content = this.exportToJSON(chain, options);
        mimeType = 'application/json';
        break;
      case 'jsonl':
        content = this.exportToJSONL(chain, options);
        mimeType = 'application/x-ndjson';
        break;
      case 'graphml':
        content = this.exportToGraphML(chain, options);
        mimeType = 'application/xml';
        break;
      case 'dot':
        content = this.exportToDOT(chain, options);
        mimeType = 'text/plain';
        break;
      case 'png':
        return this.exportToImage(chain, 'png', filename, options);
      case 'svg':
        return this.exportToImage(chain, 'svg', filename, options);
      default:
        throw new Error(`Unsupported export format: ${options.format}`);
    }

    this.downloadFile(content, filename, mimeType);
  }

  /**
   * Export to JSON format.
   */
  private exportToJSON(chain: ProvenanceChain, options: ExportOptions): string {
    const exportData: any = {
      nodes: chain.nodes.map(node => this.serializeNode(node, options)),
      edges: chain.edges.map(edge => this.serializeEdge(edge, options)),
      metadata: {
        exportedAt: new Date().toISOString(),
        nodeCount: chain.nodes.length,
        edgeCount: chain.edges.length
      }
    };

    return JSON.stringify(exportData, null, 2);
  }

  /**
   * Export to JSONL format (one JSON object per line).
   */
  private exportToJSONL(chain: ProvenanceChain, options: ExportOptions): string {
    const lines: string[] = [];

    // Export nodes
    for (const node of chain.nodes) {
      const serialized = this.serializeNode(node, options);
      // Ensure type field is 'node' (not overwritten by node.type)
      lines.push(JSON.stringify({
        ...serialized,
        type: 'node'
      }));
    }

    // Export edges
    for (const edge of chain.edges) {
      const serialized = this.serializeEdge(edge, options);
      // Ensure type field is 'edge' (not overwritten by edge.type)
      lines.push(JSON.stringify({
        ...serialized,
        type: 'edge'
      }));
    }

    return lines.join('\n');
  }

  /**
   * Export to GraphML format.
   */
  private exportToGraphML(chain: ProvenanceChain, options: ExportOptions): string {
    const xml: string[] = [];
    
    xml.push('<?xml version="1.0" encoding="UTF-8"?>');
    xml.push('<graphml xmlns="http://graphml.graphdrawing.org/xmlns"');
    xml.push('         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"');
    xml.push('         xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns');
    xml.push('         http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">');
    
    // Define attribute keys
    xml.push('  <key id="d0" for="node" attr.name="type" attr.type="string"/>');
    xml.push('  <key id="d1" for="node" attr.name="dimension" attr.type="string"/>');
    xml.push('  <key id="d2" for="node" attr.name="pattern" attr.type="string"/>');
    xml.push('  <key id="d3" for="node" attr.name="agentId" attr.type="string"/>');
    xml.push('  <key id="d4" for="edge" attr.name="type" attr.type="string"/>');
    xml.push('  <key id="d5" for="edge" attr.name="weight" attr.type="double"/>');
    
    xml.push('  <graph id="provenance-chain" edgedefault="directed">');
    
    // Export nodes
    for (const node of chain.nodes) {
      xml.push(`    <node id="${this.escapeXml(node.id)}">`);
      xml.push(`      <data key="d0">${this.escapeXml(node.type)}</data>`);
      if (node.metadata.dimension) {
        xml.push(`      <data key="d1">${this.escapeXml(node.metadata.dimension)}</data>`);
      }
      if (node.metadata.pattern) {
        xml.push(`      <data key="d2">${this.escapeXml(node.metadata.pattern)}</data>`);
      }
      if (node.metadata.agentId) {
        xml.push(`      <data key="d3">${this.escapeXml(node.metadata.agentId)}</data>`);
      }
      if (options.includeMetadata) {
        xml.push(`      <data key="position">${node.position.join(',')}</data>`);
      }
      xml.push('    </node>');
    }
    
    // Export edges
    for (const edge of chain.edges) {
      xml.push(`    <edge id="${this.escapeXml(edge.id)}" source="${this.escapeXml(edge.from)}" target="${this.escapeXml(edge.to)}">`);
      xml.push(`      <data key="d4">${this.escapeXml(edge.type)}</data>`);
      if (edge.metadata.weight) {
        xml.push(`      <data key="d5">${edge.metadata.weight}</data>`);
      }
      xml.push('    </edge>');
    }
    
    xml.push('  </graph>');
    xml.push('</graphml>');
    
    return xml.join('\n');
  }

  /**
   * Export to DOT format (Graphviz).
   */
  private exportToDOT(chain: ProvenanceChain, options: ExportOptions): string {
    const lines: string[] = [];
    
    lines.push('digraph ProvenanceChain {');
    lines.push('  rankdir=LR;');
    lines.push('  node [shape=box, style=rounded];');
    
    // Export nodes
    for (const node of chain.nodes) {
      const label = this.escapeDotLabel(
        node.metadata.pattern || node.id || node.type
      );
      const color = this.getNodeColor(node.metadata.dimension);
      lines.push(`  "${this.escapeDotId(node.id)}" [label="${label}", color="${color}"];`);
    }
    
    // Export edges
    for (const edge of chain.edges) {
      const fromId = this.escapeDotId(edge.from);
      const toId = this.escapeDotId(edge.to);
      const label = edge.type ? ` [label="${this.escapeDotLabel(edge.type)}"]` : '';
      lines.push(`  "${fromId}" -> "${toId}"${label};`);
    }
    
    lines.push('}');
    
    return lines.join('\n');
  }

  /**
   * Export to PNG or SVG image.
   */
  private async exportToImage(
    chain: ProvenanceChain,
    format: 'png' | 'svg',
    filename: string,
    options: ExportOptions
  ): Promise<void> {
    // Create SVG representation
    const svg = this.generateSVG(chain, options);
    
    if (format === 'svg') {
      this.downloadFile(svg, filename, 'image/svg+xml');
      return;
    }
    
    // Convert SVG to PNG
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');
    if (!ctx) {
      throw new Error('Canvas context not available');
    }
    
    const width = options.imageOptions?.width || 1920;
    const height = options.imageOptions?.height || 1080;
    canvas.width = width;
    canvas.height = height;
    
    // Set background color
    if (options.imageOptions?.backgroundColor) {
      ctx.fillStyle = options.imageOptions.backgroundColor;
      ctx.fillRect(0, 0, width, height);
    }
    
    // Convert SVG to image
    const img = new Image();
    const svgBlob = new Blob([svg], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(svgBlob);
    
    return new Promise((resolve, reject) => {
      img.onload = () => {
        ctx.drawImage(img, 0, 0, width, height);
        canvas.toBlob((blob) => {
          if (blob) {
            this.downloadFile(blob, filename, 'image/png');
            URL.revokeObjectURL(url);
            resolve();
          } else {
            reject(new Error('Failed to create PNG blob'));
          }
        }, 'image/png');
      };
      img.onerror = () => {
        URL.revokeObjectURL(url);
        reject(new Error('Failed to load SVG image'));
      };
      img.src = url;
    });
  }

  /**
   * Generate SVG representation of provenance chain.
   */
  private generateSVG(chain: ProvenanceChain, options: ExportOptions): string {
    const width = options.imageOptions?.width || 1920;
    const height = options.imageOptions?.height || 1080;
    const padding = 50;
    
    // Calculate node positions (simple force-directed layout)
    const positions = this.calculateNodePositions(chain, width - padding * 2, height - padding * 2);
    
    const svg: string[] = [];
    svg.push(`<svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">`);
    
    // Background
    if (options.imageOptions?.backgroundColor) {
      svg.push(`<rect width="${width}" height="${height}" fill="${options.imageOptions.backgroundColor}"/>`);
    }
    
    // Draw edges first (so they appear behind nodes)
    for (const edge of chain.edges) {
      const fromNode = chain.nodes.find(n => n.id === edge.from);
      const toNode = chain.nodes.find(n => n.id === edge.to);
      if (fromNode && toNode) {
        const fromPos = positions.get(edge.from) || [0, 0];
        const toPos = positions.get(edge.to) || [0, 0];
        const x1 = fromPos[0] + padding;
        const y1 = fromPos[1] + padding;
        const x2 = toPos[0] + padding;
        const y2 = toPos[1] + padding;
        
        svg.push(`<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="#666" stroke-width="2" opacity="0.5"/>`);
      }
    }
    
    // Draw nodes
    for (const node of chain.nodes) {
      const pos = positions.get(node.id) || [0, 0];
      const x = pos[0] + padding;
      const y = pos[1] + padding;
      const color = this.getNodeColor(node.metadata.dimension);
      const label = node.metadata.pattern || node.id || node.type;
      
      // Node circle
      svg.push(`<circle cx="${x}" cy="${y}" r="20" fill="${color}" stroke="#333" stroke-width="2"/>`);
      
      // Node label
      svg.push(`<text x="${x}" y="${y + 5}" text-anchor="middle" font-size="12" fill="#fff">${this.escapeXml(label)}</text>`);
    }
    
    svg.push('</svg>');
    
    return svg.join('\n');
  }

  /**
   * Calculate node positions for visualization.
   */
  private calculateNodePositions(
    chain: ProvenanceChain,
    width: number,
    height: number
  ): Map<string, [number, number]> {
    const positions = new Map<string, [number, number]>();
    
    // Simple grid layout
    const cols = Math.ceil(Math.sqrt(chain.nodes.length));
    const rows = Math.ceil(chain.nodes.length / cols);
    const cellWidth = width / cols;
    const cellHeight = height / rows;
    
    chain.nodes.forEach((node, index) => {
      const col = index % cols;
      const row = Math.floor(index / cols);
      positions.set(node.id, [
        col * cellWidth + cellWidth / 2,
        row * cellHeight + cellHeight / 2
      ]);
    });
    
    return positions;
  }

  /**
   * Get color for node based on dimension.
   */
  private getNodeColor(dimension?: string): string {
    const colorMap: Record<string, string> = {
      '0D': '#ef4444',
      '1D': '#f59e0b',
      '2D': '#eab308',
      '3D': '#84cc16',
      '4D': '#22c55e',
      '5D': '#10b981',
      '6D': '#14b8a6',
      '7D': '#06b6d4'
    };
    return colorMap[dimension || ''] || '#6b7280';
  }

  /**
   * Serialize node for export.
   */
  private serializeNode(node: ProvenanceNode, options: ExportOptions): any {
    const serialized: any = {
      id: node.id,
      type: node.type
    };
    
    if (options.includeMetadata) {
      serialized.position = node.position;
      serialized.metadata = node.metadata;
      serialized.data = node.data;
    } else {
      serialized.metadata = {
        dimension: node.metadata.dimension,
        pattern: node.metadata.pattern,
        agentId: node.metadata.agentId,
        timestamp: node.metadata.timestamp
      };
    }
    
    return serialized;
  }

  /**
   * Serialize edge for export.
   */
  private serializeEdge(edge: ProvenanceEdge, options: ExportOptions): any {
    const serialized: any = {
      id: edge.id,
      edgeType: edge.type,  // Use edgeType to avoid conflict with JSONL 'type' field
      from: edge.from,
      to: edge.to
    };
    
    if (options.includeMetadata) {
      serialized.metadata = edge.metadata;
    } else {
      serialized.metadata = {
        weight: edge.metadata.weight,
        timestamp: edge.metadata.timestamp
      };
    }
    
    return serialized;
  }

  /**
   * Generate filename based on format.
   */
  private generateFilename(format: ExportFormat): string {
    const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);
    const extensions: Record<ExportFormat, string> = {
      json: 'json',
      jsonl: 'jsonl',
      graphml: 'graphml',
      dot: 'dot',
      png: 'png',
      svg: 'svg'
    };
    return `provenance-chain-${timestamp}.${extensions[format]}`;
  }

  /**
   * Download file to user's computer.
   */
  private downloadFile(content: string | Blob, filename: string, mimeType: string): void {
    const blob = typeof content === 'string' 
      ? new Blob([content], { type: mimeType })
      : content;
    
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  }

  /**
   * Escape XML special characters.
   */
  private escapeXml(text: string): string {
    return String(text)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }

  /**
   * Escape DOT ID special characters.
   */
  private escapeDotId(id: string): string {
    return String(id).replace(/[^a-zA-Z0-9_]/g, '_');
  }

  /**
   * Escape DOT label special characters.
   */
  private escapeDotLabel(label: string): string {
    return String(label)
      .replace(/\\/g, '\\\\')
      .replace(/"/g, '\\"')
      .replace(/\n/g, '\\n');
  }
}

// Export singleton instance
export const provenanceExportService = new ProvenanceExportService();

