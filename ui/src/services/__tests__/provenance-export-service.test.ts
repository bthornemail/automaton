/**
 * Provenance Export Service Tests
 * 
 * Tests for exporting provenance chains in multiple formats.
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { provenanceExportService, ExportFormat } from '../provenance-export-service';
import { ProvenanceChain, ProvenanceNode, ProvenanceEdge } from '../provenance-slide-service';

describe('ProvenanceExportService', () => {
  let mockChain: ProvenanceChain;
  let mockDownload: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    // Reset mocks
    vi.clearAllMocks();
    
    // Create mock provenance chain
    mockChain = {
      nodes: [
        {
          id: 'node-1',
          type: 'agent',
          position: [0, 0, 0],
          metadata: {
            timestamp: 1000,
            file: 'test.jsonl',
            line: 1,
            agentId: '0D-Topology-Agent',
            dimension: '0D',
            pattern: 'identity',
            churchEncoding: 'λf.λx.x'
          },
          data: {}
        },
        {
          id: 'node-2',
          type: 'document',
          position: [1, 1, 1],
          metadata: {
            timestamp: 2000,
            file: 'test.jsonl',
            line: 2,
            agentId: '1D-Temporal-Agent',
            dimension: '1D',
            pattern: 'successor'
          },
          data: {}
        }
      ],
      edges: [
        {
          id: 'edge-1',
          type: 'evolves',
          from: 'node-1',
          to: 'node-2',
          metadata: {
            timestamp: 1500,
            weight: 1.0,
            context: 'dimensional-progression'
          }
        }
      ]
    };

    // Mock download functionality
    mockDownload = vi.fn();
    global.URL.createObjectURL = vi.fn(() => 'blob:mock-url');
    global.URL.revokeObjectURL = vi.fn();
    
    // Mock document.createElement to return link by default
    const mockLink = {
      href: '',
      download: '',
      click: mockDownload,
      style: {}
    };
    vi.spyOn(document, 'createElement').mockImplementation((tag: string) => {
      if (tag === 'a') return mockLink as any;
      return {} as any;
    });
    vi.spyOn(document.body, 'appendChild').mockImplementation(() => mockLink as any);
    vi.spyOn(document.body, 'removeChild').mockImplementation(() => mockLink as any);
  });

  describe('exportChain', () => {
    it('should export to JSON format', async () => {
      await provenanceExportService.exportChain(mockChain, {
        format: 'json',
        includeMetadata: true
      });

      expect(document.createElement).toHaveBeenCalledWith('a');
      expect(mockDownload).toHaveBeenCalled();
    });

    it('should export to JSONL format', async () => {
      await provenanceExportService.exportChain(mockChain, {
        format: 'jsonl',
        includeMetadata: false
      });

      expect(document.createElement).toHaveBeenCalledWith('a');
      expect(mockDownload).toHaveBeenCalled();
    });

    it('should export to GraphML format', async () => {
      await provenanceExportService.exportChain(mockChain, {
        format: 'graphml'
      });

      expect(document.createElement).toHaveBeenCalledWith('a');
      expect(mockDownload).toHaveBeenCalled();
    });

    it('should export to DOT format', async () => {
      await provenanceExportService.exportChain(mockChain, {
        format: 'dot'
      });

      expect(document.createElement).toHaveBeenCalledWith('a');
      expect(mockDownload).toHaveBeenCalled();
    });

    it('should generate filename if not provided', async () => {
      await provenanceExportService.exportChain(mockChain, {
        format: 'json'
      });

      const linkElement = (document.createElement as any).mock.results[0].value;
      expect(linkElement.download).toMatch(/^provenance-chain-.*\.json$/);
    });

    it('should use provided filename', async () => {
      const mockLink = {
        href: '',
        download: '',
        click: vi.fn(),
        style: {}
      };
      vi.spyOn(document, 'createElement').mockReturnValue(mockLink as any);
      
      await provenanceExportService.exportChain(mockChain, {
        format: 'json',
        filename: 'custom-name.json'
      });

      expect(mockLink.download).toBe('custom-name.json');
    });

    it('should include metadata when requested', async () => {
      const exportSpy = vi.spyOn(provenanceExportService as any, 'exportToJSON');
      
      await provenanceExportService.exportChain(mockChain, {
        format: 'json',
        includeMetadata: true
      });

      expect(exportSpy).toHaveBeenCalledWith(mockChain, expect.objectContaining({
        includeMetadata: true
      }));
    });

    it('should exclude metadata when not requested', async () => {
      const exportSpy = vi.spyOn(provenanceExportService as any, 'exportToJSON');
      
      await provenanceExportService.exportChain(mockChain, {
        format: 'json',
        includeMetadata: false
      });

      expect(exportSpy).toHaveBeenCalledWith(mockChain, expect.objectContaining({
        includeMetadata: false
      }));
    });
  });

  describe('exportToJSON', () => {
    it('should export nodes and edges in JSON format', () => {
      const result = (provenanceExportService as any).exportToJSON(mockChain, {
        includeMetadata: true
      });

      const parsed = JSON.parse(result);
      expect(parsed.nodes).toHaveLength(2);
      expect(parsed.edges).toHaveLength(1);
      expect(parsed.metadata).toBeDefined();
      expect(parsed.metadata.nodeCount).toBe(2);
      expect(parsed.metadata.edgeCount).toBe(1);
    });

    it('should include full metadata when includeMetadata is true', () => {
      const result = (provenanceExportService as any).exportToJSON(mockChain, {
        includeMetadata: true
      });

      const parsed = JSON.parse(result);
      expect(parsed.nodes[0].position).toBeDefined();
      expect(parsed.nodes[0].data).toBeDefined();
    });

    it('should include minimal metadata when includeMetadata is false', () => {
      const result = (provenanceExportService as any).exportToJSON(mockChain, {
        includeMetadata: false
      });

      const parsed = JSON.parse(result);
      expect(parsed.nodes[0].metadata.dimension).toBeDefined();
      expect(parsed.nodes[0].metadata.pattern).toBeDefined();
      expect(parsed.nodes[0].position).toBeUndefined();
    });
  });

  describe('exportToJSONL', () => {
    it('should export one JSON object per line', () => {
      const result = (provenanceExportService as any).exportToJSONL(mockChain, {
        includeMetadata: true
      });

      const lines = result.split('\n');
      expect(lines.length).toBeGreaterThanOrEqual(3); // 2 nodes + 1 edge
      
      // Verify each line is valid JSON
      lines.forEach(line => {
        if (line.trim()) {
          expect(() => JSON.parse(line)).not.toThrow();
        }
      });
    });

    it('should include type field for each line', () => {
      const result = (provenanceExportService as any).exportToJSONL(mockChain, {
        includeMetadata: true
      });

      const lines = result.split('\n').filter(l => l.trim());
      // First line should be a node
      const firstLine = JSON.parse(lines[0]);
      expect(firstLine.type).toBe('node');
      expect(firstLine.id).toBeDefined();
      
      // Last line should be an edge
      const lastLine = JSON.parse(lines[lines.length - 1]);
      expect(lastLine.type).toBe('edge');
    });
  });

  describe('exportToGraphML', () => {
    it('should export valid GraphML XML', () => {
      const result = (provenanceExportService as any).exportToGraphML(mockChain, {
        includeMetadata: true
      });

      expect(result).toContain('<?xml');
      expect(result).toContain('<graphml');
      expect(result).toContain('<graph');
      expect(result).toContain('<node');
      expect(result).toContain('<edge');
    });

    it('should include node attributes', () => {
      const result = (provenanceExportService as any).exportToGraphML(mockChain, {
        includeMetadata: true
      });

      expect(result).toContain('node-1');
      expect(result).toContain('node-2');
      expect(result).toContain('0D');
      expect(result).toContain('identity');
    });

    it('should include edge attributes', () => {
      const result = (provenanceExportService as any).exportToGraphML(mockChain, {
        includeMetadata: true
      });

      expect(result).toContain('edge-1');
      expect(result).toContain('evolves');
    });
  });

  describe('exportToDOT', () => {
    it('should export valid DOT format', () => {
      const result = (provenanceExportService as any).exportToDOT(mockChain, {
        includeMetadata: true
      });

      expect(result).toContain('digraph');
      // DOT IDs are escaped (node-1 becomes node_1)
      expect(result).toContain('node_1');
      expect(result).toContain('node_2');
      expect(result).toContain('->');
    });

    it('should include node labels', () => {
      const result = (provenanceExportService as any).exportToDOT(mockChain, {
        includeMetadata: true
      });

      expect(result).toContain('identity');
      expect(result).toContain('successor');
    });

    it('should include edge labels', () => {
      const result = (provenanceExportService as any).exportToDOT(mockChain, {
        includeMetadata: true
      });

      expect(result).toContain('evolves');
    });
  });

  describe('exportToImage', () => {
    let mockImage: any;
    let mockCanvas: any;
    let mockContext: any;
    let mockLink: any;

    beforeEach(() => {
      // Mock Image constructor
      mockImage = {
        onload: null as any,
        onerror: null as any,
        src: '',
        width: 0,
        height: 0
      };
      
      // Use vi.stubGlobal for Image constructor
      vi.stubGlobal('Image', vi.fn().mockImplementation(function(this: any) {
        const img = {
          onload: null as any,
          onerror: null as any,
          src: '',
          width: 0,
          height: 0
        };
        // Trigger onload after a short delay
        setTimeout(() => {
          if (img.onload) {
            img.onload();
          }
        }, 10);
        return img;
      }));

      // Mock Canvas
      mockContext = {
        fillRect: vi.fn(),
        drawImage: vi.fn(),
        fillStyle: ''
      };
      
      mockCanvas = {
        width: 0,
        height: 0,
        getContext: vi.fn(() => mockContext),
        toBlob: vi.fn((callback: any) => {
          if (callback) {
            callback(new Blob(['mock'], { type: 'image/png' }));
          }
        })
      };
      
      // Mock link element
      mockLink = {
        href: '',
        download: '',
        click: mockDownload,
        style: {}
      };
      
      vi.spyOn(document, 'createElement').mockImplementation((tag: string) => {
        if (tag === 'canvas') return mockCanvas as any;
        if (tag === 'a') return mockLink as any;
        return {} as any;
      });
    });

    it('should export to SVG format', async () => {
      await provenanceExportService.exportChain(mockChain, {
        format: 'svg',
        imageOptions: {
          width: 1920,
          height: 1080,
          backgroundColor: '#ffffff'
        }
      });

      expect(document.createElement).toHaveBeenCalled();
      expect(mockDownload).toHaveBeenCalled();
    });

    it('should export to PNG format', async () => {
      // Use a promise to wait for async operations
      const exportPromise = provenanceExportService.exportChain(mockChain, {
        format: 'png',
        imageOptions: {
          width: 1920,
          height: 1080
        }
      });

      // Wait for image to load
      await new Promise(resolve => setTimeout(resolve, 50));
      
      await exportPromise;

      expect(document.createElement).toHaveBeenCalledWith('canvas');
      expect(mockCanvas.getContext).toHaveBeenCalledWith('2d');
    });
  });

  describe('generateFilename', () => {
    it('should generate filename with timestamp', () => {
      const filename = (provenanceExportService as any).generateFilename('json');
      expect(filename).toMatch(/^provenance-chain-.*\.json$/);
    });

    it('should use correct extension for each format', () => {
      const formats: ExportFormat[] = ['json', 'jsonl', 'graphml', 'dot', 'png', 'svg'];
      formats.forEach(format => {
        const filename = (provenanceExportService as any).generateFilename(format);
        const extension = filename.split('.').pop();
        expect(extension).toBe(format === 'jsonl' ? 'jsonl' : format);
      });
    });
  });

  describe('getNodeColor', () => {
    it('should return color for each dimension', () => {
      const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      dimensions.forEach(dim => {
        const color = (provenanceExportService as any).getNodeColor(dim);
        expect(color).toMatch(/^#[0-9a-f]{6}$/i);
      });
    });

    it('should return default color for unknown dimension', () => {
      const color = (provenanceExportService as any).getNodeColor('unknown');
      expect(color).toBe('#6b7280');
    });
  });
});

