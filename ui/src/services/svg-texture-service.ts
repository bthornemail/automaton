/**
 * SVG Texture Service
 * 
 * Converts SVG to WebGL textures with caching and real-time updates.
 */

import * as THREE from 'three';

export interface SVGTextureOptions {
  width?: number;
  height?: number;
  cacheKey?: string;
}

export class SVGTextureService {
  private textureCache: Map<string, THREE.Texture> = new Map();
  private maxCacheSize: number = 50;
  private defaultCacheTimeout: number = 5 * 60 * 1000; // 5 minutes

  /**
   * Convert SVG element to Three.js texture
   */
  svgToTexture(svg: SVGElement, options: SVGTextureOptions = {}): THREE.Texture {
    const cacheKey = options.cacheKey || this.generateSVGKey(svg);
    
    // Check cache first
    const cached = this.textureCache.get(cacheKey);
    if (cached) {
      return cached.clone();
    }
    
    // Serialize SVG to string
    const serializer = new XMLSerializer();
    const svgString = serializer.serializeToString(svg);
    
    // Create base64 data URL
    const base64 = btoa(unescape(encodeURIComponent(svgString)));
    const dataUrl = `data:image/svg+xml;base64,${base64}`;
    
    // Load texture
    const loader = new THREE.TextureLoader();
    const texture = loader.load(dataUrl);
    texture.needsUpdate = true;
    
    // Set texture options
    if (options.width) texture.image.width = options.width;
    if (options.height) texture.image.height = options.height;
    
    // Cache texture
    this.cacheTexture(cacheKey, texture);
    
    return texture;
  }

  /**
   * Update SVG texture on a plane
   */
  updateSVGTexture(plane: THREE.Mesh, svg: SVGElement, options: SVGTextureOptions = {}): void {
    const texture = this.svgToTexture(svg, options);
    
    if (plane.material instanceof THREE.MeshBasicMaterial) {
      plane.material.map = texture;
      plane.material.needsUpdate = true;
    } else if (plane.material instanceof THREE.MeshStandardMaterial) {
      plane.material.map = texture;
      plane.material.needsUpdate = true;
    }
  }

  /**
   * Create SVG texture from string
   */
  svgStringToTexture(svgString: string, options: SVGTextureOptions = {}): THREE.Texture {
    // Create temporary SVG element
    const parser = new DOMParser();
    const svgDoc = parser.parseFromString(svgString, 'image/svg+xml');
    const svgElement = svgDoc.documentElement;
    
    return this.svgToTexture(svgElement, options);
  }

  /**
   * Generate cache key from SVG
   */
  private generateSVGKey(svg: SVGElement): string {
    const serializer = new XMLSerializer();
    const svgString = serializer.serializeToString(svg);
    // Create hash from SVG string (simple hash)
    let hash = 0;
    for (let i = 0; i < svgString.length; i++) {
      const char = svgString.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32-bit integer
    }
    return `svg-${hash}`;
  }

  /**
   * Cache texture
   */
  private cacheTexture(key: string, texture: THREE.Texture): void {
    // Evict oldest if cache is full
    if (this.textureCache.size >= this.maxCacheSize) {
      const oldestKey = Array.from(this.textureCache.keys())[0];
      const oldestTexture = this.textureCache.get(oldestKey);
      if (oldestTexture) {
        oldestTexture.dispose();
      }
      this.textureCache.delete(oldestKey);
    }

    this.textureCache.set(key, texture);
  }

  /**
   * Clear cache
   */
  clearCache(): void {
    this.textureCache.forEach(texture => texture.dispose());
    this.textureCache.clear();
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): { size: number; maxSize: number; keys: string[] } {
    return {
      size: this.textureCache.size,
      maxSize: this.maxCacheSize,
      keys: Array.from(this.textureCache.keys())
    };
  }

  /**
   * Create procedural SVG for topology diagram
   */
  createTopologyDiagramSVG(
    nodes: Array<{ id: string; x: number; y: number; label: string }>,
    edges: Array<{ from: string; to: string }>
  ): string {
    const width = 800;
    const height = 600;
    
    let svg = `<svg width="${width}" height="${height}" xmlns="http://www.w3.org/2000/svg">`;
    
    // Add edges
    edges.forEach(edge => {
      const fromNode = nodes.find(n => n.id === edge.from);
      const toNode = nodes.find(n => n.id === edge.to);
      if (fromNode && toNode) {
        svg += `<line x1="${fromNode.x}" y1="${fromNode.y}" x2="${toNode.x}" y2="${toNode.y}" stroke="#4b5563" stroke-width="2"/>`;
      }
    });
    
    // Add nodes
    nodes.forEach(node => {
      svg += `<circle cx="${node.x}" cy="${node.y}" r="10" fill="#3b82f6"/>`;
      svg += `<text x="${node.x}" y="${node.y - 15}" text-anchor="middle" fill="white" font-size="12">${node.label}</text>`;
    });
    
    svg += '</svg>';
    return svg;
  }
}

export const svgTextureService = new SVGTextureService();

