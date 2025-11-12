/**
 * Avatar Loader Service
 * 
 * Centralized avatar loading and management with caching.
 * Handles GLTF model loading, error handling, and fallbacks.
 */

import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader.js';
import * as THREE from 'three';
import { AvatarConfig } from './provenance-slide-service';
import { avatarTemplateService } from './avatar-template-service';

export interface LoadedAvatar {
  model: THREE.Group;
  config: AvatarConfig;
  loadedAt: number;
}

export class AvatarLoaderService {
  private loader: GLTFLoader;
  private cache: Map<string, LoadedAvatar> = new Map();
  private loadingPromises: Map<string, Promise<THREE.Group>> = new Map();
  private maxCacheSize: number = 50;
  private defaultCacheTimeout: number = 5 * 60 * 1000; // 5 minutes

  constructor() {
    this.loader = new GLTFLoader();
  }

  /**
   * Load a GLTF avatar model
   */
  async loadAvatar(config: AvatarConfig): Promise<THREE.Group> {
    const cacheKey = this.getCacheKey(config);

    // Check cache first
    const cached = this.cache.get(cacheKey);
    if (cached && Date.now() - cached.loadedAt < this.defaultCacheTimeout) {
      return cached.model.clone();
    }

    // Check if already loading
    const existingPromise = this.loadingPromises.get(cacheKey);
    if (existingPromise) {
      return existingPromise.then(model => model.clone());
    }

    // Start loading
    const loadPromise = this.loadGLTFModel(config.gltfModel)
      .then(gltf => {
        const model = gltf.scene;
        
        // Apply scale
        model.scale.set(...config.scale);
        
        // Apply color for AI agents
        if (config.type === 'ai-agent' && config.color) {
          model.traverse((child) => {
            if (child instanceof THREE.Mesh) {
              const material = child.material as THREE.MeshStandardMaterial;
              if (material) {
                material.color.setHex(parseInt(config.color!.replace('#', '0x')));
              }
            }
          });
        }

        // Cache the loaded model
        this.cacheModel(cacheKey, model, config);

        // Remove from loading promises
        this.loadingPromises.delete(cacheKey);

        return model;
      })
      .catch(error => {
        // Remove from loading promises on error
        this.loadingPromises.delete(cacheKey);
        
        // Try fallback template
        console.warn(`Failed to load avatar ${config.gltfModel}, trying fallback:`, error);
        return this.loadFallbackAvatar(config.type);
      });

    this.loadingPromises.set(cacheKey, loadPromise);
    return loadPromise.then(model => model.clone());
  }

  /**
   * Load GLTF model from URL
   */
  private async loadGLTFModel(url: string): Promise<{ scene: THREE.Group }> {
    return new Promise((resolve, reject) => {
      this.loader.load(
        url,
        (gltf) => resolve(gltf),
        (progress) => {
          // Progress tracking can be added here
        },
        (error) => reject(error)
      );
    });
  }

  /**
   * Load fallback avatar (Khronos glTF Sample Models)
   */
  private async loadFallbackAvatar(type: 'human' | 'ai-agent'): Promise<THREE.Group> {
    const fallbackUrl = type === 'human'
      ? 'https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0/DamagedHelmet/glTF-Binary/DamagedHelmet.glb'
      : 'https://raw.githubusercontent.com/KhronosGroup/glTF-Sample-Models/master/2.0/Fox/glTF-Binary/Fox.glb';
    
    const fallbackScale: [number, number, number] = type === 'human'
      ? [0.5, 0.5, 0.5]
      : [0.003, 0.003, 0.003];

    try {
      const gltf = await this.loadGLTFModel(fallbackUrl);
      const model = gltf.scene;
      model.scale.set(...fallbackScale);
      
      if (type === 'ai-agent') {
        model.traverse((child) => {
          if (child instanceof THREE.Mesh) {
            const material = child.material as THREE.MeshStandardMaterial;
            if (material) {
              material.color.setHex(0x00ff88);
            }
          }
        });
      }

      return model;
    } catch (error) {
      console.error('Failed to load fallback avatar:', error);
      // Return a simple sphere as last resort
      return this.createDefaultAvatar(type);
    }
  }

  /**
   * Create a default avatar (simple geometry)
   */
  private createDefaultAvatar(type: 'human' | 'ai-agent'): THREE.Group {
    const group = new THREE.Group();
    const geometry = new THREE.SphereGeometry(0.5, 16, 16);
    const material = new THREE.MeshStandardMaterial({
      color: type === 'ai-agent' ? 0x00ff88 : 0xffffff
    });
    const mesh = new THREE.Mesh(geometry, material);
    group.add(mesh);
    return group;
  }

  /**
   * Cache a loaded model
   */
  private cacheModel(key: string, model: THREE.Group, config: AvatarConfig): void {
    // Evict oldest if cache is full
    if (this.cache.size >= this.maxCacheSize) {
      const oldestKey = Array.from(this.cache.entries())
        .sort((a, b) => a[1].loadedAt - b[1].loadedAt)[0][0];
      this.cache.delete(oldestKey);
    }

    this.cache.set(key, {
      model: model.clone(),
      config,
      loadedAt: Date.now()
    });
  }

  /**
   * Generate cache key from config
   */
  private getCacheKey(config: AvatarConfig): string {
    return `${config.gltfModel}:${config.scale.join(',')}:${config.type}:${config.color || ''}`;
  }

  /**
   * Clear cache
   */
  clearCache(): void {
    this.cache.clear();
    this.loadingPromises.clear();
  }

  /**
   * Get cache statistics
   */
  getCacheStats(): { size: number; maxSize: number; keys: string[] } {
    return {
      size: this.cache.size,
      maxSize: this.maxCacheSize,
      keys: Array.from(this.cache.keys())
    };
  }

  /**
   * Preload avatar templates
   */
  async preloadTemplates(): Promise<void> {
    const templates = avatarTemplateService.getAllTemplates();
    const loadPromises = templates.map(template => {
      const config = avatarTemplateService.createAvatarConfig(template.id);
      if (config) {
        return this.loadAvatar(config).catch(error => {
          console.warn(`Failed to preload template ${template.id}:`, error);
        });
      }
    });

    await Promise.all(loadPromises);
  }
}

export const avatarLoaderService = new AvatarLoaderService();

