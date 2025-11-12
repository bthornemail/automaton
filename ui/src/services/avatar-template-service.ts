/**
 * Avatar Template Service
 * 
 * Manages avatar templates for human and AI agent visualization.
 * Provides template registry and configuration for GLTF models.
 */

export interface AvatarTemplate {
  id: string;
  name: string;
  gltfModel: string;
  scale: [number, number, number];
  type: 'human' | 'ai-agent';
  defaultColor?: string;
  description?: string;
}

export class AvatarTemplateService {
  private templates: Map<string, AvatarTemplate> = new Map();
  private defaultTemplates: AvatarTemplate[] = [];

  constructor() {
    this.initializeDefaultTemplates();
  }

  private initializeDefaultTemplates(): void {
    // Human avatar templates
    const humanTemplates: AvatarTemplate[] = [
      {
        id: 'angelica',
        name: 'Angelica',
        gltfModel: '/evolutions/angelica.glb', // File should be in ui/public/evolutions/
        scale: [0.5, 0.5, 0.5],
        type: 'human',
        defaultColor: '#ffffff',
        description: 'Human avatar option 1'
      },
      {
        id: 'shantae',
        name: 'Shantae',
        gltfModel: '/evolutions/shantae.glb', // File should be in ui/public/evolutions/
        scale: [0.5, 0.5, 0.5],
        type: 'human',
        defaultColor: '#ffffff',
        description: 'Human avatar option 2'
      }
    ];

    // AI agent avatar template
    const aiTemplates: AvatarTemplate[] = [
      {
        id: 'sploot',
        name: 'Sploot',
        gltfModel: '/evolutions/sploot.glb', // File should be in ui/public/evolutions/
        scale: [0.5, 0.5, 0.5],
        type: 'ai-agent',
        defaultColor: '#00ff88',
        description: 'AI agent avatar'
      }
    ];

    // Register all templates
    [...humanTemplates, ...aiTemplates].forEach(template => {
      this.templates.set(template.id, template);
    });

    this.defaultTemplates = [...humanTemplates, ...aiTemplates];
  }

  /**
   * Get a template by ID
   */
  getTemplate(id: string): AvatarTemplate | null {
    return this.templates.get(id) || null;
  }

  /**
   * Get all templates of a specific type
   */
  getTemplatesByType(type: 'human' | 'ai-agent'): AvatarTemplate[] {
    return Array.from(this.templates.values()).filter(t => t.type === type);
  }

  /**
   * Get a random template of a specific type
   */
  getRandomTemplate(type: 'human' | 'ai-agent'): AvatarTemplate | null {
    const templates = this.getTemplatesByType(type);
    if (templates.length === 0) return null;
    return templates[Math.floor(Math.random() * templates.length)];
  }

  /**
   * Get default template for a type (first available)
   */
  getDefaultTemplate(type: 'human' | 'ai-agent'): AvatarTemplate | null {
    const templates = this.getTemplatesByType(type);
    return templates[0] || null;
  }

  /**
   * Register a custom template
   */
  registerTemplate(template: AvatarTemplate): void {
    this.templates.set(template.id, template);
  }

  /**
   * Get all registered templates
   */
  getAllTemplates(): AvatarTemplate[] {
    return Array.from(this.templates.values());
  }

  /**
   * Create avatar config from template
   */
  createAvatarConfig(
    templateId: string,
    overrides?: Partial<AvatarTemplate>
  ): import('./provenance-slide-service').AvatarConfig | null {
    const template = this.getTemplate(templateId);
    if (!template) return null;

    return {
      gltfModel: overrides?.gltfModel || template.gltfModel,
      scale: overrides?.scale || template.scale,
      type: overrides?.type || template.type,
      label: overrides?.name || template.name,
      color: overrides?.defaultColor || template.defaultColor
    };
  }
}

export const avatarTemplateService = new AvatarTemplateService();

