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
   * Get an avatar template by ID.
   * 
   * Retrieves a registered avatar template by its unique ID. Returns null
   * if the template is not found.
   * 
   * @param {string} id - Template ID
   * @returns {AvatarTemplate | null} Template or null if not found
   * 
   * @example
   * ```typescript
   * const template = avatarTemplateService.getTemplate('angelica');
   * if (template) {
   *   // Use template
   * }
   * ```
   */
  getTemplate(id: string): AvatarTemplate | null {
    return this.templates.get(id) || null;
  }

  /**
   * Get all templates of a specific type.
   * 
   * Returns all registered templates that match the specified type (human or AI agent).
   * Useful for filtering templates or selecting random templates of a specific type.
   * 
   * @param {'human' | 'ai-agent'} type - Template type to filter by
   * @returns {AvatarTemplate[]} Array of templates matching the type
   * 
   * @example
   * ```typescript
   * const humanTemplates = avatarTemplateService.getTemplatesByType('human');
   * // humanTemplates contains all human avatar templates
   * ```
   */
  getTemplatesByType(type: 'human' | 'ai-agent'): AvatarTemplate[] {
    return Array.from(this.templates.values()).filter(t => t.type === type);
  }

  /**
   * Get a random template of a specific type.
   * 
   * Returns a randomly selected template from all templates of the specified type.
   * Returns null if no templates of that type are available.
   * 
   * @param {'human' | 'ai-agent'} type - Template type
   * @returns {AvatarTemplate | null} Random template or null if none available
   * 
   * @example
   * ```typescript
   * const randomHuman = avatarTemplateService.getRandomTemplate('human');
   * // randomHuman is a randomly selected human avatar template
   * ```
   */
  getRandomTemplate(type: 'human' | 'ai-agent'): AvatarTemplate | null {
    const templates = this.getTemplatesByType(type);
    if (templates.length === 0) return null;
    return templates[Math.floor(Math.random() * templates.length)];
  }

  /**
   * Get default template for a type (first available).
   * 
   * Returns the first available template of the specified type. This is useful
   * as a fallback when a specific template is not available.
   * 
   * @param {'human' | 'ai-agent'} type - Template type
   * @returns {AvatarTemplate | null} Default template or null if none available
   * 
   * @example
   * ```typescript
   * const defaultHuman = avatarTemplateService.getDefaultTemplate('human');
   * // defaultHuman is the first human avatar template
   * ```
   */
  getDefaultTemplate(type: 'human' | 'ai-agent'): AvatarTemplate | null {
    const templates = this.getTemplatesByType(type);
    return templates[0] || null;
  }

  /**
   * Register a custom avatar template.
   * 
   * Adds a new avatar template to the registry. Custom templates can override
   * default templates or add new avatar options. Templates are identified by
   * their unique ID.
   * 
   * @param {AvatarTemplate} template - Template to register
   * 
   * @example
   * ```typescript
   * avatarTemplateService.registerTemplate({
   *   id: 'custom-avatar',
   *   name: 'Custom Avatar',
   *   gltfModel: '/avatars/custom.glb',
   *   scale: [0.5, 0.5, 0.5],
   *   type: 'human'
   * });
   * ```
   */
  registerTemplate(template: AvatarTemplate): void {
    this.templates.set(template.id, template);
  }

  /**
   * Get all registered templates.
   * 
   * Returns all avatar templates currently registered in the service, regardless
   * of type. Useful for listing all available avatars or debugging.
   * 
   * @returns {AvatarTemplate[]} Array of all registered templates
   * 
   * @example
   * ```typescript
   * const allTemplates = avatarTemplateService.getAllTemplates();
   * console.log(`Total templates: ${allTemplates.length}`);
   * ```
   */
  getAllTemplates(): AvatarTemplate[] {
    return Array.from(this.templates.values());
  }

  /**
   * Create avatar configuration from a template.
   * 
   * Creates an AvatarConfig object from a template ID, with optional overrides
   * for specific properties. This is the standard way to create avatar
   * configurations for use with the avatar loader service.
   * 
   * @param {string} templateId - ID of the template to use
   * @param {Partial<AvatarTemplate>} [overrides] - Optional property overrides
   * @returns {AvatarConfig | null} Avatar configuration or null if template not found
   * 
   * @example
   * ```typescript
   * const config = avatarTemplateService.createAvatarConfig('angelica', {
   *   label: 'My Avatar'
   * });
   * if (config) {
   *   const model = await avatarLoaderService.loadAvatar(config);
   * }
   * ```
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

