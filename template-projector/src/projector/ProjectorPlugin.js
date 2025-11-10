/**
 * ProjectorPlugin - Projector as a plugin (self-extending)
 * 
 * The Projector itself extends BasePlugin, enabling it to be used
 * as a plugin in other contexts or to extend itself.
 */

import { Projector } from './Projector.js';

export class ProjectorPlugin extends Projector {
  constructor(config = {}) {
    super(config);
    this.name = 'ProjectorPlugin';
  }

  /**
   * Self-extend: Load additional plugins dynamically
   */
  async selfExtend() {
    // The projector can extend itself by loading additional plugins
    // This enables recursive/self-referential plugin loading
    console.log('ProjectorPlugin: Self-extending...');
    
    // TODO: Implement self-extension logic
  }
}
