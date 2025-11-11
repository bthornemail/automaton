/**
 * 2D-StructuralAgent - Populates 2D slides with spatial structure content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class StructuralAgent2D extends DimensionalAgent {
  constructor(contentLoader) {
    super('2D', contentLoader);
    this.name = '2D-Structural-Agent';
  }

  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    const prioritized = [];
    const topology = matches.find(e => e.id === '2D-topology');
    const system = matches.find(e => e.id === '2D-system');
    const automaton = matches.find(e => e.id === '2D-automaton');
    
    if (topology) prioritized.push(topology);
    if (system) prioritized.push(system);
    if (automaton) prioritized.push(automaton);
    
    for (const entry of matches) {
      if (!prioritized.includes(entry)) {
        prioritized.push(entry);
      }
    }
    
    return prioritized;
  }

  extractText(entries) {
    const result = super.extractText(entries);
    
    if (!result.title) {
      result.title = '2D: Spatial Structure';
    }
    
    return result;
  }
}

