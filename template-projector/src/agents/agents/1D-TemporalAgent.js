/**
 * 1D-TemporalAgent - Populates 1D slides with temporal evolution content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class TemporalAgent1D extends DimensionalAgent {
  constructor(contentLoader) {
    super('1D', contentLoader);
    this.name = '1D-Temporal-Agent';
  }

  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    const prioritized = [];
    const topology = matches.find(e => e.id === '1D-topology');
    const system = matches.find(e => e.id === '1D-system');
    const automaton = matches.find(e => e.id === '1D-automaton');
    
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
      result.title = '1D: Temporal Evolution';
    }
    
    return result;
  }
}

