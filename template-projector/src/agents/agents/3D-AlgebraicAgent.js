/**
 * 3D-AlgebraicAgent - Populates 3D slides with Church algebra content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class AlgebraicAgent3D extends DimensionalAgent {
  constructor(contentLoader) {
    super('3D', contentLoader);
    this.name = '3D-Algebraic-Agent';
  }

  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    const prioritized = [];
    const algebra = matches.find(e => e.id === '3D-algebra');
    const analysis = matches.find(e => e.id === '3D-analysis');
    const automaton = matches.find(e => e.id === '3D-automaton');
    
    if (algebra) prioritized.push(algebra);
    if (analysis) prioritized.push(analysis);
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
      result.title = '3D: Church Algebra';
    }
    
    return result;
  }
}

