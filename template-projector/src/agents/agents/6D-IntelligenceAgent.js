/**
 * 6D-IntelligenceAgent - Populates 6D slides with neural network content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class IntelligenceAgent6D extends DimensionalAgent {
  constructor(kernelLoader) {
    super('6D', kernelLoader);
    this.name = '6D-Intelligence-Agent';
  }

  matchKernelEntries(slide, kernelData) {
    const matches = super.matchKernelEntries(slide, kernelData);
    
    const prioritized = [];
    const intelligence = matches.find(e => e.id === '6D-intelligence');
    const automaton = matches.find(e => e.id === '6D-automaton');
    
    if (intelligence) prioritized.push(intelligence);
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
      result.title = '6D: Neural Networks';
    }
    
    return result;
  }
}

