/**
 * 5D-ConsensusAgent - Populates 5D slides with blockchain consensus content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class ConsensusAgent5D extends DimensionalAgent {
  constructor(contentLoader) {
    super('5D', contentLoader);
    this.name = '5D-Consensus-Agent';
  }

  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    const prioritized = [];
    const consensus = matches.find(e => e.id === '5D-consensus');
    const automaton = matches.find(e => e.id === '5D-automaton');
    
    if (consensus) prioritized.push(consensus);
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
      result.title = '5D: Blockchain Consensus';
    }
    
    return result;
  }
}

