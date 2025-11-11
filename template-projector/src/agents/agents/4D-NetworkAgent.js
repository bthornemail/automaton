/**
 * 4D-NetworkAgent - Populates 4D slides with network topology content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class NetworkAgent4D extends DimensionalAgent {
  constructor(contentLoader) {
    super('4D', contentLoader);
    this.name = '4D-Network-Agent';
  }

  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    const prioritized = [];
    const network = matches.find(e => e.id === '4D-network');
    const automaton = matches.find(e => e.id === '4D-automaton');
    
    if (network) prioritized.push(network);
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
      result.title = '4D: Network Topology';
    }
    
    return result;
  }
}

