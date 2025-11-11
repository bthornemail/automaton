/**
 * 7D-QuantumAgent - Populates 7D slides with quantum computing content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class QuantumAgent7D extends DimensionalAgent {
  constructor(contentLoader) {
    super('7D', contentLoader);
    this.name = '7D-Quantum-Agent';
  }

  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    const prioritized = [];
    const quantum = matches.find(e => e.id === '7D-quantum');
    const automaton = matches.find(e => e.id === '7D-automaton');
    
    if (quantum) prioritized.push(quantum);
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
      result.title = '7D: Quantum Computing';
    }
    
    return result;
  }
}

