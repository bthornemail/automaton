/**
 * 0D-TopologyAgent - Populates 0D slides with quantum vacuum topology content
 */

import { DimensionalAgent } from '../DimensionalAgent.js';

export class TopologyAgent0D extends DimensionalAgent {
  constructor(contentLoader) {
    super('0D', contentLoader);
    this.name = '0D-Topology-Agent';
  }

  /**
   * Override matchContentEntries to specifically match 0D topology and system entries
   */
  matchContentEntries(slide, contentData) {
    const matches = super.matchContentEntries(slide, contentData);
    
    // Prioritize 0D-topology and 0D-system entries
    const prioritized = [];
    
    // Add topology entry first
    const topology = matches.find(e => e.id === '0D-topology');
    if (topology) prioritized.push(topology);
    
    // Add system entry second
    const system = matches.find(e => e.id === '0D-system');
    if (system) prioritized.push(system);
    
    // Add automaton entry
    const automaton = matches.find(e => e.id === '0D-automaton');
    if (automaton) prioritized.push(automaton);
    
    // Add any other 0D entries
    for (const entry of matches) {
      if (!prioritized.includes(entry)) {
        prioritized.push(entry);
      }
    }
    
    return prioritized;
  }

  /**
   * Override extractText to provide 0D-specific formatting
   */
  extractText(entries) {
    const result = super.extractText(entries);
    
    // Enhance with 0D-specific content
    const topologyEntry = entries.find(e => e.id === '0D-topology');
    const systemEntry = entries.find(e => e.id === '0D-system');
    
    if (topologyEntry && !result.title) {
      result.title = '0D: Quantum Vacuum Topology';
    }
    
    if (topologyEntry && systemEntry) {
      // Combine topology and system content
      result.content = `# ${result.title || '0D Topology'}\n\n` +
        `## Topology\n${topologyEntry.text || ''}\n\n` +
        `## System\n${systemEntry.text || ''}`;
    }
    
    return result;
  }
}

