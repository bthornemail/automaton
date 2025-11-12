/**
 * Example 1: Basic Provenance Chain Building
 * 
 * This example demonstrates how to build a basic provenance chain
 * from an evolution directory.
 */

import { provenanceSlideService } from '@/services/provenance-slide-service';
import { ProvenanceChain } from '@/services/provenance-slide-service';

async function buildBasicProvenanceChain() {
  // Step 1: Initialize the service
  await provenanceSlideService.init();
  console.log('Service initialized');

  // Step 2: Build provenance chain
  const evolutionPath = '/evolutions/advanced-automaton';
  const chain: ProvenanceChain = await provenanceSlideService.buildProvenanceChain(evolutionPath);

  // Step 3: Display results
  console.log(`Provenance chain built:`);
  console.log(`  Nodes: ${chain.nodes.length}`);
  console.log(`  Edges: ${chain.edges.length}`);

  // Step 4: Inspect nodes
  chain.nodes.forEach(node => {
    console.log(`Node: ${node.id}`);
    console.log(`  Dimension: ${node.metadata.dimension}`);
    console.log(`  Pattern: ${node.metadata.pattern}`);
    console.log(`  Agent: ${node.metadata.agentId}`);
  });

  // Step 5: Inspect edges
  chain.edges.forEach(edge => {
    console.log(`Edge: ${edge.from} -> ${edge.to}`);
    console.log(`  Type: ${edge.type}`);
  });

  return chain;
}

// Run the example
buildBasicProvenanceChain()
  .then(chain => {
    console.log('Example completed successfully');
  })
  .catch(error => {
    console.error('Example failed:', error);
  });

