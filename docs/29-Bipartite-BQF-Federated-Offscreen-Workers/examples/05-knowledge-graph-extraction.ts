/**
 * Example 5: Extracting Knowledge Graphs from Slides
 * 
 * This example demonstrates how to extract and render knowledge
 * graphs from slides for agent thought processes.
 */

import { provenanceSlideService } from '@/services/provenance-slide-service';
import { knowledgeGraphCardService } from '@/services/knowledge-graph-card-service';
import { KnowledgeGraphCard } from '@/services/knowledge-graph-card-service';

async function extractKnowledgeGraphs() {
  // Step 1: Generate slides
  await provenanceSlideService.init();
  const slides = await provenanceSlideService.generateSlidesFromEvolution('/evolutions/advanced-automaton');

  // Step 2: Extract unique agent IDs
  const agentIds = new Set<string>();
  slides.forEach(slide => {
    slide.provenanceChain?.nodes.forEach(node => {
      if (node.metadata.agentId) {
        agentIds.add(node.metadata.agentId);
      }
    });
  });

  console.log(`Found ${agentIds.size} unique agents:`, Array.from(agentIds));

  // Step 3: Build knowledge graphs for each agent
  const knowledgeGraphs: KnowledgeGraphCard[] = [];

  slides.forEach(slide => {
    agentIds.forEach(agentId => {
      const card = knowledgeGraphCardService.buildKnowledgeGraph(slide, agentId);
      knowledgeGraphs.push(card);
    });
  });

  console.log(`Created ${knowledgeGraphs.length} knowledge graph cards`);

  // Step 4: Render knowledge graphs as SVG
  knowledgeGraphs.forEach(card => {
    console.log(`\nKnowledge Graph: ${card.agentId} (${card.slideId})`);
    console.log(`  Nodes: ${card.nodes.length}`);
    console.log(`  Edges: ${card.edges.length}`);
    console.log(`  Dimension: ${card.metadata.dimension}`);

    // Render as SVG
    const svg = knowledgeGraphCardService.renderKnowledgeGraph(card);

    // Display SVG (in browser)
    // document.body.appendChild(svg);

    // Or export SVG string
    const svgString = new XMLSerializer().serializeToString(svg);
    console.log(`  SVG size: ${svgString.length} bytes`);
  });

  return knowledgeGraphs;
}

// Run the example
extractKnowledgeGraphs()
  .then(graphs => {
    console.log('\nKnowledge graph extraction completed');
    console.log(`Total graphs: ${graphs.length}`);
  })
  .catch(error => {
    console.error('Knowledge graph extraction failed:', error);
  });

