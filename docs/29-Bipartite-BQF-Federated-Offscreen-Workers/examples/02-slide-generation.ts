/**
 * Example 2: Slide Generation from Evolution Directory
 * 
 * This example demonstrates how to generate slides and cards
 * from an evolution directory.
 */

import { provenanceSlideService } from '@/services/provenance-slide-service';
import { Slide } from '@/services/provenance-slide-service';

async function generateSlides() {
  // Step 1: Initialize the service
  await provenanceSlideService.init();

  // Step 2: Generate slides
  const evolutionPath = '/evolutions/advanced-automaton';
  const slides: Slide[] = await provenanceSlideService.generateSlidesFromEvolution(evolutionPath);

  // Step 3: Display slide information
  console.log(`Generated ${slides.length} slides:`);
  slides.forEach(slide => {
    console.log(`\nSlide: ${slide.dimension}`);
    console.log(`  Title: ${slide.title}`);
    console.log(`  Nodes: ${slide.provenanceChain?.nodes.length || 0}`);
    console.log(`  Edges: ${slide.provenanceChain?.edges.length || 0}`);
    console.log(`  Cards: ${slide.cards.length}`);

    // Display card information
    slide.cards.forEach(card => {
      console.log(`    Card: ${card.pattern}`);
      console.log(`      JSONL Lines: ${card.jsonlLines.length}`);
      console.log(`      Church Encoding: ${card.metadata?.churchEncoding || 'N/A'}`);
    });
  });

  // Step 4: Use slide content
  slides.forEach(slide => {
    // Display markdown content
    console.log(`\n${slide.content}`);
  });

  return slides;
}

// Run the example
generateSlides()
  .then(slides => {
    console.log('\nExample completed successfully');
    console.log(`Total slides: ${slides.length}`);
  })
  .catch(error => {
    console.error('Example failed:', error);
  });

