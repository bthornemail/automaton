/**
 * Example 4: Integrating 3D Avatars with Thought Cards
 * 
 * This example demonstrates how to integrate 3D avatars with
 * thought cards for agent visualization.
 */

import { provenanceSlideService } from '@/services/provenance-slide-service';
import { avatarLoaderService } from '@/services/avatar-loader-service';
import { thoughtCardService } from '@/services/thought-card-service';
import { ThoughtCard } from '@/services/thought-card-service';
import * as THREE from 'three';

async function integrateAvatars() {
  // Step 1: Preload avatar templates
  console.log('Preloading avatar templates...');
  await avatarLoaderService.preloadTemplates();
  console.log('Avatars preloaded');

  // Step 2: Build provenance chain (avatars are automatically assigned)
  await provenanceSlideService.init();
  const chain = await provenanceSlideService.buildProvenanceChain('/evolutions/advanced-automaton');

  // Step 3: Load avatars for agent nodes
  const avatarPromises = chain.nodes
    .filter(node => node.avatar)
    .map(async (node) => {
      const model = await avatarLoaderService.loadAvatar(node.avatar!);
      
      // Position model at node position
      model.position.set(...node.position);
      
      return { node, model };
    });

  const avatars = await Promise.all(avatarPromises);
  console.log(`Loaded ${avatars.length} avatars`);

  // Step 4: Create thought cards for avatars
  const thoughtCards: ThoughtCard[] = chain.nodes
    .filter(node => node.avatar)
    .map(node => thoughtCardService.createThoughtCardFromNode(node))
    .filter((card): card is ThoughtCard => card !== null);

  console.log(`Created ${thoughtCards.length} thought cards`);

  // Step 5: Add avatars and cards to scene
  const scene = new THREE.Scene();

  avatars.forEach(({ node, model }) => {
    // Add avatar to scene
    scene.add(model);

    // Get thought card for this avatar
    const card = thoughtCards.find(c => c.avatarId === node.id);
    if (card) {
      // Create thought card mesh
      const texture = thoughtCardService.getCardTexture(card.id);
      if (texture) {
        const cardMaterial = new THREE.MeshBasicMaterial({
          map: new THREE.CanvasTexture(texture),
          transparent: true,
          opacity: card.opacity
        });

        const cardGeometry = new THREE.PlaneGeometry(...card.size);
        const cardMesh = new THREE.Mesh(cardGeometry, cardMaterial);

        // Position card relative to avatar
        const [offsetX, offsetY, offsetZ] = card.offset;
        cardMesh.position.set(
          node.position[0] + offsetX,
          node.position[1] + offsetY,
          node.position[2] + offsetZ
        );

        // Make card face camera (billboard)
        cardMesh.lookAt(0, 0, 0); // Adjust based on camera position

        scene.add(cardMesh);
      }
    }
  });

  console.log('Avatars and thought cards added to scene');
  return { scene, avatars, thoughtCards };
}

// Run the example
integrateAvatars()
  .then(({ scene, avatars, thoughtCards }) => {
    console.log('Avatar integration completed');
    console.log(`Scene contains ${scene.children.length} objects`);
    // Use scene for rendering
  })
  .catch(error => {
    console.error('Avatar integration failed:', error);
  });

