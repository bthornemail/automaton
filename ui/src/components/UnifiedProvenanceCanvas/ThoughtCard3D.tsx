/**
 * 3D Thought Card Component
 * 
 * Renders 3D thought cards near avatars using billboard planes.
 */

import React, { useRef, useEffect, useMemo } from 'react';
import { useFrame, useThree } from '@react-three/fiber';
import * as THREE from 'three';
import { ThoughtCard } from '../../services/thought-card-service';
import { thoughtCardService } from '../../services/thought-card-service';

interface ThoughtCard3DProps {
  card: ThoughtCard;
  avatarPosition: [number, number, number];
  isVisible: boolean;
}

export const ThoughtCard3D: React.FC<ThoughtCard3DProps> = ({
  card,
  avatarPosition,
  isVisible
}) => {
  const meshRef = useRef<THREE.Mesh>(null);
  const { camera } = useThree();
  const textureRef = useRef<THREE.Texture | null>(null);

  // Create texture from card canvas
  useEffect(() => {
    // Ensure card texture is created
    const canvas = thoughtCardService.getCardTexture(card.id);
    if (!canvas) {
      // Create texture if it doesn't exist
      thoughtCardService.createCardTexture(card);
      const newCanvas = thoughtCardService.getCardTexture(card.id);
      if (!newCanvas) return;
    }

    const finalCanvas = canvas || thoughtCardService.getCardTexture(card.id);
    if (!finalCanvas) return;

    const texture = new THREE.CanvasTexture(finalCanvas);
    texture.needsUpdate = true;
    textureRef.current = texture;

    return () => {
      texture.dispose();
    };
  }, [card.id, card.content]);

  // Update position and billboard effect
  useFrame(() => {
    if (!meshRef.current || !isVisible) return;

    // Calculate position relative to avatar
    const [offsetX, offsetY, offsetZ] = card.offset;
    meshRef.current.position.set(
      avatarPosition[0] + offsetX,
      avatarPosition[1] + offsetY,
      avatarPosition[2] + offsetZ
    );

    // Billboard effect - always face camera
    meshRef.current.lookAt(camera.position);

    // Gentle floating animation
    meshRef.current.position.y += Math.sin(Date.now() * 0.001) * 0.01;
  });

  const material = useMemo(() => {
    if (!textureRef.current) {
      return new THREE.MeshBasicMaterial({
        color: 0x1e1e1e,
        transparent: true,
        opacity: card.opacity
      });
    }

    return new THREE.MeshBasicMaterial({
      map: textureRef.current,
      transparent: true,
      opacity: card.opacity,
      side: THREE.DoubleSide
    });
  }, [card.opacity, textureRef.current]);

  if (!isVisible) return null;

  return (
    <mesh
      ref={meshRef}
      material={material}
    >
      <planeGeometry args={card.size} />
    </mesh>
  );
};

