/**
 * SVG Texture Renderer Component
 * 
 * Renders SVG textures on WebGL planes with real-time updates.
 */

import React, { useEffect, useRef, useState } from 'react';
import { useFrame } from '@react-three/fiber';
import * as THREE from 'three';
import { svgTextureService } from '../../services/svg-texture-service';

interface SVGTextureRendererProps {
  svg: SVGElement | string;
  position: [number, number, number];
  rotation?: [number, number, number];
  scale?: [number, number, number];
  width?: number;
  height?: number;
  updateInterval?: number; // Update interval in milliseconds (0 = every frame)
  cacheKey?: string;
}

export const SVGTextureRenderer: React.FC<SVGTextureRendererProps> = ({
  svg,
  position,
  rotation = [0, 0, 0],
  scale = [1, 1, 1],
  width = 10,
  height = 10,
  updateInterval = 0,
  cacheKey
}) => {
  const meshRef = useRef<THREE.Mesh>(null);
  const [texture, setTexture] = useState<THREE.Texture | null>(null);
  const lastUpdateRef = useRef<number>(0);

  // Load initial texture
  useEffect(() => {
    let newTexture: THREE.Texture;
    
    if (typeof svg === 'string') {
      newTexture = svgTextureService.svgStringToTexture(svg, { width, height, cacheKey });
    } else {
      newTexture = svgTextureService.svgToTexture(svg, { width, height, cacheKey });
    }
    
    setTexture(newTexture);
    
    return () => {
      // Don't dispose texture here as it might be cached
    };
  }, [typeof svg === 'string' ? svg : undefined, width, height, cacheKey]);

  // Real-time updates
  useFrame((state) => {
    if (!meshRef.current || !texture) return;
    
    // Update texture if interval is 0 (every frame) or interval has passed
    if (updateInterval === 0 || state.clock.elapsedTime * 1000 - lastUpdateRef.current >= updateInterval) {
      let newTexture: THREE.Texture;
      
      if (typeof svg === 'string') {
        newTexture = svgTextureService.svgStringToTexture(svg, { width, height, cacheKey });
      } else {
        // For SVG elements, update if they've changed
        newTexture = svgTextureService.svgToTexture(svg, { width, height, cacheKey });
      }
      
      if (newTexture !== texture) {
        // Update material
        const material = meshRef.current.material as THREE.MeshBasicMaterial;
        if (material) {
          material.map = newTexture;
          material.needsUpdate = true;
        }
        setTexture(newTexture);
      }
      
      lastUpdateRef.current = state.clock.elapsedTime * 1000;
    }
  });

  if (!texture) {
    return null;
  }

  return (
    <mesh
      ref={meshRef}
      position={position}
      rotation={rotation}
      scale={scale}
    >
      <planeGeometry args={[width, height]} />
      <meshBasicMaterial map={texture} transparent side={THREE.DoubleSide} />
    </mesh>
  );
};

/**
 * Procedural UI Generator Component
 */
interface ProceduralUIGeneratorProps {
  nodes: Array<{ id: string; x: number; y: number; label: string }>;
  edges: Array<{ from: string; to: string }>;
  position: [number, number, number];
  scale?: [number, number, number];
}

export const ProceduralUIGenerator: React.FC<ProceduralUIGeneratorProps> = ({
  nodes,
  edges,
  position,
  scale = [1, 1, 1]
}) => {
  const svgString = svgTextureService.createTopologyDiagramSVG(nodes, edges);
  
  return (
    <SVGTextureRenderer
      svg={svgString}
      position={position}
      scale={scale}
      width={8}
      height={6}
      updateInterval={100} // Update every 100ms
    />
  );
};

