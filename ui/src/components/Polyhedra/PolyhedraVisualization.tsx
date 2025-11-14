/**
 * Polyhedra Visualization Component
 * 
 * Renders Platonic solids (and Archimedean solids) with Three.js
 * Integrates with BQF transformations and consensus patterns
 * 
 * Source: docs/32-Regulay-Polyhedra-Geometry/
 */

import React, { useRef, useEffect, useState } from 'react';
import { Canvas, useFrame, useThree } from '@react-three/fiber';
import { OrbitControls, Text } from '@react-three/drei';
import * as THREE from 'three';
import { bqfTransformationService, type BQF } from '../../services/bqf-transformation-service';

export interface PolyhedronConfig {
  name: string;
  type: 'tetrahedron' | 'cube' | 'octahedron' | 'icosahedron' | 'dodecahedron';
  position: [number, number, number];
  bqf: BQF;
  color?: number;
  wireframe?: boolean;
}

interface PolyhedraVisualizationProps {
  polyhedra?: PolyhedronConfig[];
  enableInteractions?: boolean;
  showLabels?: boolean;
  onPolyhedronClick?: (config: PolyhedronConfig) => void;
}

// Individual Polyhedron Component
const PolyhedronMesh: React.FC<{
  config: PolyhedronConfig;
  onClick?: () => void;
  showLabel?: boolean;
}> = ({ config, onClick, showLabel }) => {
  const meshRef = useRef<THREE.Mesh>(null);
  const [bqf, setBQF] = useState<BQF>(config.bqf);

  // Store BQF in userData for transformations
  useEffect(() => {
    if (meshRef.current) {
      meshRef.current.userData.bqf = bqf;
      meshRef.current.userData.type = config.type;
      meshRef.current.userData.config = config;
    }
  }, [bqf, config]);

  // Create geometry based on type
  const getGeometry = () => {
    switch (config.type) {
      case 'tetrahedron':
        return new THREE.TetrahedronGeometry(1);
      case 'cube':
        return new THREE.BoxGeometry(1, 1, 1);
      case 'octahedron':
        return new THREE.OctahedronGeometry(1);
      case 'icosahedron':
        return new THREE.IcosahedronGeometry(1);
      case 'dodecahedron':
        return new THREE.DodecahedronGeometry(1);
      default:
        return new THREE.BoxGeometry(1, 1, 1);
    }
  };

  // Get color from BQF or config
  const getColor = (): number => {
    if (config.color) return config.color;
    const [a, b, c] = bqf;
    const hue = ((a + b + c) * 30) % 360;
    return new THREE.Color().setHSL(hue / 360, 0.7, 0.5).getHex();
  };

  const geometry = getGeometry();
  const material = new THREE.MeshPhongMaterial({
    color: getColor(),
    wireframe: config.wireframe ?? false,
    transparent: true,
    opacity: 0.8,
  });

  // Animate rotation for visual interest
  useFrame(() => {
    if (meshRef.current) {
      meshRef.current.rotation.x += 0.005;
      meshRef.current.rotation.y += 0.01;
    }
  });

  return (
    <group position={config.position}>
      <mesh
        ref={meshRef}
        geometry={geometry}
        material={material}
        onClick={onClick}
        onPointerOver={(e) => {
          e.stopPropagation();
          document.body.style.cursor = 'pointer';
        }}
        onPointerOut={() => {
          document.body.style.cursor = 'default';
        }}
      />
      {showLabel && (
        <Text
          position={[0, -1.5, 0]}
          fontSize={0.2}
          color="white"
          anchorX="center"
          anchorY="middle"
        >
          {config.name}
        </Text>
      )}
    </group>
  );
};

// Main Visualization Component
export const PolyhedraVisualization: React.FC<PolyhedraVisualizationProps> = ({
  polyhedra = [],
  enableInteractions = true,
  showLabels = true,
  onPolyhedronClick,
}) => {
  const [configs, setConfigs] = useState<PolyhedronConfig[]>(polyhedra);

  // Default polyhedra if none provided
  useEffect(() => {
    if (polyhedra.length === 0) {
      setConfigs([
        {
          name: 'Tetrahedron',
          type: 'tetrahedron',
          position: [-4, 0, 0],
          bqf: [4, 6, 4],
          color: 0xff0000,
        },
        {
          name: 'Cube',
          type: 'cube',
          position: [-2, 0, 0],
          bqf: [8, 12, 6],
          color: 0x00ff00,
        },
        {
          name: 'Octahedron',
          type: 'octahedron',
          position: [0, 0, 0],
          bqf: [6, 12, 8],
          color: 0x0000ff,
        },
        {
          name: 'Icosahedron',
          type: 'icosahedron',
          position: [2, 0, 0],
          bqf: [12, 30, 20],
          color: 0xffff00,
        },
        {
          name: 'Dodecahedron',
          type: 'dodecahedron',
          position: [4, 0, 0],
          bqf: [20, 30, 12],
          color: 0xff00ff,
        },
      ]);
    } else {
      setConfigs(polyhedra);
    }
  }, [polyhedra]);

  const handlePolyhedronClick = (config: PolyhedronConfig) => {
    if (onPolyhedronClick) {
      onPolyhedronClick(config);
    }
  };

  return (
    <div style={{ width: '100%', height: '100%' }}>
      <Canvas camera={{ position: [0, 0, 10], fov: 50 }}>
        <ambientLight intensity={0.5} />
        <directionalLight position={[10, 10, 5]} intensity={1} />
        <pointLight position={[-10, -10, -5]} intensity={0.5} />

        {configs.map((config, index) => (
          <PolyhedronMesh
            key={`${config.type}-${index}`}
            config={config}
            onClick={() => handlePolyhedronClick(config)}
            showLabel={showLabels}
          />
        ))}

        {enableInteractions && <OrbitControls />}
      </Canvas>
    </div>
  );
};

export default PolyhedraVisualization;

