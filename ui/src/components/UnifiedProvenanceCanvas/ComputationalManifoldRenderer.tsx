/**
 * Computational Manifold Renderer Component
 * 
 * Renders computational manifold visualizations with 3D spatial encoding,
 * evaluation traces, and polynomial shaders.
 */

import React, { useRef, useMemo } from 'react';
import { useFrame } from '@react-three/fiber';
import * as THREE from 'three';
import { ProvenanceNode } from '../../services/provenance-slide-service';
import { computationalManifoldService, TypeSpaceCoordinates, EvaluationKeyframe } from '../../services/computational-manifold-service';

// Shader source code (inline for now - can be loaded from files with proper build config)
const evaluationVertexShader = `
  uniform float evaluationTime;
  uniform vec3 evaluationPosition;
  uniform float evaluationScale;

  attribute vec3 position;
  attribute vec3 normal;

  varying vec3 vPosition;
  varying vec3 vNormal;

  void main() {
    vec3 transformedPosition = position;
    transformedPosition += sin(evaluationTime) * normal * 0.1;
    transformedPosition *= evaluationScale;
    transformedPosition += evaluationPosition;
    
    vPosition = transformedPosition;
    vNormal = normal;
    
    gl_Position = projectionMatrix * modelViewMatrix * vec4(transformedPosition, 1.0);
  }
`;

const polynomialFragmentShader = `
  uniform vec3 monadCoords;
  uniform vec3 functorCoords; 
  uniform vec3 perceptronCoords;
  uniform float evaluationTime;
  uniform vec3 yCombinator;
  uniform vec3 zCombinator;
  uniform float opacity;

  varying vec3 vPosition;
  varying vec3 vNormal;

  void main() {
    float monadRing = sin(length(vPosition - monadCoords) * 10.0 - evaluationTime);
    float functorRing = sin(length(vPosition - functorCoords) * 15.0 - evaluationTime * 1.5);
    float perceptronRing = sin(length(vPosition - perceptronCoords) * 20.0 - evaluationTime * 2.0);
    
    vec3 yField = normalize(vPosition - yCombinator);
    vec3 zField = normalize(vPosition - zCombinator);
    float combinatorEffect = dot(yField, zField);
    
    vec3 color = vec3(
      monadRing * 0.8 + combinatorEffect * 0.2,
      functorRing * 0.6 + combinatorEffect * 0.4, 
      perceptronRing * 0.7 + combinatorEffect * 0.3
    );
    
    color = normalize(color) * 0.5 + 0.5;
    
    gl_FragColor = vec4(color, opacity);
  }
`;

const combinatorShader = `
  uniform vec3 yCombinator;
  uniform vec3 zCombinator;
  uniform vec3 mCombinator;
  uniform vec3 sCombinator;
  uniform float fieldStrength;

  varying vec3 vPosition;
  varying vec3 vNormal;

  void main() {
    float distY = length(vPosition - yCombinator);
    float distZ = length(vPosition - zCombinator);
    float distM = length(vPosition - mCombinator);
    float distS = length(vPosition - sCombinator);
    
    float fieldY = fieldStrength / (distY * distY + 1.0);
    float fieldZ = fieldStrength / (distZ * distZ + 1.0);
    float fieldM = fieldStrength / (distM * distM + 1.0);
    float fieldS = fieldStrength / (distS * distS + 1.0);
    
    float totalField = fieldY + fieldZ + fieldM + fieldS;
    
    vec3 color = vec3(
      fieldY * 0.5 + fieldM * 0.3,
      fieldZ * 0.5 + fieldS * 0.3,
      totalField * 0.2
    );
    
    color = normalize(color) * min(1.0, totalField);
    
    gl_FragColor = vec4(color, 0.8);
  }
`;

interface ComputationalManifoldRendererProps {
  nodes: ProvenanceNode[];
  evaluationTime?: number;
  showCombinators?: boolean;
  showEvaluationTraces?: boolean;
}

interface ManifoldNodeProps {
  node: ProvenanceNode;
  coords: TypeSpaceCoordinates;
  evaluationTime: number;
  isSelected: boolean;
}

const ManifoldNode: React.FC<ManifoldNodeProps> = ({
  node,
  coords,
  evaluationTime,
  isSelected
}) => {
  const meshRef = useRef<THREE.Mesh>(null);
  
  // Create shader material
  const material = useMemo(() => {
    return new THREE.ShaderMaterial({
      vertexShader: evaluationVertexShader,
      fragmentShader: polynomialFragmentShader,
      uniforms: {
        monadCoords: { value: new THREE.Vector3(...coords.position) },
        functorCoords: { value: new THREE.Vector3(...coords.position).multiplyScalar(1.2) },
        perceptronCoords: { value: new THREE.Vector3(...coords.position).multiplyScalar(1.5) },
        evaluationTime: { value: evaluationTime },
        yCombinator: { value: new THREE.Vector3(0, 0, 0) },
        zCombinator: { value: new THREE.Vector3(2, 0, 0) },
        opacity: { value: coords.opacity }
      },
      transparent: true,
      side: THREE.DoubleSide
    });
  }, [coords, evaluationTime]);

  useFrame(() => {
    if (meshRef.current && material) {
      // Update evaluation time uniform
      (material.uniforms.evaluationTime.value as number) = evaluationTime;
    }
  });

  return (
    <mesh
      ref={meshRef}
      position={coords.position}
      rotation={coords.rotation}
      scale={coords.scale}
      material={material}
    >
      <icosahedronGeometry args={[0.5, 0]} />
    </mesh>
  );
};

const CombinatorField: React.FC<{
  position: [number, number, number];
  type: 'y' | 'z' | 'm' | 's';
  fieldStrength: number;
}> = ({ position, type, fieldStrength }) => {
  const material = useMemo(() => {
    return new THREE.ShaderMaterial({
      vertexShader: `
        varying vec3 vPosition;
        void main() {
          vPosition = position;
          gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
        }
      `,
      fragmentShader: combinatorShader,
      uniforms: {
        yCombinator: { value: new THREE.Vector3(...(type === 'y' ? position : [0, 0, 0])) },
        zCombinator: { value: new THREE.Vector3(...(type === 'z' ? position : [0, 0, 0])) },
        mCombinator: { value: new THREE.Vector3(...(type === 'm' ? position : [0, 0, 0])) },
        sCombinator: { value: new THREE.Vector3(...(type === 's' ? position : [0, 0, 0])) },
        fieldStrength: { value: fieldStrength }
      },
      transparent: true,
      side: THREE.DoubleSide
    });
  }, [position, type, fieldStrength]);

  return (
    <mesh position={position} material={material}>
      <sphereGeometry args={[1, 32, 32]} />
    </mesh>
  );
};

const EvaluationTrace: React.FC<{
  keyframes: EvaluationKeyframe[];
  currentTime: number;
}> = ({ keyframes, currentTime }) => {
  // Find current keyframe
  const currentKeyframe = keyframes.find(
    kf => currentTime >= kf.time && currentTime < kf.time + kf.duration
  ) || keyframes[0];

  if (!currentKeyframe) return null;

  const progress = currentKeyframe.duration > 0
    ? (currentTime - currentKeyframe.time) / currentKeyframe.duration
    : 0;

  // Render particle effect based on type
  const effect = currentKeyframe.effect;
  
  return (
    <group>
      {effect.type === 'particle-burst' && (
        <points>
          <bufferGeometry>
            <bufferAttribute
              attach="attributes-position"
              count={100}
              array={new Float32Array(300).fill(0).map(() => Math.random() * 2 - 1)}
              itemSize={3}
            />
          </bufferGeometry>
          <pointsMaterial
            color={effect.color}
            size={0.1 * (effect.intensity || 1)}
            transparent
            opacity={1 - progress}
          />
        </points>
      )}
    </group>
  );
};

export const ComputationalManifoldRenderer: React.FC<ComputationalManifoldRendererProps> = ({
  nodes,
  evaluationTime = 0,
  showCombinators = true,
  showEvaluationTraces = false
}) => {
  const [currentEvaluationTime, setCurrentEvaluationTime] = React.useState(evaluationTime);

  useFrame((state) => {
    setCurrentEvaluationTime(state.clock.elapsedTime);
  });

  // Map nodes to type-space coordinates
  const nodeCoords = useMemo(() => {
    return nodes.map(node => ({
      node,
      coords: computationalManifoldService.mapNodeToTypeSpace(node)
    }));
  }, [nodes]);

  // Combinator positions (Y, Z, M, S combinators)
  const combinatorPositions = useMemo(() => [
    { type: 'y' as const, position: [0, 0, 0] as [number, number, number] },
    { type: 'z' as const, position: [2, 0, 0] as [number, number, number] },
    { type: 'm' as const, position: [0, 2, 0] as [number, number, number] },
    { type: 's' as const, position: [0, 0, 2] as [number, number, number] }
  ], []);

  return (
    <group>
      {/* Render nodes in type-space */}
      {nodeCoords.map(({ node, coords }) => (
        <ManifoldNode
          key={node.id}
          node={node}
          coords={coords}
          evaluationTime={currentEvaluationTime}
          isSelected={false}
        />
      ))}

      {/* Render combinator fields */}
      {showCombinators && combinatorPositions.map(({ type, position }) => (
        <CombinatorField
          key={type}
          position={position}
          type={type}
          fieldStrength={0.5}
        />
      ))}

      {/* Render evaluation traces */}
      {showEvaluationTraces && nodes.map(node => {
        // Generate mock evaluation trace for demonstration
        const trace = [
          { type: 'β-reduction' as const, before: null, after: null },
          { type: 'primitive-application' as const, operator: '+', args: [1, 2] }
        ];
        const keyframes = computationalManifoldService.evalTraceToAnimation(trace);
        
        return (
          <EvaluationTrace
            key={`trace-${node.id}`}
            keyframes={keyframes}
            currentTime={currentEvaluationTime}
          />
        );
      })}
    </group>
  );
};

