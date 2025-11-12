/**
 * Provenance Avatar Component
 * 
 * Renders GLTF avatars for provenance nodes in the 3D scene.
 */

import React, { useEffect, useRef, useState } from 'react';
import { useFrame } from '@react-three/fiber';
import { Text } from '@react-three/drei';
import * as THREE from 'three';
import { ProvenanceNode } from '../../services/provenance-slide-service';
import { avatarLoaderService } from '../../services/avatar-loader-service';

interface ProvenanceAvatarProps {
  node: ProvenanceNode;
  isSelected: boolean;
  isHovered: boolean;
  onClick: () => void;
  onHover: (hovered: boolean) => void;
}

export const ProvenanceAvatar: React.FC<ProvenanceAvatarProps> = ({
  node,
  isSelected,
  isHovered,
  onClick,
  onHover
}) => {
  const groupRef = useRef<THREE.Group>(null);
  const [avatarModel, setAvatarModel] = useState<THREE.Group | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  // Load avatar when node or avatar config changes
  useEffect(() => {
    if (!node.avatar) {
      setLoading(false);
      return;
    }

    setLoading(true);
    setError(null);

    avatarLoaderService.loadAvatar(node.avatar)
      .then(model => {
        setAvatarModel(model);
        setLoading(false);
      })
      .catch(err => {
        console.error('Failed to load avatar:', err);
        setError(err.message);
        setLoading(false);
      });
  }, [node.avatar?.gltfModel, node.avatar?.type]);

  // Animation and interaction
  useFrame((state) => {
    if (groupRef.current) {
      // Gentle rotation
      groupRef.current.rotation.y = state.clock.elapsedTime * 0.2;
      
      // Pulsing for selected
      if (isSelected) {
        const scale = 1 + Math.sin(state.clock.elapsedTime * 2) * 0.1;
        groupRef.current.scale.setScalar(scale);
      } else {
        groupRef.current.scale.setScalar(1);
      }

      // Hover effect
      if (isHovered) {
        groupRef.current.position.y = node.position[1] + 0.2;
      } else {
        groupRef.current.position.y = node.position[1];
      }
    }
  });

  if (!node.avatar) {
    return null;
  }

  if (loading) {
    return (
      <group position={[node.position[0], node.position[1], node.position[2]]}>
        <mesh>
          <sphereGeometry args={[0.3, 16, 16]} />
          <meshStandardMaterial color="#888888" />
        </mesh>
      </group>
    );
  }

  if (error || !avatarModel) {
    // Fallback to simple sphere
    return (
      <group 
        ref={groupRef}
        position={[node.position[0], node.position[1], node.position[2]]}
        onClick={onClick}
        onPointerOver={() => onHover(true)}
        onPointerOut={() => onHover(false)}
      >
        <mesh>
          <sphereGeometry args={[0.5, 16, 16]} />
          <meshStandardMaterial 
            color={node.avatar.type === 'ai-agent' ? '#00ff88' : '#ffffff'} 
          />
        </mesh>
        {node.avatar.label && (
          <Text
            position={[0, 1, 0]}
            fontSize={0.3}
            color="white"
            anchorX="center"
            anchorY="middle"
          >
            {node.avatar.label}
          </Text>
        )}
      </group>
    );
  }

  return (
    <group
      ref={groupRef}
      position={[node.position[0], node.position[1], node.position[2]]}
      onClick={onClick}
      onPointerOver={() => onHover(true)}
      onPointerOut={() => onHover(false)}
    >
      <primitive object={avatarModel.clone()} />
      
      {/* Avatar label */}
      {node.avatar.label && (
        <Text
          position={[0, 2, 0]}
          fontSize={0.3}
          color="white"
          anchorX="center"
          anchorY="middle"
        >
          {node.avatar.label}
        </Text>
      )}
      
      {/* Selection indicator */}
      {isSelected && (
        <mesh position={[0, 0, 0]}>
          <ringGeometry args={[1.5, 1.6, 32]} />
          <meshBasicMaterial color="#3b82f6" side={THREE.DoubleSide} transparent opacity={0.6} />
        </mesh>
      )}

      {/* Hover indicator */}
      {isHovered && !isSelected && (
        <mesh position={[0, 0, 0]}>
          <ringGeometry args={[1.3, 1.4, 32]} />
          <meshBasicMaterial color="#ffffff" side={THREE.DoubleSide} transparent opacity={0.4} />
        </mesh>
      )}
    </group>
  );
};

