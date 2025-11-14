/**
 * Archimedean Solids Geometries
 * 
 * Custom Three.js geometries for Archimedean solids
 * Focus on Cuboctahedron (Archimedes 6) and Rhombicuboctahedron (Archimedes 7)
 * 
 * Source: docs/32-Regulay-Polyhedra-Geometry/02-ARCHIMEDEAN-SOLIDS.md
 */

import * as THREE from 'three';

/**
 * Create Cuboctahedron (Archimedes 6) geometry
 * 
 * Geometric Properties:
 * - Vertices: 12
 * - Edges: 24
 * - Faces: 14 (8 triangles + 6 squares)
 * - BQF Encoding: [12, 24, 14]
 * 
 * Computational Properties:
 * - Type: Consensus pattern (balanced expansion)
 * - Consensus: 6 squares for stable agreement, 8 triangles for multi-view consensus
 */
export function createCuboctahedronGeometry(radius: number = 1): THREE.BufferGeometry {
  const geometry = new THREE.BufferGeometry();
  
  // Cuboctahedron vertices (12 vertices at edge midpoints of cube/octahedron)
  // Coordinates for a cuboctahedron centered at origin
  const vertices = new Float32Array([
    // Top square (z = radius)
    1, 1, 0,   // 0
    1, -1, 0,  // 1
    -1, -1, 0, // 2
    -1, 1, 0,  // 3
    
    // Middle square (y = 0)
    1, 0, 1,   // 4
    1, 0, -1,  // 5
    -1, 0, -1, // 6
    -1, 0, 1,  // 7
    
    // Bottom square (z = -radius)
    0, 1, 1,   // 8
    0, 1, -1,  // 9
    0, -1, -1, // 10
    0, -1, 1,  // 11
  ]);
  
  // Scale by radius
  for (let i = 0; i < vertices.length; i += 3) {
    vertices[i] *= radius;
    vertices[i + 1] *= radius;
    vertices[i + 2] *= radius;
  }
  
  // Cuboctahedron faces (14 faces: 8 triangles + 6 squares)
  // Triangles (8):
  // Top triangles
  const triangles = [
    0, 4, 8,   // Top front triangle
    0, 9, 5,   // Top right triangle
    1, 5, 10,  // Right bottom triangle
    1, 11, 4,  // Right front triangle
    2, 6, 10,  // Bottom back triangle
    2, 9, 6,   // Bottom left triangle
    3, 7, 8,   // Left front triangle
    3, 6, 7,   // Left back triangle
  ];
  
  // Squares (6):
  const squares = [
    0, 1, 5, 4,   // Top square (z=0 plane)
    1, 2, 6, 5,   // Right square
    2, 3, 7, 6,   // Bottom square
    3, 0, 4, 7,   // Left square
    0, 3, 9, 8,   // Front square (y=0 plane)
    1, 10, 11, 4, // Back square
  ];
  
  // Combine triangles and squares into indices
  const indices: number[] = [];
  
  // Add triangles (3 indices each)
  for (let i = 0; i < triangles.length; i += 3) {
    indices.push(triangles[i], triangles[i + 1], triangles[i + 2]);
  }
  
  // Add squares (2 triangles per square)
  for (let i = 0; i < squares.length; i += 4) {
    indices.push(
      squares[i], squares[i + 1], squares[i + 2],
      squares[i], squares[i + 2], squares[i + 3]
    );
  }
  
  geometry.setAttribute('position', new THREE.BufferAttribute(vertices, 3));
  geometry.setIndex(indices);
  geometry.computeVertexNormals();
  
  return geometry;
}

/**
 * Create Rhombicuboctahedron (Archimedes 7) geometry
 * 
 * Geometric Properties:
 * - Vertices: 24
 * - Edges: 48
 * - Faces: 26 (8 triangles + 18 squares)
 * - BQF Encoding: [24, 48, 26]
 * 
 * Computational Properties:
 * - Type: Constraint pointer (expanded symmetry)
 * - Constraint: Rhombic twists create directional pointers
 */
export function createRhombicuboctahedronGeometry(radius: number = 1): THREE.BufferGeometry {
  const geometry = new THREE.BufferGeometry();
  
  // Rhombicuboctahedron vertices (24 vertices)
  // This is a simplified approximation - full implementation would require
  // precise calculation of all 24 vertices
  const vertices = new Float32Array([
    // Top layer (8 vertices)
    1, 1, 1 + Math.sqrt(2),   // 0
    1, -1, 1 + Math.sqrt(2),   // 1
    -1, -1, 1 + Math.sqrt(2),  // 2
    -1, 1, 1 + Math.sqrt(2),   // 3
    1 + Math.sqrt(2), 1, 1,    // 4
    1 + Math.sqrt(2), -1, 1,   // 5
    -1 - Math.sqrt(2), -1, 1,  // 6
    -1 - Math.sqrt(2), 1, 1,   // 7
    
    // Middle layer (8 vertices)
    1, 1 + Math.sqrt(2), 1,    // 8
    1, 1 + Math.sqrt(2), -1,   // 9
    1, -1 - Math.sqrt(2), -1,  // 10
    1, -1 - Math.sqrt(2), 1,   // 11
    -1, 1 + Math.sqrt(2), 1,   // 12
    -1, 1 + Math.sqrt(2), -1,  // 13
    -1, -1 - Math.sqrt(2), -1, // 14
    -1, -1 - Math.sqrt(2), 1,  // 15
    
    // Bottom layer (8 vertices)
    1, 1, -1 - Math.sqrt(2),   // 16
    1, -1, -1 - Math.sqrt(2),  // 17
    -1, -1, -1 - Math.sqrt(2), // 18
    -1, 1, -1 - Math.sqrt(2),  // 19
    1 + Math.sqrt(2), 1, -1,   // 20
    1 + Math.sqrt(2), -1, -1,  // 21
    -1 - Math.sqrt(2), -1, -1, // 22
    -1 - Math.sqrt(2), 1, -1,  // 23
  ]);
  
  // Scale by radius
  const scale = radius / (1 + Math.sqrt(2));
  for (let i = 0; i < vertices.length; i++) {
    vertices[i] *= scale;
  }
  
  // Rhombicuboctahedron faces (26 faces: 8 triangles + 18 squares)
  // This is a simplified face structure
  // Full implementation would require precise face calculation
  const indices: number[] = [
    // Top triangles (4)
    0, 4, 8,
    1, 5, 11,
    2, 6, 15,
    3, 7, 12,
    
    // Bottom triangles (4)
    16, 20, 9,
    17, 21, 10,
    18, 22, 14,
    19, 23, 13,
    
    // Squares (18) - simplified representation
    // Top squares
    0, 1, 5, 4,
    1, 2, 6, 5,
    2, 3, 7, 6,
    3, 0, 4, 7,
    // Middle squares
    4, 5, 11, 8,
    5, 6, 15, 11,
    6, 7, 12, 15,
    7, 4, 8, 12,
    // Side squares
    8, 9, 20, 4,
    9, 13, 23, 20,
    10, 14, 22, 21,
    11, 15, 14, 10,
    // Bottom squares
    16, 17, 21, 20,
    17, 18, 22, 21,
    18, 19, 23, 22,
    19, 16, 20, 23,
    // Additional squares
    12, 13, 9, 8,
    13, 19, 23, 12,
  ];
  
  // Convert squares to triangles
  const triangleIndices: number[] = [];
  for (let i = 0; i < indices.length; i += 4) {
    triangleIndices.push(
      indices[i], indices[i + 1], indices[i + 2],
      indices[i], indices[i + 2], indices[i + 3]
    );
  }
  
  geometry.setAttribute('position', new THREE.BufferAttribute(vertices, 3));
  geometry.setIndex(triangleIndices);
  geometry.computeVertexNormals();
  
  return geometry;
}

/**
 * Get BQF encoding for Archimedean solid
 */
export function getArchimedeanBQF(type: 'cuboctahedron' | 'rhombicuboctahedron'): [number, number, number] {
  switch (type) {
    case 'cuboctahedron':
      return [12, 24, 14]; // Vertices, edges, faces
    case 'rhombicuboctahedron':
      return [24, 48, 26];
    default:
      return [0, 0, 0];
  }
}

