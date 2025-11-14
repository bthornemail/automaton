/**
 * Archimedean Solids Geometry Tests
 * 
 * Tests for Archimedean solids geometries
 */

import { describe, it, expect } from 'vitest';
import * as THREE from 'three';
import {
  createCuboctahedronGeometry,
  createRhombicuboctahedronGeometry,
  getArchimedeanBQF
} from '../archimedean-solids';

describe('Archimedean Solids Geometries', () => {
  describe('createCuboctahedronGeometry', () => {
    it('should create valid cuboctahedron geometry', () => {
      const geometry = createCuboctahedronGeometry(1);
      
      expect(geometry).toBeInstanceOf(THREE.BufferGeometry);
      expect(geometry.attributes.position).toBeDefined();
      expect(geometry.index).toBeDefined();
    });

    it('should have correct vertex count (12 vertices)', () => {
      const geometry = createCuboctahedronGeometry(1);
      const position = geometry.attributes.position;
      
      expect(position.count).toBe(12);
    });

    it('should have valid face indices', () => {
      const geometry = createCuboctahedronGeometry(1);
      const index = geometry.index;
      
      expect(index).toBeDefined();
      expect(index!.count).toBeGreaterThan(0);
    });

    it('should compute vertex normals', () => {
      const geometry = createCuboctahedronGeometry(1);
      
      expect(geometry.attributes.normal).toBeDefined();
      expect(geometry.attributes.normal.count).toBe(12);
    });

    it('should scale with radius parameter', () => {
      const geometry1 = createCuboctahedronGeometry(1);
      const geometry2 = createCuboctahedronGeometry(2);
      
      const pos1 = geometry1.attributes.position;
      const pos2 = geometry2.attributes.position;
      
      // Check that positions are scaled
      expect(pos2.getX(0)).toBeCloseTo(pos1.getX(0) * 2, 5);
    });

    it('should create geometry with default radius', () => {
      const geometry = createCuboctahedronGeometry();
      expect(geometry).toBeInstanceOf(THREE.BufferGeometry);
    });
  });

  describe('createRhombicuboctahedronGeometry', () => {
    it('should create valid rhombicuboctahedron geometry', () => {
      const geometry = createRhombicuboctahedronGeometry(1);
      
      expect(geometry).toBeInstanceOf(THREE.BufferGeometry);
      expect(geometry.attributes.position).toBeDefined();
      expect(geometry.index).toBeDefined();
    });

    it('should have correct vertex count (24 vertices)', () => {
      const geometry = createRhombicuboctahedronGeometry(1);
      const position = geometry.attributes.position;
      
      expect(position.count).toBe(24);
    });

    it('should have valid face indices', () => {
      const geometry = createRhombicuboctahedronGeometry(1);
      const index = geometry.index;
      
      expect(index).toBeDefined();
      expect(index!.count).toBeGreaterThan(0);
    });

    it('should compute vertex normals', () => {
      const geometry = createRhombicuboctahedronGeometry(1);
      
      expect(geometry.attributes.normal).toBeDefined();
      expect(geometry.attributes.normal.count).toBe(24);
    });

    it('should scale with radius parameter', () => {
      const geometry1 = createRhombicuboctahedronGeometry(1);
      const geometry2 = createRhombicuboctahedronGeometry(2);
      
      const pos1 = geometry1.attributes.position;
      const pos2 = geometry2.attributes.position;
      
      // Check that positions are scaled (approximately)
      const scale = 1 + Math.sqrt(2); // Scale factor used in implementation
      expect(Math.abs(pos2.getX(0))).toBeGreaterThan(Math.abs(pos1.getX(0)));
    });

    it('should create geometry with default radius', () => {
      const geometry = createRhombicuboctahedronGeometry();
      expect(geometry).toBeInstanceOf(THREE.BufferGeometry);
    });
  });

  describe('getArchimedeanBQF', () => {
    it('should return correct BQF for cuboctahedron', () => {
      const bqf = getArchimedeanBQF('cuboctahedron');
      
      expect(bqf).toEqual([12, 24, 14]);
      expect(bqf).toHaveLength(3);
    });

    it('should return correct BQF for rhombicuboctahedron', () => {
      const bqf = getArchimedeanBQF('rhombicuboctahedron');
      
      expect(bqf).toEqual([24, 48, 26]);
      expect(bqf).toHaveLength(3);
    });

    it('should return [0,0,0] for unknown type', () => {
      const bqf = getArchimedeanBQF('unknown' as any);
      
      expect(bqf).toEqual([0, 0, 0]);
    });
  });

  describe('geometry properties', () => {
    it('should create cuboctahedron with correct BQF encoding', () => {
      const geometry = createCuboctahedronGeometry(1);
      const bqf = getArchimedeanBQF('cuboctahedron');
      
      // BQF: [vertices, edges, faces] = [12, 24, 14]
      expect(bqf[0]).toBe(12); // Vertices
      expect(bqf[1]).toBe(24); // Edges
      expect(bqf[2]).toBe(14); // Faces (8 triangles + 6 squares)
      
      // Verify geometry has correct vertex count
      expect(geometry.attributes.position.count).toBe(bqf[0]);
    });

    it('should create rhombicuboctahedron with correct BQF encoding', () => {
      const geometry = createRhombicuboctahedronGeometry(1);
      const bqf = getArchimedeanBQF('rhombicuboctahedron');
      
      // BQF: [vertices, edges, faces] = [24, 48, 26]
      expect(bqf[0]).toBe(24); // Vertices
      expect(bqf[1]).toBe(48); // Edges
      expect(bqf[2]).toBe(26); // Faces (8 triangles + 18 squares)
      
      // Verify geometry has correct vertex count
      expect(geometry.attributes.position.count).toBe(bqf[0]);
    });
  });

  describe('integration tests', () => {
    it('should create both geometries and verify BQF encodings', () => {
      const cubocta = createCuboctahedronGeometry(1);
      const rhombic = createRhombicuboctahedronGeometry(1);
      
      const cuboctaBQF = getArchimedeanBQF('cuboctahedron');
      const rhombicBQF = getArchimedeanBQF('rhombicuboctahedron');
      
      expect(cubocta.attributes.position.count).toBe(cuboctaBQF[0]);
      expect(rhombic.attributes.position.count).toBe(rhombicBQF[0]);
      
      // Rhombicuboctahedron should have more vertices than cuboctahedron
      expect(rhombicBQF[0]).toBeGreaterThan(cuboctaBQF[0]);
    });

    it('should create geometries usable with Three.js materials', () => {
      const cubocta = createCuboctahedronGeometry(1);
      const material = new THREE.MeshBasicMaterial({ color: 0x00ff00 });
      const mesh = new THREE.Mesh(cubocta, material);
      
      expect(mesh.geometry).toBe(cubocta);
      expect(mesh.material).toBe(material);
    });
  });
});

