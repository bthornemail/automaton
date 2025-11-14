/**
 * Mock Polyhedra Data
 * 
 * Mock data generators for polyhedra tests
 */

import type { BQF } from '../../bqf-transformation-service';
import type { PolyhedronConfig } from '../../../components/Polyhedra/PolyhedraVisualization';
import type { PolyhedraVectorClock } from '../../polyhedra-vector-clock-service';

/**
 * Mock BQF values for Platonic solids
 */
export const mockBQFs = {
  tetrahedron: [4, 6, 4] as BQF,
  cube: [8, 12, 6] as BQF,
  octahedron: [6, 12, 8] as BQF,
  icosahedron: [12, 30, 20] as BQF,
  dodecahedron: [20, 30, 12] as BQF,
};

/**
 * Mock BQF values for Archimedean solids
 */
export const mockArchimedeanBQFs = {
  cuboctahedron: [12, 24, 14] as BQF,
  rhombicuboctahedron: [24, 48, 26] as BQF,
};

/**
 * Create mock polyhedron config
 */
export function createMockPolyhedronConfig(
  name: string,
  type: PolyhedronConfig['type'],
  position: [number, number, number] = [0, 0, 0],
  bqf?: BQF
): PolyhedronConfig {
  return {
    name,
    type,
    position,
    bqf: bqf || mockBQFs[type],
    color: 0x00ff00,
    wireframe: false,
  };
}

/**
 * Create mock polyhedra vector clock
 */
export function createMockPolyhedraVectorClock(
  file: string = 'test.jsonl',
  line: number = 1,
  timestamp: number = 1000,
  pattern: string = 'test-pattern',
  polyhedronType: PolyhedraVectorClock['polyhedronType'] = 'cube',
  bqf: BQF = mockBQFs.cube
): PolyhedraVectorClock {
  return {
    vectorClock: [file, line, timestamp, pattern, ...bqf],
    polyhedronType,
    bqf,
    file,
    line,
    timestamp,
    pattern,
  };
}

/**
 * Create mock constraint pointer data
 */
export function createMockConstraintPointerData() {
  return {
    cubeBQF: mockBQFs.cube,
    octaBQF: mockBQFs.octahedron,
    icosaBQF: mockBQFs.icosahedron,
    dodecaBQF: mockBQFs.dodecahedron,
  };
}

/**
 * Create mock consensus data
 */
export function createMockConsensusData() {
  return {
    tetrahedronFacts: [2, 4, 6, 8],
    cubeTypes: [1, 2, 3, 4, 5, 6, 7, 8],
    octahedronPoints: [2, 3, 4, 5, 6, 7],
    icosahedronNodes: Array.from({ length: 12 }, (_, i) => i + 1),
    dodecahedronEntities: Array.from({ length: 20 }, (_, i) => i + 1),
  };
}

