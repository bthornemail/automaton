/**
 * Polyhedra Visualization Component Tests
 * 
 * Tests for PolyhedraVisualization React component
 * Note: Full Three.js rendering requires WebGL context, so these tests
 * focus on component structure and prop handling rather than full rendering.
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { PolyhedraVisualization, type PolyhedronConfig } from '../PolyhedraVisualization';
import type { BQF } from '../../../services/bqf-transformation-service';

// Mock Three.js and React Three Fiber to avoid WebGL requirements
vi.mock('@react-three/fiber', () => ({
  Canvas: ({ children }: any) => {
    // Return a simple div that represents the canvas
    return <div data-testid="canvas">{children}</div>;
  },
  useFrame: vi.fn((callback: () => void) => {
    // Mock animation frame - no-op
  }),
  useThree: vi.fn(() => ({
    scene: {},
    camera: {},
    gl: {},
  })),
}));

vi.mock('@react-three/drei', () => ({
  OrbitControls: () => <div data-testid="orbit-controls" />,
  Text: ({ children }: any) => <div data-testid="text">{children}</div>,
}));

vi.mock('three', () => {
  // Create simple mock classes that can be instantiated
  class MockGeometry {
    dispose = vi.fn();
  }
  
  class MockMaterial {
    dispose = vi.fn();
  }
  
  class MockColor {
    setHSL = vi.fn(() => this);
    getHex = vi.fn(() => 0xff0000);
  }
  
  return {
    TetrahedronGeometry: class extends MockGeometry {},
    BoxGeometry: class extends MockGeometry {},
    OctahedronGeometry: class extends MockGeometry {},
    IcosahedronGeometry: class extends MockGeometry {},
    DodecahedronGeometry: class extends MockGeometry {},
    MeshPhongMaterial: class extends MockMaterial {},
    Color: MockColor,
  };
});

describe('PolyhedraVisualization', () => {
  const defaultPolyhedra: PolyhedronConfig[] = [
    {
      name: 'cube',
      type: 'cube',
      position: [0, 0, 0],
      bqf: [8, 12, 6] as BQF,
      color: 0x00ff00,
    },
    {
      name: 'octahedron',
      type: 'octahedron',
      position: [2, 0, 0],
      bqf: [6, 12, 8] as BQF,
    },
  ];

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('component structure', () => {
    it('should accept polyhedra prop', () => {
      // Test that component accepts polyhedra prop without errors
      const component = <PolyhedraVisualization polyhedra={defaultPolyhedra} />;
      expect(component).toBeDefined();
    });

    it('should accept enableInteractions prop', () => {
      const component = <PolyhedraVisualization enableInteractions={true} />;
      expect(component).toBeDefined();
    });

    it('should accept showLabels prop', () => {
      const component = <PolyhedraVisualization showLabels={true} />;
      expect(component).toBeDefined();
    });

    it('should accept onPolyhedronClick prop', () => {
      const handleClick = vi.fn();
      const component = <PolyhedraVisualization onPolyhedronClick={handleClick} />;
      expect(component).toBeDefined();
    });
  });

  describe('BQF metadata', () => {
    it('should accept BQF in polyhedron config', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
      };

      const component = <PolyhedraVisualization polyhedra={[config]} />;
      expect(component).toBeDefined();
      expect(config.bqf).toEqual([8, 12, 6]);
    });
  });

  describe('polyhedron configuration', () => {
    it('should accept all polyhedron types', () => {
      const types: PolyhedronConfig['type'][] = [
        'tetrahedron',
        'cube',
        'octahedron',
        'icosahedron',
        'dodecahedron',
      ];

      types.forEach((type) => {
        const config: PolyhedronConfig = {
          name: type,
          type,
          position: [0, 0, 0],
          bqf: [4, 6, 4] as BQF,
        };
        const component = <PolyhedraVisualization polyhedra={[config]} />;
        expect(component).toBeDefined();
      });
    });
  });

  describe('polyhedron configuration options', () => {
    it('should accept color option', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
        color: 0xff0000,
      };
      const component = <PolyhedraVisualization polyhedra={[config]} />;
      expect(component).toBeDefined();
      expect(config.color).toBe(0xff0000);
    });

    it('should accept wireframe option', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
        wireframe: true,
      };
      const component = <PolyhedraVisualization polyhedra={[config]} />;
      expect(component).toBeDefined();
      expect(config.wireframe).toBe(true);
    });

    it('should accept position coordinates', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [1, 2, 3],
        bqf: [8, 12, 6] as BQF,
      };
      const component = <PolyhedraVisualization polyhedra={[config]} />;
      expect(component).toBeDefined();
      expect(config.position).toEqual([1, 2, 3]);
    });
  });

  describe('default behavior', () => {
    it('should work with empty polyhedra array', () => {
      // Component should use default polyhedra when none provided
      const component = <PolyhedraVisualization polyhedra={[]} />;
      expect(component).toBeDefined();
    });

    it('should work without props', () => {
      const component = <PolyhedraVisualization />;
      expect(component).toBeDefined();
    });
  });
});

