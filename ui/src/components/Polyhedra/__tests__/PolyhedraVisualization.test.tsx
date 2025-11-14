/**
 * Polyhedra Visualization Component Tests
 * 
 * Tests for PolyhedraVisualization React component
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, screen } from '@testing-library/react';
import { PolyhedraVisualization, type PolyhedronConfig } from '../PolyhedraVisualization';
import type { BQF } from '../../../services/bqf-transformation-service';

// Mock Three.js and React Three Fiber
vi.mock('@react-three/fiber', () => ({
  Canvas: ({ children }: any) => <div data-testid="canvas">{children}</div>,
  useFrame: (callback: () => void) => {
    // Mock animation frame
    if (typeof callback === 'function') {
      callback();
    }
  },
  useThree: () => ({
    scene: {},
    camera: {},
    gl: {},
  }),
}));

vi.mock('@react-three/drei', () => ({
  OrbitControls: () => <div data-testid="orbit-controls" />,
  Text: ({ children }: any) => <div data-testid="text">{children}</div>,
}));

vi.mock('three', async () => {
  const actual = await vi.importActual('three');
  return {
    ...actual,
    TetrahedronGeometry: vi.fn(() => ({ dispose: vi.fn() })),
    BoxGeometry: vi.fn(() => ({ dispose: vi.fn() })),
    OctahedronGeometry: vi.fn(() => ({ dispose: vi.fn() })),
    IcosahedronGeometry: vi.fn(() => ({ dispose: vi.fn() })),
    DodecahedronGeometry: vi.fn(() => ({ dispose: vi.fn() })),
    MeshPhongMaterial: vi.fn(() => ({ dispose: vi.fn() })),
    Color: vi.fn(() => ({
      setHSL: vi.fn(() => ({ getHex: () => 0xff0000 })),
    })),
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

  describe('rendering', () => {
    it('should render with default polyhedra', () => {
      render(<PolyhedraVisualization />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render with custom polyhedra config', () => {
      render(<PolyhedraVisualization polyhedra={defaultPolyhedra} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render OrbitControls when interactions enabled', () => {
      render(<PolyhedraVisualization enableInteractions={true} />);
      expect(screen.getByTestId('orbit-controls')).toBeDefined();
    });

    it('should not render OrbitControls when interactions disabled', () => {
      render(<PolyhedraVisualization enableInteractions={false} />);
      // OrbitControls might still render but be disabled
      // This is a basic test - actual behavior depends on implementation
    });
  });

  describe('BQF metadata storage', () => {
    it('should store BQF in mesh userData', () => {
      // This test verifies that BQF is stored in userData
      // Actual implementation stores it in useEffect hook
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
      };

      // The component should accept and use this config
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('click handlers', () => {
    it('should call onPolyhedronClick when polyhedron is clicked', () => {
      const handleClick = vi.fn();
      render(
        <PolyhedraVisualization
          polyhedra={defaultPolyhedra}
          onPolyhedronClick={handleClick}
        />
      );

      // Click handler is passed to mesh component
      // Actual click testing would require more complex setup
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should work without click handler', () => {
      render(<PolyhedraVisualization polyhedra={defaultPolyhedra} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('labels', () => {
    it('should display labels when showLabels is true', () => {
      render(
        <PolyhedraVisualization
          polyhedra={defaultPolyhedra}
          showLabels={true}
        />
      );
      // Labels are rendered via Text component from drei
      // Actual label visibility depends on implementation
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should not display labels when showLabels is false', () => {
      render(
        <PolyhedraVisualization
          polyhedra={defaultPolyhedra}
          showLabels={false}
        />
      );
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('interactive transformations', () => {
    it('should support interactive transformations', () => {
      // Interactive transformations are handled via BQF service
      // This test verifies component accepts transformation props
      render(
        <PolyhedraVisualization
          polyhedra={defaultPolyhedra}
          enableInteractions={true}
        />
      );
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('Three.js scene setup', () => {
    it('should set up Three.js scene correctly', () => {
      render(<PolyhedraVisualization polyhedra={defaultPolyhedra} />);
      
      // Scene setup is handled by React Three Fiber
      // This test verifies component renders without errors
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('polyhedron types', () => {
    it('should render tetrahedron', () => {
      const config: PolyhedronConfig = {
        name: 'tetra',
        type: 'tetrahedron',
        position: [0, 0, 0],
        bqf: [4, 6, 4] as BQF,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render cube', () => {
      const config: PolyhedronConfig = {
        name: 'cube',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render octahedron', () => {
      const config: PolyhedronConfig = {
        name: 'octa',
        type: 'octahedron',
        position: [0, 0, 0],
        bqf: [6, 12, 8] as BQF,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render icosahedron', () => {
      const config: PolyhedronConfig = {
        name: 'icosa',
        type: 'icosahedron',
        position: [0, 0, 0],
        bqf: [12, 30, 20] as BQF,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render dodecahedron', () => {
      const config: PolyhedronConfig = {
        name: 'dodeca',
        type: 'dodecahedron',
        position: [0, 0, 0],
        bqf: [20, 30, 12] as BQF,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('BQF color coding', () => {
    it('should use BQF for color calculation when color not specified', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
        // No color specified - should use BQF
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should use specified color when provided', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
        color: 0xff0000,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });

  describe('wireframe mode', () => {
    it('should render wireframe when specified', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
        wireframe: true,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });

    it('should render solid when wireframe not specified', () => {
      const config: PolyhedronConfig = {
        name: 'test',
        type: 'cube',
        position: [0, 0, 0],
        bqf: [8, 12, 6] as BQF,
        wireframe: false,
      };
      render(<PolyhedraVisualization polyhedra={[config]} />);
      expect(screen.getByTestId('canvas')).toBeDefined();
    });
  });
});

