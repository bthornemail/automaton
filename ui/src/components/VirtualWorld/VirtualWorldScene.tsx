/**
 * Virtual World Scene Component
 * Main scene component integrating terrain, skybox, and world layout
 */

import React, { Suspense } from 'react';
import { Canvas } from '@react-three/fiber';
import { OrbitControls } from '@react-three/drei';
import { VirtualWorldTerrain, TerrainConfig } from './VirtualWorldTerrain';
import { VirtualWorldSkybox, SkyboxConfig, AtmosphericFog } from './VirtualWorldSkybox';
import { WorldLayoutProvider, useWorldLayout } from './WorldLayoutManager';
import { ProvenanceCanvasErrorBoundary } from '../shared/ProvenanceCanvasErrorBoundary';
import * as THREE from 'three';

export interface VirtualWorldSceneConfig {
  terrain?: TerrainConfig;
  skybox?: SkyboxConfig;
  fog?: {
    color?: string;
    near?: number;
    far?: number;
  };
  camera?: {
    position?: [number, number, number];
    fov?: number;
  };
  enableControls?: boolean;
}

interface VirtualWorldSceneProps {
  config?: VirtualWorldSceneConfig;
  children?: React.ReactNode;
}

// Inner scene component that uses the layout context
const SceneContent: React.FC<{
  config?: VirtualWorldSceneConfig;
  children?: React.ReactNode;
}> = ({ config, children }) => {
  const { layout } = useWorldLayout();

  return (
    <>
      {/* Lighting - Ensure sufficient light for visibility */}
      <ambientLight intensity={0.8} color="#ffffff" />
      <directionalLight
        position={[10, 10, 5]}
        intensity={1.2}
        castShadow
        shadow-mapSize-width={2048}
        shadow-mapSize-height={2048}
        shadow-camera-far={50}
        shadow-camera-left={-50}
        shadow-camera-right={50}
        shadow-camera-top={50}
        shadow-camera-bottom={-50}
        color="#ffffff"
      />
      <pointLight position={[-10, 10, -10]} intensity={0.6} color="#ffffff" />
      <hemisphereLight
        args={[0x87ceeb, 0x4a5568, 0.5]}
      />
      
      {/* Test object to verify rendering works */}
      {import.meta.env.DEV && (
        <mesh position={[0, 2, 0]}>
          <boxGeometry args={[2, 2, 2]} />
          <meshStandardMaterial color="#ff6b6b" />
        </mesh>
      )}
      
      {/* Skybox */}
      <VirtualWorldSkybox config={config?.skybox} />
      
      {/* Atmospheric fog */}
      {config?.fog && (
        <AtmosphericFog
          color={config.fog.color}
          near={config.fog.near}
          far={config.fog.far}
        />
      )}
      
      {/* Terrain */}
      <VirtualWorldTerrain config={config?.terrain} />
      
      {/* Zone visualization (debug) */}
      {import.meta.env.DEV && (
        <ZoneVisualization />
      )}
      
      {/* Children components (avatars, buildings, etc.) */}
      {children}
      
      {/* Camera controls */}
      {config?.enableControls !== false && (
        <OrbitControls
          enableDamping
          dampingFactor={0.05}
          minDistance={5}
          maxDistance={200}
          enablePan
          enableZoom
          enableRotate
          target={[0, 0, 0]}
        />
      )}
    </>
  );
};

// Zone visualization for debugging
const ZoneVisualization: React.FC = () => {
  const { layout } = useWorldLayout();

  return (
    <>
      {layout.zones.map(zone => {
        const { min, max } = zone.bounds;
        const width = max[0] - min[0];
        const height = max[1] - min[1];
        const depth = max[2] - min[2];
        const center: [number, number, number] = [
          (min[0] + max[0]) / 2,
          (min[1] + max[1]) / 2,
          (min[2] + max[2]) / 2
        ];

        return (
          <mesh key={zone.id} position={center}>
            <boxGeometry args={[width, height, depth]} />
            <meshBasicMaterial
              color={zone.color}
              opacity={0.1}
              transparent
              wireframe
            />
          </mesh>
        );
      })}
    </>
  );
};

export const VirtualWorldScene: React.FC<VirtualWorldSceneProps> = ({
  config,
  children
}) => {
  const [webglError, setWebglError] = React.useState<string | null>(null);
  const [webglInitialized, setWebglInitialized] = React.useState(false);
  const [webglChecked, setWebglChecked] = React.useState(false);
  const cameraConfig = config?.camera || {
    position: [0, 15, 25] as [number, number, number],
    fov: 75
  };

  // Pre-check WebGL context creation before attempting to render
  React.useEffect(() => {
    const checkWebGL = async () => {
      try {
        // Test WebGL context creation
        const testCanvas = document.createElement('canvas');
        const contextAttributes: WebGLContextAttributes = {
          antialias: true,
          alpha: false,
          preserveDrawingBuffer: false,
          powerPreference: 'high-performance',
          failIfMajorPerformanceCaveat: false,
          stencil: false,
          depth: true,
          premultipliedAlpha: false,
          desynchronized: false
        };
        
        // Try WebGL2 first
        let gl = testCanvas.getContext('webgl2', contextAttributes) as WebGL2RenderingContext | null;
        
        // Fall back to WebGL1
        if (!gl) {
          gl = testCanvas.getContext('webgl', contextAttributes) as WebGLRenderingContext | null;
        }
        
        // Fall back to experimental-webgl
        if (!gl) {
          gl = testCanvas.getContext('experimental-webgl', contextAttributes) as WebGLRenderingContext | null;
        }
        
        if (!gl) {
          setWebglError('WebGL is not supported in this browser');
          setWebglChecked(true);
          setWebglInitialized(true);
          return;
        }
        
        // Test if we can actually create a Three.js renderer
        try {
          // Wrap in try-catch to catch any synchronous errors
          let testRenderer: THREE.WebGLRenderer | null = null;
          try {
            testRenderer = new THREE.WebGLRenderer({ 
              canvas: testCanvas,
              antialias: true,
              powerPreference: 'high-performance',
              failIfMajorPerformanceCaveat: false
            });
          } catch (constructorError) {
            // Catch constructor errors (like "error2 is not a function")
            const errorMessage = constructorError instanceof Error ? constructorError.message : String(constructorError);
            console.error('[VirtualWorldScene] WebGL renderer constructor failed:', errorMessage);
            
            // Check for specific Chrome GPU errors
            if (errorMessage.includes('BindToCurrentSequence') || errorMessage.includes('GPU process') || errorMessage.includes('error2 is not a function') || errorMessage.includes('WebGL context')) {
              setWebglError('Chrome GPU process error. Try restarting Chrome or using Chrome flags: --disable-gpu or --use-gl=swiftshader');
            } else {
              setWebglError(`WebGL renderer creation failed: ${errorMessage}`);
            }
            setWebglChecked(true);
            setWebglInitialized(true);
            return;
          }
          
          // Test if renderer actually works
          if (testRenderer) {
            try {
              testRenderer.setSize(100, 100);
              testRenderer.setClearColor(0x000000, 1);
              testRenderer.clear();
              testRenderer.dispose();
              setWebglError(null);
              console.log('[VirtualWorldScene] WebGL pre-check passed');
            } catch (renderError) {
              const errorMessage = renderError instanceof Error ? renderError.message : String(renderError);
              console.error('[VirtualWorldScene] WebGL renderer test failed:', errorMessage);
              setWebglError(`WebGL renderer test failed: ${errorMessage}`);
              setWebglChecked(true);
              setWebglInitialized(true);
              return;
            }
          }
        } catch (rendererError) {
          const errorMessage = rendererError instanceof Error ? rendererError.message : String(rendererError);
          console.error('[VirtualWorldScene] WebGL renderer creation failed:', errorMessage);
          
          // Check for specific Chrome GPU errors
          if (errorMessage.includes('BindToCurrentSequence') || errorMessage.includes('GPU process') || errorMessage.includes('error2 is not a function')) {
            setWebglError('Chrome GPU process error. Try restarting Chrome or using Chrome flags: --disable-gpu or --use-gl=swiftshader');
          } else {
            setWebglError(`WebGL renderer creation failed: ${errorMessage}`);
          }
          setWebglChecked(true);
          setWebglInitialized(true);
          return;
        }
        
        setWebglChecked(true);
      } catch (error) {
        console.error('[VirtualWorldScene] WebGL pre-check error:', error);
        const errorMessage = error instanceof Error ? error.message : String(error);
        
        // Check for specific Chrome GPU errors
        if (errorMessage.includes('BindToCurrentSequence') || errorMessage.includes('GPU process')) {
          setWebglError('Chrome GPU process error. Try restarting Chrome or using Chrome flags: --disable-gpu or --use-gl=swiftshader');
        } else {
          setWebglError(errorMessage || 'WebGL check failed');
        }
        
        setWebglChecked(true);
        setWebglInitialized(true);
      }
    };
    
    // Small delay to ensure DOM is ready
    const timeoutId = setTimeout(checkWebGL, 100);
    return () => clearTimeout(timeoutId);
  }, []);

  // Show loading state while checking WebGL
  if (!webglChecked) {
    return (
      <WorldLayoutProvider>
        <div style={{ width: '100%', height: '100%', position: 'relative', minHeight: '400px', display: 'flex', alignItems: 'center', justifyContent: 'center', backgroundColor: '#1a1a1a' }}>
          <div style={{ textAlign: 'center', color: '#ffffff' }}>
            <p>Checking WebGL support...</p>
          </div>
        </div>
      </WorldLayoutProvider>
    );
  }

  // Show fallback UI only if WebGL actually failed to initialize
  if (webglError && webglInitialized) {
    return (
      <WorldLayoutProvider>
        <div style={{ width: '100%', height: '100%', position: 'relative', minHeight: '400px', display: 'flex', alignItems: 'center', justifyContent: 'center', backgroundColor: '#1a1a1a' }}>
          <div style={{ textAlign: 'center', color: '#ffffff', padding: '2rem', maxWidth: '600px' }}>
            <h2 style={{ fontSize: '1.5rem', marginBottom: '1rem', color: '#ef4444' }}>3D World View Unavailable</h2>
            <p style={{ marginBottom: '0.5rem', color: '#94a3b8' }}>WebGL is required to render the 3D world with avatars.</p>
            <p style={{ marginBottom: '1rem', color: '#64748b', fontSize: '0.875rem' }}>Error: {webglError}</p>
            <div style={{ marginTop: '1.5rem', padding: '1rem', backgroundColor: '#1e293b', borderRadius: '0.5rem', textAlign: 'left' }}>
              <p style={{ marginBottom: '0.5rem', fontSize: '0.875rem', fontWeight: 'bold' }}>Possible solutions:</p>
              <ul style={{ marginLeft: '1.5rem', fontSize: '0.875rem', color: '#cbd5e1' }}>
                <li>Restart Chrome/Chromium (the GPU process may need to restart)</li>
                <li>Try Chrome flags: <code style={{ fontSize: '0.75rem', backgroundColor: '#1e293b', padding: '0.25rem 0.5rem', borderRadius: '0.25rem' }}>--disable-gpu</code> or <code style={{ fontSize: '0.75rem', backgroundColor: '#1e293b', padding: '0.25rem 0.5rem', borderRadius: '0.25rem' }}>--use-gl=swiftshader</code></li>
                <li>Update your graphics drivers</li>
                <li>Enable hardware acceleration in your browser settings</li>
                <li>Try a different browser (Firefox, Edge)</li>
                <li>Check if WebGL is enabled: <a href="https://get.webgl.org/" target="_blank" rel="noopener noreferrer" style={{ color: '#3b82f6' }}>get.webgl.org</a></li>
              </ul>
            </div>
          </div>
        </div>
      </WorldLayoutProvider>
    );
  }

  return (
    <WorldLayoutProvider>
      <ProvenanceCanvasErrorBoundary
        onRetry={() => {
          // Reset error state to allow retry
          setWebglError(null);
          setWebglInitialized(false);
        }}
        fallback={
          <div style={{ width: '100%', height: '100%', position: 'relative', minHeight: '400px', display: 'flex', alignItems: 'center', justifyContent: 'center', backgroundColor: '#1a1a1a' }}>
            <div style={{ textAlign: 'center', color: '#ffffff', padding: '2rem', maxWidth: '600px' }}>
              <h2 style={{ fontSize: '1.5rem', marginBottom: '1rem', color: '#ef4444' }}>3D World View Error</h2>
              <p style={{ marginBottom: '0.5rem', color: '#94a3b8' }}>An error occurred while initializing the 3D world.</p>
              <p style={{ marginBottom: '1rem', color: '#64748b', fontSize: '0.875rem' }}>
                This may be due to a WebGL context creation failure. Please check the browser console for details.
              </p>
            </div>
          </div>
        }
      >
        <div style={{ width: '100%', height: '100%', position: 'relative', minHeight: '400px' }}>
          <Suspense fallback={
            <div style={{ width: '100%', height: '100%', display: 'flex', alignItems: 'center', justifyContent: 'center', backgroundColor: '#1a1a1a', color: '#ffffff' }}>
              <div style={{ textAlign: 'center' }}>
                <p>Loading 3D world...</p>
              </div>
            </div>
          }>
            <Canvas
              camera={{
                position: cameraConfig.position,
                fov: cameraConfig.fov
              }}
              gl={{
                antialias: true,
                alpha: false,
                preserveDrawingBuffer: false,
                powerPreference: 'high-performance',
                failIfMajorPerformanceCaveat: false,
                stencil: false,
                depth: true,
                premultipliedAlpha: false,
                desynchronized: false
              }}
              dpr={[1, 2]}
              frameloop="always"
              onCreated={({ gl, scene, camera }) => {
                try {
                  console.log('[VirtualWorldScene] Canvas created successfully');
                  
                  // Set scene background color (important for visibility)
                  scene.background = new THREE.Color(0x1a1a2e);
                  
                  // Configure renderer
                  gl.shadowMap.enabled = true;
                  gl.shadowMap.type = THREE.PCFSoftShadowMap;
                  gl.setClearColor(0x1a1a2e, 1); // Dark blue background
                  
                  // Log camera and scene info
                  console.log('[VirtualWorldScene] Camera position:', camera.position);
                  console.log('[VirtualWorldScene] Scene children count:', scene.children.length);
                  
                  // Mark WebGL as successfully initialized
                  setWebglInitialized(true);
                  setWebglError(null);
                  
                  const webglContext = gl.getContext() as WebGLRenderingContext | WebGL2RenderingContext;
                  if (webglContext) {
                    console.log('[VirtualWorldScene] WebGL renderer initialized:', {
                      vendor: webglContext.getParameter(webglContext.VENDOR),
                      renderer: webglContext.getParameter(webglContext.RENDERER),
                      version: webglContext.getParameter(webglContext.VERSION),
                      webglVersion: webglContext instanceof WebGL2RenderingContext ? 'WebGL2' : 'WebGL1'
                    });
                  }
                } catch (error) {
                  console.error('[VirtualWorldScene] Failed to configure WebGL renderer:', error);
                  const errorMessage = error instanceof Error ? error.message : String(error);
                  
                  // Check for specific Chrome GPU errors
                  if (errorMessage.includes('BindToCurrentSequence') || errorMessage.includes('GPU process')) {
                    setWebglError('Chrome GPU process error. Try restarting Chrome or disabling hardware acceleration.');
                  } else {
                    setWebglError(errorMessage || 'WebGL initialization failed');
                  }
                  
                  setWebglInitialized(true);
                }
              }}
              style={{ width: '100%', height: '100%', display: 'block' }}
            >
              <SceneContent config={config}>
                {children}
              </SceneContent>
            </Canvas>
          </Suspense>
        </div>
      </ProvenanceCanvasErrorBoundary>
    </WorldLayoutProvider>
  );
};

// Export WorldLayoutProvider for components that need context outside Canvas
export { WorldLayoutProvider } from './WorldLayoutManager';
