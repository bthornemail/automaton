/**
 * Unified Provenance Canvas Component
 * 
 * Combines MetaverseCanvas3D and DimensionalCanvas with offscreen canvas integration
 * for visualizing provenance chains of automaton evolutions.
 */

import React, { useState, useEffect, useRef } from 'react';
import { Canvas, useFrame, useThree } from '@react-three/fiber';
import { OrbitControls, Text, Html } from '@react-three/drei';
import * as THREE from 'three';
import * as d3 from 'd3';
import { ProvenanceSlideService, Slide, ProvenanceChain, ProvenanceNode } from '../../services/provenance-slide-service';
import { ProvenanceCanvasWorkerService } from '../../services/provenance-canvas-worker-service';
import { canvasl3DService, Canvas3D } from '../../services/canvasl-3d-service';
import { motion, AnimatePresence } from 'framer-motion';
import { Card } from '../shared/Card';
import { Button } from '../shared/Button';
import { ChevronLeft, ChevronRight, ZoomIn, ZoomOut, RotateCcw, Info } from 'lucide-react';
import { WorkerErrorBoundary } from '../shared/WorkerErrorBoundary';
import { errorLoggingService } from '../../services/error-logging-service';
import { useDebounce } from '../../hooks/useDebounce';
import { performanceMonitoringService } from '../../services/performance-monitoring-service';
import { VirtualizedCardList } from '../shared/VirtualizedCardList';
import { ProvenanceAvatar } from './ProvenanceAvatar';
import { SVGTextureRenderer, ProceduralUIGenerator } from './SVGTextureRenderer';
import { ComputationalManifoldRenderer } from './ComputationalManifoldRenderer';
import { ThoughtCard3D } from './ThoughtCard3D';
import { KnowledgeGraphCard2D } from './KnowledgeGraphCard2D';
import { thoughtCardService } from '../../services/thought-card-service';
import { knowledgeGraphCardService } from '../../services/knowledge-graph-card-service';

interface UnifiedProvenanceCanvasProps {
  evolutionPath?: string;
  slides?: Slide[];
  onSlideChange?: (slide: Slide) => void;
  readOnly?: boolean;
}

export const UnifiedProvenanceCanvas: React.FC<UnifiedProvenanceCanvasProps> = ({
  evolutionPath,
  slides: initialSlides,
  onSlideChange,
  readOnly = false
}) => {
  const [slides, setSlides] = useState<Slide[]>(initialSlides || []);
  const [currentSlideIndex, setCurrentSlideIndex] = useState(0);
  const [selectedNode, setSelectedNode] = useState<ProvenanceNode | null>(null);
  const [hoveredNode, setHoveredNode] = useState<ProvenanceNode | null>(null);
  const [currentDimension, setCurrentDimension] = useState('0D');
  const [viewMode, setViewMode] = useState<'3d' | '2d' | 'combined'>('combined');
  const [loading, setLoading] = useState(false);
  const [workerFallbackMode, setWorkerFallbackMode] = useState<'normal' | '2d-only'>('normal');
  const [showComputationalManifold, setShowComputationalManifold] = useState(false);
  const [showThoughtCards, setShowThoughtCards] = useState(true);
  const [showKnowledgeGraphCards, setShowKnowledgeGraphCards] = useState(true);
  const [selectedKnowledgeGraphCard, setSelectedKnowledgeGraphCard] = useState<string | null>(null);
  
  // Debounce dimension changes for performance
  const debouncedDimension = useDebounce(currentDimension, 300);
  
  // Effect to handle debounced dimension changes
  useEffect(() => {
    if (debouncedDimension !== currentDimension && slides.length > 0) {
      // Filter slides by debounced dimension
      const dimensionSlide = slides.find(s => s.dimension === debouncedDimension);
      if (dimensionSlide) {
        const slideIndex = slides.indexOf(dimensionSlide);
        setCurrentSlideIndex(slideIndex);
        setCurrentDimension(debouncedDimension);
      }
    }
  }, [debouncedDimension, slides, currentDimension]);
  
  const provenanceService = useRef(new ProvenanceSlideService());
  const workerService = useRef<ProvenanceCanvasWorkerService | null>(null);
  const offscreenCanvasRef = useRef<HTMLCanvasElement>(null);
  const svgRef = useRef<SVGSVGElement>(null);
  const containerRef = useRef<HTMLDivElement>(null);

  // Start performance monitoring
  useEffect(() => {
    performanceMonitoringService.startMonitoring();
    return () => {
      performanceMonitoringService.stopMonitoring();
    };
  }, []);

  // Initialize services
  useEffect(() => {
    const init = async () => {
      setLoading(true);
      try {
        await provenanceService.current.init();
        
        // Load slides from evolution if path provided
        if (evolutionPath && slides.length === 0) {
          const generatedSlides = await provenanceService.current.generateSlidesFromEvolution(evolutionPath);
          setSlides(generatedSlides);
        }
        
        // Initialize offscreen canvas worker with error handling
        if (offscreenCanvasRef.current) {
          // Check for OffscreenCanvas support
          if (!ProvenanceCanvasWorkerService.isSupported()) {
            console.warn('OffscreenCanvas not supported, falling back to main thread rendering');
            // Fallback: continue without worker (could render on main thread)
            setLoading(false);
            return;
          }

          try {
            const offscreenCanvas = offscreenCanvasRef.current.transferControlToOffscreen();
            workerService.current = new ProvenanceCanvasWorkerService();
            
            // Set up error handler
            workerService.current.onMessage('error', (error: any) => {
              console.error('Worker error:', error);
              errorLoggingService.logError(
                new Error(error.message || 'Unknown worker error'),
                {
                  component: 'UnifiedProvenanceCanvas',
                  service: 'ProvenanceCanvasWorkerService',
                  action: 'worker-error',
                  severity: 'error'
                }
              );
            });
            
            // Set up recovery handler
            workerService.current.onMessage('recovered', (data: any) => {
              console.log('Worker recovered:', data);
              setWorkerFallbackMode('normal');
            });
            
            await workerService.current.init(offscreenCanvas, {
              width: offscreenCanvasRef.current.width,
              height: offscreenCanvasRef.current.height,
              antialias: true
            });
            
            // Check fallback mode
            const fallbackMode = workerService.current.getFallbackMode();
            setWorkerFallbackMode(fallbackMode);
            
            // Load current slide's provenance chain
            if (slides.length > 0 && fallbackMode === 'normal') {
              const currentSlide = slides[currentSlideIndex];
              if (currentSlide.provenanceChain) {
                // Track message latency
                const messageId = `load-${Date.now()}`;
                performanceMonitoringService.trackMessageStart('load', messageId);
                
                workerService.current.loadProvenanceChain(currentSlide.provenanceChain);
                
                // Track latency end (will be called when worker responds)
                setTimeout(() => {
                  performanceMonitoringService.trackMessageEnd(messageId);
                }, 100);
                
                // Update node/edge counts for monitoring
                performanceMonitoringService.updateNodeEdgeCounts(
                  currentSlide.provenanceChain.nodes.length,
                  currentSlide.provenanceChain.edges.length
                );
              }
            }
          } catch (workerError) {
            console.error('Failed to initialize worker:', workerError);
            
            // Log error
            const errorObj = workerError instanceof Error ? workerError : new Error(String(workerError));
            errorLoggingService.logError(errorObj, {
              component: 'UnifiedProvenanceCanvas',
              service: 'ProvenanceCanvasWorkerService',
              action: 'init',
              severity: 'error'
            });
            
            // Set fallback mode to 2D only
            setWorkerFallbackMode('2d-only');
          }
        }
      } catch (error) {
        console.error('Failed to initialize UnifiedProvenanceCanvas:', error);
      } finally {
        setLoading(false);
      }
    };
    
    init();
    
    return () => {
      if (workerService.current) {
        workerService.current.dispose();
      }
    };
  }, [evolutionPath]);

  // Update worker when slide changes
  useEffect(() => {
    if (workerService.current && slides.length > 0) {
      const currentSlide = slides[currentSlideIndex];
      if (currentSlide.provenanceChain) {
        workerService.current.loadProvenanceChain(currentSlide.provenanceChain);
        setCurrentDimension(currentSlide.dimension || '0D');
      }
    }
  }, [currentSlideIndex, slides]);

  // Handle canvas resize
  useEffect(() => {
    const handleResize = () => {
      if (offscreenCanvasRef.current && workerService.current) {
        const width = offscreenCanvasRef.current.width;
        const height = offscreenCanvasRef.current.height;
        workerService.current.resize(width, height);
      }
    };
    
    window.addEventListener('resize', handleResize);
    return () => window.removeEventListener('resize', handleResize);
  }, []);

  // Handle slide navigation
  const goToNextSlide = () => {
    if (currentSlideIndex < slides.length - 1) {
      const newIndex = currentSlideIndex + 1;
      setCurrentSlideIndex(newIndex);
      const slide = slides[newIndex];
      onSlideChange?.(slide);
    }
  };

  const goToPrevSlide = () => {
    if (currentSlideIndex > 0) {
      const newIndex = currentSlideIndex - 1;
      setCurrentSlideIndex(newIndex);
      const slide = slides[newIndex];
      onSlideChange?.(slide);
    }
  };

  const goToSlide = (index: number) => {
    if (index >= 0 && index < slides.length) {
      setCurrentSlideIndex(index);
      const slide = slides[index];
      onSlideChange?.(slide);
    }
  };

  // Handle canvas click
  const handleCanvasClick = async (event: React.MouseEvent<HTMLCanvasElement>) => {
    if (!workerService.current || !offscreenCanvasRef.current) return;
    
    const rect = offscreenCanvasRef.current.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    
    const node = await workerService.current.handleInteraction(
      x,
      y,
      rect.width,
      rect.height,
      'click'
    );
    
    if (node) {
      setSelectedNode(node);
    }
  };

  // Handle canvas hover
  const handleCanvasHover = async (event: React.MouseEvent<HTMLCanvasElement>) => {
    if (!workerService.current || !offscreenCanvasRef.current) return;
    
    const rect = offscreenCanvasRef.current.getBoundingClientRect();
    const x = event.clientX - rect.left;
    const y = event.clientY - rect.top;
    
    const node = await workerService.current.handleInteraction(
      x,
      y,
      rect.width,
      rect.height,
      'hover'
    );
    
    setHoveredNode(node);
  };

  const currentSlide = slides[currentSlideIndex];

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-500 mx-auto mb-4"></div>
          <p className="text-gray-400">Loading provenance visualization...</p>
        </div>
      </div>
    );
  }

  if (slides.length === 0) {
    return (
      <div className="flex items-center justify-center h-full">
        <div className="text-center">
          <p className="text-gray-400 mb-4">No slides available</p>
          {evolutionPath && (
            <Button onClick={() => provenanceService.current.generateSlidesFromEvolution(evolutionPath)}>
              Generate Slides
            </Button>
          )}
        </div>
      </div>
    );
  }

  return (
    <div ref={containerRef} className="h-full flex flex-col bg-gray-900">
      {/* Toolbar */}
      <div className="flex items-center justify-between p-4 border-b border-gray-700">
        <div className="flex items-center gap-4">
          <Button
            onClick={goToPrevSlide}
            disabled={currentSlideIndex === 0}
            variant="outline"
            size="sm"
          >
            <ChevronLeft className="w-4 h-4" />
          </Button>
          
          <span className="text-sm text-gray-400">
            Slide {currentSlideIndex + 1} of {slides.length}
          </span>
          
          <Button
            onClick={goToNextSlide}
            disabled={currentSlideIndex === slides.length - 1}
            variant="outline"
            size="sm"
          >
            <ChevronRight className="w-4 h-4" />
          </Button>
        </div>
        
        <div className="flex items-center gap-2">
          <select
            value={viewMode}
            onChange={(e) => setViewMode(e.target.value as '3d' | '2d' | 'combined')}
            className="bg-gray-800 text-white px-3 py-1 rounded border border-gray-700"
          >
            <option value="3d">3D View</option>
            <option value="2d">2D View</option>
            <option value="combined">Combined</option>
          </select>
          
          <label className="flex items-center gap-2 text-sm text-gray-400">
            <input
              type="checkbox"
              checked={showComputationalManifold}
              onChange={(e) => setShowComputationalManifold(e.target.checked)}
              className="rounded"
            />
            Computational Manifold
          </label>
          
          <label className="flex items-center gap-2 text-sm text-gray-400">
            <input
              type="checkbox"
              checked={showThoughtCards}
              onChange={(e) => setShowThoughtCards(e.target.checked)}
              className="rounded"
            />
            Thought Cards
          </label>
          
          <label className="flex items-center gap-2 text-sm text-gray-400">
            <input
              type="checkbox"
              checked={showKnowledgeGraphCards}
              onChange={(e) => setShowKnowledgeGraphCards(e.target.checked)}
              className="rounded"
            />
            Knowledge Graphs
          </label>
        </div>
      </div>

      {/* Main Canvas Area */}
      <div className="flex-1 flex relative">
        <WorkerErrorBoundary
          onRetry={() => {
            // Retry worker initialization
            if (offscreenCanvasRef.current && !ProvenanceCanvasWorkerService.isSupported()) {
              setWorkerFallbackMode('2d-only');
            } else if (offscreenCanvasRef.current) {
              const offscreenCanvas = offscreenCanvasRef.current.transferControlToOffscreen();
              workerService.current = new ProvenanceCanvasWorkerService();
              workerService.current.init(offscreenCanvas, {
                width: offscreenCanvasRef.current.width,
                height: offscreenCanvasRef.current.height,
                antialias: true
              }).then(() => {
                setWorkerFallbackMode('normal');
              }).catch(() => {
                setWorkerFallbackMode('2d-only');
              });
            }
          }}
        >
          {/* 3D Provenance Canvas (Offscreen) - Only show if not in fallback mode */}
          {(viewMode === '3d' || viewMode === 'combined') && workerFallbackMode === 'normal' && (
            <div className={viewMode === 'combined' ? 'w-1/2 border-r border-gray-700 relative' : 'w-full relative'}>
              <canvas
                ref={offscreenCanvasRef}
                className="w-full h-full"
                width={800}
                height={600}
                onClick={handleCanvasClick}
                onMouseMove={handleCanvasHover}
              />
              {/* Avatar Overlay Canvas (React Three Fiber) */}
              {currentSlide?.provenanceChain && (
                <div className="absolute inset-0 pointer-events-auto">
                  <Canvas
                    camera={{ position: [10, 10, 10], fov: 60 }}
                    gl={{ antialias: true, alpha: true }}
                    style={{ width: '100%', height: '100%' }}
                    onCreated={({ gl }) => {
                      gl.setClearColor('#000000', 0); // Transparent background
                    }}
                  >
                    <ambientLight intensity={0.5} />
                    <pointLight position={[10, 10, 10]} intensity={1.0} />
                    <directionalLight position={[0, 10, 5]} intensity={0.5} />
                    
                    {/* Render avatars for nodes with avatar config */}
                    {currentSlide.provenanceChain.nodes
                      .filter(node => node.avatar)
                      .map(node => {
                        // Create thought card for avatar if enabled
                        const thoughtCard = showThoughtCards 
                          ? thoughtCardService.createThoughtCardFromNode(node)
                          : null;
                        
                        return (
                          <React.Fragment key={node.id}>
                            <ProvenanceAvatar
                              node={node}
                              isSelected={selectedNode?.id === node.id}
                              isHovered={hoveredNode?.id === node.id}
                              onClick={() => setSelectedNode(node)}
                              onHover={(hovered) => setHoveredNode(hovered ? node : null)}
                            />
                            {/* Render 3D thought card */}
                            {thoughtCard && (
                              <ThoughtCard3D
                                card={thoughtCard}
                                avatarPosition={node.position}
                                isVisible={showThoughtCards}
                              />
                            )}
                          </React.Fragment>
                        );
                      })}
                    
                    {/* SVG Texture Overlay for Topology Diagram */}
                    {currentSlide.provenanceChain && (
                      <ProceduralUIGenerator
                        nodes={currentSlide.provenanceChain.nodes.map(node => ({
                          id: node.id,
                          x: node.position[0] * 50 + 400, // Scale and center
                          y: node.position[1] * 50 + 300,
                          label: node.metadata.agentId || node.id
                        }))}
                        edges={currentSlide.provenanceChain.edges}
                        position={[0, 0, -5]}
                        scale={[0.1, 0.1, 1]}
                      />
                    )}
                    
                    {/* Computational Manifold Visualization */}
                    {showComputationalManifold && currentSlide.provenanceChain && (
                      <ComputationalManifoldRenderer
                        nodes={currentSlide.provenanceChain.nodes}
                        showCombinators={true}
                        showEvaluationTraces={true}
                      />
                    )}
                    
                    <OrbitControls enableDamping dampingFactor={0.05} />
                  </Canvas>
                </div>
              )}
            </div>
          )}

          {/* 2D Dimensional Canvas - Always show, or show only if in fallback mode */}
          {(viewMode === '2d' || viewMode === 'combined' || workerFallbackMode === '2d-only') && (
            <div className={
              (viewMode === 'combined' && workerFallbackMode === 'normal') 
                ? 'w-1/2' 
                : 'w-full'
            }>
              <DimensionalView
                svgRef={svgRef}
                currentDimension={currentDimension}
                dimensions={['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D']}
              />
            </div>
          )}
        </WorkerErrorBoundary>
      </div>

      {/* Slide Info Panel */}
      {currentSlide && (
        <div className="p-4 border-t border-gray-700 bg-gray-800">
          <div className="flex items-start justify-between">
            <div>
              <h3 className="text-lg font-semibold text-white mb-2">
                {currentSlide.title || `Slide ${currentSlideIndex + 1}`}
              </h3>
              {currentSlide.description && (
                <p className="text-sm text-gray-400 mb-2">{currentSlide.description}</p>
              )}
              {currentSlide.dimension && (
                <span className="inline-block px-2 py-1 bg-blue-900 text-blue-200 rounded text-xs">
                  {currentSlide.dimension}
                </span>
              )}
            </div>
            
            {currentSlide.provenanceChain && (
              <div className="text-right">
                <p className="text-sm text-gray-400">
                  Nodes: {currentSlide.provenanceChain.nodes.length}
                </p>
                <p className="text-sm text-gray-400">
                  Edges: {currentSlide.provenanceChain.edges.length}
                </p>
              </div>
            )}
          </div>

          {/* Knowledge Graph Cards - 2D CanvasL Cards */}
          {showKnowledgeGraphCards && currentSlide && (() => {
            const knowledgeGraphCards = knowledgeGraphCardService.buildKnowledgeGraphsForSlide(currentSlide);
            return knowledgeGraphCards.length > 0 && (
              <div className="mt-4">
                <h4 className="text-sm font-semibold text-gray-300 mb-2">
                  Agent Thought Processes ({knowledgeGraphCards.length})
                </h4>
                <div className="space-y-2 max-h-96 overflow-y-auto">
                  {knowledgeGraphCards.map((kgCard) => (
                    <KnowledgeGraphCard2D
                      key={kgCard.id}
                      card={kgCard}
                      onClose={() => setSelectedKnowledgeGraphCard(null)}
                    />
                  ))}
                </div>
              </div>
            );
          })()}

          {/* Pattern Cards - Use virtual scrolling for large lists */}
          {currentSlide.cards && currentSlide.cards.length > 0 && (
            <div className="mt-4">
              <h4 className="text-sm font-semibold text-gray-300 mb-2">
                Pattern Cards ({currentSlide.cards.length})
              </h4>
              {currentSlide.cards.length > 50 ? (
                <VirtualizedCardList
                  cards={currentSlide.cards}
                  itemHeight={80}
                  overscan={5}
                  onCardSelect={(card) => {
                    // Handle card selection
                    console.log('Card selected:', card);
                  }}
                  className="h-64"
                />
              ) : (
                <div className="grid grid-cols-3 gap-2">
                  {currentSlide.cards.map((card) => (
                    <Card key={card.id} className="p-2 bg-gray-700">
                      <div className="text-xs text-gray-300">
                        <div className="font-semibold">{card.pattern}</div>
                        <div className="text-gray-500">{card.jsonlLines.length} lines</div>
                      </div>
                    </Card>
                  ))}
                </div>
              )}
            </div>
          )}
        </div>
      )}

      {/* Node Details Modal */}
      <AnimatePresence>
        {selectedNode && (
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: 20 }}
            className="fixed bottom-4 right-4 w-80 bg-gray-800 border border-gray-700 rounded-lg p-4 shadow-xl z-50"
          >
            <h4 className="text-lg font-semibold text-white mb-2">{selectedNode.id}</h4>
            <div className="text-sm text-gray-400 space-y-1">
              <p><strong>Type:</strong> {selectedNode.type}</p>
              <p><strong>Dimension:</strong> {selectedNode.metadata.dimension || 'N/A'}</p>
              <p><strong>Pattern:</strong> {selectedNode.metadata.pattern || 'N/A'}</p>
              <p><strong>File:</strong> {selectedNode.metadata.file}</p>
              <p><strong>Line:</strong> {selectedNode.metadata.line}</p>
            </div>
            <Button
              onClick={() => setSelectedNode(null)}
              variant="outline"
              size="sm"
              className="mt-4 w-full"
            >
              Close
            </Button>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  );
};

// Dimensional View Component (2D D3 visualization)
const DimensionalView: React.FC<{
  svgRef: React.RefObject<SVGSVGElement>;
  currentDimension: string;
  dimensions: string[];
}> = ({ svgRef, currentDimension, dimensions }) => {
  useEffect(() => {
    if (!svgRef.current) return;

    const svg = d3.select(svgRef.current);
    svg.selectAll('*').remove();

    const width = 800;
    const height = 400;
    svg.attr('width', width).attr('height', height);

    const container = svg.append('g');

    // Create nodes for each dimension
    const nodes = dimensions.map((dim, i) => ({
      id: dim,
      x: 100 + (i % 4) * 150,
      y: 100 + Math.floor(i / 4) * 150,
      isCurrent: dim === currentDimension
    }));

    // Draw links
    const links = nodes.slice(0, -1).map((node, i) => ({
      source: node.id,
      target: nodes[i + 1].id
    }));

    container.selectAll('line')
      .data(links)
      .enter()
      .append('line')
      .attr('x1', (d: any) => nodes.find(n => n.id === d.source)!.x)
      .attr('y1', (d: any) => nodes.find(n => n.id === d.source)!.y)
      .attr('x2', (d: any) => nodes.find(n => n.id === d.target)!.x)
      .attr('y2', (d: any) => nodes.find(n => n.id === d.target)!.y)
      .attr('stroke', '#4b5563')
      .attr('stroke-width', 2);

    // Draw nodes
    container.selectAll('circle')
      .data(nodes)
      .enter()
      .append('circle')
      .attr('cx', d => d.x)
      .attr('cy', d => d.y)
      .attr('r', d => d.isCurrent ? 20 : 15)
      .attr('fill', d => d.isCurrent ? '#6366f1' : '#6b7280')
      .attr('stroke', d => d.isCurrent ? '#818cf8' : '#9ca3af')
      .attr('stroke-width', d => d.isCurrent ? 3 : 1);

    // Draw labels
    container.selectAll('text')
      .data(nodes)
      .enter()
      .append('text')
      .attr('x', d => d.x)
      .attr('y', d => d.y + 35)
      .attr('text-anchor', 'middle')
      .attr('fill', '#e5e7eb')
      .attr('font-size', '14px')
      .text(d => d.id);
  }, [currentDimension, dimensions]);

  return (
    <div className="w-full h-full bg-gray-900 p-4">
      <svg
        ref={svgRef}
        className="w-full h-full border border-gray-700 rounded-lg"
        viewBox="0 0 800 400"
      />
    </div>
  );
};

export default UnifiedProvenanceCanvas;

