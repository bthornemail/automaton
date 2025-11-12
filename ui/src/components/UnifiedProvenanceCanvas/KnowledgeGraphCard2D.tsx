/**
 * 2D Knowledge Graph Card Component
 * 
 * Renders 2D CanvasL knowledge graph cards for agent thought processes.
 */

import React, { useEffect, useRef } from 'react';
import { KnowledgeGraphCard } from '../../services/knowledge-graph-card-service';
import { knowledgeGraphCardService } from '../../services/knowledge-graph-card-service';
import { Card } from '../shared/Card';

interface KnowledgeGraphCard2DProps {
  card: KnowledgeGraphCard;
  onClose?: () => void;
}

export const KnowledgeGraphCard2D: React.FC<KnowledgeGraphCard2DProps> = ({
  card,
  onClose
}) => {
  const svgRef = useRef<SVGSVGElement>(null);

  useEffect(() => {
    if (!svgRef.current) return;

    // Render knowledge graph as SVG
    const svg = knowledgeGraphCardService.renderKnowledgeGraph(card);
    
    // Clear existing content
    svgRef.current.innerHTML = '';
    
    // Copy SVG content
    while (svg.firstChild) {
      svgRef.current.appendChild(svg.firstChild);
    }
  }, [card]);

  return (
    <Card className="p-4 bg-gray-800 border border-gray-700">
      <div className="flex items-start justify-between mb-2">
        <div>
          <h4 className="text-sm font-semibold text-white mb-1">
            {card.metadata.thoughtProcess}
          </h4>
          <div className="flex items-center gap-2 text-xs text-gray-400">
            {card.metadata.dimension && (
              <span className="px-2 py-1 bg-blue-900 text-blue-200 rounded">
                {card.metadata.dimension}
              </span>
            )}
            {card.metadata.pattern && (
              <span className="px-2 py-1 bg-green-900 text-green-200 rounded">
                {card.metadata.pattern}
              </span>
            )}
            <span className="text-gray-500">
              Agent: {card.agentId}
            </span>
          </div>
        </div>
        {onClose && (
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-white"
          >
            ×
          </button>
        )}
      </div>
      
      <div className="mt-4 border border-gray-700 rounded overflow-hidden">
        <svg
          ref={svgRef}
          className="w-full h-64"
          viewBox="0 0 800 600"
          preserveAspectRatio="xMidYMid meet"
        />
      </div>
      
      <div className="mt-2 text-xs text-gray-500">
        {card.nodes.length} nodes, {card.edges.length} edges
      </div>
    </Card>
  );
};

