/**
 * Slide Reorderer Component
 * 
 * UI for reordering slides using drag and drop.
 */

import React, { useState } from 'react';
import { GripVertical, Save, X } from 'lucide-react';
import { Button } from '../shared/Button';
import { Card } from '../shared/Card';
import { Slide } from '../../services/provenance-slide-service';
import { slideEditingService } from '../../services/slide-editing-service';

interface SlideReordererProps {
  slides: Slide[];
  onSave: (reorderedSlides: Slide[]) => void;
  onCancel: () => void;
}

export const SlideReorderer: React.FC<SlideReordererProps> = ({
  slides,
  onSave,
  onCancel
}) => {
  const [reorderedSlides, setReorderedSlides] = useState<Slide[]>([...slides]);

  const moveSlide = (fromIndex: number, toIndex: number) => {
    const newSlides = [...reorderedSlides];
    const [removed] = newSlides.splice(fromIndex, 1);
    newSlides.splice(toIndex, 0, removed);
    setReorderedSlides(newSlides);
  };

  const moveUp = (index: number) => {
    if (index > 0) {
      moveSlide(index, index - 1);
    }
  };

  const moveDown = (index: number) => {
    if (index < reorderedSlides.length - 1) {
      moveSlide(index, index + 1);
    }
  };

  const handleSave = () => {
    const saved = slideEditingService.reorderSlides(reorderedSlides);
    onSave(saved);
  };

  return (
    <Card className="p-4 bg-gray-800 border border-gray-700">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-lg font-semibold text-white">Reorder Slides</h3>
        <button
          onClick={onCancel}
          className="text-gray-400 hover:text-white transition-colors"
        >
          <X className="w-5 h-5" />
        </button>
      </div>

      <div className="space-y-2 max-h-96 overflow-y-auto">
        {reorderedSlides.map((slide, index) => (
          <div
            key={slide.id}
            className="flex items-center gap-2 p-3 bg-gray-700 rounded"
          >
            <GripVertical className="w-5 h-5 text-gray-400" />
            <div className="flex-1">
              <div className="text-sm font-medium text-white">
                {slide.title || `Slide ${index + 1}`}
              </div>
              <div className="text-xs text-gray-400">
                {slide.dimension || 'No dimension'} • {slide.cards?.length || 0} cards
              </div>
            </div>
            <div className="flex gap-1">
              <Button
                onClick={() => moveUp(index)}
                variant="outline"
                size="sm"
                disabled={index === 0}
                className="text-xs"
              >
                ↑
              </Button>
              <Button
                onClick={() => moveDown(index)}
                variant="outline"
                size="sm"
                disabled={index === reorderedSlides.length - 1}
                className="text-xs"
              >
                ↓
              </Button>
            </div>
          </div>
        ))}
      </div>

      <div className="flex items-center justify-end gap-2 mt-4">
        <Button
          onClick={onCancel}
          variant="outline"
          size="sm"
        >
          <X className="w-4 h-4 mr-1" />
          Cancel
        </Button>
        <Button
          onClick={handleSave}
          variant="primary"
          size="sm"
        >
          <Save className="w-4 h-4 mr-1" />
          Save Order
        </Button>
      </div>
    </Card>
  );
};

