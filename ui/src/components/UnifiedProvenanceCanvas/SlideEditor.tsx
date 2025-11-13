/**
 * Slide Editor Component
 * 
 * Inline editor for editing slide content (title, description, content).
 */

import React, { useState, useEffect } from 'react';
import { Edit2, Save, X } from 'lucide-react';
import { Button } from '../shared/Button';
import { Card } from '../shared/Card';
import { Slide } from '../../services/provenance-slide-service';
import { slideEditingService } from '../../services/slide-editing-service';

interface SlideEditorProps {
  slide: Slide;
  onSave: (updatedSlide: Slide) => void;
  onCancel: () => void;
}

export const SlideEditor: React.FC<SlideEditorProps> = ({
  slide,
  onSave,
  onCancel
}) => {
  const [title, setTitle] = useState(slide.title || '');
  const [description, setDescription] = useState(slide.description || '');
  const [content, setContent] = useState(slide.content || '');

  useEffect(() => {
    setTitle(slide.title || '');
    setDescription(slide.description || '');
    setContent(slide.content || '');
  }, [slide]);

  const handleSave = () => {
    const updatedSlide = slideEditingService.editSlide(slide.id, {
      title,
      description,
      content
    });
    onSave(updatedSlide);
  };

  return (
    <Card className="p-4 bg-gray-800 border border-gray-700">
      <div className="space-y-4">
        <div>
          <label className="block text-sm font-medium text-gray-300 mb-1">
            Title
          </label>
          <input
            type="text"
            value={title}
            onChange={(e) => setTitle(e.target.value)}
            className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="Slide title"
          />
        </div>

        <div>
          <label className="block text-sm font-medium text-gray-300 mb-1">
            Description
          </label>
          <textarea
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="Slide description"
            rows={3}
          />
        </div>

        <div>
          <label className="block text-sm font-medium text-gray-300 mb-1">
            Content
          </label>
          <textarea
            value={content}
            onChange={(e) => setContent(e.target.value)}
            className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500 font-mono text-sm"
            placeholder="Slide content (markdown)"
            rows={10}
          />
        </div>

        <div className="flex items-center justify-end gap-2">
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
            Save
          </Button>
        </div>
      </div>
    </Card>
  );
};

