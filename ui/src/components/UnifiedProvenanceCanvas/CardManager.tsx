/**
 * Card Manager Component
 * 
 * UI for adding and removing cards from slides.
 */

import React, { useState } from 'react';
import { Plus, Trash2, X } from 'lucide-react';
import { Button } from '../shared/Button';
import { Card } from '../shared/Card';
import { Slide, Card as SlideCard } from '../../services/provenance-slide-service';
import { slideEditingService } from '../../services/slide-editing-service';

interface CardManagerProps {
  slide: Slide;
  onUpdate: (updatedSlide: Slide) => void;
  onClose: () => void;
}

export const CardManager: React.FC<CardManagerProps> = ({
  slide,
  onUpdate,
  onClose
}) => {
  const [newCardPattern, setNewCardPattern] = useState('');

  const handleAddCard = () => {
    if (!newCardPattern.trim()) {
      return;
    }

    const newCard: SlideCard = {
      id: `card-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
      pattern: newCardPattern.trim(),
      jsonlLines: [],
      metadata: {}
    };

    const updatedSlide = slideEditingService.addCardToSlide(slide.id, newCard);
    onUpdate(updatedSlide);
    setNewCardPattern('');
  };

  const handleRemoveCard = (cardId: string) => {
    const updatedSlide = slideEditingService.removeCardFromSlide(slide.id, cardId);
    onUpdate(updatedSlide);
  };

  return (
    <Card className="p-4 bg-gray-800 border border-gray-700">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-lg font-semibold text-white">Manage Cards</h3>
        <button
          onClick={onClose}
          className="text-gray-400 hover:text-white transition-colors"
        >
          <X className="w-5 h-5" />
        </button>
      </div>

      <div className="space-y-4">
        {/* Add new card */}
        <div>
          <label className="block text-sm font-medium text-gray-300 mb-1">
            Add New Card
          </label>
          <div className="flex gap-2">
            <input
              type="text"
              value={newCardPattern}
              onChange={(e) => setNewCardPattern(e.target.value)}
              onKeyPress={(e) => {
                if (e.key === 'Enter') {
                  handleAddCard();
                }
              }}
              className="flex-1 px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500"
              placeholder="Card pattern (e.g., 'identity', 'successor')"
            />
            <Button
              onClick={handleAddCard}
              variant="primary"
              size="sm"
            >
              <Plus className="w-4 h-4 mr-1" />
              Add
            </Button>
          </div>
        </div>

        {/* Existing cards */}
        <div>
          <label className="block text-sm font-medium text-gray-300 mb-2">
            Existing Cards ({slide.cards?.length || 0})
          </label>
          <div className="space-y-2 max-h-64 overflow-y-auto">
            {slide.cards && slide.cards.length > 0 ? (
              slide.cards.map((card) => (
                <div
                  key={card.id}
                  className="flex items-center justify-between p-2 bg-gray-700 rounded"
                >
                  <div className="flex-1">
                    <div className="text-sm font-medium text-white">
                      {card.pattern}
                    </div>
                    <div className="text-xs text-gray-400">
                      {card.jsonlLines.length} lines
                    </div>
                  </div>
                  <Button
                    onClick={() => handleRemoveCard(card.id)}
                    variant="outline"
                    size="sm"
                    className="text-red-400 hover:text-red-300"
                  >
                    <Trash2 className="w-4 h-4" />
                  </Button>
                </div>
              ))
            ) : (
              <div className="text-sm text-gray-400 text-center py-4">
                No cards in this slide
              </div>
            )}
          </div>
        </div>
      </div>
    </Card>
  );
};

