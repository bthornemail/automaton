/**
 * Conflict Resolution Dialog Component
 * 
 * Dialog for manually resolving conflicts in provenance chain updates.
 * Displays conflicting updates side-by-side and allows user to choose resolution.
 */

import React, { useState } from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import { X, Check, AlertTriangle, Clock, User } from 'lucide-react';
import { Button } from '../shared/Button';
import { Card } from '../shared/Card';
import type { Conflict, ChainUpdate, SlideUpdate, CardUpdate } from '../../types/provenance-updates';

interface ConflictResolutionDialogProps {
  conflict: Conflict;
  onResolve: (resolution: ChainUpdate | SlideUpdate | CardUpdate) => void;
  onDismiss: () => void;
}

export const ConflictResolutionDialog: React.FC<ConflictResolutionDialogProps> = ({
  conflict,
  onResolve,
  onDismiss
}) => {
  const [selectedUpdate, setSelectedUpdate] = useState<ChainUpdate | SlideUpdate | CardUpdate | null>(null);
  const [customResolution, setCustomResolution] = useState<string>('');

  const formatTimestamp = (timestamp: number): string => {
    return new Date(timestamp).toLocaleString();
  };

  const getUpdateTypeLabel = (update: ChainUpdate | SlideUpdate | CardUpdate): string => {
    if ('type' in update) {
      return update.type;
    } else if ('slideId' in update) {
      return 'Slide Update';
    } else if ('cardId' in update) {
      return 'Card Update';
    }
    return 'Unknown';
  };

  const getUpdateDescription = (update: ChainUpdate | SlideUpdate | CardUpdate): string => {
    if ('type' in update) {
      const chainUpdate = update as ChainUpdate;
      switch (chainUpdate.type) {
        case 'node:added':
          return `Added node: ${chainUpdate.data.node?.id || chainUpdate.data.nodeId || 'unknown'}`;
        case 'node:updated':
          return `Updated node: ${chainUpdate.data.node?.id || chainUpdate.data.nodeId || 'unknown'}`;
        case 'node:removed':
          return `Removed node: ${chainUpdate.data.nodeId || 'unknown'}`;
        case 'edge:added':
          return `Added edge: ${chainUpdate.data.edge?.id || chainUpdate.data.edgeId || 'unknown'}`;
        case 'edge:removed':
          return `Removed edge: ${chainUpdate.data.edgeId || 'unknown'}`;
        case 'chain:rebuilt':
          return 'Chain rebuilt from scratch';
        default:
          return getUpdateTypeLabel(update);
      }
    } else if ('slideId' in update) {
      const slideUpdate = update as SlideUpdate;
      return `Updated slide: ${slideUpdate.slideId}`;
    } else if ('cardId' in update) {
      const cardUpdate = update as CardUpdate;
      return `Updated card: ${cardUpdate.cardId}`;
    }
    return 'Unknown update';
  };

  const handleResolve = () => {
    if (selectedUpdate) {
      onResolve(selectedUpdate);
    }
  };

  const handleLastWriteWins = () => {
    // Select the most recent update
    const sortedUpdates = [...conflict.updates].sort((a, b) => b.timestamp - a.timestamp);
    onResolve(sortedUpdates[0]);
  };

  return (
    <AnimatePresence>
      <motion.div
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        exit={{ opacity: 0 }}
        className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50"
        onClick={onDismiss}
      >
        <motion.div
          initial={{ scale: 0.9, opacity: 0 }}
          animate={{ scale: 1, opacity: 1 }}
          exit={{ scale: 0.9, opacity: 0 }}
          className="bg-gray-900 border border-gray-700 rounded-lg shadow-xl max-w-4xl w-full mx-4 max-h-[90vh] overflow-y-auto"
          onClick={(e) => e.stopPropagation()}
        >
          {/* Header */}
          <div className="flex items-center justify-between p-6 border-b border-gray-700">
            <div className="flex items-center gap-3">
              <AlertTriangle className="w-6 h-6 text-yellow-500" />
              <h2 className="text-2xl font-bold text-white">Conflict Resolution</h2>
            </div>
            <button
              onClick={onDismiss}
              className="text-gray-400 hover:text-white transition-colors"
            >
              <X className="w-6 h-6" />
            </button>
          </div>

          {/* Conflict Info */}
          <div className="p-6 border-b border-gray-700">
            <div className="flex items-center gap-2 text-sm text-gray-400 mb-2">
              <span>Type: {conflict.type}</span>
              <span>•</span>
              <span>ID: {conflict.id}</span>
              <span>•</span>
              <span>Path: {conflict.evolutionPath}</span>
            </div>
            <p className="text-gray-300">
              Multiple clients have modified the same {conflict.type}. Choose which update to keep.
            </p>
          </div>

          {/* Conflicting Updates */}
          <div className="p-6">
            <h3 className="text-lg font-semibold text-white mb-4">Conflicting Updates</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {conflict.updates.map((update, index) => {
                const isSelected = selectedUpdate === update;
                const isMostRecent = update.timestamp === Math.max(...conflict.updates.map(u => u.timestamp));

                return (
                  <Card
                    key={index}
                    className={`p-4 cursor-pointer transition-all ${
                      isSelected
                        ? 'border-blue-500 bg-blue-900 bg-opacity-20'
                        : 'border-gray-700 hover:border-gray-600'
                    }`}
                    onClick={() => setSelectedUpdate(update)}
                  >
                    <div className="flex items-start justify-between mb-3">
                      <div className="flex items-center gap-2">
                        <User className="w-4 h-4 text-gray-400" />
                        <span className="text-sm text-gray-400">
                          Client: {update.clientId.substring(0, 8)}...
                        </span>
                      </div>
                      {isMostRecent && (
                        <span className="text-xs bg-green-900 text-green-300 px-2 py-1 rounded">
                          Most Recent
                        </span>
                      )}
                    </div>

                    <div className="mb-3">
                      <div className="text-sm font-semibold text-white mb-1">
                        {getUpdateTypeLabel(update)}
                      </div>
                      <div className="text-sm text-gray-400">
                        {getUpdateDescription(update)}
                      </div>
                    </div>

                    <div className="flex items-center gap-2 text-xs text-gray-500">
                      <Clock className="w-3 h-3" />
                      <span>{formatTimestamp(update.timestamp)}</span>
                    </div>

                    {isSelected && (
                      <div className="mt-3 pt-3 border-t border-gray-700">
                        <div className="flex items-center gap-2 text-sm text-blue-400">
                          <Check className="w-4 h-4" />
                          <span>Selected for resolution</span>
                        </div>
                      </div>
                    )}
                  </Card>
                );
              })}
            </div>
          </div>

          {/* Resolution Options */}
          <div className="p-6 border-t border-gray-700 bg-gray-800">
            <div className="flex items-center justify-between gap-4">
              <div className="flex gap-2">
                <Button
                  onClick={handleLastWriteWins}
                  variant="outline"
                  size="sm"
                  className="text-sm"
                >
                  Use Most Recent
                </Button>
                <Button
                  onClick={handleResolve}
                  variant="primary"
                  size="sm"
                  disabled={!selectedUpdate}
                  className="text-sm"
                >
                  Resolve with Selected
                </Button>
              </div>
              <Button
                onClick={onDismiss}
                variant="outline"
                size="sm"
                className="text-sm"
              >
                Cancel
              </Button>
            </div>
          </div>
        </motion.div>
      </motion.div>
    </AnimatePresence>
  );
};

