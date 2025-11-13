/**
 * Card Detail View Component
 * 
 * Expandable card details with JSONL line viewer, provenance history timeline,
 * and pattern visualization.
 */

import React, { useState } from 'react';
import { X, ChevronDown, ChevronUp, Code, History, Layers, FileText } from 'lucide-react';
import { motion, AnimatePresence } from 'framer-motion';
import { Card } from '../../services/provenance-slide-service';
import { Button } from '../shared/Button';

interface CardDetailViewProps {
  card: Card;
  onClose: () => void;
}

export const CardDetailView: React.FC<CardDetailViewProps> = ({
  card,
  onClose
}) => {
  const [expandedSections, setExpandedSections] = useState<Set<string>>(new Set(['details']));
  const [selectedJsonlLine, setSelectedJsonlLine] = useState<number | null>(null);

  const toggleSection = (section: string) => {
    const newExpanded = new Set(expandedSections);
    if (newExpanded.has(section)) {
      newExpanded.delete(section);
    } else {
      newExpanded.add(section);
    }
    setExpandedSections(newExpanded);
  };

  const formatJsonlLine = (line: any, index: number): string => {
    try {
      return JSON.stringify(line, null, 2);
    } catch {
      return String(line);
    }
  };

  const formatTimestamp = (timestamp: number): string => {
    return new Date(timestamp).toLocaleString();
  };

  return (
    <motion.div
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      exit={{ opacity: 0, y: 20 }}
      className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4"
      onClick={onClose}
    >
      <motion.div
        initial={{ scale: 0.9 }}
        animate={{ scale: 1 }}
        exit={{ scale: 0.9 }}
        className="bg-gray-900 border border-gray-700 rounded-lg shadow-xl max-w-4xl w-full max-h-[90vh] overflow-hidden flex flex-col"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b border-gray-700">
          <div>
            <h2 className="text-2xl font-bold text-white mb-1">Card Details</h2>
            <p className="text-sm text-gray-400">Pattern: {card.pattern}</p>
          </div>
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-white transition-colors"
          >
            <X className="w-6 h-6" />
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-y-auto p-6 space-y-4">
          {/* Card Details Section */}
          <div className="bg-gray-800 rounded-lg border border-gray-700">
            <button
              onClick={() => toggleSection('details')}
              className="w-full flex items-center justify-between p-4 text-left"
            >
              <div className="flex items-center gap-2">
                <FileText className="w-5 h-5 text-gray-400" />
                <span className="font-semibold text-white">Card Details</span>
              </div>
              {expandedSections.has('details') ? (
                <ChevronUp className="w-5 h-5 text-gray-400" />
              ) : (
                <ChevronDown className="w-5 h-5 text-gray-400" />
              )}
            </button>
            <AnimatePresence>
              {expandedSections.has('details') && (
                <motion.div
                  initial={{ height: 0, opacity: 0 }}
                  animate={{ height: 'auto', opacity: 1 }}
                  exit={{ height: 0, opacity: 0 }}
                  className="overflow-hidden"
                >
                  <div className="px-4 pb-4 space-y-3">
                    <div>
                      <label className="text-sm font-medium text-gray-400">Pattern</label>
                      <p className="text-white mt-1">{card.pattern}</p>
                    </div>
                    <div>
                      <label className="text-sm font-medium text-gray-400">Card ID</label>
                      <p className="text-white mt-1 font-mono text-sm">{card.id}</p>
                    </div>
                    <div>
                      <label className="text-sm font-medium text-gray-400">JSONL Lines</label>
                      <p className="text-white mt-1">{card.jsonlLines.length} lines</p>
                    </div>
                    {card.metadata.churchEncoding && (
                      <div>
                        <label className="text-sm font-medium text-gray-400">Church Encoding</label>
                        <p className="text-white mt-1 font-mono text-sm">{card.metadata.churchEncoding}</p>
                      </div>
                    )}
                    {card.metadata.bqfCoefficients && (
                      <div>
                        <label className="text-sm font-medium text-gray-400">BQF Coefficients</label>
                        <p className="text-white mt-1 font-mono text-sm">
                          [{card.metadata.bqfCoefficients.join(', ')}]
                        </p>
                      </div>
                    )}
                  </div>
                </motion.div>
              )}
            </AnimatePresence>
          </div>

          {/* JSONL Line Viewer Section */}
          <div className="bg-gray-800 rounded-lg border border-gray-700">
            <button
              onClick={() => toggleSection('jsonl')}
              className="w-full flex items-center justify-between p-4 text-left"
            >
              <div className="flex items-center gap-2">
                <Code className="w-5 h-5 text-gray-400" />
                <span className="font-semibold text-white">
                  JSONL Lines ({card.jsonlLines.length})
                </span>
              </div>
              {expandedSections.has('jsonl') ? (
                <ChevronUp className="w-5 h-5 text-gray-400" />
              ) : (
                <ChevronDown className="w-5 h-5 text-gray-400" />
              )}
            </button>
            <AnimatePresence>
              {expandedSections.has('jsonl') && (
                <motion.div
                  initial={{ height: 0, opacity: 0 }}
                  animate={{ height: 'auto', opacity: 1 }}
                  exit={{ height: 0, opacity: 0 }}
                  className="overflow-hidden"
                >
                  <div className="px-4 pb-4">
                    <div className="bg-gray-900 rounded border border-gray-700 max-h-96 overflow-y-auto">
                      <table className="w-full text-sm">
                        <thead className="bg-gray-800 sticky top-0">
                          <tr>
                            <th className="px-3 py-2 text-left text-gray-400 font-medium">Line</th>
                            <th className="px-3 py-2 text-left text-gray-400 font-medium">Content</th>
                          </tr>
                        </thead>
                        <tbody>
                          {card.jsonlLines.map((line, index) => (
                            <tr
                              key={index}
                              className={`border-t border-gray-700 hover:bg-gray-800 cursor-pointer ${
                                selectedJsonlLine === index ? 'bg-blue-900 bg-opacity-20' : ''
                              }`}
                              onClick={() => setSelectedJsonlLine(selectedJsonlLine === index ? null : index)}
                            >
                              <td className="px-3 py-2 text-gray-500 font-mono text-xs">
                                {index + 1}
                              </td>
                              <td className="px-3 py-2">
                                <pre className="text-gray-300 font-mono text-xs whitespace-pre-wrap break-words">
                                  {formatJsonlLine(line, index)}
                                </pre>
                              </td>
                            </tr>
                          ))}
                        </tbody>
                      </table>
                    </div>
                  </div>
                </motion.div>
              )}
            </AnimatePresence>
          </div>

          {/* Provenance History Timeline Section */}
          <div className="bg-gray-800 rounded-lg border border-gray-700">
            <button
              onClick={() => toggleSection('provenance')}
              className="w-full flex items-center justify-between p-4 text-left"
            >
              <div className="flex items-center gap-2">
                <History className="w-5 h-5 text-gray-400" />
                <span className="font-semibold text-white">
                  Provenance History ({card.metadata.provenanceHistory?.length || 0})
                </span>
              </div>
              {expandedSections.has('provenance') ? (
                <ChevronUp className="w-5 h-5 text-gray-400" />
              ) : (
                <ChevronDown className="w-5 h-5 text-gray-400" />
              )}
            </button>
            <AnimatePresence>
              {expandedSections.has('provenance') && (
                <motion.div
                  initial={{ height: 0, opacity: 0 }}
                  animate={{ height: 'auto', opacity: 1 }}
                  exit={{ height: 0, opacity: 0 }}
                  className="overflow-hidden"
                >
                  <div className="px-4 pb-4">
                    {card.metadata.provenanceHistory && card.metadata.provenanceHistory.length > 0 ? (
                      <div className="space-y-3">
                        {card.metadata.provenanceHistory.map((entry: any, index: number) => (
                          <div
                            key={index}
                            className="flex items-start gap-3 p-3 bg-gray-900 rounded border border-gray-700"
                          >
                            <div className="flex-shrink-0 w-2 h-2 rounded-full bg-blue-500 mt-2" />
                            <div className="flex-1">
                              <div className="flex items-center justify-between mb-1">
                                <span className="text-sm font-medium text-white">
                                  {entry.file || 'Unknown file'}
                                </span>
                                <span className="text-xs text-gray-500">
                                  {entry.timestamp ? formatTimestamp(entry.timestamp) : 'Unknown time'}
                                </span>
                              </div>
                              <div className="text-xs text-gray-400 space-y-1">
                                {entry.line && (
                                  <p>Line: {entry.line}</p>
                                )}
                                {entry.pattern && (
                                  <p>Pattern: {entry.pattern}</p>
                                )}
                                {entry.agentId && (
                                  <p>Agent: {entry.agentId}</p>
                                )}
                              </div>
                            </div>
                          </div>
                        ))}
                      </div>
                    ) : (
                      <div className="text-center py-8 text-gray-400">
                        No provenance history available
                      </div>
                    )}
                  </div>
                </motion.div>
              )}
            </AnimatePresence>
          </div>

          {/* Pattern Visualization Section */}
          <div className="bg-gray-800 rounded-lg border border-gray-700">
            <button
              onClick={() => toggleSection('pattern')}
              className="w-full flex items-center justify-between p-4 text-left"
            >
              <div className="flex items-center gap-2">
                <Layers className="w-5 h-5 text-gray-400" />
                <span className="font-semibold text-white">Pattern Visualization</span>
              </div>
              {expandedSections.has('pattern') ? (
                <ChevronUp className="w-5 h-5 text-gray-400" />
              ) : (
                <ChevronDown className="w-5 h-5 text-gray-400" />
              )}
            </button>
            <AnimatePresence>
              {expandedSections.has('pattern') && (
                <motion.div
                  initial={{ height: 0, opacity: 0 }}
                  animate={{ height: 'auto', opacity: 1 }}
                  exit={{ height: 0, opacity: 0 }}
                  className="overflow-hidden"
                >
                  <div className="px-4 pb-4">
                    <div className="bg-gray-900 rounded border border-gray-700 p-4">
                      <div className="space-y-3">
                        <div>
                          <label className="text-sm font-medium text-gray-400">Pattern Name</label>
                          <p className="text-white mt-1 font-mono">{card.pattern}</p>
                        </div>
                        {card.metadata.churchEncoding && (
                          <div>
                            <label className="text-sm font-medium text-gray-400">Church Encoding</label>
                            <div className="mt-1 p-2 bg-gray-800 rounded border border-gray-700">
                              <code className="text-blue-400 font-mono text-sm">
                                {card.metadata.churchEncoding}
                              </code>
                            </div>
                          </div>
                        )}
                        {card.metadata.bqfCoefficients && (
                          <div>
                            <label className="text-sm font-medium text-gray-400">BQF Form</label>
                            <div className="mt-1 p-2 bg-gray-800 rounded border border-gray-700">
                              <code className="text-green-400 font-mono text-sm">
                                {card.metadata.bqfCoefficients[0]}x² + {card.metadata.bqfCoefficients[1]}xy + {card.metadata.bqfCoefficients[2]}y²
                              </code>
                            </div>
                          </div>
                        )}
                        <div>
                          <label className="text-sm font-medium text-gray-400">Pattern Frequency</label>
                          <p className="text-white mt-1">{card.jsonlLines.length} occurrences</p>
                        </div>
                      </div>
                    </div>
                  </div>
                </motion.div>
              )}
            </AnimatePresence>
          </div>
        </div>

        {/* Footer */}
        <div className="flex items-center justify-end gap-2 p-6 border-t border-gray-700">
          <Button
            onClick={onClose}
            variant="outline"
            size="sm"
          >
            Close
          </Button>
        </div>
      </motion.div>
    </motion.div>
  );
};

