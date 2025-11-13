/**
 * Export Dialog Component
 * 
 * UI for exporting provenance chains in multiple formats.
 */

import React, { useState } from 'react';
import { Download, X, FileJson, FileText, Image, Loader } from 'lucide-react';
import { motion, AnimatePresence } from 'framer-motion';
import { Button } from '../shared/Button';
import { Card } from '../shared/Card';
import { ProvenanceChain } from '../../services/provenance-slide-service';
import { provenanceExportService, ExportFormat, ExportOptions } from '../../services/provenance-export-service';

interface ExportDialogProps {
  chain: ProvenanceChain;
  onClose: () => void;
}

export const ExportDialog: React.FC<ExportDialogProps> = ({
  chain,
  onClose
}) => {
  const [selectedFormat, setSelectedFormat] = useState<ExportFormat>('json');
  const [filename, setFilename] = useState('');
  const [includeMetadata, setIncludeMetadata] = useState(true);
  const [exporting, setExporting] = useState(false);
  const [imageWidth, setImageWidth] = useState(1920);
  const [imageHeight, setImageHeight] = useState(1080);
  const [backgroundColor, setBackgroundColor] = useState('#1f2937');

  const formats: Array<{ value: ExportFormat; label: string; icon: React.ReactNode; description: string }> = [
    { value: 'json', label: 'JSON', icon: <FileJson className="w-5 h-5" />, description: 'Structured JSON format' },
    { value: 'jsonl', label: 'JSONL', icon: <FileText className="w-5 h-5" />, description: 'JSON Lines format (one object per line)' },
    { value: 'graphml', label: 'GraphML', icon: <FileText className="w-5 h-5" />, description: 'GraphML XML format for graph tools' },
    { value: 'dot', label: 'DOT', icon: <FileText className="w-5 h-5" />, description: 'Graphviz DOT format' },
    { value: 'png', label: 'PNG', icon: <Image className="w-5 h-5" />, description: 'PNG image export' },
    { value: 'svg', label: 'SVG', icon: <Image className="w-5 h-5" />, description: 'SVG vector image export' }
  ];

  const handleExport = async () => {
    setExporting(true);
    try {
      const options: ExportOptions = {
        format: selectedFormat,
        filename: filename || undefined,
        includeMetadata,
        imageOptions: (selectedFormat === 'png' || selectedFormat === 'svg') ? {
          width: imageWidth,
          height: imageHeight,
          backgroundColor
        } : undefined
      };

      await provenanceExportService.exportChain(chain, options);
      onClose();
    } catch (error) {
      console.error('Export failed:', error);
      alert(`Export failed: ${error instanceof Error ? error.message : String(error)}`);
    } finally {
      setExporting(false);
    }
  };

  const selectedFormatInfo = formats.find(f => f.value === selectedFormat);

  return (
    <motion.div
      initial={{ opacity: 0 }}
      animate={{ opacity: 1 }}
      exit={{ opacity: 0 }}
      className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50 p-4"
      onClick={onClose}
    >
      <motion.div
        initial={{ scale: 0.9, y: 20 }}
        animate={{ scale: 1, y: 0 }}
        exit={{ scale: 0.9, y: 20 }}
        className="bg-gray-900 border border-gray-700 rounded-lg shadow-xl max-w-2xl w-full max-h-[90vh] overflow-hidden flex flex-col"
        onClick={(e) => e.stopPropagation()}
      >
        {/* Header */}
        <div className="flex items-center justify-between p-6 border-b border-gray-700">
          <div className="flex items-center gap-3">
            <Download className="w-6 h-6 text-blue-400" />
            <h2 className="text-2xl font-bold text-white">Export Provenance Chain</h2>
          </div>
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-white transition-colors"
          >
            <X className="w-6 h-6" />
          </button>
        </div>

        {/* Content */}
        <div className="flex-1 overflow-y-auto p-6 space-y-6">
          {/* Format Selection */}
          <div>
            <label className="block text-sm font-medium text-gray-300 mb-3">
              Export Format
            </label>
            <div className="grid grid-cols-2 gap-3">
              {formats.map((format) => (
                <button
                  key={format.value}
                  onClick={() => setSelectedFormat(format.value)}
                  className={`p-4 rounded-lg border-2 transition-all ${
                    selectedFormat === format.value
                      ? 'border-blue-500 bg-blue-500 bg-opacity-10'
                      : 'border-gray-700 bg-gray-800 hover:border-gray-600'
                  }`}
                >
                  <div className="flex items-center gap-3 mb-2">
                    <div className={`${
                      selectedFormat === format.value ? 'text-blue-400' : 'text-gray-400'
                    }`}>
                      {format.icon}
                    </div>
                    <span className={`font-semibold ${
                      selectedFormat === format.value ? 'text-white' : 'text-gray-300'
                    }`}>
                      {format.label}
                    </span>
                  </div>
                  <p className="text-xs text-gray-400 text-left">
                    {format.description}
                  </p>
                </button>
              ))}
            </div>
          </div>

          {/* Format Info */}
          {selectedFormatInfo && (
            <div className="bg-gray-800 rounded-lg p-4 border border-gray-700">
              <div className="flex items-center gap-2 mb-2">
                {selectedFormatInfo.icon}
                <span className="font-semibold text-white">{selectedFormatInfo.label}</span>
              </div>
              <p className="text-sm text-gray-400">{selectedFormatInfo.description}</p>
              <div className="mt-3 text-xs text-gray-500">
                <p>Nodes: {chain.nodes.length}</p>
                <p>Edges: {chain.edges.length}</p>
              </div>
            </div>
          )}

          {/* Filename */}
          <div>
            <label className="block text-sm font-medium text-gray-300 mb-2">
              Filename (optional)
            </label>
            <input
              type="text"
              value={filename}
              onChange={(e) => setFilename(e.target.value)}
              placeholder="Auto-generated if empty"
              className="w-full px-3 py-2 bg-gray-800 border border-gray-700 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500"
            />
          </div>

          {/* Metadata Option */}
          {(selectedFormat === 'json' || selectedFormat === 'jsonl') && (
            <div className="flex items-center gap-3">
              <input
                type="checkbox"
                id="includeMetadata"
                checked={includeMetadata}
                onChange={(e) => setIncludeMetadata(e.target.checked)}
                className="rounded"
              />
              <label htmlFor="includeMetadata" className="text-sm text-gray-300">
                Include full metadata (position, data, etc.)
              </label>
            </div>
          )}

          {/* Image Options */}
          {(selectedFormat === 'png' || selectedFormat === 'svg') && (
            <div className="space-y-4 bg-gray-800 rounded-lg p-4 border border-gray-700">
              <h3 className="text-sm font-semibold text-white mb-3">Image Options</h3>
              
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <label className="block text-sm font-medium text-gray-300 mb-2">
                    Width (px)
                  </label>
                  <input
                    type="number"
                    value={imageWidth}
                    onChange={(e) => setImageWidth(parseInt(e.target.value) || 1920)}
                    min={100}
                    max={7680}
                    className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500"
                  />
                </div>
                
                <div>
                  <label className="block text-sm font-medium text-gray-300 mb-2">
                    Height (px)
                  </label>
                  <input
                    type="number"
                    value={imageHeight}
                    onChange={(e) => setImageHeight(parseInt(e.target.value) || 1080)}
                    min={100}
                    max={4320}
                    className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white focus:outline-none focus:ring-2 focus:ring-blue-500"
                  />
                </div>
              </div>
              
              <div>
                <label className="block text-sm font-medium text-gray-300 mb-2">
                  Background Color
                </label>
                <div className="flex items-center gap-2">
                  <input
                    type="color"
                    value={backgroundColor}
                    onChange={(e) => setBackgroundColor(e.target.value)}
                    className="w-12 h-10 rounded border border-gray-600"
                  />
                  <input
                    type="text"
                    value={backgroundColor}
                    onChange={(e) => setBackgroundColor(e.target.value)}
                    className="flex-1 px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white font-mono text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
                  />
                </div>
              </div>
            </div>
          )}
        </div>

        {/* Footer */}
        <div className="flex items-center justify-end gap-3 p-6 border-t border-gray-700">
          <Button
            onClick={onClose}
            variant="outline"
            size="sm"
            disabled={exporting}
          >
            Cancel
          </Button>
          <Button
            onClick={handleExport}
            variant="primary"
            size="sm"
            disabled={exporting}
          >
            {exporting ? (
              <>
                <Loader className="w-4 h-4 mr-2 animate-spin" />
                Exporting...
              </>
            ) : (
              <>
                <Download className="w-4 h-4 mr-2" />
                Export
              </>
            )}
          </Button>
        </div>
      </motion.div>
    </motion.div>
  );
};

