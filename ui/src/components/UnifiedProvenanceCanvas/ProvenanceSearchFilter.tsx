/**
 * Provenance Search and Filter Component
 * 
 * UI for searching and filtering provenance chains with advanced query builder
 * and filter preset management.
 */

import React, { useState, useEffect } from 'react';
import { Search, Filter, X, Save, Share2, Trash2, Plus } from 'lucide-react';
import { Button } from '../shared/Button';
import { Card } from '../shared/Card';
import { ProvenanceChain, ProvenanceNode, ProvenanceEdge } from '../../services/provenance-slide-service';
import { provenanceSearchService, SearchQuery, FilterPreset } from '../../services/provenance-search-service';

interface ProvenanceSearchFilterProps {
  chain: ProvenanceChain;
  onFilterChange: (filteredChain: ProvenanceChain) => void;
  onClose?: () => void;
}

export const ProvenanceSearchFilter: React.FC<ProvenanceSearchFilterProps> = ({
  chain,
  onFilterChange,
  onClose
}) => {
  const [searchQuery, setSearchQuery] = useState<SearchQuery>({});
  const [filteredChain, setFilteredChain] = useState<ProvenanceChain>(chain);
  const [presets, setPresets] = useState<FilterPreset[]>([]);
  const [showPresetDialog, setShowPresetDialog] = useState(false);
  const [presetName, setPresetName] = useState('');
  const [queryLogic, setQueryLogic] = useState<'AND' | 'OR'>('AND');
  const [advancedQueries, setAdvancedQueries] = useState<SearchQuery[]>([]);

  useEffect(() => {
    // Load presets
    const loadedPresets = provenanceSearchService.getAllPresets();
    setPresets(loadedPresets);

    // Apply initial filter
    applyFilter(searchQuery);
  }, []);

  useEffect(() => {
    applyFilter(searchQuery);
  }, [chain, searchQuery]);

  const applyFilter = (query: SearchQuery) => {
    const result = provenanceSearchService.searchChain(chain, query);
    setFilteredChain({
      nodes: result.nodes,
      edges: result.edges
    });
    onFilterChange({
      nodes: result.nodes,
      edges: result.edges
    });
  };

  const handleSearchChange = (field: keyof SearchQuery, value: any) => {
    const newQuery = { ...searchQuery, [field]: value || undefined };
    setSearchQuery(newQuery);
  };

  const handleClearFilter = () => {
    setSearchQuery({});
    setAdvancedQueries([]);
    setFilteredChain(chain);
    onFilterChange(chain);
  };

  const handleSavePreset = () => {
    if (!presetName.trim()) {
      return;
    }

    const queryToSave = advancedQueries.length > 0
      ? provenanceSearchService.buildAdvancedQuery(advancedQueries, queryLogic)
      : searchQuery;

    provenanceSearchService.savePreset(presetName, queryToSave);
    
    // Reload presets
    setPresets(provenanceSearchService.getAllPresets());
    setShowPresetDialog(false);
    setPresetName('');
  };

  const handleLoadPreset = (preset: FilterPreset) => {
    setSearchQuery(preset.query);
    setAdvancedQueries([]);
  };

  const handleDeletePreset = (presetId: string) => {
    provenanceSearchService.deletePreset(presetId);
    setPresets(provenanceSearchService.getAllPresets());
  };

  const handleExportPreset = (presetId: string) => {
    const exported = provenanceSearchService.exportPreset(presetId);
    navigator.clipboard.writeText(exported);
    alert('Preset copied to clipboard!');
  };

  const handleAddAdvancedQuery = () => {
    setAdvancedQueries([...advancedQueries, {}]);
  };

  const handleUpdateAdvancedQuery = (index: number, query: SearchQuery) => {
    const updated = [...advancedQueries];
    updated[index] = query;
    setAdvancedQueries(updated);
    
    // Apply combined query
    if (updated.every(q => Object.keys(q).length > 0)) {
      const combined = provenanceSearchService.buildAdvancedQuery(updated, queryLogic);
      applyFilter(combined);
    }
  };

  const handleRemoveAdvancedQuery = (index: number) => {
    const updated = advancedQueries.filter((_, i) => i !== index);
    setAdvancedQueries(updated);
    
    if (updated.length === 0) {
      applyFilter(searchQuery);
    } else {
      const combined = provenanceSearchService.buildAdvancedQuery(updated, queryLogic);
      applyFilter(combined);
    }
  };

  return (
    <Card className="p-4 bg-gray-800 border border-gray-700">
      <div className="flex items-center justify-between mb-4">
        <div className="flex items-center gap-2">
          <Search className="w-5 h-5 text-gray-400" />
          <h3 className="text-lg font-semibold text-white">Search & Filter</h3>
        </div>
        {onClose && (
          <button
            onClick={onClose}
            className="text-gray-400 hover:text-white transition-colors"
          >
            <X className="w-5 h-5" />
          </button>
        )}
      </div>

      <div className="space-y-4">
        {/* Basic Search Fields */}
        <div className="grid grid-cols-2 gap-3">
          <div>
            <label className="block text-sm font-medium text-gray-300 mb-1">
              Pattern
            </label>
            <input
              type="text"
              value={searchQuery.pattern || ''}
              onChange={(e) => handleSearchChange('pattern', e.target.value)}
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              placeholder="e.g., identity, successor"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-300 mb-1">
              Dimension
            </label>
            <select
              value={searchQuery.dimension || ''}
              onChange={(e) => handleSearchChange('dimension', e.target.value)}
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
            >
              <option value="">All Dimensions</option>
              <option value="0D">0D</option>
              <option value="1D">1D</option>
              <option value="2D">2D</option>
              <option value="3D">3D</option>
              <option value="4D">4D</option>
              <option value="5D">5D</option>
              <option value="6D">6D</option>
              <option value="7D">7D</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-300 mb-1">
              Agent ID
            </label>
            <input
              type="text"
              value={searchQuery.agentId || ''}
              onChange={(e) => handleSearchChange('agentId', e.target.value)}
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              placeholder="e.g., 0D-Topology-Agent"
            />
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-300 mb-1">
              Node Type
            </label>
            <select
              value={searchQuery.nodeType || ''}
              onChange={(e) => handleSearchChange('nodeType', e.target.value as ProvenanceNode['type'])}
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
            >
              <option value="">All Types</option>
              <option value="agent">Agent</option>
              <option value="document">Document</option>
              <option value="code">Code</option>
              <option value="interaction">Interaction</option>
              <option value="evolution">Evolution</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-300 mb-1">
              Edge Type
            </label>
            <select
              value={searchQuery.edgeType || ''}
              onChange={(e) => handleSearchChange('edgeType', e.target.value as ProvenanceEdge['type'])}
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
            >
              <option value="">All Types</option>
              <option value="consumes">Consumes</option>
              <option value="produces">Produces</option>
              <option value="references">References</option>
              <option value="evolves">Evolves</option>
              <option value="interacts">Interacts</option>
            </select>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-300 mb-1">
              File
            </label>
            <input
              type="text"
              value={searchQuery.file || ''}
              onChange={(e) => handleSearchChange('file', e.target.value)}
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              placeholder="e.g., automaton.jsonl"
            />
          </div>
        </div>

        {/* Advanced Query Builder */}
        <div className="border-t border-gray-700 pt-4">
          <div className="flex items-center justify-between mb-3">
            <h4 className="text-sm font-semibold text-white">Advanced Query Builder</h4>
            <Button
              onClick={handleAddAdvancedQuery}
              variant="outline"
              size="sm"
            >
              <Plus className="w-4 h-4 mr-1" />
              Add Query
            </Button>
          </div>

          {advancedQueries.length > 0 && (
            <div className="space-y-2 mb-3">
              <div className="flex items-center gap-2">
                <span className="text-xs text-gray-400">Logic:</span>
                <select
                  value={queryLogic}
                  onChange={(e) => setQueryLogic(e.target.value as 'AND' | 'OR')}
                  className="px-2 py-1 bg-gray-700 border border-gray-600 rounded text-white text-xs"
                >
                  <option value="AND">AND</option>
                  <option value="OR">OR</option>
                </select>
              </div>

              {advancedQueries.map((query, index) => (
                <div key={index} className="flex items-center gap-2 p-2 bg-gray-700 rounded">
                  <span className="text-xs text-gray-400 w-8">#{index + 1}</span>
                  <input
                    type="text"
                    placeholder="Pattern"
                    value={query.pattern || ''}
                    onChange={(e) => handleUpdateAdvancedQuery(index, { ...query, pattern: e.target.value })}
                    className="flex-1 px-2 py-1 bg-gray-600 border border-gray-500 rounded text-white text-xs"
                  />
                  <select
                    value={query.dimension || ''}
                    onChange={(e) => handleUpdateAdvancedQuery(index, { ...query, dimension: e.target.value })}
                    className="px-2 py-1 bg-gray-600 border border-gray-500 rounded text-white text-xs"
                  >
                    <option value="">Any Dimension</option>
                    <option value="0D">0D</option>
                    <option value="1D">1D</option>
                    <option value="2D">2D</option>
                    <option value="3D">3D</option>
                    <option value="4D">4D</option>
                    <option value="5D">5D</option>
                    <option value="6D">6D</option>
                    <option value="7D">7D</option>
                  </select>
                  <Button
                    onClick={() => handleRemoveAdvancedQuery(index)}
                    variant="outline"
                    size="sm"
                    className="text-red-400"
                  >
                    <X className="w-3 h-3" />
                  </Button>
                </div>
              ))}
            </div>
          )}
        </div>

        {/* Filter Presets */}
        <div className="border-t border-gray-700 pt-4">
          <div className="flex items-center justify-between mb-3">
            <h4 className="text-sm font-semibold text-white">Saved Presets</h4>
            <Button
              onClick={() => setShowPresetDialog(true)}
              variant="outline"
              size="sm"
            >
              <Save className="w-4 h-4 mr-1" />
              Save Preset
            </Button>
          </div>

          {presets.length > 0 ? (
            <div className="space-y-2 max-h-48 overflow-y-auto">
              {presets.map((preset) => (
                <div
                  key={preset.id}
                  className="flex items-center justify-between p-2 bg-gray-700 rounded"
                >
                  <div className="flex-1">
                    <div className="text-sm text-white">{preset.name}</div>
                    <div className="text-xs text-gray-400">
                      {preset.query.pattern && `Pattern: ${preset.query.pattern} `}
                      {preset.query.dimension && `Dimension: ${preset.query.dimension}`}
                    </div>
                  </div>
                  <div className="flex items-center gap-1">
                    <Button
                      onClick={() => handleLoadPreset(preset)}
                      variant="outline"
                      size="sm"
                      className="text-xs"
                    >
                      Load
                    </Button>
                    <Button
                      onClick={() => handleExportPreset(preset.id)}
                      variant="outline"
                      size="sm"
                      className="text-xs"
                    >
                      <Share2 className="w-3 h-3" />
                    </Button>
                    <Button
                      onClick={() => handleDeletePreset(preset.id)}
                      variant="outline"
                      size="sm"
                      className="text-xs text-red-400"
                    >
                      <Trash2 className="w-3 h-3" />
                    </Button>
                  </div>
                </div>
              ))}
            </div>
          ) : (
            <div className="text-sm text-gray-400 text-center py-4">
              No saved presets
            </div>
          )}
        </div>

        {/* Results Summary */}
        <div className="border-t border-gray-700 pt-4">
          <div className="flex items-center justify-between text-sm">
            <span className="text-gray-400">Results:</span>
            <div className="flex items-center gap-4">
              <span className="text-white">
                {filteredChain.nodes.length} / {chain.nodes.length} nodes
              </span>
              <span className="text-white">
                {filteredChain.edges.length} / {chain.edges.length} edges
              </span>
            </div>
          </div>
        </div>

        {/* Actions */}
        <div className="flex items-center justify-end gap-2">
          <Button
            onClick={handleClearFilter}
            variant="outline"
            size="sm"
          >
            Clear Filter
          </Button>
        </div>
      </div>

      {/* Save Preset Dialog */}
      {showPresetDialog && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
          <div className="bg-gray-800 border border-gray-700 rounded-lg p-4 w-96">
            <h4 className="text-lg font-semibold text-white mb-4">Save Filter Preset</h4>
            <input
              type="text"
              value={presetName}
              onChange={(e) => setPresetName(e.target.value)}
              placeholder="Preset name"
              className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded text-white mb-4"
              onKeyPress={(e) => {
                if (e.key === 'Enter') {
                  handleSavePreset();
                }
              }}
            />
            <div className="flex items-center justify-end gap-2">
              <Button
                onClick={() => {
                  setShowPresetDialog(false);
                  setPresetName('');
                }}
                variant="outline"
                size="sm"
              >
                Cancel
              </Button>
              <Button
                onClick={handleSavePreset}
                variant="primary"
                size="sm"
                disabled={!presetName.trim()}
              >
                Save
              </Button>
            </div>
          </div>
        </div>
      )}
    </Card>
  );
};

