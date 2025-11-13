/**
 * AI Portal Tabs Panel Component
 * 
 * Extracted from AIPortal Modal to be used in UnifiedEditor
 * Provides Evolution Engine, Metrics, Canvas Editor, LLM Config, and Agent API tabs
 */

import React, { useState } from 'react';
import { 
  Zap, BarChart3, FileText, Brain, Bot as BotIcon, Play, Pause
} from 'lucide-react';
import UnifiedEditor from '../UnifiedEditor';
import MetaverseCanvas3D from '@/components/MetaverseCanvas3D/MetaverseCanvas3D';
import { LLMProviderSettingsModal } from '@/components/AIPortal/components/LLMProviderSettingsModal';
import { AgentList, AgentExecution, StatusDashboard, MultiAgentCoordinator } from '@/components/AgentAPI';

interface AIPortalTabsPanelProps {
  selectedJSONLFile?: string;
  onJSONLFileChange?: (file: string) => void;
  onEvolutionLog?: (message: string) => void;
  // Add other necessary props as needed
}

export const AIPortalTabsPanel: React.FC<AIPortalTabsPanelProps> = ({
  selectedJSONLFile: initialSelectedJSONLFile = 'automaton-kernel.jsonl',
  onJSONLFileChange,
  onEvolutionLog
}) => {
  const [activeAITab, setActiveAITab] = useState<'evolution' | 'metrics' | 'canvas' | 'llm-config' | 'agent-api'>('canvas');
  const [selectedJSONLFile, setSelectedJSONLFile] = useState<string>(initialSelectedJSONLFile);
  const [canvasEditorView, setCanvasEditorView] = useState<'editor' | 'canvasl-3d'>('editor');

  const handleJSONLFileChange = (file: string) => {
    setSelectedJSONLFile(file);
    onJSONLFileChange?.(file);
  };

  const addEvolutionLog = (message: string) => {
    onEvolutionLog?.(message);
  };

  return (
    <div className="h-full flex flex-col bg-gray-900">
      {/* Tab Navigation */}
      <div className="flex gap-2 border-b border-gray-700 flex-shrink-0 px-4 pt-2">
        <button
          onClick={() => setActiveAITab('evolution')}
          className={`px-4 py-2 text-sm font-medium transition-colors ${
            activeAITab === 'evolution'
              ? 'text-white border-b-2 border-purple-500'
              : 'text-gray-400 hover:text-gray-300'
          }`}
        >
          <div className="flex items-center gap-2">
            <Zap className="w-4 h-4" />
            Evolution Engine
          </div>
        </button>
        <button
          onClick={() => setActiveAITab('metrics')}
          className={`px-4 py-2 text-sm font-medium transition-colors ${
            activeAITab === 'metrics'
              ? 'text-white border-b-2 border-purple-500'
              : 'text-gray-400 hover:text-gray-300'
          }`}
        >
          <div className="flex items-center gap-2">
            <BarChart3 className="w-4 h-4" />
            Metrics & Logs
          </div>
        </button>
        <button
          onClick={() => setActiveAITab('canvas')}
          className={`px-4 py-2 text-sm font-medium transition-colors ${
            activeAITab === 'canvas'
              ? 'text-white border-b-2 border-purple-500'
              : 'text-gray-400 hover:text-gray-300'
          }`}
        >
          <div className="flex items-center gap-2">
            <FileText className="w-4 h-4" />
            Canvas Editor
          </div>
        </button>
        <button
          onClick={() => setActiveAITab('llm-config')}
          className={`px-4 py-2 text-sm font-medium transition-colors ${
            activeAITab === 'llm-config'
              ? 'text-white border-b-2 border-purple-500'
              : 'text-gray-400 hover:text-gray-300'
          }`}
        >
          <div className="flex items-center gap-2">
            <Brain className="w-4 h-4" />
            LLM Configuration
          </div>
        </button>
        <button
          onClick={() => setActiveAITab('agent-api')}
          className={`px-4 py-2 text-sm font-medium transition-colors ${
            activeAITab === 'agent-api'
              ? 'text-white border-b-2 border-purple-500'
              : 'text-gray-400 hover:text-gray-300'
          }`}
        >
          <div className="flex items-center gap-2">
            <BotIcon className="w-4 h-4" />
            Agent API
          </div>
        </button>
      </div>

      {/* Tab Content */}
      <div className="min-h-[400px] max-h-[calc(90vh-200px)] overflow-y-auto pr-2 flex-1 px-4 pb-4">
        {/* Evolution Engine Tab */}
        {activeAITab === 'evolution' && (
          <div className="space-y-4">
            <div className="text-gray-400 text-sm">
              Evolution Engine controls will be available here. This requires integration with AIPortal state management.
            </div>
          </div>
        )}

        {/* Metrics & Logs Tab */}
        {activeAITab === 'metrics' && (
          <div className="space-y-4">
            <div className="text-gray-400 text-sm">
              Metrics & Logs will be available here. This requires integration with AIPortal state management.
            </div>
          </div>
        )}

        {/* Canvas Editor Tab */}
        {activeAITab === 'canvas' && (
          <div className="space-y-4">
            {/* View Mode Toggle */}
            <div className="flex gap-2 border-b border-gray-700 pb-2">
              <button
                onClick={() => setCanvasEditorView('editor')}
                className={`px-4 py-2 text-sm font-medium transition-colors ${
                  canvasEditorView === 'editor'
                    ? 'text-white border-b-2 border-purple-500'
                    : 'text-gray-400 hover:text-gray-300'
                }`}
              >
                Editor
              </button>
              <button
                onClick={() => setCanvasEditorView('canvasl-3d')}
                className={`px-4 py-2 text-sm font-medium transition-colors ${
                  canvasEditorView === 'canvasl-3d'
                    ? 'text-white border-b-2 border-purple-500'
                    : 'text-gray-400 hover:text-gray-300'
                }`}
              >
                CanvasL 3D
              </button>
            </div>

            {/* File Selection */}
            <div>
              <label className="block text-sm text-gray-400 mb-2">Select JSONL/CanvasL File</label>
              <select
                value={selectedJSONLFile}
                onChange={(e) => handleJSONLFileChange(e.target.value)}
                className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded-lg text-white"
              >
                <option value="automaton-kernel.jsonl">automaton-kernel.jsonl</option>
                <option value="generate.metaverse.jsonl">generate.metaverse.jsonl</option>
                <option value="automaton.jsonl">automaton.jsonl</option>
                <option value="automaton.canvas.space.jsonl">automaton.canvas.space.jsonl</option>
                <option value="automaton-kernel.canvasl">automaton-kernel.canvasl (CanvasL)</option>
                <option value="generate.metaverse.canvasl">generate.metaverse.canvasl (CanvasL)</option>
              </select>
            </div>

            {/* Editor View */}
            {canvasEditorView === 'editor' && (
              <div className="border border-gray-700 rounded-lg overflow-hidden" style={{ height: '600px' }}>
                <UnifiedEditor
                  filename={selectedJSONLFile}
                  initialMode="auto"
                  height="100%"
                  onSave={(content, format) => {
                    addEvolutionLog(`Saved canvas: ${selectedJSONLFile} (${format})`);
                  }}
                />
              </div>
            )}

            {/* CanvasL 3D View */}
            {canvasEditorView === 'canvasl-3d' && (
              <div className="border border-gray-700 rounded-lg overflow-hidden" style={{ height: '600px' }}>
                <MetaverseCanvas3D
                  filename={selectedJSONLFile}
                  onSave={(canvas3D) => {
                    addEvolutionLog(`Saved 3D canvas: ${selectedJSONLFile}`);
                  }}
                />
              </div>
            )}
          </div>
        )}

        {/* LLM Configuration Tab */}
        {activeAITab === 'llm-config' && (
          <div className="space-y-4">
            <div className="text-gray-400 text-sm">
              LLM Configuration will be available here. This requires integration with AIPortal state management.
            </div>
          </div>
        )}

        {/* Agent API Tab */}
        {activeAITab === 'agent-api' && (
          <div className="space-y-4 overflow-y-auto">
            <div className="space-y-4">
              <AgentList />
              <AgentExecution />
              <StatusDashboard />
              <MultiAgentCoordinator />
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

