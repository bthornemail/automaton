import React, { useState, useEffect, useRef } from 'react';
import { Code, Search, Play, Settings, Loader2, CheckCircle, AlertCircle } from 'lucide-react';
import { opencodeApi } from '../../services/api';
import { EditorView } from '@codemirror/view';
import { EditorState } from '@codemirror/state';
import { javascript } from '@codemirror/lang-javascript';
import { oneDark } from '@codemirror/theme-one-dark';
import { defaultKeymap } from '@codemirror/commands';
import { keymap } from '@codemirror/view';

interface AnalysisResult {
  patterns: string[];
  suggestions: string[];
  codeQuality: number;
  complexity: number;
  recommendations: string[];
}

interface Agent {
  name: string;
  description: string;
  endpoint: string;
}

interface AgentTask {
  agent: string;
  task: string;
  response: string;
  timestamp: number;
  model?: string;
  fallback?: boolean;
}

const OpenCodeInterface: React.FC = () => {
  const [isConnected, setIsConnected] = useState(false);
  const [isAnalyzing, setIsAnalyzing] = useState(false);
  const [code, setCode] = useState(`// Welcome to OpenCode Editor with AI Agents
// Start typing your code here...

function fibonacci(n) {
  if (n <= 1) return n;
  return fibonacci(n - 1) + fibonacci(n - 2);
}

console.log(fibonacci(10));`);
  const [analysis, setAnalysis] = useState<AnalysisResult | null>(null);
  const [searchQuery, setSearchQuery] = useState('');
  const [availableAgents, setAvailableAgents] = useState<Agent[]>([]);
  const [availableModels, setAvailableModels] = useState<string[]>([]);
  const [currentModel, setCurrentModel] = useState<string>('llama2');
  const [selectedAgent, setSelectedAgent] = useState<string>('');
  const [agentTask, setAgentTask] = useState<string>('');
  const [agentResponses, setAgentResponses] = useState<AgentTask[]>([]);
  const [isExecutingAgent, setIsExecutingAgent] = useState(false);
  const [activeTab, setActiveTab] = useState<'analysis' | 'agents'>('analysis');
  const editorRef = useRef<HTMLDivElement>(null);
  const viewRef = useRef<EditorView | null>(null);

  useEffect(() => {
    checkStatus();
    loadAgents();
    loadModels();
  }, []);

  useEffect(() => {
    if (editorRef.current && !viewRef.current) {
      const startState = EditorState.create({
        doc: code,
        extensions: [
          javascript(),
          oneDark,
          keymap.of(defaultKeymap),
          EditorView.updateListener.of((update) => {
            if (update.docChanged) {
              setCode(update.state.doc.toString());
            }
          }),
          EditorView.theme({
            '&': {
              height: '100%',
              fontSize: '14px'
            },
            '.cm-scroller': {
              overflow: 'auto'
            },
            '.cm-content': {
              padding: '12px'
            }
          })
        ]
      });

      viewRef.current = new EditorView({
        state: startState,
        parent: editorRef.current
      });
    }

    return () => {
      if (viewRef.current) {
        viewRef.current.destroy();
        viewRef.current = null;
      }
    };
  }, []);

  const checkStatus = async () => {
    try {
      const response = await opencodeApi.getStatus();
      setIsConnected(response.data.connected);
      setAvailableAgents(response.data.availableAgents || []);
      setAvailableModels(response.data.availableModels || []);
    } catch (error) {
      console.error('Failed to check status:', error);
      setIsConnected(false);
    }
  };

  const loadAgents = async () => {
    try {
      const response = await opencodeApi.getAgents();
      setAvailableAgents(response.data as Agent[] || []);
    } catch (error) {
      console.error('Failed to load agents:', error);
    }
  };

  const loadModels = async () => {
    try {
      const response = await opencodeApi.getModels();
      setAvailableModels(response.data as string[] || []);
    } catch (error) {
      console.error('Failed to load models:', error);
    }
  };

  const analyzeCode = async () => {
    if (!code.trim()) return;
    
    setIsAnalyzing(true);
    try {
      const response = await opencodeApi.analyzeCode(code);
      setAnalysis(response.data as AnalysisResult);
    } catch (error) {
      console.error('Analysis failed:', error);
    } finally {
      setIsAnalyzing(false);
    }
  };

  const runCode = () => {
    console.log('Running code:', code);
  };

  const executeAgentTask = async () => {
    if (!selectedAgent || !agentTask.trim()) return;
    
    setIsExecutingAgent(true);
    try {
      const response = await opencodeApi.executeAgent(selectedAgent, agentTask);
      const responseData = response.data as any;
      const agentResponse: AgentTask = {
        agent: selectedAgent,
        task: agentTask,
        response: responseData.response || 'No response',
        timestamp: responseData.timestamp || Date.now(),
        model: responseData.model,
        fallback: responseData.fallback
      };
      
      setAgentResponses(prev => [agentResponse, ...prev.slice(0, 4)]); // Keep last 5 responses
    } catch (error) {
      console.error('Agent execution failed:', error);
    } finally {
      setIsExecutingAgent(false);
    }
  };

  const switchModel = async (model: string) => {
    try {
      await opencodeApi.setModel(model);
      setCurrentModel(model);
    } catch (error) {
      console.error('Failed to switch model:', error);
    }
  };

  return (
    <div className="flex flex-col h-full bg-gray-900 text-white">
      {/* Header */}
      <div className="flex items-center justify-between p-4 border-b border-gray-700">
        <div className="flex items-center space-x-3">
          <Code className="w-5 h-5 text-blue-400" />
          <h2 className="text-lg font-semibold">OpenCode Editor</h2>
          <div className={`flex items-center space-x-1 px-2 py-1 rounded-full text-xs ${
            isConnected ? 'bg-green-900 text-green-300' : 'bg-red-900 text-red-300'
          }`}>
            {isConnected ? (
              <>
                <CheckCircle className="w-3 h-3" />
                <span>Connected</span>
              </>
            ) : (
              <>
                <AlertCircle className="w-3 h-3" />
                <span>Disconnected</span>
              </>
            )}
          </div>
        </div>
        
        <div className="flex items-center space-x-2">
          <button
            onClick={analyzeCode}
            disabled={isAnalyzing || !code.trim()}
            className="flex items-center space-x-2 px-3 py-1 bg-blue-600 hover:bg-blue-700 disabled:bg-gray-600 disabled:cursor-not-allowed rounded-lg transition-colors"
          >
            {isAnalyzing ? (
              <Loader2 className="w-4 h-4 animate-spin" />
            ) : (
              <Search className="w-4 h-4" />
            )}
            <span>Analyze</span>
          </button>
          
          <button
            onClick={runCode}
            className="flex items-center space-x-2 px-3 py-1 bg-green-600 hover:bg-green-700 rounded-lg transition-colors"
          >
            <Play className="w-4 h-4" />
            <span>Run</span>
          </button>
          
          <button className="p-1 hover:bg-gray-700 rounded transition-colors">
            <Settings className="w-4 h-4" />
          </button>
        </div>
      </div>

      {/* Main Content */}
      <div className="flex-1 flex overflow-hidden">
        {/* Code Editor */}
        <div className="flex-1 flex flex-col">
          <div className="flex items-center justify-between px-4 py-2 bg-gray-800 border-b border-gray-700">
            <span className="text-sm text-gray-400">JavaScript Editor</span>
            <span className="text-xs text-gray-500">CodeMirror 6</span>
          </div>
          <div className="flex-1 overflow-hidden" ref={editorRef} />
        </div>

        {/* Right Panel */}
        <div className="w-96 bg-gray-800 border-l border-gray-700 flex flex-col">
          {/* Tab Navigation */}
          <div className="flex border-b border-gray-700">
            <button
              onClick={() => setActiveTab('analysis')}
              className={`flex-1 px-4 py-2 text-sm font-medium transition-colors ${
                activeTab === 'analysis' 
                  ? 'bg-gray-700 text-blue-400' 
                  : 'text-gray-400 hover:text-white hover:bg-gray-700'
              }`}
            >
              Analysis
            </button>
            <button
              onClick={() => setActiveTab('agents')}
              className={`flex-1 px-4 py-2 text-sm font-medium transition-colors ${
                activeTab === 'agents' 
                  ? 'bg-gray-700 text-blue-400' 
                  : 'text-gray-400 hover:text-white hover:bg-gray-700'
              }`}
            >
              AI Agents
            </button>
          </div>

          {/* Analysis Panel */}
          {activeTab === 'analysis' && analysis && (
            <div className="flex-1 overflow-y-auto p-4">
              <h3 className="text-lg font-semibold mb-4 text-blue-400">Analysis Results</h3>
              
              {/* Code Quality */}
              <div className="mb-6">
                <div className="flex justify-between items-center mb-2">
                  <span className="text-sm font-medium">Code Quality</span>
                  <span className="text-sm text-gray-400">{analysis.codeQuality}%</span>
                </div>
                <div className="w-full bg-gray-700 rounded-full h-2">
                  <div 
                    className={`h-2 rounded-full transition-all ${
                      analysis.codeQuality >= 80 ? 'bg-green-500' :
                      analysis.codeQuality >= 60 ? 'bg-yellow-500' : 'bg-red-500'
                    }`}
                    style={{ width: `${analysis.codeQuality}%` }}
                  />
                </div>
              </div>

              {/* Complexity */}
              <div className="mb-6">
                <div className="flex justify-between items-center mb-2">
                  <span className="text-sm font-medium">Complexity</span>
                  <span className="text-sm text-gray-400">{analysis.complexity}</span>
                </div>
                <div className="w-full bg-gray-700 rounded-full h-2">
                  <div 
                    className={`h-2 rounded-full transition-all ${
                      analysis.complexity <= 3 ? 'bg-green-500' :
                      analysis.complexity <= 6 ? 'bg-yellow-500' : 'bg-red-500'
                    }`}
                    style={{ width: `${Math.min(analysis.complexity * 10, 100)}%` }}
                  />
                </div>
              </div>

              {/* Patterns */}
              <div className="mb-6">
                <h4 className="text-sm font-semibold mb-2 text-green-400">Detected Patterns</h4>
                <ul className="space-y-1">
                  {analysis.patterns.map((pattern, index) => (
                    <li key={index} className="text-xs text-gray-300 flex items-start">
                      <span className="text-green-400 mr-2">•</span>
                      {pattern}
                    </li>
                  ))}
                </ul>
              </div>

              {/* Suggestions */}
              <div className="mb-6">
                <h4 className="text-sm font-semibold mb-2 text-yellow-400">Suggestions</h4>
                <ul className="space-y-1">
                  {analysis.suggestions.map((suggestion, index) => (
                    <li key={index} className="text-xs text-gray-300 flex items-start">
                      <span className="text-yellow-400 mr-2">•</span>
                      {suggestion}
                    </li>
                  ))}
                </ul>
              </div>

              {/* Recommendations */}
              <div>
                <h4 className="text-sm font-semibold mb-2 text-blue-400">Recommendations</h4>
                <ul className="space-y-1">
                  {analysis.recommendations.map((recommendation, index) => (
                    <li key={index} className="text-xs text-gray-300 flex items-start">
                      <span className="text-blue-400 mr-2">•</span>
                      {recommendation}
                    </li>
                  ))}
                </ul>
              </div>
            </div>
          )}

          {/* AI Agents Panel */}
          {activeTab === 'agents' && (
            <div className="flex-1 overflow-y-auto p-4">
              {/* Model Selection */}
              <div className="mb-6">
                <h4 className="text-sm font-semibold mb-2 text-green-400">AI Model</h4>
                <select
                  value={currentModel}
                  onChange={(e) => switchModel(e.target.value)}
                  className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded-lg text-white focus:outline-none focus:border-blue-500"
                >
                  {availableModels.map(model => (
                    <option key={model} value={model}>{model}</option>
                  ))}
                </select>
              </div>

              {/* Agent Selection */}
              <div className="mb-6">
                <h4 className="text-sm font-semibold mb-2 text-purple-400">Available Agents</h4>
                <div className="space-y-2">
                  {availableAgents.map(agent => (
                    <div
                      key={agent.name}
                      onClick={() => setSelectedAgent(agent.name)}
                      className={`p-3 rounded-lg cursor-pointer transition-colors ${
                        selectedAgent === agent.name
                          ? 'bg-blue-600 border border-blue-500'
                          : 'bg-gray-700 border border-gray-600 hover:bg-gray-600'
                      }`}
                    >
                      <div className="font-medium text-white">{agent.name}</div>
                      <div className="text-xs text-gray-300 mt-1">{agent.description}</div>
                    </div>
                  ))}
                </div>
              </div>

              {/* Agent Task Input */}
              <div className="mb-6">
                <h4 className="text-sm font-semibold mb-2 text-yellow-400">Agent Task</h4>
                <textarea
                  value={agentTask}
                  onChange={(e) => setAgentTask(e.target.value)}
                  placeholder="Describe what you want the agent to do..."
                  className="w-full px-3 py-2 bg-gray-700 border border-gray-600 rounded-lg text-white placeholder-gray-400 focus:outline-none focus:border-blue-500 resize-none"
                  rows={3}
                />
                <button
                  onClick={executeAgentTask}
                  disabled={!selectedAgent || !agentTask.trim() || isExecutingAgent}
                  className="mt-2 w-full flex items-center justify-center space-x-2 px-4 py-2 bg-purple-600 hover:bg-purple-700 disabled:bg-gray-600 disabled:cursor-not-allowed rounded-lg transition-colors"
                >
                  {isExecutingAgent ? (
                    <Loader2 className="w-4 h-4 animate-spin" />
                  ) : (
                    <Code className="w-4 h-4" />
                  )}
                  <span>Execute Agent Task</span>
                </button>
              </div>

              {/* Agent Responses */}
              {agentResponses.length > 0 && (
                <div>
                  <h4 className="text-sm font-semibold mb-2 text-cyan-400">Recent Responses</h4>
                  <div className="space-y-3">
                    {agentResponses.map((response, index) => (
                      <div key={index} className="p-3 bg-gray-700 rounded-lg border border-gray-600">
                        <div className="flex items-center justify-between mb-2">
                          <span className="text-xs font-medium text-blue-300">{response.agent}</span>
                          <span className="text-xs text-gray-400">
                            {new Date(response.timestamp).toLocaleTimeString()}
                          </span>
                        </div>
                        <div className="text-xs text-gray-300 mb-1">Task: {response.task}</div>
                        <div className="text-sm text-white bg-gray-800 p-2 rounded border border-gray-600">
                          {response.response}
                        </div>
                        {response.model && (
                          <div className="text-xs text-gray-400 mt-1">Model: {response.model}</div>
                        )}
                        {response.fallback && (
                          <div className="text-xs text-yellow-400 mt-1">⚠️ Fallback response</div>
                        )}
                      </div>
                    ))}
                  </div>
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    </div>
  );
};

export default OpenCodeInterface;