import { BaseMetaLogPlugin, PluginConfig } from '../core/plugin.js';

/**
 * OpenCode adapter for Meta-Log plugin
 */
export class OpenCodeMetaLogPlugin extends BaseMetaLogPlugin {
  private tools: any[] = [];
  private opencodeAvailable: boolean = false;

  constructor(config: PluginConfig) {
    super(config);
  }

  /**
   * Check if OpenCode plugin API is available
   */
  async checkOpenCodeAvailability(): Promise<boolean> {
    if (this.opencodeAvailable) {
      return true;
    }

    try {
      const module = await import('@opencode-ai/plugin');
      if (module && typeof module.tool === 'function') {
        this.opencodeAvailable = true;
        return true;
      }
    } catch (error) {
      // Module not available or import failed
      this.opencodeAvailable = false;
      return false;
    }

    return false;
  }

  /**
   * Load plugin - register OpenCode tools
   */
  async onLoad(): Promise<void> {
    this.setEnabled(false);
    
    // Load canvas if path provided
    if (this.config.canvasPath) {
      try {
        await this.loadCanvas(this.config.canvasPath);
      } catch (error) {
        console.warn('Failed to load canvas:', error);
        // Continue without canvas - plugin can still function
      }
    }

    // Register OpenCode tools if available
    const isAvailable = await this.checkOpenCodeAvailability();
    
    if (!isAvailable) {
      console.info('OpenCode plugin API not available - tools will not be registered. Install @opencode-ai/plugin as a peer dependency to enable tool registration.');
      return;
    }

    try {
      const { tool } = await import('@opencode-ai/plugin');
      
      // ProLog query tool
      const prologTool = tool({
        description: "Query Meta-Log database with ProLog",
        args: {
          query: tool.schema.string().describe("ProLog query string")
        },
        execute: async (args: any, context: any) => {
          try {
            const query = await this.beforeQuery(args.query);
            const results = await this.db.prologQuery(query);
            return await this.afterQuery(query, results);
          } catch (error) {
            return { error: String(error), query: args.query };
          }
        }
      });
      this.tools.push(prologTool);

      // DataLog query tool
      const datalogTool = tool({
        description: "Query Meta-Log database with DataLog",
        args: {
          query: tool.schema.string().describe("DataLog query string"),
          program: tool.schema.string().optional().describe("DataLog program (optional)")
        },
        execute: async (args: any, context: any) => {
          try {
            const query = await this.beforeQuery(args.query);
            let program;
            if (args.program) {
              try {
                program = typeof args.program === 'string' ? JSON.parse(args.program) : args.program;
              } catch (parseError) {
                return { error: `Invalid program JSON: ${parseError}`, query: args.query };
              }
            }
            const results = await this.db.datalogQuery(query, program);
            return await this.afterQuery(query, results);
          } catch (error) {
            return { error: String(error), query: args.query };
          }
        }
      });
      this.tools.push(datalogTool);

      // SPARQL query tool
      const sparqlTool = tool({
        description: "Query Meta-Log database with SPARQL",
        args: {
          query: tool.schema.string().describe("SPARQL query string")
        },
        execute: async (args: any, context: any) => {
          try {
            const query = await this.beforeQuery(args.query);
            const results = await this.db.sparqlQuery(query);
            return await this.afterQuery(query, results);
          } catch (error) {
            return { error: String(error), query: args.query };
          }
        }
      });
      this.tools.push(sparqlTool);

      // Load canvas tool
      const loadCanvasTool = tool({
        description: "Load JSONL/CanvasL canvas file",
        args: {
          path: tool.schema.string().describe("Path to canvas file")
        },
        execute: async (args: any, context: any) => {
          try {
            await this.loadCanvas(args.path);
            return { success: true, path: args.path };
          } catch (error) {
            return { success: false, error: String(error), path: args.path };
          }
        }
      });
      this.tools.push(loadCanvasTool);

      // Autonomous operations tools
      const { regenerate, selfModify, goalNegotiate, consensus, autonomousEvolve } = await import('../tools/autonomous-operations.js');
      
      const regenerateTool = tool({
        description: "Regenerate kernel from seed file (Autonomous CanvasL protocol)",
        args: {
          source: tool.schema.string().describe("Path to kernel seed file (e.g., automaton.kernel.seed.canvasl)"),
          target: tool.schema.string().describe("Path to target kernel file (e.g., automaton.kernel.canvasl)"),
          validate: tool.schema.boolean().optional().describe("Validate regenerated kernel (default: true)"),
          preserveProvenance: tool.schema.boolean().optional().describe("Preserve provenance history (default: true)"),
          createSnapshot: tool.schema.boolean().optional().describe("Create snapshot before regeneration (default: true)")
        },
        execute: async (args: any, context: any) => {
          try {
            const request = {
              operation: 'regenerate' as const,
              protocol: 'autonomous-canvasl' as const,
              version: '1.0.0' as const,
              source: args.source,
              target: args.target,
              options: {
                validate: args.validate !== false,
                preserveProvenance: args.preserveProvenance !== false,
                createSnapshot: args.createSnapshot !== false
              },
              metadata: {
                timestamp: new Date().toISOString(),
                agent: context.agent || 'OpenCode-Agent'
              }
            };
            return await regenerate(request, this.db);
          } catch (error) {
            return {
              success: false,
              error: {
                code: 'REGENERATION_ERROR',
                message: error instanceof Error ? error.message : String(error)
              }
            };
          }
        }
      });
      this.tools.push(regenerateTool);

      const selfModifyTool = tool({
        description: "Self-modify CanvasL file with snapshot/rollback (Autonomous CanvasL protocol)",
        args: {
          targetFile: tool.schema.string().describe("Path to target file to modify"),
          modificationType: (tool.schema as any).enum(['add-node', 'modify-node', 'add-edge', 'modify-edge', 'update-metadata']).describe("Type of modification"),
          nodeId: tool.schema.string().optional().describe("Node/edge ID to modify (required for modify operations)"),
          data: tool.schema.string().describe("JSON string of modification data"),
          createSnapshot: tool.schema.boolean().optional().describe("Create snapshot before modification (default: true)"),
          validateBefore: tool.schema.boolean().optional().describe("Validate before modification (default: true)"),
          validateAfter: tool.schema.boolean().optional().describe("Validate after modification (default: true)"),
          rollbackOnFailure: tool.schema.boolean().optional().describe("Rollback on failure (default: true)")
        },
        execute: async (args: any, context: any) => {
          try {
            const data = typeof args.data === 'string' ? JSON.parse(args.data) : args.data;
            const request = {
              operation: 'self-modify' as const,
              protocol: 'autonomous-canvasl' as const,
              version: '1.0.0' as const,
              targetFile: args.targetFile,
              modification: {
                type: args.modificationType,
                nodeId: args.nodeId || null,
                data,
                pattern: args.pattern
              },
              options: {
                createSnapshot: args.createSnapshot !== false,
                validateBefore: args.validateBefore !== false,
                validateAfter: args.validateAfter !== false,
                rollbackOnFailure: args.rollbackOnFailure !== false
              },
              metadata: {
                timestamp: new Date().toISOString(),
                agent: context.agent || 'OpenCode-Agent',
                reason: args.reason
              }
            };
            return await selfModify(request, this.db);
          } catch (error) {
            return {
              success: false,
              error: {
                code: 'MODIFICATION_ERROR',
                message: error instanceof Error ? error.message : String(error)
              }
            };
          }
        }
      });
      this.tools.push(selfModifyTool);

      const goalNegotiateTool = tool({
        description: "Negotiate goals with multi-agent system (Autonomous CanvasL protocol)",
        args: {
          agents: tool.schema.string().describe("JSON array of agent IDs with optional weights"),
          goals: tool.schema.string().describe("JSON array of goals with id, description, priority, constraints"),
          algorithm: (tool.schema as any).enum(['borda', 'grover', 'consensus']).optional().describe("Negotiation algorithm (default: borda)"),
          timeout: tool.schema.number().optional().describe("Timeout in milliseconds (default: 5000)"),
          requireConsensus: tool.schema.boolean().optional().describe("Require consensus (default: true)")
        },
        execute: async (args: any, context: any) => {
          try {
            const agents = typeof args.agents === 'string' ? JSON.parse(args.agents) : args.agents;
            const goals = typeof args.goals === 'string' ? JSON.parse(args.goals) : args.goals;
            const request = {
              operation: 'goal-negotiation' as const,
              protocol: 'autonomous-canvasl' as const,
              version: '1.0.0' as const,
              agents,
              goals,
              options: {
                algorithm: args.algorithm || 'borda',
                timeout: args.timeout || 5000,
                requireConsensus: args.requireConsensus !== false
              },
              metadata: {
                timestamp: new Date().toISOString(),
                initiator: context.agent || 'OpenCode-Agent'
              }
            };
            return await goalNegotiate(request, this.db);
          } catch (error) {
            return {
              success: false,
              error: {
                code: 'NEGOTIATION_ERROR',
                message: error instanceof Error ? error.message : String(error)
              }
            };
          }
        }
      });
      this.tools.push(goalNegotiateTool);

      const consensusTool = tool({
        description: "Consensus vote on proposal (Autonomous CanvasL protocol)",
        args: {
          proposal: tool.schema.string().describe("JSON proposal object with id, type, description, target, changes"),
          agents: tool.schema.string().describe("JSON array of agent IDs with optional required flag"),
          threshold: tool.schema.number().optional().describe("Consensus threshold (0-1, default: 0.75)"),
          timeout: tool.schema.number().optional().describe("Timeout in milliseconds (default: 10000)"),
          quorum: tool.schema.number().optional().describe("Quorum count (default: all agents)")
        },
        execute: async (args: any, context: any) => {
          try {
            const proposal = typeof args.proposal === 'string' ? JSON.parse(args.proposal) : args.proposal;
            const agents = typeof args.agents === 'string' ? JSON.parse(args.agents) : args.agents;
            const request = {
              operation: 'consensus' as const,
              protocol: 'autonomous-canvasl' as const,
              version: '1.0.0' as const,
              proposal,
              agents,
              options: {
                threshold: args.threshold || 0.75,
                timeout: args.timeout || 10000,
                quorum: args.quorum || agents.length
              },
              metadata: {
                timestamp: new Date().toISOString(),
                proposer: context.agent || 'OpenCode-Agent'
              }
            };
            return await consensus(request, this.db);
          } catch (error) {
            return {
              success: false,
              error: {
                code: 'CONSENSUS_ERROR',
                message: error instanceof Error ? error.message : String(error)
              }
            };
          }
        }
      });
      this.tools.push(consensusTool);

      const autonomousEvolveTool = tool({
        description: "Autonomous evolution with fitness evaluation (Autonomous CanvasL protocol)",
        args: {
          currentStateFile: tool.schema.string().describe("Path to current state file"),
          goalFitness: tool.schema.number().optional().describe("Target fitness (0-1)"),
          maxIterations: tool.schema.number().optional().describe("Maximum iterations (default: 100)"),
          mutationRate: tool.schema.number().optional().describe("Mutation rate (0-1, default: 0.1)"),
          selectionStrategy: tool.schema.string().optional().describe("Selection strategy (default: fitness-proportional)"),
          validateEachStep: tool.schema.boolean().optional().describe("Validate each step (default: true)")
        },
        execute: async (args: any, context: any) => {
          try {
            const request = {
              operation: 'autonomous-evolution' as const,
              protocol: 'autonomous-canvasl' as const,
              version: '1.0.0' as const,
              currentState: {
                file: args.currentStateFile,
                fitness: args.currentFitness,
                metrics: args.metrics
              },
              goalState: args.goalFitness ? {
                fitness: args.goalFitness,
                targetMetrics: args.targetMetrics
              } : undefined,
              options: {
                maxIterations: args.maxIterations || 100,
                mutationRate: args.mutationRate || 0.1,
                selectionStrategy: args.selectionStrategy || 'fitness-proportional',
                validateEachStep: args.validateEachStep !== false
              },
              metadata: {
                timestamp: new Date().toISOString(),
                agent: context.agent || 'OpenCode-Agent'
              }
            };
            return await autonomousEvolve(request, this.db);
          } catch (error) {
            return {
              success: false,
              error: {
                code: 'EVOLUTION_ERROR',
                message: error instanceof Error ? error.message : String(error)
              }
            };
          }
        }
      });
      this.tools.push(autonomousEvolveTool);

      // Geometric operations tools
      const { bqfEncode, polyhedraTransform, computeMapping, geometricValidate } = await import('../tools/geometric-operations.js');
      
      const bqfEncodeTool = tool({
        description: "Encode polyhedron as BQF (Binary Quadratic Form) - docs/32-Regulay-Polyhedra-Geometry",
        args: {
          polyhedron: (tool.schema as any).enum(['tetrahedron', 'cube', 'octahedron', 'icosahedron', 'dodecahedron']).describe("Polyhedron name"),
          includeForm: tool.schema.boolean().optional().describe("Include BQF form string (default: true)"),
          includeSignature: tool.schema.boolean().optional().describe("Include signature (default: true)")
        },
        execute: async (args: any) => {
          return bqfEncode({
            polyhedron: args.polyhedron,
            includeForm: args.includeForm !== false,
            includeSignature: args.includeSignature !== false
          });
        }
      });
      this.tools.push(bqfEncodeTool);

      const polyhedraTransformTool = tool({
        description: "Transform polyhedra using BQF operations - docs/32-Regulay-Polyhedra-Geometry",
        args: {
          operation: (tool.schema as any).enum(['dual-swap', 'apply-bqf', 'abstract-bqf']).describe("Transformation operation"),
          input: tool.schema.string().describe("BQF coefficients as JSON array [a,b,c] or polyhedron name"),
          preserveStructure: tool.schema.boolean().optional().describe("Preserve structure (default: false)")
        },
        execute: async (args: any) => {
          let input: any;
          try {
            input = JSON.parse(args.input);
          } catch {
            input = args.input; // Assume it's a polyhedron name
          }
          return polyhedraTransform({
            operation: args.operation,
            input,
            options: {
              preserveStructure: args.preserveStructure || false
            }
          });
        }
      });
      this.tools.push(polyhedraTransformTool);

      const computeMappingTool = tool({
        description: "Compute R5RS type to polyhedra mapping - docs/32-Regulay-Polyhedra-Geometry/04-COMPUTATIONAL-MAPPING.md",
        args: {
          r5rsType: (tool.schema as any).enum(['boolean', 'pair', 'symbol', 'number', 'char', 'string', 'vector', 'procedure']).describe("R5RS type"),
          includeDimension: tool.schema.boolean().optional().describe("Include dimension (default: true)"),
          includeBQF: tool.schema.boolean().optional().describe("Include BQF encoding (default: true)")
        },
        execute: async (args: any) => {
          return computeMapping({
            r5rsType: args.r5rsType,
            includeDimension: args.includeDimension !== false,
            includeBQF: args.includeBQF !== false
          });
        }
      });
      this.tools.push(computeMappingTool);

      const geometricValidateTool = tool({
        description: "Validate geometric structure - docs/32-Regulay-Polyhedra-Geometry",
        args: {
          structureType: (tool.schema as any).enum(['polyhedron', 'bqf', 'mapping']).describe("Structure type"),
          structureData: tool.schema.string().describe("JSON structure data"),
          validateBQF: tool.schema.boolean().optional().describe("Validate BQF (default: false)"),
          validateDimensional: tool.schema.boolean().optional().describe("Validate dimensional progression (default: false)"),
          validateBipartite: tool.schema.boolean().optional().describe("Validate bipartite structure (default: false)")
        },
        execute: async (args: any) => {
          const structureData = typeof args.structureData === 'string' ? JSON.parse(args.structureData) : args.structureData;
          return geometricValidate({
            structure: {
              type: args.structureType,
              data: structureData
            },
            constraints: {
              validateBQF: args.validateBQF || false,
              validateDimensional: args.validateDimensional || false,
              validateBipartite: args.validateBipartite || false
            }
          });
        }
      });
      this.tools.push(geometricValidateTool);

      console.info(`Registered ${this.tools.length} OpenCode tools`);
    } catch (error) {
      // OpenCode plugin API not available or error during tool registration
      const errorMessage = error instanceof Error ? error.message : String(error);
      console.warn('Failed to register OpenCode tools:', errorMessage);
      this.opencodeAvailable = false;
    }
  }

  /**
   * Unload plugin - cleanup tools
   */
  async onUnload(): Promise<void> {
    this.tools = [];
    this.setEnabled(false);
  }

  /**
   * Enable plugin functionality
   */
  async onEnable(): Promise<void> {
    this.setEnabled(true);
    this.emit('enabled');
  }

  /**
   * Disable plugin functionality
   */
  async onDisable(): Promise<void> {
    this.setEnabled(false);
    this.emit('disabled');
  }

  /**
   * Get registered tools
   */
  getTools(): any[] {
    return this.tools;
  }

  /**
   * Check if OpenCode is available
   */
  isOpenCodeAvailable(): boolean {
    return this.opencodeAvailable;
  }

  /**
   * Get tool count
   */
  getToolCount(): number {
    return this.tools.length;
  }
}
