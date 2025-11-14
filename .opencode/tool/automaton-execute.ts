import { tool } from "@opencode-ai/plugin"
import * as fs from "fs"
import * as path from "path"

/**
 * Automaton Execute Tool
 * 
 * Related Commands:
 * - /execute-operations - Execute specific automaton operations for targeted experimentation
 * - /run-experiments - Comprehensive experiment suite including operation testing
 * 
 * This tool is used by:
 * - execute-operations.md - Primary command for operation execution
 * - run-experiments.md - Part of comprehensive experiment suite
 */
export default tool({
  description: "I'm your execution specialist - I know how to run specific automaton operations on demand. Think of me as your precision operator who can trigger individual Church encoding operations, self-modifications, or dimensional transitions without running the whole continuous loop. Perfect when you want to test something specific or execute a targeted operation.",
  args: {
    operation: tool.schema.enum([
      "evolve", "self-reference", "self-modify", "self-io", "validate", "train", "observe", "compose",
      "regenerate", "goal-negotiate", "consensus", "autonomous-evolve",
      "bqf-encode", "polyhedra-transform", "compute-mapping"
    ]).describe("🎯 Operation type: 'evolve' advances dimensional progression, 'self-reference' triggers recursion, 'self-modify' mutates structure, 'self-io' performs I/O, 'validate' checks constraints, 'train' learns patterns, 'observe' analyzes state, 'compose' combines operations. Autonomous: 'regenerate' kernel from seed, 'goal-negotiate' with agents, 'consensus' vote, 'autonomous-evolve' with fitness. Geometric: 'bqf-encode' polyhedra, 'polyhedra-transform' using BQF, 'compute-mapping' R5RS types"),
    automatonFile: tool.schema.string().optional().describe("📁 Path to the automaton's self-referential JSONL data file (default: ./automaton.jsonl)"),
    saveAfter: tool.schema.boolean().optional().describe("💾 Persist state changes to disk after operation execution (default: true)")
  },
  async execute(args, context) {
    const { agent, sessionID } = context
    
    try {
      // Import the automaton dynamically to avoid circular dependencies
      const { AdvancedSelfReferencingAutomaton } = await import("../../advanced-automaton")
      const automaton = new AdvancedSelfReferencingAutomaton(
        args.automatonFile || "./automaton.jsonl"
      )
      
      const currentAutomaton = automaton.getCurrentAutomaton()
      if (!currentAutomaton) {
        return "❌ No current automaton state found"
      }
      
      let result = ""
      const beforeState = {
        dimension: (automaton as any).currentDimension,
        modifications: (automaton as any).selfModificationCount,
        state: currentAutomaton.currentState
      }
      
      switch (args.operation) {
        case "evolve":
          (automaton as any).executeEvolution()
          result = `🔄 Executed evolution from dimension ${beforeState.dimension}`
          break
          
        case "self-reference":
          (automaton as any).executeSelfReference()
          result = `🔗 Executed self-reference operation`
          break
          
        case "self-modify":
          (automaton as any).executeSelfModification()
          result = `🔧 Executed self-modification`
          break
          
        case "self-io":
          (automaton as any).executeSelfIO()
          result = `💾 Executed self-I/O operation`
          break
          
        case "validate":
          (automaton as any).executeSelfValidation()
          result = `✅ Executed self-validation`
          break
          
        case "train":
          (automaton as any).executeSelfTraining()
          result = `🧠 Executed self-training`
          break
          
        case "observe":
          (automaton as any).executeSelfObservation()
          result = `👁️ Executed self-observation`
          break
          
        case "compose":
          (automaton as any).executeComposition()
          result = `🎼 Executed composition`
          break
          
        case "regenerate":
          // Import autonomous operations tool
          const { regenerate: regenerateOp } = await import("../../plugin/meta-log-plugin/src/tools/autonomous-operations")
          const regenerateRequest = {
            operation: "regenerate" as const,
            protocol: "autonomous-canvasl" as const,
            version: "1.0.0" as const,
            source: args.source || "evolutions/automaton.kernel.seed.canvasl",
            target: args.target || "evolutions/automaton.kernel.canvasl",
            options: {
              validate: args.validate !== false,
              preserveProvenance: args.preserveProvenance !== false,
              createSnapshot: args.createSnapshot !== false
            },
            metadata: {
              timestamp: new Date().toISOString(),
              agent: agent || "OpenCode-Agent"
            }
          }
          const regenerateResult = await regenerateOp(regenerateRequest, (automaton as any).db || {})
          result = regenerateResult.success 
            ? `🔄 Regenerated kernel: ${regenerateResult.result?.lineCount} lines, ${regenerateResult.result?.nodeCount} nodes, ${regenerateResult.result?.edgeCount} edges`
            : `❌ Regeneration failed: ${regenerateResult.error?.message}`
          break
          
        case "goal-negotiate":
          const { goalNegotiate } = await import("../../plugin/meta-log-plugin/src/tools/autonomous-operations")
          const goalRequest = {
            operation: "goal-negotiation" as const,
            protocol: "autonomous-canvasl" as const,
            version: "1.0.0" as const,
            agents: args.agents || [],
            goals: args.goals || [],
            options: {
              algorithm: args.algorithm || "borda",
              timeout: args.timeout || 5000,
              requireConsensus: args.requireConsensus !== false
            },
            metadata: {
              timestamp: new Date().toISOString(),
              initiator: agent || "OpenCode-Agent"
            }
          }
          const goalResult = await goalNegotiate(goalRequest, (automaton as any).db || {})
          result = goalResult.success
            ? `🤝 Goal negotiation: ${goalResult.result?.negotiatedGoals.length} goals negotiated`
            : `❌ Goal negotiation failed: ${goalResult.error?.message}`
          break
          
        case "consensus":
          const { consensus: consensusOp } = await import("../../plugin/meta-log-plugin/src/tools/autonomous-operations")
          const consensusRequest = {
            operation: "consensus" as const,
            protocol: "autonomous-canvasl" as const,
            version: "1.0.0" as const,
            proposal: args.proposal || {},
            agents: args.agents || [],
            options: {
              threshold: args.threshold || 0.75,
              timeout: args.timeout || 10000,
              quorum: args.quorum
            },
            metadata: {
              timestamp: new Date().toISOString(),
              proposer: agent || "OpenCode-Agent"
            }
          }
          const consensusResult = await consensusOp(consensusRequest, (automaton as any).db || {})
          result = consensusResult.success
            ? `🗳️ Consensus: ${consensusResult.result?.status} (${consensusResult.result?.votes.for}/${consensusResult.result?.votes.total} for)`
            : `❌ Consensus failed: ${consensusResult.error?.message}`
          break
          
        case "autonomous-evolve":
          const { autonomousEvolve } = await import("../../plugin/meta-log-plugin/src/tools/autonomous-operations")
          const evolveRequest = {
            operation: "autonomous-evolution" as const,
            protocol: "autonomous-canvasl" as const,
            version: "1.0.0" as const,
            currentState: {
              file: args.currentStateFile || args.automatonFile || "./automaton.jsonl",
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
              selectionStrategy: args.selectionStrategy || "fitness-proportional",
              validateEachStep: args.validateEachStep !== false
            },
            metadata: {
              timestamp: new Date().toISOString(),
              agent: agent || "OpenCode-Agent"
            }
          }
          const evolveResult = await autonomousEvolve(evolveRequest, (automaton as any).db || {})
          result = evolveResult.success
            ? `🧬 Evolved: ${evolveResult.result?.iterations} iterations, fitness ${evolveResult.result?.finalFitness.toFixed(3)}`
            : `❌ Evolution failed: ${evolveResult.error?.message}`
          break
          
        case "bqf-encode":
          const { bqfEncode } = await import("../../plugin/meta-log-plugin/src/tools/geometric-operations")
          const bqfResult = bqfEncode({
            polyhedron: args.polyhedron || "tetrahedron",
            includeForm: args.includeForm !== false,
            includeSignature: args.includeSignature !== false
          })
          result = bqfResult.success
            ? `📐 BQF: ${bqfResult.result?.bqf.form} (${bqfResult.result?.vertices}V, ${bqfResult.result?.edges}E, ${bqfResult.result?.faces}F)`
            : `❌ BQF encoding failed: ${bqfResult.error?.message}`
          break
          
        case "polyhedra-transform":
          const { polyhedraTransform } = await import("../../plugin/meta-log-plugin/src/tools/geometric-operations")
          let transformInput: any = args.input
          try {
            transformInput = JSON.parse(transformInput)
          } catch {
            // Assume it's a polyhedron name
          }
          const transformResult = polyhedraTransform({
            operation: args.transformOperation || "dual-swap",
            input: transformInput,
            options: {
              preserveStructure: args.preserveStructure || false
            }
          })
          result = transformResult.success
            ? `🔄 Transform: ${transformResult.result?.transformation}`
            : `❌ Transformation failed: ${transformResult.error?.message}`
          break
          
        case "compute-mapping":
          const { computeMapping } = await import("../../plugin/meta-log-plugin/src/tools/geometric-operations")
          const mappingResult = computeMapping({
            r5rsType: args.r5rsType || "pair",
            includeDimension: args.includeDimension !== false,
            includeBQF: args.includeBQF !== false
          })
          result = mappingResult.success
            ? `🗺️ Mapping: ${mappingResult.result?.r5rsType} → ${mappingResult.result?.dimension} → ${mappingResult.result?.polyhedron}`
            : `❌ Mapping failed: ${mappingResult.error?.message}`
          break
          
        default:
          return "❌ Unknown operation"
      }
      
      const afterState = {
        dimension: (automaton as any).currentDimension,
        modifications: (automaton as any).selfModificationCount,
        state: (automaton.getCurrentAutomaton())?.currentState
      }
      
      if (args.saveAfter !== false) {
        (automaton as any).save()
        result += " (💾 Saved)"
      }
      
      return {
        operation: args.operation,
        result,
        before: beforeState,
        after: afterState,
        changes: {
          dimensionChanged: beforeState.dimension !== afterState.dimension,
          modificationsAdded: afterState.modifications - beforeState.modifications,
          stateChanged: beforeState.state !== afterState.state
        },
        agent,
        sessionID
      }
      
    } catch (error) {
      return `❌ Error executing ${args.operation}: ${error instanceof Error ? error.message : String(error)}`
    }
  }
})