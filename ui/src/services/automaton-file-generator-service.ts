/**
 * Automaton File Generator Service
 * 
 * Generates standard automaton CanvasL files:
 * - automaton.kernel.canvasl: Core automaton structure
 * - automaton.seed.canvasl: Versioning and regeneration
 * - metaverse.topology.canvasl: Topology partition (Bipartite-BQF left side)
 * - metaverse.system.canvasl: System partition (Bipartite-BQF right side)
 */

import { validateAutomatonState } from '../utils/error-handling';
import { FileSystemError, ValidationError } from '../utils/error-types';
import { errorLoggingService } from './error-logging-service';
import { writeToTempFile, moveTempFile } from '../utils/error-handling';
import { databaseService } from './database-service';

export interface AutomatonState {
  id: string;
  dimension: string;
  topology: any[];
  system: any[];
  kernel: any[];
  seed: any[];
  [key: string]: any;
}

export interface BQFCoefficients {
  a: number;
  b: number;
  c: number;
}

export class AutomatonFileGeneratorService {
  private tempFiles: Map<string, string> = new Map(); // Track temp files: finalPath -> tempPath

  /**
   * Generate automaton.kernel.canvasl from automaton state.
   * 
   * Creates a CanvasL file containing the core automaton structure (kernel entries).
   * The file includes version and schema directives, and all kernel entries from
   * the automaton state with their dimension metadata.
   * 
   * @param {AutomatonState} state - Automaton state containing kernel entries
   * @returns {string} CanvasL file content as a string
   * @throws {ValidationError} If automaton state is invalid
   * 
   * @example
   * ```typescript
   * const service = new AutomatonFileGeneratorService();
   * const kernelContent = service.generateKernelCanvasL(automatonState);
   * // kernelContent contains CanvasL format with kernel entries
   * ```
   */
  generateKernelCanvasL(state: AutomatonState): string {
    // Validate state before generation
    validateAutomatonState(state);
    const lines: string[] = [];
    
    // Add version directive
    lines.push('@version 1.0.0');
    lines.push('@schema automaton-kernel');
    lines.push('');
    
    // Add kernel entries
    for (const entry of state.kernel) {
      lines.push(JSON.stringify({
        ...entry,
        type: entry.type || 'automaton',
        dimension: state.dimension
      }));
    }
    
    return lines.join('\n');
  }

  /**
   * Generate automaton.seed.canvasl with versioning.
   * 
   * Creates a CanvasL file containing the seed entry for automaton regeneration.
   * The seed file includes version information, kernel URL reference, and
   * regeneration metadata for self-building capabilities.
   * 
   * @param {AutomatonState} state - Automaton state containing seed data
   * @returns {string} CanvasL file content as a string
   * 
   * @example
   * ```typescript
   * const seedContent = service.generateSeedCanvasL(automatonState);
   * // seedContent contains seed entry with regeneration metadata
   * ```
   */
  generateSeedCanvasL(state: AutomatonState): string {
    const lines: string[] = [];
    
    // Add version directive
    lines.push('@version 1.0.0');
    lines.push('@schema automaton-seed');
    lines.push('');
    
    // Add seed entry
    lines.push(JSON.stringify({
      id: `${state.id}-seed`,
      type: 'seed',
      dimension: state.dimension,
      version: '1.0.0',
      kernelUrl: `./automaton.kernel.canvasl`,
      regeneration: {
        function: 'r5rs:parse-jsonl-canvas',
        args: ['automaton.kernel.canvasl'],
        context: {
          module: 'MODULE 2: JSONL Parser & Canvas Loader'
        }
      },
      provenanceHistory: state.seed || []
    }));
    
    return lines.join('\n');
  }

  /**
   * Generate metaverse.topology.canvasl from topology partition.
   * 
   * Creates a CanvasL file containing the topology partition entries (Bipartite-BQF
   * left side). Each entry includes Bipartite-BQF metadata with BQF coefficients
   * calculated for the topology partition.
   * 
   * @param {AutomatonState} state - Automaton state containing topology entries
   * @returns {string} CanvasL file content as a string
   * 
   * @example
   * ```typescript
   * const topologyContent = service.generateTopologyCanvasL(automatonState);
   * // topologyContent contains topology entries with BQF metadata
   * ```
   */
  generateTopologyCanvasL(state: AutomatonState): string {
    const lines: string[] = [];
    
    // Add version directive
    lines.push('@version 1.0.0');
    lines.push('@schema metaverse-topology');
    lines.push('');
    
    // Add topology entries with Bipartite-BQF metadata
    for (const entry of state.topology) {
      const bqf = this.calculateBQF(state.dimension, 'topology');
      
      lines.push(JSON.stringify({
        ...entry,
        type: entry.type || 'topology',
        dimension: state.dimension,
        frontmatter: {
          ...entry.frontmatter,
          bipartite: {
            partition: 'topology',
            dimension: state.dimension,
            bqf: {
              coefficients: [bqf.a, bqf.b, bqf.c],
              form: `${bqf.a}x² + ${bqf.b}xy + ${bqf.c}y²`,
              signature: this.getSignature(state.dimension)
            }
          }
        }
      }));
    }
    
    return lines.join('\n');
  }

  /**
   * Generate metaverse.system.canvasl from system partition.
   * 
   * Creates a CanvasL file containing the system partition entries (Bipartite-BQF
   * right side). Each entry includes Bipartite-BQF metadata with BQF coefficients
   * calculated for the system partition.
   * 
   * @param {AutomatonState} state - Automaton state containing system entries
   * @returns {string} CanvasL file content as a string
   * 
   * @example
   * ```typescript
   * const systemContent = service.generateSystemCanvasL(automatonState);
   * // systemContent contains system entries with BQF metadata
   * ```
   */
  generateSystemCanvasL(state: AutomatonState): string {
    const lines: string[] = [];
    
    // Add version directive
    lines.push('@version 1.0.0');
    lines.push('@schema metaverse-system');
    lines.push('');
    
    // Add system entries with Bipartite-BQF metadata
    for (const entry of state.system) {
      const bqf = this.calculateBQF(state.dimension, 'system');
      
      lines.push(JSON.stringify({
        ...entry,
        type: entry.type || 'system',
        dimension: state.dimension,
        frontmatter: {
          ...entry.frontmatter,
          bipartite: {
            partition: 'system',
            dimension: state.dimension,
            bqf: {
              coefficients: [bqf.a, bqf.b, bqf.c],
              form: `${bqf.a}x² + ${bqf.b}xy + ${bqf.c}y²`,
              signature: this.getSignature(state.dimension)
            }
          }
        }
      }));
    }
    
    return lines.join('\n');
  }

  /**
   * Calculate BQF coefficients for dimension and partition
   */
  private calculateBQF(dimension: string, partition: string): BQFCoefficients {
    const dimNum = parseInt(dimension.replace('D', '')) || 0;
    
    // Simplified BQF calculation
    // Topology: mathematical foundations
    // System: computational implementations
    if (partition === 'topology') {
      return {
        a: 1,
        b: 0,
        c: dimNum
      };
    } else {
      return {
        a: dimNum,
        b: 1,
        c: 1
      };
    }
  }

  /**
   * Get signature for dimension
   */
  private getSignature(dimension: string): string {
    const dimNum = parseInt(dimension.replace('D', '')) || 0;
    
    if (dimNum === 0) return 'identity';
    if (dimNum === 1) return 'successor';
    if (dimNum === 2) return 'pairing';
    if (dimNum === 3 || dimNum === 4) return 'lorentz';
    if (dimNum === 5) return 'consensus';
    if (dimNum === 6) return 'intelligence';
    if (dimNum === 7) return 'quantum';
    
    return 'euclidean';
  }

  /**
   * Generate all files for an automaton state.
   * 
   * Generates all four standard automaton CanvasL files in a single call:
   * - automaton.kernel.canvasl
   * - automaton.seed.canvasl
   * - metaverse.topology.canvasl
   * - metaverse.system.canvasl
   * 
   * This is a convenience method that calls all individual generation methods
   * and returns their results in a single object.
   * 
   * @param {AutomatonState} state - Automaton state containing all partitions
   * @returns {{kernel: string, seed: string, topology: string, system: string}} 
   *   Object containing all generated file contents
   * @throws {ValidationError} If automaton state is invalid
   * 
   * @example
   * ```typescript
   * const files = service.generateAllFiles(automatonState);
   * await service.saveFile('automaton.kernel.canvasl', files.kernel);
   * await service.saveFile('automaton.seed.canvasl', files.seed);
   * await service.saveFile('metaverse.topology.canvasl', files.topology);
   * await service.saveFile('metaverse.system.canvasl', files.system);
   * ```
   */
  generateAllFiles(state: AutomatonState): {
    kernel: string;
    seed: string;
    topology: string;
    system: string;
  } {
    // Validate state before generation
    validateAutomatonState(state);
    
    return {
      kernel: this.generateKernelCanvasL(state),
      seed: this.generateSeedCanvasL(state),
      topology: this.generateTopologyCanvasL(state),
      system: this.generateSystemCanvasL(state)
    };
  }

  /**
   * Save file with temp file write and atomic move.
   * 
   * Saves a CanvasL file using an atomic write pattern: writes to a temporary file
   * first, validates the content, then atomically moves it to the final location.
   * This ensures data integrity and prevents partial writes from corrupting files.
   * 
   * The method handles content validation, temporary file tracking, and cleanup
   * on errors. In browser environments, file deletion may not be possible, so
   * temporary files are tracked for manual cleanup.
   * 
   * @param {string} filePath - Path where the file should be saved
   * @param {string} content - CanvasL file content to save
   * @returns {Promise<void>} Promise that resolves when file is saved
   * @throws {ValidationError} If content is empty or invalid
   * @throws {FileSystemError} If file cannot be written or moved
   * 
   * @example
   * ```typescript
   * const content = service.generateKernelCanvasL(automatonState);
   * await service.saveFile('automaton.kernel.canvasl', content);
   * // File is now saved atomically
   * ```
   */
  async saveFile(filePath: string, content: string): Promise<void> {
    try {
      // Validate content
      if (!content || content.trim().length === 0) {
        throw new ValidationError('File content cannot be empty', { content: ['Content is required'] });
      }

      // Write to temp file first
      const tempPath = await writeToTempFile(
        content,
        filePath,
        async (path: string, content: string) => {
          // Convert content to array of lines for writeJSONL
          const lines = content.split('\n').filter(line => line.trim());
          const data = lines.map(line => {
            try {
              return JSON.parse(line);
            } catch {
              return { raw: line };
            }
          });
          await databaseService.writeJSONL(path, data);
        }
      );

      // Track temp file
      this.tempFiles.set(filePath, tempPath);

      // Validate temp file content
      const tempContent = await databaseService.readJSONL(tempPath);
      if (!tempContent || tempContent.length === 0) {
        throw new FileSystemError('Temporary file is empty after write', tempPath, 'write');
      }

      // Atomically move temp file to final location
      await moveTempFile(
        tempPath,
        filePath,
        async (path: string) => {
          const data = await databaseService.readJSONL(path);
          return data.map(item => JSON.stringify(item)).join('\n');
        },
        async (path: string, content: string) => {
          const lines = content.split('\n').filter(line => line.trim());
          const data = lines.map(line => {
            try {
              return JSON.parse(line);
            } catch {
              return { raw: line };
            }
          });
          await databaseService.writeJSONL(path, data);
        },
        async (path: string) => {
          // Delete operation - in browser environment, we can't actually delete
          // Just remove from tracking
          this.tempFiles.delete(filePath);
        }
      );

      // Remove from tracking after successful move
      this.tempFiles.delete(filePath);
    } catch (error) {
      const errorObj = error instanceof Error ? error : new Error(String(error));
      
      // Log error with context
      errorLoggingService.logError(errorObj, {
        service: 'AutomatonFileGeneratorService',
        action: 'saveFile',
        metadata: { filePath },
        severity: 'error'
      });

      // Clean up temp file if exists
      const tempPath = this.tempFiles.get(filePath);
      if (tempPath) {
        try {
          // Try to clean up temp file (in browser, this might not be possible)
          this.tempFiles.delete(filePath);
        } catch (cleanupError) {
          // Ignore cleanup errors
        }
      }

      // Re-throw with appropriate error type
      if (errorObj instanceof ValidationError || errorObj instanceof FileSystemError) {
        throw errorObj;
      }
      
      throw new FileSystemError(
        `Failed to save file: ${errorObj.message}`,
        filePath,
        'write',
        errorObj
      );
    }
  }

  /**
   * Clean up temporary files.
   * 
   * Removes all tracked temporary files from the internal tracking map.
   * In browser environments, actual file deletion may not be possible, so
   * this primarily cleans up the tracking state. This should be called
   * periodically or when the service is disposed to prevent memory leaks.
   * 
   * @returns {Promise<void>} Promise that resolves when cleanup is complete
   * 
   * @example
   * ```typescript
   * // Clean up on service disposal
   * await service.cleanupTempFiles();
   * // All temp file tracking is cleared
   * ```
   */
  async cleanupTempFiles(): Promise<void> {
    for (const [finalPath, tempPath] of this.tempFiles.entries()) {
      try {
        // In browser environment, we can't actually delete files
        // Just remove from tracking
        this.tempFiles.delete(finalPath);
      } catch (error) {
        const errorObj = error instanceof Error ? error : new Error(String(error));
        errorLoggingService.logError(errorObj, {
          service: 'AutomatonFileGeneratorService',
          action: 'cleanupTempFiles',
          metadata: { tempPath, finalPath },
          severity: 'warning'
        });
      }
    }
  }
}

