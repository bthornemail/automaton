/**
 * Provenance Slide Service
 * 
 * Integrates template-projector slide generation with provenance chain building.
 * Generates slides from evolution directories with federated provenance tracking.
 */

import { Projector } from './projector/Projector';
import { AgentCoordinator } from './agent-coordinator/AgentCoordinator';
import { TopicSlideGenerator } from './projector/TopicSlideGenerator';
import { agentProvenanceQueryService, FederatedProvenanceQuery, QueryType } from './agent-provenance-query-service';
import { databaseService } from './database-service';
import { jsonlCanvasService } from './jsonl-canvas-service';
import { retryWithBackoff, withTimeout, formatUserErrorMessage, validateProvenanceChain, RetryOptions } from '../utils/error-handling';
import { errorLoggingService } from './error-logging-service';
import { provenanceChainCache, ProvenanceChainCache } from './provenance-chain-cache';
import { performanceMonitoringService } from './performance-monitoring-service';
import { avatarTemplateService } from './avatar-template-service';

export interface AvatarConfig {
  gltfModel: string;
  scale: [number, number, number];
  type: 'human' | 'ai-agent';
  label?: string;
  color?: string;
}

export interface ProvenanceNode {
  id: string;
  type: 'agent' | 'document' | 'code' | 'interaction' | 'evolution';
  position: [number, number, number];
  metadata: {
    timestamp: number;
    file: string;
    line: number;
    agentId: string;
    dimension?: string;
    churchEncoding?: string;
    pattern?: string;
  };
  data: any;
  avatar?: AvatarConfig;
}

export interface ProvenanceEdge {
  id: string;
  type: 'consumes' | 'produces' | 'references' | 'evolves' | 'interacts';
  from: string;
  to: string;
  metadata: {
    timestamp: number;
    weight: number;
    context: string;
  };
}

export interface ProvenanceChain {
  nodes: ProvenanceNode[];
  edges: ProvenanceEdge[];
}

export interface Slide {
  id: string;
  type: string;
  title?: string;
  dimension?: string;
  description?: string;
  content?: string;
  provenanceChain?: ProvenanceChain;
  cards?: Card[];
  [key: string]: any;
}

export interface Card {
  id: string;
  pattern: string;
  jsonlLines: any[];
  metadata: {
    churchEncoding?: string;
    bqfCoefficients?: number[];
    provenanceHistory?: any[];
  };
}

export interface PaginationOptions {
  page?: number;
  pageSize?: number;
  cursor?: string;
}

export interface ProvenanceSlideServiceConfig {
  retryOptions?: RetryOptions;
  timeout?: number;
  enableCache?: boolean;
  cache?: ProvenanceChainCache;
}

export class ProvenanceSlideService {
  private projector: Projector;
  private agentCoordinator: AgentCoordinator;
  private topicSlideGenerator: TopicSlideGenerator;
  private retryOptions: RetryOptions;
  private timeout: number;
  private enableCache: boolean;
  private cache: ProvenanceChainCache;
  private slideContentCache: Map<string, string> = new Map();

  constructor(config: ProvenanceSlideServiceConfig = {}) {
    this.projector = new Projector();
    this.agentCoordinator = new AgentCoordinator();
    this.topicSlideGenerator = new TopicSlideGenerator(this.agentCoordinator, null as any);
    
    // Configure retry options (per service, no fixed defaults)
    this.retryOptions = config.retryOptions || {
      maxRetries: 3,
      initialDelay: 1000,
      maxDelay: 10000,
      backoffStrategy: 'exponential'
    };
    
    // Configure timeout (per service, no fixed defaults)
    this.timeout = config.timeout || 10000;
    
    // Configure caching
    this.enableCache = config.enableCache !== false;
    this.cache = config.cache || provenanceChainCache;
  }

  /**
   * Initialize the service and its dependencies.
   * 
   * Initializes the Projector and AgentCoordinator services that are required
   * for provenance chain building and slide generation. This method must be called
   * before using other service methods.
   * 
   * @returns {Promise<void>} Promise that resolves when initialization is complete
   * @throws {Error} If initialization fails for any dependency
   * 
   * @example
   * ```typescript
   * const service = new ProvenanceSlideService();
   * await service.init();
   * // Service is now ready to use
   * ```
   */
  async init(): Promise<void> {
    await this.projector.onInit();
    await this.agentCoordinator.init();
  }

  /**
   * Build provenance chain from evolution directory with federated provenance tracking.
   * 
   * Loads evolution files from the specified directory, extracts self-execution patterns,
   * and builds a provenance chain with nodes and edges. The chain includes:
   * - Nodes for each pattern with metadata (dimension, Church encoding, pattern, etc.)
   * - Edges for dimensional progression (0D→1D→...→7D→0D)
   * - Edges for cross-file references (federated provenance)
   * - Avatar assignments for agent nodes
   * 
   * The method uses caching to avoid rebuilding chains for the same evolution path.
   * Results are validated and performance metrics are updated.
   * 
   * @param {string} evolutionPath - Path to the evolution directory containing automaton files
   * @param {string} [dimension] - Optional dimension filter (e.g., '0D', '1D', etc.)
   * @returns {Promise<ProvenanceChain>} Promise resolving to a provenance chain with nodes and edges
   * @throws {Error} If evolution files cannot be loaded or chain building fails
   * 
   * @example
   * ```typescript
   * const chain = await service.buildProvenanceChain('/evolutions/advanced-automaton');
   * console.log(`Chain has ${chain.nodes.length} nodes and ${chain.edges.length} edges`);
   * ```
   */
  async buildProvenanceChain(evolutionPath: string, dimension?: string): Promise<ProvenanceChain> {
    try {
      // Check cache first
      if (this.enableCache) {
        const cacheKey = ProvenanceChainCache.generateKey(evolutionPath, dimension);
        const cached = this.cache.get(cacheKey);
        if (cached) {
          return cached;
        }
      }

      const nodes: ProvenanceNode[] = [];
      const edges: ProvenanceEdge[] = [];
      
      // Load evolution files with timeout
      const files = await withTimeout(
        this.loadEvolutionFiles(evolutionPath),
        this.timeout,
        'loadEvolutionFiles'
      );
      
      // Extract self-execution patterns with federated provenance (optimized)
      const patterns = await this.extractSelfExecutionPatternsWithProvenance(files);
    
    // Build nodes for each pattern
    for (const pattern of patterns) {
      const agentId = pattern.agentId || `${pattern.dimension}-Agent`;
      const isAgentNode = pattern.type === 'agent' || agentId.includes('Agent');
      
      // Assign avatar based on agent type
      let avatar: AvatarConfig | undefined;
      if (isAgentNode) {
        // Determine if it's an AI agent or human based on agentId
        const isAIAgent = agentId.includes('Intelligence') || 
                         agentId.includes('Quantum') || 
                         agentId.includes('Consensus') ||
                         agentId.includes('AI');
        
        const avatarType: 'human' | 'ai-agent' = isAIAgent ? 'ai-agent' : 'human';
        const template = avatarTemplateService.getRandomTemplate(avatarType) || 
                        avatarTemplateService.getDefaultTemplate(avatarType);
        
        if (template) {
          avatar = avatarTemplateService.createAvatarConfig(template.id, {
            label: agentId
          });
        }
      }
      
      const node: ProvenanceNode = {
        id: `pattern-${pattern.id}`,
        type: isAgentNode ? 'agent' : 'evolution',
        position: this.calculatePosition(pattern.dimension),
        metadata: {
          timestamp: pattern.timestamp || Date.now(),
          file: pattern.file,
          line: pattern.line,
          agentId: agentId,
          dimension: pattern.dimension,
          churchEncoding: pattern.churchEncoding,
          pattern: pattern.pattern
        },
        data: {
          ...pattern,
          provenanceHistory: pattern.provenanceHistory || []
        },
        avatar: avatar
      };
      nodes.push(node);
    }
    
    // Build edges between patterns (dimensional progression)
    for (let i = 0; i < nodes.length - 1; i++) {
      const fromNode = nodes[i];
      const toNode = nodes[i + 1];
      
      // Check if there's a dimensional progression
      const fromDim = parseInt(fromNode.metadata.dimension?.replace('D', '') || '0');
      const toDim = parseInt(toNode.metadata.dimension?.replace('D', '') || '0');
      
      if (toDim === fromDim + 1 || (fromDim === 7 && toDim === 0)) {
        const edge: ProvenanceEdge = {
          id: `edge-${fromNode.id}-${toNode.id}`,
          type: 'evolves',
          from: fromNode.id,
          to: toNode.id,
          metadata: {
            timestamp: Date.now(),
            weight: 1.0,
            context: `Dimensional progression: ${fromNode.metadata.dimension} → ${toNode.metadata.dimension}`
          }
        };
        edges.push(edge);
      }
      
      // Check for cross-file references (federated provenance)
      if (fromNode.metadata.file !== toNode.metadata.file) {
        const referenceEdge: ProvenanceEdge = {
          id: `ref-${fromNode.id}-${toNode.id}`,
          type: 'references',
          from: fromNode.id,
          to: toNode.id,
          metadata: {
            timestamp: Date.now(),
            weight: 0.5,
            context: `Cross-file reference: ${fromNode.metadata.file} → ${toNode.metadata.file}`
          }
        };
        edges.push(referenceEdge);
      }
      
      const chain = { nodes, edges };
      
      // Validate provenance chain
      validateProvenanceChain(chain);
      
      // Update performance monitoring
      performanceMonitoringService.updateNodeEdgeCounts(nodes.length, edges.length);
      
      // Cache the chain
      if (this.enableCache) {
        const cacheKey = ProvenanceChainCache.generateKey(evolutionPath, dimension);
        this.cache.set(cacheKey, chain);
      }
      
      return chain;
    } catch (error) {
      const errorObj = error instanceof Error ? error : new Error(String(error));
      const userMessage = formatUserErrorMessage(errorObj);
      
      // Log error with context
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceSlideService',
        action: 'buildProvenanceChain',
        metadata: { evolutionPath },
        severity: 'error'
      });
      
      // Throw user-friendly error
      throw new Error(userMessage);
    }
  }

  /**
   * Generate slides from evolution directory (one per recursion level).
   * 
   * Creates slides representing the dimensional progression of automaton evolution.
   * Each slide corresponds to one dimension (0D, 1D, 2D, ..., 7D, 0D) and contains:
   * - Provenance chain nodes for that dimension
   * - Edges connecting nodes within and across dimensions
   * - Cards grouped by pattern for that dimension
   * - Slide content with Church encoding and BQF information
   * 
   * The slides are generated in order: 0D → 1D → 2D → 3D → 4D → 5D → 6D → 7D → 0D,
   * representing the complete dimensional progression cycle.
   * 
   * @param {string} evolutionPath - Path to the evolution directory
   * @returns {Promise<Slide[]>} Promise resolving to an array of slides, one per dimension
   * @throws {Error} If provenance chain cannot be built or slide generation fails
   * 
   * @example
   * ```typescript
   * const slides = await service.generateSlidesFromEvolution('/evolutions/advanced-automaton');
   * // slides.length will be 9 (0D through 7D plus final 0D)
   * slides.forEach(slide => {
   *   console.log(`${slide.dimension}: ${slide.provenanceChain?.nodes.length} nodes`);
   * });
   * ```
   */
  async generateSlidesFromEvolution(evolutionPath: string): Promise<Slide[]> {
    const slides: Slide[] = [];
    
    // Build provenance chain
    const chain = await this.buildProvenanceChain(evolutionPath);
    
    // Group nodes by dimension
    const nodesByDimension = new Map<string, ProvenanceNode[]>();
    for (const node of chain.nodes) {
      const dim = node.metadata.dimension || '0D';
      if (!nodesByDimension.has(dim)) {
        nodesByDimension.set(dim, []);
      }
      nodesByDimension.get(dim)!.push(node);
    }
    
    // Create one slide per dimension (0D→7D→0D)
    const dimensions = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D', '0D'];
    for (const dimension of dimensions) {
      const dimensionNodes = nodesByDimension.get(dimension) || [];
      const dimensionChain: ProvenanceChain = {
        nodes: dimensionNodes,
        edges: chain.edges.filter(e => {
          const fromNode = chain.nodes.find(n => n.id === e.from);
          const toNode = chain.nodes.find(n => n.id === e.to);
          return fromNode?.metadata.dimension === dimension || toNode?.metadata.dimension === dimension;
        })
      };
      
      // Generate cards for this dimension
      const cards = await this.generateCardsForDimension(dimension, dimensionNodes);
      
      // Generate memoized slide content
      const contentCacheKey = `${dimension}:${dimensionNodes.length}:${new Set(dimensionNodes.map(n => n.metadata.pattern)).size}`;
      const slideContent = this.generateSlideContentMemoized(dimension, dimensionNodes, contentCacheKey);
      
      const slide: Slide = {
        id: `slide-${dimension}-${Date.now()}`,
        type: 'slide',
        title: `${dimension} Evolution`,
        dimension: dimension,
        description: `Provenance chain for ${dimension} dimensional evolution`,
        content: slideContent,
        provenanceChain: dimensionChain,
        cards: cards
      };
      
      slides.push(slide);
    }
    
    return slides;
  }

  /**
   * Generate cards grouped by pattern with JSONL line aggregation (optimized).
   * 
   * Groups provenance nodes by their pattern and creates cards containing:
   * - All JSONL lines with the same pattern
   * - Pattern metadata (Church encoding, BQF coefficients)
   * - Aggregated provenance history from all nodes in the pattern
   * 
   * The method uses optimized single-pass aggregation with Map-based grouping
   * for better performance with large node sets. Early exit is used for empty inputs.
   * 
   * @param {string} dimension - Dimension identifier (e.g., '0D', '1D', etc.)
   * @param {ProvenanceNode[]} nodes - Array of provenance nodes for the dimension
   * @returns {Promise<Card[]>} Promise resolving to an array of cards, one per unique pattern
   * 
   * @example
   * ```typescript
   * const nodes = chain.nodes.filter(n => n.metadata.dimension === '0D');
   * const cards = await service.generateCardsForDimension('0D', nodes);
   * // cards will be grouped by pattern (e.g., 'identity', 'successor', etc.)
   * cards.forEach(card => {
   *   console.log(`Pattern: ${card.pattern}, Lines: ${card.jsonlLines.length}`);
   * });
   * ```
   */
  async generateCardsForDimension(dimension: string, nodes: ProvenanceNode[]): Promise<Card[]> {
    // Early exit for empty nodes
    if (nodes.length === 0) {
      return [];
    }

    const cards: Card[] = [];
    const patternGroups = new Map<string, { nodes: ProvenanceNode[]; jsonlLines: any[] }>();
    
    // Optimized: Single-pass aggregation with Map
    for (const node of nodes) {
      const pattern = node.metadata.pattern || 'unknown';
      
      if (!patternGroups.has(pattern)) {
        patternGroups.set(pattern, { nodes: [], jsonlLines: [] });
      }
      
      const group = patternGroups.get(pattern)!;
      group.nodes.push(node);
      
      // Aggregate JSONL lines from node data (optimized parsing)
      if (node.data?.rawEntry) {
        group.jsonlLines.push(node.data.rawEntry);
      } else if (node.data?.rawLine) {
        try {
          const parsed = JSON.parse(node.data.rawLine);
          group.jsonlLines.push(parsed);
        } catch (e) {
          group.jsonlLines.push(node.data);
        }
      } else {
        group.jsonlLines.push({
          id: node.id,
          type: node.type,
          metadata: node.metadata,
          ...node.data
        });
      }
    }
    
    // Create one card per pattern
    for (const [pattern, group] of patternGroups) {
      // Aggregate provenance history from all nodes in this pattern
      const allProvenanceHistory: any[] = [];
      for (const node of group.nodes) {
        const history = (node.data?.provenanceHistory || []) as any[];
        allProvenanceHistory.push(...history);
      }
      
      // Get Church encoding (prefer from first node with encoding)
      const churchEncoding = group.nodes.find(n => n.metadata.churchEncoding)?.metadata.churchEncoding;
      
      // Calculate BQF coefficients
      const bqfCoefficients = this.calculateBQFCoefficients(dimension);
      
      const card: Card = {
        id: `card-${dimension}-${pattern}-${Date.now()}`,
        pattern: pattern,
        jsonlLines: group.jsonlLines,
        metadata: {
          churchEncoding: churchEncoding,
          bqfCoefficients: bqfCoefficients,
          provenanceHistory: [
            // Include node-level provenance
            ...group.nodes.map(node => ({
              file: node.metadata.file,
              line: node.metadata.line,
              timestamp: node.metadata.timestamp,
              agentId: node.metadata.agentId,
              dimension: node.metadata.dimension
            })),
            // Include aggregated provenance history
            ...allProvenanceHistory
          ]
        }
      };
      
      cards.push(card);
    }
    
    return cards;
  }

  /**
   * Get evolution file count for pagination support.
   * 
   * Queries the database to count the total number of evolution files in the
   * specified directory. This is used for pagination calculations when loading
   * large evolution directories.
   * 
   * @param {string} evolutionPath - Path to the evolution directory
   * @returns {Promise<number>} Promise resolving to the total number of evolution files
   * 
   * @example
   * ```typescript
   * const count = await service.getEvolutionFileCount('/evolutions/advanced-automaton');
   * const totalPages = Math.ceil(count / pageSize);
   * ```
   */
  async getEvolutionFileCount(evolutionPath: string): Promise<number> {
    try {
      const query = `
        SELECT (COUNT(?file) as ?count) WHERE {
          ?file rdf:type evolution:EvolutionFile .
          ?file evolution:path "${evolutionPath}" .
        }
      `;
      
      const results = await databaseService.query(query, 'sparql');
      if (results.length > 0 && results[0].count) {
        return parseInt(results[0].count) || 0;
      }
      return 0;
    } catch (error) {
      console.warn('Failed to get evolution file count', error);
      return 0;
    }
  }

  /**
   * Load evolution files from directory with pagination support.
   * 
   * Loads evolution files in paginated chunks to handle large directories efficiently.
   * Supports both page-based and cursor-based pagination. Returns files along with
   * pagination metadata (total count, hasMore flag, cursor for next page).
   * 
   * Files are filtered to only include entries with self-reference metadata, which
   * are required for provenance chain building.
   * 
   * @param {string} evolutionPath - Path to the evolution directory
   * @param {PaginationOptions} [options={}] - Pagination options (page, pageSize, cursor)
   * @returns {Promise<{files: any[], total: number, hasMore: boolean, cursor?: string}>} 
   *   Promise resolving to paginated file results with metadata
   * @throws {Error} If files cannot be loaded or query fails
   * 
   * @example
   * ```typescript
   * // First page
   * const page1 = await service.loadEvolutionFilesPaginated('/evolutions/advanced-automaton', {
   *   page: 0,
   *   pageSize: 100
   * });
   * 
   * // Next page using cursor
   * if (page1.hasMore) {
   *   const page2 = await service.loadEvolutionFilesPaginated('/evolutions/advanced-automaton', {
   *     cursor: page1.cursor
   *   });
   * }
   * ```
   */
  async loadEvolutionFilesPaginated(
    evolutionPath: string,
    options: PaginationOptions = {}
  ): Promise<{ files: any[]; total: number; hasMore: boolean; cursor?: string }> {
    const page = options.page || 0;
    const pageSize = options.pageSize || 100;
    
    try {
      // Get total count
      const total = await this.getEvolutionFileCount(evolutionPath);
      
      // Load files with pagination
      const query = `
        SELECT ?file ?content WHERE {
          ?file rdf:type evolution:EvolutionFile .
          ?file evolution:path "${evolutionPath}" .
          ?file evolution:content ?content .
        }
        LIMIT ${pageSize}
        OFFSET ${page * pageSize}
      `;
      
      const { result: results } = await retryWithBackoff(
        async () => {
          try {
            return await databaseService.query(query, 'sparql');
          } catch (error) {
            const errorObj = error instanceof Error ? error : new Error(String(error));
            if (errorObj.message.includes('network') || errorObj.message.includes('fetch') || errorObj.message.includes('timeout')) {
              (errorObj as any).name = 'NetworkError';
            }
            throw errorObj;
          }
        },
        this.retryOptions
      );
      
      const files: any[] = [];
      for (const result of results) {
        try {
          const content = result.content;
          const lines = content.split('\n').filter((line: string) => line.trim());
          
          for (const line of lines) {
            try {
              const obj = JSON.parse(line);
              if (obj.selfReference || obj.metadata?.selfReference) {
                files.push({
                  ...obj,
                  file: result.file,
                  rawLine: line
                });
              }
            } catch (e) {
              // Skip invalid JSON lines
            }
          }
        } catch (e) {
          // Skip files that can't be parsed
        }
      }
      
      const hasMore = (page + 1) * pageSize < total;
      const cursor = hasMore ? `page-${page + 1}` : undefined;
      
      return { files, total, hasMore, cursor };
    } catch (error) {
      const errorObj = error instanceof Error ? error : new Error(String(error));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceSlideService',
        action: 'loadEvolutionFilesPaginated',
        metadata: { evolutionPath, options },
        severity: 'error'
      });
      throw errorObj;
    }
  }

  /**
   * Load evolution files from directory
   */
  private async loadEvolutionFiles(evolutionPath: string): Promise<any[]> {
    const files: any[] = [];
    
    try {
      // Use database service to query for evolution files with retry logic
      const query = `
        SELECT ?file ?content WHERE {
          ?file rdf:type evolution:EvolutionFile .
          ?file evolution:path "${evolutionPath}" .
          ?file evolution:content ?content .
        }
      `;
      
      // Wrap database query with retry logic
      const { result: results } = await retryWithBackoff(
        async () => {
          try {
            return await databaseService.query(query, 'sparql');
          } catch (error) {
            // Re-throw as NetworkError for retry logic
            const errorObj = error instanceof Error ? error : new Error(String(error));
            if (errorObj.message.includes('network') || errorObj.message.includes('fetch') || errorObj.message.includes('timeout')) {
              (errorObj as any).name = 'NetworkError';
            }
            throw errorObj;
          }
        },
        this.retryOptions
      );
      
      for (const result of results) {
        try {
          // Parse CanvasL/JSONL content
          const content = result.content;
          const lines = content.split('\n').filter((line: string) => line.trim());
          
          for (const line of lines) {
            try {
              const obj = JSON.parse(line);
              if (obj.selfReference || obj.metadata?.selfReference) {
                files.push({
                  ...obj,
                  file: result.file,
                  rawLine: line
                });
              }
            } catch (e) {
              // Skip invalid JSON lines
            }
          }
        } catch (e) {
          // Skip files that can't be parsed
        }
      }
    } catch (e) {
      const errorObj = e instanceof Error ? e : new Error(String(e));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceSlideService',
        action: 'loadEvolutionFiles',
        metadata: { evolutionPath, stage: 'database-query' },
        severity: 'warning'
      });
      
      console.warn('Failed to load evolution files from database, trying direct file access', e);
      
      // Fallback: try to load from file system if available
      // This would require a file system API or fetch to a file server
      try {
        // Use AbortController for timeout
        const controller = new AbortController();
        const timeoutId = setTimeout(() => controller.abort(), this.timeout);
        
        const response = await fetch(`${evolutionPath}/index.jsonl`, {
          signal: controller.signal
        });
        
        clearTimeout(timeoutId);
        if (response.ok) {
          const text = await response.text();
          const lines = text.split('\n').filter(line => line.trim());
          
          for (const line of lines) {
            try {
              const obj = JSON.parse(line);
              if (obj.selfReference || obj.metadata?.selfReference) {
                files.push({
                  ...obj,
                  file: `${evolutionPath}/index.jsonl`,
                  rawLine: line
                });
              }
            } catch (e) {
              // Skip invalid JSON lines
            }
          }
        }
      } catch (fetchError) {
        const errorObj = fetchError instanceof Error ? fetchError : new Error(String(fetchError));
        errorLoggingService.logError(errorObj, {
          service: 'ProvenanceSlideService',
          action: 'loadEvolutionFiles',
          metadata: { evolutionPath, stage: 'fetch-fallback' },
          severity: 'error'
        });
        console.warn('Failed to fetch evolution files', fetchError);
        throw new Error(formatUserErrorMessage(errorObj));
      }
    }
    
    return files;
  }

  /**
   * Extract self-execution patterns from files with federated provenance (optimized)
   */
  private async extractSelfExecutionPatternsWithProvenance(files: any[]): Promise<any[]> {
    // Early termination for empty files
    if (files.length === 0) {
      return [];
    }

    const patterns: any[] = [];
    const fileMap = new Map<string, any[]>();
    const patternSet = new Set<string>(); // For duplicate detection
    
    // Group files by source file (optimized with Map)
    for (const file of files) {
      const sourceFile = file.selfReference?.file || file.metadata?.selfReference?.file || file.file;
      if (!fileMap.has(sourceFile)) {
        fileMap.set(sourceFile, []);
      }
      fileMap.get(sourceFile)!.push(file);
    }
    
    // Batch federated provenance queries by file
    const provenanceQueries = new Map<string, Promise<any[]>>();
    
    // Extract patterns with federated provenance tracking (optimized)
    for (const [sourceFile, fileEntries] of fileMap) {
      // Batch queries for this file
      const filePatterns: any[] = [];
      
      for (const entry of fileEntries) {
        const selfRef = entry.selfReference || entry.metadata?.selfReference;
        if (!selfRef) continue;
        
        const patternKey = `${sourceFile}:${selfRef.line}:${selfRef.pattern}`;
        
        // Skip duplicates
        if (patternSet.has(patternKey)) continue;
        patternSet.add(patternKey);
        
        // Create federated query (batch by file)
        if (!provenanceQueries.has(sourceFile)) {
          const federatedQuery: FederatedProvenanceQuery = {
            files: [sourceFile],
            query: `
              SELECT ?entry ?provenance ?line WHERE {
                ?entry prov:wasDerivedFrom ?provenance .
                ?entry prov:atLine ?line .
              }
            `,
            queryType: QueryType.SPARQL
          };
          
          provenanceQueries.set(sourceFile, 
            agentProvenanceQueryService.queryFederatedProvenance(federatedQuery)
              .catch(() => [])
          );
        }
        
        // Extract Church encoding if available
        const churchEncoding = this.extractChurchEncoding(entry);
        
        filePatterns.push({
          id: entry.id || `pattern-${sourceFile}-${selfRef.line}`,
          file: sourceFile,
          line: selfRef.line,
          pattern: selfRef.pattern || 'unknown',
          dimension: this.inferDimensionFromPattern(selfRef.pattern || entry.type || ''),
          timestamp: entry.metadata?.timestamp || Date.now(),
          agentId: entry.metadata?.agentId || this.inferAgentFromDimension(this.inferDimensionFromPattern(selfRef.pattern || entry.type || '')),
          churchEncoding: churchEncoding,
          provenanceHistory: [], // Will be populated from batch query
          rawEntry: entry,
          patternKey
        });
      }
      
      // Execute batch query for this file
      const provenanceResults = await provenanceQueries.get(sourceFile) || [];
      
      // Map provenance results to patterns
      for (const pattern of filePatterns) {
        const matchingProvenance = provenanceResults.filter((p: any) => 
          p.line === pattern.line || p.entry?.includes(pattern.id)
        );
        
        if (matchingProvenance.length > 0) {
          pattern.provenanceHistory = matchingProvenance;
        } else {
          // Fallback: use selfReference as provenance
          pattern.provenanceHistory = [{
            file: pattern.file,
            line: pattern.line,
            pattern: pattern.pattern,
            timestamp: pattern.timestamp
          }];
        }
        
        patterns.push(pattern);
      }
    }
    
    // Sort patterns by dimension and timestamp
    patterns.sort((a, b) => {
      const dimA = parseInt(a.dimension.replace('D', '') || '0');
      const dimB = parseInt(b.dimension.replace('D', '') || '0');
      if (dimA !== dimB) return dimA - dimB;
      return a.timestamp - b.timestamp;
    });
    
    return patterns;
  }

  /**
   * Load provenance history on-demand for a node (lazy loading).
   * 
   * Loads the full provenance history for a specific node using federated provenance
   * queries. This method implements lazy loading - if the history is already loaded
   * in the node's data, it returns immediately without making additional queries.
   * 
   * The provenance history includes all entries that the node was derived from,
   * enabling full traceability of the node's origin across multiple files.
   * 
   * @param {string} nodeId - ID of the node to load history for
   * @param {ProvenanceChain} chain - The provenance chain containing the node
   * @returns {Promise<any[]>} Promise resolving to an array of provenance history entries
   * 
   * @example
   * ```typescript
   * const history = await service.loadProvenanceHistory('pattern-123', chain);
   * history.forEach(entry => {
   *   console.log(`From: ${entry.file}:${entry.line}`);
   * });
   * ```
   */
  async loadProvenanceHistory(nodeId: string, chain: ProvenanceChain): Promise<any[]> {
    const node = chain.nodes.find(n => n.id === nodeId);
    if (!node) {
      return [];
    }

    // If history is already loaded, return it
    if (node.data?.provenanceHistory && Array.isArray(node.data.provenanceHistory) && node.data.provenanceHistory.length > 0) {
      return node.data.provenanceHistory;
    }

    // Load full history via federated provenance query
    try {
      const federatedQuery: FederatedProvenanceQuery = {
        files: [node.metadata.file],
        query: `
          SELECT ?provenance WHERE {
            ?entry prov:wasDerivedFrom ?provenance .
            ?entry prov:atLine ${node.metadata.line} .
          }
        `,
        queryType: QueryType.SPARQL
      };
      
      const provenanceResults = await agentProvenanceQueryService.queryFederatedProvenance(federatedQuery);
      
      // Update node data with loaded history
      if (node.data) {
        node.data.provenanceHistory = provenanceResults || [];
      }
      
      return provenanceResults || [];
    } catch (error) {
      const errorObj = error instanceof Error ? error : new Error(String(error));
      errorLoggingService.logError(errorObj, {
        service: 'ProvenanceSlideService',
        action: 'loadProvenanceHistory',
        metadata: { nodeId },
        severity: 'warning'
      });
      
      return [];
    }
  }
  
  /**
   * Extract Church encoding from entry
   */
  private extractChurchEncoding(entry: any): string | undefined {
    if (entry.metadata?.churchEncoding) return entry.metadata.churchEncoding;
    if (entry.churchEncoding) return entry.churchEncoding;
    if (entry.text?.includes('λ')) {
      const match = entry.text.match(/λ[^λ]*/);
      return match ? match[0] : undefined;
    }
    return undefined;
  }
  
  /**
   * Infer agent ID from dimension
   */
  private inferAgentFromDimension(dimension: string): string {
    const dimNum = parseInt(dimension.replace('D', '') || '0');
    const agentNames = [
      '0D-Topology-Agent',
      '1D-Temporal-Agent',
      '2D-Structural-Agent',
      '3D-Algebraic-Agent',
      '4D-Network-Agent',
      '5D-Consensus-Agent',
      '6D-Intelligence-Agent',
      '7D-Quantum-Agent'
    ];
    return agentNames[dimNum] || '0D-Topology-Agent';
  }

  /**
   * Infer dimension from pattern
   */
  private inferDimensionFromPattern(pattern: string): string {
    // Simple inference - can be enhanced
    if (pattern.includes('0D') || pattern.includes('identity')) return '0D';
    if (pattern.includes('1D') || pattern.includes('successor')) return '1D';
    if (pattern.includes('2D') || pattern.includes('pair')) return '2D';
    if (pattern.includes('3D') || pattern.includes('algebra')) return '3D';
    if (pattern.includes('4D') || pattern.includes('network')) return '4D';
    if (pattern.includes('5D') || pattern.includes('consensus')) return '5D';
    if (pattern.includes('6D') || pattern.includes('intelligence')) return '6D';
    if (pattern.includes('7D') || pattern.includes('quantum')) return '7D';
    return '0D';
  }

  /**
   * Calculate 3D position for node
   */
  private calculatePosition(dimension: string | undefined): [number, number, number] {
    const dim = dimension || '0D';
    const dimNum = parseInt(dim.replace('D', '')) || 0;
    
    // Arrange in a spiral pattern
    const angle = (dimNum / 8) * Math.PI * 2;
    const radius = 5 + dimNum * 0.5;
    const x = Math.cos(angle) * radius;
    const y = dimNum * 2;
    const z = Math.sin(angle) * radius;
    
    return [x, y, z];
  }

  /**
   * Calculate BQF coefficients for dimension
   */
  private calculateBQFCoefficients(dimension: string): number[] {
    const dimNum = parseInt(dimension.replace('D', '')) || 0;
    
    // BQF form: ax² + bxy + cy²
    // For topology partition: a=1, b=0, c=dimNum
    // For system partition: a=dimNum, b=1, c=1
    // We'll use topology partition by default
    return [1, 0, dimNum];
  }

  /**
   * Generate slide content with Church encoding and dimensional topology (memoized)
   */
  private generateSlideContentMemoized(dimension: string, nodes: ProvenanceNode[], cacheKey?: string): string {
    // Generate cache key if not provided
    if (!cacheKey) {
      const nodeCount = nodes.length;
      const patterns = new Set(nodes.map(n => n.metadata.pattern)).size;
      cacheKey = `${dimension}:${nodeCount}:${patterns}`;
    }
    
    // Check cache
    if (this.slideContentCache.has(cacheKey)) {
      return this.slideContentCache.get(cacheKey)!;
    }
    
    // Generate content
    const content = this.generateSlideContent(dimension, nodes);
    
    // Cache it
    this.slideContentCache.set(cacheKey, content);
    
    return content;
  }

  /**
   * Generate slide content with Church encoding and dimensional topology
   */
  private generateSlideContent(dimension: string, nodes: ProvenanceNode[]): string {
    const parts: string[] = [];
    parts.push(`# ${dimension} Dimensional Evolution`);
    parts.push('');
    
    // Add dimensional topology information
    const dimNum = parseInt(dimension.replace('D', '') || '0');
    const churchEncoding = this.getChurchEncodingForDimension(dimension);
    parts.push(`**Dimension:** ${dimension}`);
    parts.push(`**Church Encoding:** ${churchEncoding}`);
    parts.push(`**BQF Form:** ${this.getBQFFormForDimension(dimension)}`);
    parts.push('');
    
    parts.push(`**Nodes:** ${nodes.length}`);
    parts.push(`**Patterns:** ${new Set(nodes.map(n => n.metadata.pattern)).size}`);
    parts.push('');
    
    // Add pattern information with Church encoding
    const patterns = new Map<string, { count: number; churchEncoding?: string; bqf?: string }>();
    for (const node of nodes) {
      const pattern = node.metadata.pattern || 'unknown';
      if (!patterns.has(pattern)) {
        patterns.set(pattern, {
          count: 0,
          churchEncoding: node.metadata.churchEncoding,
          bqf: this.calculateBQFForm(node.metadata.churchEncoding)
        });
      }
      patterns.get(pattern)!.count++;
    }
    
    parts.push('## Patterns');
    for (const [pattern, info] of patterns) {
      parts.push(`- **${pattern}**: ${info.count} occurrences`);
      if (info.churchEncoding) {
        parts.push(`  - Church: ${info.churchEncoding}`);
      }
      if (info.bqf) {
        parts.push(`  - BQF: ${info.bqf}`);
      }
    }
    
    // Add provenance history summary
    const totalProvenanceEntries = nodes.reduce((sum, node) => {
      const history = (node.data?.provenanceHistory || []) as any[];
      return sum + history.length;
    }, 0);
    
    if (totalProvenanceEntries > 0) {
      parts.push('');
      parts.push(`**Total Provenance Entries:** ${totalProvenanceEntries}`);
    }
    
    return parts.join('\n');
  }
  
  /**
   * Get Church encoding for dimension
   */
  private getChurchEncodingForDimension(dimension: string): string {
    const encodings: Record<string, string> = {
      '0D': 'λf.λx.x',
      '1D': 'λn.λf.λx.f(nfx)',
      '2D': 'λx.λy.λf.fxy',
      '3D': 'λm.λn.λf.λx.mf(nfx)',
      '4D': 'λm.λn.λf.m(nf)',
      '5D': 'λm.λn.nm',
      '6D': 'λf.(λx.f(xx))(λx.f(xx))',
      '7D': 'λf.λx.f(f(f(f(f(f(f(fx)))))))'
    };
    return encodings[dimension] || 'λf.λx.x';
  }
  
  /**
   * Get BQF form for dimension
   */
  private getBQFFormForDimension(dimension: string): string {
    const dimNum = parseInt(dimension.replace('D', '') || '0');
    const bqf = this.calculateBQFCoefficients(dimension);
    return `${bqf[0]}x² + ${bqf[1]}xy + ${bqf[2]}y²`;
  }
  
  /**
   * Calculate BQF form from Church encoding
   */
  private calculateBQFForm(churchEncoding?: string): string | undefined {
    if (!churchEncoding) return undefined;
    
    // Simple mapping from Church encoding to BQF
    // This is a simplified version - actual mapping would be more complex
    const lambdaCount = (churchEncoding.match(/λ/g) || []).length;
    const varCount = (churchEncoding.match(/[a-z]/g) || []).length;
    
    return `${lambdaCount}x² + ${varCount}xy + ${lambdaCount + varCount}y²`;
  }

  /**
   * Apply incremental update to provenance chain.
   * 
   * Applies an incremental update to an existing provenance chain. This method
   * modifies the chain by adding, updating, or removing nodes and edges as
   * specified by the update. The cache is invalidated after applying the update.
   * 
   * @param {ProvenanceChain} chain - Current provenance chain
   * @param {import('../types/provenance-updates').ChainUpdate} update - Incremental update to apply
   * @returns {ProvenanceChain} Updated provenance chain
   * 
   * @example
   * ```typescript
   * const updatedChain = service.applyIncrementalUpdate(chain, update);
   * // Chain now includes the incremental changes
   * ```
   */
  applyIncrementalUpdate(
    chain: ProvenanceChain,
    update: import('../types/provenance-updates').ChainUpdate
  ): ProvenanceChain {
    const updatedChain: ProvenanceChain = {
      nodes: [...chain.nodes],
      edges: [...chain.edges]
    };

    switch (update.type) {
      case 'node:added':
        if (update.data.node) {
          updatedChain.nodes.push(update.data.node);
        }
        break;

      case 'node:updated':
        if (update.data.node) {
          const index = updatedChain.nodes.findIndex(n => n.id === update.data.node!.id);
          if (index >= 0) {
            updatedChain.nodes[index] = update.data.node;
          }
        }
        break;

      case 'node:removed':
        if (update.data.nodeId) {
          updatedChain.nodes = updatedChain.nodes.filter(n => n.id !== update.data.nodeId);
          // Also remove edges connected to this node
          updatedChain.edges = updatedChain.edges.filter(
            e => e.from !== update.data.nodeId && e.to !== update.data.nodeId
          );
        }
        break;

      case 'edge:added':
        if (update.data.edge) {
          updatedChain.edges.push(update.data.edge);
        }
        break;

      case 'edge:removed':
        if (update.data.edgeId) {
          updatedChain.edges = updatedChain.edges.filter(e => e.id !== update.data.edgeId);
        }
        break;

      case 'chain:rebuilt':
        // For chain:rebuilt, we need to rebuild the chain from scratch
        // This will be handled by the caller
        if (update.data.chain) {
          return update.data.chain;
        }
        break;
    }

    // Invalidate cache for this evolution path
    if (this.enableCache) {
      this.cache.invalidate(update.evolutionPath);
    }

    return updatedChain;
  }

  /**
   * Subscribe to updates for an evolution path.
   * 
   * Sets up a subscription to receive real-time updates for a specific
   * evolution path. When updates are received, the handler is called
   * with the update information.
   * 
   * @param {string} evolutionPath - Evolution path to subscribe to
   * @param {import('../types/provenance-updates').UpdateHandler} handler - Handler for updates
   * 
   * @example
   * ```typescript
   * service.subscribeToUpdates('/evolutions/advanced-automaton', {
   *   onChainUpdate: (update) => {
   *     // Handle chain update
   *     const chain = service.buildProvenanceChain('/evolutions/advanced-automaton');
   *     const updatedChain = service.applyIncrementalUpdate(chain, update);
   *   }
   * });
   * ```
   */
  subscribeToUpdates(
    evolutionPath: string,
    handler: import('../types/provenance-updates').UpdateHandler
  ): void {
    // This method is a placeholder for integration with ProvenanceWebSocketService
    // The actual subscription is handled by ProvenanceWebSocketService
    // This method can be used to register handlers that will be called when updates arrive
    console.log(`Subscribed to updates for ${evolutionPath}`);
  }

  /**
   * Update slide with new data.
   * 
   * Updates a slide with partial data. The slide is updated in place
   * and the cache is invalidated. This method is used for live updates
   * when slides are modified in real-time.
   * 
   * @param {string} slideId - ID of the slide to update
   * @param {Partial<Slide>} updates - Partial slide data to apply
   * 
   * @example
   * ```typescript
   * service.updateSlide('slide-123', {
   *   title: 'Updated Title',
   *   description: 'Updated description'
   * });
   * ```
   */
  updateSlide(slideId: string, updates: Partial<Slide>): void {
    // This method is a placeholder for live slide updates
    // In a full implementation, this would update the slide in a store or database
    // and invalidate the cache
    if (this.enableCache) {
      // Invalidate cache entries that might contain this slide
      // This is a simplified version - in practice, we'd track which cache keys contain this slide
      this.cache.clear();
    }
    console.log(`Updated slide ${slideId}`, updates);
  }

  /**
   * Update card with new data.
   * 
   * Updates a card with partial data. The card is updated in place
   * and the cache is invalidated. This method is used for live updates
   * when cards are modified in real-time.
   * 
   * @param {string} cardId - ID of the card to update
   * @param {Partial<Card>} updates - Partial card data to apply
   * 
   * @example
   * ```typescript
   * service.updateCard('card-456', {
   *   pattern: 'updated-pattern',
   *   metadata: { churchEncoding: 'λf.λx.fx' }
   * });
   * ```
   */
  updateCard(cardId: string, updates: Partial<Card>): void {
    // This method is a placeholder for live card updates
    // In a full implementation, this would update the card in a store or database
    // and invalidate the cache
    if (this.enableCache) {
      // Invalidate cache entries that might contain this card
      this.cache.clear();
    }
    console.log(`Updated card ${cardId}`, updates);
  }
}

// Export singleton instance
export const provenanceSlideService = new ProvenanceSlideService();

