/**
 * TopicSlideGenerator - Generates slides dynamically based on user topics
 * 
 * Aligns with bipartite-BQF specifications for dimension, partition, and BQF forms
 */

export interface Slide {
  id: string;
  type: string;
  title?: string;
  dimension?: string;
  description?: string;
  content?: string;
  frontmatter?: {
    bipartite?: {
      partition?: string;
      dimension?: string;
      bqf?: any;
    };
  };
  metadata?: any;
  _populated?: boolean;
  _generated?: boolean;
  _populatedBy?: string;
  [key: string]: any;
}

export interface TopicSlideOptions {
  dimension?: string;
  partition?: string;
  force?: boolean;
  includeProvenance?: boolean;
}

export interface ContentEntry {
  type?: string;
  dimension?: string;
  title?: string;
  description?: string;
  tags?: string[];
  frontmatter?: {
    bipartite?: {
      partition?: string;
      dimension?: string;
      bqf?: any;
    };
  };
  [key: string]: any;
}

export class TopicSlideGenerator {
  private agentCoordinator: any; // AgentCoordinator
  private contentLoader: any; // ContentLoader
  private generatedSlides: Map<string, Slide>;

  constructor(agentCoordinator: any, contentLoader: any) {
    this.agentCoordinator = agentCoordinator;
    this.contentLoader = contentLoader;
    this.generatedSlides = new Map(); // Cache generated slides by topic
  }

  /**
   * Generate slide from user topic input
   */
  async generateSlideFromTopic(topic: string, options: TopicSlideOptions = {}): Promise<Slide> {
    if (!topic || typeof topic !== 'string' || topic.trim().length === 0) {
      throw new Error('Topic must be a non-empty string');
    }

    const normalizedTopic = topic.trim().toLowerCase();
    
    // Check cache
    if (this.generatedSlides.has(normalizedTopic) && !options.force) {
      return this.generatedSlides.get(normalizedTopic)!;
    }

    // Determine dimension from topic keywords
    const dimension = this.inferDimensionFromTopic(normalizedTopic, options.dimension);
    
    // Determine partition (topology vs system)
    const partition = this.inferPartitionFromTopic(normalizedTopic, options.partition);
    
    // Find matching content entries
    await this.contentLoader.loadAll();
    const allEntries = this.contentLoader.getAllEntries();
    
    const matchingEntries = this.findMatchingEntries(normalizedTopic, dimension, partition, allEntries);
    
    // Generate BQF metadata if dimension specified
    const bqfMetadata = dimension ? this.generateBQFMetadata(dimension, partition) : null;
    
    // Build slide object aligned with bipartite-BQF spec
    const slide: Slide = {
      id: `topic-${normalizedTopic.replace(/[^a-z0-9-]/g, '-')}-${Date.now()}`,
      type: 'slide',
      title: this.generateTitle(topic, dimension, matchingEntries),
      dimension: dimension,
      description: this.generateDescription(topic, dimension, matchingEntries),
      content: this.generateContent(topic, dimension, matchingEntries),
      frontmatter: {
        bipartite: {
          partition: partition,
          dimension: dimension,
          ...(bqfMetadata ? { bqf: bqfMetadata } : {})
        }
      },
      metadata: {
        topic: topic,
        generatedAt: new Date().toISOString(),
        source: 'user-input',
        matchingEntriesCount: matchingEntries.length
      },
      _populated: false,
      _generated: true
    };

    // Populate slide with agent content
    if (this.agentCoordinator && dimension) {
      try {
        const populatedSlide = await this.agentCoordinator.populateSlide(slide);
        slide.content = populatedSlide.content || slide.content;
        slide.title = populatedSlide.title || slide.title;
        slide.description = populatedSlide.description || slide.description;
        slide._populated = true;
        slide._populatedBy = `${dimension}-Agent`;
      } catch (error) {
        console.warn('Failed to populate slide with agent content:', error);
      }
    }

    // Cache generated slide
    this.generatedSlides.set(normalizedTopic, slide);
    
    return slide;
  }

  /**
   * Infer dimension from topic keywords
   */
  private inferDimensionFromTopic(topic: string, explicitDimension: string | null | undefined): string {
    if (explicitDimension) {
      return String(explicitDimension).trim().toUpperCase().replace(/D+$/i, '') + 'D';
    }

    const topicLower = topic.toLowerCase();
    
    // Dimension-specific keywords
    const dimensionKeywords: Record<string, string[]> = {
      '0D': ['quantum', 'vacuum', 'empty', 'zero', 'identity', 'base', 'foundation'],
      '1D': ['temporal', 'time', 'successor', 'line', 'sequence', 'evolution'],
      '2D': ['structural', 'spatial', 'pair', 'pattern', 'structure', 'space'],
      '3D': ['algebraic', 'algebra', 'add', 'multiply', 'operation', 'math'],
      '4D': ['network', 'spacetime', 'ipv4', 'ipv6', 'localhost', 'ci/cd', 'deployment'],
      '5D': ['consensus', 'blockchain', 'distributed', 'ledger', 'merkle', 'approval'],
      '6D': ['intelligence', 'ai', 'neural', 'network', 'transformer', 'attention', 'learning'],
      '7D': ['quantum', 'superposition', 'entanglement', 'qubit', 'bloch', 'quantum computing']
    };

    // Score each dimension
    const scores: Record<string, number> = {};
    for (const [dim, keywords] of Object.entries(dimensionKeywords)) {
      scores[dim] = keywords.reduce((score, keyword) => {
        return score + (topicLower.includes(keyword) ? 1 : 0);
      }, 0);
    }

    // Find dimension with highest score
    const maxScore = Math.max(...Object.values(scores));
    if (maxScore === 0) {
      return '0D'; // Default to 0D
    }

    const bestDimension = Object.entries(scores).find(([_, score]) => score === maxScore)?.[0];
    return bestDimension || '0D';
  }

  /**
   * Infer partition from topic keywords
   */
  private inferPartitionFromTopic(topic: string, explicitPartition: string | null | undefined): string {
    if (explicitPartition === 'topology' || explicitPartition === 'system') {
      return explicitPartition;
    }

    const topicLower = topic.toLowerCase();
    
    // System keywords
    const systemKeywords = ['implementation', 'system', 'network', 'blockchain', 'ai', 'quantum computing', 'deployment', 'ci/cd'];
    
    // Topology keywords
    const topologyKeywords = ['topology', 'mathematical', 'foundation', 'theory', 'structure', 'pattern', 'church'];
    
    const systemScore = systemKeywords.reduce((score, keyword) => 
      score + (topicLower.includes(keyword) ? 1 : 0), 0
    );
    
    const topologyScore = topologyKeywords.reduce((score, keyword) => 
      score + (topicLower.includes(keyword) ? 1 : 0), 0
    );

    return systemScore > topologyScore ? 'system' : 'topology';
  }

  /**
   * Find matching content entries for topic
   */
  private findMatchingEntries(topic: string, dimension: string, partition: string, allEntries: ContentEntry[]): ContentEntry[] {
    const matches: Array<{ entry: ContentEntry; score: number }> = [];
    const topicWords = topic.split(/\s+/).filter(w => w.length > 2);
    
    for (const entry of allEntries) {
      if (entry.type !== 'document') continue;
      
      let score = 0;
      
      // Dimension match
      if (entry.dimension === dimension || entry.frontmatter?.bipartite?.dimension === dimension) {
        score += 3;
      }
      
      // Partition match
      if (entry.frontmatter?.bipartite?.partition === partition) {
        score += 2;
      }
      
      // Title match
      if (entry.title) {
        const titleLower = entry.title.toLowerCase();
        score += topicWords.reduce((s, word) => s + (titleLower.includes(word) ? 1 : 0), 0);
      }
      
      // Description match
      if (entry.description) {
        const descLower = entry.description.toLowerCase();
        score += topicWords.reduce((s, word) => s + (descLower.includes(word) ? 0.5 : 0), 0);
      }
      
      // Tag/keyword match
      if (entry.tags) {
        const tagMatch = entry.tags.some(tag => topicWords.some(word => tag.toLowerCase().includes(word)));
        if (tagMatch) score += 1;
      }
      
      if (score > 0) {
        matches.push({ entry, score });
      }
    }
    
    // Sort by score and return top matches
    return matches
      .sort((a, b) => b.score - a.score)
      .slice(0, 10)
      .map(m => m.entry);
  }

  /**
   * Generate BQF metadata aligned with dimension and partition
   */
  private generateBQFMetadata(dimension: string, partition: string): any {
    const dimensionNum = parseInt(dimension.replace('D', ''));
    
    // Expected BQF forms for each dimension
    const bqfForms: Record<string, { form: string; variables: string[]; coefficients: number[] }> = {
      '0D': { form: 'Q() = 0', variables: [], coefficients: [0] },
      '1D': { form: 'Q(x) = x²', variables: ['x'], coefficients: [1] },
      '2D': { form: 'Q(x,y) = x² + y²', variables: ['x', 'y'], coefficients: [1, 0, 1] },
      '3D': { form: 'Q(x,y,z,t) = x² + y² + z² - t²', variables: ['x', 'y', 'z', 't'], coefficients: [1, 0, 1, 0, 1, 0, -1] },
      '4D': { form: 'Q(w,x,y,z,t) = w² + x² + y² + z² - t²', variables: ['w', 'x', 'y', 'z', 't'], coefficients: [1, 0, 1, 0, 1, 0, 1, 0, -1] },
      '5D': { form: 'Q(...) = Σᵢ xᵢ² - t²', variables: Array.from({ length: 5 }, (_, i) => `x${i}`), coefficients: Array(5).fill(1).concat([-1]) },
      '6D': { form: 'Q(...) = Σᵢ xᵢ² - t² + higher terms', variables: Array.from({ length: 6 }, (_, i) => `x${i}`), coefficients: Array(6).fill(1).concat([-1, 0.5]) },
      '7D': { form: 'Q(...) = Σᵢ xᵢ² - t² + quantum terms', variables: Array.from({ length: 7 }, (_, i) => `x${i}`), coefficients: Array(7).fill(1).concat([-1, 0.5, 0.25]) }
    };
    
    const bqf = bqfForms[dimension] || bqfForms['0D'];
    
    // Determine signature based on partition and dimension
    let signature = 'euclidean';
    if (dimension === '3D' || dimension === '4D') {
      signature = 'lorentz'; // Minkowski signature
    } else if (dimension === '5D') {
      signature = 'consensus';
    } else if (dimension === '6D') {
      signature = 'intelligence';
    } else if (dimension === '7D') {
      signature = 'quantum';
    }
    
    return {
      form: bqf.form,
      coefficients: bqf.coefficients,
      signature: signature,
      variables: bqf.variables
    };
  }

  /**
   * Generate slide title
   */
  private generateTitle(topic: string, dimension: string | undefined, matchingEntries: ContentEntry[]): string {
    // Use title from best matching entry if available
    if (matchingEntries.length > 0 && matchingEntries[0].title) {
      return matchingEntries[0].title;
    }
    
    // Generate title from topic
    const topicTitle = topic.split(/\s+/).map(word => 
      word.charAt(0).toUpperCase() + word.slice(1)
    ).join(' ');
    
    return dimension ? `${dimension}: ${topicTitle}` : topicTitle;
  }

  /**
   * Generate slide description
   */
  private generateDescription(topic: string, dimension: string | undefined, matchingEntries: ContentEntry[]): string {
    if (matchingEntries.length > 0 && matchingEntries[0].description) {
      return matchingEntries[0].description;
    }
    
    return `Dynamic slide generated from topic: "${topic}"${dimension ? ` (${dimension})` : ''}`;
  }

  /**
   * Generate slide content aligned with bipartite-BQF specifications
   */
  private generateContent(topic: string, dimension: string | undefined, matchingEntries: ContentEntry[]): string {
    const contentParts: string[] = [];
    
    // Add topic header
    contentParts.push(`# ${this.generateTitle(topic, dimension, matchingEntries)}`);
    contentParts.push('');
    contentParts.push(this.generateDescription(topic, dimension, matchingEntries));
    contentParts.push('');
    
    // Add dimension and partition info if available
    if (dimension) {
      const partition = this.inferPartitionFromTopic(topic, undefined);
      contentParts.push(`**Dimension:** ${dimension}`);
      if (partition) {
        contentParts.push(`**Partition:** ${partition}`);
      }
      contentParts.push('');
    }
    
    // Add BQF information if dimension specified
    if (dimension) {
      const partition = this.inferPartitionFromTopic(topic, undefined);
      const bqfMetadata = this.generateBQFMetadata(dimension, partition);
      if (bqfMetadata) {
        contentParts.push('## Bipartite-BQF Metadata');
        contentParts.push('');
        contentParts.push(`**BQF Form:** ${bqfMetadata.form}`);
        contentParts.push(`**Signature:** ${bqfMetadata.signature}`);
        if (bqfMetadata.variables && bqfMetadata.variables.length > 0) {
          contentParts.push(`**Variables:** ${bqfMetadata.variables.join(', ')}`);
        }
        contentParts.push('');
      }
    }
    
    // Add matching entries content
    if (matchingEntries.length > 0) {
      contentParts.push('## Related Content');
      contentParts.push('');
      
      for (const entry of matchingEntries.slice(0, 5)) {
        if (entry.title) {
          contentParts.push(`### ${entry.title}`);
        }
        if (entry.description) {
          contentParts.push(entry.description);
        }
        if (entry.dimension) {
          contentParts.push(`**Dimension:** ${entry.dimension}`);
        }
        if (entry.frontmatter?.bipartite) {
          const bipartite = entry.frontmatter.bipartite;
          if (bipartite.partition) {
            contentParts.push(`**Partition:** ${bipartite.partition}`);
          }
          if (bipartite.bqf?.form) {
            contentParts.push(`**BQF:** ${bipartite.bqf.form}`);
          }
        }
        if (entry.tags && entry.tags.length > 0) {
          contentParts.push(`**Tags:** ${entry.tags.join(', ')}`);
        }
        contentParts.push('');
      }
    }
    
    return contentParts.join('\n');
  }

  /**
   * Clear generated slides cache
   */
  clearCache(): void {
    this.generatedSlides.clear();
  }
}

