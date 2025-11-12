/**
 * DimensionalAgent - Base class for all dimensional agents (0D-7D)
 * 
 * Provides common functionality for extracting content from kernel entries
 * and populating slide templates.
 */

import { ContentLoader } from './ContentLoader';

export interface Slide {
  id?: string;
  type?: string;
  title?: string;
  dimension?: string;
  description?: string;
  content?: string;
  text?: string;
  rdfTriples?: any[];
  uiComponents?: any[];
  _populated?: boolean;
  _populatedBy?: string;
  _populatedAt?: string;
  [key: string]: any;
}

export class DimensionalAgent {
  protected dimension: string; // e.g., "0D", "1D", etc.
  protected contentLoader: ContentLoader;
  public name: string;

  constructor(dimension: string, contentLoader: ContentLoader) {
    this.dimension = dimension;
    this.contentLoader = contentLoader;
    this.name = `${dimension}-Agent`;
  }

  /**
   * Populate a slide with content from all sources
   */
  async populateSlide(slide: Slide, contentData: any[] | null = null): Promise<Slide> {
    // Load content if not provided
    if (!contentData) {
      await this.contentLoader.loadAll();
      contentData = this.contentLoader.getAllEntries();
    }

    // Find matching content entries
    const matchingEntries = this.matchContentEntries(slide, contentData);
    
    console.log(`[${this.name}] Populating slide ${slide.id || 'unnamed'} (${this.dimension}): found ${matchingEntries.length} content entries`);
    
    if (matchingEntries.length === 0) {
      console.warn(`[${this.name}] No content entries found for ${this.dimension} slide: ${slide.id || 'unnamed'}`);
      return slide;
    }

    // Extract and populate content
    const populatedSlide = { ...slide };
    
    // Extract text content
    const textContent = this.extractText(matchingEntries);
    
    // Always add/merge content from kernel
    if (textContent.title) {
      if (populatedSlide.title && populatedSlide.title !== textContent.title) {
        (populatedSlide as any).subtitle = textContent.title;
      } else if (!populatedSlide.title) {
        populatedSlide.title = textContent.title;
      }
    }
    
    if (textContent.content) {
      if (populatedSlide.content) {
        populatedSlide.content = populatedSlide.content + '\n\n---\n\n' + textContent.content;
      } else {
        populatedSlide.content = textContent.content;
      }
    }
    
    if (textContent.description) {
      if (populatedSlide.description) {
        populatedSlide.description = populatedSlide.description + '\n\n' + textContent.description;
      } else {
        populatedSlide.description = textContent.description;
      }
    }

    // Extract RDF triples
    const rdfTriples = this.extractRDF(matchingEntries, slide);
    if (rdfTriples.length > 0) {
      if (!populatedSlide.rdfTriples) {
        populatedSlide.rdfTriples = [];
      }
      populatedSlide.rdfTriples.push(...rdfTriples);
    }

    // Extract UI components
    const uiComponents = this.extractUIComponents(matchingEntries);
    if (uiComponents.length > 0) {
      if (!populatedSlide.uiComponents) {
        populatedSlide.uiComponents = [];
      }
      // Merge UI components (avoid duplicates)
      const existingUrls = new Set((populatedSlide.uiComponents || []).map((c: any) => c.url || c.content).filter(Boolean));
      for (const component of uiComponents) {
        const key = component.url || component.content || JSON.stringify(component);
        if (!existingUrls.has(key)) {
          populatedSlide.uiComponents.push(component);
          existingUrls.add(key);
        }
      }
    }

    // Mark as populated
    populatedSlide._populated = true;
    populatedSlide._populatedBy = this.name;
    populatedSlide._populatedAt = new Date().toISOString();

    return populatedSlide;
  }

  /**
   * Match content entries to slide across all sources
   */
  protected matchContentEntries(slide: Slide, contentData: any[] | null = null): any[] {
    const matches: any[] = [];
    
    // Find entries matching this dimension across all sources
    const dimensionEntries = this.contentLoader.findByDimension(this.dimension);
    matches.push(...dimensionEntries);

    // Find entries by tag
    const dimLower = this.dimension.toLowerCase();
    const tagVariations = [
      `${this.dimension}-topology`,
      `${dimLower}-topology`,
      `${this.dimension}-topology-agent`,
      `${dimLower}-topology-agent`,
      `${this.dimension}-agent`,
      `${dimLower}-agent`
    ];
    for (const tag of tagVariations) {
      const tagMatches = this.contentLoader.findByTag(tag);
      matches.push(...tagMatches);
    }

    // Find entries by keyword
    const keywordMatches = this.contentLoader.findByKeyword(this.dimension.toLowerCase());
    matches.push(...keywordMatches);
    
    // Also search for dimension number
    const dimNumber = this.dimension.replace(/D$/i, '');
    const numberKeywordMatches = this.contentLoader.findByKeyword(dimNumber);
    matches.push(...numberKeywordMatches);

    // Find topology/system/automaton entries for this dimension
    const allEntries = this.contentLoader.getAllEntries();
    const topologyEntries = allEntries.filter((e: any) => {
      const id = String(e.id || '');
      const tags = Array.isArray(e.tags) ? e.tags : [];
      const keywords = Array.isArray(e.keywords) ? e.keywords : [];
      const hasTopology = id.includes('topology') || tags.includes('topology') || keywords.includes('topology');
      const hasDimension = id.includes(this.dimension) || id.includes(dimLower) || 
                          tags.some((t: any) => String(t).includes(this.dimension) || String(t).includes(dimLower)) ||
                          keywords.some((k: any) => String(k).includes(this.dimension) || String(k).includes(dimLower));
      return hasTopology && hasDimension;
    });
    matches.push(...topologyEntries);

    // Remove duplicates
    const uniqueMatches: any[] = [];
    const seenIds = new Set<string>();
    for (const entry of matches) {
      const entryId = entry.id || entry.filePath;
      if (entryId && !seenIds.has(entryId)) {
        seenIds.add(entryId);
        uniqueMatches.push(entry);
      }
    }

    return uniqueMatches;
  }

  /**
   * Extract text content from content entries
   */
  protected extractText(entries: any[]): { title: string | null; content: string | null; description: string | null } {
    const result = {
      title: null as string | null,
      content: null as string | null,
      description: null as string | null
    };

    // Process frontmatter documents first
    const frontmatterDocs = entries.filter((e: any) => e.type === 'document' && e.frontmatter);
    
    const contentParts: string[] = [];
    const titles: string[] = [];
    const descriptions: string[] = [];
    
    for (const doc of frontmatterDocs) {
      if (doc.title) {
        titles.push(doc.title);
      }
      
      if (doc.description) {
        descriptions.push(doc.description);
      }
      
      if (doc.body && doc.body.trim()) {
        const bodyContent = doc.body.trim();
        if (doc.title) {
          contentParts.push(`# ${doc.title}\n\n${bodyContent}`);
        } else {
          contentParts.push(bodyContent);
        }
      }
    }
    
    if (titles.length > 0 && !result.title) {
      result.title = titles[0];
    }
    
    if (descriptions.length > 0 && !result.description) {
      result.description = descriptions[0];
    }
    
    if (contentParts.length > 0) {
      result.content = contentParts.join('\n\n---\n\n');
    }

    // Process kernel entries
    const kernelEntries = entries.filter((e: any) => e.text && !e.type);
    for (const kernelEntry of kernelEntries) {
      if (kernelEntry.text && kernelEntry.text.trim()) {
        const kernelText = kernelEntry.text.trim();
        
        const titleMatch = kernelText.match(/^#\s+(.+)$/m);
        if (titleMatch && !result.title) {
          result.title = titleMatch[1].trim();
        }
        
        if (kernelEntry.id && kernelEntry.id.includes('topology')) {
          contentParts.unshift(kernelText);
        } else {
          contentParts.push(kernelText);
        }
      }
    }
    
    if (contentParts.length > 0) {
      result.content = contentParts.join('\n\n---\n\n');
    }

    return result;
  }

  /**
   * Extract RDF triples from kernel entries
   */
  protected extractRDF(entries: any[], slide: Slide): any[] {
    const triples: any[] = [];

    // Create sameAs links to kernel entries
    for (const entry of entries) {
      if (entry.id) {
        triples.push({
          type: 'rdf-triple',
          subject: `#${slide.id || 'slide'}`,
          predicate: 'schema:sameAs',
          object: `kernel:${entry.id}`
        });
      }
    }

    // Extract relationships from ContentLoader
    for (const entry of entries) {
      if (entry.id) {
        const relationships = this.contentLoader.getRelationships(entry.id);
        
        // Vertical relationships
        for (const rel of relationships.vertical || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${rel.fromNode}`,
            predicate: 'rdfs:subClassOf',
            object: `#${rel.toNode}`
          });
        }

        // Horizontal relationships
        for (const rel of relationships.horizontal || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${rel.fromNode}`,
            predicate: 'rdfs:seeAlso',
            object: `#${rel.toNode}`
          });
        }
      }
    }

    return triples;
  }

  /**
   * Extract UI components from kernel entries
   */
  protected extractUIComponents(entries: any[]): any[] {
    const components: any[] = [];

    for (const entry of entries) {
      const content = entry.text || entry.body || '';
      if (!content) continue;
      
      // Parse markdown to extract images
      const lines = content.split('\n');
      
      for (const line of lines) {
        // Images: ![alt](url)
        const imageMatch = line.match(/!\[([^\]]*)\]\(([^)]+)\)/);
        if (imageMatch) {
          components.push({
            type: 'image',
            alt: imageMatch[1] || '',
            url: imageMatch[2],
            title: imageMatch[1] || ''
          });
        }
      }
    }

    return components;
  }
}

