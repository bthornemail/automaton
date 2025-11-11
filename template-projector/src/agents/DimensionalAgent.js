/**
 * DimensionalAgent - Base class for all dimensional agents (0D-7D)
 * 
 * Provides common functionality for extracting content from kernel entries
 * and populating slide templates.
 */

export class DimensionalAgent {
  constructor(dimension, contentLoader) {
    this.dimension = dimension; // e.g., "0D", "1D", etc.
    this.contentLoader = contentLoader; // Unified ContentLoader
    this.name = `${dimension}-Agent`;
  }

  /**
   * Populate a slide with content from all sources
   * @param {Object} slide - Slide object to populate
   * @param {Array} contentData - Content entries (optional, will load if not provided)
   * @returns {Promise<Object>} Populated slide
   */
  async populateSlide(slide, contentData = null) {
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
    console.log(`[${this.name}] Extracted text content:`, { 
      hasTitle: !!textContent.title, 
      hasContent: !!textContent.content, 
      hasDescription: !!textContent.description 
    });
    
    // Always add/merge content from kernel (don't skip if slide already has content)
    if (textContent.title) {
      // If slide already has a title, append kernel title as subtitle
      if (populatedSlide.title && populatedSlide.title !== textContent.title) {
        populatedSlide.subtitle = textContent.title;
        console.log(`[${this.name}] Added subtitle: ${textContent.title}`);
      } else if (!populatedSlide.title) {
        populatedSlide.title = textContent.title;
        console.log(`[${this.name}] Set title: ${textContent.title}`);
      }
    }
    
    if (textContent.content) {
      // Always add content - merge if exists, otherwise set
      if (populatedSlide.content) {
        populatedSlide.content = populatedSlide.content + '\n\n---\n\n' + textContent.content;
        console.log(`[${this.name}] Merged content (${textContent.content.length} chars)`);
      } else {
        populatedSlide.content = textContent.content;
        console.log(`[${this.name}] Set content (${textContent.content.length} chars)`);
      }
    }
    
    if (textContent.description) {
      // Merge description
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
      populatedSlide.uiComponents.push(...uiComponents);
    }

    // Mark as populated
    populatedSlide._populated = true;
    populatedSlide._populatedBy = this.name;
    populatedSlide._populatedAt = new Date().toISOString();

    return populatedSlide;
  }

  /**
   * Match content entries to slide across all sources
   * @param {Object} slide - Slide object
   * @param {Array} contentData - All content entries (optional, queries ContentLoader if not provided)
   * @returns {Array} Matching content entries
   */
  matchContentEntries(slide, contentData = null) {
    const matches = [];
    
    // Find entries matching this dimension across all sources
    const dimensionEntries = this.contentLoader.findByDimension(this.dimension);
    matches.push(...dimensionEntries);

    // Find entries by tag (e.g., "0D-topology")
    const tagMatches = this.contentLoader.findByTag(`${this.dimension}-topology`);
    matches.push(...tagMatches);

    // Find entries by keyword (dimension-related keywords)
    const keywordMatches = this.contentLoader.findByKeyword(this.dimension.toLowerCase());
    matches.push(...keywordMatches);

    // Find topology entries for this dimension
    const topologyEntries = dimensionEntries.filter(e => 
      e.id && (e.id.includes('topology') || e.tags?.includes('topology'))
    );
    matches.push(...topologyEntries);

    // Find system entries for this dimension
    const systemEntries = dimensionEntries.filter(e => 
      e.id && (e.id.includes('system') || e.tags?.includes('system'))
    );
    matches.push(...systemEntries);

    // Find automaton entries for this dimension
    const automatonEntries = dimensionEntries.filter(e => 
      e.type === 'automaton' || (e.id && e.id.includes('automaton')) || e.tags?.includes('automaton')
    );
    matches.push(...automatonEntries);

    // Remove duplicates
    const uniqueMatches = [];
    const seenIds = new Set();
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
   * Extract text content from content entries (kernel + frontmatter)
   * @param {Array} entries - Content entries
   * @returns {Object} Text content with title, content, description
   */
  extractText(entries) {
    const result = {
      title: null,
      content: null,
      description: null
    };

    // Process frontmatter documents first (they have structured metadata)
    const frontmatterDocs = entries.filter(e => e.type === 'document' && e.frontmatter);
    for (const doc of frontmatterDocs) {
      // Use title from frontmatter
      if (doc.title && !result.title) {
        result.title = doc.title;
      }
      
      // Use description from frontmatter
      if (doc.description && !result.description) {
        result.description = doc.description;
      }
      
      // Build content from frontmatter metadata
      if (doc.frontmatter) {
        const contentParts = [];
        if (doc.title) contentParts.push(`# ${doc.title}`);
        if (doc.description) contentParts.push(doc.description);
        if (doc.tags && doc.tags.length > 0) {
          contentParts.push(`\n**Tags:** ${doc.tags.join(', ')}`);
        }
        if (doc.keywords && doc.keywords.length > 0) {
          contentParts.push(`\n**Keywords:** ${doc.keywords.join(', ')}`);
        }
        if (doc.level) contentParts.push(`\n**Level:** ${doc.level}`);
        if (doc.docType) contentParts.push(`\n**Type:** ${doc.docType}`);
        
        const docContent = contentParts.join('\n\n');
        if (docContent) {
          if (!result.content) {
            result.content = docContent;
          } else {
            result.content += '\n\n---\n\n' + docContent;
          }
        }
      }
    }

    // Find topology entry for title (kernel entries)
    const topologyEntry = entries.find(e => e.id && e.id.includes('topology') && e.text);
    if (topologyEntry && topologyEntry.text) {
      // Extract title from markdown (first # heading)
      const titleMatch = topologyEntry.text.match(/^#\s+(.+)$/m);
      if (titleMatch && !result.title) {
        result.title = titleMatch[1].trim();
      }
      
      // Use full text as content
      if (!result.content) {
        result.content = topologyEntry.text;
      } else {
        result.content += '\n\n---\n\n' + topologyEntry.text;
      }
    }

    // Find system entry for additional content (kernel entries)
    const systemEntry = entries.find(e => e.id && e.id.includes('system') && e.text);
    if (systemEntry && systemEntry.text) {
      if (!result.content) {
        result.content = systemEntry.text;
      } else {
        result.content += '\n\n---\n\n' + systemEntry.text;
      }
      
      // Extract description (first paragraph after title)
      if (!result.description) {
        const descMatch = systemEntry.text.match(/^\*\*(.+?)\*\*\s*\n\n(.+?)(?:\n\n|\*|$)/s);
        if (descMatch) {
          result.description = descMatch[2].trim();
        }
      }
    }

    // Fallback: use first entry's text (kernel entries)
    if (!result.content && entries.length > 0 && entries[0].text) {
      result.content = entries[0].text;
      const titleMatch = entries[0].text.match(/^#\s+(.+)$/m);
      if (titleMatch && !result.title) {
        result.title = titleMatch[1].trim();
      }
    }

    return result;
  }

  /**
   * Extract RDF triples from kernel entries
   * @param {Array} entries - Kernel entries
   * @param {Object} slide - Slide object
   * @returns {Array} RDF triple objects
   */
  extractRDF(entries, slide) {
    const triples = [];

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
        
        // Vertical relationships (dimensional progression)
        for (const rel of relationships.vertical || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${rel.fromNode}`,
            predicate: 'rdfs:subClassOf',
            object: `#${rel.toNode}`
          });
        }

        // Horizontal relationships (topology ↔ system)
        for (const rel of relationships.horizontal || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${rel.fromNode}`,
            predicate: 'rdfs:seeAlso',
            object: `#${rel.toNode}`
          });
        }

        // Prerequisites relationships
        for (const prereq of relationships.prerequisites || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${entry.id}`,
            predicate: 'rdfs:prerequisite',
            object: `#${prereq.toNode || prereq}`
          });
        }

        // Enables relationships
        for (const enable of relationships.enables || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${entry.id}`,
            predicate: 'rdfs:enables',
            object: `#${enable.toNode || enable}`
          });
        }

        // Related relationships
        for (const related of relationships.related || []) {
          triples.push({
            type: 'rdf-triple',
            subject: `#${entry.id}`,
            predicate: 'rdfs:seeAlso',
            object: `#${related.toNode || related}`
          });
        }

        // Also get RDF triples directly from ContentLoader
        const directTriples = this.contentLoader.getRDFTriples(entry.id);
        triples.push(...directTriples);
      }
    }

    return triples;
  }

  /**
   * Extract UI components from kernel entries
   * @param {Array} entries - Kernel entries
   * @returns {Array} UI component objects
   */
  extractUIComponents(entries) {
    const components = [];

    for (const entry of entries) {
      if (entry.text) {
        // Parse markdown to extract structured content
        const lines = entry.text.split('\n');
        let currentSection = null;
        
        for (const line of lines) {
          // Headings become section headers
          if (line.match(/^#+\s+(.+)$/)) {
            const title = line.replace(/^#+\s+/, '').trim();
            currentSection = {
              type: 'section',
              title: title,
              content: []
            };
            components.push(currentSection);
          }
          // Bold text becomes emphasis
          else if (line.match(/^\*\*(.+?)\*\*/)) {
            const emphasis = line.replace(/\*\*(.+?)\*\*/g, '$1').trim();
            if (currentSection) {
              currentSection.content.push({ type: 'emphasis', text: emphasis });
            } else {
              components.push({ type: 'emphasis', text: emphasis });
            }
          }
          // List items
          else if (line.match(/^-\s+(.+)$/)) {
            const item = line.replace(/^-\s+/, '').trim();
            if (currentSection) {
              currentSection.content.push({ type: 'list-item', text: item });
            } else {
              components.push({ type: 'list-item', text: item });
            }
          }
          // Code blocks
          else if (line.match(/^`(.+?)`$/)) {
            const code = line.replace(/^`(.+?)`$/, '$1');
            if (currentSection) {
              currentSection.content.push({ type: 'code', text: code });
            } else {
              components.push({ type: 'code', text: code });
            }
          }
          // Regular text
          else if (line.trim() && !line.match(/^[*#-]/)) {
            if (currentSection) {
              currentSection.content.push({ type: 'text', text: line.trim() });
            }
          }
        }
      }

      // Create card component for relationships
      if (entry.id) {
        const relationships = this.contentLoader.getRelationships(entry.id);
        const hasRelationships = 
          (relationships.vertical && relationships.vertical.length > 0) ||
          (relationships.horizontal && relationships.horizontal.length > 0) ||
          (relationships.prerequisites && relationships.prerequisites.length > 0) ||
          (relationships.enables && relationships.enables.length > 0) ||
          (relationships.related && relationships.related.length > 0);
        
        if (hasRelationships) {
          components.push({
            type: 'card',
            title: `Relationships: ${entry.id}`,
            relationships: {
              vertical: (relationships.vertical || []).map(r => ({
                from: r.fromNode,
                to: r.toNode,
                label: r.label
              })),
              horizontal: (relationships.horizontal || []).map(r => ({
                from: r.fromNode,
                to: r.toNode,
                label: r.label
              })),
              prerequisites: (relationships.prerequisites || []).map(r => ({
                from: entry.id,
                to: r.toNode || r,
                label: 'prerequisite'
              })),
              enables: (relationships.enables || []).map(r => ({
                from: entry.id,
                to: r.toNode || r,
                label: 'enables'
              })),
              related: (relationships.related || []).map(r => ({
                from: entry.id,
                to: r.toNode || r,
                label: 'related'
              }))
            }
          });
        }
      }
    }

    return components;
  }

  /**
   * Get agent name
   * @returns {string} Agent name
   */
  getName() {
    return this.name;
  }

  /**
   * Get dimension
   * @returns {string} Dimension string
   */
  getDimension() {
    return this.dimension;
  }
}

