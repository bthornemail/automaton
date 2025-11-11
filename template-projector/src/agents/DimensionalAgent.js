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
      // Merge UI components (avoid duplicates)
      const existingUrls = new Set((populatedSlide.uiComponents || []).map(c => c.url || c.content).filter(Boolean));
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
   * @param {Object} slide - Slide object
   * @param {Array} contentData - All content entries (optional, queries ContentLoader if not provided)
   * @returns {Array} Matching content entries
   */
  matchContentEntries(slide, contentData = null) {
    const matches = [];
    
    // Find entries matching this dimension across all sources
    const dimensionEntries = this.contentLoader.findByDimension(this.dimension);
    matches.push(...dimensionEntries);

    // Find entries by tag (e.g., "0D-topology", "0d-topology", "0D-topology-agent")
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

    // Find entries by keyword (dimension-related keywords)
    const keywordMatches = this.contentLoader.findByKeyword(this.dimension.toLowerCase());
    matches.push(...keywordMatches);
    
    // Also search for dimension number (e.g., "0" for "0D")
    const dimNumber = this.dimension.replace(/D$/i, '');
    const numberKeywordMatches = this.contentLoader.findByKeyword(dimNumber);
    matches.push(...numberKeywordMatches);

    // Find topology entries for this dimension (from all entries, not just dimension-matched)
    const allEntries = this.contentLoader.getAllEntries();
    const topologyEntries = allEntries.filter(e => {
      const id = String(e.id || '');
      const tags = Array.isArray(e.tags) ? e.tags : [];
      const keywords = Array.isArray(e.keywords) ? e.keywords : [];
      const hasTopology = id.includes('topology') || tags.includes('topology') || keywords.includes('topology');
      const hasDimension = id.includes(this.dimension) || id.includes(dimLower) || 
                          tags.some(t => String(t).includes(this.dimension) || String(t).includes(dimLower)) ||
                          keywords.some(k => String(k).includes(this.dimension) || String(k).includes(dimLower));
      return hasTopology && hasDimension;
    });
    matches.push(...topologyEntries);

    // Find system entries for this dimension
    const systemEntries = allEntries.filter(e => {
      const id = String(e.id || '');
      const tags = Array.isArray(e.tags) ? e.tags : [];
      const keywords = Array.isArray(e.keywords) ? e.keywords : [];
      const hasSystem = id.includes('system') || tags.includes('system');
      const hasDimension = id.includes(this.dimension) || id.includes(dimLower) || 
                          tags.some(t => String(t).includes(this.dimension) || String(t).includes(dimLower)) ||
                          keywords.some(k => String(k).includes(this.dimension) || String(k).includes(dimLower));
      return hasSystem && hasDimension;
    });
    matches.push(...systemEntries);

    // Find automaton entries for this dimension
    const automatonEntries = allEntries.filter(e => {
      const id = String(e.id || '');
      const tags = Array.isArray(e.tags) ? e.tags : [];
      const keywords = Array.isArray(e.keywords) ? e.keywords : [];
      const hasAutomaton = e.type === 'automaton' || id.includes('automaton') || tags.includes('automaton');
      const hasDimension = id.includes(this.dimension) || id.includes(dimLower) || 
                          tags.some(t => String(t).includes(this.dimension) || String(t).includes(dimLower)) ||
                          keywords.some(k => String(k).includes(this.dimension) || String(k).includes(dimLower));
      return hasAutomaton && hasDimension;
    });
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

    // Process frontmatter documents first (they have structured metadata and body content)
    const frontmatterDocs = entries.filter(e => e.type === 'document' && e.frontmatter);
    
    // Accumulate content from ALL matching documents (not just first)
    const contentParts = [];
    const titles = [];
    const descriptions = [];
    
    for (const doc of frontmatterDocs) {
      // Collect titles
      if (doc.title) {
        titles.push(doc.title);
      }
      
      // Collect descriptions
      if (doc.description) {
        descriptions.push(doc.description);
      }
      
      // Prioritize body content (actual markdown content) - accumulate ALL bodies
      if (doc.body && doc.body.trim()) {
        const bodyContent = doc.body.trim();
        // Add document header if we have title
        if (doc.title) {
          contentParts.push(`# ${doc.title}\n\n${bodyContent}`);
        } else {
          contentParts.push(bodyContent);
        }
      } else if (doc.frontmatter) {
        // Fallback: Build content from frontmatter metadata if no body
        const docParts = [];
        if (doc.title) docParts.push(`# ${doc.title}`);
        if (doc.description) docParts.push(doc.description);
        if (doc.tags && doc.tags.length > 0) {
          docParts.push(`**Tags:** ${doc.tags.join(', ')}`);
        }
        if (doc.keywords && doc.keywords.length > 0) {
          docParts.push(`**Keywords:** ${doc.keywords.join(', ')}`);
        }
        if (doc.level) docParts.push(`**Level:** ${doc.level}`);
        if (doc.docType) docParts.push(`**Type:** ${doc.docType}`);
        
        if (docParts.length > 0) {
          contentParts.push(docParts.join('\n\n'));
        }
      }
    }
    
    // Set title from first document
    if (titles.length > 0 && !result.title) {
      result.title = titles[0];
    }
    
    // Set description from first document
    if (descriptions.length > 0 && !result.description) {
      result.description = descriptions[0];
    }
    
    // Combine ALL content parts
    if (contentParts.length > 0) {
      result.content = contentParts.join('\n\n---\n\n');
    }

    // Process kernel entries (they have `text` property, not `body`)
    const kernelEntries = entries.filter(e => e.text && !e.type);
    for (const kernelEntry of kernelEntries) {
      if (kernelEntry.text && kernelEntry.text.trim()) {
        const kernelText = kernelEntry.text.trim();
        
        // Extract title from markdown (first # heading)
        const titleMatch = kernelText.match(/^#\s+(.+)$/m);
        if (titleMatch && !result.title) {
          result.title = titleMatch[1].trim();
        }
        
        // Add kernel text to content
        if (kernelEntry.id && kernelEntry.id.includes('topology')) {
          // Topology entries get priority header
          contentParts.unshift(kernelText); // Add to beginning
        } else {
          contentParts.push(kernelText);
        }
        
        // Extract description (first paragraph after title)
        if (!result.description) {
          const descMatch = kernelText.match(/^\*\*(.+?)\*\*\s*\n\n(.+?)(?:\n\n|\*|$)/s);
          if (descMatch) {
            result.description = descMatch[2].trim();
          }
        }
      }
    }
    
    // Recombine all content parts (frontmatter + kernel)
    if (contentParts.length > 0) {
      result.content = contentParts.join('\n\n---\n\n');
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
   * Extract UI components from kernel entries (including images, diagrams, quotes)
   * @param {Array} entries - Kernel entries
   * @returns {Array} UI component objects
   */
  extractUIComponents(entries) {
    const components = [];

    for (const entry of entries) {
      const content = entry.text || entry.body || '';
      if (!content) continue;
      
      // Parse markdown to extract structured content
      const lines = content.split('\n');
      let currentSection = null;
      let inCodeBlock = false;
      let codeBlockLang = '';
      let codeBlockContent = [];
      
      for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        
        // Code blocks (fenced)
        if (line.match(/^```(\w+)?$/)) {
          if (inCodeBlock) {
            // End of code block
            const codeContent = codeBlockContent.join('\n');
            const isDiagram = codeBlockLang && ['mermaid', 'graphviz', 'dot', 'plantuml', 'diagram'].includes(codeBlockLang.toLowerCase());
            
            if (isDiagram) {
              components.push({
                type: 'diagram',
                format: codeBlockLang.toLowerCase(),
                content: codeContent,
                language: codeBlockLang
              });
            } else {
              components.push({
                type: 'code-block',
                language: codeBlockLang || 'text',
                content: codeContent
              });
            }
            
            codeBlockContent = [];
            codeBlockLang = '';
            inCodeBlock = false;
          } else {
            // Start of code block
            inCodeBlock = true;
            codeBlockLang = line.match(/^```(\w+)?$/)[1] || '';
          }
          continue;
        }
        
        if (inCodeBlock) {
          codeBlockContent.push(line);
          continue;
        }
        
        // Images: ![alt](url) or ![alt](url "title")
        const imageMatch = line.match(/!\[([^\]]*)\]\(([^)]+)(?:\s+"([^"]+)")?\)/);
        if (imageMatch) {
          components.push({
            type: 'image',
            alt: imageMatch[1] || '',
            url: imageMatch[2],
            title: imageMatch[3] || imageMatch[1] || ''
          });
          continue;
        }
        
        // Blockquotes: > quote text
        if (line.match(/^>\s+(.+)$/)) {
          const quoteText = line.replace(/^>\s+/, '').trim();
          components.push({
            type: 'quote',
            text: quoteText,
            author: null // Could extract from next line or metadata
          });
          continue;
        }
        
        // Headings become section headers
        if (line.match(/^#+\s+(.+)$/)) {
          const title = line.replace(/^#+\s+/, '').trim();
          currentSection = {
            type: 'section',
            title: title,
            content: []
          };
          components.push(currentSection);
          continue;
        }
        
        // Bold text becomes emphasis
        if (line.match(/^\*\*(.+?)\*\*/)) {
          const emphasis = line.replace(/\*\*(.+?)\*\*/g, '$1').trim();
          if (currentSection) {
            currentSection.content.push({ type: 'emphasis', text: emphasis });
          } else {
            components.push({ type: 'emphasis', text: emphasis });
          }
          continue;
        }
        
        // List items
        if (line.match(/^-\s+(.+)$/)) {
          const item = line.replace(/^-\s+/, '').trim();
          if (currentSection) {
            currentSection.content.push({ type: 'list-item', text: item });
          } else {
            components.push({ type: 'list-item', text: item });
          }
          continue;
        }
        
        // Inline code
        if (line.match(/^`(.+?)`$/)) {
          const code = line.replace(/^`(.+?)`$/, '$1');
          if (currentSection) {
            currentSection.content.push({ type: 'code', text: code });
          } else {
            components.push({ type: 'code', text: code });
          }
          continue;
        }
        
        // Regular text
        if (line.trim() && !line.match(/^[*#->]/)) {
          if (currentSection) {
            currentSection.content.push({ type: 'text', text: line.trim() });
          }
        }
      }
      
      // Also check for images in body content (frontmatter documents)
      if (entry.body) {
        const imageRegex = /!\[([^\]]*)\]\(([^)]+)(?:\s+"([^"]+)")?\)/g;
        let match;
        while ((match = imageRegex.exec(entry.body)) !== null) {
          components.push({
            type: 'image',
            alt: match[1] || '',
            url: match[2],
            title: match[3] || match[1] || ''
          });
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

