/**
 * Agent Worker - Runs agents in off-canvas Web Worker
 * 
 * Handles slide population and generates edit suggestions
 * 
 * Note: This worker runs in a separate thread and cannot access DOM or main thread globals
 */

self.onmessage = async function(e) {
  const { type, payload } = e.data;
  
  try {
    switch (type) {
      case 'POPULATE_SLIDE':
        await handlePopulateSlide(payload);
        break;
      
      case 'GENERATE_SUGGESTIONS':
        await handleGenerateSuggestions(payload);
        break;
      
      case 'ANALYZE_COMPONENT':
        await handleAnalyzeComponent(payload);
        break;
      
      default:
        self.postMessage({
          type: 'ERROR',
          error: `Unknown message type: ${type}`
        });
    }
  } catch (error) {
    self.postMessage({
      type: 'ERROR',
      error: error.message,
      stack: error.stack
    });
  }
};

/**
 * Handle slide population request
 */
async function handlePopulateSlide(payload) {
  const { slide, contentData, dimension } = payload;
  
  // Simulate agent processing
  const populatedSlide = await populateSlideWithAgent(slide, contentData, dimension);
  
  self.postMessage({
    type: 'POPULATE_SLIDE_RESULT',
    payload: {
      slide: populatedSlide,
      suggestions: generatePopulationSuggestions(populatedSlide, slide)
    }
  });
}

/**
 * Handle suggestion generation request
 */
async function handleGenerateSuggestions(payload) {
  const { slide, component, action } = payload;
  
  const suggestions = await generateEditSuggestions(slide, component, action);
  
  self.postMessage({
    type: 'SUGGESTIONS_RESULT',
    payload: {
      suggestions,
      component,
      action
    }
  });
}

/**
 * Handle component analysis request
 */
async function handleAnalyzeComponent(payload) {
  const { component, slide, contentData } = payload;
  
  const analysis = await analyzeComponent(component, slide, contentData);
  
  self.postMessage({
    type: 'ANALYSIS_RESULT',
    payload: {
      analysis,
      component
    }
  });
}

/**
 * Populate slide using agent logic
 */
async function populateSlideWithAgent(slide, contentData, dimension) {
  // Match content entries by dimension
  const matchingEntries = matchContentEntries(slide, contentData, dimension);
  
  // Extract text content
  const textContent = extractText(matchingEntries);
  
  // Extract UI components
  const uiComponents = extractUIComponents(matchingEntries);
  
  // Build populated slide
  const populatedSlide = {
    ...slide,
    title: textContent.title || slide.title,
    content: textContent.content || slide.content,
    description: textContent.description || slide.description,
    uiComponents: uiComponents,
    _populated: true,
    _populatedBy: `${dimension}-Agent-Worker`
  };
  
  return populatedSlide;
}

/**
 * Match content entries by dimension
 */
function matchContentEntries(slide, contentData, dimension) {
  if (!contentData || !Array.isArray(contentData)) {
    return [];
  }
  
  const dimLower = dimension.toLowerCase();
  const matches = [];
  
  for (const entry of contentData) {
    const id = String(entry.id || '');
    const tags = Array.isArray(entry.tags) ? entry.tags : [];
    const keywords = Array.isArray(entry.keywords) ? entry.keywords : [];
    const entryDimension = entry.dimension || '';
    
    // Match by dimension
    if (id.includes(dimension) || id.includes(dimLower) ||
        entryDimension === dimension || entryDimension === dimLower ||
        tags.some(t => String(t).includes(dimension) || String(t).includes(dimLower)) ||
        keywords.some(k => String(k).includes(dimension) || String(k).includes(dimLower))) {
      matches.push(entry);
    }
  }
  
  return matches;
}

/**
 * Extract text content from entries
 */
function extractText(entries) {
  const result = { title: null, content: null, description: null };
  const contentParts = [];
  
  for (const entry of entries) {
    if (entry.title && !result.title) {
      result.title = entry.title;
    }
    
    if (entry.body && entry.body.trim()) {
      contentParts.push(entry.body.trim());
    } else if (entry.text && entry.text.trim()) {
      contentParts.push(entry.text.trim());
    }
    
    if (entry.description && !result.description) {
      result.description = entry.description;
    }
  }
  
  if (contentParts.length > 0) {
    result.content = contentParts.join('\n\n---\n\n');
  }
  
  return result;
}

/**
 * Extract UI components from entries
 */
function extractUIComponents(entries) {
  const components = [];
  
  for (const entry of entries) {
    const content = entry.text || entry.body || '';
    if (!content) continue;
    
    const lines = content.split('\n');
    let inCodeBlock = false;
    let codeBlockLang = '';
    let codeBlockContent = [];
    
    for (const line of lines) {
      // Code blocks
      if (line.match(/^```(\w+)?$/)) {
        if (inCodeBlock) {
          const codeContent = codeBlockContent.join('\n');
          const isDiagram = codeBlockLang && ['mermaid', 'graphviz', 'dot', 'plantuml'].includes(codeBlockLang.toLowerCase());
          if (isDiagram) {
            components.push({ type: 'diagram', format: codeBlockLang.toLowerCase(), content: codeContent });
          }
          codeBlockContent = [];
          codeBlockLang = '';
          inCodeBlock = false;
        } else {
          inCodeBlock = true;
          codeBlockLang = line.match(/^```(\w+)?$/)[1] || '';
        }
        continue;
      }
      
      if (inCodeBlock) {
        codeBlockContent.push(line);
        continue;
      }
      
      // Images
      const imageMatch = line.match(/!\[([^\]]*)\]\(([^)]+)\)/);
      if (imageMatch) {
        components.push({ type: 'image', alt: imageMatch[1] || '', url: imageMatch[2] });
        continue;
      }
      
      // Quotes
      if (line.match(/^>\s+(.+)$/)) {
        const quoteText = line.replace(/^>\s+/, '').trim();
        components.push({ type: 'quote', text: quoteText });
        continue;
      }
    }
  }
  
  return components;
}

/**
 * Generate suggestions based on population
 */
function generatePopulationSuggestions(populatedSlide, originalSlide) {
  const suggestions = [];
  
  // Suggest title improvements
  if (!populatedSlide.title && originalSlide.title) {
    suggestions.push({
      type: 'ENHANCE_TITLE',
      priority: 'high',
      message: 'Consider enhancing the title with more descriptive content',
      action: 'enhance_title',
      data: { slideId: populatedSlide.id }
    });
  }
  
  // Suggest content additions
  if (!populatedSlide.content || populatedSlide.content.length < 100) {
    suggestions.push({
      type: 'ADD_CONTENT',
      priority: 'medium',
      message: 'Slide content is minimal. Consider adding more details.',
      action: 'add_content',
      data: { slideId: populatedSlide.id }
    });
  }
  
  // Suggest UI component additions
  if (!populatedSlide.uiComponents || populatedSlide.uiComponents.length === 0) {
    suggestions.push({
      type: 'ADD_COMPONENTS',
      priority: 'low',
      message: 'Consider adding images, diagrams, or quotes to enhance visual appeal',
      action: 'add_components',
      data: { slideId: populatedSlide.id }
    });
  }
  
  return suggestions;
}

/**
 * Generate edit suggestions based on component click
 */
async function generateEditSuggestions(slide, component, action) {
  const suggestions = [];
  
  if (!component) {
    return suggestions;
  }
  
  switch (component.type) {
    case 'image':
      suggestions.push({
        type: 'IMAGE_OPTIMIZATION',
        priority: 'medium',
        message: `Optimize image "${component.alt || component.url}"`,
        action: 'optimize_image',
        data: { component, slideId: slide.id }
      });
      suggestions.push({
        type: 'IMAGE_REPLACEMENT',
        priority: 'low',
        message: `Replace or add alternative image for "${component.alt || component.url}"`,
        action: 'replace_image',
        data: { component, slideId: slide.id }
      });
      break;
    
    case 'diagram':
      suggestions.push({
        type: 'DIAGRAM_ENHANCEMENT',
        priority: 'high',
        message: `Enhance diagram with more details or interactive elements`,
        action: 'enhance_diagram',
        data: { component, slideId: slide.id }
      });
      break;
    
    case 'quote':
      suggestions.push({
        type: 'QUOTE_ATTRIBUTION',
        priority: 'medium',
        message: `Add author attribution to quote`,
        action: 'add_attribution',
        data: { component, slideId: slide.id }
      });
      break;
  }
  
  // General suggestions based on action
  if (action === 'populate') {
    suggestions.push({
      type: 'CONTENT_EXPANSION',
      priority: 'high',
      message: 'Expand content based on related documents',
      action: 'expand_content',
      data: { slideId: slide.id, component }
    });
  }
  
  return suggestions;
}

/**
 * Analyze component for suggestions
 */
async function analyzeComponent(component, slide, contentData) {
  const analysis = {
    component,
    suggestions: [],
    relatedContent: []
  };
  
  // Find related content
  if (component.type === 'image' && component.alt) {
    // Search for content related to image alt text
    for (const entry of contentData || []) {
      const content = (entry.text || entry.body || '').toLowerCase();
      if (content.includes(component.alt.toLowerCase())) {
        analysis.relatedContent.push(entry);
      }
    }
  }
  
  // Generate analysis-based suggestions
  if (analysis.relatedContent.length > 0) {
    analysis.suggestions.push({
      type: 'RELATED_CONTENT',
      priority: 'medium',
      message: `Found ${analysis.relatedContent.length} related content entries`,
      action: 'show_related',
      data: { component, relatedContent: analysis.relatedContent }
    });
  }
  
  return analysis;
}

