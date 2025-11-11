#!/usr/bin/env tsx
/**
 * Script to add CanvasL-Semantic-Slides-Project compatible frontmatter
 * to all wiki markdown files
 */

import * as fs from 'fs';
import * as path from 'path';

interface FrontmatterConfig {
  id: string;
  title: string;
  level: 'foundational' | 'intermediate' | 'advanced';
  type: 'specification' | 'guide' | 'reference' | 'status-report' | 'research' | 'meta' | 'navigation';
  tags: string[];
  keywords: string[];
  prerequisites: string[];
  enables: string[];
  related: string[];
  readingTime: number;
  difficulty: number;
  blackboard: {
    status: 'active' | 'inactive' | 'pending';
    assignedAgent?: string;
    lastUpdate: string;
    dependencies: string[];
    watchers: string[];
  };
}

function generateId(filePath: string): string {
  // Convert file path to kebab-case ID
  const relativePath = filePath.replace(/^.*\/wiki\//, '').replace(/\.md$/, '');
  return relativePath
    .split('/')
    .map(part => part.replace(/[^a-zA-Z0-9]/g, '-').toLowerCase())
    .join('-');
}

function extractTitle(content: string): string {
  // Extract title from first H1 or use filename
  const h1Match = content.match(/^#\s+(.+)$/m);
  if (h1Match) {
    return h1Match[1].trim();
  }
  return '';
}

function determineLevel(filePath: string, content: string): 'foundational' | 'intermediate' | 'advanced' {
  // Determine level based on path and content
  if (filePath.includes('0D') || filePath.includes('foundation') || filePath.includes('getting-started')) {
    return 'foundational';
  }
  if (filePath.includes('7D') || filePath.includes('quantum') || filePath.includes('advanced')) {
    return 'advanced';
  }
  if (content.toLowerCase().includes('advanced') || content.toLowerCase().includes('expert')) {
    return 'advanced';
  }
  return 'intermediate';
}

function determineType(filePath: string): 'specification' | 'guide' | 'reference' | 'status-report' | 'research' | 'meta' | 'navigation' {
  if (filePath.includes('references/')) return 'reference';
  if (filePath.includes('guides/')) return 'guide';
  if (filePath.includes('research/')) return 'research';
  if (filePath.includes('meta/')) return 'meta';
  if (filePath.includes('navigation/')) return 'navigation';
  if (filePath.includes('status') || filePath.includes('STATUS')) return 'status-report';
  if (filePath.includes('spec') || filePath.includes('SPEC')) return 'specification';
  return 'guide';
}

function generateTags(filePath: string, content: string): string[] {
  const tags: string[] = [];
  
  // Path-based tags
  if (filePath.includes('topology/')) tags.push('topology');
  if (filePath.includes('system/')) tags.push('system');
  if (filePath.includes('references/')) tags.push('references');
  if (filePath.includes('by-design/')) tags.push('mathematical-foundations');
  if (filePath.includes('by-concept/')) tags.push('concepts');
  if (filePath.includes('by-dimension/')) tags.push('dimensional-progression');
  if (filePath.includes('by-paradigm/')) tags.push('paradigms');
  
  // Dimension tags
  for (let d = 0; d <= 7; d++) {
    if (filePath.includes(`${d}D`)) tags.push(`${d}d-topology`);
  }
  
  // Content-based tags
  const lowerContent = content.toLowerCase();
  if (lowerContent.includes('church')) tags.push('church-encoding');
  if (lowerContent.includes('lambda')) tags.push('lambda-calculus');
  if (lowerContent.includes('prolog')) tags.push('prolog');
  if (lowerContent.includes('datalog')) tags.push('datalog');
  if (lowerContent.includes('rdf') || lowerContent.includes('sparql')) tags.push('semantic-web');
  if (lowerContent.includes('shacl')) tags.push('shacl');
  if (lowerContent.includes('agent')) tags.push('multi-agent-system');
  if (lowerContent.includes('blackboard')) tags.push('blackboard-architecture');
  if (lowerContent.includes('automaton')) tags.push('automaton');
  
  return [...new Set(tags)]; // Remove duplicates
}

function generateKeywords(filePath: string, title: string): string[] {
  const keywords: string[] = [];
  
  // Extract keywords from title
  const titleWords = title.toLowerCase().split(/[\s-]+/).filter(w => w.length > 3);
  keywords.push(...titleWords);
  
  // Path-based keywords
  const pathParts = filePath.split('/').filter(p => p && p !== 'wiki' && !p.endsWith('.md'));
  keywords.push(...pathParts.map(p => p.replace(/[^a-zA-Z0-9]/g, '-').toLowerCase()));
  
  return [...new Set(keywords)].slice(0, 10); // Limit to 10
}

function estimateReadingTime(content: string): number {
  // Estimate reading time: ~200 words per minute
  const words = content.split(/\s+/).length;
  return Math.max(5, Math.ceil(words / 200));
}

function estimateDifficulty(filePath: string, content: string): number {
  // Estimate difficulty 1-5
  if (filePath.includes('0D') || filePath.includes('getting-started')) return 1;
  if (filePath.includes('7D') || filePath.includes('quantum')) return 5;
  if (filePath.includes('advanced') || content.toLowerCase().includes('advanced')) return 4;
  if (filePath.includes('foundation') || filePath.includes('overview')) return 2;
  return 3;
}

function determineAgent(filePath: string, content: string): string | undefined {
  // Determine assigned agent based on dimension or content
  const lowerContent = content.toLowerCase();
  
  if (filePath.includes('0D') || lowerContent.includes('0d') || lowerContent.includes('topology agent')) {
    return '0D-Topology-Agent';
  }
  if (filePath.includes('1D') || lowerContent.includes('1d') || lowerContent.includes('temporal')) {
    return '1D-Temporal-Agent';
  }
  if (filePath.includes('2D') || lowerContent.includes('2d') || lowerContent.includes('structural')) {
    return '2D-Structural-Agent';
  }
  if (filePath.includes('3D') || lowerContent.includes('3d') || lowerContent.includes('algebraic')) {
    return '3D-Algebraic-Agent';
  }
  if (filePath.includes('4D') || lowerContent.includes('4d') || lowerContent.includes('network')) {
    return '4D-Network-Agent';
  }
  if (filePath.includes('5D') || lowerContent.includes('5d') || lowerContent.includes('consensus')) {
    return '5D-Consensus-Agent';
  }
  if (filePath.includes('6D') || lowerContent.includes('6d') || lowerContent.includes('intelligence')) {
    return '6D-Intelligence-Agent';
  }
  if (filePath.includes('7D') || lowerContent.includes('7d') || lowerContent.includes('quantum')) {
    return '7D-Quantum-Agent';
  }
  if (lowerContent.includes('multi-agent') || lowerContent.includes('blackboard')) {
    return '5D-Consensus-Agent';
  }
  if (lowerContent.includes('rdf') || lowerContent.includes('sparql')) {
    return '3D-Algebraic-Agent';
  }
  if (lowerContent.includes('prolog') || lowerContent.includes('datalog')) {
    return '2D-Structural-Agent';
  }
  
  return undefined;
}

function generateFrontmatter(filePath: string, content: string): FrontmatterConfig {
  const id = generateId(filePath);
  const title = extractTitle(content) || path.basename(filePath, '.md').replace(/[-_]/g, ' ');
  const level = determineLevel(filePath, content);
  const type = determineType(filePath);
  const tags = generateTags(filePath, content);
  const keywords = generateKeywords(filePath, title);
  const readingTime = estimateReadingTime(content);
  const difficulty = estimateDifficulty(filePath, content);
  const assignedAgent = determineAgent(filePath, content);
  
  return {
    id,
    title,
    level,
    type,
    tags,
    keywords,
    prerequisites: [],
    enables: [],
    related: [],
    readingTime,
    difficulty,
    blackboard: {
      status: 'active',
      assignedAgent,
      lastUpdate: '2025-01-07',
      dependencies: [],
      watchers: []
    }
  };
}

function formatFrontmatter(config: FrontmatterConfig): string {
  const lines = ['---'];
  lines.push(`id: ${config.id}`);
  lines.push(`title: "${config.title}"`);
  lines.push(`level: ${config.level}`);
  lines.push(`type: ${config.type}`);
  lines.push(`tags: [${config.tags.map(t => t).join(', ')}]`);
  lines.push(`keywords: [${config.keywords.map(k => k).join(', ')}]`);
  lines.push(`prerequisites: [${config.prerequisites.join(', ')}]`);
  lines.push(`enables: [${config.enables.join(', ')}]`);
  lines.push(`related: [${config.related.join(', ')}]`);
  lines.push(`readingTime: ${config.readingTime}`);
  lines.push(`difficulty: ${config.difficulty}`);
  lines.push(`blackboard:`);
  lines.push(`  status: ${config.blackboard.status}`);
  if (config.blackboard.assignedAgent) {
    lines.push(`  assignedAgent: "${config.blackboard.assignedAgent}"`);
  }
  lines.push(`  lastUpdate: "${config.blackboard.lastUpdate}"`);
  lines.push(`  dependencies: [${config.blackboard.dependencies.join(', ')}]`);
  lines.push(`  watchers: [${config.blackboard.watchers.map(w => `"${w}"`).join(', ')}]`);
  lines.push('---');
  lines.push('');
  
  return lines.join('\n');
}

function hasFrontmatter(content: string): boolean {
  // Check if file already has proper frontmatter
  const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n/);
  if (!frontmatterMatch) return false;
  
  const frontmatter = frontmatterMatch[1];
  // Must have all required fields: id, title, level, type
  return frontmatter.includes('id:') && 
         frontmatter.includes('title:') && 
         frontmatter.includes('level:') &&
         frontmatter.includes('type:');
}

function processFile(filePath: string): void {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');
    
    // Skip if already has proper frontmatter
    if (hasFrontmatter(content)) {
      console.log(`✓ Skipping ${filePath} - already has frontmatter`);
      return;
    }
    
    // Generate frontmatter
    const frontmatterConfig = generateFrontmatter(filePath, content);
    const frontmatter = formatFrontmatter(frontmatterConfig);
    
    // Remove existing empty or incomplete frontmatter if present
    let newContent = content;
    
    // Handle empty frontmatter (---\n---\n)
    if (content.startsWith('---\n---\n')) {
      newContent = content.replace(/^---\n---\n/, '');
    } 
    // Handle incomplete frontmatter (---\n without proper fields)
    else if (content.startsWith('---\n')) {
      const lines = content.split('\n');
      const endIndex = lines.findIndex((line, i) => i > 0 && line.trim() === '---');
      
      if (endIndex > 0) {
        const frontmatterSection = lines.slice(1, endIndex).join('\n');
        // If frontmatter doesn't have required fields, remove it
        if (!frontmatterSection.includes('id:') || 
            !frontmatterSection.includes('title:') || 
            !frontmatterSection.includes('level:') ||
            !frontmatterSection.includes('type:')) {
          newContent = lines.slice(endIndex + 1).join('\n');
        }
      }
    }
    
    // Add frontmatter
    const updatedContent = frontmatter + newContent;
    
    // Write back
    fs.writeFileSync(filePath, updatedContent, 'utf-8');
    console.log(`✓ Added frontmatter to ${filePath}`);
  } catch (error) {
    console.error(`✗ Error processing ${filePath}:`, error);
  }
}

function main() {
  const wikiDir = path.join(__dirname);
  const files: string[] = [];
  
  function walkDir(dir: string) {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        walkDir(fullPath);
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        files.push(fullPath);
      }
    }
  }
  
  walkDir(wikiDir);
  
  console.log(`Found ${files.length} markdown files`);
  console.log('Processing files...\n');
  
  for (const file of files) {
    processFile(file);
  }
  
  console.log(`\n✓ Processed ${files.length} files`);
}

if (require.main === module) {
  main();
}

export { generateFrontmatter, formatFrontmatter, processFile };

