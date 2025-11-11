#!/usr/bin/env tsx
/**
 * Build Content Index
 * 
 * Pre-processes all content sources (wiki, docs, grok_files, evolutions)
 * into a unified JSONL file for agent content population.
 */

import * as fs from 'fs';
import * as path from 'path';
import yaml from 'js-yaml';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

interface DocumentEntry {
  type: 'document';
  id: string;
  source: 'wiki' | 'docs' | 'grok_files' | 'evolutions';
  filePath: string;
  dimension?: string;
  level?: string;
  docType?: string;
  title?: string;
  description?: string;
  tags?: string[];
  keywords?: string[];
  frontmatter?: any;
  relationships?: {
    prerequisites?: string[];
    enables?: string[];
    related?: string[];
  };
  readingTime?: number;
  difficulty?: number;
}

interface RelationshipEntry {
  type: 'relationship';
  from: string;
  to: string;
  relType: 'prerequisite' | 'enables' | 'related';
}

interface RDFTripleEntry {
  type: 'rdf-triple';
  subject: string;
  predicate: string;
  object: string;
}

type ContentIndexEntry = DocumentEntry | RelationshipEntry | RDFTripleEntry;

/**
 * Parse frontmatter from markdown file
 */
function parseFrontmatter(filePath: string): { frontmatter: any | null; body: string } | null {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');
    const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n([\s\S]*)$/);
    
    if (!frontmatterMatch) {
      return { frontmatter: null, body: content };
    }

    const frontmatterYaml = frontmatterMatch[1];
    const body = frontmatterMatch[2];

    if (!frontmatterYaml) {
      return { frontmatter: null, body: content };
    }
    
    const frontmatter = yaml.load(frontmatterYaml) as any;
    return { frontmatter, body: body || '' };
  } catch (error) {
    console.warn(`Failed to parse frontmatter from ${filePath}:`, error);
    return null;
  }
}

/**
 * Find all markdown files in directory
 */
function findMarkdownFiles(dir: string, baseDir: string = dir): string[] {
  const files: string[] = [];
  
  if (!fs.existsSync(dir)) {
    return files;
  }

  const entries = fs.readdirSync(dir, { withFileTypes: true });
  
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    
    if (entry.isDirectory()) {
      // Skip node_modules and other common ignore dirs
      if (entry.name === 'node_modules' || entry.name === '.git' || entry.name.startsWith('.')) {
        continue;
      }
      files.push(...findMarkdownFiles(fullPath, baseDir));
    } else if (entry.isFile() && entry.name.endsWith('.md')) {
      files.push(fullPath);
    }
  }
  
  return files;
}

/**
 * Generate document ID from file path
 */
function generateDocId(filePath: string, source: string): string {
  // Use relative path as base for ID
  const relativePath = filePath.replace(/^.*\/(wiki|docs|grok_files|evolutions)\//, '');
  const baseName = path.basename(relativePath, '.md');
  const dirName = path.dirname(relativePath).replace(/\//g, '-');
  
  if (dirName && dirName !== '.') {
    return `${source}-${dirName}-${baseName}`.toLowerCase().replace(/[^a-z0-9-]/g, '-');
  }
  return `${source}-${baseName}`.toLowerCase().replace(/[^a-z0-9-]/g, '-');
}

/**
 * Extract dimension from frontmatter or tags/keywords
 */
function extractDimension(frontmatter: any): string | undefined {
  // Check direct dimension field
  if (frontmatter.dimension) {
    return frontmatter.dimension;
  }
  
  // Check tags
  if (frontmatter.tags && Array.isArray(frontmatter.tags)) {
    const dimTag = frontmatter.tags.find((t: string) => /^\d+D$/.test(t));
    if (dimTag) return dimTag;
  }
  
  // Check keywords
  if (frontmatter.keywords && Array.isArray(frontmatter.keywords)) {
    const dimKeyword = frontmatter.keywords.find((k: string) => /^\d+D$/.test(k));
    if (dimKeyword) return dimKeyword;
  }
  
  return undefined;
}

/**
 * Process a single markdown file
 */
function processFile(filePath: string, source: string, workspaceRoot: string): ContentIndexEntry[] {
  const entries: ContentIndexEntry[] = [];
  
  const parsed = parseFrontmatter(filePath);
  if (!parsed) {
    return entries;
  }
  
  const { frontmatter, body } = parsed;
  
  // Skip if no frontmatter
  if (!frontmatter) {
    return entries;
  }
  
  // Generate document ID
  const docId = frontmatter.id || generateDocId(filePath, source);
  
  // Get relative file path
  const relativePath = path.relative(workspaceRoot, filePath);
  
  // Extract dimension
  const dimension = extractDimension(frontmatter);
  
  // Create document entry
  const docEntry: DocumentEntry = {
    type: 'document',
    id: docId,
    source: source as any,
    filePath: relativePath,
    dimension,
    level: frontmatter.level,
    docType: frontmatter.type,
    title: frontmatter.title,
    description: frontmatter.description,
    tags: frontmatter.tags,
    keywords: frontmatter.keywords,
    frontmatter,
    relationships: {
      prerequisites: frontmatter.prerequisites || [],
      enables: frontmatter.enables || [],
      related: frontmatter.related || []
    },
    readingTime: frontmatter.readingTime,
    difficulty: frontmatter.difficulty
  };
  
  entries.push(docEntry);
  
  // Create relationship entries
  if (frontmatter.prerequisites && Array.isArray(frontmatter.prerequisites)) {
    for (const prereq of frontmatter.prerequisites) {
      entries.push({
        type: 'relationship',
        from: docId,
        to: prereq,
        relType: 'prerequisite'
      });
      
      // Create RDF triple
      entries.push({
        type: 'rdf-triple',
        subject: `#${docId}`,
        predicate: 'rdfs:prerequisite',
        object: `#${prereq}`
      });
    }
  }
  
  if (frontmatter.enables && Array.isArray(frontmatter.enables)) {
    for (const enable of frontmatter.enables) {
      entries.push({
        type: 'relationship',
        from: docId,
        to: enable,
        relType: 'enables'
      });
      
      // Create RDF triple
      entries.push({
        type: 'rdf-triple',
        subject: `#${docId}`,
        predicate: 'rdfs:enables',
        object: `#${enable}`
      });
    }
  }
  
  if (frontmatter.related && Array.isArray(frontmatter.related)) {
    for (const related of frontmatter.related) {
      entries.push({
        type: 'relationship',
        from: docId,
        to: related,
        relType: 'related'
      });
      
      // Create RDF triple
      entries.push({
        type: 'rdf-triple',
        subject: `#${docId}`,
        predicate: 'rdfs:seeAlso',
        object: `#${related}`
      });
    }
  }
  
  return entries;
}

/**
 * Build content index from all sources
 */
async function buildContentIndex(): Promise<void> {
  const workspaceRoot = path.resolve(__dirname, '../..');
  const outputPath = path.join(__dirname, '..', 'content-index.jsonl');
  
  console.log('📚 Building content index...');
  console.log(`   Workspace root: ${workspaceRoot}`);
  console.log(`   Output: ${outputPath}`);
  
  const allEntries: ContentIndexEntry[] = [];
  
  // Process each source directory
  const sources = [
    { name: 'wiki', path: path.join(workspaceRoot, 'wiki') },
    { name: 'docs', path: path.join(workspaceRoot, 'docs') },
    { name: 'grok_files', path: path.join(workspaceRoot, 'grok_files') },
    { name: 'evolutions', path: path.join(workspaceRoot, 'evolutions') }
  ];
  
  for (const source of sources) {
    console.log(`\n   Processing ${source.name}...`);
    
    if (!fs.existsSync(source.path)) {
      console.warn(`   ⚠️  Directory not found: ${source.path}`);
      continue;
    }
    
    const files = findMarkdownFiles(source.path);
    console.log(`   Found ${files.length} markdown files`);
    
    let processed = 0;
    for (const file of files) {
      const entries = processFile(file, source.name, workspaceRoot);
      allEntries.push(...entries);
      processed++;
      
      if (processed % 50 === 0) {
        console.log(`   Processed ${processed}/${files.length} files...`);
      }
    }
    
    console.log(`   ✅ Processed ${processed} files from ${source.name}`);
  }
  
  // Write to JSONL file
  console.log(`\n   Writing ${allEntries.length} entries to ${outputPath}...`);
  const jsonl = allEntries.map(entry => JSON.stringify(entry)).join('\n');
  fs.writeFileSync(outputPath, jsonl, 'utf-8');
  
  // Print summary
  const docCount = allEntries.filter(e => e.type === 'document').length;
  const relCount = allEntries.filter(e => e.type === 'relationship').length;
  const rdfCount = allEntries.filter(e => e.type === 'rdf-triple').length;
  
  console.log('\n✅ Content index built successfully!');
  console.log(`   Documents: ${docCount}`);
  console.log(`   Relationships: ${relCount}`);
  console.log(`   RDF Triples: ${rdfCount}`);
  console.log(`   Total entries: ${allEntries.length}`);
}

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  buildContentIndex().catch(error => {
    console.error('❌ Error building content index:', error);
    process.exit(1);
  });
}

export { buildContentIndex };

