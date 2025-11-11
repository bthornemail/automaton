#!/usr/bin/env tsx
/**
 * Update Internal Links in Wiki Markdown Files
 * 
 * Updates all markdown links to reflect new directory structure
 */

import * as fs from 'fs';
import * as path from 'path';

const wikiDir = path.dirname(__filename);

// File mapping: old path -> new path
const fileMappings: { [oldPath: string]: string } = {
  // Agents
  '0D_Topology_Agent.md': 'topology/0D-topology/0D_Topology_Agent.md',
  '1D_Temporal_Agent.md': 'topology/1D-topology/1D_Temporal_Agent.md',
  '2D_Structural_Agent.md': 'topology/2D-topology/2D_Structural_Agent.md',
  '3D_Algebraic_Agent.md': 'topology/3D-topology/3D_Algebraic_Agent.md',
  '4D_Network_Agent.md': 'topology/4D-topology/4D_Network_Agent.md',
  '5D_Consensus_Agent.md': 'topology/5D-topology/5D_Consensus_Agent.md',
  '6D_Intelligence_Agent.md': 'topology/6D-topology/6D_Intelligence_Agent.md',
  '7D_Quantum_Agent.md': 'topology/7D-topology/7D_Quantum_Agent.md',
  
  // Core concepts
  'Church_Encoding.md': 'topology/0D-topology/Church_Encoding.md',
  'R5RS_Integration.md': 'system/0D-system/R5RS_Integration.md',
  'Automaton_System.md': 'system/0D-system/Automaton_System.md',
  'Dimensional_Progression.md': 'vertical/Dimensional_Progression.md',
  'ProLog_Integration.md': 'system/2D-system/ProLog_Integration.md',
  'DataLog_Integration.md': 'system/2D-system/DataLog_Integration.md',
  'RDF_SPARQL_Integration.md': 'system/3D-system/RDF_SPARQL_Integration.md',
  'SHACL_Validation.md': 'system/3D-system/SHACL_Validation.md',
  'Multi_Agent_System.md': 'system/4D-system/Multi_Agent_System.md',
  'Blackboard_Architecture.md': 'system/5D-system/Blackboard_Architecture.md',
  'Meta_Log_Framework.md': 'system/6D-system/Meta_Log_Framework.md',
  
  // Navigation
  'WELCOME.md': 'navigation/WELCOME.md',
  'WELCOME_NEW.md': 'navigation/WELCOME_NEW.md',
  'Table_of_Contents.md': 'navigation/Table_of_Contents.md',
  'NAVIGATION.md': 'navigation/NAVIGATION.md',
  'INDEX.md': 'navigation/INDEX.md',
  'README.md': 'navigation/README.md',
  'Computational_Topology_Canvas.md': 'navigation/Computational_Topology_Canvas.md',
  
  // Research
  'Theoretical_Foundations.md': 'research/Theoretical_Foundations.md',
  'Research_Methodology.md': 'research/Research_Methodology.md',
  'Literature_Review.md': 'research/Literature_Review.md',
  'Research_Contributions.md': 'research/Research_Contributions.md',
  'Future_Research_Directions.md': 'research/Future_Research_Directions.md',
  'arxiv-paper.tex': 'research/arxiv-paper.tex',
  'bibliography.bib': 'research/bibliography.bib',
  
  // Guides
  'Getting_Started.md': 'guides/Getting_Started.md',
  'API_Reference.md': 'guides/API_Reference.md',
  'HUMANIZATION_GUIDE.md': 'guides/HUMANIZATION_GUIDE.md',
  'HUMANIZATION_SUMMARY.md': 'guides/HUMANIZATION_SUMMARY.md',
  
  // Horizontal
  'CanvasL_Format.md': 'horizontal/CanvasL_Format.md',
  'Architecture_Overview.md': 'horizontal/Architecture_Overview.md',
  
  // Meta
  'The_Story_of_CTC.md': 'meta/The_Story_of_CTC.md',
  'TRANSFORMATION_LOG.md': 'meta/TRANSFORMATION_LOG.md',
  'INTEGRATION_STATUS.md': 'meta/INTEGRATION_STATUS.md',
  'INTEGRATION_COMPLETE.md': 'meta/INTEGRATION_COMPLETE.md',
  'COMPLETION_SUMMARY.md': 'meta/COMPLETION_SUMMARY.md',
  'DOCUMENTATION_SUMMARY.md': 'meta/DOCUMENTATION_SUMMARY.md',
};

// Calculate relative path from source to target
function getRelativePath(from: string, to: string): string {
  const fromDir = path.dirname(from);
  const relPath = path.relative(fromDir, to);
  return relPath.replace(/\\/g, '/'); // Normalize to forward slashes
}

// Update links in a file
function updateLinksInFile(filePath: string): void {
  let content = fs.readFileSync(filePath, 'utf-8');
  let updated = false;
  
  // Update each file reference
  Object.keys(fileMappings).forEach(oldFile => {
    const newFile = fileMappings[oldFile];
    const oldFileName = path.basename(oldFile, '.md');
    
    // Pattern 1: [text](filename.md)
    const pattern1 = new RegExp(`\\[([^\\]]+)\\]\\(${oldFile.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}\\)`, 'g');
    if (pattern1.test(content)) {
      const relPath = getRelativePath(filePath, path.join(wikiDir, newFile));
      content = content.replace(pattern1, `[$1](${relPath})`);
      updated = true;
    }
    
    // Pattern 2: [[filename]] (wiki-style links)
    const pattern2 = new RegExp(`\\[\\[${oldFileName}\\]\\]`, 'gi');
    if (pattern2.test(content)) {
      const relPath = getRelativePath(filePath, path.join(wikiDir, newFile));
      content = content.replace(pattern2, `[[${relPath}]]`);
      updated = true;
    }
    
    // Pattern 3: See [[filename]] (with See prefix)
    const pattern3 = new RegExp(`See \\[\\[${oldFileName}\\]\\]`, 'gi');
    if (pattern3.test(content)) {
      const relPath = getRelativePath(filePath, path.join(wikiDir, newFile));
      content = content.replace(pattern3, `See [[${relPath}]]`);
      updated = true;
    }
  });
  
  if (updated) {
    fs.writeFileSync(filePath, content, 'utf-8');
    console.log(`  Updated: ${path.relative(wikiDir, filePath)}`);
  }
}

// Recursively find all markdown files
function findMarkdownFiles(dir: string): string[] {
  const files: string[] = [];
  const entries = fs.readdirSync(dir, { withFileTypes: true });
  
  for (const entry of entries) {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory() && !entry.name.startsWith('.') && entry.name !== 'node_modules') {
      files.push(...findMarkdownFiles(fullPath));
    } else if (entry.isFile() && entry.name.endsWith('.md')) {
      files.push(fullPath);
    }
  }
  
  return files;
}

// Main
console.log('Updating links in markdown files...\n');
const markdownFiles = findMarkdownFiles(wikiDir);
markdownFiles.forEach(file => updateLinksInFile(file));
console.log(`\n✅ Updated ${markdownFiles.length} markdown files`);
