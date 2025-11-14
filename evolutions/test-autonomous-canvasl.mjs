#!/usr/bin/env node

/**
 * Test script for Autonomous CanvasL files
 * Validates all foundation files created for the Autonomous CanvasL system
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Test results
const results = {
  passed: 0,
  failed: 0,
  errors: []
};

function log(message) {
  console.log(message);
}

function error(message) {
  console.error(`❌ ${message}`);
  results.failed++;
  results.errors.push(message);
}

function success(message) {
  console.log(`✅ ${message}`);
  results.passed++;
}

function testFileExists(filePath) {
  const fullPath = path.join(__dirname, filePath);
  if (fs.existsSync(fullPath)) {
    success(`File exists: ${filePath}`);
    return true;
  } else {
    error(`File missing: ${filePath}`);
    return false;
  }
}

function parseCanvasL(filePath) {
  const fullPath = path.join(__dirname, filePath);
  const content = fs.readFileSync(fullPath, 'utf-8');
  const lines = content.split('\n').filter(line => line.trim());
  
  const directives = {};
  const entries = [];
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    
    // Parse directives
    if (trimmed.startsWith('@')) {
      const match = trimmed.match(/^@(\w+)\s+(.+)$/);
      if (match) {
        directives[match[1]] = match[2];
      }
    } else {
      // Parse JSONL entry
      try {
        const entry = JSON.parse(trimmed);
        entries.push(entry);
      } catch (e) {
        error(`Invalid JSON in ${filePath} at line: ${trimmed.substring(0, 50)}...`);
        return null;
      }
    }
  }
  
  return { directives, entries, content };
}

function testCanvasLDirectives(parsed, filePath) {
  if (!parsed.directives.version) {
    error(`${filePath}: Missing @version directive`);
    return false;
  } else {
    success(`${filePath}: Has @version directive`);
  }
  
  if (!parsed.directives.schema) {
    error(`${filePath}: Missing @schema directive`);
    return false;
  } else {
    success(`${filePath}: Has @schema directive`);
  }
  
  return true;
}

function testBipartiteBQF(entries, filePath) {
  let hasBipartite = 0;
  let hasBQF = 0;
  
  for (const entry of entries) {
    if (entry.bipartite) {
      hasBipartite++;
      if (entry.bipartite.bqf) {
        hasBQF++;
        // Validate BQF structure
        if (!entry.bipartite.bqf.coefficients || !Array.isArray(entry.bipartite.bqf.coefficients)) {
          error(`${filePath}: Invalid BQF coefficients in entry ${entry.id}`);
        }
        if (!entry.bipartite.bqf.form || typeof entry.bipartite.bqf.form !== 'string') {
          error(`${filePath}: Invalid BQF form in entry ${entry.id}`);
        }
        if (!entry.bipartite.bqf.signature || typeof entry.bipartite.bqf.signature !== 'string') {
          error(`${filePath}: Invalid BQF signature in entry ${entry.id}`);
        }
      }
    }
  }
  
  if (hasBipartite > 0) {
    success(`${filePath}: ${hasBipartite} entries have bipartite metadata`);
  } else {
    error(`${filePath}: No entries have bipartite metadata`);
  }
  
  if (hasBQF > 0) {
    success(`${filePath}: ${hasBQF} entries have BQF encoding`);
  }
  
  return hasBipartite > 0;
}

function testRegenerationMetadata(entries, filePath) {
  let hasRegenerate = 0;
  
  for (const entry of entries) {
    if (entry.metadata && entry.metadata.regenerate) {
      hasRegenerate++;
      const regen = entry.metadata.regenerate;
      if (!regen.function || typeof regen.function !== 'string') {
        error(`${filePath}: Invalid regenerate function in entry ${entry.id}`);
      }
      if (!regen.args || !Array.isArray(regen.args)) {
        error(`${filePath}: Invalid regenerate args in entry ${entry.id}`);
      }
    }
  }
  
  if (hasRegenerate > 0) {
    success(`${filePath}: ${hasRegenerate} entries have regeneration metadata`);
  } else {
    error(`${filePath}: No entries have regeneration metadata`);
  }
  
  return hasRegenerate > 0;
}

function testDimensionalProgression(entries, filePath) {
  const dimensions = new Set();
  const verticalEdges = [];
  
  for (const entry of entries) {
    if (entry.dimension) {
      dimensions.add(entry.dimension);
    }
    if (entry.type === 'edge' && entry.bipartite && entry.bipartite.type === 'vertical') {
      verticalEdges.push(entry);
    }
  }
  
  if (dimensions.size > 0) {
    success(`${filePath}: Found ${dimensions.size} unique dimensions: ${Array.from(dimensions).sort().join(', ')}`);
  }
  
  if (verticalEdges.length > 0) {
    success(`${filePath}: Found ${verticalEdges.length} vertical edges for dimensional progression`);
  }
  
  return dimensions.size > 0;
}

function testSelfReference(entries, filePath) {
  let hasSelfRef = 0;
  
  for (const entry of entries) {
    if (entry.id === 'self-ref' || entry.type === 'file') {
      hasSelfRef++;
      if (!entry.file) {
        error(`${filePath}: Self-reference entry missing file property`);
      }
    }
    if (entry.selfReference) {
      hasSelfRef++;
      if (!entry.selfReference.file) {
        error(`${filePath}: Entry ${entry.id} has selfReference but missing file`);
      }
    }
  }
  
  if (hasSelfRef > 0) {
    success(`${filePath}: Found ${hasSelfRef} self-reference patterns`);
  }
  
  return hasSelfRef > 0;
}

function testFileStructure(filePath) {
  log(`\n📋 Testing ${filePath}...`);
  
  if (!testFileExists(filePath)) {
    return false;
  }
  
  const parsed = parseCanvasL(filePath);
  if (!parsed) {
    return false;
  }
  
  log(`   Found ${parsed.entries.length} entries`);
  
  // Test directives
  if (!testCanvasLDirectives(parsed, filePath)) {
    return false;
  }
  
  // Test Bipartite-BQF encoding
  testBipartiteBQF(parsed.entries, filePath);
  
  // Test regeneration metadata
  testRegenerationMetadata(parsed.entries, filePath);
  
  // Test dimensional progression
  testDimensionalProgression(parsed.entries, filePath);
  
  // Test self-reference patterns
  testSelfReference(parsed.entries, filePath);
  
  return true;
}

function testSpecificRequirements() {
  log('\n🔍 Testing specific requirements...');
  
  // Test seed file line count
  const seedPath = path.join(__dirname, 'automaton.kernel.seed.canvasl');
  if (fs.existsSync(seedPath)) {
    const content = fs.readFileSync(seedPath, 'utf-8');
    const lines = content.split('\n').filter(line => line.trim());
    if (lines.length <= 100) {
      success(`Seed file has ${lines.length} lines (requirement: <100)`);
    } else {
      error(`Seed file has ${lines.length} lines (requirement: <100)`);
    }
  }
  
  // Test that all foundation files reference each other
  const basisPath = path.join(__dirname, 'autonomous.basis.canvasl');
  if (fs.existsSync(basisPath)) {
    const parsed = parseCanvasL('autonomous.basis.canvasl');
    if (parsed) {
      const hasKernelLink = parsed.entries.some(e => 
        e.toNode && e.toNode.includes('kernel.seed')
      );
      const hasShapeLink = parsed.entries.some(e => 
        e.toNode && e.toNode.includes('metaverse.shape')
      );
      const hasCentroidLink = parsed.entries.some(e => 
        e.toNode && e.toNode.includes('metaverse.centroid')
      );
      
      if (hasKernelLink) success('Autonomous basis links to kernel seed');
      else error('Autonomous basis missing link to kernel seed');
      
      if (hasShapeLink) success('Autonomous basis links to metaverse shape');
      else error('Autonomous basis missing link to metaverse shape');
      
      if (hasCentroidLink) success('Autonomous basis links to metaverse centroid');
      else error('Autonomous basis missing link to metaverse centroid');
    }
  }
  
  // Test unified automaton references
  const unifiedPath = path.join(__dirname, 'unified.automaton.canvasl');
  if (fs.existsSync(unifiedPath)) {
    const parsed = parseCanvasL('unified.automaton.canvasl');
    if (parsed) {
      const unifiedEntry = parsed.entries.find(e => e.id === 'unified-automaton');
      if (unifiedEntry && unifiedEntry.references) {
        const refs = unifiedEntry.references;
        const requiredRefs = ['kernel', 'seed', 'shape', 'centroid', 'basis'];
        for (const ref of requiredRefs) {
          if (refs[ref]) {
            success(`Unified automaton references ${ref}`);
          } else {
            error(`Unified automaton missing reference to ${ref}`);
          }
        }
      } else {
        error('Unified automaton missing references object');
      }
    }
  }
}

// Main test execution
log('🧪 Testing Autonomous CanvasL Files\n');
log('='.repeat(60));

// Test all foundation files
const files = [
  'metaverse.shape.canvasl',
  'metaverse.centroid.canvasl',
  'automaton.kernel.seed.canvasl',
  'autonomous.basis.canvasl',
  'unified.automaton.canvasl'
];

for (const file of files) {
  testFileStructure(file);
}

// Test specific requirements
testSpecificRequirements();

// Summary
log('\n' + '='.repeat(60));
log('\n📊 Test Summary:');
log(`   ✅ Passed: ${results.passed}`);
log(`   ❌ Failed: ${results.failed}`);

if (results.errors.length > 0) {
  log('\n❌ Errors:');
  results.errors.forEach((err, i) => {
    log(`   ${i + 1}. ${err}`);
  });
}

if (results.failed === 0) {
  log('\n🎉 All tests passed!');
  process.exit(0);
} else {
  log('\n⚠️  Some tests failed. Please review the errors above.');
  process.exit(1);
}

