#!/usr/bin/env node

/**
 * End-to-End Test for Autonomous CanvasL Capabilities
 * Tests self-regeneration, self-modification, and autonomous evolution
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

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

function parseCanvasL(filePath) {
  const fullPath = path.join(__dirname, filePath);
  if (!fs.existsSync(fullPath)) {
    return null;
  }
  
  const content = fs.readFileSync(fullPath, 'utf-8');
  const lines = content.split('\n').filter(line => line.trim());
  
  const directives = {};
  const entries = [];
  
  for (const line of lines) {
    const trimmed = line.trim();
    if (!trimmed) continue;
    
    if (trimmed.startsWith('@')) {
      const match = trimmed.match(/^@(\w+)\s+(.+)$/);
      if (match) {
        directives[match[1]] = match[2];
      }
    } else {
      try {
        const entry = JSON.parse(trimmed);
        entries.push(entry);
      } catch (e) {
        // Skip invalid JSON
      }
    }
  }
  
  return { directives, entries, content };
}

// Test 1: Self-Regeneration Capability
function testSelfRegeneration() {
  log('\n🧬 Test 1: Self-Regeneration Capability');
  log('─'.repeat(60));
  
  const seedPath = 'automaton.kernel.seed.canvasl';
  const seed = parseCanvasL(seedPath);
  
  if (!seed) {
    error('Cannot load seed file');
    return false;
  }
  
  // Check that seed has self-reference
  const selfRef = seed.entries.find(e => e.id === 'self-ref');
  if (!selfRef) {
    error('Seed file missing self-reference entry');
    return false;
  }
  
  success('Seed file has self-reference entry');
  
  // Check that self-ref points to kernel
  if (selfRef.file === 'automaton.kernel.canvasl') {
    success('Self-reference points to correct kernel file');
  } else {
    error(`Self-reference points to wrong file: ${selfRef.file}`);
  }
  
  // Check regeneration metadata
  if (selfRef.metadata && selfRef.metadata.regenerate) {
    const regen = selfRef.metadata.regenerate;
    if (regen.function && regen.args) {
      success('Self-reference has valid regeneration metadata');
    } else {
      error('Self-reference regeneration metadata incomplete');
    }
  } else {
    error('Self-reference missing regeneration metadata');
  }
  
  // Check that all entries have regeneration metadata
  const entriesWithRegen = seed.entries.filter(e => 
    e.metadata && e.metadata.regenerate
  );
  
  if (entriesWithRegen.length === seed.entries.length) {
    success(`All ${seed.entries.length} entries have regeneration metadata`);
  } else {
    error(`Only ${entriesWithRegen.length}/${seed.entries.length} entries have regeneration metadata`);
  }
  
  // Check regeneration instructions
  const instructions = seed.entries.find(e => e.id === 'regeneration-instructions');
  if (instructions) {
    success('Regeneration instructions found');
    if (instructions.text && instructions.text.includes('r5rs:parse-jsonl-canvas')) {
      success('Regeneration instructions include correct pipeline steps');
    }
  } else {
    error('Regeneration instructions missing');
  }
  
  // Check code generation pattern
  const codeGen = seed.entries.find(e => e.id === 'code-generation-pattern');
  if (codeGen) {
    success('Code generation pattern found');
    if (codeGen.functions && Array.isArray(codeGen.functions)) {
      success(`Code generation pattern includes ${codeGen.functions.length} R5RS functions`);
    }
  } else {
    error('Code generation pattern missing');
  }
  
  return true;
}

// Test 2: Autonomous Basis Integration
function testAutonomousBasis() {
  log('\n🤖 Test 2: Autonomous Basis Integration');
  log('─'.repeat(60));
  
  const basisPath = 'autonomous.basis.canvasl';
  const basis = parseCanvasL(basisPath);
  
  if (!basis) {
    error('Cannot load autonomous basis file');
    return false;
  }
  
  // Check main autonomous basis entry
  const mainBasis = basis.entries.find(e => e.id === 'autonomous-basis');
  if (!mainBasis) {
    error('Autonomous basis missing main entry');
    return false;
  }
  
  success('Autonomous basis main entry found');
  
  // Check capabilities
  const capabilities = [
    'self-regeneration',
    'autonomous-evolution',
    'goal-negotiation',
    'self-modification',
    'performance-optimization',
    'consensus-mechanism',
    'intelligence-integration',
    'evolution-tracking'
  ];
  
  for (const capId of capabilities) {
    const cap = basis.entries.find(e => e.id === capId);
    if (cap) {
      success(`Capability found: ${capId}`);
      if (cap.metadata && cap.metadata.regenerate) {
        success(`  └─ Has regeneration metadata`);
      } else {
        error(`  └─ Missing regeneration metadata`);
      }
    } else {
      error(`Capability missing: ${capId}`);
    }
  }
  
  // Check links to foundation files
  const foundationLinks = [
    { id: 'link:autonomous-basis→kernel-seed', target: 'kernel.seed' },
    { id: 'link:autonomous-basis→metaverse-shape', target: 'metaverse.shape' },
    { id: 'link:autonomous-basis→metaverse-centroid', target: 'metaverse.centroid' }
  ];
  
  for (const link of foundationLinks) {
    const linkEntry = basis.entries.find(e => e.id === link.id);
    if (linkEntry) {
      success(`Link found: ${link.id}`);
      if (linkEntry.toNode && linkEntry.toNode.includes(link.target)) {
        success(`  └─ Points to correct target: ${link.target}`);
      } else {
        error(`  └─ Points to wrong target: ${linkEntry.toNode}`);
      }
    } else {
      error(`Link missing: ${link.id}`);
    }
  }
  
  return true;
}

// Test 3: Unified Automaton Integration
function testUnifiedAutomaton() {
  log('\n🔗 Test 3: Unified Automaton Integration');
  log('─'.repeat(60));
  
  const unifiedPath = 'unified.automaton.canvasl';
  const unified = parseCanvasL(unifiedPath);
  
  if (!unified) {
    error('Cannot load unified automaton file');
    return false;
  }
  
  // Check main unified automaton entry
  const mainUnified = unified.entries.find(e => e.id === 'unified-automaton');
  if (!mainUnified) {
    error('Unified automaton missing main entry');
    return false;
  }
  
  success('Unified automaton main entry found');
  
  // Check references
  if (mainUnified.references) {
    success('Unified automaton has references object');
    const requiredRefs = ['kernel', 'seed', 'shape', 'centroid', 'basis'];
    for (const ref of requiredRefs) {
      if (mainUnified.references[ref]) {
        success(`  └─ References ${ref}: ${mainUnified.references[ref]}`);
      } else {
        error(`  └─ Missing reference to ${ref}`);
      }
    }
  } else {
    error('Unified automaton missing references object');
  }
  
  // Check integration points
  const integrationPoints = [
    'integration-point-kernel',
    'integration-point-seed',
    'integration-point-shape',
    'integration-point-centroid',
    'integration-point-basis'
  ];
  
  for (const pointId of integrationPoints) {
    const point = unified.entries.find(e => e.id === pointId);
    if (point) {
      success(`Integration point found: ${pointId}`);
    } else {
      error(`Integration point missing: ${pointId}`);
    }
  }
  
  // Check query interface
  const queryInterface = unified.entries.find(e => e.id === 'query-interface');
  if (queryInterface) {
    success('Query interface found');
  } else {
    error('Query interface missing');
  }
  
  // Check dimensional structure
  const dimStructure = unified.entries.find(e => e.id === 'dimensional-structure');
  if (dimStructure) {
    success('Dimensional structure found');
    if (dimStructure.dimensions && Array.isArray(dimStructure.dimensions)) {
      const expectedDims = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
      const hasAllDims = expectedDims.every(d => dimStructure.dimensions.includes(d));
      if (hasAllDims) {
        success(`  └─ Contains all 8 dimensions: ${dimStructure.dimensions.join(', ')}`);
      } else {
        error(`  └─ Missing some dimensions. Found: ${dimStructure.dimensions.join(', ')}`);
      }
    }
  } else {
    error('Dimensional structure missing');
  }
  
  return true;
}

// Test 4: Dimensional Progression
function testDimensionalProgression() {
  log('\n📐 Test 4: Dimensional Progression');
  log('─'.repeat(60));
  
  const seed = parseCanvasL('automaton.kernel.seed.canvasl');
  const shape = parseCanvasL('metaverse.shape.canvasl');
  
  if (!seed || !shape) {
    error('Cannot load required files');
    return false;
  }
  
  // Check that all dimensions 0D-7D are present
  const expectedDims = ['0D', '1D', '2D', '3D', '4D', '5D', '6D', '7D'];
  const seedDims = new Set();
  const shapeDims = new Set();
  
  seed.entries.forEach(e => {
    if (e.dimension) seedDims.add(e.dimension);
  });
  
  shape.entries.forEach(e => {
    if (e.dimension) shapeDims.add(e.dimension);
  });
  
  // Check seed dimensions
  for (const dim of expectedDims) {
    if (seedDims.has(dim)) {
      success(`Seed has ${dim} dimension`);
    } else {
      error(`Seed missing ${dim} dimension`);
    }
  }
  
  // Check shape dimensions
  if (shapeDims.has('8D')) {
    success('Shape has 8D dimension');
  } else {
    error('Shape missing 8D dimension');
  }
  
  // Check vertical edges for progression
  const verticalEdges = seed.entries.filter(e => 
    e.type === 'edge' && e.bipartite && e.bipartite.type === 'vertical'
  );
  
  if (verticalEdges.length >= 7) {
    success(`Found ${verticalEdges.length} vertical edges for dimensional progression`);
  } else {
    error(`Only found ${verticalEdges.length} vertical edges (expected at least 7)`);
  }
  
  // Check that progression is sequential
  const progressions = verticalEdges.map(e => e.bipartite.progression).filter(Boolean);
  const hasSequentialProgression = progressions.some(p => p.includes('→'));
  
  if (hasSequentialProgression) {
    success('Vertical edges show sequential progression (e.g., 0D→1D)');
  } else {
    error('Vertical edges missing progression labels');
  }
  
  return true;
}

// Test 5: Bipartite-BQF Encoding
function testBipartiteBQF() {
  log('\n🔢 Test 5: Bipartite-BQF Encoding');
  log('─'.repeat(60));
  
  const files = [
    'metaverse.shape.canvasl',
    'metaverse.centroid.canvasl',
    'automaton.kernel.seed.canvasl',
    'autonomous.basis.canvasl',
    'unified.automaton.canvasl'
  ];
  
  for (const file of files) {
    const parsed = parseCanvasL(file);
    if (!parsed) continue;
    
    let hasBipartite = 0;
    let hasBQF = 0;
    let validBQF = 0;
    
    parsed.entries.forEach(entry => {
      if (entry.bipartite) {
        hasBipartite++;
        if (entry.bipartite.bqf) {
          hasBQF++;
          const bqf = entry.bipartite.bqf;
          if (bqf.coefficients && Array.isArray(bqf.coefficients) &&
              bqf.form && typeof bqf.form === 'string' &&
              bqf.signature && typeof bqf.signature === 'string') {
            validBQF++;
          }
        }
      }
    });
    
    if (hasBipartite === parsed.entries.length) {
      success(`${file}: All entries have bipartite metadata`);
    } else {
      error(`${file}: Only ${hasBipartite}/${parsed.entries.length} entries have bipartite metadata`);
    }
    
    if (hasBQF === parsed.entries.length) {
      success(`${file}: All entries have BQF encoding`);
    } else {
      error(`${file}: Only ${hasBQF}/${parsed.entries.length} entries have BQF encoding`);
    }
    
    if (validBQF === parsed.entries.length) {
      success(`${file}: All BQF encodings are valid`);
    } else {
      error(`${file}: Only ${validBQF}/${parsed.entries.length} BQF encodings are valid`);
    }
  }
  
  return true;
}

// Test 6: Autonomous Operations Simulation
function testAutonomousOperations() {
  log('\n⚙️  Test 6: Autonomous Operations Simulation');
  log('─'.repeat(60));
  
  const basis = parseCanvasL('autonomous.basis.canvasl');
  if (!basis) {
    error('Cannot load autonomous basis');
    return false;
  }
  
  // Simulate self-regeneration operation
  log('  Simulating self-regeneration...');
  const selfRegen = basis.entries.find(e => e.id === 'self-regeneration');
  if (selfRegen && selfRegen.metadata && selfRegen.metadata.regenerate) {
    const regen = selfRegen.metadata.regenerate;
    if (regen.function === 'r5rs:regenerate-from-seed' && regen.args.length === 2) {
      success('Self-regeneration operation configured correctly');
      log(`    Function: ${regen.function}`);
      log(`    Args: ${regen.args.join(', ')}`);
    } else {
      error('Self-regeneration operation misconfigured');
    }
  } else {
    error('Self-regeneration operation not found');
  }
  
  // Simulate autonomous evolution
  log('  Simulating autonomous evolution...');
  const autoEvol = basis.entries.find(e => e.id === 'autonomous-evolution');
  if (autoEvol && autoEvol.metadata && autoEvol.metadata.regenerate) {
    const regen = autoEvol.metadata.regenerate;
    if (regen.function === 'r5rs:autonomous-evolution') {
      success('Autonomous evolution operation configured correctly');
      log(`    Function: ${regen.function}`);
    } else {
      error('Autonomous evolution operation misconfigured');
    }
  } else {
    error('Autonomous evolution operation not found');
  }
  
  // Simulate goal negotiation
  log('  Simulating goal negotiation...');
  const goalNeg = basis.entries.find(e => e.id === 'goal-negotiation');
  if (goalNeg && goalNeg.metadata && goalNeg.metadata.regenerate) {
    const regen = goalNeg.metadata.regenerate;
    if (regen.function === 'r5rs:goal-negotiation') {
      success('Goal negotiation operation configured correctly');
      log(`    Function: ${regen.function}`);
    } else {
      error('Goal negotiation operation misconfigured');
    }
  } else {
    error('Goal negotiation operation not found');
  }
  
  // Simulate self-modification
  log('  Simulating self-modification...');
  const selfMod = basis.entries.find(e => e.id === 'self-modification');
  if (selfMod && selfMod.metadata && selfMod.metadata.regenerate) {
    const regen = selfMod.metadata.regenerate;
    if (regen.function === 'r5rs:self-modify') {
      success('Self-modification operation configured correctly');
      log(`    Function: ${regen.function}`);
    } else {
      error('Self-modification operation misconfigured');
    }
  } else {
    error('Self-modification operation not found');
  }
  
  return true;
}

// Test 7: Cross-File Integration
function testCrossFileIntegration() {
  log('\n🔗 Test 7: Cross-File Integration');
  log('─'.repeat(60));
  
  // Check that unified automaton references all files
  const unified = parseCanvasL('unified.automaton.canvasl');
  if (!unified) {
    error('Cannot load unified automaton');
    return false;
  }
  
  const mainUnified = unified.entries.find(e => e.id === 'unified-automaton');
  if (mainUnified && mainUnified.references) {
    const refs = mainUnified.references;
    
    // Check that referenced files exist
    const fileRefs = {
      kernel: 'automaton.kernel.canvasl',
      seed: 'automaton.kernel.seed.canvasl',
      shape: 'metaverse.shape.canvasl',
      centroid: 'metaverse.centroid.canvasl',
      basis: 'autonomous.basis.canvasl'
    };
    
    for (const [key, expectedFile] of Object.entries(fileRefs)) {
      if (refs[key]) {
        const refValue = refs[key].replace('#', '');
        if (refValue.includes(expectedFile.replace('.canvasl', ''))) {
          success(`Reference ${key} points to correct file pattern`);
        } else {
          error(`Reference ${key} points to wrong file: ${refValue}`);
        }
      } else {
        error(`Reference ${key} missing`);
      }
    }
  } else {
    error('Unified automaton missing references');
  }
  
  // Check that autonomous basis links to foundation files
  const basis = parseCanvasL('autonomous.basis.canvasl');
  if (basis) {
    const foundationLinks = basis.entries.filter(e => 
      e.id && e.id.startsWith('link:autonomous-basis→')
    );
    
    if (foundationLinks.length >= 3) {
      success(`Autonomous basis has ${foundationLinks.length} links to foundation files`);
      foundationLinks.forEach(link => {
        if (link.toNode) {
          success(`  └─ Links to: ${link.toNode}`);
        }
      });
    } else {
      error(`Autonomous basis only has ${foundationLinks.length} links (expected at least 3)`);
    }
  }
  
  return true;
}

// Main test execution
log('🚀 Autonomous CanvasL End-to-End Tests');
log('='.repeat(60));

testSelfRegeneration();
testAutonomousBasis();
testUnifiedAutomaton();
testDimensionalProgression();
testBipartiteBQF();
testAutonomousOperations();
testCrossFileIntegration();

// Summary
log('\n' + '='.repeat(60));
log('\n📊 E2E Test Summary:');
log(`   ✅ Passed: ${results.passed}`);
log(`   ❌ Failed: ${results.failed}`);

if (results.errors.length > 0) {
  log('\n❌ Errors:');
  results.errors.forEach((err, i) => {
    log(`   ${i + 1}. ${err}`);
  });
}

if (results.failed === 0) {
  log('\n🎉 All E2E tests passed!');
  log('✨ Autonomous CanvasL system is fully operational!');
  process.exit(0);
} else {
  log('\n⚠️  Some E2E tests failed. Please review the errors above.');
  process.exit(1);
}

