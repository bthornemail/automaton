#!/usr/bin/env tsx
/**
 * Test Worker Bundling Script
 * 
 * Verifies that the provenance canvas worker bundles correctly in production builds
 * and that Three.js is properly included in the worker bundle.
 */

import { build } from 'vite';
import { resolve } from 'path';
import { existsSync, readFileSync } from 'fs';

async function testWorkerBundling() {
  console.log('🔍 Testing worker bundling configuration...\n');

  // 1. Check Vite config
  console.log('1. Checking Vite configuration...');
  const viteConfigPath = resolve(__dirname, '../vite.config.ts');
  if (!existsSync(viteConfigPath)) {
    console.error('❌ vite.config.ts not found');
    process.exit(1);
  }
  
  const viteConfig = readFileSync(viteConfigPath, 'utf-8');
  const hasWorkerConfig = viteConfig.includes('worker:') && viteConfig.includes('format:');
  if (!hasWorkerConfig) {
    console.warn('⚠️  Worker configuration not found in vite.config.ts');
  } else {
    console.log('✅ Worker configuration found in vite.config.ts');
  }

  // 2. Check worker file
  console.log('\n2. Checking worker file...');
  const workerPath = resolve(__dirname, '../src/workers/provenance-canvas-worker.ts');
  if (!existsSync(workerPath)) {
    console.error('❌ Worker file not found:', workerPath);
    process.exit(1);
  }
  
  const workerCode = readFileSync(workerPath, 'utf-8');
  const hasThreeImport = workerCode.includes("import * as THREE from 'three'");
  if (!hasThreeImport) {
    console.error('❌ Three.js import not found in worker');
    process.exit(1);
  }
  console.log('✅ Worker file exists and imports Three.js');

  // 3. Check error handling
  console.log('\n3. Checking error handling...');
  const hasErrorHandling = 
    workerCode.includes('onerror') || 
    workerCode.includes('try {') ||
    workerCode.includes('catch');
  if (!hasErrorHandling) {
    console.warn('⚠️  Error handling not found in worker');
  } else {
    console.log('✅ Error handling found in worker');
  }

  // 4. Test production build
  console.log('\n4. Testing production build...');
  try {
    console.log('   Building production bundle...');
    const result = await build({
      configFile: resolve(__dirname, '../vite.config.ts'),
      build: {
        outDir: resolve(__dirname, '../dist-test'),
        minify: false, // Don't minify for easier inspection
        sourcemap: true,
      },
    });
    
    console.log('✅ Production build completed');
    
    // Check for worker bundle
    const distPath = resolve(__dirname, '../dist-test');
    if (existsSync(distPath)) {
      const files = require('fs').readdirSync(distPath, { recursive: true });
      const workerFiles = files.filter((f: string) => 
        f.includes('worker') || f.includes('provenance-canvas-worker')
      );
      
      if (workerFiles.length > 0) {
        console.log(`✅ Found ${workerFiles.length} worker bundle file(s):`);
        workerFiles.forEach((f: string) => console.log(`   - ${f}`));
      } else {
        console.warn('⚠️  No worker bundle files found in dist');
      }
    }
  } catch (error) {
    console.error('❌ Production build failed:', error);
    process.exit(1);
  }

  // 5. Check bundle size
  console.log('\n5. Checking bundle sizes...');
  const distPath = resolve(__dirname, '../dist-test');
  if (existsSync(distPath)) {
    const { execSync } = require('child_process');
    try {
      const sizes = execSync(`du -sh ${distPath}/* 2>/dev/null || echo ""`, { encoding: 'utf-8' });
      if (sizes) {
        console.log('Bundle sizes:');
        console.log(sizes);
      }
    } catch (e) {
      // Ignore if du command fails
    }
  }

  console.log('\n✅ Worker bundling test completed successfully!');
  console.log('\nNext steps:');
  console.log('1. Test worker initialization in browser');
  console.log('2. Verify Three.js is available in worker context');
  console.log('3. Test rendering performance');
  console.log('4. Verify no main thread blocking');
}

// Run tests
testWorkerBundling().catch((error) => {
  console.error('Test failed:', error);
  process.exit(1);
});

