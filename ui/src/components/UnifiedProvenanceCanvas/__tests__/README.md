# UnifiedProvenanceCanvas Integration Tests

## Overview

This directory contains integration tests for the `UnifiedProvenanceCanvas` component, which combines MetaverseCanvas3D and DimensionalCanvas with offscreen canvas integration for visualizing provenance chains.

## Test Coverage

### Component Initialization (4 tests)
- ✅ Mount component with provenance chain
- ✅ Mount component without provenance chain
- ✅ Handle error during initialization
- ⚠️ Load slides from evolution path when provided

### Slide Navigation (5 tests)
- ⚠️ Generate slides from evolution directory
- ✅ Navigate to next slide
- ✅ Navigate to previous slide
- ✅ Disable navigation buttons at boundaries
- ✅ Filter by dimension

### Card Display (3 tests)
- ✅ Render cards for each pattern
- ✅ Display card interaction on click
- ✅ Display card detail views

### Worker Communication (5 tests)
- ⚠️ Initialize worker on mount
- ⚠️ Load provenance chain into worker
- ⚠️ Handle click interaction events
- ⚠️ Handle hover interaction events
- ⚠️ Synchronize camera updates

## Test Status

**Current**: 10/17 tests passing (58.8%)

### Passing Tests
- Component mounting and initialization
- Slide navigation (next/previous)
- Navigation button states
- Dimension filtering
- Card rendering and interaction

### Failing Tests
- Evolution path loading (async timing issues)
- Worker initialization (offscreen canvas mocking)
- Worker interaction events (event handling)
- Camera synchronization (state updates)

## Running Tests

```bash
# Run all integration tests
npm test -- src/components/UnifiedProvenanceCanvas/__tests__/UnifiedProvenanceCanvas.integration.test.tsx

# Run with watch mode
npm test -- src/components/UnifiedProvenanceCanvas/__tests__/UnifiedProvenanceCanvas.integration.test.tsx --watch
```

## Mocking Strategy

The tests use comprehensive mocking for:
- **ProvenanceSlideService**: Mocked to avoid actual database/file system access
- **ProvenanceCanvasWorkerService**: Mocked to avoid actual worker thread creation
- **React Three Fiber**: Mocked to avoid WebGL context requirements
- **D3.js**: Mocked to avoid SVG manipulation in test environment
- **Framer Motion**: Mocked to simplify animation testing
- **BiwaScheme & Meta-Log DB**: Mocked to avoid external dependencies

## Known Issues

1. **Offscreen Canvas**: Browser APIs for `OffscreenCanvas` and `transferControlToOffscreen` are mocked but may not fully simulate real behavior
2. **Async Timing**: Some tests require longer timeouts due to async service initialization
3. **Worker Communication**: Worker message passing is mocked and may not reflect actual behavior

## Next Steps

1. Fix remaining async timing issues in evolution path loading tests
2. Improve worker mocking to better simulate actual behavior
3. Add E2E tests with Playwright for full browser environment testing
4. Add performance tests for large provenance chains

