# Unit Tests for Agent System

This directory contains unit tests for the agent content population system.

## Test Files

- **`ContentLoader.test.js`** - Tests for unified content loader
- **`FrontmatterLoader.test.js`** - Tests for frontmatter content loader  
- **`DimensionalAgent.test.js`** - Tests for dimensional agent base class
- **`AgentCoordinator.test.js`** - Tests for agent coordinator
- **`validation.test.js`** - Tests for content index validation

## Running Tests

```bash
# Run all unit tests
npm run test:unit

# Run tests in watch mode
npm run test:unit:watch

# Run specific test file
npm run test:jest tests/unit/ContentLoader.test.js
```

## Test Coverage

The tests cover:

1. **ContentLoader**:
   - Loading kernel and content index
   - Querying by dimension, tag, keyword
   - Relationship traversal
   - RDF triple extraction

2. **FrontmatterLoader**:
   - Loading and parsing JSONL
   - Dimension/tag/keyword queries
   - Relationship extraction
   - Knowledge graph building

3. **DimensionalAgent**:
   - Content matching logic
   - Text extraction from frontmatter and kernel
   - RDF triple generation
   - UI component extraction

4. **AgentCoordinator**:
   - Agent initialization
   - Slide population routing
   - Multi-agent coordination

5. **Validation**:
   - Document entry validation
   - Bipartite section validation
   - BQF and polynomial validation
   - Relationship and RDF triple validation

## Test Configuration

Tests use Jest with ES module support. Configuration is in `jest.config.js`.

## Note on Mocking

Due to ES module limitations, some tests manually set mock objects on instances rather than using Jest's automatic mocking. This is a known limitation when testing ES modules.

## Integration with E2E Tests

These unit tests complement the Playwright e2e tests in `tests/*.spec.js`. Unit tests focus on individual component logic, while e2e tests verify the full system integration.

