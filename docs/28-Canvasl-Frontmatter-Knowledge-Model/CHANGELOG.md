# Changelog

All notable changes to the Bipartite-BQF CanvasL Extension specification will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2025-01-07

### Added
- Initial Bipartite-BQF extension specification
- Meta-specification for coordination
- Protocol specification
- Frontmatter integration specification
- Complete examples and reference materials
- Versioning infrastructure
- Package metadata (PACKAGE.json)
- Comprehensive README with navigation
- CHANGELOG for version tracking

### Specification Files
- `00-META-SPECIFICATION-RFC2119.md` - Meta-specification coordinating all related specs
- `01-BIPARTITE-BQF-EXTENSION-RFC2119.md` - Main Bipartite-BQF extension specification
- `02-PROTOCOL-SPECIFICATION-RFC2119.md` - Protocol specification for Bipartite-BQF operations
- `03-FRONTMATTER-INTEGRATION-RFC2119.md` - Frontmatter knowledge model integration

### Examples
- `examples/complete-bipartite-bqf.canvasl` - Complete working example
- `examples/frontmatter-example.md` - Frontmatter example with bipartite metadata
- `examples/dimensional-progression.json` - Reference data for 0D-7D progression

### Reference Materials
- `reference/grammar-extension.md` - Grammar extension reference
- `reference/validation-rules.md` - Validation rules reference
- `reference/r5rs-functions.md` - R5RS function extensions

### Infrastructure
- Semantic versioning support
- Git tagging strategy documentation
- Immutability policy
- Version directory structure

### Implemented (2025-01-07)

#### Phase 7.2: Grammar Extension
- Extended CanvasL grammar (`ui/src/grammars/canvasl.grammar`) with Bipartite-BQF support
- Added grammar rules for `BipartiteMetadata`, `BipartiteObject`, `BQFObject`, `BQFTransformation`, and `PolynomialObject`
- Implemented flexible parsing with semantic validation deferred to parser layer

#### Phase 7.3: Parser Implementation
- Extended CanvasL parser (`ui/src/extensions/canvasl-language.ts`) with bipartite metadata parsing
- Implemented comprehensive validation functions: `validateBQF()`, `validateBQFTransformation()`, `validatePolynomial()`, `validateBipartite()`
- Added AST validation with `validateAST()` and `getNodeValidationErrors()`
- Enhanced LSP service (`ui/src/services/canvasl-lsp-service.ts`) with bipartite metadata support

#### Phase 7.4: R5RS Integration
- Implemented BQF evaluation functions: `r5rs:bqf-eval()`, `r5rs:bqf-transform()`, `r5rs:poly-to-bqf()`, `r5rs:bqf-to-procedure()`
- Implemented polynomial operation functions: `r5rs:poly-add()`, `r5rs:poly-mult()`, `r5rs:poly-compose()`, `r5rs:poly-eval()`
- Added functions to both Node.js (`meta-log-db/src/r5rs/registry.ts`) and browser (`meta-log-db/src/browser/r5rs/browser-registry.ts`) registries

#### Phase 7.5: Frontmatter Integration
- Extended frontmatter interface (`evolutions/obsidian-frontmatter-knowledge-model/obsidian-frontmatter-knowledge-model.ts`) with bipartite metadata structure
- Implemented `BipartiteBQFSynchronizer` class for CanvasL ↔ Frontmatter bidirectional synchronization
- Added bipartite graph building: `buildBipartiteGraph()`, `getBipartiteStatistics()`, `validateBQFForms()`
- Enhanced knowledge model with bipartite graph validation

#### Phase 7.6: Validation
- Created comprehensive `BipartiteBQFValidator` class (`meta-log-db/src/validation/bipartite-bqf-validator.ts`)
- Implemented BQF progression validation against dimensional patterns (0D-7D)
- Implemented bipartite structure validation (horizontal/vertical edges, consistency)
- Implemented polynomial → BQF mapping validation
- Implemented frontmatter ↔ CanvasL synchronization validation
- Enhanced existing validators with progression and form pattern matching

### Implementation Files

#### Grammar and Parser
- `ui/src/grammars/canvasl.grammar` - Extended grammar with Bipartite-BQF rules
- `ui/src/extensions/canvasl-language.ts` - Parser with bipartite metadata support
- `ui/src/services/canvasl-lsp-service.ts` - LSP service with validation

#### R5RS Functions
- `meta-log-db/src/r5rs/registry.ts` - Node.js R5RS registry with BQF functions
- `meta-log-db/src/browser/r5rs/browser-registry.ts` - Browser R5RS registry with BQF functions

#### Frontmatter Integration
- `evolutions/obsidian-frontmatter-knowledge-model/obsidian-frontmatter-knowledge-model.ts` - Extended knowledge model
- `evolutions/obsidian-frontmatter-knowledge-model/bipartite-bqf-synchronizer.ts` - Synchronization handler

#### Validation
- `meta-log-db/src/validation/bipartite-bqf-validator.ts` - Comprehensive validator
- `meta-log-db/src/validation/frontmatter-validator.ts` - Enhanced with progression validation

### Unified CanvasL Metaverse Browser Module (2025-01-07)

#### Phase 1: Unified Module Creation
- Created `CanvasLMetaverseBrowser` class (`meta-log-db/src/browser/canvasl-browser.ts`) as unified browser API
- Consolidated disparate implementations from `template-projector` and `ui` packages
- Implemented comprehensive CanvasL object execution (`executeCanvasLObject`, `executeCanvasLObjects`)
- Added support for all CanvasL object types: `rdf-triple`, `r5rs-call`, `prolog-query`, `datalog-query`, `sparql-construct`, `shacl-validate`, `slide`
- Standardized API with consistent parameter order (`loadCanvas(path, url)`)
- Exported unified module from `meta-log-db/browser` for use in plugins

#### Phase 2: Consumer Migration
- Updated `template-projector` to use `CanvasLMetaverseBrowser` directly
- Refactored `ui` package `MetaLogBrowserAdapter` to wrap `CanvasLMetaverseBrowser` for backward compatibility
- Added optional browser support to `meta-log-plugin` with dynamic imports
- Maintained backward compatibility through adapter getters and wrapper classes
- Updated Playwright tests to use unified browser module

#### Phase 3: Testing and Documentation
- Created comprehensive test suite (`meta-log-db/src/browser/__tests__/canvasl-browser.test.ts`)
- Implemented Jest projects configuration for separate Node.js and browser test environments
- Created browser test setup file with IndexedDB, fetch, and window mocks
- Created complete API reference documentation (`meta-log-db/docs/CANVASL_METAVERSE_BROWSER_API.md`)
- Created migration guide (`meta-log-db/docs/MIGRATION_GUIDE.md`)
- Created testing guide (`meta-log-db/TESTING.md`)
- Updated consumer documentation (`template-projector/README.md`, `ui/README.md`)

#### Phase 3 Next Steps: Examples and Verification
- Created comprehensive real-world examples (`meta-log-db/docs/EXAMPLES.md`)
  - Basic usage patterns
  - CanvasL presentation workflows
  - Batch execution patterns
  - React integration examples
  - Template-projector integration
  - Error handling patterns
  - Performance optimization strategies
  - Advanced patterns (custom handlers, event-driven execution)
- Verified all documentation examples for accuracy
- Verified parameter orders match implementation
- Updated all consumer READMEs with integration examples
- Added links to examples and migration guides throughout documentation

### Browser Module Files

#### Unified Browser Module
- `meta-log-db/src/browser/canvasl-browser.ts` - Core CanvasLMetaverseBrowser implementation
- `meta-log-db/src/browser/index.ts` - Browser module exports

#### Testing
- `meta-log-db/src/browser/__tests__/canvasl-browser.test.ts` - Comprehensive test suite
- `meta-log-db/src/browser/__tests__/setup.js` - Browser test environment setup
- `meta-log-db/jest.config.js` - Updated Jest configuration with projects

#### Documentation
- `meta-log-db/docs/CANVASL_METAVERSE_BROWSER_API.md` - Complete API reference
- `meta-log-db/docs/MIGRATION_GUIDE.md` - Migration guide from MetaLogBrowserAdapter
- `meta-log-db/docs/EXAMPLES.md` - Real-world usage examples
- `meta-log-db/TESTING.md` - Testing guide and execution instructions

#### Consumer Updates
- `template-projector/src/projector/MetaLogBridge.js` - Updated to use CanvasLMetaverseBrowser
- `template-projector/src/projector/CanvasLExecutor.js` - Updated to use unified execution methods
- `ui/src/services/meta-log-browser-adapter.ts` - Refactored to wrap CanvasLMetaverseBrowser
- `plugin/meta-log-plugin/src/core/plugin.ts` - Added optional browser support

