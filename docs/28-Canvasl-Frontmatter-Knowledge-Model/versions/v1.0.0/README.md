# Bipartite-BQF CanvasL Extension - Immutable Reference Package

**Version**: 1.0.0  
**Status**: Implementation Complete  
**Last Updated**: 2025-01-07

## Package Overview

This package provides a fully definitive, immutable reference specification for the **Bipartite Binary Quadratic Polynomial Form (Bipartite-BQF)** extension to CanvasL. The extension enables optimal mathematical encoding of the dimensional progression (0D-7D) through bipartite graph structures with quadratic polynomial forms, integrating seamlessly with the Obsidian Frontmatter Knowledge Model.

## Quick Start

### Reading the Specifications

1. **Start Here**: [`00-META-SPECIFICATION-RFC2119.md`](./00-META-SPECIFICATION-RFC2119.md) - Meta-specification coordinating all related specs
2. **Main Extension**: [`01-BIPARTITE-BQF-EXTENSION-RFC2119.md`](./01-BIPARTITE-BQF-EXTENSION-RFC2119.md) - Complete Bipartite-BQF extension specification
3. **Protocol**: [`02-PROTOCOL-SPECIFICATION-RFC2119.md`](./02-PROTOCOL-SPECIFICATION-RFC2119.md) - Protocol specification for operations
4. **Integration**: [`03-FRONTMATTER-INTEGRATION-RFC2119.md`](./03-FRONTMATTER-INTEGRATION-RFC2119.md) - Frontmatter knowledge model integration

### Examples

- [`examples/complete-bipartite-bqf.canvasl`](./examples/complete-bipartite-bqf.canvasl) - Complete working example
- [`examples/frontmatter-example.md`](./examples/frontmatter-example.md) - Frontmatter example with bipartite metadata
- [`examples/dimensional-progression.json`](./examples/dimensional-progression.json) - Reference data for 0D-7D progression

### Reference Materials

- [`reference/grammar-extension.md`](./reference/grammar-extension.md) - Grammar extension reference
- [`reference/validation-rules.md`](./reference/validation-rules.md) - Validation rules reference
- [`reference/r5rs-functions.md`](./reference/r5rs-functions.md) - R5RS function extensions

## Specification Navigation

### Core Specifications

| File | Purpose | Status |
|------|---------|--------|
| `00-META-SPECIFICATION-RFC2119.md` | Coordinates all related specs, versioning, immutability | Draft |
| `01-BIPARTITE-BQF-EXTENSION-RFC2119.md` | Main Bipartite-BQF extension specification | Draft |
| `02-PROTOCOL-SPECIFICATION-RFC2119.md` | Protocol for Bipartite-BQF operations | Draft |
| `03-FRONTMATTER-INTEGRATION-RFC2119.md` | Frontmatter knowledge model integration | Draft |

### Dependencies

This package depends on:

- **CanvasL Base Specification**: `docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`
- **Multiverse Canvas Specification**: `docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md`
- **Frontmatter Knowledge Model**: `evolutions/obsidian-frontmatter-knowledge-model/`

## Versioning Policy

This package uses **Semantic Versioning** (SemVer) following the pattern `MAJOR.MINOR.PATCH`:

- **MAJOR** (X.0.0): Breaking changes to spec structure
- **MINOR** (x.Y.0): New features, backward compatible additions
- **PATCH** (x.y.Z): Bug fixes, clarifications, non-breaking changes

### Version Tags

- `v{MAJOR}.{MINOR}.{PATCH}` - Standard version tags
- `v{MAJOR}.{MINOR}.{PATCH}-immutable` - Immutable snapshots (no further changes)

### Version History

See [`CHANGELOG.md`](./CHANGELOG.md) for complete version history.

## Immutability Policy

### Draft Status

While in **Draft** status, specifications MAY be modified. Changes MUST be documented in `CHANGELOG.md`.

### Immutable Releases

When a version is marked as **immutable**:

1. A git tag `v{version}-immutable` is created
2. All files are copied to `versions/v{version}/` directory
3. No further changes are allowed to that version
4. New features require a new version number

### Creating Immutable Releases

```bash
# Create version tag
git tag -a v1.0.0 -m "Bipartite-BQF Extension v1.0.0 - Initial Release"

# Create immutable snapshot
git tag -a v1.0.0-immutable -m "Immutable snapshot v1.0.0"

# Copy files to version directory
mkdir -p versions/v1.0.0
cp *.md versions/v1.0.0/
cp PACKAGE.json versions/v1.0.0/
```

## Contributing Guidelines

### Specification Changes

1. **Draft Status**: Changes MAY be made with CHANGELOG updates
2. **Review Required**: All changes MUST be reviewed before merging
3. **RFC 2119 Compliance**: All specifications MUST use RFC 2119 keywords correctly
4. **Cross-References**: All related specifications MUST be updated

### Adding Examples

1. Examples MUST be placed in `examples/` directory
2. Examples MUST be validated against specifications
3. Examples MUST include comments explaining usage

### Reference Materials

1. Reference materials MUST be placed in `reference/` directory
2. Reference materials MUST be kept up-to-date with specifications
3. Reference materials MUST include version information

## Reference Links

### Related Specifications

- [`docs/04-CanvasL/CANVASL-RFC2119-SPEC.md`](../04-CanvasL/CANVASL-RFC2119-SPEC.md) - Base CanvasL specification
- [`docs/05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md`](../05-Meta-Log/MULTIVERSE-CANVAS-RFC2119-SPEC.md) - Multiverse canvas specification
- [`wiki/horizontal/integration-guides/topology-to-system-mappings.md`](../../wiki/horizontal/integration-guides/topology-to-system-mappings.md) - Bipartite structure explanation

### Implementation References

- [`evolutions/obsidian-frontmatter-knowledge-model/`](../../evolutions/obsidian-frontmatter-knowledge-model/) - Frontmatter knowledge model
- [`ui/src/grammars/canvasl.grammar`](../../ui/src/grammars/canvasl.grammar) - CanvasL grammar (to be extended)
- [`r5rs-canvas-engine.scm`](../../r5rs-canvas-engine.scm) - R5RS function implementations (to be extended)

### Mathematical References

- [Binary Quadratic Forms](https://en.wikipedia.org/wiki/Binary_quadratic_form) - Wikipedia
- [Bipartite Graphs](https://en.wikipedia.org/wiki/Bipartite_graph) - Wikipedia
- [Semantic Versioning](https://semver.org/) - SemVer specification
- [RFC 2119](https://tools.ietf.org/html/rfc2119) - Key words for use in RFCs

## Package Metadata

See [`PACKAGE.json`](./PACKAGE.json) for complete package metadata including:
- Package name and version
- Specification format and status
- File references
- Dependencies
- Repository information

## License

MIT License - See repository LICENSE file for details.

---

**Last Updated**: 2025-01-07  
**Package Version**: 1.0.0  
**Status**: Draft

