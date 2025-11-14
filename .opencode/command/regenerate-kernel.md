# Regenerate Kernel

Regenerate full kernel from minimal seed file following Autonomous CanvasL protocol specification.

## Usage

```
regenerate-kernel [options]
```

## Options

- `--source <path>` - Path to kernel seed file (default: `evolutions/automaton.kernel.seed.canvasl`)
- `--target <path>` - Path to target kernel file (default: `evolutions/automaton.kernel.canvasl`)
- `--validate` - Validate regenerated kernel (default: true)
- `--no-validate` - Skip validation
- `--preserve-provenance` - Preserve provenance history (default: true)
- `--no-provenance` - Skip provenance preservation
- `--create-snapshot` - Create snapshot before regeneration (default: true)
- `--no-snapshot` - Skip snapshot creation

## Protocol

Follows Autonomous CanvasL Protocol Specification Section 4.1:
1. Validate seed file exists
2. Create snapshot (if requested)
3. Load seed: `r5rs:parse-jsonl-canvas`
4. Extract facts: `r5rs:extract-facts`
5. Generate RDF: `r5rs:jsonl-to-rdf`
6. Query patterns: `r5rs:sparql-query`
7. Invoke R5RS functions: `r5rs:invoke-from-jsonl`
8. Generate nodes and edges
9. Load R5RS functions from trie
10. Validate: `r5rs:shacl-validate`
11. Write kernel file

## Examples

```bash
# Regenerate kernel with defaults
regenerate-kernel

# Regenerate with custom paths
regenerate-kernel --source custom.seed.canvasl --target custom.kernel.canvasl

# Regenerate without validation
regenerate-kernel --no-validate

# Regenerate without snapshot
regenerate-kernel --no-snapshot
```

## Related

- `docs/33-Autonomous-CanvasL/AUTONOMOUS-CANVASL-RFC2119-SPEC.md` - Main specification
- `docs/33-Autonomous-CanvasL/02-PROTOCOL-SPECIFICATION-RFC2119.md` - Protocol specification Section 4.1

