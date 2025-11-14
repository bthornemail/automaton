# Self-Modify

Self-modify CanvasL file with snapshot/rollback safety following Autonomous CanvasL protocol specification.

## Usage

```
self-modify [options]
```

## Options

- `--target <path>` - Path to target file to modify (required)
- `--type <type>` - Modification type: `add-node`, `modify-node`, `add-edge`, `modify-edge`, `update-metadata` (required)
- `--node-id <id>` - Node/edge ID to modify (required for modify operations)
- `--data <json>` - JSON string of modification data (required)
- `--pattern <name>` - Modification pattern name (optional)
- `--create-snapshot` - Create snapshot before modification (default: true)
- `--no-snapshot` - Skip snapshot creation
- `--validate-before` - Validate before modification (default: true)
- `--no-validate-before` - Skip validation before
- `--validate-after` - Validate after modification (default: true)
- `--no-validate-after` - Skip validation after
- `--rollback-on-failure` - Rollback on failure (default: true)
- `--no-rollback` - Skip rollback
- `--reason <text>` - Reason for modification (optional)

## Protocol

Follows Autonomous CanvasL Protocol Specification Section 4.2:
1. Validate target file exists
2. Create snapshot: `r5rs:snapshot-current`
3. Validate current state: `r5rs:shacl-validate`
4. Validate modification: `r5rs:validate-modification`
5. Apply modification: `r5rs:self-modify`
6. Validate modified state: `r5rs:shacl-validate`
7. Rollback on failure: `r5rs:rollback`
8. Update provenance history

## Examples

```bash
# Add a new node
self-modify --target automaton.kernel.canvasl --type add-node --data '{"id":"new-node","type":"text","text":"New content"}'

# Modify existing node
self-modify --target automaton.kernel.canvasl --type modify-node --node-id existing-node --data '{"text":"Updated content"}'

# Add edge
self-modify --target automaton.kernel.canvasl --type add-edge --data '{"id":"new-edge","type":"edge","fromNode":"node1","toNode":"node2"}'

# Update metadata
self-modify --target automaton.kernel.canvasl --type update-metadata --node-id node-id --data '{"metadata":{"updated":true}}' --reason "Performance optimization"
```

## Related

- `docs/33-Autonomous-CanvasL/AUTONOMOUS-CANVASL-RFC2119-SPEC.md` - Main specification
- `docs/33-Autonomous-CanvasL/02-PROTOCOL-SPECIFICATION-RFC2119.md` - Protocol specification Section 4.2

