# Consensus Vote

Execute consensus vote on proposal following Autonomous CanvasL protocol specification.

## Usage

```
consensus-vote [options]
```

## Options

- `--proposal <json>` - JSON proposal object with id, type, description, target, changes (required)
- `--agents <json>` - JSON array of agent IDs with optional required flag (required)
- `--threshold <rate>` - Consensus threshold (0-1, default: 0.75)
- `--timeout <ms>` - Timeout in milliseconds (default: 10000)
- `--quorum <count>` - Quorum count (default: all agents)

## Protocol

Follows Autonomous CanvasL Protocol Specification Section 4.4:
1. Validate proposal
2. Broadcast proposal to required agents
3. Collect votes from agents
4. Calculate consensus (check threshold, quorum)
5. Execute if consensus reached
6. Return approval/rejection

## Examples

```bash
# Vote on modification proposal
consensus-vote \
  --proposal '{"id":"proposal-1","type":"modification","description":"Optimize kernel","target":"automaton.kernel.canvasl"}' \
  --agents '[{"id":"5D-Consensus-Agent","required":true},{"id":"6D-Intelligence-Agent","required":true}]' \
  --threshold 0.75

# Vote with custom quorum
consensus-vote \
  --proposal '{"id":"proposal-2","type":"evolution","description":"Evolve automaton","target":"automaton.kernel.canvasl"}' \
  --agents '[{"id":"5D-Consensus-Agent"},{"id":"6D-Intelligence-Agent"},{"id":"0D-Topology-Agent"}]' \
  --quorum 2 \
  --threshold 0.8
```

## Related

- `docs/33-Autonomous-CanvasL/AUTONOMOUS-CANVASL-RFC2119-SPEC.md` - Main specification
- `docs/33-Autonomous-CanvasL/02-PROTOCOL-SPECIFICATION-RFC2119.md` - Protocol specification Section 4.4

