# Goal Negotiate

Negotiate goals with multi-agent system following Autonomous CanvasL protocol specification.

## Usage

```
goal-negotiate [options]
```

## Options

- `--agents <json>` - JSON array of agent IDs with optional weights (required)
- `--goals <json>` - JSON array of goals with id, description, priority, constraints (required)
- `--algorithm <algo>` - Negotiation algorithm: `borda`, `grover`, `consensus` (default: `borda`)
- `--timeout <ms>` - Timeout in milliseconds (default: 5000)
- `--require-consensus` - Require consensus (default: true)
- `--no-consensus` - Don't require consensus

## Protocol

Follows Autonomous CanvasL Protocol Specification Section 4.3:
1. Validate agents exist and are available
2. Broadcast goals to all agents
3. Collect agent responses (votes, preferences, constraints)
4. Apply negotiation algorithm (Borda, Grover, consensus)
5. Calculate negotiated goals
6. Generate action plan

## Examples

```bash
# Negotiate with Borda algorithm
goal-negotiate \
  --agents '[{"id":"0D-Topology-Agent","weight":1.0},{"id":"6D-Intelligence-Agent","weight":2.0}]' \
  --goals '[{"id":"goal-1","description":"Optimize kernel regeneration","priority":"high"}]'

# Negotiate with Grover algorithm
goal-negotiate \
  --agents '[{"id":"5D-Consensus-Agent"},{"id":"6D-Intelligence-Agent"}]' \
  --goals '[{"id":"goal-1","description":"Improve performance","priority":"medium"}]' \
  --algorithm grover

# Negotiate with timeout
goal-negotiate \
  --agents '[{"id":"0D-Topology-Agent"}]' \
  --goals '[{"id":"goal-1","description":"Test goal","priority":"low"}]' \
  --timeout 10000
```

## Related

- `docs/33-Autonomous-CanvasL/AUTONOMOUS-CANVASL-RFC2119-SPEC.md` - Main specification
- `docs/33-Autonomous-CanvasL/02-PROTOCOL-SPECIFICATION-RFC2119.md` - Protocol specification Section 4.3

