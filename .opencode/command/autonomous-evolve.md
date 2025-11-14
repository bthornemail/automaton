# Autonomous Evolve

Execute autonomous evolution with fitness evaluation following Autonomous CanvasL protocol specification.

## Usage

```
autonomous-evolve [options]
```

## Options

- `--current-state <path>` - Path to current state file (required)
- `--goal-fitness <rate>` - Target fitness (0-1, optional)
- `--max-iterations <count>` - Maximum iterations (default: 100)
- `--mutation-rate <rate>` - Mutation rate (0-1, default: 0.1)
- `--selection-strategy <strategy>` - Selection strategy (default: `fitness-proportional`)
- `--validate-each-step` - Validate each step (default: true)
- `--no-validate` - Skip validation

## Protocol

Follows Autonomous CanvasL Protocol Specification Section 4.5:
1. Load current state
2. Evaluate current fitness
3. For each iteration (up to maxIterations):
   - Generate candidate mutations
   - Evaluate fitness of candidates
   - Select best candidate
   - Apply mutation (if validation passes)
   - Update fitness
   - Check termination condition
4. Return best evolved state

## Examples

```bash
# Evolve with default settings
autonomous-evolve --current-state automaton.kernel.canvasl

# Evolve with fitness target
autonomous-evolve --current-state automaton.kernel.canvasl --goal-fitness 0.95

# Evolve with custom parameters
autonomous-evolve \
  --current-state automaton.kernel.canvasl \
  --max-iterations 200 \
  --mutation-rate 0.15 \
  --selection-strategy fitness-proportional

# Evolve without validation
autonomous-evolve --current-state automaton.kernel.canvasl --no-validate
```

## Related

- `docs/33-Autonomous-CanvasL/AUTONOMOUS-CANVASL-RFC2119-SPEC.md` - Main specification
- `docs/33-Autonomous-CanvasL/02-PROTOCOL-SPECIFICATION-RFC2119.md` - Protocol specification Section 4.5

