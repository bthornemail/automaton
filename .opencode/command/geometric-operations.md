# Geometric Operations

Execute regular polyhedra geometry operations following docs/32-Regulay-Polyhedra-Geometry.

## Usage

```
geometric-operations <operation> [options]
```

## Operations

### bqf-encode

Encode polyhedron as BQF (Binary Quadratic Form).

**Options:**
- `--polyhedron <name>` - Polyhedron name: `tetrahedron`, `cube`, `octahedron`, `icosahedron`, `dodecahedron` (required)
- `--include-form` - Include BQF form string (default: true)
- `--no-form` - Skip form string
- `--include-signature` - Include signature (default: true)
- `--no-signature` - Skip signature

**Example:**
```bash
geometric-operations bqf-encode --polyhedron tetrahedron
```

### polyhedra-transform

Transform polyhedra using BQF operations.

**Options:**
- `--operation <op>` - Transformation: `dual-swap`, `apply-bqf`, `abstract-bqf` (required)
- `--input <input>` - BQF coefficients as JSON array `[a,b,c]` or polyhedron name (required)
- `--preserve-structure` - Preserve structure (default: false)

**Example:**
```bash
geometric-operations polyhedra-transform --operation dual-swap --input cube
geometric-operations polyhedra-transform --operation apply-bqf --input '[4,6,4]'
```

### compute-mapping

Compute R5RS type to polyhedra mapping.

**Options:**
- `--r5rs-type <type>` - R5RS type: `boolean`, `pair`, `symbol`, `number`, `char`, `string`, `vector`, `procedure` (required)
- `--include-dimension` - Include dimension (default: true)
- `--no-dimension` - Skip dimension
- `--include-bqf` - Include BQF encoding (default: true)
- `--no-bqf` - Skip BQF

**Example:**
```bash
geometric-operations compute-mapping --r5rs-type pair
geometric-operations compute-mapping --r5rs-type procedure --no-dimension
```

### validate

Validate geometric structure.

**Options:**
- `--structure-type <type>` - Structure type: `polyhedron`, `bqf`, `mapping` (required)
- `--structure-data <json>` - JSON structure data (required)
- `--validate-bqf` - Validate BQF (default: false)
- `--validate-dimensional` - Validate dimensional progression (default: false)
- `--validate-bipartite` - Validate bipartite structure (default: false)

**Example:**
```bash
geometric-operations validate --structure-type bqf --structure-data '{"bqf":{"coefficients":[4,6,4]}}' --validate-bqf
```

## Related

- `docs/32-Regulay-Polyhedra-Geometry/README.md` - Overview
- `docs/32-Regulay-Polyhedra-Geometry/04-COMPUTATIONAL-MAPPING.md` - R5RS type mapping
- `docs/32-Regulay-Polyhedra-Geometry/05-BQF-ENCODING.md` - BQF encoding

