# Content Index Validation

This document describes the validation system for `content-index.jsonl` that ensures compliance with the Bipartite-BQF Frontmatter Integration Specification (`docs/28-Canvasl-Frontmatter-Knowledge-Model/03-FRONTMATTER-INTEGRATION-RFC2119.md`).

## Overview

The validation system checks:

1. **Document Entries**: Required fields, valid sources, dimension format
2. **Relationship Entries**: Valid relationship types and references
3. **RDF Triple Entries**: Valid subject-predicate-object structure
4. **Bipartite Metadata**: When present, validates BQF and polynomial objects according to RFC 2119 spec
5. **BQF Progression**: Validates BQF forms match expected patterns for each dimension
6. **Variable Count**: Validates variable count matches dimension requirements
7. **Mapping Chain**: Validates Symbol → Polynomial → BQF → Procedure chain
8. **Cross-Entry Consistency**: Validates bipartite relationships reference existing nodes
9. **Dimensional Progression**: Validates BQF progression consistency across dimensions
10. **Bipartite Graph**: Validates graph structure consistency

## Validation Rules

### Document Entry Validation

- **Required Fields**:
  - `id`: Must be a string
  - `type`: Must be `"document"`
  - `source`: Must be one of: `wiki`, `docs`, `grok_files`, `evolutions`

- **Optional Fields**:
  - `dimension`: If present, must match pattern `"0D"` through `"7D"`
  - `frontmatter`: If present, must be an object
  - `frontmatter.bipartite`: If present, must conform to bipartite schema

### Bipartite Section Validation

When `frontmatter.bipartite` is present:

- **Required Fields**:
  - `partition`: Must be `"topology"` or `"system"`
  - `dimension`: Must be `"0D"` through `"7D"`

- **Optional Fields**:
  - `bqf`: BQF object (if present, must have valid form, coefficients, signature, variables)
  - `polynomial`: Polynomial object (if present, must have monad, functor, perceptron arrays with 8 numbers each)
  - `relationships`: Relationships object (if present, topology and system must be strings or null)

### BQF Object Validation

When `bipartite.bqf` is present:

- **Required Fields**:
  - `form`: Must be a string matching expected progression pattern for dimension
  - `coefficients`: Must be an array of valid numbers (not NaN or Infinity)
  - `signature`: Must be one of: `"euclidean"`, `"lorentz"`, `"consensus"`, `"intelligence"`, `"quantum"`, `"custom"`
  - `variables`: Must be an array of strings matching expected count for dimension

- **Optional Fields**:
  - `polynomial`: String (optional, must match BQF form if present)
  - `symbol`: String (optional, must be valid S-expression or identifier)
  - `procedure`: String (optional, must be valid R5RS Scheme expression)

#### BQF Progression Validation

BQF forms must match expected patterns for each dimension:

- **0D**: `Q() = 0` (0 variables)
- **1D**: `Q(x) = x²` (1 variable: `x`)
- **2D**: `Q(x,y) = x² + y²` (2 variables: `x`, `y`)
- **3D**: `Q(x,y,z,t) = x² + y² + z² - t²` (4 variables: `x`, `y`, `z`, `t`)
- **4D**: `Q(w,x,y,z,t) = w² + x² + y² + z² - t²` (5 variables: `w`, `x`, `y`, `z`, `t`)
- **5D**: `Q(...) = Σᵢ xᵢ² - t²` (5+ variables)
- **6D**: `Q(...) = Σᵢ xᵢ² - t² + higher terms` (6+ variables)
- **7D**: `Q(...) = Σᵢ xᵢ² - t² + quantum terms` (7+ variables)

#### Variable Count Validation

Variable count must match dimension requirements:

- **0D**: 0 variables
- **1D**: 1 variable (`x`)
- **2D**: 2 variables (`x`, `y`)
- **3D**: 4 variables (`x`, `y`, `z`, `t`)
- **4D**: 5 variables (`w`, `x`, `y`, `z`, `t`)
- **5D+**: Variable count matches dimension number

#### Mapping Chain Validation

When all mapping components are present (symbol, polynomial, procedure), they are validated as a chain:

- **Symbol**: Must be valid S-expression (matching parentheses) or identifier
- **Polynomial**: Must match BQF form structure
- **BQF Form**: Must match polynomial expression
- **Procedure**: Must be valid R5RS Scheme syntax (matching parentheses, valid lambda syntax)

### Cross-Entry Validation

The validator performs cross-entry consistency checks:

- **Bipartite Relationships**: References in `bipartite.relationships.topology` and `bipartite.relationships.system` must point to existing document IDs
- **Dimensional Progression**: BQF progression consistency is checked across dimensions (0D → 1D → 2D → ...)
- **Graph Structure**: Bipartite graph structure is validated for consistency

### Polynomial Object Validation

When `bipartite.polynomial` is present:

- **Required Arrays** (each must have exactly 8 numbers):
  - `monad`: Array of 8 numbers
  - `functor`: Array of 8 numbers
  - `perceptron`: Array of 8 numbers

### Relationship Entry Validation

- **Required Fields**:
  - `type`: Must be `"relationship"`
  - `from`: Must be a string (document ID)
  - `to`: Must be a string (document ID)
  - `relType`: Must be one of: `"prerequisite"`, `"enables"`, `"related"`

### RDF Triple Entry Validation

- **Required Fields**:
  - `type`: Must be `"rdf-triple"`
  - `subject`: Must be a string
  - `predicate`: Must be a string
  - `object`: Must be a string

## Error Codes

### Document Errors

- `DOC_MISSING_ID`: Document entry missing id field
- `DOC_INVALID_TYPE`: Document entry has invalid type
- `DOC_INVALID_SOURCE`: Document entry has invalid source
- `DOC_INVALID_DIMENSION`: Document dimension format invalid (warning)
- `DOC_INVALID_FRONTMATTER`: Frontmatter is not an object

### Bipartite Errors

- `FRONTMATTER_INVALID_PARTITION`: Partition must be "topology" or "system"
- `FRONTMATTER_INVALID_DIMENSION`: Dimension must be "0D" through "7D"
- `FRONTMATTER_INVALID_RELATIONSHIPS`: Relationships object invalid
- `FRONTMATTER_INVALID_RELATIONSHIP`: Individual relationship invalid

### BQF Errors

- `BQF_INVALID_FORMAT`: BQF must be an object
- `BQF_MISSING_FORM`: BQF form is required
- `BQF_MISSING_COEFFICIENTS`: BQF coefficients array is required
- `BQF_INVALID_COEFFICIENTS`: BQF coefficients must be numbers
- `BQF_MISSING_SIGNATURE`: BQF signature is required
- `BQF_INVALID_SIGNATURE`: BQF signature must be one of: euclidean, lorentz, consensus, intelligence, quantum, custom
- `BQF_INVALID_PROGRESSION`: BQF form does not match expected progression pattern for dimension
- `BQF_INVALID_VARIABLE_COUNT`: BQF variable count does not match dimension requirements
- `BQF_MISSING_VARIABLES`: BQF variables array is required
- `BQF_INVALID_VARIABLES`: BQF variables must be strings

### Polynomial Errors

- `POLY_INVALID_FORMAT`: Polynomial must be an object
- `POLY_MISSING_ARRAY`: Polynomial array is required
- `POLY_INVALID_LENGTH`: Polynomial array must have 8 elements
- `POLY_INVALID_NUMBERS`: Polynomial array must contain only valid numbers

### Mapping Chain Errors

- `MAPPING_INVALID_SYMBOL`: Symbol format invalid (must be valid S-expression or identifier)
- `MAPPING_INVALID_POLYNOMIAL`: Polynomial expression does not match BQF form
- `MAPPING_INVALID_PROCEDURE`: Procedure syntax invalid (must be valid R5RS Scheme)

### Relationship Errors

- `REL_INVALID_TYPE`: Relationship entry has invalid type
- `REL_MISSING_FROM`: Relationship entry missing from field
- `REL_MISSING_TO`: Relationship entry missing to field
- `REL_INVALID_TYPE`: Relationship relType invalid

### RDF Errors

- `RDF_INVALID_TYPE`: RDF triple entry has invalid type
- `RDF_MISSING_SUBJECT`: RDF triple entry missing subject field
- `RDF_MISSING_PREDICATE`: RDF triple entry missing predicate field
- `RDF_MISSING_OBJECT`: RDF triple entry missing object field

## Usage

### Command Line

```bash
# Validate content index
npm run validate:content-index

# Build and validate (automatic)
npm run build:content-index
```

### Programmatic

```typescript
import { validateContentIndex } from './scripts/validate-content-index.js';

const result = validateContentIndex('./content-index.jsonl');

if (result.valid) {
  console.log('✅ Content index is valid!');
} else {
  console.error(`❌ Found ${result.errors.length} errors`);
  for (const error of result.errors) {
    console.error(`  [${error.code}] ${error.message}`);
  }
}
```

## Integration

Validation is automatically run:

1. **After Build**: `build-content-index.ts` runs validation after building the index
2. **Before Production Build**: `prebuild` hook runs validation before Vite build
3. **Standalone**: Can be run independently with `npm run validate:content-index`

## Compliance

This validation system ensures compliance with:

- **`docs/28-Canvasl-Frontmatter-Knowledge-Model/03-FRONTMATTER-INTEGRATION-RFC2119.md`**: Frontmatter Integration Specification
- **`docs/28-Canvasl-Frontmatter-Knowledge-Model/reference/validation-rules.md`**: Validation Rules Reference

All validation rules follow RFC 2119 keywords:
- **MUST**: Required (errors if missing/invalid)
- **SHOULD**: Recommended (warnings if missing/invalid)
- **MAY**: Optional (no validation)

