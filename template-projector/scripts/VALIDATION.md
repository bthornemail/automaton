# Content Index Validation

This document describes the validation system for `content-index.jsonl` that ensures compliance with the Bipartite-BQF Frontmatter Integration Specification (`docs/28-Canvasl-Frontmatter-Knowledge-Model/03-FRONTMATTER-INTEGRATION-RFC2119.md`).

## Overview

The validation system checks:

1. **Document Entries**: Required fields, valid sources, dimension format
2. **Relationship Entries**: Valid relationship types and references
3. **RDF Triple Entries**: Valid subject-predicate-object structure
4. **Bipartite Metadata**: When present, validates BQF and polynomial objects according to RFC 2119 spec

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
  - `form`: Must be a string
  - `coefficients`: Must be an array of numbers
  - `signature`: Must be one of: `"euclidean"`, `"lorentz"`, `"custom"`
  - `variables`: Must be an array of strings

- **Optional Fields**:
  - `polynomial`: String (optional)
  - `symbol`: String (optional)
  - `procedure`: String (optional)

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
- `BQF_INVALID_SIGNATURE`: BQF signature must be euclidean, lorentz, or custom
- `BQF_MISSING_VARIABLES`: BQF variables array is required
- `BQF_INVALID_VARIABLES`: BQF variables must be strings

### Polynomial Errors

- `POLY_INVALID_FORMAT`: Polynomial must be an object
- `POLY_MISSING_ARRAY`: Polynomial array is required
- `POLY_INVALID_LENGTH`: Polynomial array must have 8 elements
- `POLY_INVALID_NUMBERS`: Polynomial array must contain only numbers

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

