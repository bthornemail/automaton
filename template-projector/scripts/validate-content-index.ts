#!/usr/bin/env tsx
/**
 * Content Index Validator
 * 
 * Validates content-index.jsonl against Bipartite-BQF Frontmatter Integration Specification
 * (docs/28-Canvasl-Frontmatter-Knowledge-Model/03-FRONTMATTER-INTEGRATION-RFC2119.md)
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

interface ValidationError {
  code: string;
  message: string;
  entry?: any;
  field?: string;
}

interface ValidationResult {
  valid: boolean;
  errors: ValidationError[];
  warnings: ValidationError[];
  stats: {
    totalEntries: number;
    documents: number;
    relationships: number;
    rdfTriples: number;
    validated: number;
  };
}

/**
 * Validate dimension value
 */
function validateDimension(dimension: any): boolean {
  if (typeof dimension !== 'string') return false;
  return /^[0-7]D$/i.test(dimension);
}

/**
 * Validate partition value
 */
function validatePartition(partition: any): boolean {
  return partition === 'topology' || partition === 'system';
}

/**
 * Validate BQF object
 */
function validateBQF(bqf: any): ValidationError[] {
  const errors: ValidationError[] = [];

  if (!bqf || typeof bqf !== 'object') {
    return [{ code: 'BQF_INVALID_FORMAT', message: 'BQF must be an object' }];
  }

  // Required fields
  if (!bqf.form || typeof bqf.form !== 'string') {
    errors.push({ code: 'BQF_MISSING_FORM', message: 'BQF form is required' });
  }

  if (!Array.isArray(bqf.coefficients)) {
    errors.push({ code: 'BQF_MISSING_COEFFICIENTS', message: 'BQF coefficients array is required' });
  } else {
    const invalidCoeffs = bqf.coefficients.filter((c: any) => typeof c !== 'number');
    if (invalidCoeffs.length > 0) {
      errors.push({ code: 'BQF_INVALID_COEFFICIENTS', message: 'BQF coefficients must be numbers' });
    }
  }

  if (!bqf.signature || typeof bqf.signature !== 'string') {
    errors.push({ code: 'BQF_MISSING_SIGNATURE', message: 'BQF signature is required' });
  } else {
    const validSignatures = ['euclidean', 'lorentz', 'custom'];
    if (!validSignatures.includes(bqf.signature)) {
      errors.push({ code: 'BQF_INVALID_SIGNATURE', message: `BQF signature must be one of: ${validSignatures.join(', ')}` });
    }
  }

  if (!Array.isArray(bqf.variables)) {
    errors.push({ code: 'BQF_MISSING_VARIABLES', message: 'BQF variables array is required' });
  } else {
    const invalidVars = bqf.variables.filter((v: any) => typeof v !== 'string');
    if (invalidVars.length > 0) {
      errors.push({ code: 'BQF_INVALID_VARIABLES', message: 'BQF variables must be strings' });
    }
  }

  return errors;
}

/**
 * Validate polynomial object
 */
function validatePolynomial(poly: any): ValidationError[] {
  const errors: ValidationError[] = [];

  if (!poly || typeof poly !== 'object') {
    return [{ code: 'POLY_INVALID_FORMAT', message: 'Polynomial must be an object' }];
  }

  // Required arrays
  const requiredArrays = ['monad', 'functor', 'perceptron'];
  for (const arrayName of requiredArrays) {
    if (!Array.isArray(poly[arrayName])) {
      errors.push({ code: 'POLY_MISSING_ARRAY', message: `Polynomial ${arrayName} array is required` });
    } else {
      if (poly[arrayName].length !== 8) {
        errors.push({ code: 'POLY_INVALID_LENGTH', message: `Polynomial ${arrayName} must have 8 elements` });
      }
      const invalidNums = poly[arrayName].filter((n: any) => typeof n !== 'number');
      if (invalidNums.length > 0) {
        errors.push({ code: 'POLY_INVALID_NUMBERS', message: `Polynomial ${arrayName} must contain only numbers` });
      }
    }
  }

  return errors;
}

/**
 * Validate bipartite section
 */
function validateBipartite(bipartite: any, entry: any): ValidationError[] {
  const errors: ValidationError[] = [];

  if (!bipartite || typeof bipartite !== 'object') {
    return errors; // Bipartite is optional
  }

  // Required fields
  if (!validatePartition(bipartite.partition)) {
    errors.push({
      code: 'FRONTMATTER_INVALID_PARTITION',
      message: `Partition must be "topology" or "system", got: ${bipartite.partition}`,
      entry,
      field: 'bipartite.partition'
    });
  }

  if (!validateDimension(bipartite.dimension)) {
    errors.push({
      code: 'FRONTMATTER_INVALID_DIMENSION',
      message: `Dimension must be "0D" through "7D", got: ${bipartite.dimension}`,
      entry,
      field: 'bipartite.dimension'
    });
  }

  // Validate BQF if present
  if (bipartite.bqf) {
    const bqfErrors = validateBQF(bipartite.bqf);
    errors.push(...bqfErrors.map(e => ({ ...e, entry, field: 'bipartite.bqf' })));
  }

  // Validate polynomial if present
  if (bipartite.polynomial) {
    const polyErrors = validatePolynomial(bipartite.polynomial);
    errors.push(...polyErrors.map(e => ({ ...e, entry, field: 'bipartite.polynomial' })));
  }

  // Validate relationships if present
  if (bipartite.relationships) {
    if (typeof bipartite.relationships !== 'object') {
      errors.push({
        code: 'FRONTMATTER_INVALID_RELATIONSHIPS',
        message: 'Relationships must be an object',
        entry,
        field: 'bipartite.relationships'
      });
    } else {
      if (bipartite.relationships.topology !== null && typeof bipartite.relationships.topology !== 'string') {
        errors.push({
          code: 'FRONTMATTER_INVALID_RELATIONSHIP',
          message: 'Topology relationship must be a string or null',
          entry,
          field: 'bipartite.relationships.topology'
        });
      }
      if (bipartite.relationships.system !== null && typeof bipartite.relationships.system !== 'string') {
        errors.push({
          code: 'FRONTMATTER_INVALID_RELATIONSHIP',
          message: 'System relationship must be a string or null',
          entry,
          field: 'bipartite.relationships.system'
        });
      }
    }
  }

  return errors;
}

/**
 * Validate document entry
 */
function validateDocumentEntry(entry: any): ValidationError[] {
  const errors: ValidationError[] = [];
  const warnings: ValidationError[] = [];

  // Required fields
  if (!entry.id || typeof entry.id !== 'string') {
    errors.push({ code: 'DOC_MISSING_ID', message: 'Document entry must have id field' });
  }

  if (!entry.type || entry.type !== 'document') {
    errors.push({ code: 'DOC_INVALID_TYPE', message: 'Document entry must have type="document"' });
  }

  if (!entry.source || !['wiki', 'docs', 'grok_files', 'evolutions'].includes(entry.source)) {
    errors.push({ code: 'DOC_INVALID_SOURCE', message: `Source must be one of: wiki, docs, grok_files, evolutions, got: ${entry.source}` });
  }

  // Validate dimension if present
  if (entry.dimension !== undefined) {
    if (!validateDimension(entry.dimension)) {
      warnings.push({
        code: 'DOC_INVALID_DIMENSION',
        message: `Dimension should be "0D" through "7D", got: ${entry.dimension}`,
        entry,
        field: 'dimension'
      });
    }
  }

  // Validate frontmatter if present
  if (entry.frontmatter) {
    if (typeof entry.frontmatter !== 'object') {
      errors.push({ code: 'DOC_INVALID_FRONTMATTER', message: 'Frontmatter must be an object', entry });
    } else {
      // Validate bipartite section if present
      if (entry.frontmatter.bipartite) {
        const bipartiteErrors = validateBipartite(entry.frontmatter.bipartite, entry);
        errors.push(...bipartiteErrors);
      }
    }
  }

  return errors;
}

/**
 * Validate relationship entry
 */
function validateRelationshipEntry(entry: any): ValidationError[] {
  const errors: ValidationError[] = [];

  if (!entry.type || entry.type !== 'relationship') {
    errors.push({ code: 'REL_INVALID_TYPE', message: 'Relationship entry must have type="relationship"' });
  }

  if (!entry.from || typeof entry.from !== 'string') {
    errors.push({ code: 'REL_MISSING_FROM', message: 'Relationship entry must have from field' });
  }

  if (!entry.to || typeof entry.to !== 'string') {
    errors.push({ code: 'REL_MISSING_TO', message: 'Relationship entry must have to field' });
  }

  if (!entry.relType || !['prerequisite', 'enables', 'related'].includes(entry.relType)) {
    errors.push({ code: 'REL_INVALID_TYPE', message: `relType must be one of: prerequisite, enables, related, got: ${entry.relType}` });
  }

  return errors;
}

/**
 * Validate RDF triple entry
 */
function validateRDFTripleEntry(entry: any): ValidationError[] {
  const errors: ValidationError[] = [];

  if (!entry.type || entry.type !== 'rdf-triple') {
    errors.push({ code: 'RDF_INVALID_TYPE', message: 'RDF triple entry must have type="rdf-triple"' });
  }

  if (!entry.subject || typeof entry.subject !== 'string') {
    errors.push({ code: 'RDF_MISSING_SUBJECT', message: 'RDF triple entry must have subject field' });
  }

  if (!entry.predicate || typeof entry.predicate !== 'string') {
    errors.push({ code: 'RDF_MISSING_PREDICATE', message: 'RDF triple entry must have predicate field' });
  }

  if (!entry.object || typeof entry.object !== 'string') {
    errors.push({ code: 'RDF_MISSING_OBJECT', message: 'RDF triple entry must have object field' });
  }

  return errors;
}

/**
 * Validate content index file
 */
function validateContentIndex(filePath: string): ValidationResult {
  const result: ValidationResult = {
    valid: true,
    errors: [],
    warnings: [],
    stats: {
      totalEntries: 0,
      documents: 0,
      relationships: 0,
      rdfTriples: 0,
      validated: 0
    }
  };

  if (!fs.existsSync(filePath)) {
    result.errors.push({ code: 'FILE_NOT_FOUND', message: `Content index file not found: ${filePath}` });
    result.valid = false;
    return result;
  }

  const content = fs.readFileSync(filePath, 'utf-8');
  const lines = content.split('\n').filter(line => line.trim());

  for (const line of lines) {
    if (!line.trim() || !line.trim().startsWith('{')) continue;

    try {
      const entry = JSON.parse(line);
      result.stats.totalEntries++;

      // Validate based on entry type
      let entryErrors: ValidationError[] = [];
      let entryWarnings: ValidationError[] = [];

      switch (entry.type) {
        case 'document':
          result.stats.documents++;
          entryErrors = validateDocumentEntry(entry);
          break;
        case 'relationship':
          result.stats.relationships++;
          entryErrors = validateRelationshipEntry(entry);
          break;
        case 'rdf-triple':
          result.stats.rdfTriples++;
          entryErrors = validateRDFTripleEntry(entry);
          break;
        default:
          entryWarnings.push({
            code: 'UNKNOWN_TYPE',
            message: `Unknown entry type: ${entry.type}`,
            entry
          });
      }

      result.errors.push(...entryErrors);
      result.warnings.push(...entryWarnings);
      result.stats.validated++;

    } catch (error) {
      result.errors.push({
        code: 'PARSE_ERROR',
        message: `Failed to parse JSON line: ${error instanceof Error ? error.message : String(error)}`,
        entry: { line: line.substring(0, 100) }
      });
    }
  }

  result.valid = result.errors.length === 0;
  return result;
}

/**
 * Main validation function
 */
async function main() {
  const contentIndexPath = path.join(__dirname, '..', 'content-index.jsonl');

  console.log('🔍 Validating content index...');
  console.log(`   File: ${contentIndexPath}`);

  const result = validateContentIndex(contentIndexPath);

  // Print statistics
  console.log('\n📊 Statistics:');
  console.log(`   Total entries: ${result.stats.totalEntries}`);
  console.log(`   Documents: ${result.stats.documents}`);
  console.log(`   Relationships: ${result.stats.relationships}`);
  console.log(`   RDF Triples: ${result.stats.rdfTriples}`);
  console.log(`   Validated: ${result.stats.validated}`);

  // Print errors
  if (result.errors.length > 0) {
    console.log(`\n❌ Errors (${result.errors.length}):`);
    for (const error of result.errors.slice(0, 20)) {
      console.log(`   [${error.code}] ${error.message}`);
      if (error.field) {
        console.log(`      Field: ${error.field}`);
      }
      if (error.entry?.id) {
        console.log(`      Entry ID: ${error.entry.id}`);
      }
    }
    if (result.errors.length > 20) {
      console.log(`   ... and ${result.errors.length - 20} more errors`);
    }
  }

  // Print warnings
  if (result.warnings.length > 0) {
    console.log(`\n⚠️  Warnings (${result.warnings.length}):`);
    for (const warning of result.warnings.slice(0, 10)) {
      console.log(`   [${warning.code}] ${warning.message}`);
      if (warning.field) {
        console.log(`      Field: ${warning.field}`);
      }
    }
    if (result.warnings.length > 10) {
      console.log(`   ... and ${result.warnings.length - 10} more warnings`);
    }
  }

  // Print summary
  if (result.valid) {
    console.log('\n✅ Content index is valid!');
    process.exit(0);
  } else {
    console.log(`\n❌ Content index has ${result.errors.length} validation errors`);
    process.exit(1);
  }
}

// Export for use in other modules
export { validateContentIndex, ValidationResult, ValidationError };

// Run if executed directly
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('❌ Validation failed:', error);
    process.exit(1);
  });
}

