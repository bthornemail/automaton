# API Reference

## Overview

Complete API reference for the Computational Topology Canvas system, covering all major modules and functions.

**Table of Contents**
- [Blackboard API](#blackboard-api)
- [Agent API](#agent-api)
- [R5RS API](#r5rs-api)
- [ProLog API](#prolog-api)
- [DataLog API](#datalog-api)
- [RDF/SPARQL API](#rdf-sparql-api)
- [SHACL API](#shacl-api)

## Blackboard API

### `blackboard.addFact(fact)`

Add a fact to the blackboard.

```typescript
blackboard.addFact(fact: Fact): Promise<string>
```

**Parameters:**
- `fact`: Fact object to add

**Returns:** Promise resolving to fact ID

**Example:**
```typescript
const id = await blackboard.addFact({
  type: 'rdf-triple',
  subject: 'ex:Alice',
  predicate: 'ex:knows',
  object: 'ex:Bob'
});
```

### `blackboard.query(pattern)`

Query facts matching a pattern.

```typescript
blackboard.query(pattern: Pattern): Promise<Fact[]>
```

**Parameters:**
- `pattern`: Query pattern with variables

**Returns:** Promise resolving to array of matching facts

**Example:**
```typescript
const facts = await blackboard.query({
  type: 'rdf-triple',
  subject: 'ex:Alice',
  predicate: '?p',
  object: '?o'
});
```

### `blackboard.retract(id)`

Remove a fact from the blackboard.

```typescript
blackboard.retract(id: string): Promise<boolean>
```

**Parameters:**
- `id`: ID of fact to remove

**Returns:** Promise resolving to success boolean

**Example:**
```typescript
await blackboard.retract('fact-123');
```

### `blackboard.subscribe(pattern, callback)`

Subscribe to fact additions matching pattern.

```typescript
blackboard.subscribe(pattern: Pattern, callback: (fact: Fact) => void): Subscription
```

**Parameters:**
- `pattern`: Pattern to match
- `callback`: Function to call when matching fact is added

**Returns:** Subscription object

**Example:**
```typescript
const sub = blackboard.subscribe(
  { type: 'rdf-triple', predicate: 'ex:knows' },
  (fact) => console.log('New relationship:', fact)
);

// Later: unsubscribe
sub.unsubscribe();
```

## Agent API

### `getAgent(agentId)`

Get an agent by ID.

```typescript
getAgent(agentId: string): Agent | undefined
```

**Parameters:**
- `agentId`: Agent identifier (e.g., '0d-topology-agent')

**Returns:** Agent instance or undefined

**Example:**
```typescript
const agent = getAgent('0d-topology-agent');
```

### `agent.query(query)`

Send a query to an agent.

```typescript
agent.query(query: Query): Promise<Response>
```

**Parameters:**
- `query`: Query object

**Returns:** Promise resolving to response

**Example:**
```typescript
const response = await agent.query({
  type: 'analyze-topology',
  data: { graph: myGraph }
});
```

### `agent.update(data)`

Update agent state.

```typescript
agent.update(data: any): Promise<void>
```

**Parameters:**
- `data`: Update data

**Returns:** Promise resolving when complete

**Example:**
```typescript
await agent.update({
  config: { maxDepth: 10 }
});
```

### `agent.status()`

Get agent status.

```typescript
agent.status(): AgentStatus
```

**Returns:** Agent status object

**Example:**
```typescript
const status = agent.status();
console.log(status.state);  // 'active', 'idle', 'error'
console.log(status.queueSize);  // Number of pending queries
```

### `registerAgent(config)`

Register a new agent.

```typescript
registerAgent(config: AgentConfig): Agent
```

**Parameters:**
- `config`: Agent configuration

**Returns:** Registered agent instance

**Example:**
```typescript
const agent = registerAgent({
  id: 'custom-agent',
  dimension: '0D',
  capabilities: ['custom-analysis'],
  handler: async (query) => {
    // Custom logic
    return { result: 'success' };
  }
});
```

## R5RS API

### `evaluateR5RS(code, env?)`

Evaluate R5RS Scheme code.

```typescript
evaluateR5RS(code: string, env?: Environment): any
```

**Parameters:**
- `code`: R5RS Scheme code string
- `env`: Optional environment (defaults to global)

**Returns:** Evaluation result

**Example:**
```typescript
const result = evaluateR5RS('((lambda (x) (* x x)) 5)');
console.log(result);  // 25
```

### `parseR5RS(code)`

Parse R5RS code to AST.

```typescript
parseR5RS(code: string): AST
```

**Parameters:**
- `code`: R5RS code string

**Returns:** Abstract syntax tree

**Example:**
```typescript
const ast = parseR5RS('(lambda (x) x)');
```

### `makeChurchNumeral(n)`

Create Church numeral from integer.

```typescript
makeChurchNumeral(n: number): ChurchNumeral
```

**Parameters:**
- `n`: Non-negative integer

**Returns:** Church numeral function

**Example:**
```typescript
const two = makeChurchNumeral(2);
const result = two((x) => x + 1)(0);  // 2
```

### `churchToInt(church)`

Convert Church numeral to integer.

```typescript
churchToInt(church: ChurchNumeral): number
```

**Parameters:**
- `church`: Church numeral function

**Returns:** Integer value

**Example:**
```typescript
const num = churchToInt(makeChurchNumeral(5));  // 5
```

## ProLog API

### `prologQuery(query)`

Execute ProLog query.

```typescript
prologQuery(query: string): Solution[]
```

**Parameters:**
- `query`: ProLog query string

**Returns:** Array of solutions

**Example:**
```typescript
const solutions = prologQuery('parent(alice, X)');
// [{ X: 'bob' }, { X: 'charlie' }]
```

### `prologAssert(clause)`

Add ProLog clause.

```typescript
prologAssert(clause: string): void
```

**Parameters:**
- `clause`: ProLog fact or rule

**Example:**
```typescript
prologAssert('parent(alice, bob)');
prologAssert('grandparent(X, Z) :- parent(X, Y), parent(Y, Z)');
```

### `prologRetract(clause)`

Remove ProLog clause.

```typescript
prologRetract(clause: string): boolean
```

**Parameters:**
- `clause`: Clause to retract

**Returns:** Success boolean

**Example:**
```typescript
prologRetract('parent(alice, bob)');
```

### `unify(term1, term2)`

Unify two ProLog terms.

```typescript
unify(term1: Term, term2: Term): Substitution | null
```

**Parameters:**
- `term1`: First term
- `term2`: Second term

**Returns:** Substitution if unifiable, null otherwise

**Example:**
```typescript
const sub = unify(
  parse('foo(X, 2)'),
  parse('foo(1, Y)')
);
// { X: 1, Y: 2 }
```

## DataLog API

### `datalogQuery(program)`

Execute DataLog program.

```typescript
datalogQuery(program: string): Relation[]
```

**Parameters:**
- `program`: DataLog program (facts, rules, query)

**Returns:** Array of relations

**Example:**
```typescript
const program = `
  edge(a, b).
  edge(b, c).
  path(X, Y) :- edge(X, Y).
  path(X, Y) :- edge(X, Z), path(Z, Y).
  ?- path(a, X).
`;

const results = datalogQuery(program);
// [{ X: 'b' }, { X: 'c' }]
```

### `datalogEvaluate(rules, facts)`

Evaluate DataLog rules over facts.

```typescript
datalogEvaluate(rules: Rule[], facts: Fact[]): Fact[]
```

**Parameters:**
- `rules`: Array of DataLog rules
- `facts`: Array of base facts

**Returns:** Derived facts (fixpoint)

**Example:**
```typescript
const rules = [
  parseRule('path(X, Y) :- edge(X, Y)'),
  parseRule('path(X, Y) :- edge(X, Z), path(Z, Y)')
];

const facts = [
  parseFact('edge(a, b)'),
  parseFact('edge(b, c)')
];

const derived = datalogEvaluate(rules, facts);
```

## RDF/SPARQL API

### `sparqlQuery(query, graph)`

Execute SPARQL query.

```typescript
sparqlQuery(query: string, graph: Triple[]): Binding[]
```

**Parameters:**
- `query`: SPARQL query string
- `graph`: RDF graph (array of triples)

**Returns:** Array of variable bindings

**Example:**
```typescript
const query = `
  SELECT ?name WHERE {
    ?person ex:name ?name .
    ?person ex:age ?age .
    FILTER(?age > 25)
  }
`;

const bindings = sparqlQuery(query, graph);
// [{ name: 'Alice' }, { name: 'Bob' }]
```

### `rdfTriple(subject, predicate, object)`

Create RDF triple.

```typescript
rdfTriple(subject: string, predicate: string, object: string): Triple
```

**Parameters:**
- `subject`: Subject URI
- `predicate`: Predicate URI
- `object`: Object URI or literal

**Returns:** Triple object

**Example:**
```typescript
const triple = rdfTriple('ex:Alice', 'ex:knows', 'ex:Bob');
```

### `parseTriples(turtle)`

Parse Turtle format to triples.

```typescript
parseTriples(turtle: string): Triple[]
```

**Parameters:**
- `turtle`: Turtle format RDF

**Returns:** Array of triples

**Example:**
```typescript
const triples = parseTriples(`
  ex:Alice ex:knows ex:Bob .
  ex:Alice ex:age 30 .
`);
```

## SHACL API

### `validateShape(data, shape)`

Validate RDF data against SHACL shape.

```typescript
validateShape(data: Triple[], shape: Shape): ValidationReport
```

**Parameters:**
- `data`: RDF triples to validate
- `shape`: SHACL shape definition

**Returns:** Validation report

**Example:**
```typescript
const shape = {
  targetClass: 'ex:Person',
  properties: [{
    path: 'ex:name',
    minCount: 1,
    datatype: 'xsd:string'
  }]
};

const report = validateShape(data, shape);
console.log(report.conforms);  // true or false
console.log(report.violations);  // Array of violations
```

### `parseShape(turtle)`

Parse SHACL shape from Turtle.

```typescript
parseShape(turtle: string): Shape
```

**Parameters:**
- `turtle`: Turtle format SHACL shape

**Returns:** Shape object

**Example:**
```typescript
const shape = parseShape(`
  ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [
      sh:path ex:name ;
      sh:minCount 1 ;
    ] .
`);
```

## Type Definitions

### Common Types

```typescript
// Blackboard
interface Fact {
  id?: string;
  type: string;
  [key: string]: any;
}

interface Pattern {
  [key: string]: any;  // '?' prefix for variables
}

// Agents
interface Query {
  type: string;
  data: any;
}

interface Response {
  status: 'success' | 'error';
  data?: any;
  error?: string;
}

interface AgentStatus {
  state: 'active' | 'idle' | 'error';
  queueSize: number;
  lastQuery?: Date;
}

// RDF
interface Triple {
  subject: string;
  predicate: string;
  object: string;
}

interface Binding {
  [variable: string]: string;
}

// SHACL
interface Shape {
  targetClass?: string;
  properties: PropertyConstraint[];
}

interface PropertyConstraint {
  path: string;
  minCount?: number;
  maxCount?: number;
  datatype?: string;
  pattern?: string;
}

interface ValidationReport {
  conforms: boolean;
  violations: Violation[];
}

interface Violation {
  focusNode: string;
  resultPath: string;
  resultMessage: string;
}
```

## Error Handling

All async APIs use Promises and should be wrapped in try-catch:

```typescript
try {
  const result = await blackboard.query(pattern);
  // Use result
} catch (error) {
  console.error('Query failed:', error);
}
```

## Performance Tips

1. **Batch Operations**: Use batch APIs when available
2. **Index Patterns**: Query by indexed fields first
3. **Limit Results**: Use LIMIT in SPARQL queries
4. **Cache Results**: Cache frequently used queries
5. **Async Operations**: Use Promise.all for parallel operations

## See Also

- [[Getting Started]] - Installation and setup
- [[Examples]] - Code examples
- [[Architecture Overview]] - System architecture
- [[Developer Guide]] - Advanced topics

---

**Last Updated**: 2025-11-10
**Version**: 1.0.0
**Maintainer**: Computational Topology Canvas Team
