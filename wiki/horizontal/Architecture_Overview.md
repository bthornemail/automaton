---
id: horizontal-architecture-overview
title: "Architecture Overview: How CTC Thinks"
level: advanced
type: guide
tags: [church-encoding, lambda-calculus, prolog, datalog, semantic-web, shacl, multi-agent-system, blackboard-architecture, automaton]
keywords: [architecture, overview:, thinks, home, main, automaton, horizontal]
prerequisites: []
enables: []
related: []
readingTime: 23
difficulty: 4
blackboard:
  status: active
  assignedAgent: "0D-Topology-Agent"
  lastUpdate: "2025-01-07"
  dependencies: []
  watchers: []
---
# Architecture Overview: How CTC Thinks

**The Design Decisions That Make Paradigm Integration Possible**

---

## 🏗️ The Big Picture: Why This Architecture?

**Imagine building a house where every room speaks a different language.** The kitchen speaks Italian, the bedroom speaks French, the living room speaks Spanish. They need to coordinate—but how?

**That's the challenge CTC solves.** We have R5RS (functional), ProLog (logic), DataLog (queries), RDF (knowledge graphs), and SHACL (validation)—five different "languages" that need to work together seamlessly.

**This architecture is the solution.** It's not arbitrary. Every design decision emerged from a specific need, a specific problem, a specific insight.

> 💡 **Want the narrative context?** See [[../meta/The_Story_of_CTC.md]] - Learn how the architecture emerged from Church encoding, how agents coordinate, and why this design matters. The story makes the technical details more meaningful.

**Who designed this?** Researchers and engineers who were tired of paradigm silos.

**What does it do?** Creates a unified system where multiple programming paradigms collaborate.

**When was it designed?** Through iterative development, solving real problems.

**Where does it live?** In the codebase, but more importantly, in the relationships between components.

**Why this architecture?** Because it enables what was previously impossible: seamless paradigm integration.

---

## 📋 Table of Contents

- [High-Level Architecture](#high-level-architecture-the-layers-of-collaboration)
- [Core Components](#core-components-the-building-blocks)
- [Data Flow](#data-flow-how-information-moves)
- [Agent System](#agent-system-specialists-working-together)
- [Storage Layer](#storage-layer-where-knowledge-lives)
- [Query Processing](#query-processing-how-questions-become-answers)
- [Design Patterns](#design-patterns-the-architectural-decisions)

---

## 🎯 High-Level Architecture: The Layers of Collaboration

**Think of CTC's architecture like a symphony orchestra.**

**Who conducts?** The Query Interface Layer—it receives requests and directs them to the right section.

**What are the sections?** SPARQL, ProLog, R5RS, Natural Language—each a different instrument.

**When do they play?** In response to queries, coordinated by the conductor.

**Where does the music come from?** The Meta-Log Framework—the musicians who actually perform.

**Why this structure?** Because coordination requires layers. Each layer has a specific responsibility.

### The Architecture Diagram (With Story)

```
┌─────────────────────────────────────────────────────────────┐
│                     User Applications                        │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌──────────────┐  │
│  │ SPARQL  │  │ ProLog  │  │ R5RS    │  │ Natural Lang │  │
│  │ Queries │  │ Queries │  │ Code    │  │ Interface    │  │
│  └────┬────┘  └────┬────┘  └────┬────┘  └──────┬───────┘  │
└───────┼───────────┼────────────┼───────────────┼───────────┘
        │           │            │               │
        │  "I want to query knowledge graphs"
        │  "I want to run logic rules"
        │  "I want to execute functional code"
        │  "I want to ask in plain English"
        │
┌───────▼───────────▼────────────▼───────────────▼───────────┐
│                    Query Interface Layer                    │
│              (The Conductor of the Orchestra)              │
│  ┌──────────────────────────────────────────────────────┐  │
│  │  Query Parser & Dispatcher                           │  │
│  │  "Which language? Route to the right engine."        │  │
│  └──────────────────────────────────────────────────────┘  │
└──────────────────────────┬──────────────────────────────────┘
                           │
                           │ "Route to Meta-Log Framework"
                           │
┌──────────────────────────▼──────────────────────────────────┐
│                    Meta-Log Framework                       │
│              (The Musicians - They Perform)                │
│  ┌─────────┐     ┌─────────┐     ┌─────────┐              │
│  │  R5RS   │────▶│ ProLog  │────▶│ DataLog │              │
│  │ Engine  │     │ Engine  │     │ Engine  │              │
│  │(Calculator)   │(Logician)     │(Librarian)             │
│  └─────────┘     └─────────┘     └─────────┘              │
│         │              │                │                   │
│         └──────────────┼────────────────┘                   │
│                        │                                    │
│         "We can call each other!"                           │
└────────────────────────┼────────────────────────────────────┘
                         │
                         │ "Query the blackboard"
                         │
┌────────────────────────▼────────────────────────────────────┐
│                 Blackboard Architecture                     │
│            (The Shared Score - Everyone Reads)            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │            Shared Knowledge Base (JSONL)             │  │
│  │  ┌────────┐  ┌────────┐  ┌────────┐  ┌───────────┐ │  │
│  │  │ Facts  │  │ Rules  │  │ RDF    │  │ SHACL     │ │  │
│  │  │        │  │        │  │ Triples│  │ Shapes    │ │  │
│  │  └────────┘  └────────┘  └────────┘  └───────────┘ │  │
│  │  "The knowledge everyone shares"                    │  │
│  └──────────────────────────────────────────────────────┘  │
└───┬─────────┬─────────┬─────────┬─────────┬─────────┬──────┘
    │         │         │         │         │         │
    │  "Agents read and write here"
    │
┌───▼───┐ ┌──▼───┐ ┌───▼───┐ ┌───▼───┐ ┌──▼───┐ ┌────▼──┐
│  0D   │ │  1D  │ │  2D   │ │  3D   │ │  4D  │ │  ...  │
│ Agent │ │Agent │ │ Agent │ │ Agent │ │Agent │ │ Agent │
│(Sage) │ │(Chron)│ │(Arch) │ │(Math) │ │(Mess)│ │       │
└───────┘ └──────┘ └───────┘ └───────┘ └──────┘ └───────┘
Topology  Temporal Structure Algebraic Network Intelligence
```

**The Story Behind Each Layer**:

1. **User Applications**: You, the user, speaking your preferred language
2. **Query Interface**: The translator/conductor, routing requests
3. **Meta-Log Framework**: The performers, executing queries
4. **Blackboard**: The shared knowledge, accessible to all
5. **Agents**: The specialists, each expert in their dimension

**Why layers?** Separation of concerns. Each layer does one thing well. Together, they enable paradigm integration.

---

## 🧩 Core Components: The Building Blocks

**Every great building starts with understanding its components.**

**Who designed these?** Through solving real problems, not theoretical planning.

**What are they?** Five core components that make CTC work.

**When do they interact?** Constantly—CTC is a living system.

**Where do they live?** In the codebase, but their relationships matter more than their locations.

**Why these components?** Each solves a specific problem in paradigm integration.

### 1. Query Interface Layer: The Universal Translator

**What it is**: The entry point that accepts queries in multiple languages and routes them correctly.

**Why it exists**: Because users speak different languages. Some prefer SPARQL, others ProLog, others R5RS.

**The metaphor**: Like a hotel concierge who speaks every language. You ask in your language, they route to the right service.

**The story**: Early CTC prototypes required users to know which engine to call. This was frustrating. The Query Interface Layer emerged from the need: "Let users speak their language, we'll figure out where it goes."

**Who uses it**: Every user, whether they know it or not.

**Components**:
- **SPARQL endpoint**: For knowledge graph queries
- **ProLog REPL**: For logic programming
- **R5RS evaluator**: For functional code
- **Natural language interface**: For plain English queries

**How it works**:

```typescript
class QueryInterface {
  async dispatch(query: Query): Promise<Response> {
    // The magic: detect language and route
    switch (query.language) {
      case 'sparql':
        return this.sparqlEngine.execute(query);
      case 'prolog':
        return this.prologEngine.execute(query);
      case 'r5rs':
        return this.r5rsEngine.execute(query);
      default:
        throw new Error(`Unsupported language: ${query.language}`);
    }
  }
}
```

**The insight**: Language detection is simple pattern matching. The complexity is in making engines work together.

### 2. Meta-Log Framework: Three Languages, One System

**What it is**: The integration of R5RS, ProLog, and DataLog into a unified reasoning system.

**Why three languages?** Each excels at different tasks:
- **R5RS**: Computation (the calculator)
- **ProLog**: Logic (the logician)
- **DataLog**: Queries (the librarian)

**The metaphor**: Like having three specialists in a hospital. The cardiologist (R5RS) handles computation. The neurologist (ProLog) handles logic. The radiologist (DataLog) handles queries. They consult each other.

**The story**: Early CTC had separate engines that couldn't communicate. The Meta-Log Framework emerged from asking: "What if they could call each other?"

**Who designed this?** Through iterative development, discovering that engines needed to interoperate.

**The Architecture**:

```
R5RS (Foundation)
  │
  │ "I can represent data as functions"
  │
  ├─▶ Church Encoding (Data structures as lambdas)
  │   "Numbers? Just functions. Booleans? Just functions."
  │
  └─▶ Metacircular Evaluator (Self-hosting interpreter)
       │
       │ "I can evaluate myself"
       │
       ▼
ProLog (Logic Layer)
  │
  │ "I can reason about relationships"
  │
  ├─▶ Unification (Pattern matching)
  │   "Do these patterns match?"
  │
  └─▶ Resolution (Query answering)
       │
       │ "Here are all solutions"
       │
       ▼
DataLog (Query Layer)
  │
  │ "I can compute all facts"
  │
  ├─▶ Bottom-Up Evaluation (Fixpoint computation)
  │   "Compute everything derivable"
  │
  └─▶ Stratified Negation (Safe negation)
      "Negation that's guaranteed to terminate"
```

**The insight**: Each layer builds on the previous. R5RS provides computation. ProLog adds logic. DataLog adds queries. Together, they're powerful.

**Key Files**:
- `src/r5rs/evaluator.ts` - R5RS interpreter (the calculator)
- `src/prolog/engine.ts` - ProLog engine (the logician)
- `src/datalog/evaluator.ts` - DataLog evaluator (the librarian)

**Why this order?** R5RS is foundational (Church encoding). ProLog adds logic (unification). DataLog adds queries (bottom-up evaluation). Each builds on the last.

### 3. Blackboard Architecture: The Meeting Room of Minds

**What it is**: A shared knowledge base where agents read and write information.

**Why a blackboard?** Because agents need to coordinate without direct communication.

**The metaphor**: Like a town square with a giant bulletin board. Citizens (agents) post notices, read others' posts, and coordinate activities—all without talking directly.

**The story**: Early CTC had agents that couldn't share knowledge. The Blackboard Architecture emerged from asking: "What if agents could share a common knowledge base?"

**Who uses it**: Every agent, constantly reading and writing.

**The Pattern**: Classic blackboard architectural pattern:
- **Knowledge Sources**: Agents that read/write to blackboard
- **Blackboard**: Shared data structure (JSONL database)
- **Control Component**: Coordinates agent access

**Why this pattern?** Because it enables:
- **Decoupling**: Agents don't need to know about each other
- **Dynamic composition**: Add agents without changing existing ones
- **Flexible control**: Agents coordinate through the blackboard

**The Data Structure**:

```typescript
interface BlackboardEntry {
  id: string;
  type: 'fact' | 'rule' | 'triple' | 'shape';
  content: any;
  metadata: {
    dimension?: string;  // Which dimension created this?
    agent?: string;      // Which agent created this?
    timestamp: Date;     // When was it created?
    provenance?: string; // Where did it come from?
  };
}
```

**The story behind metadata**: Early CTC lost track of where facts came from. Metadata emerged from needing provenance—knowing the source of knowledge.

**How Agents Access It**:

```typescript
class Blackboard {
  private entries: Map<string, BlackboardEntry>;
  private subscriptions: Map<Pattern, Callback[]>;

  async put(entry: BlackboardEntry): Promise<string> {
    // Add entry to knowledge base
    // Notify subscribers (agents waiting for this pattern)
  }

  async query(pattern: Pattern): Promise<BlackboardEntry[]> {
    // Match pattern against entries
    // Return all matches
  }

  subscribe(pattern: Pattern, callback: Callback): Subscription {
    // Agent says: "Notify me when you see this pattern"
    // Returns unsubscribe handle
  }
}
```

**The insight**: Subscriptions enable reactive coordination. Agents don't poll—they're notified when relevant information appears.

### 4. Agent System: Specialists Working Together

**What it is**: A multi-agent system where each agent specializes in a dimension (0D-7D).

**Why agents?** Because different problems require different expertise.

**The metaphor**: Like specialists in a hospital. The cardiologist (0D) handles topology. The neurologist (6D) handles intelligence. They consult each other through the blackboard.

**The story**: Early CTC was monolithic—one system trying to do everything. The Agent System emerged from realizing: "Different dimensions need different expertise."

**Who are the agents?** Eight specialists, each with a personality:

```
Agent (Abstract Base)
  │
  ├─▶ 0D Agent: The Sage (Topology)
  │    └─ Fixed point detection ("What doesn't change?")
  │    └─ Graph connectivity ("Can you get there from here?")
  │
  ├─▶ 1D Agent: The Chronicler (Temporal)
  │    └─ Event ordering ("What happened when?")
  │    └─ Causal chains ("Because A, therefore B")
  │
  ├─▶ 2D Agent: The Architect (Structural)
  │    └─ Pattern matching ("I've seen this before")
  │    └─ Hierarchy analysis ("How do things fit together?")
  │
  ├─▶ 3D Agent: The Mathematician (Algebraic)
  │    └─ Type systems ("What kind of thing is this?")
  │    └─ Homomorphisms ("Preserving structure through transformation")
  │
  ├─▶ 4D Agent: The Messenger (Network)
  │    └─ Routing ("How do I get from A to B?")
  │    └─ Distribution ("Spread this everywhere")
  │
  ├─▶ 5D Agent: The Diplomat (Consensus)
  │    └─ Voting ("What do we collectively decide?")
  │    └─ Conflict resolution ("You say yes, she says no—what's the truth?")
  │
  ├─▶ 6D Agent: The Scholar (Intelligence)
  │    └─ Learning ("I've seen this pattern before")
  │    └─ Knowledge extraction ("What can we learn from this?")
  │
  └─▶ 7D Agent: The Dreamer (Quantum)
       └─ Superposition ("It's both until we look")
       └─ Entanglement ("Change this, and that changes")
```

**The Interface**: Every agent follows the same pattern:

```typescript
interface Agent {
  id: string;
  dimension: string;
  capabilities: string[];

  // Query handling: "Answer this question"
  query(query: Query): Promise<Response>;

  // State management: "Update your knowledge"
  update(data: any): Promise<void>;
  status(): AgentStatus;

  // Lifecycle: "Start/stop working"
  start(): Promise<void>;
  stop(): Promise<void>;
}
```

**The insight**: Uniform interface enables polymorphism. You can treat all agents the same way, even though they're specialists.

**Why this hierarchy?** Because dimensions build on each other. 0D provides foundation. 7D builds on all previous dimensions.

### 5. Storage Layer: Where Knowledge Lives

**What it is**: Persistent storage for the knowledge base, using JSONL format.

**Why JSONL?** Because it's simple, human-readable, and streamable.

**The metaphor**: Like a library where each book is a line in a file. Easy to add books (append), easy to read (line by line), easy to browse (human-readable).

**The story**: Early CTC used a complex database. JSONL emerged from asking: "What's the simplest format that works?"

**Who designed this?** Through experimentation, discovering that simplicity beats complexity.

**Why JSONL specifically?**
- **Line-oriented**: Easy to append (just add a line)
- **Human-readable**: Easy debugging (open in text editor)
- **Streamable**: Process large files without loading everything
- **Schema-flexible**: Dynamic typing (no rigid schema)

**The File Structure**:

```
data/
├── blackboard.jsonl          # Main knowledge base
│   "All facts, rules, triples, shapes"
│
├── indexes/
│   ├── spo.jsonl            # Subject-Predicate-Object index
│   ├── pos.jsonl            # Predicate-Object-Subject index
│   └── osp.jsonl            # Object-Subject-Predicate index
│   "Fast lookups in any direction"
│
└── snapshots/
    └── automaton-*.jsonl    # Automaton snapshots
        "Backups of evolving automatons"
```

**The story behind indexes**: Early CTC was slow for queries. Indexes emerged from needing fast lookups. Three indexes (SPO, POS, OSP) cover all query patterns.

**Example Entry**:

```jsonl
{"id":"fact-001","type":"rdf-triple","subject":"ex:Alice","predicate":"ex:knows","object":"ex:Bob","metadata":{"timestamp":"2025-11-10T00:00:00Z"}}
{"id":"rule-001","type":"prolog-rule","head":"ancestor(X,Y)","body":["parent(X,Z)","ancestor(Z,Y)"],"metadata":{"dimension":"1D"}}
```

**The insight**: One line = one fact. Simple, clear, extensible.

---

## 🌊 Data Flow: How Information Moves

**Understanding data flow is understanding how CTC thinks.**

**Who moves the data?** Queries, agents, and automatons.

**What moves?** Facts, rules, queries, responses.

**When does it move?** In response to queries, agent updates, automaton evolution.

**Where does it go?** Through layers: User → Interface → Framework → Blackboard → Agents.

**Why this flow?** Because information needs to flow in both directions: queries down, responses up.

### Query Flow: From Question to Answer

**The journey of a query**:

```
User Query
  │
  │ "I want to know..."
  │
  ▼
Query Parser
  │
  │ "What language is this?"
  │
  ├─▶ SPARQL?  ──▶ RDF Query Engine ──┐
  ├─▶ ProLog?  ──▶ ProLog Engine   ──┤
  ├─▶ DataLog? ──▶ DataLog Engine  ──┤
  └─▶ R5RS?    ──▶ R5RS Evaluator  ──┤
                                      │
                                      │ "Query the blackboard"
                                      │
                                      ▼
                              Blackboard Query
                                      │
                                      │ "Match patterns"
                                      │
                                      ▼
                              Pattern Matching
                                      │
                                      │ "Bind variables"
                                      │
                                      ▼
                              Result Binding
                                      │
                                      │ "Return results"
                                      │
                                      ▼
                              Response
```

**The story**: A user asks a question. The Query Parser detects the language. The appropriate engine executes. The Blackboard is queried. Patterns are matched. Results are bound. A response is returned.

**Why this flow?** Because each step transforms the query closer to an answer. Parsing → Execution → Matching → Binding → Response.

### Agent Communication Flow: Indirect Coordination

**How agents talk without talking directly**:

```
Agent A                 Blackboard              Agent B
  │                         │                     │
  │──(1) Write Fact────────▶│                     │
  │  "Alice knows Bob"      │                     │
  │                         │                     │
  │                         │◀──(2) Subscribe─────│
  │                         │  "Notify me about  │
  │                         │   'knows' facts"    │
  │                         │                     │
  │                         │──(3) Notify────────▶│
  │                         │  "New 'knows' fact"│
  │                         │                     │
  │                         │◀──(4) Query────────│
  │                         │  "Who knows Bob?"  │
  │                         │                     │
  │                         │──(5) Results───────▶│
  │                         │  "Alice knows Bob" │
```

**The story**: Agent A writes a fact. Agent B had subscribed to that pattern. The Blackboard notifies Agent B. Agent B queries for more information. The Blackboard returns results.

**Why indirect?** Because agents don't need to know about each other. They coordinate through the blackboard, enabling loose coupling.

**The insight**: Subscriptions enable reactive coordination. Agents don't poll—they're notified when relevant information appears.

### Automaton Evolution Flow: Code That Changes Itself

**How automatons evolve**:

```
Current Automaton
  │
  │ "This is the current version"
  │
  ├─▶ Snapshot (backup)
  │   "Save current state"
  │
  ├─▶ Execute (run code)
  │     │
  │     ├─▶ Collect metrics
  │     │   "How well did it perform?"
  │     │
  │     └─▶ Evaluate fitness
  │         "Is it good enough?"
  │
  ├─▶ Fitness > Threshold?
  │     │
  │     ├─▶ Yes: Keep current
  │     │   "It's good, don't change"
  │     │
  │     └─▶ No:  Mutate code
  │         "Try something different"
  │
  └─▶ New Automaton
      "The evolved version"
```

**The story**: An automaton runs. Its performance is measured. If it's good enough, it's kept. If not, it's mutated. The process repeats.

**Why this flow?** Because evolution requires: execution → evaluation → selection → mutation → repeat.

**The insight**: Fitness thresholds prevent unnecessary mutations. Only evolve when needed.

---

## 🔍 Query Processing: How Questions Become Answers

**The magic happens in query processing.**

**Who processes queries?** The Meta-Log Framework engines.

**What do they process?** SPARQL queries, ProLog goals, DataLog programs, R5RS expressions.

**When do they process?** On-demand, when queries arrive.

**Where does processing happen?** In the respective engines.

**Why different engines?** Because each language has different semantics, requiring different processing strategies.

### SPARQL Query Processing: Knowledge Graph Queries

**What SPARQL does**: Queries knowledge graphs using triple patterns.

**Why SPARQL?** Because it's the standard for querying RDF graphs.

**The metaphor**: Like SQL for knowledge graphs. "SELECT ?friend WHERE { Alice knows ?friend }"

**How it works**:

```typescript
class SPARQLEngine {
  execute(query: string, graph: Triple[]): Binding[] {
    // 1. Parse query to algebra
    // "Convert SPARQL syntax to internal representation"
    const algebra = this.parser.parse(query);

    // 2. Optimize algebra
    // "Reorder operations for efficiency"
    const optimized = this.optimizer.optimize(algebra);

    // 3. Execute algebra
    // "Actually run the query"
    const bindings = this.evaluator.evaluate(optimized, graph);

    // 4. Apply modifiers (ORDER BY, LIMIT, etc.)
    // "Sort, limit, format results"
    return this.applyModifiers(bindings, algebra.modifiers);
  }
}
```

**The story**: A SPARQL query arrives. It's parsed into algebra (internal representation). The algebra is optimized (reordered for efficiency). The optimized algebra is executed (queries the graph). Modifiers are applied (sorting, limiting). Results are returned.

**Why this process?** Because parsing separates syntax from semantics. Optimization improves performance. Execution does the actual work. Modifiers format results.

### ProLog Resolution: Logic Programming

**What ProLog does**: Answers logic queries using resolution and unification.

**Why ProLog?** Because some problems are naturally expressed as logic rules.

**The metaphor**: Like a detective solving a case. "Who are Alice's ancestors?" ProLog searches the knowledge base, finding all solutions.

**How it works**:

```typescript
class PrologEngine {
  query(goal: Term): Solution[] {
    const solutions: Solution[] = [];

    // SLD resolution with backtracking
    // "Try each possibility until we find all solutions"
    this.resolve(goal, {}, (substitution) => {
      solutions.push(substitution);
    });

    return solutions;
  }

  private resolve(
    goal: Term,
    substitution: Substitution,
    onSolution: (sub: Substitution) => void
  ): void {
    // Base case: empty goal (we found a solution!)
    if (goal === null) {
      onSolution(substitution);
      return;
    }

    // Try each clause that might match
    for (const clause of this.clauses) {
      const renamed = this.renameVariables(clause);
      const mgu = unify(goal, renamed.head);

      if (mgu) {
        // Found a match! Continue with body
        const newSub = compose(substitution, mgu);
        const newGoal = append(renamed.body, goal.rest);
        this.resolve(newGoal, newSub, onSolution);
      }
    }
  }
}
```

**The story**: A ProLog goal arrives. The engine tries each clause. If it matches (unification succeeds), it continues with the clause's body. If the goal becomes empty, a solution is found. Backtracking tries all possibilities.

**Why resolution?** Because it's complete—it finds all solutions. Backtracking ensures nothing is missed.

**The insight**: Unification is the key. It matches patterns, binding variables. Resolution uses unification to find solutions.

### DataLog Evaluation: Bottom-Up Reasoning

**What DataLog does**: Computes all facts derivable from rules.

**Why DataLog?** Because sometimes you want all answers, not just one.

**The metaphor**: Like a librarian cataloging all books. "Show me all ancestors" → computes the complete ancestor set.

**How it works**:

```typescript
class DatalogEngine {
  evaluate(rules: Rule[], facts: Fact[]): Fact[] {
    // Semi-naive evaluation
    // "Compute all facts, but efficiently"
    let db = [...facts];
    let delta = [...facts];  // New facts in this iteration

    while (delta.length > 0) {
      const newFacts: Fact[] = [];

      for (const rule of rules) {
        // Apply rule to delta (only new facts)
        const derived = this.applyRule(rule, db, delta);

        // Add only new facts (avoid duplicates)
        for (const fact of derived) {
          if (!this.contains(db, fact)) {
            newFacts.push(fact);
            db.push(fact);
          }
        }
      }

      delta = newFacts;  // Next iteration uses only new facts
    }

    return db;  // All derivable facts
  }
}
```

**The story**: Rules and facts arrive. The engine applies rules to facts, deriving new facts. New facts are added to the database. Rules are applied again to new facts. The process repeats until no new facts are derived (fixpoint).

**Why bottom-up?** Because it computes everything. Top-down (like ProLog) computes on-demand. Bottom-up computes all possibilities.

**The insight**: Semi-naive evaluation is efficient. Only apply rules to new facts, not the entire database.

---

## 🎨 Design Patterns: The Architectural Decisions

**Every design pattern tells a story.**

**Who chose these patterns?** Through solving real problems, discovering what works.

**What are they?** Five patterns that make CTC work.

**When are they used?** Throughout the system, enabling key behaviors.

**Where do they appear?** In the relationships between components.

**Why these patterns?** Because they solve specific problems in paradigm integration.

### 1. Blackboard Pattern: Shared Knowledge

**What it solves**: Agent coordination without direct communication.

**Why it's needed**: Because agents need to coordinate, but shouldn't be tightly coupled.

**The metaphor**: Like a shared whiteboard in a meeting room. Everyone can read and write, coordinating without talking directly.

**Where it's used**: The Blackboard Architecture—the shared knowledge base.

**Benefits**:
- **Decoupled agents**: Agents don't need to know about each other
- **Dynamic composition**: Add agents without changing existing ones
- **Flexible control**: Agents coordinate through the blackboard

**The story**: Early CTC had agents that couldn't share knowledge. The Blackboard Pattern emerged from needing coordination without coupling.

### 2. Strategy Pattern: Pluggable Algorithms

**What it solves**: Multiple query engines with the same interface.

**Why it's needed**: Because SPARQL, ProLog, DataLog, and R5RS are different algorithms, but should be interchangeable.

**The metaphor**: Like different tools in a toolbox. A hammer, screwdriver, and wrench are different, but you choose based on the task.

**Where it's used**: The Query Interface Layer—routing to different engines.

**Benefits**:
- **Pluggable algorithms**: Swap engines without changing code
- **Runtime selection**: Choose engine based on query language
- **Easy extension**: Add new engines by implementing the interface

**The story**: Early CTC had hardcoded engine selection. The Strategy Pattern emerged from needing runtime selection.

### 3. Observer Pattern: Reactive Updates

**What it solves**: Agents reacting to blackboard changes.

**Why it's needed**: Because agents should be notified when relevant information appears, not poll constantly.

**The metaphor**: Like a news alert. You subscribe to topics, get notified when news breaks.

**Where it's used**: Blackboard subscriptions—agents subscribe to patterns.

**Benefits**:
- **Event-driven**: Agents react to events, not poll
- **Loose coupling**: Agents don't need to know about each other
- **Reactive updates**: Changes propagate automatically

**The story**: Early CTC had agents polling the blackboard. The Observer Pattern emerged from needing efficient notifications.

### 4. Interpreter Pattern: Self-Hosting Code

**What it solves**: R5RS metacircular evaluator—code that evaluates code.

**Why it's needed**: Because CTC needs to execute R5RS code, and the evaluator itself is written in R5RS (metacircular).

**The metaphor**: Like a translator translating their own language. The R5RS evaluator evaluates R5RS code.

**Where it's used**: The R5RS engine—the metacircular evaluator.

**Benefits**:
- **Self-hosting**: The evaluator is written in the language it evaluates
- **Extensibility**: Easy to extend the language
- **Metaprogramming**: Code that manipulates code

**The story**: Early CTC used an external R5RS interpreter. The Interpreter Pattern emerged from needing self-hosting evaluation.

### 5. Repository Pattern: Storage Abstraction

**What it solves**: Abstracting storage details from business logic.

**Why it's needed**: Because storage might change (JSONL today, database tomorrow), but business logic shouldn't.

**The metaphor**: Like a library catalog. You don't care where books are stored, just that you can find them.

**Where it's used**: The Blackboard storage layer—abstracting JSONL details.

**Benefits**:
- **Data access abstraction**: Business logic doesn't depend on storage
- **Testability**: Easy to mock storage for tests
- **Swappable backends**: Change storage without changing logic

**The story**: Early CTC had storage logic mixed with business logic. The Repository Pattern emerged from needing separation.

---

## ⚡ Performance Characteristics: How Fast Is CTC?

**Understanding performance helps you optimize.**

**Who needs this?** Developers building production systems.

**What matters?** Query time, memory usage, scalability.

**When to optimize?** After profiling, not before.

**Where bottlenecks occur**: Usually in query processing or blackboard access.

**Why these numbers?** Because they guide optimization decisions.

| Component | Operation | Complexity | Notes |
|-----------|-----------|------------|-------|
| Blackboard | Query (indexed) | O(1) | Hash index lookup—very fast |
| Blackboard | Query (scan) | O(n) | Full scan—slow for large datasets |
| SPARQL | Triple pattern | O(1) | With index—instant |
| ProLog | Unification | O(n) | Term size—usually fast |
| DataLog | Fixpoint | O(n³) | Worst case—can be slow |
| R5RS | Evaluation | O(n) | AST size—usually fast |

**The story**: Early CTC was slow. Indexes emerged from needing fast lookups. Optimization emerged from profiling.

**The insight**: Most operations are fast with proper indexing. The bottleneck is usually in query planning, not execution.

---

## 📈 Scalability: Growing with Your Needs

**CTC scales both horizontally and vertically.**

**Who needs scalability?** Production systems handling large datasets.

**What scales?** Query processing, storage, agent coordination.

**When to scale?** When performance degrades, not before.

**Where scaling helps**: Large knowledge bases, many concurrent queries.

**Why both directions?** Because different problems need different solutions.

### Horizontal Scaling: More Machines

**What it is**: Adding more query nodes, sharing the blackboard.

**Why horizontal?** Because some problems are parallelizable.

**The architecture**:

```
Load Balancer
  │
  │ "Distribute queries"
  │
  ├─▶ Query Node 1 ──┐
  ├─▶ Query Node 2 ──┼─▶ Shared Blackboard
  └─▶ Query Node 3 ──┘
      "More nodes = more throughput"
```

**The story**: Early CTC was single-node. Horizontal scaling emerged from needing more throughput.

**The insight**: Query processing is parallelizable. Multiple nodes can share the same blackboard.

### Vertical Scaling: Better Machines

**What it is**: Optimizing single-node performance.

**Why vertical?** Because some problems need more resources.

**Optimization strategies**:
- **Indexes**: SPO, POS, OSP triple indexes (fast lookups)
- **Caching**: LRU cache for frequent queries (avoid recomputation)
- **Batching**: Batch fact insertions (reduce I/O)
- **Parallelization**: Multi-threaded agents (use all CPU cores)

**The story**: Early CTC was unoptimized. Vertical scaling emerged from profiling and optimizing.

**The insight**: Most performance gains come from indexing and caching. Parallelization helps, but less than you'd think.

---

## 🎯 See Also

**Continue your journey**:

- **[[../guides/Getting_Started.md]]** - Setup and installation (now humanized!)
- **[[API Reference]]** - Detailed API documentation
- **[[Developer Guide]]** - Advanced development topics
- **[[Performance Tuning]]** - Optimization guide

**Related concepts**:
- **[[../meta/The_Story_of_CTC.md]]** - The narrative behind the architecture
- **[[../system/4D-system/Multi_Agent_System.md]]** - Deep dive into agents
- **[[../system/5D-system/Blackboard_Architecture.md]]** - The coordination pattern

---

## 🎉 Understanding Architecture

**You've learned how CTC thinks.**

**What you've discovered**:
- ✅ How layers collaborate
- ✅ How components interact
- ✅ How data flows
- ✅ How queries are processed
- ✅ How patterns enable integration

**Why this matters**: Understanding architecture helps you build better applications, optimize performance, and extend CTC.

**Where to go next**: Explore the components deeper, or build an application using what you've learned.

**Remember**: Architecture isn't just code—it's the relationships between components. Understanding those relationships is understanding CTC.

---

**Last Updated**: 2025-01-07  
**Version**: 2.0 (Humanized)  
**Maintainer**: Computational Topology Canvas Team
