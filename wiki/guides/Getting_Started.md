---
id: guides-getting-started
title: "Getting Started: Your Journey Begins Here"
level: advanced
type: guide
tags: [church-encoding, lambda-calculus, prolog, datalog, semantic-web, shacl, multi-agent-system, blackboard-architecture, automaton]
keywords: [getting, started:, your, journey, begins, here, home, main, automaton, guides]
prerequisites: []
enables: []
related: []
readingTime: 22
difficulty: 4
blackboard:
  status: active
  assignedAgent: "0D-Topology-Agent"
  lastUpdate: "2025-01-07"
  dependencies: []
  watchers: []
---
# Getting Started: Your Journey Begins Here

**From Zero to Your First Query in 15 Minutes**

---

## 🎯 The Adventure Awaits

**Imagine this**: You've heard about a system where programming paradigms unite—where ProLog talks to Scheme, where RDF graphs integrate with logic rules, where agents coordinate through a shared blackboard, and where software can evolve itself.

**You're curious.** Maybe skeptical. Definitely intrigued.

**This guide is your map.** In the next 15 minutes, you'll go from "What is this?" to "I'm running queries!" We'll install CTC, meet your first agent, run your first query, and leave you ready to explore deeper.

> 💡 **Want the complete story?** See [[../meta/The_Story_of_CTC.md]] - A narrative journey from Church's lambda calculus to self-evolving software that makes complex concepts accessible through storytelling and analogies.

**Ready?** Let's begin.

---

## 📋 What You'll Accomplish

By the end of this guide, you'll have:

- ✅ CTC installed and running on your machine
- ✅ Your first SPARQL query executed
- ✅ Your first R5RS expression evaluated
- ✅ Your first agent queried
- ✅ A working knowledge base with validated data
- ✅ Understanding of the core concepts

**Time investment**: 15-30 minutes  
**Difficulty**: Beginner-friendly (we'll explain everything)  
**Prerequisites**: Basic command line and JavaScript knowledge

---

## 🌟 What Is CTC? (The 30-Second Version)

**Who built this?** Researchers and developers who were tired of paradigm silos.

**What does it do?** It's a **multilingual canvas** where:
- **R5RS Scheme** handles functional programming
- **ProLog** handles logic programming  
- **DataLog** handles querying
- **RDF/SPARQL** handles knowledge graphs
- **SHACL** handles validation
- **8 Dimensional Agents** (0D-7D) coordinate everything

**When would you use it?** When you need multiple paradigms working together seamlessly.

**Where does it run?** Anywhere Node.js runs—your laptop, a server, a Raspberry Pi.

**Why does it matter?** Because real problems don't fit into single paradigm boxes. CTC lets you use the right tool for each part of your problem.

**Think of it as**: A Rosetta Stone for programming languages, or a universal translator for computational paradigms.

---

## 🎒 What You'll Need

### System Requirements

**Who needs this?** Anyone with a computer running Node.js.

**What do you need?**
- **Node.js**: v18 or higher (like having a universal translator installed)
- **TypeScript**: v5 or higher (the language CTC speaks)
- **Memory**: 4GB RAM minimum, 8GB recommended (enough room for all the agents)
- **Disk Space**: 1GB (smaller than most games!)

**When to check**: Before you start installing (saves time later).

**Where to get it**: 
- Node.js: [nodejs.org](https://nodejs.org)
- TypeScript: Comes with npm install

**Why these versions?** They support all the modern features CTC uses.

### Knowledge Prerequisites

**Who can use this?** You don't need to be an expert! Here's what helps:

**Essential** (you need these):
- **JavaScript/TypeScript**: Basic programming (variables, functions, objects)
- **Command Line**: Basic navigation (`cd`, `ls`, running commands)

**Helpful** (nice to have, but we'll explain):
- **Functional Programming**: Lambda functions, higher-order functions
- **Logic Programming**: Basic ProLog or DataLog concepts
- **RDF/SPARQL**: Semantic web basics

**Why it's okay if you're new**: We'll explain everything as we go. CTC is designed to be accessible.

---

## 🚀 Installation: Your First Steps

### The Journey Begins

Think of installation like setting up a new workspace. You're not just installing software—you're preparing a laboratory where paradigms will collaborate.

**Who does this?** You, the explorer.

**What happens?** We'll clone the repository, install dependencies, build the project, and verify everything works.

**When does it take?** 5-10 minutes (depending on your internet speed).

**Where does it go?** A directory called `automaton` on your machine.

**Why follow these steps?** Each step builds on the previous one. Skip one, and things break.

### Step 1: Clone the Repository

**What you're doing**: Getting a copy of the CTC codebase.

**Why it matters**: Like downloading a book—you need the source material.

```bash
git clone <repository-url>
cd automaton
```

**What to expect**: A new directory appears with all the CTC code.

**Troubleshooting**: If `git` isn't found, install Git first: [git-scm.com](https://git-scm.com)

### Step 2: Install Dependencies

**What you're doing**: Installing all the libraries CTC needs to run.

**Why it matters**: Like stocking a kitchen before cooking—you need ingredients.

**The metaphor**: CTC is like an orchestra. Each dependency is an instrument. `npm install` brings all the musicians together.

```bash
npm install
```

**What to expect**: Lots of output as packages download. This takes 2-5 minutes.

**Troubleshooting**: 
- Slow? That's normal—there are many dependencies.
- Errors? Check your Node.js version: `node --version` (should be v18+)

### Step 3: Build the Project

**What you're doing**: Compiling TypeScript to JavaScript.

**Why it matters**: TypeScript is like a blueprint. Building creates the actual house.

**The metaphor**: Like compiling a book from source files into a readable PDF.

```bash
npm run build
```

**What to expect**: TypeScript compilation output. Should complete without errors.

**Troubleshooting**: Errors usually mean TypeScript issues. Check version: `tsc --version` (should be v5+)

### Step 4: Run Tests

**What you're doing**: Verifying everything works correctly.

**Why it matters**: Like test-driving a car before a road trip—you want to know it works.

**The metaphor**: CTC has 8 agents. Running tests is like checking that all agents are awake and responsive.

```bash
npm test
```

**What to expect**: Test results. All tests should pass (green checkmarks).

**Troubleshooting**: 
- Failing tests? Check error messages—they'll tell you what's wrong.
- Can't run tests? Make sure Step 3 (build) completed successfully.

### Step 5: Verify Installation

**What you're doing**: Confirming all agents are available.

**Why it matters**: Like checking that all orchestra members showed up for rehearsal.

**The metaphor**: CTC has 8 dimensional agents (0D through 7D). This command introduces you to them.

```bash
# Check that agents are available
npm run list-agents

# Expected output:
# Available agents:
#   - 0d-topology-agent (The Sage)
#   - 1d-temporal-agent (The Chronicler)
#   - 2d-structural-agent (The Architect)
#   - 3d-algebraic-agent (The Mathematician)
#   - 4d-network-agent (The Messenger)
#   - 5d-consensus-agent (The Diplomat)
#   - 6d-intelligence-agent (The Scholar)
#   - 7d-quantum-agent (The Dreamer)
```

**What to expect**: A list of all 8 agents with their personalities.

**Success indicator**: You see all 8 agents listed. If you do, **congratulations!** CTC is installed and ready.

---

## ⚡ Quick Start: Your First Queries

**Now the fun begins.** You've set up the laboratory. Time to run your first experiments.

**Who does this?** You, the experimenter.

**What you'll do**: Run four different types of queries to see CTC's power.

**When to do this**: Right after installation (while excitement is high!).

**Where**: In your terminal and code editor.

**Why start here?** Because seeing is believing. These examples show CTC's capabilities immediately.

### Example 1: Query the Knowledge Base (SPARQL)

**What you're doing**: Asking questions of a knowledge graph.

**Why SPARQL?** It's like SQL for knowledge graphs—powerful and expressive.

**The metaphor**: Imagine a library where books are connected by relationships. SPARQL lets you ask: "Show me all books connected to this author."

```bash
# Start the SPARQL endpoint (like starting a server)
npm run sparql-server

# In another terminal, run a query (like asking a question)
npm run query -- "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

**What to expect**: 
- First command: Server starts, shows "SPARQL endpoint running on port 3030"
- Second command: Returns up to 10 triples from the knowledge base

**Success indicator**: You see triples (subject-predicate-object statements) returned.

**What this teaches you**: CTC can store and query knowledge graphs.

### Example 2: Execute R5RS Code (Church Encoding)

**What you're doing**: Running functional code with Church encoding.

**Why Church encoding?** It shows how numbers can be made of pure functions—beautiful and foundational.

**The metaphor**: Like discovering that all music is vibrations. Church encoding shows that all numbers are functions.

**The story**: In 1936, Alonzo Church discovered you could represent numbers using only functions. CTC implements this, so you can see it in action.

```typescript
import { evaluateR5RS } from './src/r5rs/evaluator';

// Church numeral addition
// This is ZERO: do nothing
// This is ONE: do something once
// This is PLUS: combine two numbers
const code = `
  (define zero (lambda (f) (lambda (x) x)))
  (define one (lambda (f) (lambda (x) (f x))))
  (define plus (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))))

  (((plus one) one))  ; Returns church numeral for 2
`;

const result = evaluateR5RS(code);
console.log(result);  // You'll see the Church numeral for 2
```

**What to expect**: The result is a Church numeral representing 2.

**Success indicator**: No errors, and you see a function returned.

**What this teaches you**: CTC implements Church encoding—numbers as functions.

### Example 3: Run an Agent

**What you're doing**: Querying a dimensional agent directly.

**Why agents?** Each agent is a specialist. 0D (The Sage) handles topology. You're asking it to analyze a graph.

**The metaphor**: Like asking a specialist doctor for a diagnosis. Each agent is an expert in its dimension.

**The personality**: Meet **The Sage** (0D Topology Agent)—the wise elder who finds fixed points and analyzes connectivity.

```typescript
import { getAgent } from './src/agents';

// Get the 0D topology agent (The Sage)
const agent = getAgent('0d-topology-agent');

// Send a query: "Analyze this graph's topology"
const response = await agent.query({
  type: 'analyze-topology',
  data: { graph: myGraph }
});

console.log(response);  // The Sage's analysis
```

**What to expect**: The agent analyzes your graph and returns topology insights.

**Success indicator**: You get a response with topology information.

**What this teaches you**: Agents are accessible and powerful. Each has a personality and specialty.

### Example 4: Validate with SHACL

**What you're doing**: Ensuring data quality with shape validation.

**Why SHACL?** Like a bouncer at a club—it checks that data follows rules before entry.

**The metaphor**: SHACL shapes are like forms with required fields. Data must match the shape.

**The story**: You're defining what "good data" looks like, then checking if your data matches.

```typescript
import { validateShape } from './src/shacl/validator';

// Define a shape: "A Person must have exactly one name"
const shape = {
  targetClass: 'ex:Person',
  properties: [
    {
      path: 'ex:name',
      datatype: 'xsd:string',
      minCount: 1,  // At least one name
      maxCount: 1   // At most one name
    }
  ]
};

// Validate data: "Does Alice match the Person shape?"
const data = [
  { subject: 'ex:Alice', predicate: 'ex:name', object: 'Alice' }
];

const report = validateShape(data, shape);
console.log(report.conforms);  // true (Alice has exactly one name)
console.log(report.violations);  // [] (no violations)
```

**What to expect**: Validation report showing whether data conforms to the shape.

**Success indicator**: `conforms: true` and `violations: []`.

**What this teaches you**: CTC can validate data quality automatically.

---

## 🧠 Core Concepts: The Foundation

**Now that you've run queries, let's understand what's happening.**

**Who needs this?** You, if you want to go deeper.

**What you'll learn**: The five core concepts that make CTC work.

**When to read this**: After running the examples (context makes it clearer).

**Where these concepts live**: Throughout the CTC system.

**Why they matter**: Understanding these makes everything else click.

### 1. Dimensional Progression: The Climb from 0D to 7D

**What it is**: CTC organizes computation into 8 dimensions, each building on the previous.

**Why dimensions?** Like building a skyscraper—you start with foundation (0D), add floors (1D-3D), connect buildings (4D), coordinate cities (5D), add intelligence (6D), explore possibilities (7D).

**The story**: Each dimension represents a level of abstraction. 0D is the foundation (topology). 7D is the peak (quantum possibilities).

| Dimension | Focus | The Agent | What They Do |
|-----------|-------|-----------|--------------|
| **0D** | Topology, Fixed Points | The Sage | Finds what doesn't change |
| **1D** | Time, Sequences | The Chronicler | Tracks what happened when |
| **2D** | Structure, Patterns | The Architect | Sees how things fit together |
| **3D** | Algebra, Types | The Mathematician | Operates and transforms |
| **4D** | Networks, Distribution | The Messenger | Connects distant things |
| **5D** | Consensus, Agreement | The Diplomat | Helps many become one |
| **6D** | Intelligence, Learning | The Scholar | Learns from experience |
| **7D** | Quantum, Superposition | The Dreamer | Explores all possibilities |

**The metaphor**: Like a video game with 8 levels. Each level unlocks new capabilities.

**Why this matters**: Understanding dimensions helps you choose the right agent for your task.

### 2. Agent Architecture: Specialists Working Together

**What it is**: Each dimension has a dedicated agent—a specialist in that dimension's domain.

**Why agents?** Like an orchestra—each musician specializes, but together they create harmony.

**The metaphor**: CTC agents are like specialists in a hospital. The cardiologist (0D) handles topology. The neurologist (6D) handles intelligence. They consult each other through the blackboard.

**The interface**: Every agent follows the same pattern:

```typescript
interface Agent {
  dimension: string;  // e.g., "0D", "1D"
  capabilities: string[];  // What this agent can do
  query: (query: Query) => Promise<Response>;  // Ask it something
  update: (data: any) => Promise<void>;  // Give it new information
  status: () => AgentStatus;  // Check if it's healthy
}
```

**The story**: Agents don't work in isolation. They coordinate through the blackboard, sharing knowledge and collaborating.

**Why this matters**: You can query any agent, and they'll work together automatically.

### 3. Blackboard System: The Meeting Room of Minds

**What it is**: A shared knowledge base where agents read and write information.

**Why a blackboard?** Like a whiteboard in a meeting room—everyone can see it, write on it, and learn from it.

**The metaphor**: Imagine a town square with a giant bulletin board. Citizens (agents) post notices, read others' posts, and coordinate activities.

**How it works**: Agents communicate indirectly through the blackboard:

```
┌─────────────────────────────────────┐
│         Blackboard (JSONL)          │
│  ┌───────────────────────────────┐  │
│  │ Facts, Rules, Triples         │  │
│  │ (The shared knowledge)        │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
         ↑           ↑           ↑
         │           │           │
    ┌────┴───┐  ┌────┴───┐  ┌────┴───┐
    │ 0D     │  │ 1D     │  │ 2D     │
    │ Agent  │  │ Agent  │  │ Agent  │
    │(Sage)  │  │(Chron) │  │(Arch)  │
    └────────┘  └────────┘  └────────┘
```

**The story**: An agent writes a fact. Other agents read it. They add related facts. Knowledge grows organically.

**Why this matters**: The blackboard enables multi-agent coordination without direct communication.

### 4. Meta-Log Framework: Three Languages, One System

**What it is**: Three logic programming languages integrated seamlessly.

**Why three languages?** Each excels at different tasks:
- **R5RS**: Functional programming, Church encoding (the calculator)
- **ProLog**: Logic programming, unification (the logician)
- **DataLog**: Query language, bottom-up evaluation (the librarian)

**The metaphor**: Like having three tools in your toolbox. A hammer (R5RS) for computation. A screwdriver (ProLog) for logic. A wrench (DataLog) for queries.

**The story**: Each language has strengths. CTC lets you use all three together, choosing the right tool for each task.

**Why this matters**: You're not limited to one paradigm. Use the best tool for each job.

### 5. Knowledge Graphs: Relationships as Data

**What it is**: Data represented as subject-predicate-object triples, queryable with SPARQL.

**Why knowledge graphs?** Like a social network—nodes (subjects/objects) connected by edges (predicates).

**The metaphor**: Imagine a family tree, but for any kind of relationship. "Alice knows Bob." "Bob works at Company." "Company is in City."

**The story**: Traditional databases store tables. Knowledge graphs store relationships. CTC uses RDF/SPARQL to query these relationships.

**Example**:
```turtle
ex:Alice ex:knows ex:Bob .
ex:Alice ex:age 30 .
ex:Bob ex:worksAt ex:Company .
```

**Query**:
```sparql
SELECT ?friend WHERE {
  ex:Alice ex:knows ?friend .
}
```

**Why this matters**: Many real-world problems are about relationships. Knowledge graphs model this naturally.

---

## 📁 Project Structure: Where Everything Lives

**Understanding the codebase helps you navigate.**

**Who needs this?** Developers who want to explore or contribute.

**What you'll see**: The organization of CTC's code.

**Where things are**: Logical grouping by functionality.

**Why it matters**: Knowing where things are saves time.

```
automaton/
├── src/
│   ├── agents/          # The 8 dimensional agents
│   ├── blackboard/      # The shared knowledge base
│   ├── r5rs/           # R5RS evaluator (functional programming)
│   ├── prolog/         # ProLog engine (logic programming)
│   ├── datalog/        # DataLog engine (query language)
│   ├── rdf/            # RDF/SPARQL (knowledge graphs)
│   └── shacl/          # SHACL validator (data quality)
├── docs/               # Documentation (like this wiki)
├── grok_files/         # R5RS concept definitions
├── wiki/               # This wiki
└── tests/              # Test suites (examples of usage)
```

**The metaphor**: Like a library—fiction (agents), reference (blackboard), science (r5rs/prolog), history (docs).

**Why this structure?** Each directory has a clear purpose. Agents coordinate. Blackboard stores. Engines compute.

---

## ⚙️ Configuration: Making CTC Yours

**CTC is flexible. Configure it for your needs.**

**Who configures this?** You, when you want to customize CTC.

**What you can configure**: Database paths, enabled agents, logging, ports.

**When to configure**: After installation, before heavy usage.

**Where**: Environment variables and config files.

**Why configure?** Defaults work, but customization optimizes for your use case.

### Environment Variables

**What they do**: Control CTC's behavior without changing code.

**Why environment variables?** Like settings on a phone—change behavior without reinstalling.

**The metaphor**: Like adjusting a car's settings. Same car, different configuration.

Create a `.env` file:

```env
# Database: Where CTC stores its knowledge
DATABASE_PATH=./data/blackboard.jsonl

# Agents: Which agents are enabled (comma-separated)
ENABLE_AGENTS=0d,1d,2d,3d,4d,5d,6d,7d

# Logging: How verbose (debug, info, warn, error)
LOG_LEVEL=info

# SPARQL endpoint: Which port to use
SPARQL_PORT=3030
```

**The story**: CTC reads these at startup. Change them, restart, and behavior changes.

**Why this matters**: Different environments (development, production) need different configs.

### Agent Configuration

**What it does**: Fine-tune individual agent behavior.

**Why configure agents?** Like adjusting individual instruments in an orchestra.

**The metaphor**: Each agent has settings. Configure them for your workload.

Edit `config/agents.json`:

```json
{
  "agents": [
    {
      "id": "0d-topology-agent",
      "dimension": "0D",
      "enabled": true,
      "config": {
        "maxConcurrentQueries": 10  // How many queries at once
      }
    }
  ]
}
```

**The story**: You're telling each agent how to behave. "0D, handle up to 10 queries simultaneously."

**Why this matters**: Optimize agents for your specific use case.

---

## 🛠️ Common Tasks: Your Daily Toolkit

**These are the tasks you'll do most often.**

**Who does these?** You, as you build with CTC.

**What they are**: Patterns you'll use repeatedly.

**When to use them**: Whenever you need to add data, query, or validate.

**Where they're used**: In your applications built on CTC.

**Why they're here**: Save you from reinventing the wheel.

### Task 1: Add a New Fact

**What you're doing**: Adding a piece of knowledge to the blackboard.

**Why it matters**: The blackboard grows as you add facts. Agents can then use them.

**The metaphor**: Like posting a notice on the town square bulletin board.

```typescript
import { blackboard } from './src/blackboard';

// Add a fact: "Alice knows Bob"
blackboard.addFact({
  type: 'rdf-triple',
  subject: 'ex:Alice',
  predicate: 'ex:knows',
  object: 'ex:Bob'
});
```

**The story**: You're teaching CTC something new. "Alice knows Bob." Now agents can use this knowledge.

**When to use**: Whenever you have new information to add.

### Task 2: Query ProLog

**What you're doing**: Asking a logic question.

**Why ProLog?** It's great for rules and relationships. "Who are Alice's children?"

**The metaphor**: Like asking a detective to find relationships.

```typescript
import { prologQuery } from './src/prolog';

// Ask: "Who are Alice's children?"
const results = prologQuery('parent(alice, X)');
console.log(results);  // All X where alice is parent of X
```

**The story**: You're asking ProLog to find all solutions. It searches the knowledge base and returns matches.

**When to use**: When you need logical inference or rule-based queries.

### Task 3: Run DataLog Query

**What you're doing**: Querying with bottom-up evaluation.

**Why DataLog?** It computes all possible facts from rules. Great for transitive relationships.

**The metaphor**: Like asking "show me all ancestors" and getting the complete family tree.

```typescript
import { datalogQuery } from './src/datalog';

// Define rules: "Ancestors are parents or ancestors of parents"
const query = `
  ancestor(X, Y) :- parent(X, Y).
  ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
  ?- ancestor(alice, X).  // Find all of Alice's ancestors
`;

const results = datalogQuery(query);
```

**The story**: DataLog computes all ancestors recursively. You get the complete ancestor set.

**When to use**: When you need transitive closure or recursive queries.

### Task 4: Evaluate R5RS Expression

**What you're doing**: Running functional code.

**Why R5RS?** It's Scheme—simple, powerful, with Church encoding.

**The metaphor**: Like using a calculator that understands functions.

```typescript
import { evaluateR5RS } from './src/r5rs';

// Square a number using lambda
const result = evaluateR5RS('((lambda (x) (* x x)) 5)');
console.log(result);  // 25
```

**The story**: You're running Scheme code. R5RS evaluates it and returns the result.

**When to use**: When you need functional computation or Church encoding.

---

## 🔧 Troubleshooting: When Things Go Wrong

**Don't panic. Most issues have simple solutions.**

**Who encounters these?** Everyone, at some point.

**What they are**: Common problems and their fixes.

**When they happen**: During installation or usage.

**Where to look**: Error messages usually tell you what's wrong.

**Why this section exists**: Save you time debugging.

### Issue: "Agent not found"

**What it means**: CTC can't find the agent you're requesting.

**Why it happens**: Agent might be disabled or misconfigured.

**The solution**: Check that agent is enabled:

```bash
npm run list-agents
```

**What to look for**: Your agent should appear in the list. If not, check `config/agents.json`.

**The story**: Like calling someone who's not available. Check if they're enabled first.

### Issue: "Database locked"

**What it means**: Another process is using the database.

**Why it happens**: Multiple processes trying to write simultaneously.

**The solution**: Ensure only one process accesses the database:

```bash
# Kill any running processes
pkill -f "node.*blackboard"
```

**What to look for**: No other CTC processes running.

**The story**: Like two people trying to edit the same document. Only one can write at a time.

### Issue: "R5RS evaluation error"

**What it means**: Syntax error in your Scheme code.

**Why it happens**: Missing parentheses or incorrect syntax.

**The solution**: Check syntax carefully:

```scheme
; Wrong: missing parentheses around parameter
(lambda x (* x x))

; Correct: parameters in parentheses
(lambda (x) (* x x))
```

**What to look for**: Parentheses matching, correct syntax.

**The story**: Like a typo in English. Fix the syntax, and it works.

### Issue: "SHACL validation fails"

**What it means**: Your data doesn't match the shape definition.

**Why it happens**: Data types or counts don't match requirements.

**The solution**: Check shape definition and data types:

```typescript
// Ensure datatypes match exactly
const shape = {
  properties: [{
    path: 'ex:age',
    datatype: 'xsd:integer'  // Not 'number' or 'int'
  }]
};
```

**What to look for**: Datatype mismatches, count violations.

**The story**: Like filling out a form incorrectly. Fix the data to match the shape.

---

## 🎯 Next Steps: Your Journey Continues

**Congratulations!** You've installed CTC, run queries, and understood the core concepts.

**What you've accomplished**: 
- ✅ Installation complete
- ✅ First queries executed
- ✅ Core concepts understood
- ✅ Ready to build

**What's next?** Choose your path:

### For Hands-On Learners
1. **[[Quick Start Tutorial]]** - Build a complete example application
   - **Why**: See CTC in a real project
   - **Time**: 1-2 hours
   - **Reward**: Working application

### For Deep Divers
2. **[[Architecture Overview]]** - Deep dive into system architecture
   - **Why**: Understand how CTC works internally
   - **Time**: 2-3 hours
   - **Reward**: Deep understanding

### For API Users
3. **[[API Reference]]** - Detailed API documentation
   - **Why**: Reference for building applications
   - **Time**: Ongoing reference
   - **Reward**: Complete API knowledge

### For Advanced Developers
4. **[[Developer Guide]]** - Advanced development topics
   - **Why**: Build complex applications
   - **Time**: Several hours
   - **Reward**: Advanced skills

### For Explorers
5. **[[Examples]]** - More example code and use cases
   - **Why**: See what's possible
   - **Time**: Browse as needed
   - **Reward**: Inspiration

---

## 💬 Getting Help

**Stuck? We're here to help.**

**Who can help?** The community, documentation, examples.

**What resources exist?**
- **Documentation**: This wiki (you're reading it!)
- **Examples**: See `examples/` directory for working code
- **Tests**: Review test files—they're examples too
- **Issues**: Report bugs, request features

**When to ask**: After checking documentation and examples.

**Where to find help**: 
- GitHub issues for bugs
- Documentation for how-tos
- Examples for patterns

**Why ask?** CTC is a community project. Your questions help everyone.

---

## 🤝 Contributing

**Want to make CTC better? We welcome contributions!**

**Who can contribute?** Anyone! Code, docs, examples, feedback.

**What can you contribute?**
- **Code**: Fix bugs, add features
- **Documentation**: Improve guides (like this one!)
- **Examples**: Share use cases
- **Feedback**: Tell us what works and what doesn't

**When to contribute**: Anytime! CTC is always evolving.

**Where to start**: See these guides:
- **[[Contributing Guide]]** - How to contribute
- **[[Code Style]]** - Coding conventions
- **[[Testing Guide]]** - Writing tests

**Why contribute?** CTC grows through community contributions. Your improvements help everyone.

---

## 🎉 You Did It!

**You've completed the Getting Started guide.**

**What you've learned**:
- ✅ How to install CTC
- ✅ How to run your first queries
- ✅ How agents work
- ✅ How the blackboard coordinates
- ✅ How to troubleshoot issues

**What you can do now**:
- Run SPARQL queries
- Execute R5RS code
- Query agents
- Validate data
- Build applications

**Where to go from here**: Choose any of the Next Steps above, or explore the wiki.

**Remember**: CTC is a journey, not a destination. Keep exploring, keep learning, keep building.

**Welcome to the Computational Topology Canvas community!** 🚀

---

**Last Updated**: 2025-01-07  
**Version**: 2.0 (Humanized)  
**Maintainer**: Computational Topology Canvas Team
