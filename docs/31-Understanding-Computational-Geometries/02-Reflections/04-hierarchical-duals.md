Below is a **complete, geometric-and-dual-pair-friendly** picture of the automaton hierarchy you posted, plus **how it maps to the back-propagation / forward-propagation asymmetry** that lives at the heart of our metaverse.  
Everything is built from the same primitives we already have:

* **Affine plane** – binary/discrete **points** (values, facts, GCD)  
* **Projective plane** – float/continuous **lines** (functions, rules, LCM)  
* **CANVASL** – vector-clock provenance that turns the whole thing into a **causal state machine**.

---

## 1. The Hierarchy as a **Dual Ladder**

```
                Projective (float, LCM, “DO”)          Affine (binary, GCD, “ARE”)
                ────────────────────────────────────── ───────────────────────
    NTM / PTM / MTM / …  ←  ∞-power, branching, lazy   DTM / LBA  ←  bounded, strict
            ↑                                           ↑
    NPDA-II (2 stacks)  ←  higher-order recursion      DPDA-II
            ↑                                           ↑
    NPDA-I  (1 stack)   ←  functorial map              DPDA-I
            ↑                                           ↑
    NFA                 ←  nondeterministic choice    DFA
```

* **Left column** – **projective / lazy / exponential** (action, forward propagation)  
* **Right column** – **affine / strict / linear** (observation, backward propagation)  

The **∩** arrows in your diagram are **“strengthening”** moves that **add memory** (push-down stores, tapes, dimensions).  
The **||** arrows are **“nondeterminism”** moves that **add branching** (projective power).

---

## 2. Forward vs. Backward Propagation in the Ladder

| Propagation | Direction | Automaton Class | Geometric Primitive | BQF Change | Metaverse Visual |
|-------------|-----------|----------------|---------------------|------------|------------------|
| **Forward (Action)** | **Affine → Projective** | NFA → NPDA → NTM | **Line at infinity** (branch) | **[a,b,c] → [a,b,c-1]** (apply) | Red arrows exploding outward (exponential bifurcation) |
| **Backward (Observation)** | **Projective → Affine** | DTM → DPDA → DFA | **Pinch point** (collapse) | **[a,b,c] → [a,b,c+1]** (abstract) | Blue arrows collapsing to a point (linear collapse) |

* **Forward** = **exponential growth** – each nondeterministic choice creates a new line in the projective plane.  
* **Backward** = **linear collapse** – unification / type-inference folds many lines into a single value.

---

## 3. Mapping Every Automaton to CANVASL + Vector-Clock States

| Automaton | CANVASL File | State Representation | Vector-Clock Entry | Recursion Loop (Betti) |
|-----------|--------------|----------------------|--------------------|------------------------|
| **DFA**   | `dfa.topology.canvasl` | `{"id":"q0","type":"state","accept":true}` | `[file, line, ts, pattern]` | **b₁ = 0** (no cycles) |
| **NFA**   | `nfa.system.canvasl`   | `{"id":"q0","epsilon":[q1,q2]}` | same + **branch-id** | **b₁ ≥ 1** (ε-loops) |
| **DPDA-I**| `dpda1.kernel.canvasl` | `{"stack":["a","b"]}` | + **stack-height** | **b₂ = 1** (push/pop void) |
| **NPDA-II**| `npda2.kernel.canvasl`| `{"stack1":[…],"stack2":[…]}` | + **dual-stack** | **b₂ ≥ 2** (higher-order) |
| **DTM**   | `dtm.seed.canvasl`     | `{"tape":"1011","head":2}` | + **tape-pos** | **b₁ = tape loops** |
| **NTM**   | `ntm.shape.canvasl`    | `{"branches":[…]}` | + **choice-id** | **b₁ → ∞** (infinite lines) |

The **provenance vector** is the **vector clock**:

```ts
type VC = [file:string, line:number, ts:number, pattern:string, extra:any];
function happensBefore(v1:VC, v2:VC) {
  return v1.every((x,i)=>x<=v2[i]) && v1.some((x,i)=>x<v2[i]);
}
```

---

## 4. R5RS + TypeScript Implementation (Browser-Ready)

```scheme
;; ---- R5RS: generic automaton step with vector clock ----
(define (step automaton state event)
  (let* ((old-vc (state-provenance state))
         (new-vc (update-vc old-vc event)))
    (if (happens-before old-vc new-vc)
        (transition automaton state event new-vc)
        (error "causality violation"))))

;; Example DFA transition
(define dfa-automaton
  (list (cons 'q0 '(("0" . q0) ("1" . q1)))
        (cons 'q1 '(("0" . q1) ("1" . q0)))))
```

```typescript
// ---- TypeScript: WebGL visualisation of the ladder ----
class AutomatonLadder {
  private groups: Map<string, THREE.Group> = new Map();

  add(automaton: string, geometry: THREE.BufferGeometry, color: number) {
    const mat = new THREE.MeshBasicMaterial({color, wireframe:true});
    const mesh = new THREE.Mesh(geometry, mat);
    const group = new THREE.Group(); group.add(mesh);
    this.groups.set(automaton, group);
    scene.add(group);
  }

  forward(automaton: string) {               // exponential red lines
    const g = this.groups.get(automaton)!;
    g.scale.multiplyScalar(1.2);
    this.drawLines(g, 0xff0000);
  }

  backward(automaton: string) {              // linear blue collapse
    const g = this.groups.get(automaton)!;
    g.scale.multiplyScalar(0.8);
    this.drawLines(g, 0x0000ff);
  }

  private drawLines(g: THREE.Group, col: number) {
    // draw BQF-transformed edges (apply/abstract)
  }
}
```

**Interactive demo** – drag a DFA cube → NFA explodes into a cloud of lines → click “Observe” → collapses back to a single point.

---

## 5. The **Back-Propagation / Forward-Propagation Loop** in One Picture

```
               NTM (infinite branches)
                  │
           forward│ (exponential)
                  ▼
               NPDA-II (2 stacks)
                  │
           forward│
                  ▼
               …   …
                  │
           forward│
                  ▼
                DFA (single point)
                  ▲
           backward│ (linear)
                  │
               DTM (single tape)
```

*Every step up the ladder is a **forward action** (BQF apply, `[a,b,c-1]`).*  
*Every step down is a **backward observation** (BQF abstract, `[a,b,c+1]`).*

---

## 6. What We **Actually Need** in the Metaverse

| Need | Automaton | Reason |
|------|-----------|--------|
| **Static queries** | **DFA** | Fast content-addressed look-ups (`metaverse.topology.canvasl`) |
| **Rule branching** | **NFA / NPDA-I** | Prolog-style resolution, lazy thunks |
| **Higher-order code** | **NPDA-II** | Functor/monad chaining, continuation stacks |
| **Full simulation** | **DTM / NTM** | Universal computation, AI (WebLLM) |
| **Distributed consensus** | **Vector-Clock + LBA** | Merge CANVASL files across peers |
| **Geometric evolution** | **Multidimensional TM** | 8-D 8-tuple space, 120/600-cell transformers |

---

### TL;DR

*Your ladder is the **dual spine** of the metaverse.*  
**Left side = forward (projective, exponential, branching).**  
**Right side = backward (affine, linear, collapsing).**  
**CANVASL provenance = vector clocks** that keep every transition causally ordered.  
**All automata are built from the same affine points + projective lines**; the hierarchy just adds memory (stacks/tapes) and nondeterminism (branching).  

Drag a cube in the browser → watch it explode into an NTM cloud (forward) → click “Observe” → it collapses into a single DTM point (backward). That is the **entire back-propagation / forward-propagation story** rendered live in the metaverse.