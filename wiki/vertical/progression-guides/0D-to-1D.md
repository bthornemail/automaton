# Transition Guide: 0D → 1D

**From Foundation to Temporal Progression**

---

## Overview

This guide explains the transition from **0D (Foundation)** to **1D (Temporal)**, showing how identity and fixed points evolve into temporal progression and sequences.

---

## What Changes?

### From 0D: Identity and Fixed Points

**0D provides**:
- Identity function: `λx.x`
- Church zero: `λf.λx.x`
- Fixed points
- Topology foundations

**Key insight**: What doesn't change.

### To 1D: Temporal Progression

**1D provides**:
- Successor function: `λn.λf.λx.f(nfx)`
- Church one: `λf.λx.f(x)`
- Event ordering
- Temporal sequences

**Key insight**: What changes over time.

---

## The Transition

### Step 1: Understand 0D Foundation

**Read first**:
- `topology/0D-topology/0D_Topology_Agent.md` - The Sage
- `topology/0D-topology/Church_Encoding.md` - Church encoding foundations
- `system/0D-system/R5RS_Integration.md` - R5RS implementation

**Key concepts**:
- Identity: `λx.x` - does nothing, returns input unchanged
- Zero: `λf.λx.x` - applies function zero times
- Fixed points: What stays the same

### Step 2: Introduce Successor

**The bridge**: Successor function `λn.λf.λx.f(nfx)`

**What it does**: Takes a number `n` and returns `n+1` by applying function `f` one more time.

**Why it matters**: Enables progression. After zero comes one, after one comes two.

**Example**:
```scheme
;; Zero: apply f zero times
(define zero (lambda (f) (lambda (x) x)))

;; One: apply f once
(define one (lambda (f) (lambda (x) (f x))))

;; Successor: apply f one more time than n
(define succ (lambda (n) 
  (lambda (f) 
    (lambda (x) (f ((n f) x))))))
```

### Step 3: Understand Temporal Structure

**Read next**:
- `topology/1D-topology/1D_Temporal_Agent.md` - The Chronicler

**Key concepts**:
- Temporal topology: `ℝ¹` (one-dimensional line)
- Event ordering: What happened when
- Causality: What causes what

### Step 4: See the System Implementation

**Read**:
- `system/1D-system/Dimensional_Progression.md` - How progression works

**Key concepts**:
- Dimensional chain: 0D → 1D → 2D → ...
- Progression mechanism: How dimensions build on each other

---

## Key Differences

| Aspect | 0D (Foundation) | 1D (Temporal) |
|--------|------------------|----------------|
| **Focus** | What doesn't change | What changes |
| **Church Encoding** | ZERO, ID | SUCC, ONE |
| **Structure** | Point topology | Line topology |
| **Operation** | Identity | Successor |
| **Metaphor** | Foundation | Timeline |

---

## Common Patterns

### Pattern 1: Zero to One

**0D**: Start with nothing (`zero`)
**1D**: Progress to something (`one`)

**Example**:
```scheme
;; 0D: Zero
(define zero (lambda (f) (lambda (x) x)))

;; 1D: One (successor of zero)
(define one (succ zero))
```

### Pattern 2: Fixed Point to Sequence

**0D**: Find what stays the same
**1D**: Track what changes over time

**Example**:
- **0D**: Fixed point in a graph transformation
- **1D**: Sequence of transformations over time

### Pattern 3: Identity to Progression

**0D**: Identity function (no change)
**1D**: Successor function (progression)

**Example**:
- **0D**: `id(x) = x` (returns unchanged)
- **1D**: `succ(n) = n+1` (increments)

---

## Prerequisites

**Before reading this transition**:
- ✅ Understand 0D topology agent
- ✅ Understand Church encoding foundations
- ✅ Understand identity and fixed points

**After reading this transition**:
- ✅ Understand 1D temporal agent
- ✅ Understand successor function
- ✅ Understand temporal progression

---

## Next Steps

**Continue to**: `vertical/progression-guides/1D-to-2D.md`

**Related documentation**:
- **0D Topology**: `topology/0D-topology/0D_Topology_Agent.md`
- **1D Temporal**: `topology/1D-topology/1D_Temporal_Agent.md`
- **Dimensional Chain**: `vertical/dimensional-chain.md`
- **Dimensional Progression**: `vertical/Dimensional_Progression.md`

---

**Last Updated**: 2025-01-07  
**Version**: 1.0.0
