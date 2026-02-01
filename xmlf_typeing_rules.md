# xMLF typing rules (from `papers/these-finale-english.txt`, §14.2)

This note extracts the *declarative* syntax and typing judgments for **xMLF** (the fully explicit, Church-style language for MLF) as presented in the thesis text file.

The relevant material is in Part III, Chapter 14, Section 14.2:

- §14.2.1: Types, terms, environments (Figures 14.2.1–14.2.4)
- §14.2.2: Type computations + type-instance judgment (Figures 14.2.5–14.2.7)
- §14.2.3: Term typing judgment (Figure 14.2.8)

## 0. Conventions and notation

- Type variables: `α, β, γ, δ`
- Term variables: `x, y, z`
- `ftv(σ)` is the set of free type variables in type `σ`.
- `dom(Γ)` is the set of variables (type + term) bound in environment `Γ`.
- Terms/types/computations are identified **up to α-renaming** (renaming of bound variables).
- Capture-avoiding substitution is written `q{v ← p}` (where `q` can be a term, type, computation, or environment).
- Capture-avoiding *replacement* of computation occurrences is written `q{α ⊳ ← ϕ}` (replace occurrences of computation `α ⊳` by computation `ϕ` in `q`).

## 1. Syntax

### 1.1 Types (Figure 14.2.1)

Types `σ`:

```
σ ::= α
    | σ → σ
    | C σ
    | ∀ (α ⩾ σ) σ
    | ⊥
```

Notes:

- `∀ (α ⩾ σ1) σ2` is **instance-bounded** (flexible) quantification.
- In xMLF, rigid bounds are not present (they are “inlined inside types”).

### 1.2 Terms (Figure 14.2.2)

Terms `a`:

```
a ::= x
    | λ(x : σ) a
    | a a
    | Λ(α ⩾ σ) a
    | a[ϕ]
    | let x = a in a
```

Notes:

- Type instantiation is explicit: `a[ϕ]` (generalizes System F type application).
- In `Λ(α ⩾ σ) a`, the bound `α ⩾ σ` is part of the binder:
  - `α` is bound in `a`
  - `α` is **not** bound in the bound type `σ`
- System F’s `Λ(α) a` can be simulated by `Λ(α ⩾ ⊥) a`.

### 1.3 Environments (Figure 14.2.3)

Environments `Γ`:

```
Γ ::= ∅
    | Γ, α ⩾ σ
    | Γ, x : σ
```

The thesis assumes environments are *implicitly well-formed* (see §2).

## 2. Well-formed environments (Figure 14.2.4)

Judgment: `wf(Γ)`

```
(wf-∅)
wf(∅)

(wf-TVar)
wf(Γ)      α ∉ dom(Γ)      ftv(σ) ⊆ dom(Γ)
──────────────────────────────────────────
wf(Γ, α ⩾ σ)

(wf-Var)
wf(Γ)      x ∉ dom(Γ)      ftv(σ) ⊆ dom(Γ)
─────────────────────────────────────────
wf(Γ, x : σ)
```

## 3. Type computations (Figure 14.2.5)

Type computations `ϕ`:

```
ϕ ::= ε
    | ⊲σ
    | α ⊳
    | ∀ (⩾ ϕ)
    | ∀ (α ⩾) ϕ
    | N
    | O
    | ϕ ; ϕ
```

Intuition (as described in §14.2.2):

- `ε` is reflexivity.
- `ϕ1 ; ϕ2` is composition / transitivity.
- `⊲σ` witnesses that **any** type `σ` is an instance of `⊥`.
- If `α ⩾ σ ∈ Γ`, then `α ⊳` witnesses that `α` is an instance of `σ`.
- `∀ (⩾ ϕ)` acts on the *bound* of an instance-bounded quantifier.
- `∀ (α ⩾) ϕ` acts on the *body* of an instance-bounded quantifier; `α` is bound in `ϕ`.
- `O` introduces a trivial quantification `∀ (α ⩾ ⊥)`.
- `N` eliminates a quantification `∀ (α ⩾ σ) σ′` by substituting `α := σ` in `σ′`.

## 4. Type instance judgment (Figure 14.2.6)

Judgment form:

- `Γ ⊢ ϕ : σ ≤ σ′`

Meaning (thesis wording):

- In environment `Γ`, computation `ϕ` witnesses that `σ′` is an **instance** of `σ`.

Rules:

```
(Inst-Reflex)
──────────────
Γ ⊢ ε : σ ≤ σ

(Inst-Trans)
Γ ⊢ ϕ1 : σ1 ≤ σ2      Γ ⊢ ϕ2 : σ2 ≤ σ3
──────────────────────────────────────
Γ ⊢ ϕ1 ; ϕ2 : σ1 ≤ σ3

(Inst-Bot)
──────────────────
Γ ⊢ ⊲σ : ⊥ ≤ σ

(Inst-Hyp)
α ⩾ σ ∈ Γ
───────────────
Γ ⊢ α ⊳ : σ ≤ α

(Inst-Inner)
Γ ⊢ ϕ : σ1 ≤ σ2
────────────────────────────────────────────
Γ ⊢ ∀ (⩾ ϕ) : ∀ (α ⩾ σ1) σ ≤ ∀ (α ⩾ σ2) σ

(Inst-Outer)
Γ, α ⩾ σ ⊢ ϕ : σ1 ≤ σ2
────────────────────────────────────────────
Γ ⊢ ∀ (α ⩾) ϕ : ∀ (α ⩾ σ) σ1 ≤ ∀ (α ⩾ σ) σ2

(Inst-Quant-Elim)
────────────────────────────────────
Γ ⊢ N : ∀ (α ⩾ σ) σ′ ≤ σ′{α ← σ}

(Inst-Quant-Intro)
α ∉ ftv(σ)
──────────────────────────────────
Γ ⊢ O : σ ≤ ∀ (α ⩾ ⊥) σ
```

Note on `Inst-Outer`:

- In the plain-text extraction of the thesis, the premise line appears with a spurious `ϕ :` (“`Γ, ϕ : α ⩾ σ ⊢ ...`”). Given the environment syntax (Figure 14.2.3), the intended premise is `Γ, α ⩾ σ ⊢ ϕ : ...` as written above.

## 5. Applying computations to types (Figure 14.2.7)

The thesis defines a (partial) function that computes the result type `σ[ϕ]`.

This function is **complete** but **not sound** w.r.t. the judgment (it intentionally does not check the premise of `Inst-Hyp`).

Definition:

```
(∀ (α ⩾ σ) σ′)[N] = σ′{α ← σ}

σ[O] = ∀ (α ⩾ ⊥) σ          with α ∉ ftv(σ)

σ[ϕ1 ; ϕ2] = (σ[ϕ1])[ϕ2]

(∀ (α ⩾ σ) σ′)[∀ (⩾ ϕ)] = ∀ (α ⩾ σ[ϕ]) σ′

(∀ (α ⩾ σ) σ′)[∀ (α ⩾) ϕ] = ∀ (α ⩾ σ) σ′[ϕ]

σ[α ⊳] = α

⊥[⊲ σ] = σ

σ[ε] = σ
```

Property (thesis):

- If `Γ ⊢ ϕ : σ ≤ σ′` then `σ[ϕ] = σ′`.

## 6. Term typing judgment (Figure 14.2.8)

Judgment form:

- `Γ ⊢ a : σ`

Rules:

```
(Var)
x : σ ∈ Γ
────────────
Γ ⊢ x : σ

(Abs)
Γ, x : σ ⊢ a : σ′
────────────────────────
Γ ⊢ λ(x : σ) a : σ → σ′

(App)
Γ ⊢ a1 : σ2 → σ1      Γ ⊢ a2 : σ2
──────────────────────────────────
Γ ⊢ a1 a2 : σ1

(TAbs)
Γ, α ⩾ σ′ ⊢ a : σ      α ∉ ftv(Γ)
──────────────────────────────────
Γ ⊢ Λ(α ⩾ σ′) a : ∀ (α ⩾ σ′) σ

(TApp)
Γ ⊢ a : σ      Γ ⊢ ϕ : σ ≤ σ′
──────────────────────────────
Γ ⊢ a[ϕ] : σ′

(Let)
Γ ⊢ a : σ      Γ, x : σ ⊢ a′ : σ′
──────────────────────────────────
Γ ⊢ let x = a in a′ : σ′
```

### 6.1 Derived “System F-like” type application (§14.2.2.1, §14.2.3)

The thesis defines syntactic sugar:

- `σ` (as a computation) abbreviates `∀ (⩾ ⊲ σ) ; N`.

This yields a derived typing rule for type application `a[σ]`:

```
Γ ⊢ a : ∀ (α ⩾ ⊥) σ′
────────────────────────
Γ ⊢ a[σ] : σ′{α ← σ}
```
