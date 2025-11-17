/**
 * CANVASL Speech Input/Output Types
 * Based on sheaf-theoretic formalization with chain complexes
 */

// ========================================
// CHAIN COMPLEX TYPES
// ========================================

export type Keyword = {
  id: string;
  name: string;
  dimension: 0;
};

export type Edge = {
  id: string;
  type: [string, string]; // [schema, identifier] for resolution
  dimension: 1;
};

export type Document = {
  id: string;
  frontmatter: {
    adjacency: {
      edges: string[];
      orientation: number[]; // ±1 for each edge
    };
    features: Record<string, any>;
  };
  body: string;
  dimension: 2;
};

export type InterfaceTriple = {
  id: string;
  triple: [string, string, string];
  dimension: 3;
};

export type EvolutionContext = {
  id: string;
  lists: string[][];
  dimension: 4;
};

export type ChainComplex = {
  C0: Keyword[];        // 0-cells (vertices)
  C1: Edge[];          // 1-cells (edges)
  C2: Document[];      // 2-cells (faces)
  C3: InterfaceTriple[]; // 3-cells (solids)
  C4: EvolutionContext[]; // 4-cells (hypervolumes)

  // Boundary operators (homomorphisms)
  ∂1: Map<string, [string, string]>;           // edge → [vertex, vertex]
  ∂2: Map<string, { edges: string[], signs: number[] }>; // doc → oriented edge cycle
  ∂3: Map<string, { faces: string[], signs: number[] }>; // solid → oriented face cycle
  ∂4: Map<string, { solids: string[], signs: number[] }>; // hypervolume → oriented solid boundary
};

// ========================================
// CANVASL TEMPLATE TYPES
// ========================================

export type CANVASLTemplate = Document & {
  frontmatter: {
    type: "canvasl-template";
    adjacency: {
      edges: string[];
      orientation: number[];
    };

    // Voice input/output configuration
    speech: {
      input: {
        lang: string; // e.g., "en-US"
        continuous: boolean;
        interimResults: boolean;
        keywords: string[]; // Trigger words
      };
      output: {
        voice: string; // e.g., "Google US English"
        rate: number;
        pitch: number;
      };
    };

    // Macro definitions: keyword → W3C API call
    macros: MacroConfig[];

    // Validation rules
    validates: {
      homology: boolean; // Check Ȟ¹ = 0
      byzantine: boolean; // Check fault tolerance
      accessibility: boolean; // WCAG compliance
    };

    features: Record<string, any>;
  };
};

export type MacroConfig = {
  keyword: string;
  api: string; // "geolocation" | "notifications" | "clipboard" | ...
  method: string;
  params: Record<string, any>;
  type: [string, string]; // Resolution type
};

// ========================================
// SPEECH API TYPES
// ========================================

export type SpeechConfig = CANVASLTemplate['frontmatter']['speech'];

export interface SpeechRecognitionHandler {
  start(): void;
  stop(): void;
}

export interface SpeechSynthesisHandler {
  speak(text: string): Promise<void>;
  cancel(): void;
}

export type SpeechHandlers = {
  recognition: SpeechRecognitionHandler;
  synthesis: SpeechSynthesisHandler;
};

// ========================================
// WEB API MACRO TYPES
// ========================================

export type Schema = string; // "web_api" | "google_drive" | "redis_key" | "node_id" | ...

export interface WebAPIMacro {
  keyword: string;
  api: string;
  method: string;
  params: Record<string, any>;
  type: [string, string];
  execute(context?: any): Promise<any>;
}

export type MacroExecutor = {
  keyword: string;
  execute: (params?: any) => Promise<any>;
};

// ========================================
// RESOLUTION FUNCTOR TYPES
// ========================================

export abstract class ResolutionFunctor {
  protected schema: Schema;

  constructor(schema: Schema) {
    this.schema = schema;
  }

  // Res_S(U): resolve on open set U
  abstract resolve(
    U: Set<string>,
    identifier: string,
    keyword: string
  ): Promise<any>;

  // Restriction: ρ_{U,V}: Res_S(U) → Res_S(V)
  abstract restrict(
    value: any,
    fromDomain: Set<string>,
    toDomain: Set<string>
  ): any;
}

// ========================================
// SHEAF TYPES
// ========================================

export type KeywordSection = {
  domain: Set<string>;  // Which automata (subset of X)
  assignments: Map<string, any>; // keyword → resolved value
};

// ========================================
// VALIDATION TYPES
// ========================================

export type ValidationResult = {
  valid: boolean;
  errors: string[];
};

// ========================================
// COMPILED APP TYPES
// ========================================

export type CompiledVoiceApp = {
  complex: ChainComplex;
  speechHandlers: SpeechHandlers;
  macroExecutors: Map<string, MacroExecutor>;
  validation: ValidationResult;
};

// ========================================
// AUTOMATON TYPES
// ========================================

export type AutomatonConfig = {
  autonomousInterval: number;
  maxTemplates: number;
  maxExecutionHistory: number;
  persistInterval: number;
  homologyCheckInterval: number;
  peerSyncInterval: number;
  enableVoice: boolean;
  enableFederation: boolean;
  dimension: number; // 0-11 for M-theory
};

export type AutomatonState = {
  id: string;
  running: boolean;
  dimension: number;
  betti: number[];
  euler: number;
  stateHash: string;
  templateCount: number;
  activeAppCount: number;
  peerCount: number;
  cellCounts: {
    C0: number;
    C1: number;
    C2: number;
    C3: number;
    C4: number;
  };
};
