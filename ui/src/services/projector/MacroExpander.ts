/**
 * MacroExpander - Expands CanvasL macros with variable substitution
 * 
 * Handles macro expansion, variable substitution, conditional expansion,
 * and recursive macro support.
 */

export interface Macro {
  name?: string;
  params?: string[];
  expansion?: any[];
  [key: string]: any;
}

export class MacroExpander {
  private macros: Map<string, Macro>;
  private variables: Map<string, any>;

  constructor() {
    this.macros = new Map();
    this.variables = new Map();
  }

  /**
   * Register a macro
   */
  registerMacro(name: string, macro: Macro): void {
    this.macros.set(name, macro);
  }

  /**
   * Load macros from CanvasL file
   */
  loadMacros(objects: any[]): void {
    for (const obj of objects) {
      if (obj.type === 'macro' && obj.name) {
        this.registerMacro(obj.name, obj);
      }
    }
  }

  /**
   * Set variable value
   */
  setVariable(name: string, value: any): void {
    this.variables.set(name, value);
  }

  /**
   * Get variable value
   */
  getVariable(name: string): any {
    return this.variables.get(name);
  }

  /**
   * Substitute variables in an object
   */
  substituteVariables(obj: any): any {
    if (typeof obj !== 'object' || obj === null) {
      return obj;
    }

    if (Array.isArray(obj)) {
      return obj.map(item => this.substituteVariables(item));
    }

    const result: any = {};
    
    for (const [key, value] of Object.entries(obj)) {
      if (typeof value === 'object' && value !== null) {
        if ((value as any).var) {
          // Variable reference: {"var": "variableName"}
          result[key] = this.getVariable((value as any).var);
        } else if ((value as any).literal) {
          // Literal with variable: {"literal": {"var": "variableName"}}
          if ((value as any).literal.var) {
            result[key] = this.getVariable((value as any).literal.var);
          } else {
            result[key] = (value as any).literal;
          }
        } else if ((value as any).repeat) {
          // Repeat with variable: {"repeat": {"var": "variableName"}}
          const array = this.getVariable((value as any).repeat.var || (value as any).repeat);
          if (Array.isArray(array)) {
            result[key] = array.map((item: any) => this.substituteVariables(value));
          } else {
            result[key] = [];
          }
        } else {
          result[key] = this.substituteVariables(value);
        }
      } else {
        result[key] = value;
      }
    }
    
    return result;
  }

  /**
   * Expand a macro call
   */
  expandMacroCall(call: any): any[] {
    if (!call.call || !this.macros.has(call.call)) {
      return [call]; // Return as-is if macro not found
    }

    const macro = this.macros.get(call.call)!;
    const expanded: any[] = [];

    // Set variables from args
    if (call.args && macro.params) {
      for (let i = 0; i < call.args.length && i < macro.params.length; i++) {
        const paramName = macro.params[i].replace('?', '').replace('var:', '');
        const argValue = call.args[i];
        
        // Handle variable references in args
        if (typeof argValue === 'object' && argValue && (argValue as any).var) {
          this.setVariable(paramName, this.getVariable((argValue as any).var));
        } else {
          this.setVariable(paramName, argValue);
        }
      }
    }

    // Expand macro expansion
    if (macro.expansion) {
      for (const item of macro.expansion) {
        const substituted = this.substituteVariables(item);
        
        // Handle conditional expansion
        if (substituted.if) {
          const condition = this.evaluateCondition(substituted.if);
          if (condition) {
            expanded.push(...this.expandMacroCall(substituted.then || substituted));
          } else if (substituted.else) {
            expanded.push(...this.expandMacroCall(substituted.else));
          }
        } else if (substituted.type === 'macro' && substituted.call) {
          // Recursive macro expansion
          expanded.push(...this.expandMacroCall(substituted));
        } else {
          expanded.push(substituted);
        }
      }
    }

    return expanded;
  }

  /**
   * Evaluate a condition
   */
  private evaluateCondition(condition: any): boolean {
    if (typeof condition === 'boolean') {
      return condition;
    }
    
    if (typeof condition === 'object' && condition !== null) {
      if ((condition as any).var) {
        return !!this.getVariable((condition as any).var);
      }
      
      if ((condition as any).not) {
        return !this.evaluateCondition((condition as any).not);
      }
      
      if ((condition as any).equals) {
        const [left, right] = (condition as any).equals;
        const leftVal = typeof left === 'object' && left && (left as any).var 
          ? this.getVariable((left as any).var) 
          : left;
        const rightVal = typeof right === 'object' && right && (right as any).var 
          ? this.getVariable((right as any).var) 
          : right;
        return leftVal === rightVal;
      }
    }
    
    return false;
  }

  /**
   * Expand all macros in objects
   */
  expandAll(objects: any[]): any[] {
    const expanded: any[] = [];
    let changed = true;
    let iterations = 0;
    const maxIterations = 100;

    let current = [...objects];

    while (changed && iterations < maxIterations) {
      changed = false;
      iterations++;
      const next: any[] = [];

      for (const obj of current) {
        if (obj.type === 'macro' && obj.call) {
          const macroExpanded = this.expandMacroCall(obj);
          if (macroExpanded.length !== 1 || macroExpanded[0] !== obj) {
            changed = true;
            next.push(...macroExpanded);
          } else {
            next.push(obj);
          }
        } else if ((obj as any)['@include']) {
          // Handle @include directive
          // TODO: Load and include file
          console.warn('@include not yet implemented:', (obj as any)['@include']);
          next.push(obj);
        } else {
          next.push(obj);
        }
      }

      current = next;
    }

    if (iterations >= maxIterations) {
      console.warn('Macro expansion reached max iterations, possible recursion');
    }

    return current;
  }

  /**
   * Clear all macros and variables
   */
  clear(): void {
    this.macros.clear();
    this.variables.clear();
  }
}

