/**
 * MacroExpander - Expands CanvasL macros with variable substitution
 * 
 * Handles macro expansion, variable substitution, conditional expansion,
 * and recursive macro support.
 */

export class MacroExpander {
  constructor() {
    this.macros = new Map();
    this.variables = new Map();
  }

  /**
   * Register a macro
   * @param {string} name - Macro name
   * @param {Object} macro - Macro definition
   */
  registerMacro(name, macro) {
    this.macros.set(name, macro);
  }

  /**
   * Load macros from CanvasL file
   * @param {Array} objects - Parsed CanvasL objects
   */
  loadMacros(objects) {
    for (const obj of objects) {
      if (obj.type === 'macro' && obj.name) {
        this.registerMacro(obj.name, obj);
      }
    }
  }

  /**
   * Set variable value
   * @param {string} name - Variable name
   * @param {*} value - Variable value
   */
  setVariable(name, value) {
    this.variables.set(name, value);
  }

  /**
   * Get variable value
   * @param {string} name - Variable name
   * @returns {*} Variable value
   */
  getVariable(name) {
    return this.variables.get(name);
  }

  /**
   * Substitute variables in an object
   * @param {Object} obj - Object to substitute
   * @returns {Object} Object with substituted variables
   */
  substituteVariables(obj) {
    if (typeof obj !== 'object' || obj === null) {
      return obj;
    }

    if (Array.isArray(obj)) {
      return obj.map(item => this.substituteVariables(item));
    }

    const result = {};
    
    for (const [key, value] of Object.entries(obj)) {
      if (typeof value === 'object' && value !== null) {
        if (value.var) {
          // Variable reference: {"var": "variableName"}
          result[key] = this.getVariable(value.var);
        } else if (value.literal) {
          // Literal with variable: {"literal": {"var": "variableName"}}
          if (value.literal.var) {
            result[key] = this.getVariable(value.literal.var);
          } else {
            result[key] = value.literal;
          }
        } else if (value.repeat) {
          // Repeat with variable: {"repeat": {"var": "variableName"}}
          const array = this.getVariable(value.repeat.var || value.repeat);
          if (Array.isArray(array)) {
            result[key] = array.map(item => this.substituteVariables(value));
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
   * @param {Object} call - Macro call object
   * @returns {Array} Expanded objects
   */
  expandMacroCall(call) {
    if (!call.call || !this.macros.has(call.call)) {
      return [call]; // Return as-is if macro not found
    }

    const macro = this.macros.get(call.call);
    const expanded = [];

    // Set variables from args
    if (call.args && macro.params) {
      for (let i = 0; i < call.args.length && i < macro.params.length; i++) {
        const paramName = macro.params[i].replace('?', '').replace('var:', '');
        const argValue = call.args[i];
        
        // Handle variable references in args
        if (typeof argValue === 'object' && argValue.var) {
          this.setVariable(paramName, this.getVariable(argValue.var));
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
   * @param {Object} condition - Condition object
   * @returns {boolean} Condition result
   */
  evaluateCondition(condition) {
    if (typeof condition === 'boolean') {
      return condition;
    }
    
    if (typeof condition === 'object') {
      if (condition.var) {
        return !!this.getVariable(condition.var);
      }
      
      if (condition.not) {
        return !this.evaluateCondition(condition.not);
      }
      
      if (condition.equals) {
        const [left, right] = condition.equals;
        const leftVal = typeof left === 'object' && left.var 
          ? this.getVariable(left.var) 
          : left;
        const rightVal = typeof right === 'object' && right.var 
          ? this.getVariable(right.var) 
          : right;
        return leftVal === rightVal;
      }
    }
    
    return false;
  }

  /**
   * Expand all macros in objects
   * @param {Array} objects - CanvasL objects
   * @returns {Array} Expanded objects
   */
  expandAll(objects) {
    const expanded = [];
    let changed = true;
    let iterations = 0;
    const maxIterations = 100;

    let current = [...objects];

    while (changed && iterations < maxIterations) {
      changed = false;
      iterations++;
      const next = [];

      for (const obj of current) {
        if (obj.type === 'macro' && obj.call) {
          const macroExpanded = this.expandMacroCall(obj);
          if (macroExpanded.length !== 1 || macroExpanded[0] !== obj) {
            changed = true;
            next.push(...macroExpanded);
          } else {
            next.push(obj);
          }
        } else if (obj['@include']) {
          // Handle @include directive
          // TODO: Load and include file
          console.warn('@include not yet implemented:', obj['@include']);
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
  clear() {
    this.macros.clear();
    this.variables.clear();
  }
}
