/**
 * IDE-Specific Macro Implementations
 * Voice commands for development environment operations
 */

import type { WebAPIMacro } from './types.js';

// ========================================
// IDE COMMAND MACRO
// ========================================

export class IDECommandMacro implements WebAPIMacro {
  keyword: string;
  api = "ide_command";
  method: string;
  params: Record<string, any>;
  type: [string, string];

  constructor(
    keyword: string,
    command: string,
    params: Record<string, any> = {}
  ) {
    this.keyword = keyword;
    this.method = command;
    this.params = params;
    this.type = ["ide_command", command];
  }

  async execute(): Promise<{ command: string; shortcut?: string; output?: string }> {
    const shortcut = this.params.text || '';

    // Copy command/shortcut to clipboard for easy pasting
    if (shortcut && navigator.clipboard) {
      try {
        await navigator.clipboard.writeText(shortcut);
        console.log(`[IDECommand] Copied to clipboard: ${shortcut}`);
      } catch (error) {
        console.warn('[IDECommand] Clipboard write failed:', error);
      }
    }

    return {
      command: this.method,
      shortcut: shortcut,
      output: `IDE command ready: ${this.keyword}`
    };
  }
}

// ========================================
// KEYBOARD SHORTCUT MACRO
// ========================================

export class KeyboardShortcutMacro implements WebAPIMacro {
  keyword: string;
  api = "keyboard";
  method = "sendKeys";
  params: { keys: string; description: string };
  type: [string, string] = ["keyboard", "shortcut"];

  constructor(keyword: string, keys: string, description: string) {
    this.keyword = keyword;
    this.params = { keys, description };
  }

  async execute(): Promise<{ keys: string; description: string }> {
    // Copy keyboard shortcut to clipboard
    if (navigator.clipboard) {
      try {
        await navigator.clipboard.writeText(this.params.keys);
        console.log(`[Keyboard] Copied shortcut: ${this.params.keys}`);
      } catch (error) {
        console.warn('[Keyboard] Clipboard write failed:', error);
      }
    }

    return {
      keys: this.params.keys,
      description: this.params.description
    };
  }
}

// ========================================
// TERMINAL COMMAND MACRO
// ========================================

export class TerminalCommandMacro implements WebAPIMacro {
  keyword: string;
  api = "terminal";
  method = "executeCommand";
  params: { command: string; args?: string[] };
  type: [string, string] = ["terminal", "command"];

  constructor(keyword: string, command: string, args: string[] = []) {
    this.keyword = keyword;
    this.params = { command, args };
  }

  async execute(): Promise<{ command: string; fullCommand: string }> {
    const fullCommand = this.params.args
      ? `${this.params.command} ${this.params.args.join(' ')}`
      : this.params.command;

    // Copy full command to clipboard
    if (navigator.clipboard) {
      try {
        await navigator.clipboard.writeText(fullCommand);
        console.log(`[Terminal] Copied command: ${fullCommand}`);
      } catch (error) {
        console.warn('[Terminal] Clipboard write failed:', error);
      }
    }

    return {
      command: this.params.command,
      fullCommand
    };
  }
}

// ========================================
// GIT COMMAND MACRO
// ========================================

export class GitCommandMacro implements WebAPIMacro {
  keyword: string;
  api = "git";
  method: string;
  params: { args?: string[]; message?: string };
  type: [string, string];

  constructor(
    keyword: string,
    gitCommand: string,
    params: { args?: string[]; message?: string } = {}
  ) {
    this.keyword = keyword;
    this.method = gitCommand;
    this.params = params;
    this.type = ["git", gitCommand];
  }

  async execute(): Promise<{ command: string; fullCommand: string }> {
    let fullCommand = `git ${this.method}`;

    if (this.params.message) {
      fullCommand += ` -m "${this.params.message}"`;
    }

    if (this.params.args && this.params.args.length > 0) {
      fullCommand += ` ${this.params.args.join(' ')}`;
    }

    // Copy git command to clipboard
    if (navigator.clipboard) {
      try {
        await navigator.clipboard.writeText(fullCommand);
        console.log(`[Git] Copied command: ${fullCommand}`);
      } catch (error) {
        console.warn('[Git] Clipboard write failed:', error);
      }
    }

    return {
      command: this.method,
      fullCommand
    };
  }
}

// ========================================
// NPM COMMAND MACRO
// ========================================

export class NPMCommandMacro implements WebAPIMacro {
  keyword: string;
  api = "npm";
  method: string;
  params: { script?: string; args?: string[] };
  type: [string, string];

  constructor(
    keyword: string,
    npmCommand: string,
    params: { script?: string; args?: string[] } = {}
  ) {
    this.keyword = keyword;
    this.method = npmCommand;
    this.params = params;
    this.type = ["npm", npmCommand];
  }

  async execute(): Promise<{ command: string; fullCommand: string }> {
    let fullCommand = `npm ${this.method}`;

    if (this.params.script) {
      fullCommand += ` ${this.params.script}`;
    }

    if (this.params.args && this.params.args.length > 0) {
      fullCommand += ` ${this.params.args.join(' ')}`;
    }

    // Copy npm command to clipboard
    if (navigator.clipboard) {
      try {
        await navigator.clipboard.writeText(fullCommand);
        console.log(`[NPM] Copied command: ${fullCommand}`);
      } catch (error) {
        console.warn('[NPM] Clipboard write failed:', error);
      }
    }

    return {
      command: this.method,
      fullCommand
    };
  }
}

// ========================================
// IDE MACRO FACTORY
// ========================================

/**
 * Create IDE-specific macros from configuration
 */
export function createIDEMacro(config: {
  keyword: string;
  api: string;
  method: string;
  params: Record<string, any>;
}): WebAPIMacro | null {
  switch (config.api) {
    case 'ide_command':
      return new IDECommandMacro(config.keyword, config.method, config.params);

    case 'keyboard':
      return new KeyboardShortcutMacro(
        config.keyword,
        config.params.keys || '',
        config.params.description || config.method
      );

    case 'terminal':
      return new TerminalCommandMacro(
        config.keyword,
        config.params.command || config.method,
        config.params.args
      );

    case 'git':
      return new GitCommandMacro(
        config.keyword,
        config.method,
        config.params
      );

    case 'npm':
      return new NPMCommandMacro(
        config.keyword,
        config.method,
        config.params
      );

    default:
      return null;
  }
}

// ========================================
// COMMON IDE COMMANDS
// ========================================

export const COMMON_IDE_COMMANDS = {
  // File Operations
  save: { keys: 'Ctrl+S', description: 'Save file' },
  open: { keys: 'Ctrl+O', description: 'Open file' },
  close: { keys: 'Ctrl+W', description: 'Close file' },
  newFile: { keys: 'Ctrl+N', description: 'New file' },

  // Navigation
  goto: { keys: 'Ctrl+G', description: 'Go to line' },
  definition: { keys: 'F12', description: 'Go to definition' },
  references: { keys: 'Shift+F12', description: 'Find references' },
  back: { keys: 'Alt+Left', description: 'Navigate back' },
  forward: { keys: 'Alt+Right', description: 'Navigate forward' },

  // Search
  search: { keys: 'Ctrl+F', description: 'Find in file' },
  find: { keys: 'Ctrl+Shift+F', description: 'Find in files' },
  replace: { keys: 'Ctrl+H', description: 'Replace' },

  // Editing
  comment: { keys: 'Ctrl+/', description: 'Toggle comment' },
  format: { keys: 'Shift+Alt+F', description: 'Format document' },
  indent: { keys: 'Tab', description: 'Indent' },
  outdent: { keys: 'Shift+Tab', description: 'Outdent' },
  rename: { keys: 'F2', description: 'Rename symbol' },

  // Clipboard
  copy: { keys: 'Ctrl+C', description: 'Copy' },
  paste: { keys: 'Ctrl+V', description: 'Paste' },
  cut: { keys: 'Ctrl+X', description: 'Cut' },
  undo: { keys: 'Ctrl+Z', description: 'Undo' },
  redo: { keys: 'Ctrl+Y', description: 'Redo' },
  select: { keys: 'Ctrl+A', description: 'Select all' },

  // UI
  terminal: { keys: 'Ctrl+`', description: 'Toggle terminal' },
  sidebar: { keys: 'Ctrl+B', description: 'Toggle sidebar' },
  panel: { keys: 'Ctrl+J', description: 'Toggle panel' },

  // Debug
  debug: { keys: 'F5', description: 'Start debugging' },
  breakpoint: { keys: 'F9', description: 'Toggle breakpoint' },
  stepOver: { keys: 'F10', description: 'Step over' },
  stepInto: { keys: 'F11', description: 'Step into' },
};

export const COMMON_GIT_COMMANDS = {
  status: 'git status',
  add: 'git add .',
  commit: 'git commit',
  push: 'git push',
  pull: 'git pull',
  checkout: 'git checkout',
  branch: 'git branch',
  merge: 'git merge',
  log: 'git log',
  diff: 'git diff',
};

export const COMMON_NPM_COMMANDS = {
  install: 'npm install',
  start: 'npm start',
  test: 'npm test',
  build: 'npm run build',
  dev: 'npm run dev',
  lint: 'npm run lint',
};
