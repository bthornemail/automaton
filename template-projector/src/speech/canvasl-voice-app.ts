/**
 * CANVASL Voice App Runtime
 * Executes compiled voice-controlled applications
 */

import type {
  CANVASLTemplate,
  CompiledVoiceApp,
  ChainComplex,
  EvolutionContext
} from './types.js';
import { TemplateCompiler, computeAllBetti, eulerCharacteristic } from './template-compiler.js';
import { SpeechRecognitionHandler, SpeechSynthesisHandler } from './speech-handlers.js';
import { MacroRegistry, ResolutionRegistry, createMacro } from './web-api-macros.js';

// ========================================
// CANVASL VOICE APP
// ========================================

export class CANVASLVoiceApp {
  private template: CANVASLTemplate;
  private compiled: CompiledVoiceApp;
  private recognitionHandler: SpeechRecognitionHandler;
  private synthesisHandler: SpeechSynthesisHandler;
  private macroRegistry: MacroRegistry;
  private resolutionRegistry: ResolutionRegistry;
  private chainComplex: ChainComplex;
  private running = false;

  constructor(template: CANVASLTemplate) {
    this.template = template;

    // Compile template
    const compiler = new TemplateCompiler();
    this.compiled = compiler.compile(template);

    if (!this.compiled.validation.valid) {
      console.warn(
        `[CANVASLVoiceApp] Template validation warnings: ${this.compiled.validation.errors.join(", ")}`
      );
    }

    // Initialize chain complex
    this.chainComplex = this.compiled.complex;

    // Setup macro registry
    this.macroRegistry = new MacroRegistry();
    this.resolutionRegistry = new ResolutionRegistry();
    this.registerMacros();

    // Setup speech handlers with actual callback
    this.recognitionHandler = new SpeechRecognitionHandler(
      template.frontmatter.speech.input,
      (keyword, transcript) => this.handleKeyword(keyword, transcript)
    );

    this.synthesisHandler = new SpeechSynthesisHandler(
      template.frontmatter.speech.output
    );

    console.log(`[CANVASLVoiceApp] Created app for template: ${template.id}`);
  }

  /**
   * Register all macros from template
   */
  private registerMacros() {
    if (!this.template.frontmatter.macros) return;

    for (const macroConfig of this.template.frontmatter.macros) {
      const macro = createMacro(macroConfig);

      if (macro) {
        this.macroRegistry.register(macro);
      } else {
        console.warn(`[CANVASLVoiceApp] Could not create macro for: ${macroConfig.api}`);
      }
    }

    // Register macro registry with resolution registry
    this.resolutionRegistry.register("web_api", this.macroRegistry);
  }

  /**
   * Handle keyword detection from speech recognition
   */
  private async handleKeyword(keyword: string, transcript: string) {
    console.log(`[CANVASLVoiceApp] Detected keyword: ${keyword} in "${transcript}"`);

    try {
      // Resolve keyword through resolution registry
      const localDomain = new Set([this.template.id]);
      const result = await this.resolutionRegistry.resolve(
        localDomain,
        ["web_api", "browser"],
        keyword
      );

      console.log(`[CANVASLVoiceApp] Execution result:`, result);

      // Speak result
      const resultText = this.formatResult(keyword, result);
      await this.synthesisHandler.speak(resultText);

      // Record execution in chain complex
      await this.recordExecution(keyword, result);

    } catch (error: any) {
      console.error(`[CANVASLVoiceApp] Error executing ${keyword}:`, error);
      await this.synthesisHandler.speak(`Error executing ${keyword}: ${error.message}`);
    }
  }

  /**
   * Format execution result for speech output
   */
  private formatResult(keyword: string, result: any): string {
    const value = result.value;

    switch (keyword) {
      case "location":
        if (value?.coords) {
          return `Your location is latitude ${value.coords.latitude.toFixed(2)}, longitude ${value.coords.longitude.toFixed(2)}`;
        }
        return "Location obtained";

      case "notify":
        return "Notification sent";

      case "save":
        return "Data saved";

      case "copy":
        return "Copied to clipboard";

      case "battery":
        if (value?.level !== undefined) {
          const percent = (value.level * 100).toFixed(0);
          return `Battery level is ${percent} percent`;
        }
        return "Battery status obtained";

      default:
        return `Executed ${keyword}`;
    }
  }

  /**
   * Record execution as C4 cell (evolution context)
   */
  private async recordExecution(keyword: string, result: any) {
    const context: EvolutionContext = {
      id: `exec-${Date.now()}`,
      lists: [[
        keyword,
        JSON.stringify(result.value),
        new Date().toISOString()
      ]],
      dimension: 4
    };

    this.chainComplex.C4.push(context);

    // Update boundary: ∂₄(context) → interface triples
    const relevantTriples = this.chainComplex.C3
      .filter(t => t.triple[0] === keyword)
      .map(t => t.id);

    if (relevantTriples.length > 0) {
      this.chainComplex.∂4.set(context.id, {
        solids: relevantTriples,
        signs: relevantTriples.map(() => 1)
      });
    }

    console.log(`[CANVASLVoiceApp] Recorded execution: ${context.id}`);
  }

  /**
   * Start voice recognition and synthesis
   */
  start() {
    if (this.running) {
      console.warn('[CANVASLVoiceApp] Already running');
      return;
    }

    this.running = true;
    this.recognitionHandler.start();
    this.synthesisHandler.speak("Voice application started. Say a keyword to begin.");
    console.log(`[CANVASLVoiceApp] Started`);
  }

  /**
   * Stop voice recognition and synthesis
   */
  stop() {
    if (!this.running) return;

    this.running = false;
    this.recognitionHandler.stop();
    this.synthesisHandler.cancel();
    console.log(`[CANVASLVoiceApp] Stopped`);
  }

  /**
   * Manually execute a keyword
   */
  async executeKeyword(keyword: string): Promise<any> {
    return await this.handleKeyword(keyword, `manual: ${keyword}`);
  }

  /**
   * Export current state as JSONL
   */
  exportState(): string {
    const nodes = [
      ...this.chainComplex.C0,
      ...this.chainComplex.C1,
      ...this.chainComplex.C2,
      ...this.chainComplex.C3,
      ...this.chainComplex.C4
    ];

    return nodes.map(n => JSON.stringify(n)).join("\n");
  }

  /**
   * Get homological summary
   */
  getSummary(): {
    betti: number[];
    euler: number;
    executionCount: number;
    templateId: string;
    running: boolean;
  } {
    const betti = computeAllBetti(this.chainComplex);
    const euler = eulerCharacteristic(betti);
    const executionCount = this.chainComplex.C4.length;

    return {
      betti,
      euler,
      executionCount,
      templateId: this.template.id,
      running: this.running
    };
  }

  /**
   * Get template
   */
  getTemplate(): CANVASLTemplate {
    return this.template;
  }

  /**
   * Get chain complex
   */
  getChainComplex(): ChainComplex {
    return this.chainComplex;
  }

  /**
   * Check if app is running
   */
  isRunning(): boolean {
    return this.running;
  }
}

// ========================================
// APP MANAGER
// ========================================

/**
 * Manages multiple CANVASL voice apps
 */
export class CANVASLAppManager {
  private apps = new Map<string, CANVASLVoiceApp>();

  /**
   * Create and register a voice app from template
   */
  createApp(template: CANVASLTemplate): string {
    const app = new CANVASLVoiceApp(template);
    const appId = template.id;
    this.apps.set(appId, app);
    console.log(`[AppManager] Created app: ${appId}`);
    return appId;
  }

  /**
   * Get app by ID
   */
  getApp(appId: string): CANVASLVoiceApp | undefined {
    return this.apps.get(appId);
  }

  /**
   * Start app
   */
  startApp(appId: string): void {
    const app = this.apps.get(appId);
    if (!app) {
      throw new Error(`App not found: ${appId}`);
    }
    app.start();
  }

  /**
   * Stop app
   */
  stopApp(appId: string): void {
    const app = this.apps.get(appId);
    if (!app) {
      throw new Error(`App not found: ${appId}`);
    }
    app.stop();
  }

  /**
   * Stop all apps
   */
  stopAll(): void {
    for (const app of this.apps.values()) {
      app.stop();
    }
  }

  /**
   * Get all app IDs
   */
  getAllAppIds(): string[] {
    return Array.from(this.apps.keys());
  }

  /**
   * Get summary of all apps
   */
  getAllSummaries(): Record<string, any> {
    const summaries: Record<string, any> = {};
    for (const [id, app] of this.apps) {
      summaries[id] = app.getSummary();
    }
    return summaries;
  }
}
