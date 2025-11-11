import { BaseMetaLogPlugin, PluginConfig } from '../core/plugin.js';

/**
 * OpenCode adapter for Meta-Log plugin
 */
export class OpenCodeMetaLogPlugin extends BaseMetaLogPlugin {
  private tools: any[] = [];
  private opencodeAvailable: boolean = false;

  constructor(config: PluginConfig) {
    super(config);
  }

  /**
   * Check if OpenCode plugin API is available
   */
  async checkOpenCodeAvailability(): Promise<boolean> {
    if (this.opencodeAvailable) {
      return true;
    }

    try {
      const module = await import('@opencode-ai/plugin');
      if (module && typeof module.tool === 'function') {
        this.opencodeAvailable = true;
        return true;
      }
    } catch (error) {
      // Module not available or import failed
      this.opencodeAvailable = false;
      return false;
    }

    return false;
  }

  /**
   * Load plugin - register OpenCode tools
   */
  async onLoad(): Promise<void> {
    this.setEnabled(false);
    
    // Load canvas if path provided
    if (this.config.canvasPath) {
      try {
        await this.loadCanvas(this.config.canvasPath);
      } catch (error) {
        console.warn('Failed to load canvas:', error);
        // Continue without canvas - plugin can still function
      }
    }

    // Register OpenCode tools if available
    const isAvailable = await this.checkOpenCodeAvailability();
    
    if (!isAvailable) {
      console.info('OpenCode plugin API not available - tools will not be registered. Install @opencode-ai/plugin as a peer dependency to enable tool registration.');
      return;
    }

    try {
      const { tool } = await import('@opencode-ai/plugin');
      
      // ProLog query tool
      const prologTool = tool({
        description: "Query Meta-Log database with ProLog",
        args: {
          query: tool.schema.string().describe("ProLog query string")
        },
        execute: async (args: any, context: any) => {
          try {
            const query = await this.beforeQuery(args.query);
            const results = await this.db.prologQuery(query);
            return await this.afterQuery(query, results);
          } catch (error) {
            return { error: String(error), query: args.query };
          }
        }
      });
      this.tools.push(prologTool);

      // DataLog query tool
      const datalogTool = tool({
        description: "Query Meta-Log database with DataLog",
        args: {
          query: tool.schema.string().describe("DataLog query string"),
          program: tool.schema.string().optional().describe("DataLog program (optional)")
        },
        execute: async (args: any, context: any) => {
          try {
            const query = await this.beforeQuery(args.query);
            let program;
            if (args.program) {
              try {
                program = typeof args.program === 'string' ? JSON.parse(args.program) : args.program;
              } catch (parseError) {
                return { error: `Invalid program JSON: ${parseError}`, query: args.query };
              }
            }
            const results = await this.db.datalogQuery(query, program);
            return await this.afterQuery(query, results);
          } catch (error) {
            return { error: String(error), query: args.query };
          }
        }
      });
      this.tools.push(datalogTool);

      // SPARQL query tool
      const sparqlTool = tool({
        description: "Query Meta-Log database with SPARQL",
        args: {
          query: tool.schema.string().describe("SPARQL query string")
        },
        execute: async (args: any, context: any) => {
          try {
            const query = await this.beforeQuery(args.query);
            const results = await this.db.sparqlQuery(query);
            return await this.afterQuery(query, results);
          } catch (error) {
            return { error: String(error), query: args.query };
          }
        }
      });
      this.tools.push(sparqlTool);

      // Load canvas tool
      const loadCanvasTool = tool({
        description: "Load JSONL/CanvasL canvas file",
        args: {
          path: tool.schema.string().describe("Path to canvas file")
        },
        execute: async (args: any, context: any) => {
          try {
            await this.loadCanvas(args.path);
            return { success: true, path: args.path };
          } catch (error) {
            return { success: false, error: String(error), path: args.path };
          }
        }
      });
      this.tools.push(loadCanvasTool);

      console.info(`Registered ${this.tools.length} OpenCode tools`);
    } catch (error) {
      // OpenCode plugin API not available or error during tool registration
      const errorMessage = error instanceof Error ? error.message : String(error);
      console.warn('Failed to register OpenCode tools:', errorMessage);
      this.opencodeAvailable = false;
    }
  }

  /**
   * Unload plugin - cleanup tools
   */
  async onUnload(): Promise<void> {
    this.tools = [];
    this.setEnabled(false);
  }

  /**
   * Enable plugin functionality
   */
  async onEnable(): Promise<void> {
    this.setEnabled(true);
    this.emit('enabled');
  }

  /**
   * Disable plugin functionality
   */
  async onDisable(): Promise<void> {
    this.setEnabled(false);
    this.emit('disabled');
  }

  /**
   * Get registered tools
   */
  getTools(): any[] {
    return this.tools;
  }

  /**
   * Check if OpenCode is available
   */
  isOpenCodeAvailable(): boolean {
    return this.opencodeAvailable;
  }

  /**
   * Get tool count
   */
  getToolCount(): number {
    return this.tools.length;
  }
}
