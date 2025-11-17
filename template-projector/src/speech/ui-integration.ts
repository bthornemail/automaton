/**
 * UI Integration for CANVASL Voice Apps
 * Provides browser UI controls and visualization
 */

import { CANVASLVoiceApp, CANVASLAppManager } from './canvasl-voice-app.js';
import type { CANVASLTemplate } from './types.js';
import { isSpeechRecognitionSupported, isSpeechSynthesisSupported } from './speech-handlers.js';

// ========================================
// UI CONTROLLER
// ========================================

export class VoiceAppUIController {
  private appManager: CANVASLAppManager;
  private currentAppId: string | null = null;
  private container: HTMLElement;

  constructor(containerElement: HTMLElement) {
    this.appManager = new CANVASLAppManager();
    this.container = containerElement;
    this.renderUI();
    this.checkBrowserSupport();
  }

  /**
   * Check browser support for required APIs
   */
  private checkBrowserSupport() {
    const status = {
      speechRecognition: isSpeechRecognitionSupported(),
      speechSynthesis: isSpeechSynthesisSupported(),
      geolocation: 'geolocation' in navigator,
      notifications: 'Notification' in window,
      clipboard: 'clipboard' in navigator,
      indexedDB: 'indexedDB' in window
    };

    console.log('[VoiceAppUI] Browser support:', status);

    if (!status.speechRecognition || !status.speechSynthesis) {
      this.showWarning('Speech APIs not fully supported in this browser');
    }
  }

  /**
   * Render UI controls
   */
  private renderUI() {
    this.container.innerHTML = `
      <div class="voice-app-ui">
        <div class="voice-app-header">
          <h2>🎤 CANVASL Voice Control</h2>
          <div class="status-indicator" id="voice-status">●</div>
        </div>

        <div class="voice-app-controls">
          <button id="start-btn" class="btn btn-primary">Start Listening</button>
          <button id="stop-btn" class="btn btn-secondary" disabled>Stop</button>
          <button id="load-template-btn" class="btn btn-tertiary">Load Template</button>
        </div>

        <div class="voice-app-info">
          <div class="info-section">
            <h3>Available Keywords</h3>
            <div id="keywords-list" class="keywords-list"></div>
          </div>

          <div class="info-section">
            <h3>Execution History</h3>
            <div id="execution-history" class="execution-history"></div>
          </div>

          <div class="info-section">
            <h3>Chain Complex State</h3>
            <div id="chain-state" class="chain-state"></div>
          </div>
        </div>

        <div id="warning-box" class="warning-box" style="display: none;"></div>
      </div>
    `;

    this.attachEventListeners();
  }

  /**
   * Attach event listeners to UI controls
   */
  private attachEventListeners() {
    const startBtn = document.getElementById('start-btn');
    const stopBtn = document.getElementById('stop-btn');
    const loadTemplateBtn = document.getElementById('load-template-btn');

    startBtn?.addEventListener('click', () => this.startApp());
    stopBtn?.addEventListener('click', () => this.stopApp());
    loadTemplateBtn?.addEventListener('click', () => this.loadTemplate());
  }

  /**
   * Load a template from file or fetch
   */
  async loadTemplate(templatePath?: string) {
    try {
      // Default to voice-demo template
      const path = templatePath || '/templates/voice-demo.md';
      const response = await fetch(path);
      const content = await response.text();

      const template = this.parseTemplate(content);
      const appId = this.appManager.createApp(template);
      this.currentAppId = appId;

      this.updateKeywordsList(template);
      this.showStatus('Template loaded: ' + template.id, 'success');

    } catch (error: any) {
      console.error('[VoiceAppUI] Error loading template:', error);
      this.showStatus('Error loading template: ' + error.message, 'error');
    }
  }

  /**
   * Parse template from markdown content
   */
  private parseTemplate(content: string): CANVASLTemplate {
    // Split frontmatter and body
    const parts = content.split(/^---$/m);

    if (parts.length < 3) {
      throw new Error('Invalid template format');
    }

    // Parse YAML frontmatter (simplified)
    const frontmatter = this.parseYAML(parts[1]);
    const body = parts.slice(2).join('---').trim();

    return {
      id: frontmatter.id || 'voice-app-' + Date.now(),
      type: 'node',
      dimension: 2,
      frontmatter: {
        type: 'canvasl-template',
        ...frontmatter
      },
      body
    };
  }

  /**
   * Simple YAML parser
   */
  private parseYAML(yaml: string): any {
    const result: any = {};
    const lines = yaml.trim().split('\n');
    let currentKey = '';
    let currentArray: any[] = [];
    let inArray = false;

    for (const line of lines) {
      const trimmed = line.trim();
      if (!trimmed || trimmed.startsWith('#')) continue;

      if (trimmed.startsWith('- ')) {
        // Array item
        const item = trimmed.substring(2);
        if (item.includes(':')) {
          // Object in array
          const obj: any = {};
          const pairs = item.split(',');
          for (const pair of pairs) {
            const [k, v] = pair.split(':').map(s => s.trim());
            obj[k] = this.parseValue(v);
          }
          currentArray.push(obj);
        } else {
          currentArray.push(this.parseValue(item));
        }
        inArray = true;
      } else if (trimmed.includes(':')) {
        // Save previous array
        if (inArray && currentKey) {
          result[currentKey] = currentArray;
          currentArray = [];
          inArray = false;
        }

        const [key, ...valueParts] = trimmed.split(':');
        const value = valueParts.join(':').trim();
        currentKey = key.trim();

        if (value) {
          result[currentKey] = this.parseValue(value);
        } else {
          result[currentKey] = {};
        }
      }
    }

    // Save final array
    if (inArray && currentKey) {
      result[currentKey] = currentArray;
    }

    return result;
  }

  /**
   * Parse YAML value
   */
  private parseValue(value: string): any {
    if (value === 'true') return true;
    if (value === 'false') return false;
    if (/^-?\d+\.?\d*$/.test(value)) return parseFloat(value);
    if (value.startsWith('[') && value.endsWith(']')) {
      return JSON.parse(value);
    }
    return value.replace(/^["']|["']$/g, '');
  }

  /**
   * Start the voice app
   */
  async startApp() {
    if (!this.currentAppId) {
      await this.loadTemplate();
    }

    if (!this.currentAppId) {
      this.showStatus('No template loaded', 'error');
      return;
    }

    try {
      this.appManager.startApp(this.currentAppId);
      this.updateUIState(true);
      this.showStatus('Listening...', 'active');
      this.startHistoryMonitor();
    } catch (error: any) {
      console.error('[VoiceAppUI] Error starting app:', error);
      this.showStatus('Error: ' + error.message, 'error');
    }
  }

  /**
   * Stop the voice app
   */
  stopApp() {
    if (!this.currentAppId) return;

    try {
      this.appManager.stopApp(this.currentAppId);
      this.updateUIState(false);
      this.showStatus('Stopped', 'inactive');
    } catch (error: any) {
      console.error('[VoiceAppUI] Error stopping app:', error);
    }
  }

  /**
   * Update UI state based on app running status
   */
  private updateUIState(running: boolean) {
    const startBtn = document.getElementById('start-btn') as HTMLButtonElement;
    const stopBtn = document.getElementById('stop-btn') as HTMLButtonElement;
    const statusIndicator = document.getElementById('voice-status');

    if (startBtn) startBtn.disabled = running;
    if (stopBtn) stopBtn.disabled = !running;

    if (statusIndicator) {
      statusIndicator.className = running ? 'status-indicator active' : 'status-indicator';
    }
  }

  /**
   * Update keywords list display
   */
  private updateKeywordsList(template: CANVASLTemplate) {
    const keywordsList = document.getElementById('keywords-list');
    if (!keywordsList) return;

    const keywords = template.frontmatter.speech?.input?.keywords || [];
    keywordsList.innerHTML = keywords
      .map(kw => `<span class="keyword-badge">${kw}</span>`)
      .join('');
  }

  /**
   * Monitor execution history
   */
  private startHistoryMonitor() {
    setInterval(() => {
      this.updateExecutionHistory();
      this.updateChainState();
    }, 1000);
  }

  /**
   * Update execution history display
   */
  private updateExecutionHistory() {
    if (!this.currentAppId) return;

    const app = this.appManager.getApp(this.currentAppId);
    if (!app) return;

    const history = document.getElementById('execution-history');
    if (!history) return;

    const complex = app.getChainComplex();
    const executions = complex.C4.slice(-5).reverse(); // Last 5 executions

    history.innerHTML = executions.length > 0
      ? executions.map(ctx => {
          const [keyword, , timestamp] = ctx.lists[0] || [];
          const time = timestamp ? new Date(timestamp).toLocaleTimeString() : '';
          return `<div class="execution-item">${time} - ${keyword}</div>`;
        }).join('')
      : '<div class="no-executions">No executions yet</div>';
  }

  /**
   * Update chain complex state display
   */
  private updateChainState() {
    if (!this.currentAppId) return;

    const app = this.appManager.getApp(this.currentAppId);
    if (!app) return;

    const chainState = document.getElementById('chain-state');
    if (!chainState) return;

    const summary = app.getSummary();

    chainState.innerHTML = `
      <div class="chain-metric">Betti numbers: [${summary.betti.join(', ')}]</div>
      <div class="chain-metric">Euler characteristic: ${summary.euler}</div>
      <div class="chain-metric">Executions: ${summary.executionCount}</div>
    `;
  }

  /**
   * Show status message
   */
  private showStatus(message: string, type: 'active' | 'inactive' | 'success' | 'error') {
    console.log(`[VoiceAppUI] ${type}: ${message}`);
    // Could add a status bar to the UI
  }

  /**
   * Show warning message
   */
  private showWarning(message: string) {
    const warningBox = document.getElementById('warning-box');
    if (warningBox) {
      warningBox.textContent = message;
      warningBox.style.display = 'block';
    }
  }
}

// ========================================
// INITIALIZE UI
// ========================================

/**
 * Initialize voice app UI on page load
 */
export function initializeVoiceUI(containerId = 'voice-app-container'): VoiceAppUIController {
  const container = document.getElementById(containerId);

  if (!container) {
    throw new Error(`Container element #${containerId} not found`);
  }

  return new VoiceAppUIController(container);
}
