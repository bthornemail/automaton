/**
 * Web Speech API Integration
 * Provides speech recognition and synthesis handlers
 */

import type { SpeechConfig, SpeechRecognitionHandler as ISpeechRecognitionHandler, SpeechSynthesisHandler as ISpeechSynthesisHandler } from './types.js';

// ========================================
// SPEECH RECOGNITION
// ========================================

export class SpeechRecognitionHandler implements ISpeechRecognitionHandler {
  private recognition: SpeechRecognition | null = null;
  private keywords: Set<string>;
  private onKeywordDetected: (keyword: string, transcript: string) => void;

  constructor(
    config: SpeechConfig['input'],
    onKeywordDetected: (keyword: string, transcript: string) => void
  ) {
    this.keywords = new Set(config.keywords);
    this.onKeywordDetected = onKeywordDetected;

    // Initialize Web Speech API
    const SpeechRecognition = (window as any).SpeechRecognition ||
                             (window as any).webkitSpeechRecognition;

    if (!SpeechRecognition) {
      console.warn("Web Speech API not supported in this browser");
      return;
    }

    this.recognition = new SpeechRecognition();
    this.recognition.lang = config.lang;
    this.recognition.continuous = config.continuous;
    this.recognition.interimResults = config.interimResults;

    this.setupHandlers();
  }

  private setupHandlers() {
    if (!this.recognition) return;

    this.recognition.onresult = (event: SpeechRecognitionEvent) => {
      const last = event.results.length - 1;
      const transcript = event.results[last][0].transcript.toLowerCase();

      console.log('[SpeechRecognition] Transcript:', transcript);

      // Check for keyword matches
      for (const keyword of this.keywords) {
        if (transcript.includes(keyword.toLowerCase())) {
          console.log('[SpeechRecognition] Keyword detected:', keyword);
          this.onKeywordDetected(keyword, transcript);
        }
      }
    };

    this.recognition.onerror = (event: any) => {
      console.error("[SpeechRecognition] Error:", event.error);
    };

    this.recognition.onend = () => {
      console.log('[SpeechRecognition] Recognition ended');
      if (this.recognition?.continuous) {
        // Restart for continuous listening
        try {
          this.recognition.start();
        } catch (e) {
          // Ignore if already started
        }
      }
    };

    this.recognition.onstart = () => {
      console.log('[SpeechRecognition] Recognition started');
    };
  }

  start() {
    if (!this.recognition) {
      console.warn('[SpeechRecognition] Cannot start: Web Speech API not available');
      return;
    }

    try {
      this.recognition.start();
      console.log('[SpeechRecognition] Started listening for keywords:', Array.from(this.keywords));
    } catch (e) {
      console.error('[SpeechRecognition] Error starting:', e);
    }
  }

  stop() {
    if (!this.recognition) return;

    try {
      this.recognition.stop();
      console.log('[SpeechRecognition] Stopped');
    } catch (e) {
      console.error('[SpeechRecognition] Error stopping:', e);
    }
  }
}

// ========================================
// SPEECH SYNTHESIS
// ========================================

export class SpeechSynthesisHandler implements ISpeechSynthesisHandler {
  private config: SpeechConfig['output'];
  private voicesLoaded = false;

  constructor(config: SpeechConfig['output']) {
    this.config = config;

    // Wait for voices to load
    if (speechSynthesis.getVoices().length === 0) {
      speechSynthesis.addEventListener('voiceschanged', () => {
        this.voicesLoaded = true;
        console.log('[SpeechSynthesis] Voices loaded:', speechSynthesis.getVoices().length);
      });
    } else {
      this.voicesLoaded = true;
    }
  }

  async speak(text: string): Promise<void> {
    return new Promise((resolve, reject) => {
      const utterance = new SpeechSynthesisUtterance(text);

      // Find matching voice
      const voices = speechSynthesis.getVoices();
      const voice = voices.find(v => v.name.includes(this.config.voice)) || voices[0];
      if (voice) utterance.voice = voice;

      utterance.rate = this.config.rate;
      utterance.pitch = this.config.pitch;

      utterance.onend = () => {
        console.log('[SpeechSynthesis] Finished speaking:', text);
        resolve();
      };

      utterance.onerror = (e) => {
        console.error('[SpeechSynthesis] Error:', e);
        reject(e);
      };

      console.log('[SpeechSynthesis] Speaking:', text);
      speechSynthesis.speak(utterance);
    });
  }

  cancel() {
    speechSynthesis.cancel();
    console.log('[SpeechSynthesis] Cancelled');
  }
}

// ========================================
// UTILITY FUNCTIONS
// ========================================

/**
 * Check if Web Speech API is supported
 */
export function isSpeechRecognitionSupported(): boolean {
  return !!(window as any).SpeechRecognition || !!(window as any).webkitSpeechRecognition;
}

/**
 * Check if Speech Synthesis is supported
 */
export function isSpeechSynthesisSupported(): boolean {
  return 'speechSynthesis' in window;
}

/**
 * Get available voices
 */
export function getAvailableVoices(): SpeechSynthesisVoice[] {
  return speechSynthesis.getVoices();
}

/**
 * Wait for voices to load
 */
export function waitForVoices(): Promise<SpeechSynthesisVoice[]> {
  return new Promise((resolve) => {
    const voices = speechSynthesis.getVoices();
    if (voices.length > 0) {
      resolve(voices);
    } else {
      speechSynthesis.addEventListener('voiceschanged', () => {
        resolve(speechSynthesis.getVoices());
      }, { once: true });
    }
  });
}
