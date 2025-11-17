/**
 * CANVASL Speech Input/Output System
 * Main exports for voice-controlled applications
 */

// Types
export * from './types.js';

// Speech Handlers
export {
  SpeechRecognitionHandler,
  SpeechSynthesisHandler,
  isSpeechRecognitionSupported,
  isSpeechSynthesisSupported,
  getAvailableVoices,
  waitForVoices
} from './speech-handlers.js';

// Web API Macros
export {
  GeolocationMacro,
  NotificationMacro,
  ClipboardMacro,
  StorageMacro,
  MediaMacro,
  BatteryMacro,
  VibrationMacro,
  MacroRegistry,
  ResolutionRegistry,
  createMacro
} from './web-api-macros.js';

// Template Compiler
export {
  TemplateCompiler,
  HomologyComputer,
  computeAllBetti,
  eulerCharacteristic
} from './template-compiler.js';

// Voice App Runtime
export {
  CANVASLVoiceApp,
  CANVASLAppManager
} from './canvasl-voice-app.js';

// IDE Macros
export {
  IDECommandMacro,
  KeyboardShortcutMacro,
  TerminalCommandMacro,
  GitCommandMacro,
  NPMCommandMacro,
  createIDEMacro,
  COMMON_IDE_COMMANDS,
  COMMON_GIT_COMMANDS,
  COMMON_NPM_COMMANDS
} from './ide-macros.js';
