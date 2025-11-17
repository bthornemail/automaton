/**
 * W3C/WebAPI Macro System
 * Type-based resolution functors for Web APIs
 */

import type { WebAPIMacro, MacroConfig, ResolutionFunctor as IResolutionFunctor, Schema } from './types.js';
import { ResolutionFunctor } from './types.js';

// ========================================
// BASE MACRO IMPLEMENTATIONS
// ========================================

/**
 * Geolocation API Macro
 */
export class GeolocationMacro implements WebAPIMacro {
  keyword = "location";
  api = "geolocation";
  method = "getCurrentPosition";
  params = { enableHighAccuracy: true };
  type: [string, string] = ["web_api", "geolocation"];

  async execute(): Promise<GeolocationPosition> {
    return new Promise((resolve, reject) => {
      if (!navigator.geolocation) {
        reject(new Error('Geolocation not supported'));
        return;
      }
      navigator.geolocation.getCurrentPosition(resolve, reject, this.params);
    });
  }
}

/**
 * Notifications API Macro
 */
export class NotificationMacro implements WebAPIMacro {
  keyword = "notify";
  api = "notifications";
  method = "showNotification";
  params: Record<string, any>;
  type: [string, string] = ["web_api", "notifications"];

  constructor(params: { title: string; body: string; icon?: string }) {
    this.params = params;
  }

  async execute(): Promise<void> {
    if (!('Notification' in window)) {
      throw new Error('Notifications not supported');
    }

    const permission = await Notification.requestPermission();
    if (permission === "granted") {
      new Notification(this.params.title, {
        body: this.params.body,
        icon: this.params.icon
      });
    } else {
      throw new Error(`Notification permission ${permission}`);
    }
  }
}

/**
 * Clipboard API Macro
 */
export class ClipboardMacro implements WebAPIMacro {
  keyword = "copy";
  api = "clipboard";
  method = "writeText";
  params: Record<string, any>;
  type: [string, string] = ["web_api", "clipboard"];

  constructor(params: { text: string }) {
    this.params = params;
  }

  async execute(): Promise<void> {
    if (!navigator.clipboard) {
      throw new Error('Clipboard API not supported');
    }
    await navigator.clipboard.writeText(this.params.text);
  }
}

/**
 * IndexedDB Storage Macro
 */
export class StorageMacro implements WebAPIMacro {
  keyword = "save";
  api = "indexeddb";
  method = "put";
  params: Record<string, any>;
  type: [string, string] = ["web_api", "indexeddb"];

  constructor(params: { store: string; key: string; value?: any }) {
    this.params = params;
  }

  async execute(): Promise<void> {
    return new Promise((resolve, reject) => {
      const request = indexedDB.open("CANVASL", 1);

      request.onupgradeneeded = (event: any) => {
        const db = event.target.result;
        if (!db.objectStoreNames.contains(this.params.store)) {
          db.createObjectStore(this.params.store);
        }
      };

      request.onsuccess = (event: any) => {
        const db = event.target.result;
        const tx = db.transaction(this.params.store, "readwrite");
        const store = tx.objectStore(this.params.store);
        const value = this.params.value || { timestamp: Date.now() };
        store.put(value, this.params.key);
        tx.oncomplete = () => resolve();
        tx.onerror = () => reject(tx.error);
      };

      request.onerror = () => reject(request.error);
    });
  }
}

/**
 * Media Devices Macro (camera/microphone)
 */
export class MediaMacro implements WebAPIMacro {
  keyword = "camera";
  api = "mediadevices";
  method = "getUserMedia";
  params: MediaStreamConstraints;
  type: [string, string] = ["web_api", "mediadevices"];

  constructor(params: MediaStreamConstraints) {
    this.params = params;
  }

  async execute(): Promise<MediaStream> {
    if (!navigator.mediaDevices) {
      throw new Error('Media Devices API not supported');
    }
    return await navigator.mediaDevices.getUserMedia(this.params);
  }
}

/**
 * Battery Status Macro
 */
export class BatteryMacro implements WebAPIMacro {
  keyword = "battery";
  api = "battery";
  method = "getBattery";
  params = {};
  type: [string, string] = ["web_api", "battery"];

  async execute(): Promise<any> {
    if (!('getBattery' in navigator)) {
      throw new Error('Battery Status API not supported');
    }
    return await (navigator as any).getBattery();
  }
}

/**
 * Vibration API Macro
 */
export class VibrationMacro implements WebAPIMacro {
  keyword = "vibrate";
  api = "vibration";
  method = "vibrate";
  params: Record<string, any>;
  type: [string, string] = ["web_api", "vibration"];

  constructor(params: { pattern: number | number[] }) {
    this.params = params;
  }

  async execute(): Promise<boolean> {
    if (!navigator.vibrate) {
      throw new Error('Vibration API not supported');
    }
    return navigator.vibrate(this.params.pattern);
  }
}

// ========================================
// MACRO REGISTRY
// ========================================

export class MacroRegistry extends ResolutionFunctor {
  private macros = new Map<string, WebAPIMacro>();

  constructor() {
    super("web_api");
  }

  register(macro: WebAPIMacro) {
    this.macros.set(macro.keyword, macro);
    console.log(`[MacroRegistry] Registered macro: ${macro.keyword} -> ${macro.api}.${macro.method}`);
  }

  async resolve(
    U: Set<string>,
    identifier: string,
    keyword: string
  ): Promise<any> {
    const macro = this.macros.get(keyword);
    if (!macro) {
      throw new Error(`No macro registered for keyword: ${keyword}`);
    }

    console.log(`[MacroRegistry] Resolving ${keyword} via ${macro.api}`);
    const result = await macro.execute();
    return { value: result, domain: U, source: macro.api };
  }

  restrict(value: any, fromDomain: Set<string>, toDomain: Set<string>): any {
    // Web API results are global, restriction is identity
    return { ...value, domain: toDomain };
  }

  hasMacro(keyword: string): boolean {
    return this.macros.has(keyword);
  }

  getMacro(keyword: string): WebAPIMacro | undefined {
    return this.macros.get(keyword);
  }

  getAllMacros(): WebAPIMacro[] {
    return Array.from(this.macros.values());
  }
}

// ========================================
// MACRO FACTORY
// ========================================

/**
 * Create a macro from configuration
 */
export function createMacro(config: MacroConfig): WebAPIMacro | null {
  // Check if it's an IDE-specific macro
  if (config.api === 'ide_command' || config.api === 'keyboard' ||
      config.api === 'terminal' || config.api === 'git' || config.api === 'npm') {
    // Try to load IDE macros dynamically
    try {
      const { createIDEMacro } = require('./ide-macros.js');
      return createIDEMacro(config);
    } catch (error) {
      console.warn('[MacroFactory] IDE macros not available, falling back to clipboard');
      // Fallback: use clipboard for IDE commands
      return new ClipboardMacro(config.params);
    }
  }

  // Standard Web API macros
  switch (config.api) {
    case "geolocation":
      return new GeolocationMacro();

    case "notifications":
      return new NotificationMacro(config.params);

    case "clipboard":
      return new ClipboardMacro(config.params);

    case "indexeddb":
      return new StorageMacro(config.params);

    case "mediadevices":
      return new MediaMacro(config.params);

    case "battery":
      return new BatteryMacro();

    case "vibration":
      return new VibrationMacro(config.params);

    default:
      console.warn(`[MacroFactory] Unknown API: ${config.api}`);
      return null;
  }
}

// ========================================
// RESOLUTION REGISTRY
// ========================================

export class ResolutionRegistry {
  private functors = new Map<Schema, IResolutionFunctor>();

  register(schema: Schema, functor: IResolutionFunctor) {
    this.functors.set(schema, functor);
    console.log(`[ResolutionRegistry] Registered functor for schema: ${schema}`);
  }

  async resolve(
    U: Set<string>,
    type: [Schema, string],
    keyword: string
  ): Promise<any> {
    const [schema, identifier] = type;
    const functor = this.functors.get(schema);

    if (!functor) {
      throw new Error(`No functor registered for schema: ${schema}`);
    }

    return await functor.resolve(U, identifier, keyword);
  }

  restrict(
    value: any,
    schema: Schema,
    fromDomain: Set<string>,
    toDomain: Set<string>
  ): any {
    const functor = this.functors.get(schema);
    if (!functor) {
      throw new Error(`No functor for schema: ${schema}`);
    }
    return functor.restrict(value, fromDomain, toDomain);
  }

  hasFunctor(schema: Schema): boolean {
    return this.functors.has(schema);
  }
}
