/**
 * A₈: BIP-32 Keymaster Automaton
 * 
 * Role: HD Wallet Management
 * Uses BIP32Wallet for key derivation and signing
 */

import { BaseAutomaton } from './types.js';
import type { AutomatonId, SwarmContext, AutomatonMessage } from './types.js';
import { BIP32Wallet, type BIP32WalletConfig } from '../identity/bip32-wallet.js';

export interface A8BIP32KeymasterState {
  addresses: Map<string, any>; // path -> address info
  defaultPath: string;
  initialized: boolean;
}

/**
 * A₈: BIP-32 Keymaster Automaton
 * 
 * Manages hierarchical deterministic wallet operations
 */
export class A8_BIP32Keymaster extends BaseAutomaton {
  readonly id: 8 = 8;
  readonly name = 'A₈ BIP-32 Keymaster';
  readonly role = 'HD Wallet Management';

  state: A8BIP32KeymasterState = {
    addresses: new Map(),
    defaultPath: "m/44'/60'/0'/0/0",
    initialized: false
  };

  private wallet?: BIP32Wallet;

  constructor(config?: BIP32WalletConfig) {
    super();
    this.wallet = new BIP32Wallet(config);
    if (config?.path) {
      this.state.defaultPath = config.path;
    }
  }

  async tick(swarm: SwarmContext): Promise<void> {
    if (!this.state.initialized) {
      await this.initialize(swarm);
    }

    // Wallet operations are request-driven, no ongoing tick operations
  }

  private async initialize(swarm: SwarmContext): Promise<void> {
    try {
      await this.wallet?.initialize();
      
      // Derive default address
      if (this.wallet) {
        const address = await this.wallet.deriveAddress(this.state.defaultPath);
        this.state.addresses.set(this.state.defaultPath, address);
      }

      this.state.initialized = true;
      console.log('A₈: BIP-32 Keymaster initialized');
    } catch (error) {
      console.error('A₈: Error initializing wallet:', error);
      // Continue without wallet if ethers not available
      this.state.initialized = true;
    }
  }

  /**
   * Derive address from BIP-32 path
   */
  async deriveAddress(path?: string): Promise<any> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized');
    }

    const derivePath = path || this.state.defaultPath;
    const address = await this.wallet.deriveAddress(derivePath);
    
    this.state.addresses.set(derivePath, address);
    return address;
  }

  /**
   * Sign message/data
   */
  async signMessage(message: string | Uint8Array, path?: string): Promise<string> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized');
    }

    return await this.wallet.signMessage(message, path || this.state.defaultPath);
  }

  /**
   * Sign data for MetaLogNode
   */
  async signData(data: string | Uint8Array, path?: string): Promise<string> {
    if (!this.wallet) {
      throw new Error('Wallet not initialized');
    }

    return await this.wallet.signData(data, path || this.state.defaultPath);
  }

  /**
   * Get mnemonic (for backup)
   */
  getMnemonic(): string | undefined {
    return this.wallet?.getMnemonic();
  }

  /**
   * Get default path
   */
  getDefaultPath(): string {
    return this.state.defaultPath;
  }

  /**
   * Set default path
   */
  setDefaultPath(path: string): void {
    this.state.defaultPath = path;
    this.wallet?.setDefaultPath(path);
  }

  async receive(from: AutomatonId, message: AutomatonMessage): Promise<void> {
    if (message.type === 'derive-address') {
      const { path } = message.payload || {};
      await this.deriveAddress(path);
    } else if (message.type === 'sign-message') {
      const { message: msg, path } = message.payload || {};
      if (msg) {
        await this.signMessage(msg, path);
      }
    } else if (message.type === 'sign-data') {
      const { data, path } = message.payload || {};
      if (data) {
        await this.signData(data, path);
      }
    }
  }

  /**
   * Get all addresses
   */
  getAddresses(): Map<string, any> {
    return this.state.addresses;
  }
}

