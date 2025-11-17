/**
 * BIP-32 HD Wallet Manager
 * 
 * Manages BIP-32/39/44 hierarchical deterministic wallets
 * Uses ethers.js (peer dependency)
 */

export interface BIP32WalletConfig {
  mnemonic?: string;
  path?: string; // BIP-32 path, e.g., "m/44'/60'/0'/0/0"
}

export interface WalletAddress {
  address: string;
  path: string;
  publicKey: string;
}

/**
 * BIP-32 HD Wallet Manager
 * 
 * Handles HD wallet operations using BIP-32/39/44
 */
export class BIP32Wallet {
  private mnemonic?: string;
  private defaultPath: string = "m/44'/60'/0'/0/0"; // Ethereum default
  private wallet?: any; // ethers.HDNodeWallet

  constructor(config: BIP32WalletConfig = {}) {
    this.mnemonic = config.mnemonic;
    if (config.path) {
      this.defaultPath = config.path;
    }
  }

  /**
   * Initialize wallet from mnemonic or generate new one
   */
  async initialize(): Promise<void> {
    // Dynamic import of ethers (peer dependency)
    let ethers: any;
    try {
      ethers = await import('ethers');
    } catch (error) {
      throw new Error('ethers package not installed. Install with: npm install ethers');
    }

    if (this.mnemonic) {
      // Initialize from existing mnemonic
      this.wallet = ethers.HDNodeWallet.fromPhrase(this.mnemonic);
    } else {
      // Generate new mnemonic
      this.wallet = ethers.Wallet.createRandom();
      this.mnemonic = this.wallet.mnemonic?.phrase;
    }
  }

  /**
   * Derive address from BIP-32 path
   */
  async deriveAddress(path?: string): Promise<WalletAddress> {
    if (!this.wallet) {
      await this.initialize();
    }

    const derivePath = path || this.defaultPath;
    const derivedWallet = this.wallet.derivePath(derivePath);

    return {
      address: derivedWallet.address,
      path: derivePath,
      publicKey: derivedWallet.publicKey
    };
  }

  /**
   * Sign message with private key
   */
  async signMessage(message: string | Uint8Array, path?: string): Promise<string> {
    if (!this.wallet) {
      await this.initialize();
    }

    const derivePath = path || this.defaultPath;
    const derivedWallet = this.wallet.derivePath(derivePath);

    return await derivedWallet.signMessage(message);
  }

  /**
   * Sign data (for MetaLogNode signing)
   */
  async signData(data: string | Uint8Array, path?: string): Promise<string> {
    if (!this.wallet) {
      await this.initialize();
    }

    const derivePath = path || this.defaultPath;
    const derivedWallet = this.wallet.derivePath(derivePath);

    // For MetaLogNode, we sign the CID
    const message = typeof data === 'string' ? data : new TextDecoder().decode(data);
    return await derivedWallet.signMessage(message);
  }

  /**
   * Get mnemonic (for backup)
   */
  getMnemonic(): string | undefined {
    return this.mnemonic;
  }

  /**
   * Get default path
   */
  getDefaultPath(): string {
    return this.defaultPath;
  }

  /**
   * Set default path
   */
  setDefaultPath(path: string): void {
    this.defaultPath = path;
  }
}

