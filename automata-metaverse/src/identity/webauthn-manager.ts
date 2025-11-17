/**
 * WebAuthn Manager
 * 
 * Manages WebAuthn credential creation and verification
 * Browser-only API
 */

export interface WebAuthnCredential {
  id: string;
  rawId: ArrayBuffer;
  response: AuthenticatorAttestationResponse | AuthenticatorAssertionResponse;
  type: string;
}

export interface WebAuthnManagerConfig {
  rpId?: string;
  rpName?: string;
  userDisplayName?: string;
}

/**
 * WebAuthn Manager
 * 
 * Handles WebAuthn credential creation and verification
 */
export class WebAuthnManager {
  private rpId: string;
  private rpName: string;
  private userDisplayName: string;

  constructor(config: WebAuthnManagerConfig = {}) {
    this.rpId = config.rpId || window.location.hostname;
    this.rpName = config.rpName || 'Automata Metaverse';
    this.userDisplayName = config.userDisplayName || 'User';
  }

  /**
   * Check if WebAuthn is supported
   */
  isSupported(): boolean {
    return typeof window !== 'undefined' && 
           'PublicKeyCredential' in window &&
           typeof PublicKeyCredential !== 'undefined';
  }

  /**
   * Create a new WebAuthn credential
   */
  async createCredential(userId: string, userName: string): Promise<WebAuthnCredential> {
    if (!this.isSupported()) {
      throw new Error('WebAuthn is not supported in this environment');
    }

    const publicKeyCredentialCreationOptions: PublicKeyCredentialCreationOptions = {
      challenge: this.generateChallenge(),
      rp: {
        name: this.rpName,
        id: this.rpId
      },
      user: {
        id: new TextEncoder().encode(userId),
        name: userName,
        displayName: this.userDisplayName
      },
      pubKeyCredParams: [
        { alg: -7, type: 'public-key' }, // ES256
        { alg: -257, type: 'public-key' } // RS256
      ],
      authenticatorSelection: {
        authenticatorAttachment: 'platform',
        userVerification: 'preferred'
      },
      timeout: 60000,
      attestation: 'direct'
    };

    try {
      const credential = await navigator.credentials.create({
        publicKey: publicKeyCredentialCreationOptions
      }) as PublicKeyCredential;

      return {
        id: credential.id,
        rawId: credential.rawId,
        response: credential.response as AuthenticatorAttestationResponse,
        type: credential.type
      };
    } catch (error) {
      console.error('WebAuthn: Error creating credential:', error);
      throw error;
    }
  }

  /**
   * Verify WebAuthn assertion
   */
  async verifyAssertion(
    credentialId: string,
    challenge: ArrayBuffer,
    allowCredentials?: PublicKeyCredentialDescriptor[]
  ): Promise<AuthenticatorAssertionResponse> {
    if (!this.isSupported()) {
      throw new Error('WebAuthn is not supported in this environment');
    }

    const publicKeyCredentialRequestOptions: PublicKeyCredentialRequestOptions = {
      challenge,
      allowCredentials: allowCredentials || [{
        id: this.base64ToArrayBuffer(credentialId),
        type: 'public-key',
        transports: ['internal', 'usb', 'nfc', 'ble']
      }],
      timeout: 60000,
      userVerification: 'preferred',
      rpId: this.rpId
    };

    try {
      const assertion = await navigator.credentials.get({
        publicKey: publicKeyCredentialRequestOptions
      }) as PublicKeyCredential;

      return assertion.response as AuthenticatorAssertionResponse;
    } catch (error) {
      console.error('WebAuthn: Error verifying assertion:', error);
      throw error;
    }
  }

  /**
   * Generate random challenge
   */
  private generateChallenge(): ArrayBuffer {
    const array = new Uint8Array(32);
    crypto.getRandomValues(array);
    return array.buffer;
  }

  /**
   * Convert base64 to ArrayBuffer
   */
  private base64ToArrayBuffer(base64: string): ArrayBuffer {
    const binaryString = atob(base64);
    const bytes = new Uint8Array(binaryString.length);
    for (let i = 0; i < binaryString.length; i++) {
      bytes[i] = binaryString.charCodeAt(i);
    }
    return bytes.buffer;
  }

  /**
   * Convert ArrayBuffer to base64
   */
  private arrayBufferToBase64(buffer: ArrayBuffer): string {
    const bytes = new Uint8Array(buffer);
    let binary = '';
    for (let i = 0; i < bytes.byteLength; i++) {
      binary += String.fromCharCode(bytes[i]);
    }
    return btoa(binary);
  }
}

