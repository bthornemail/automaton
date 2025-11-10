/**
 * AgentProtection - ProLog-based agent protection for federated queries
 * 
 * Provides agent consent management and access control for private endpoints
 */

export class AgentProtection {
  constructor(metaLogBridge) {
    this.metaLog = metaLogBridge;
    this.consentRules = [];
  }

  /**
   * Initialize agent protection rules
   */
  async init() {
    // Add ProLog rules for consent checking
    const consentRules = [
      // Consent fact: consent(User, Endpoint, Granted)
      // Access rule: access allowed if consent exists
      'access(User, Endpoint, Query) :- consent(User, Endpoint, true).',
      // Deny by default if no consent
      'access(User, Endpoint, Query) :- not consent(User, Endpoint, true), fail.'
    ];

    for (const rule of consentRules) {
      await this.metaLog.addPrologRule(rule);
    }
  }

  /**
   * Grant consent for endpoint
   * @param {string} user - User identifier
   * @param {string} endpoint - Endpoint URI
   * @returns {Promise<void>}
   */
  async grantConsent(user, endpoint) {
    const fact = `consent(${user}, ${endpoint}, true).`;
    await this.metaLog.addPrologFact(fact);
    this.consentRules.push({ user, endpoint, granted: true });
  }

  /**
   * Revoke consent for endpoint
   * @param {string} user - User identifier
   * @param {string} endpoint - Endpoint URI
   * @returns {Promise<void>}
   */
  async revokeConsent(user, endpoint) {
    const fact = `consent(${user}, ${endpoint}, false).`;
    await this.metaLog.addPrologFact(fact);
    this.consentRules = this.consentRules.filter(
      r => !(r.user === user && r.endpoint === endpoint)
    );
  }

  /**
   * Check access permission
   * @param {string} user - User identifier
   * @param {string} endpoint - Endpoint URI
   * @param {string} query - Query string (optional)
   * @returns {Promise<boolean>} True if access allowed
   */
  async checkAccess(user, endpoint, query = '') {
    try {
      const accessQuery = `access(${user}, ${endpoint}, "${query}")`;
      const result = await this.metaLog.prologQuery(accessQuery);
      return result && result.length > 0;
    } catch (error) {
      console.warn('Access check failed:', error);
      return false; // Deny by default
    }
  }

  /**
   * Get consent status
   * @param {string} user - User identifier
   * @param {string} endpoint - Endpoint URI
   * @returns {Promise<boolean>} True if consent granted
   */
  async getConsentStatus(user, endpoint) {
    try {
      const consentQuery = `consent(${user}, ${endpoint}, true)`;
      const result = await this.metaLog.prologQuery(consentQuery);
      return result && result.length > 0;
    } catch (error) {
      return false;
    }
  }
}
