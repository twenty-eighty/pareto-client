// Contact storage using NDK (Nostr Development Kit)
// ES6 module version for clean imports

import { NDKEvent, NDKKind } from "@nostr-dev-kit/ndk";
import { HttpClient } from "./EncryptedContacts/http.js";
import { ContactsApi } from "./EncryptedContacts/contacts.js";

const API_URL = 'http://localhost:4003';

// =============================================================================
// CONTACTS CLASS
// =============================================================================

export class Contacts {
  constructor(ndk, pubkey, apiUrl = API_URL) {
    console.log('Contacts constructor called with:', { ndk, pubkey, apiUrl });
    this.ndk = ndk;
    this.apiUrl = apiUrl;
    this.pubkey = pubkey;

    // Initialize HTTP client and Contacts API wrapper (token provided after authenticate)
    this.http = new HttpClient({
      baseUrl: this.apiUrl,
      getAuthToken: async () => this.jwt
    });
    this.contactsApi = new ContactsApi(this.http);
  }

  // =============================================================================
  // AUTHENTICATION
  // =============================================================================

  /**
   * Authenticate with NIP-98 using NDK
   */
  async authenticate() {
    try {
      // 1. Get challenge from server
      const challengeResponse = await fetch(`${API_URL}/api/auth/challenge`);
      if (!challengeResponse.ok) {
        throw new Error('Failed to get challenge');
      }
      
      const challengeData = await challengeResponse.json();
      const challenge = challengeData.challenge;
      // salt to be stored in database for user when created
      const salt = await this.generateSalt();
      
      // 2. Create NIP-42 event
      const loginUrl = `${API_URL}/api/auth/login`;
      const nip42Event = await this.createNIP42Event(loginUrl, challenge);
      
      // 3. Send authentication request

      const encodedEvent = btoa(JSON.stringify(nip42Event.rawEvent()));
      const authHeader = `Bearer Nostr ${encodedEvent}`;
      
      const loginResponse = await fetch(loginUrl + '?salt=' + salt, {
        method: 'POST',
        headers: {
          'Authorization': authHeader,
          'Content-Type': 'application/json'
        }
      });
      
      if (!loginResponse.ok) {
        throw new Error('Authentication failed');
      }
      
      const loginData = await loginResponse.json();
      
      // Store authentication data
      this.jwt = loginData.token;
      this.pubkey = nip42Event.pubkey;
      this.salt = await this.decodeSalt(loginData.salt);
      
      return {
        success: true,
        jwt: loginData.token,
        pubkey: nip42Event.pubkey
      };
      
    } catch (error) {
      return {
        success: false,
        error: error.message
      };
    }
  }

  async generateSalt() {
    // use NIP-44 to encrypt salt to be stored in database along with user
    const randomString = this.generateRandomString(); // 32 character hex string
    const encrypted = btoa(await this.ndk.signer.encrypt({ pubkey: this.pubkey }, randomString, 'nip44'));
    return encrypted;
  }


  async decodeSalt(encryptedSalt) {
    return await this.ndk.signer.decrypt({ pubkey: this.pubkey }, atob(encryptedSalt), 'nip44');
  }

  generateRandomString(length = 32) {
    const array = new Uint8Array(length);
    crypto.getRandomValues(array);
    return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('');
    }

  /**
   * Create and sign a NIP-98 event using NDK
   */
  async createNIP42Event(url, challenge) {
    const event = new NDKEvent(window.ndk, {
      content: "",
      kind: 22242,
      tags: [
        ["server", url],
        ["challenge", challenge],
      ],
    });

    await event.sign();
    return event;
  }

  // =============================================================================
  // ENCRYPTION & HASHING
  // =============================================================================

  /**
   * Encrypt contact data using NDK's NIP-44 implementation
   */
  async encryptContact(contactData, recipientPubkey) {
    // Convert contact data to JSON string
    const jsonData = JSON.stringify(contactData);
    
    // Use NDK's NIP-44 encryption
    const encryptedData = await this.ndk.signer.encrypt({ pubkey: recipientPubkey }, jsonData, 'nip44');
    
    return encryptedData;
  }

  /**
   * Decrypt contact data using NDK's NIP-44 implementation
   */
  async decryptContact(encryptedData, senderPubkey) {
    // Use NDK's NIP-44 decryption
    const decryptedData = await this.ndk.signer.decrypt({ pubkey: senderPubkey }, encryptedData, 'nip44');
    
    return JSON.parse(decryptedData);
  }

  /**
   * Decrypt tag data using NDK's NIP-44 implementation
   */
  async decryptTag(encryptedData, senderPubkey) {
    // Use NDK's NIP-44 decryption
    const decryptedData = await this.ndk.signer.decrypt({ pubkey: senderPubkey }, encryptedData, 'nip44');
    
    return decryptedData;
  }

  async _decryptContactRecords(records) {
    const senderPubkey = this.pubkey;
    const decryptedContacts = [];
    const decryptionErrors = [];

    if (!records || !records.length) {
      return { contacts: decryptedContacts, errors: decryptionErrors };
    }

    for (const record of records) {
      try {
        const decryptedData = await this.decryptContact(record.encrypted_data, senderPubkey);
        decryptedContacts.push(decryptedData);
      } catch (decryptError) {
        decryptionErrors.push(decryptError.message);
      }
    }

    return { contacts: decryptedContacts, errors: decryptionErrors };
  }

  /**
   * Generate SHA256 hash using NDK
   */
  async generateHash(input) {
    const hash = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(input));
    const hashArray = Array.from(new Uint8Array(hash));
    const hashHex = hashArray.map(b => b.toString(16).padStart(2, '0')).join('');
    return hashHex;
  }

  /**
   * Generate email hash for uniqueness constraint
   */
  async generateEmailHash(email) {
    return await this.generateHash(`${this.salt}:${email.toLowerCase()}`);
  }

  /**
   * Generate search tokens for encrypted searching
   */
  async generateSearchTokens(searchableTerms) {
    const tokens = [];
    for (const term of searchableTerms) {
      const token = await this.generateHash(`${this.salt}:search:${term.toLowerCase()}`);
      tokens.push(token);
    }
    return tokens;
  }

  /**
   * Generate tag hashes for encrypted tag filtering
   */
  async generateTagHash(tag) {
    const hash = await this.generateHash(`${this.salt}:tag:${tag.toLowerCase()}`);
    return hash;
  }

  /**
   * Generate tag hashes for encrypted tag filtering
   */
  async generateTagHashes(tags) {
    const hashes = [];
    for (const tag of tags) {
      const hash = await this.generateTagHash(tag);
      hashes.push(hash);
    }
    return hashes;
  }

  /**
   * Add a tag to the database
   */
  async addTag(tag) {
    if (!this.jwt || !this.pubkey) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    const blindIndex = await this.generateTagHash(tag);
    const ciphertextTag = await this.ndk.signer.encrypt({ pubkey: this.pubkey }, tag, 'nip44');
    return await this.contactsApi.upsertTag({ blind_index: blindIndex, ciphertext_tag: ciphertextTag, key_version: 1 });
  }

  /**
   * Delete a tag from the database
   */
  async deleteTag(tag) {
    if (!this.jwt) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    const blindIndex = await this.generateTagHash(tag);
    // Use the ContactsApi wrapper to call the DELETE endpoint
    await this.contactsApi.deleteTag(blindIndex);
    return { result: true };
  }


  // =============================================================================
  // CONTACT OPERATIONS
  // =============================================================================

  /**
   * Store a single contact using NDK encryption
   */
  async storeContact(contact) {
    if (!this.jwt || !this.pubkey) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    try {
      const encryptedData = await this.encryptContact(contact, this.pubkey);
      
      const emailHash = await this.generateEmailHash(contact.email);
      
      // 4. Create search tokens from contact fields
      const searchableTerms = [
        contact.firstName,
        contact.lastName,
        contact.email,
        contact.company,
        ...(contact.notes ? contact.notes.split(' ').filter(word => word.length > 2) : [])
      ].filter(Boolean);
      
      const searchTokens = await this.generateSearchTokens(searchableTerms);
      const tagHashes = await this.generateTagHashes(contact.tags || []);
      
      // 5. Send to API via EncryptedContacts ContactsApi
      return await this.contactsApi.create({
        encrypted_data: encryptedData,
        email_hash: emailHash,
        search_tokens: searchTokens,
        tag_hashes: tagHashes,
        version: 1
      });
      
    } catch (error) {
      throw new Error(`Failed to store contact: ${error.message}`);
    }
  }

  /**
   * Store multiple contacts using NDK encryption
   */
  async storeContactsBulk(contacts, overwrite = false) {
    if (!this.jwt || !this.pubkey) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    try {
      const processedContacts = [];
      
      for (const contact of contacts) {
        // Encrypt contact data
        const encryptedData = await this.encryptContact(contact, this.pubkey);
        
        // Generate hashes
        const emailHash = await this.generateEmailHash(contact.email);
        
        const searchableTerms = [
          contact.firstName,
          contact.lastName,
          contact.email,
          contact.company,
          ...(contact.notes ? contact.notes.split(' ').filter(word => word.length > 2) : [])
        ].filter(Boolean);
        
        const searchTokens = await this.generateSearchTokens(searchableTerms);
        const tagHashes = await this.generateTagHashes(contact.tags || []);
        
        processedContacts.push({
          encrypted_data: encryptedData,
          email_hash: emailHash,
          search_tokens: searchTokens,
          tag_hashes: tagHashes,
          version: 1
        });
      }
      
      return await this.http.request("POST", "/api/contacts/bulk", { contacts: processedContacts, overwrite });
      
    } catch (error) {
      throw new Error(`Failed to store contacts: ${error.message} ${error.stack}`);
    }
  }

  /**
   * Get all contacts and decrypt them
   */
  async getContacts(page = 1, perPage = 100) {
    if (!this.jwt) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    try {
      const result = await this.contactsApi.list({ page: page, per_page: perPage });
      return await this._decryptContactRecords(result.contacts);
      
    } catch (error) {
      throw new Error(`Failed to get contacts: ${error.message}`);
    }
  }

  async countContacts(filter = undefined) {
    if (!this.jwt) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    try {
      const hasFilter = filter && typeof filter === 'object' && Object.keys(filter).length > 0;

      if (hasFilter) {
        const result = await this.contactsApi.tagsCount(filter);
        return result?.count ?? 0;
      }

      const total = await this.contactsApi.count();
      return total?.count ?? 0;
    } catch (error) {
      throw new Error(`Failed to count contacts: ${error.message}`);
    }
  }

  async getContactsByCriteria(filter, page = 1, perPage = 100) {
    if (!this.jwt) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    if (!filter) {
      throw new Error('Filter is required to fetch contacts by criteria');
    }

    try {
      const result = await this.contactsApi.tagsFilter(filter, { page: page, per_page: perPage });
      return await this._decryptContactRecords(result.contacts);

    } catch (error) {
      throw new Error(`Failed to get contacts by criteria: ${error.message}`);
    }
  }

  /**
   * Get all contact tags
   */
  async getContactTags() {
    if (!this.jwt || !this.pubkey) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }


    try {
      const result = await this.contactsApi.listTags();

      // Decrypt the contacts
      const senderPubkey = this.pubkey;
      const decryptedTags = [];
      const decryptionErrors = [];
      
      for (const tag of result.tags) {
        try {
          const decryptedData = await this.decryptTag(tag.ciphertext_tag, senderPubkey);
          decryptedTags.push( decryptedData );
        } catch (decryptError) {
          decryptionErrors.push(decryptError.message);
        }
      }
      
      return {
        tags: decryptedTags,
        errors: decryptionErrors
      };
      

    } catch (error) {
      throw new Error(`Failed to get contact tags: ${error.message}`);
    }
  }


  /**
   * Search contacts by term
   */
  async searchContacts(searchTerm) {
    if (!this.jwt || !this.pubkey) {
      throw new Error('Not authenticated. Call authenticate() first.');
    }

    try {
      const searchToken = await this.generateHash(`${this.pubkey}:search:${searchTerm.toLowerCase()}`);
      
      const result = await this.contactsApi.searchByToken(searchToken);
      
      // Decrypt the search results
      const senderPubkey = this.pubkey;
      const decryptedContacts = [];
      
      for (const contact of result.contacts) {
        try {
          const decryptedData = await this.decryptContact(contact.encrypted_data, senderPubkey);
          decryptedContacts.push({
            ...contact,
            decrypted_data: decryptedData
          });
        } catch (decryptError) {
          console.warn(`Failed to decrypt contact ${contact.id}:`, decryptError);
          decryptedContacts.push({
            ...contact,
            decrypted_data: null,
            decrypt_error: decryptError.message
          });
        }
      }
      
      return {
        ...result,
        contacts: decryptedContacts
      };
      
    } catch (error) {
      throw new Error(`Failed to search contacts: ${error.message}`);
    }
  }

  // =============================================================================
  // UTILITY METHODS
  // =============================================================================

  /**
   * Check if authenticated
   */
  isAuthenticated() {
    return !!(this.jwt && this.pubkey);
  }

  /**
   * Get current JWT token
   */
  getJWT() {
    return this.jwt;
  }

  /**
   * Get current public key
   */
  getPubkey() {
    return this.pubkey;
  }

  /**
   * Clear authentication data
   */
  logout() {
    this.jwt = null;
    this.pubkey = null;
  }
}

// =============================================================================
// FACTORY FUNCTION
// =============================================================================

/**
 * Create a new Contacts instance with NDK
 */
export function createContacts(ndk) {
  return new Contacts(ndk);
}

// =============================================================================
// EXAMPLE USAGE
// =============================================================================

/**
 * Example of how to use the Contacts class
 */
export async function exampleUsage(ndk) {
  try {
    console.log('🚀 Starting Contacts example...');
    
    // 1. Create contacts instance
    const contacts = new Contacts(ndk);
    
    // 2. Authenticate
    console.log('🔐 Authenticating...');
    const authResult = await contacts.authenticate();
    if (!authResult.success) {
      throw new Error(authResult.error);
    }
    console.log('✅ Authentication successful');
    
    // 3. Sample contacts
    const sampleContacts = [
      {
        firstName: "Alice",
        lastName: "Johnson",
        email: "alice@example.com",
        phone: "+1-555-0101",
        company: "Tech Corp",
        notes: "Met at conference, interested in blockchain",
        tags: ["work", "conference", "blockchain"]
      },
      {
        firstName: "Bob",
        lastName: "Smith",
        email: "bob@example.com",
        phone: "+1-555-0102",
        company: "Design Studio",
        notes: "Freelance designer, worked on mobile app",
        tags: ["work", "design", "mobile"]
      }
    ];
    
    // 4. Store contacts
    console.log('📝 Storing contacts...');
    const storeResult = await contacts.storeContactsBulk(sampleContacts);
    console.log('✅ Contacts stored successfully');
    
    // 5. Retrieve and decrypt contacts
    console.log('📋 Retrieving contacts...');
    const allContacts = await contacts.getContacts();
    console.log('✅ Retrieved contacts:', allContacts.contacts.length);
    
    // 6. Search contacts
    console.log('🔍 Searching for "alice"...');
    const searchResults = await contacts.searchContacts('alice');
    console.log('✅ Search results:', searchResults.contacts.length);
    
    console.log('🎉 Example completed successfully!');
    
  } catch (error) {
    console.error('❌ Example failed:', error.message);
  }
}

// =============================================================================
// DEFAULT EXPORT
// =============================================================================

// Default export for convenience
export default Contacts; 