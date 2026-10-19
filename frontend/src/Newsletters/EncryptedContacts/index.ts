const LOCAL_CONTACTS_API_URL = "http://localhost:4003";
const HOSTED_CONTACTS_API_URL = "https://contacts.pareto.space";

/** Local pages talk to the dev server. Every other host uses the hosted database. */
export function contactsApiBaseUrl(): string {
  const host = typeof window !== "undefined" ? window.location.hostname : "";
  if (host === "localhost" || host === "127.0.0.1") {
    return LOCAL_CONTACTS_API_URL;
  }
  return HOSTED_CONTACTS_API_URL;
}

export * from "./types";
export * from "./http";
export * from "./contacts";
export * from "./signer";
export { EncryptedContacts } from "./encrypted-contacts";
export type { ContactPage, EncryptedContactsOptions } from "./encrypted-contacts";
