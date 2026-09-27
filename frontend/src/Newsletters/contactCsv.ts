import type { ContactInput, DecryptedContact } from "./EncryptedContacts/types";

const PREVIEW_ROWS = 10;
const PREVIEW_BYTE_LIMIT = 1024 * 1024;
const IMPORT_BATCH_SIZE = 100;
const EXPORT_PAGE_SIZE = 100;

export const CSV_EXPORT_FIELDS = [
  "dnd",
  "email",
  "firstName",
  "lastName",
  "pubkey",
  "source",
  "datesub",
  "dateunsub",
  "tags",
] as const;

export type CsvMapping = { index: number; field: string };

export type CsvImportProgress = {
  stored: number;
  skipped: number;
  errors: number;
  done: boolean;
  error?: string;
};

type CsvChunk = { records: string[][]; rest: string };

/**
 * Split complete CSV records out of a text buffer. Quoted commas and newlines
 * stay inside a field. The unfinished tail is returned so the next chunk can continue it.
 */
export function consumeCsv(buffer: string, chunk: string, done: boolean): CsvChunk {
  const text = buffer + chunk;
  const records: string[][] = [];
  let row: string[] = [];
  let field = "";
  let inQuotes = false;
  let lastComplete = 0;

  for (let i = 0; i < text.length; i++) {
    const char = text[i];
    if (inQuotes) {
      if (char === '"') {
        if (text[i + 1] === '"') {
          field += '"';
          i += 1;
        } else {
          inQuotes = false;
        }
      } else {
        field += char;
      }
      continue;
    }

    if (char === '"') {
      inQuotes = true;
    } else if (char === ",") {
      row.push(field);
      field = "";
    } else if (char === "\n" || char === "\r") {
      if (char === "\r" && text[i + 1] === "\n") {
        i += 1;
      }
      row.push(field);
      records.push(row);
      row = [];
      field = "";
      lastComplete = i + 1;
    } else {
      field += char;
    }
  }

  if (done && (field.length > 0 || row.length > 0 || inQuotes)) {
    row.push(field);
    records.push(row);
    return { records, rest: "" };
  }

  return { records, rest: text.slice(lastComplete) };
}

export async function readCsvPreview(file: File, limit = PREVIEW_ROWS): Promise<string[][]> {
  const reader = file.stream().getReader();
  const decoder = new TextDecoder();
  const rows: string[][] = [];
  let rest = "";
  let bytes = 0;
  let stripBom = true;

  try {
    while (rows.length < limit && bytes < PREVIEW_BYTE_LIMIT) {
      const { done, value } = await reader.read();
      if (value) {
        bytes += value.byteLength;
      }
      let chunk = value ? decoder.decode(value, { stream: !done }) : "";
      if (stripBom) {
        chunk = chunk.replace(/^\uFEFF/, "");
        stripBom = false;
      }
      const parsed = consumeCsv(rest, chunk, done);
      rest = parsed.rest;
      for (const record of parsed.records) {
        if (rows.length < limit) {
          rows.push(record);
        }
      }
      if (done) {
        break;
      }
    }
  } finally {
    await reader.cancel().catch(() => undefined);
  }

  return rows;
}

export async function* iterateCsvRecords(file: File): AsyncGenerator<string[]> {
  const reader = file.stream().getReader();
  const decoder = new TextDecoder();
  let rest = "";
  let stripBom = true;

  try {
    while (true) {
      const { done, value } = await reader.read();
      let chunk = value ? decoder.decode(value, { stream: !done }) : "";
      if (stripBom) {
        chunk = chunk.replace(/^\uFEFF/, "");
        stripBom = false;
      }
      const parsed = consumeCsv(rest, chunk, done);
      rest = parsed.rest;
      for (const record of parsed.records) {
        yield record;
      }
      if (done) {
        return;
      }
    }
  } finally {
    reader.releaseLock();
  }
}

export function contactFromCsvRow(record: string[], mapping: CsvMapping[], extraTags: string[]): ContactInput | null {
  const contact: ContactInput = { email: "", source: "CSV" };

  for (const column of mapping) {
    const value = String(record[column.index] ?? "").trim();
    if (!value) {
      continue;
    }
    assignField(contact, column.field, value);
  }

  const email = String(contact.email || "").trim();
  if (!email) {
    return null;
  }
  contact.email = email;

  const tags = uniqueTags([...(Array.isArray(contact.tags) ? contact.tags : []), ...extraTags]);
  if (tags.length) {
    contact.tags = tags;
  }

  return contact;
}

export function contactToCsvLine(contact: DecryptedContact): string {
  return CSV_EXPORT_FIELDS.map((field) => csvEscape(exportCell(contact, field))).join(",");
}

export function csvHeaderLine(): string {
  return CSV_EXPORT_FIELDS.join(",");
}

function assignField(contact: ContactInput, field: string, value: string): void {
  switch (field) {
    case "email":
      contact.email = value;
      return;
    case "name": {
      const parts = value.split(/\s+/).filter(Boolean);
      if (!contact.firstName && parts[0]) {
        contact.firstName = parts[0];
      }
      if (!contact.lastName && parts.length > 1) {
        contact.lastName = parts.slice(1).join(" ");
      }
      return;
    }
    case "firstName":
      contact.firstName = value;
      return;
    case "lastName":
      contact.lastName = value;
      return;
    case "pubkey":
      contact.pubkey = value;
      return;
    case "source":
      contact.source = value;
      return;
    case "dnd":
      contact.dnd = parseBool(value);
      return;
    case "datesub": {
      const parsed = parseDate(value);
      if (parsed !== null) {
        contact.datesub = parsed;
      }
      return;
    }
    case "dateunsub": {
      const parsed = parseDate(value);
      if (parsed !== null) {
        contact.dateunsub = parsed;
        contact.dateUnsubscription = parsed;
      }
      return;
    }
    case "tags":
      contact.tags = uniqueTags(value.split(","));
      return;
    case "locale":
      contact.locale = value;
      return;
    case "undeliverable":
      contact.undeliverable = value;
      return;
    default:
      contact[field] = value;
  }
}

function parseBool(value: string): boolean {
  const normalized = value.trim().toLowerCase();
  return normalized === "true" || normalized === "yes" || normalized === "1";
}

function parseDate(value: string): number | null {
  const trimmed = value.trim();
  if (!trimmed) {
    return null;
  }
  if (/^\d+$/.test(trimmed)) {
    const millis = Number(trimmed);
    return Number.isFinite(millis) ? millis : null;
  }
  const millis = Date.parse(trimmed);
  return Number.isNaN(millis) ? null : millis;
}

function uniqueTags(values: string[]): string[] {
  const seen = new Set<string>();
  const tags: string[] = [];
  for (const value of values) {
    const tag = value.trim();
    if (!tag || seen.has(tag)) {
      continue;
    }
    seen.add(tag);
    tags.push(tag);
  }
  return tags;
}

function exportCell(contact: DecryptedContact, field: (typeof CSV_EXPORT_FIELDS)[number]): string {
  if (field === "tags") {
    return Array.isArray(contact.tags) ? contact.tags.join(",") : "";
  }
  if (field === "dnd") {
    return contact.dnd === true || contact.dnd === "true" || contact.dnd === 1 ? "true" : "false";
  }
  if (field === "pubkey") {
    return stringValue(contact.pubkey ?? contact.pubKey);
  }
  if (field === "datesub") {
    return formatDate(contact.datesub ?? contact.dateSubscription);
  }
  if (field === "dateunsub") {
    return formatDate(contact.dateunsub ?? contact.dateUnsubscription);
  }
  return stringValue(contact[field]);
}

function stringValue(value: unknown): string {
  if (value === undefined || value === null) {
    return "";
  }
  return String(value);
}

function formatDate(value: unknown): string {
  if (value === undefined || value === null || value === "" || value === 0 || value === "0") {
    return "";
  }
  const millis = typeof value === "number" ? value : /^\d+$/.test(String(value)) ? Number(value) : Date.parse(String(value));
  if (!Number.isFinite(millis)) {
    return String(value);
  }
  return new Date(millis).toISOString();
}

function csvEscape(value: string): string {
  if (/[",\n\r]/.test(value)) {
    return `"${value.replace(/"/g, '""')}"`;
  }
  return value;
}

export const csvImportBatchSize = IMPORT_BATCH_SIZE;
export const csvExportPageSize = EXPORT_PAGE_SIZE;

type BulkStore = {
  storeContactsBulk: (
    contacts: ContactInput[],
    overwrite?: boolean,
  ) => Promise<{ stored: number; tagErrors: string[] }>;
};

type PagedContacts = {
  getContacts: (page?: number, perPage?: number) => Promise<{ contacts: DecryptedContact[]; sourceCount: number }>;
};

export async function importContactCsvFile(
  file: File,
  options: { skipRows: number; mapping: CsvMapping[]; overwrite: boolean; tags: string[] },
  api: BulkStore,
  onProgress: (progress: CsvImportProgress) => void,
  isCancelled: () => boolean,
): Promise<void> {
  const progress: CsvImportProgress = { stored: 0, skipped: 0, errors: 0, done: false };
  let seen = 0;
  let batch: ContactInput[] = [];

  const report = () => onProgress({ ...progress });

  try {
  const flush = async () => {
    if (!batch.length) {
      return;
    }
    const pending = batch;
    batch = [];
    const result = await api.storeContactsBulk(pending, options.overwrite);
    progress.stored += result.stored || 0;
    progress.errors += result.tagErrors?.length || 0;
    report();
  };

  for await (const record of iterateCsvRecords(file)) {
    if (isCancelled()) {
      progress.done = true;
      progress.error = "Import cancelled";
      report();
      return;
    }
    if (seen <= options.skipRows) {
      seen += 1;
      continue;
    }
    seen += 1;
    const contact = contactFromCsvRow(record, options.mapping, options.tags);
    if (!contact) {
      progress.skipped += 1;
      continue;
    }
    batch.push(contact);
    if (batch.length >= IMPORT_BATCH_SIZE) {
      await flush();
    }
  }

  await flush();
  progress.done = true;
  report();
  } catch (error) {
    progress.done = true;
    progress.error = error instanceof Error ? error.message : "Failed to import contacts";
    report();
  }
}

export async function exportContactCsvFile(
  api: PagedContacts,
  write: (chunk: string) => Promise<void>,
  onProgress: (progress: { exported: number; done: boolean; error?: string }) => void,
  isCancelled: () => boolean,
): Promise<number> {
  let page = 1;
  let exported = 0;

  try {
  await write(`${csvHeaderLine()}\n`);

  while (!isCancelled()) {
    const result = await api.getContacts(page, EXPORT_PAGE_SIZE);
    const contacts = result.contacts || [];
    let chunk = "";
    for (const contact of contacts) {
      chunk += `${contactToCsvLine(contact)}\n`;
      exported += 1;
    }
    if (chunk) {
      await write(chunk);
    }
    onProgress({ exported, done: false });
    if ((result.sourceCount || 0) < EXPORT_PAGE_SIZE) {
      break;
    }
    page += 1;
  }

  if (isCancelled()) {
    onProgress({ exported, done: true, error: "Export cancelled" });
    return exported;
  }

  onProgress({ exported, done: true });
  return exported;
  } catch (error) {
    onProgress({
      exported,
      done: true,
      error: error instanceof Error ? error.message : "Failed to export contacts",
    });
    throw error;
  }
}
