import assert from "node:assert/strict";
import test from "node:test";

import { consumeCsv, contactFromCsvRow, contactToCsvLine } from "./contactCsv.ts";

test("consumeCsv keeps quoted commas and newlines across chunks", () => {
  const first = consumeCsv("", 'email,name\n"a@b.c","Ada', false);
  assert.deepEqual(first.records, [["email", "name"]]);
  const second = consumeCsv(first.rest, ' Lovelace"\nplain@example.com,Grace\n', true);
  assert.deepEqual(second.records, [
    ["a@b.c", "Ada Lovelace"],
    ["plain@example.com", "Grace"],
  ]);
});

test("contact mapping skips rows without an email and splits a full name", () => {
  const contact = contactFromCsvRow(
    ["Ada Lovelace", "ada@example.com", "vip, press"],
    [
      { index: 1, field: "email" },
      { index: 0, field: "name" },
      { index: 2, field: "tags" },
    ],
    ["import_2026"],
  );
  assert.equal(contact?.email, "ada@example.com");
  assert.equal(contact?.firstName, "Ada");
  assert.equal(contact?.lastName, "Lovelace");
  assert.deepEqual(contact?.tags, ["vip", "press", "import_2026"]);
  assert.equal(contactFromCsvRow(["no email"], [{ index: 0, field: "name" }], []), null);
});

test("csv export escapes quotes", () => {
  const line = contactToCsvLine({
    id: "1",
    tags: ["vip", "press"],
    email: "ada@example.com",
    firstName: 'Ada "A"',
    dnd: false,
  });
  assert.equal(line.split(",")[1], "ada@example.com");
  assert.match(line, /"Ada ""A"""/);
});
