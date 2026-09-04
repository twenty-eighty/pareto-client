# Queue Server API Guide (Producer + Consumer)

This guide explains how to use the Queue Server API reliably, from creating a campaign, uploading jobs, committing uploads, and tracking/processing. It is designed so an automated client (or AI) can use the API correctly and idempotently.

## Base URL

- Development: `http://localhost:4433/v1`

## Authentication

- Producer (upload/commit/status): JWT Bearer
  1) Obtain a JWT using NIP‑98: `POST /auth/token` with header `Authorization: Nostr <base64-encoded NIP-98 event>`
  2) Use `Authorization: Bearer <jwt>` for producer endpoints.

- Consumer (leasing/completing jobs): static token or mTLS (implementation dependent). Send `Authorization: Bearer <token>` if configured.

## Producer flow (reliable upload + commit)

Upload while the campaign is in `draft`, then finalize with a `commit` call to start processing.

### 1) Create campaign (state=draft)
- POST `/campaigns`
- Auth: Bearer JWT
- Body:
```json
{
  "externalId": "optional-client-id",
  "ownerId": "<hex pubkey>",
  "queue": "newsletter",
  "title": "Optional title"
}
```
- 201 `{ "campaign_id": "<server-generated numeric id as string>" }`, 422 on validation

### 2) Bulk upload jobs (NDJSON)
- POST `/campaigns/{id}/jobs/bulk`
- Auth: Bearer JWT; `Content-Type: application/x-ndjson`
- Each line:
```json
{
  "idem": "<stable idempotency key>",
  "ciphertext_b64": "<encrypted payload base64>",
  "enc": { "alg": "<algorithm>", "kid": "<pubkey-or-key-id>" },
  "size_bytes": 1234
}
```
- 200 `{ accepted, duplicates, errors, last_id }`
- 400 invalid NDJSON, 409 wrong state, 422 invalid data

Notes:
- Idempotent by `(campaign_id, idem)`; re-sending is safe.
- Server increments `uploaded_jobs_count` by `accepted`.

### 3) Commit uploads (finalize)
- PATCH `/campaigns/{id}/commit`
- Body (optional): `{ "expected_jobs": 12345 }`
- 200 `{ campaign_id, state, expected_jobs }`
- 409 `counts_mismatch` or `already_committed`; 422 `invalid_expected`

### 4) Status
- GET `/campaigns/{id}/status`
- 200 `{ campaign_id, state, uploaded_jobs, expected_jobs, counts, updated_at }`

## Consumer flow
- Lease: `POST /jobs/lease` with `{ queue, max, visibility_timeout_s }`
- Heartbeat: `POST /jobs/{job_id}/heartbeat` with `{ lease_token, progress? }`
- Complete: `POST /jobs/{job_id}/complete` with `{ lease_token }`
- Fail: `POST /jobs/{job_id}/fail` with `{ lease_token, error_code?, error_hint? }`

## Errors
- 401 unauthorized; 403 forbidden; 404 not found
- 409 conflict (wrong state, not leased, counts mismatch)
- 422 unprocessable entity (invalid data)
- 500 internal error (unexpected)

Format:
```json
{ "error": { "code": "string", "message": "string" } }
```

## Reliability
- Upload is idempotent with `idem`.
- Upload multiple NDJSON batches while `draft`.
- Only start processing after `commit`.

## Example (producer)
```bash
JWT=...
# Create
curl -s -X POST http://localhost:4433/v1/campaigns \
  -H "Authorization: Bearer $JWT" -H "Content-Type: application/json" \
  -d '{"id":"camp-123","ownerId":"<pubkey>","queue":"newsletter","title":"My NL"}'
# Upload NDJSON
cat > jobs.ndjson <<'LINES'
{"idem":"c1-1","ciphertext_b64":"...","enc":{"alg":"JWE-ECDH-ES+A256GCM","kid":"<pubkey>"},"size_bytes":1234}
{"idem":"c1-2","ciphertext_b64":"...","enc":{"alg":"JWE-ECDH-ES+A256GCM","kid":"<pubkey>"},"size_bytes":2345}
LINES
curl -s -X POST http://localhost:4433/v1/campaigns/camp-123/jobs/bulk \
  -H "Authorization: Bearer $JWT" -H "Content-Type: application/x-ndjson" \
  --data-binary @jobs.ndjson
# Commit
curl -s -X PATCH http://localhost:4433/v1/campaigns/camp-123/commit \
  -H "Authorization: Bearer $JWT" -H "Content-Type: application/json" \
  -d '{"expected_jobs":2}'
# Status
curl -s http://localhost:4433/v1/campaigns/camp-123/status -H "Authorization: Bearer $JWT"
```

## TypeScript client
- Generated in `ts-client/`.
- Configure base path:
```ts
import { Configuration, DefaultApi } from './ts-client';
const api = new DefaultApi(new Configuration({ basePath: 'http://localhost:4433/v1' }));
```
- Methods: `createCampaign`, `bulkEnqueueJobs`, `commitCampaign`, `getCampaignStatus`.
- For NDJSON, send raw text with `application/x-ndjson`.
