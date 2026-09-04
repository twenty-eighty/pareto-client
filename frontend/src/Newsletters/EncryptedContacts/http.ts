/**
 * Function that returns a bearer token string used for Authorization headers.
 * Can be synchronous or async. Return undefined for no auth header.
 */
export type AuthTokenProvider = () => string | undefined | Promise<string | undefined>

export interface HttpClientOptions {
  /** Base URL of the API, e.g. https://api.example.com */
  baseUrl: string
  /** Optional provider for dynamic auth tokens */
  getAuthToken?: AuthTokenProvider
  /** Optional static token that always takes precedence */
  staticToken?: string
}

/**
 * Minimal fetch-based HTTP client with JSON handling and bearer auth.
 */
export class HttpClient {
  private readonly baseUrl: string
  private tokenProvider?: AuthTokenProvider
  private staticToken?: string

  constructor(options: HttpClientOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, '')
    this.tokenProvider = options.getAuthToken
    this.staticToken = options.staticToken
  }

  setTokenProvider(provider: AuthTokenProvider) {
    this.tokenProvider = provider
  }

  setStaticToken(token: string | undefined) {
    this.staticToken = token
  }

  /**
   * Perform an HTTP request.
   * @param method HTTP method (GET, POST, ...)
   * @param path API path (e.g. /api/contacts)
   * @param body Optional JSON body (object will be JSON.stringified)
   * @param query Optional query params
   */
  async request<T = any>(
    method: string,
    path: string,
    body?: unknown,
    query?: Record<string, string | number | boolean | undefined | null>
  ): Promise<T> {
    const url = this.buildUrl(path, query)
    const headers: Record<string, string> = { Accept: 'application/json' }
    if (body !== undefined && body !== null) {
      headers['Content-Type'] = 'application/json'
    }
    const authToken = await this.getToken()
    if (authToken) {
      headers['Authorization'] = `Bearer ${authToken}`
    }
    const response = await fetch(url, {
      method,
      headers,
      body: body !== undefined && body !== null ? JSON.stringify(body) : undefined,
    })
    const contentType = response.headers.get('content-type') || ''
    const isJson = contentType.includes('application/json')
    if (!response.ok) {
      const errorPayload = isJson
        ? await response.json().catch(() => undefined)
        : await response.text().catch(() => undefined)
      throw new Error(
        `HTTP ${response.status} ${response.statusText}${
          errorPayload
            ? ': ' + (typeof errorPayload === 'string' ? errorPayload : JSON.stringify(errorPayload))
            : ''
        }`
      )
    }
    return (isJson ? await response.json() : await response.text()) as T
  }

  private async getToken() {
    if (this.staticToken) return this.staticToken
    if (this.tokenProvider) return await this.tokenProvider()
    return undefined
  }

  private buildUrl(
    path: string,
    query?: Record<string, string | number | boolean | undefined | null>
  ) {
    const normalizedPath = path.startsWith('/') ? path : `/${path}`
    const url = new URL(this.baseUrl + normalizedPath)
    if (query) {
      Object.entries(query).forEach(([k, v]) => {
        if (v !== undefined && v !== null) url.searchParams.set(k, String(v))
      })
    }
    return url.toString()
  }
}
