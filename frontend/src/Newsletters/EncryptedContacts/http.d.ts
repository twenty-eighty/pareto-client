/**
 * Function that returns a bearer token string used for Authorization headers.
 * Can be synchronous or async. Return undefined for no auth header.
 */
export type AuthTokenProvider = () => string | undefined | Promise<string | undefined>;
export interface HttpClientOptions {
    /** Base URL of the API, e.g. https://api.example.com */
    baseUrl: string;
    /** Optional provider for dynamic auth tokens */
    getAuthToken?: AuthTokenProvider;
    /** Optional static token that always takes precedence */
    staticToken?: string;
}
/**
 * Minimal fetch-based HTTP client with JSON handling and bearer auth.
 */
export declare class HttpClient {
    private readonly baseUrl;
    private tokenProvider?;
    private staticToken?;
    constructor(options: HttpClientOptions);
    setTokenProvider(provider: AuthTokenProvider): void;
    setStaticToken(token: string | undefined): void;
    /**
     * Perform an HTTP request.
     * @param method HTTP method (GET, POST, ...)
     * @param path API path (e.g. /api/contacts)
     * @param body Optional JSON body (object will be JSON.stringified)
     * @param query Optional query params
     */
    request<T>(method: string, path: string, body?: unknown, query?: Record<string, string | number | boolean | undefined>): Promise<T>;
    private getToken;
    private buildUrl;
}
