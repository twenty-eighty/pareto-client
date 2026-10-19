export type AuthTokenProvider = () => string | undefined | Promise<string | undefined>;

export interface HttpClientOptions {
  baseUrl: string;
  getAuthToken?: AuthTokenProvider;
  staticToken?: string;
}

export class HttpClient {
  private readonly baseUrl: string;
  private tokenProvider?: AuthTokenProvider;
  private staticToken?: string;

  constructor(options: HttpClientOptions) {
    this.baseUrl = options.baseUrl.replace(/\/$/, "");
    this.tokenProvider = options.getAuthToken;
    this.staticToken = options.staticToken;
  }

  setTokenProvider(provider: AuthTokenProvider): void {
    this.tokenProvider = provider;
  }

  setStaticToken(token: string | undefined): void {
    this.staticToken = token;
  }

  async request<T>(
    method: string,
    path: string,
    body?: unknown,
    query?: Record<string, string | number | boolean | undefined>,
    options?: { authorization?: string; skipAuth?: boolean },
  ): Promise<T> {
    const url = this.buildUrl(path, query);
    const headers: Record<string, string> = { Accept: "application/json" };

    if (body !== undefined && body !== null) {
      headers["Content-Type"] = "application/json";
    }

    if (options?.authorization) {
      headers.Authorization = options.authorization;
    } else if (!options?.skipAuth) {
      const authToken = await this.getToken();
      if (authToken) {
        headers.Authorization = `Bearer ${authToken}`;
      }
    }

    const response = await fetch(url, {
      method,
      headers,
      body: body !== undefined && body !== null ? JSON.stringify(body) : undefined,
    });

    const contentType = response.headers.get("content-type") || "";
    const isJson = contentType.includes("application/json");
    if (!response.ok) {
      const errorPayload = isJson
        ? await response.json().catch(() => undefined)
        : await response.text().catch(() => undefined);
      const details = errorPayload
        ? `: ${typeof errorPayload === "string" ? errorPayload : JSON.stringify(errorPayload)}`
        : "";
      throw new Error(`HTTP ${response.status} ${response.statusText}${details}`);
    }

    return (isJson ? await response.json() : await response.text()) as T;
  }

  private async getToken(): Promise<string | undefined> {
    if (this.staticToken) return this.staticToken;
    if (this.tokenProvider) return await this.tokenProvider();
    return undefined;
  }

  private buildUrl(path: string, query?: Record<string, string | number | boolean | undefined>): string {
    const normalizedPath = path.startsWith("/") ? path : `/${path}`;
    const url = new URL(this.baseUrl + normalizedPath);
    if (query) {
      Object.entries(query).forEach(([key, value]) => {
        if (value !== undefined && value !== null) {
          url.searchParams.set(key, String(value));
        }
      });
    }
    return url.toString();
  }
}
