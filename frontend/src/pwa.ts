type ElmPorts = {
  sendCommand: {
    subscribe: (callback: (msg: { command: string; value: any }) => void) => void;
  };
  receiveMessage: {
    send: (msg: { messageType: string; value: any }) => void;
  };
};

type ElmApp = {
  ports: ElmPorts;
};

type VersionPayload = {
  gitVersion?: string;
  js?: string;
};

const VERSION_POLL_MS = 5 * 60 * 1000;

let waitingWorker: ServiceWorker | null = null;
let notifiedNewVersion = false;
let reloading = false;

function runningBundlePath(): string | null {
  const el = document.querySelector<HTMLScriptElement>(
    'script[type="module"][src*="/assets/index-"]'
  );
  if (!el) {
    return null;
  }
  try {
    return new URL(el.src, window.location.origin).pathname;
  } catch {
    return el.getAttribute("src");
  }
}

function notifyNewVersion(app: ElmApp): void {
  if (notifiedNewVersion) {
    return;
  }
  notifiedNewVersion = true;
  app.ports.receiveMessage.send({
    messageType: "newVersionAvailable",
    value: null,
  });
}

function reloadOnce(): void {
  if (reloading) {
    return;
  }
  reloading = true;
  window.location.reload();
}

export function reloadForNewVersion(): void {
  if (waitingWorker) {
    waitingWorker.postMessage({ type: "SKIP_WAITING" });
    window.setTimeout(reloadOnce, 400);
    return;
  }
  reloadOnce();
}

async function checkVersion(app: ElmApp, runningJs: string): Promise<void> {
  try {
    const response = await fetch("/version.json", { cache: "no-store" });
    if (!response.ok) {
      return;
    }
    const payload = (await response.json()) as VersionPayload;
    if (payload.js && payload.js !== runningJs) {
      notifyNewVersion(app);
    }
  } catch {
    // Offline or older backends without /version.json.
  }
}

function startVersionPolling(app: ElmApp, runningJs: string): void {
  const maybeCheck = () => {
    if (document.visibilityState === "visible") {
      void checkVersion(app, runningJs);
    }
  };

  maybeCheck();
  window.setInterval(maybeCheck, VERSION_POLL_MS);
  document.addEventListener("visibilitychange", maybeCheck);
  window.addEventListener("online", maybeCheck);
}

function watchWaitingWorker(app: ElmApp, worker: ServiceWorker): void {
  if (worker.state === "installed" && navigator.serviceWorker.controller) {
    waitingWorker = worker;
    notifyNewVersion(app);
    return;
  }
  worker.addEventListener("statechange", () => {
    if (worker.state === "installed" && navigator.serviceWorker.controller) {
      waitingWorker = worker;
      notifyNewVersion(app);
    }
  });
}

async function registerServiceWorker(app: ElmApp): Promise<void> {
  try {
    const registration = await navigator.serviceWorker.register("/sw.js", {
      scope: "/",
    });

    if (registration.waiting && navigator.serviceWorker.controller) {
      waitingWorker = registration.waiting;
      notifyNewVersion(app);
    }

    registration.addEventListener("updatefound", () => {
      if (registration.installing) {
        watchWaitingWorker(app, registration.installing);
      }
    });

    window.setInterval(() => {
      void registration.update();
    }, VERSION_POLL_MS);
  } catch (error) {
    console.warn("Pareto service worker registration failed", error);
  }
}

export function initPwa(app: ElmApp): void {
  const runningJs = runningBundlePath();
  const canUseProductionPwa =
    Boolean(runningJs) && "serviceWorker" in navigator;

  if (runningJs) {
    startVersionPolling(app, runningJs);
  }

  if (!canUseProductionPwa) {
    return;
  }

  void registerServiceWorker(app);
}
