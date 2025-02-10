'use strict';

let zapHandlerSet = false;

class ZapComponent extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        console.log('Custom element added to page.');
        const scripts = document.getElementsByTagName("script");
        const myScripts = Array.from(scripts).filter(elem => elem.getAttribute("src") === "/node_modules/nostr-zap/dist/main.js");
        if (myScripts.length === 0) {
            const newScript = document.createElement('script');
            newScript.src = "/node_modules/nostr-zap/dist/main.js";
            document.body.appendChild(newScript);
            zapHandlerSet = true;
            console.log("Added script element");
        } else {
            const zapButton = document.getElementById(this.zapButtonId);
            if (window.nostrZap && !zapHandlerSet) {
                window.nostrZap.initTarget(zapButton);
                zapHandlerSet = true;
                console.log("Zap button initialized");
            }
        }
    }

    disconnectedCallback() {
        zapHandlerSet = false;
        console.log('Custom element removed from page.');
    }

    set buttonId(id) {
        this.zapButtonId = id;
      }
}

customElements.define('js-zap-component', ZapComponent);