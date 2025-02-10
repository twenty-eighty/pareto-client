'use strict';


class ZapComponent extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        console.log('zap-component added to page.');
        const scripts = document.getElementsByTagName("script");
        const myScripts = Array.from(scripts).filter(elem => elem.getAttribute("src") === "/node_modules/nostr-zap/dist/main.js");
        if (myScripts.length === 0) {
            const newScript = document.createElement('script');
            newScript.src = "/node_modules/nostr-zap/dist/main.js";
            document.body.appendChild(newScript);
            console.log("Added script element");
        } else {
            const zapButton = document.getElementById(this.zapButtonId);
            if (window.nostrZap) {
                window.nostrZap.initTarget(zapButton);
                console.log("zap button initialized");
            }
        }
    }

    disconnectedCallback() {
        console.log('zap-component removed from page.');
    }

    set buttonId(id) {
        this.zapButtonId = id;
      }
}

customElements.define('js-zap-component', ZapComponent);