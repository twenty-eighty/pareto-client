'use strict';


class ZapComponent extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        console.log('zap-component added to page.');
        const scripts = document.getElementsByTagName("script");
        const myScripts = Array.from(scripts).filter(elem => elem.getAttribute("src") === "/js/nostr-zap.js");
        if (myScripts.length === 0) {
            const newScript = document.createElement('script');
            newScript.src = "/js/nostr-zap.js";
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

    get buttonId() {
        return this.zapButtonId;
    }
}

customElements.define('js-zap-component', ZapComponent);