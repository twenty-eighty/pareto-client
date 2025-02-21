
class ScriptElement extends HTMLElement {
  constructor() {
    super();
    const shadow = this.attachShadow({ mode: 'open' });

    // Create a textarea element
    this.scriptElement = document.createElement('script');

    // Append script element to shadow DOM
    shadow.appendChild(this.scriptElement);
  }

  // Define observed attributes for syncing with Elm
  static get observedAttributes() {
    return ['src'];
  }

  // React to attribute changes
  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'src') {
      this.scriptElement.src = newValue;
    }
  }
}

customElements.define('script-element', ScriptElement);
