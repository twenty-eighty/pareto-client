
class AutoResizeTextarea extends HTMLElement {
  constructor() {
    super();
    const shadow = this.attachShadow({ mode: 'open' });

    // Create a textarea element
    this.textarea = document.createElement('textarea');
    this.textarea.style.borderWidth = '0px';
    this.textarea.style.boxSizing = 'border-box';
    this.textarea.style.color = this.color;
    this.textarea.style['background-color'] = this.backgroundcolor;
    this.textarea.style.outline = 'none';
    this.textarea.style.overflow = 'hidden';
    this.textarea.style.paddingLeft = '20px';
    this.textarea.style.resize = 'none';
    this.textarea.style.width = '100%';
    this.textarea.maxlength = 200;
    this.textarea.rows = 1;

    // Append textarea to shadow DOM
    shadow.appendChild(this.textarea);

    // Auto-resize on input
    this.textarea.addEventListener('input', () => {
      this._resize();
      this._emitValue();
    });

    // Initial resize
    this._resize();
  }

  // Define observed attributes for syncing with Elm
  static get observedAttributes() {
    return ['value', 'color', 'backgroundcolor', 'placeholder', 'fontfamily', 'fontsize', 'fontweight'];
  }

  // React to attribute changes
  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'value') {
      this.textarea.value = newValue || '';
      this._resize();
    } else if (name === 'color') {
      this.textarea.style.color = newValue || '';
    } else if (name === 'backgroundcolor') {
      this.textarea.style['background-color'] = newValue || '';
    } else if (name === 'fontfamily') {
      this.textarea.style.fontFamily = newValue || '';
    } else if (name === 'fontsize') {
      this.textarea.style.fontSize = newValue || '';
    } else if (name === 'fontweight') {
      this.textarea.style.fontWeight = newValue || '';
    } else if (name === 'placeholder') {
      this.textarea.placeholder = newValue || '';
    }
  }

  // Auto-resize method
  _resize() {
    this.textarea.style.height = 'auto';
    this.textarea.style.height = this.textarea.scrollHeight + 'px';
  }

  // Emit the value to the outside world via a custom event
  _emitValue() {
    this.dispatchEvent(new CustomEvent('input-change', {
      detail: { value: this.textarea.value }
    }));
  }

  // Property getter and setter for the 'value' attribute
  get color() {
    return this.textarea.style.color;
  }

  get backgroundcolor() {
    return this.textarea.style['background-color'];
  }

  get fontfamily() {
    return this.textarea.fontfamily;
  }

  get fontsize() {
    return this.textarea.fontsize;
  }

  get fontweight() {
    return this.textarea.fontweight;
  }

  get placeholder() {
    return this.textarea.placeholder;
  }

  get value() {
    return this.textarea.value;
  }

  set value(newValue) {
    this.setAttribute('value', newValue);
  }
}

customElements.define('auto-resize-textarea', AutoResizeTextarea);
