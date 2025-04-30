'use strict';

class ClipboardComponent extends HTMLElement {

  constructor() {
    super();
    console.log('ClipboardComponent constructor called');
  }

  connectedCallback() {
    console.log('ClipboardComponent connectedCallback called');
    var thisComponent = this;

    // Use MutationObserver to watch for changes in the DOM
    const observer = new MutationObserver((mutations) => {
      console.log('DOM mutation observed, looking for button with ID:', this.copyButtonId);
      const buttonElement = document.getElementById(this.copyButtonId);
      if (buttonElement) {
        console.log('Button found, setting up click handler');
        observer.disconnect();
        this.setupClickHandler(buttonElement);
      }
    });

    // Start observing the document with the configured parameters
    observer.observe(document.body, { 
      childList: true, 
      subtree: true 
    });

    // Also check immediately in case the button already exists
    const buttonElement = document.getElementById(this.copyButtonId);
    if (buttonElement) {
      console.log('Button found immediately, setting up click handler');
      observer.disconnect();
      this.setupClickHandler(buttonElement);
    }
  }

  setupClickHandler(buttonElement) {
    console.log('Setting up click handler for button:', buttonElement.id);
    const thisComponent = this;
    buttonElement.addEventListener("click", function (event) {
      console.log('Click event received on button:', this.id);
      event.preventDefault();
      event.stopPropagation();
      
      if (navigator.clipboard) {
        console.log('Using modern clipboard API');
        thisComponent.copyToClipboardApi(thisComponent.textContents);
      } else {
        console.log('Using fallback clipboard method');
        thisComponent.copyToClipboardOldStyle(thisComponent.textContents);
      }
    });
  }

  copyToClipboardApi(str) {
    console.log('Copying to clipboard:', str);
    navigator.clipboard.writeText(str).then(() => {
      console.log('Successfully copied to clipboard');
      this.dispatchEvent(new CustomEvent('copiedToClipboard', { detail: true }));
    }).catch(err => {
      console.error('Failed to copy to clipboard:', err);
    });
  }

  copyToClipboardOldStyle(str) {
    console.log('Using old-style clipboard copy for:', str);
    const el = document.createElement('textarea');
    el.value = str;
    el.setAttribute('readonly', '');
    el.style.position = 'absolute';
    el.style.left = '-9999px';
    document.body.appendChild(el);
    el.select();
    el.setSelectionRange(0, 99999);
    document.execCommand('copy');
    document.body.removeChild(el);
    console.log('Successfully copied using old method');
    this.dispatchEvent(new CustomEvent('copiedToClipboard', { detail: true }));
  };

  disconnectedCallback() {
    console.log('ClipboardComponent disconnected');
  }

  adoptedCallback() {
    console.log('ClipboardComponent adopted');
  }

  set buttonId(content) {
    console.log('Setting buttonId to:', content);
    this.copyButtonId = content;
  }

  set copyContent(content) {
    console.log('Setting copyContent to:', content);
    this.textContents = content;
  }
}

customElements.define('js-clipboard-component', ClipboardComponent);
