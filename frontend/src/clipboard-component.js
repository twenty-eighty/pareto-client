'use strict';

class ClipboardComponent extends HTMLElement {

  constructor() {
    super();
  }

  connectedCallback() {
    // console.log('Custom element added to page.');

    var thisComponent = this;

    var buttonElement = document.getElementById(this.copyButtonId);
    if (buttonElement == undefined) {
      return;
    }

    buttonElement.addEventListener("click", function (event) {
      if (navigator.clipboard) {
        thisComponent.copyToClipboardApi(thisComponent.textContents);
      } else {
        thisComponent.copyToClipboardOldStyle(thisComponent.textContents);
      }
    });
  }

  copyToClipboardApi(str) {
    navigator.clipboard.writeText(str).then(() => {
      this.dispatchEvent(new CustomEvent('copiedToClipboard', { detail: true }));
    })
  }

  copyToClipboardOldStyle(str) {
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
    this.dispatchEvent(new CustomEvent('copiedToClipboard', { detail: true }));
  };

  disconnectedCallback() {
    // console.log('Custom element removed from page.');
  }

  adoptedCallback() {
    // console.log('Custom element moved to new page.');
  }

  set buttonId(content) {
    this.copyButtonId = content;
  }

  set copyContent(content) {
    this.textContents = content;
  }
}

customElements.define('js-clipboard-component', ClipboardComponent);
