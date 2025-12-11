import debug from 'debug';

const clipboardLog = debug('pareto:clipboard-component');

class ClipboardComponent extends HTMLElement {
  constructor() {
    super();
    clipboardLog('constructor called');
    this.copyButtonId = null;
    this.textContents = '';
    this.handleDocumentClick = this.handleDocumentClick.bind(this);
  }

  connectedCallback() {
    clipboardLog('connectedCallback');
    document.addEventListener('click', this.handleDocumentClick, true);
  }

  disconnectedCallback() {
    clipboardLog('disconnectedCallback');
    document.removeEventListener('click', this.handleDocumentClick, true);
  }

  adoptedCallback() {
    clipboardLog('adoptedCallback');
  }

  set buttonId(content) {
    clipboardLog('buttonId set', content);
    this.copyButtonId = content;
  }

  get buttonId() {
    return this.copyButtonId;
  }

  set copyContent(content) {
    clipboardLog('copyContent set', content);
    this.textContents = content;
  }

  get copyContent() {
    return this.textContents;
  }

  handleDocumentClick(event) {
    if (!this.copyButtonId) return;

    const path = event.composedPath ? event.composedPath() : [];
    const matches = path.some((node) => node && node.id === this.copyButtonId);
    if (!matches) {
      return;
    }

    clipboardLog('click received', this.copyButtonId);
    event.preventDefault();
    event.stopPropagation();

    if (navigator.clipboard) {
      this.copyToClipboardApi(this.textContents);
    } else {
      this.copyToClipboardOldStyle(this.textContents);
    }
  }

  copyToClipboardApi(str) {
    clipboardLog('copying via Clipboard API', str);
    navigator.clipboard.writeText(str).then(() => {
      clipboardLog('copy success');
      this.dispatchEvent(new CustomEvent('copiedToClipboard', { detail: true }));
    }).catch(err => {
      clipboardLog('copy failed', err);
    });
  }

  copyToClipboardOldStyle(str) {
    clipboardLog('copying via fallback', str);
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
    clipboardLog('fallback copy success');
    this.dispatchEvent(new CustomEvent('copiedToClipboard', { detail: true }));
  }
}

customElements.define('js-clipboard-component', ClipboardComponent);
