import 'prismjs/themes/prism.css';
import '@toast-ui/editor/dist/toastui-editor.css';
import '@toast-ui/editor/dist/theme/toastui-editor-dark.css'
import '@toast-ui/editor-plugin-code-syntax-highlight/dist/toastui-editor-plugin-code-syntax-highlight.css';
// import '@toast-ui/editor-plugin-table-merged-cell/dist/toastui-editor-plugin-table-merged-cell.css';
// import tableMergedCell from '@toast-ui/editor-plugin-table-merged-cell';
import { Editor } from "@toast-ui/editor";
import codeSyntaxHighlight from '@toast-ui/editor-plugin-code-syntax-highlight/dist/toastui-editor-plugin-code-syntax-highlight-all.js';

const LOGLEVEL = 0

function log(level, s) {
    if (level <= LOGLEVEL) { console.log(s); }
}

function toString(s) { return JSON.stringify(s); }
class ElmToastUiEditor extends HTMLElement {
    constructor() {
        super();
    }

    _getOptions(element) {
        const options = {
            el: element,
            height: '600px',
            initialEditType: 'markdown',
            previewStyle: 'tab',
            usageStatistics: false,
            frontMatter: true,
            minHeight: '240px',
            toolbarItems:
                [['codeblock', 'code'],
                ['heading', 'bold', 'italic',],
                ['ul', 'ol'],
                ['quote'], ['scrollSync']
                ],
            plugins: [[codeSyntaxHighlight, { highlighter: Prism }]],
            placeholder: "",
            initialValue: "Hello from JavaScript default options.initialValue"
        }
        if (this.hasAttribute("height")) {
            options.height = this.getAttribute("height")
        }

        if (this.hasAttribute("preview-style")) {
            options.previewStyle = this.getAttribute("preview-style")
        }

        if (this.hasAttribute("initialvalue")) {
            options.initialValue = this.getAttribute("initialvalue")
        }
        if (this.hasAttribute("theme")) {
            options.theme = this.getAttribute("theme");
        }
        if (this.hasAttribute("mode")) {
            options.initialEditType = this.getAttribute("mode");
        }
        if (this.hasAttribute("zoom")) {
            options.zoom = this.getAttribute("zoom");
        }
        if (this.hasAttribute("destroy")) {
            log(5, "OPTS.DESTROY")
            options.destroy = this.getAttribute("destroy");
            log(5, `OPTS.DESTROY=${options.destroy}`)
        }
        options.events = {
            "change": this.onChange.bind(this),
            //"load": this.onLoad.bind(this),
            //"focus": this.onFocus.bind(this),
            //"blur": this.onBlur.bind(this)
        }
        return options
    }

    _getState() {
        if (!this._editor) { return; }
        return {
            mode: this._getMode(),
            content: this._editor.getMarkdown(),
            selection: this._editor.getSelection(),
            scrollpos: this._editor.getScrollTop()
        }
    }

    _sendStateEvent(str) {
        log(5, str);
        const state = this._getState();
        if (!state) { return; }
        this.dispatchEvent(new CustomEvent(str,
            {
                detail:
                    state

            }
        ))
    }


    onChange(e) {
        // log("js:onChange")
        this._sendStateEvent('change');
    }
    onLoad(e) {
        // log("js:onLoad");
        this._sendStateEvent('load');
    }
    onFocus(e) {
        // log("js:onFocus");
        this._sendStateEvent('focus');
    }
    onBlur(e) {
        // log("js:onBlur");
        this._sendStateEvent('blur_');
    }

    _setMainZoom(zoom) {
        let main = this._element.querySelector(".toastui-editor-ww-mode")
        main.style.setProperty("zoom", zoom);
    }

    _newEditor() {
        // console.log("js:_newEditor")
        let element = null;
        if (!element) { element = this._initElement(); }
        let options = this._getOptions(element);
        let editor = new Editor(options);
        // if (options.cursorToEnd) {
        //     editor.moveCursorToEnd(); //setMarkdown(, false);
        // } //else { editor.moveCursorToStart(); }
        // if (options.zoom) {
        //     this._setMainZoom(options.zoom);
        // }
        return editor;
    }
    _initElement() {
        // console.log("_initElement")
        this._element = document.createElement('div');
        this._element.id = 'toastui-editor-root';
        this.appendChild(this._element);
        return this._element;
    }

    connectedCallback() {
        log(5, "cc 1");
        if (!this._element) { this._initElement(); }
        let options = this._getOptions(this._element);
        if (options.destroy == "true") {
            if (this._editor) {
                log(6, "DESTRUCTION IMMINENT")
                this._editor.destroy()
                this._editor = null;
            }
        }
        if (!this._editor) {
            log(6, "NEW EDITOR INBOUND")
            this._editor = this._newEditor();
        }
        log(5, "cc 2");
        if (this.hasAttribute("zoom")) {
            log("cc 2.1");
            let zoom = this.getAttribute("zoom");
            this._setMainZoom(zoom);
        }
        log(5, "cc 3");

        // let main = this._element.querySelector(".toastui-editor-ww-mode")
        // main.style.setProperty("zoom", "1.2");

        let toolbar = this._element.querySelector(".toastui-editor-toolbar")
        if (toolbar) {
            toolbar.style.setProperty("zoom", "0.8");
        }
    }

    disconnectedCallback() {
        if (!this._editor) { log(5, "disconnected. noeditor"); return; }
        this._editor.off("load");
        this._editor.off("focus");
        this._editor.off("blur");
        this._editor.off("change");
        return;
    }

    attributeChangedCallback(attr, oldval, newval) {
        if (!this._editor) { return false; }
        switch (attr) {
            case "content":
                log(5, "content change");
                this._editor.setMarkdown(newval);
                return true;
            case "somethingelse":
                return true;
        }
        return false;
    }

    static get observedAttributes() { return ["content"]; }

    _getMode() {
        var mode = "wysiwyg"
        if (this._editor.isMarkdownMode()) {
            mode = "markdown"
        }
        return mode
    }


}
customElements.define('elm-toastui-editor', ElmToastUiEditor);