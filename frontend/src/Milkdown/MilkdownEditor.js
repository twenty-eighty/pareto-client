
import { Crepe } from '@milkdown/crepe';
// import { ImageBlockFeatureConfig } from './feature/index';
import { imageBlockComponent } from './image-block';
import { imageBlockConfig } from './image-block/config'
import "@milkdown/crepe/theme/frame.css";
import "@milkdown/crepe/theme/common/style.css";

/*
import { defaultValueCtx, Editor, rootCtx } from '@milkdown/kit/core';

import { block } from '@milkdown/plugin-block'
import { clipboard } from '@milkdown/plugin-clipboard';
import { cursor } from '@milkdown/plugin-cursor';
import { emoji } from '@milkdown/plugin-emoji';
import { history } from '@milkdown/kit/plugin/history';
import { upload } from '@milkdown/plugin-upload';
import { imageInlineComponent } from '@milkdown/kit/component/image-inline';
import { listItemBlockComponent } from '@milkdown/kit/component/list-item-block';
import { tableBlock, tableBlockConfig } from '@milkdown/kit/component/table-block'
import { getMarkdown } from '@milkdown/utils';
*/
// import { imageBlockComponent } from '@milkdown/kit/component/image-block'
import { imageInlineComponent } from '@milkdown/kit/component/image-inline'

import { listener, listenerCtx } from '@milkdown/plugin-listener';
import { commonmark } from '@milkdown/preset-commonmark';
import { nord } from '@milkdown/theme-nord'
import '@milkdown/theme-nord/style.css'

// We have some themes for you to choose

import { BlossomClient } from "blossom-client-sdk/client";


class ElmMilkdownEditor extends HTMLElement {
    /*************  ✨ Codeium Command ⭐  *************/
    /**
     * Create a new MilkdownEditor element.
     *
     * The component will pick up the content from the 'content' attribute.
     *
     * @param {string} [content=''] - Initial content of the editor.
     */
    /******  901380eb-22ff-4e1a-8553-35557df4502b  *******/
    constructor() {
        super();
        this._editor = null;
        this._element = null;
        this._content = this.getAttribute('content') || '';
        this._caption = '';
        this._alt = '';
        this._imageSelectedResolve = null;
    }

    connectedCallback() {
        if (!this._element) {
            this._initElement();
        }
        const destroy = this.getAttribute('destroy');
        if (destroy === 'true' && this._editor) {
            this._editor.destroy();
            this._editor = null;
        }
        if (!this._editor) {
            this._initEditor();
        }
    }

    disconnectedCallback() {
        if (this._editor) {
            this._editor.destroy();
            this._editor = null;
        }
    }

    static get observedAttributes() {
        return ['content', 'selectedfile', 'height', 'theme', 'destroy'];
    }

    attributeChangedCallback(attrName, oldValue, newValue) {
        if (attrName === 'content') {
            this._content = newValue;
            this._setContent(newValue);
        } else if (attrName === 'selectedfile') {
            const decoded = JSON.parse(newValue)
            const url = decoded.url;
            if (url != undefined) {
                console.log("Selected file ", decoded);
                const caption = decoded.caption;
                const alt = decoded.alt;

                this._imageSelectedResolve({ url: url, caption: caption, alt: alt });
                this.dispatchEvent(new CustomEvent('receivedSelectedFile'));
            }
        } else if (attrName === 'height') {
            if (this._element) {
                this._element.style.height = newValue;
            }
        } else if (attrName === 'theme') {
            if (this._element) {
                this._setTheme(newValue);
            }
        } else if (attrName === 'destroy') {
            if (newValue === 'true' && this._editor) {
                this._editor.destroy();
                this._editor = null;
            }
        }
    }

    _initElement() {
        this._element = document.createElement('div');
        const height = this.getAttribute('height');
        if (height) {
            this._element.style.height = height;
        }
        this.appendChild(this._element);
    }

    _getOptions() {
        return {
            initialValue: this.getAttribute('content') || '',
        };
    }

    _initEditor() {
        const options = this._getOptions();
        const themeAttr = this.getAttribute('theme') || 'light';
        this._element.setAttribute('data-theme', themeAttr);

        // /*
        const language = 'DE';

        const crepe = new Crepe({
            root: this._element,
            defaultValue: this._content,
            features: {
                [Crepe.Feature.ImageBlock]: false,
            },
            featureConfigs: {
                // we don't need all these programming languages
                [Crepe.Feature.CodeMirror]: {
                    languages: []
                },
                [Crepe.Feature.ImageBlock]: {
                    inlineUploadButton: () => language === 'JA' ? 'アップロード' : 'Upload',
                    inlineUploadPlaceholderText: language === 'JA' ? 'またはリンクを貼り付ける' : 'or paste link',
                    inlineConfirmButton: () => language === 'JA' ? '確認' : 'Confirm',
                    blockUploadButton: () => language === 'JA' ? 'ファイルをアップロード' : 'Upload file',
                    blockUploadPlaceholderText: language === 'JA' ? 'またはリンクを貼り付ける' : 'or paste link',
                    blockCaptionPlaceholderText: language === 'JA' ? '画像の説明を書く...' : 'Write Image Caption',
                    blockConfirmButton: () => language === 'JA' ? '確認' : 'Confirm',
                }
            }
        });

        crepe.editor
            .config(nord)
            .use(commonmark)
            .use(listener)
            .use(imageBlockComponent);

        crepe.create().then((editor) => {

            editor.remove(imageInlineComponent);
            // */

            /*
            const milkdown = Editor
                .make()
                .config((ctx) => {
                    ctx.set(rootCtx, this._element)
                    ctx.set(defaultValueCtx, this._content)
                })
                .config(nord)
                .use(commonmark)
                .use(listener)
                .use(clipboard)
                .use(history)
                .use(imageBlockComponent)
                .use(imageInlineComponent)
                .use(listItemBlockComponent)
    
                .use(tableBlock)
                .use(block)
                .use(cursor)
                .use(emoji)
                .use(upload)
    
                .create()
                .then((editor) => {
                */

            this._editor = editor;
            editor.action((ctx) => {

                const listener = ctx.get(listenerCtx);

                listener.markdownUpdated((ctx, markdown) => {
                    this.dispatchEvent(new CustomEvent('change', { detail: { content: markdown } }));
                });
                listener.focus(() => {
                    this.dispatchEvent(new CustomEvent('focus'));
                });
                listener.blur(() => {
                    this.dispatchEvent(new CustomEvent('blur'));
                });

                ctx.update(imageBlockConfig.key, defaultConfig => ({
                    ...defaultConfig,
                    onClickUploader: async () => {
                        console.log("image upload");
                        this.dispatchEvent(new CustomEvent('filerequest'));

                        return new Promise((resolve, reject) => {
                            this._imageSelectedResolve = resolve;
                        });
                    }
                })
                );
            });
            this.dispatchEvent(new CustomEvent('load'));
        });
    }

    _setContent(content) {
        if (this._editor) {
            //            this._editor.action(setMarkdown(content));
        }
    }

    _setTheme(theme) {
        if (theme !== 'nord' && theme !== 'nord-dark') {
            theme = 'nord';
        }
        this._element.setAttribute('data-theme', theme);
        this.crepe._setTheme(theme);
    }
}

customElements.define('elm-milkdown-editor', ElmMilkdownEditor);
