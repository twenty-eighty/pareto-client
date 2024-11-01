import { Crepe } from '@milkdown/crepe';
import { imageBlockConfig } from '@milkdown/kit/component/image-block'
import "@milkdown/crepe/theme/common/style.css";


// We have some themes for you to choose
import "@milkdown/crepe/theme/frame.css";

import { commonmark } from '@milkdown/preset-commonmark';
import { listener, listenerCtx } from '@milkdown/plugin-listener';
import { getMarkdown } from '@milkdown/utils';

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
        return ['content', 'height', 'theme', 'destroy'];
    }

    attributeChangedCallback(attrName, oldValue, newValue) {
        if (attrName === 'content') {
            this._content = newValue;
            this._setContent(newValue);
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

        const crepe = new Crepe({
            root: this._element,
            defaultValue: this._content,
        });

        crepe.editor
            .use(commonmark)
            .use(listener);

        crepe.create().then((editor) => {
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
                    onUpload: async (file) => {
                        console.log("image upload", file);

                        async function signer(event) {
                            return await window.nostr.signEvent(event);
                        }

                        const client = new BlossomClient("http://localhost:4884/", signer);

                        try {
                            // create an upload auth event
                            const server = "https://naughty-wood-53400.pktriot.net";
                            // const server = "http://localhost:4884/";
                            const uploadAuth = await client.getUploadAuth(file, "image in editor");

                            // encode it using base64
                            const encodedAuthHeader = BlossomClient.encodeAuthorizationHeader(uploadAuth);

                            BlossomClient.fetch = window.fetch.bind(window);

                            const res = await BlossomClient.uploadBlob(server, file, uploadAuth);

                            // check if successful
                            if (res.ok) {
                                console.log("Blob uploaded!");
                                return res.url;
                            }

                            return res.url; // Return the actual URL
                        } catch (error) {
                            console.error("Error uploading file:", error);
                            throw error; // Optionally rethrow or handle the error as needed
                        }

                        /*
                                                window.ndk.httpFetch = window.fetch.bind(window);
                                                // const nip96 = window.ndk.getNip96("cool-darkness-73116.pktriot.net");
                                                const nip96 = window.ndk.getNip96("files.sovbit.host");
                        
                                                const xhr = new XMLHttpRequest();
                                                // xhr.upload.addEventListener("progress", function(e) {
                                                // const percentComplete = e.loaded / e.total;
                                                // console.log(percentComplete);
                                                // });
                                                // const nip96 = ndk.getNip96("nostrcheck.me");
                                                // const blob = new Blob(["Hello, world!"], { type: "text/plain" });
                        
                                                try {
                                                    // Await the upload response
                                                    // const uploadResponse = await nip96.upload(file);
                                                    const uploadResponse = await nip96.xhrUpload(xhr, file);
                        
                                                    // Extract the URL from the response (assuming the response contains the URL)
                                                    const url = uploadResponse.url; // Adjust based on the actual structure of the response
                        
                                                    console.log("Uploaded file URL:", url);
                        
                                                    return url; // Return the actual URL
                                                } catch (error) {
                                                    console.error("Error uploading file:", error);
                                                    throw error; // Optionally rethrow or handle the error as needed
                                                }
                        */
                        return url;
                    }
                })
                );
            });
            this.dispatchEvent(new CustomEvent('load'));
        });

        /*
        Editor.make()
            .config((ctx) => {
                ctx.set(rootCtx, this._element);
                ctx.set(defaultValueCtx, options.initialValue);
            })
            .use(commonmark)
            .use(listener)
            .create()
            .then((editor) => {
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
                });
                this.dispatchEvent(new CustomEvent('load'));
            });
            */
    }

    _setContent(content) {
        if (this._editor) {
            //            this._editor.action(setMarkdown(content));
        }
    }

    _setTheme(theme) {
        if (theme !== 'light' && theme !== 'dark') {
            theme = 'light';
        }
        this._element.setAttribute('data-theme', theme);
    }
}

customElements.define('elm-milkdown-editor', ElmMilkdownEditor);
