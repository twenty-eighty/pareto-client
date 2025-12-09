
import { Crepe } from '@milkdown/crepe';
// import { ImageBlockFeatureConfig } from './feature/index';
import { imageBlockComponent } from './image-block';
import { imageBlockConfig } from './image-block/config'
import { loadMilkdownLocale } from './translations/locale';
import { getImageBlockTranslations } from './translations/image-block';
import { getBlockEditTranslations } from './translations/block-edit';
import { getPlaceholderText } from './translations/placeholder';
import { getToolbarTranslations } from './translations/toolbar';
import { commandsCtx, editorViewCtx } from '@milkdown/kit/core';
import {
    blockquoteSchema,
    bulletListSchema,
    clearTextInCurrentBlockCommand,
    codeBlockSchema,
    headingSchema,
    orderedListSchema,
    paragraphSchema,
    setBlockTypeCommand,
    wrapInBlockTypeCommand,
} from '@milkdown/kit/preset/commonmark';
import {
    liftListItemCommand,
    wrapInBulletListCommand,
    wrapInOrderedListCommand,
} from '@milkdown/preset-commonmark';
import { liftTarget } from '@milkdown/prose/transform';
import "@milkdown/crepe/theme/common/style.css";
import "@milkdown/crepe/theme/frame.css";

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

import { listener, listenerCtx } from '@milkdown/plugin-listener';
import { commonmark as presetCommonmark, remarkLineBreak, remarkPreserveEmptyLinePlugin } from '@milkdown/preset-commonmark';
import { nord } from '@milkdown/theme-nord'
import '@milkdown/theme-nord/style.css'
import { htmlSanitizer } from './plugins/htmlSanitizer';

// We have some themes for you to choose

import { BlossomClient } from "blossom-client-sdk/client";
import debug from 'debug';
import {
    bulletListIcon,
    codeIcon,
    h1Icon,
    h2Icon,
    h3Icon,
    orderedListIcon,
    quoteIcon,
    textIcon,
} from './icons/toolbar';

const quoteDebugLog = debug('milkdown:quote');

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
        this._theme = 'nord';
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
            this._initEditor().catch((error) => {
                console.error('Failed to initialize Milkdown editor', error);
            });
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
            this._setTheme(newValue);
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

    async _initEditor() {
        const options = this._getOptions();
        const themeAttr = this.getAttribute('theme') || 'light';
        this._element.setAttribute('data-theme', themeAttr);

        // /*
        const language = navigator.language || navigator.userLanguage || 'en';
        const locale = await loadMilkdownLocale(language);
        const toolbarLabels = getToolbarTranslations(locale);

        const withIconTitle = (icon, title) => {
            if (!icon || !title) {
                return icon;
            }
            const safeTitle = title.replace(/"/g, '&quot;');
            return `<span class="milkdown-toolbar__label" role="img" title="${safeTitle}" aria-label="${safeTitle}">${icon}</span>`;
        };

        const toolbarIcons = {
            paragraph: withIconTitle(textIcon, toolbarLabels.paragraph),
            heading1: withIconTitle(h1Icon, toolbarLabels.heading1),
            heading2: withIconTitle(h2Icon, toolbarLabels.heading2),
            heading3: withIconTitle(h3Icon, toolbarLabels.heading3),
            quote: withIconTitle(quoteIcon, toolbarLabels.quote),
            bulletList: withIconTitle(bulletListIcon, toolbarLabels.bulletList),
            orderedList: withIconTitle(orderedListIcon, toolbarLabels.orderedList),
            codeBlock: withIconTitle(codeIcon, toolbarLabels.codeBlock),
        };

        const filteredCommonmark =
            Array.isArray(presetCommonmark)
                ? presetCommonmark.filter(
                    (plugin) =>
                        plugin !== remarkLineBreak &&
                        plugin !== remarkLineBreak.plugin &&
                        plugin !== remarkLineBreak.options &&
                        plugin !== remarkPreserveEmptyLinePlugin &&
                        plugin !== remarkPreserveEmptyLinePlugin.plugin &&
                        plugin !== remarkPreserveEmptyLinePlugin.options
                )
                : presetCommonmark;


        const crepe = new Crepe({
            root: this._element,
            defaultValue: this._content,
            features: {
                [Crepe.Feature.Latex]: false,
                [Crepe.Feature.Toolbar]: true,
            },
            featureConfigs: {
                // we don't need all these programming languages
                [Crepe.Feature.CodeMirror]: {
                    languages: []
                },
                [Crepe.Feature.ImageBlock]: getImageBlockTranslations(locale),
                [Crepe.Feature.BlockEdit]: getBlockEditTranslations(locale),
                [Crepe.Feature.Placeholder]: {
                    text: getPlaceholderText(locale),
                },
                [Crepe.Feature.Toolbar]: {
                    buildToolbar: (builder) => {
                        const selectionAncestorDepth = (ctx, nodeType) => {
                            const view = ctx.get(editorViewCtx);
                            const selection = view?.state?.selection;
                            if (!selection) {
                                quoteDebugLog('no selection found');
                                return -1;
                            }
                            if (!nodeType) {
                                quoteDebugLog('missing nodeType');
                                return -1;
                            }
                            const nodeName = nodeType.name;
                            const ancestors = [];
                            const { $from } = selection;
                            for (let depth = $from.depth; depth >= 0; depth--) {
                                const currentNode = $from.node(depth);
                                const currentName = currentNode.type?.name;
                                ancestors.push({
                                    depth,
                                    name: currentName,
                                    attrs: currentNode.attrs,
                                });
                                if (currentName === nodeName) {
                                    quoteDebugLog('ancestors', ancestors);
                                    return depth;
                                }
                            }
                            quoteDebugLog(
                                'ancestors',
                                ancestors,
                                '| blockquote not found (looking for',
                                nodeName,
                                ')',
                            );
                            return -1;
                        };
                        const isSelectionInNode = (ctx, nodeType) =>
                            selectionAncestorDepth(ctx, nodeType);

                        const toggleList = ({ ctx, wrapCommand, listSchema, opposingSchema }) => {
                            const commands = ctx.get(commandsCtx);
                            const listType = listSchema.type(ctx);
                            const isInList = isSelectionInNode(ctx, listType) >= 0;
                            const isInOpposingList =
                                opposingSchema &&
                                isSelectionInNode(ctx, opposingSchema.type(ctx)) >= 0;

                            if (isInList) {
                                while (commands.call(liftListItemCommand.key)) {
                                    // keep lifting until we're out of the list
                                }
                            } else {
                                if (isInOpposingList) {
                                    while (commands.call(liftListItemCommand.key)) {
                                        // exit opposing list before applying new one
                                    }
                                }
                                commands.call(clearTextInCurrentBlockCommand.key);
                                commands.call(setBlockTypeCommand.key, {
                                    nodeType: paragraphSchema.type(ctx),
                                });
                                commands.call(wrapCommand.key);
                            }
                        };

                        const ensureNotList = (commands) => {
                            while (commands.call(liftListItemCommand.key)) {
                                // keep lifting until we exit the surrounding list
                            }
                        };

                        const liftNodeOfType = (ctx, nodeType) => {
                            const view = ctx.get(editorViewCtx);
                            if (!view) return false;
                            const { state, dispatch } = view;
                            const { $from, $to } = state.selection;
                            const nodeName = nodeType?.name;
                            if (!nodeName) {
                                quoteDebugLog('no nodeName for lift');
                                return false;
                            }
                            const range = $from.blockRange(
                                $to,
                                (node) => node.type?.name === nodeName,
                            );
                            quoteDebugLog('block range result', range);
                            if (!range) return false;
                            const target = liftTarget(range);
                            quoteDebugLog('lift target', target);
                            if (target == null) return false;
                            dispatch(state.tr.lift(range, target));
                            return true;
                        };

                        builder.addGroup('block', 'Block').addItem('paragraph', {
                            icon: toolbarIcons.paragraph,
                            active: () => false,
                            onRun: (ctx) => {
                                const commands = ctx.get(commandsCtx);
                                ensureNotList(commands);
                                const paragraph = paragraphSchema.type(ctx);
                                commands.call(clearTextInCurrentBlockCommand.key);
                                commands.call(setBlockTypeCommand.key, {
                                    nodeType: paragraph,
                                });
                            },
                        }).addItem('heading-1', {
                            icon: toolbarIcons.heading1,
                            active: () => false,
                            onRun: (ctx) => {
                                const commands = ctx.get(commandsCtx);
                                ensureNotList(commands);
                                const heading = headingSchema.type(ctx);
                                commands.call(clearTextInCurrentBlockCommand.key);
                                commands.call(setBlockTypeCommand.key, {
                                    nodeType: heading,
                                    attrs: { level: 1 },
                                });
                            },
                        }).addItem('heading-2', {
                            icon: toolbarIcons.heading2,
                            active: () => false,
                            onRun: (ctx) => {
                                const commands = ctx.get(commandsCtx);
                                ensureNotList(commands);
                                const heading = headingSchema.type(ctx);
                                commands.call(clearTextInCurrentBlockCommand.key);
                                commands.call(setBlockTypeCommand.key, {
                                    nodeType: heading,
                                    attrs: { level: 2 },
                                });
                            },
                        }).addItem('heading-3', {
                            icon: toolbarIcons.heading3,
                            active: () => false,
                            onRun: (ctx) => {
                                const commands = ctx.get(commandsCtx);
                                ensureNotList(commands);
                                const heading = headingSchema.type(ctx);
                                commands.call(clearTextInCurrentBlockCommand.key);
                                commands.call(setBlockTypeCommand.key, {
                                    nodeType: heading,
                                    attrs: { level: 3 },
                                });
                            },
                        }).addItem('quote', {
                            icon: toolbarIcons.quote,
                            active: () => false,
                            onRun: (ctx) => {
                                const commands = ctx.get(commandsCtx);
                                const blockquote = blockquoteSchema.type(ctx);
                                const depth = isSelectionInNode(ctx, blockquote);

                                ensureNotList(commands);
                                if (depth >= 0) {
                                        quoteDebugLog('inside quote depth', depth);
                                    liftNodeOfType(ctx, blockquote);
                                } else {
                                        quoteDebugLog('not inside quote, wrapping');
                                    commands.call(clearTextInCurrentBlockCommand.key);
                                    commands.call(wrapInBlockTypeCommand.key, {
                                        nodeType: blockquote,
                                    });
                                }
                            },
                        });

                        builder.addGroup('list', 'List').addItem('bullet-list', {
                            icon: toolbarIcons.bulletList,
                            active: () => false,
                            onRun: (ctx) => {
                                toggleList({
                                    ctx,
                                    wrapCommand: wrapInBulletListCommand,
                                    listSchema: bulletListSchema,
                                    opposingSchema: orderedListSchema,
                                });
                            },
                        }).addItem('ordered-list', {
                            icon: toolbarIcons.orderedList,
                            active: () => false,
                            onRun: (ctx) => {
                                toggleList({
                                    ctx,
                                    wrapCommand: wrapInOrderedListCommand,
                                    listSchema: orderedListSchema,
                                    opposingSchema: bulletListSchema,
                                });
                            },
                        });

                        builder.addGroup('advanced', 'Advanced').addItem('code-block', {
                            icon: toolbarIcons.codeBlock,
                            active: () => false,
                            onRun: (ctx) => {
                                const commands = ctx.get(commandsCtx);
                                const codeBlock = codeBlockSchema.type(ctx);
                                commands.call(clearTextInCurrentBlockCommand.key);
                                commands.call(setBlockTypeCommand.key, {
                                    nodeType: codeBlock,
                                });
                            },
                        });
                    },
                },
            }
        });

        await crepe.editor.remove([
            remarkLineBreak,
            remarkLineBreak.plugin,
            remarkLineBreak.options,
            remarkPreserveEmptyLinePlugin,
            remarkPreserveEmptyLinePlugin.plugin,
            remarkPreserveEmptyLinePlugin.options,
        ]);

        crepe.editor
            .config(nord)
            .use(filteredCommonmark)
            .use(htmlSanitizer)
            .use(listener)
            .use(imageBlockComponent);

        // crepe.theme(this._theme)

        crepe.create().then((editor) => {

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
                    ...getImageBlockTranslations(locale),
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
        this._theme = theme;

        if (this._element) {
            this._element.setAttribute('data-theme', theme);
        }
        if (this.crepe) {
            this.crepe._setTheme(theme);
        }
    }
}

customElements.define('elm-milkdown-editor', ElmMilkdownEditor);
