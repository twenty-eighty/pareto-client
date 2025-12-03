const LOCALE_FILES = {
    en: 'milkdown-en_US.json',
    de: 'milkdown-de_DE.json',
    fr: 'milkdown-fr_FR.json',
    it: 'milkdown-it_IT.json',
    pt: 'milkdown-pt_PT.json',
    pl: 'milkdown-pl_PL.json',
    ru: 'milkdown-ru_RU.json',
    es: 'milkdown-es_ES.json',
    sv: 'milkdown-sv_SE.json',
};

const FALLBACK_LOCALE = {
    imageBlock: {
        inlineUploadButton: 'Upload',
        inlineUploadPlaceholderText: 'or paste the link ...',
        inlineConfirmButton: 'Confirm',
        blockUploadButton: 'Upload',
        blockUploadPlaceholderText: 'or paste the link ...',
        blockCaptionPlaceholderText: 'Caption',
        blockConfirmButton: 'Confirm',
    },
    blockEdit: {
        textGroup: {
            label: 'Text',
            text: { label: 'Text' },
            h1: { label: 'Heading 1' },
            h2: { label: 'Heading 2' },
            h3: { label: 'Heading 3' },
            h4: { label: 'Heading 4' },
            h5: { label: 'Heading 5' },
            h6: { label: 'Heading 6' },
            quote: { label: 'Quote' },
            divider: { label: 'Divider' },
        },
        listGroup: {
            label: 'List',
            bulletList: { label: 'Bullet list' },
            orderedList: { label: 'Numbered list' },
            taskList: { label: 'Task list' },
        },
        advancedGroup: {
            label: 'Advanced',
            image: { label: 'Image' },
            codeBlock: { label: 'Code' },
            table: { label: 'Table' },
            math: { label: 'Math' },
        },
    },
    placeholder: 'Please enter...',
    toolbar: {
        paragraph: 'Paragraph',
        heading1: 'Heading 1',
        heading2: 'Heading 2',
        heading3: 'Heading 3',
        quote: 'Quote',
        bulletList: 'Bullet list',
        orderedList: 'Numbered list',
        codeBlock: 'Code block',
    },
};

const localeCache = {};

const normalizeCode = (language) => (language || 'en').slice(0, 2).toLowerCase();

const fetchLocale = async (code) => {
    if (localeCache[code]) {
        return localeCache[code];
    }

    const fileName = LOCALE_FILES[code];
    if (!fileName) {
        throw new Error(`Unknown locale code "${code}"`);
    }

    const response = await fetch(`/translations/${fileName}`);
    if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
    }
    const data = await response.json();
    localeCache[code] = data;
    return data;
};

export const loadMilkdownLocale = async (language) => {
    const code = normalizeCode(language);
    const primaryCode = LOCALE_FILES[code] ? code : 'en';

    try {
        return await fetchLocale(primaryCode);
    } catch (error) {
        console.warn(`[milkdown] failed to load locale "${primaryCode}", falling back to English`, error);
        if (primaryCode !== 'en') {
            try {
                return await fetchLocale('en');
            } catch (fallbackError) {
                console.error('[milkdown] failed to load fallback English locale', fallbackError);
            }
        }
        return FALLBACK_LOCALE;
    }
};

