const toButton = (label) => () => label;

export function getImageBlockTranslations(locale) {
    const data = locale.imageBlock;

    return {
        inlineUploadButton: toButton(data.inlineUploadButton),
        inlineUploadPlaceholderText: data.inlineUploadPlaceholderText,
        inlineConfirmButton: toButton(data.inlineConfirmButton),
        blockUploadButton: toButton(data.blockUploadButton),
        blockUploadPlaceholderText: data.blockUploadPlaceholderText,
        blockCaptionPlaceholderText: data.blockCaptionPlaceholderText,
        blockConfirmButton: toButton(data.blockConfirmButton),
        uploadButton: toButton(data.blockUploadButton),
        confirmButton: toButton(data.blockConfirmButton),
        uploadPlaceholderText: data.blockUploadPlaceholderText,
        captionPlaceholderText: data.blockCaptionPlaceholderText,
    };
}

