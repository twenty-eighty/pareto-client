export const imageBlockTranslations = {
  en: {
    inlineUploadButton: () => 'Upload',
    inlineUploadPlaceholderText: 'or paste the image link ...',
    inlineConfirmButton: () => 'Confirm',
    blockUploadButton: () => 'Upload file',
    blockUploadPlaceholderText: 'or paste the image link ...',
    blockCaptionPlaceholderText: 'Image caption',
    blockConfirmButton: () => 'Confirm',
  },
  de: {
    inlineUploadButton: () => 'Datei auswählen',
    inlineUploadPlaceholderText: 'oder Bildlink einfügen ...',
    inlineConfirmButton: () => 'Bestätigen',
    blockUploadButton: () => 'Datei auswählen',
    blockUploadPlaceholderText: 'oder Bildlink einfügen ...',
    blockCaptionPlaceholderText: 'Bildbeschreibung',
    blockConfirmButton: () => 'Bestätigen',
  },
  fr: {
    inlineUploadButton: () => 'Choisir un fichier',
    inlineUploadPlaceholderText: 'ou collez le lien de l’image ...',
    inlineConfirmButton: () => 'Confirmer',
    blockUploadButton: () => 'Choisir un fichier',
    blockUploadPlaceholderText: 'ou collez le lien de l’image ...',
    blockCaptionPlaceholderText: 'Légende de l’image',
    blockConfirmButton: () => 'Confirmer',
  },
  it: {
    inlineUploadButton: () => 'Seleziona file',
    inlineUploadPlaceholderText: 'oppure incolla il link dell’immagine ...',
    inlineConfirmButton: () => 'Conferma',
    blockUploadButton: () => 'Seleziona file',
    blockUploadPlaceholderText: 'oppure incolla il link dell’immagine ...',
    blockCaptionPlaceholderText: 'Didascalia immagine',
    blockConfirmButton: () => 'Conferma',
  },
  pt: {
    inlineUploadButton: () => 'Selecionar arquivo',
    inlineUploadPlaceholderText: 'ou cole o link da imagem ...',
    inlineConfirmButton: () => 'Confirmar',
    blockUploadButton: () => 'Selecionar arquivo',
    blockUploadPlaceholderText: 'ou cole o link da imagem ...',
    blockCaptionPlaceholderText: 'Legenda da imagem',
    blockConfirmButton: () => 'Confirmar',
  },
  pl: {
    inlineUploadButton: () => 'Wybierz plik',
    inlineUploadPlaceholderText: 'lub wklej link do obrazu ...',
    inlineConfirmButton: () => 'Zatwierdź',
    blockUploadButton: () => 'Wybierz plik',
    blockUploadPlaceholderText: 'lub wklej link do obrazu ...',
    blockCaptionPlaceholderText: 'Podpis zdjęcia',
    blockConfirmButton: () => 'Zatwierdź',
  },
  ru: {
    inlineUploadButton: () => 'Выбрать файл',
    inlineUploadPlaceholderText: 'или вставьте ссылку на изображение ...',
    inlineConfirmButton: () => 'Подтвердить',
    blockUploadButton: () => 'Выбрать файл',
    blockUploadPlaceholderText: 'или вставьте ссылку на изображение ...',
    blockCaptionPlaceholderText: 'Подпись к изображению',
    blockConfirmButton: () => 'Подтвердить',
  },
  es: {
    inlineUploadButton: () => 'Seleccionar archivo',
    inlineUploadPlaceholderText: 'o pega el enlace de la imagen ...',
    inlineConfirmButton: () => 'Confirmar',
    blockUploadButton: () => 'Seleccionar archivo',
    blockUploadPlaceholderText: 'o pega el enlace de la imagen ...',
    blockCaptionPlaceholderText: 'Descripción de la imagen',
    blockConfirmButton: () => 'Confirmar',
  },
  sv: {
    inlineUploadButton: () => 'Välj fil',
    inlineUploadPlaceholderText: 'eller klistra in bildlänken ...',
    inlineConfirmButton: () => 'Bekräfta',
    blockUploadButton: () => 'Välj fil',
    blockUploadPlaceholderText: 'eller klistra in bildlänken ...',
    blockCaptionPlaceholderText: 'Bildtext',
    blockConfirmButton: () => 'Bekräfta',
  },
};

export function getImageBlockTranslations(language) {
  const code = (language || 'en').slice(0, 2).toLowerCase();
  const translations = imageBlockTranslations[code] || imageBlockTranslations.en;

  return {
    ...translations,
    uploadButton: translations.blockUploadButton || translations.inlineUploadButton,
    confirmButton: translations.blockConfirmButton || translations.inlineConfirmButton,
    uploadPlaceholderText:
      translations.blockUploadPlaceholderText || translations.inlineUploadPlaceholderText,
    captionPlaceholderText: translations.blockCaptionPlaceholderText,
  };
}

