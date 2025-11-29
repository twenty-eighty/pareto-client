const placeholderTranslations = {
  en: 'Please enter...',
  de: 'Bitte eingeben...',
  fr: 'Veuillez saisir...',
  it: 'Inserisci...',
  pt: 'Digite...',
  pl: 'Wpisz...',
  ru: 'Пожалуйста, введите...',
  es: 'Por favor, escribe...',
  sv: 'Ange text...',
};

export function getPlaceholderText(language) {
  const code = (language || 'en').slice(0, 2).toLowerCase();
  return placeholderTranslations[code] || placeholderTranslations.en;
}

