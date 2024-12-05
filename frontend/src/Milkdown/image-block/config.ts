import { $ctx } from '@milkdown/utils'
import { html } from 'atomico'
import { withMeta } from '../__internal__/meta'

export interface ImageBlockConfig {
  imageIcon: () => ReturnType<typeof html> | string | HTMLElement
  captionIcon: () => ReturnType<typeof html> | string | HTMLElement
  uploadButton: () => ReturnType<typeof html> | string | HTMLElement
  confirmButton: () => ReturnType<typeof html> | string | HTMLElement
  uploadPlaceholderText: string
  captionPlaceholderText: string
  onUpload: (file: File) => Promise<string>
  onClickUploader: () => Promise<string>
}

export const defaultImageBlockConfig: ImageBlockConfig = {
  imageIcon: () => '🌌',
  captionIcon: () => '💬',
  uploadButton: () => html`Select file`,
  confirmButton: () => html`Confirm ⏎`,
  uploadPlaceholderText: 'or paste the image link ...',
  captionPlaceholderText: 'Image caption',
  onUpload: file => Promise.resolve(URL.createObjectURL(file)),
  onClickUploader: () => Promise.resolve("")
}

export const imageBlockConfig = $ctx(defaultImageBlockConfig, 'imageBlockConfigCtx')

withMeta(imageBlockConfig, {
  displayName: 'Config<image-block>',
  group: 'ImageBlock',
})
