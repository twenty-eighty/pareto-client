import type { MilkdownPlugin } from '@milkdown/kit/ctx'
import { imageBlockSchema } from './schema'
import { remarkImageBlockPlugin } from './remark-plugin'
import { imageBlockView } from './view'
import { imageBlockConfig } from './config'

export * from './schema'
export * from './remark-plugin'
export * from './config'
export * from './view'

export const imageBlockComponent: MilkdownPlugin[] = [
  remarkImageBlockPlugin,
  imageBlockSchema,
  imageBlockView,
  imageBlockConfig,
].flat()
