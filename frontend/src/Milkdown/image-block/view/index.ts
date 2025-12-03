import DOMPurify from 'dompurify'

import { $view } from '@milkdown/utils'
import type { NodeViewConstructor } from '@milkdown/prose/view'
import type { Node } from '@milkdown/prose/model'
import { imageBlockSchema } from '../schema'
import { imageBlockConfig } from '../config'
import { withMeta } from '../../__internal__/meta'
import { defIfNotExists } from '../../__internal__/helper'
import type { ImageComponentProps } from './component'
import { ImageElement } from './component'

defIfNotExists('milkdown-image-block', ImageElement)

export const imageBlockView = $view(imageBlockSchema.node, (ctx): NodeViewConstructor => {
  return (initialNode, view, getPos) => {
    const dom = document.createElement('milkdown-image-block') as HTMLElement & ImageComponentProps
    dom.classList.add('milkdown-image-block')
    const config = ctx.get(imageBlockConfig.key)

    const bindAttrs = (node: Node) => {
      const proxy = config.proxyDomURL
      if (!proxy) {
        dom.src = node.attrs.src
      } else {
        const proxied = proxy(node.attrs.src)
        if (typeof proxied === 'string') {
          dom.src = proxied
        } else {
          proxied.then((url) => {
            dom.src = url
          }).catch(console.error)
        }
      }
      dom.ratio = node.attrs.ratio
      dom.caption = node.attrs.caption
      dom.readonly = !view.editable
    }

    bindAttrs(initialNode)
    dom.selected = false
    dom.setAttr = (attr, value) => {
      const pos = getPos()
      if (pos == null)
        return
      const sanitized = attr === 'src' ? DOMPurify.sanitize(value as string) : value
      view.dispatch(view.state.tr.setNodeAttribute(pos, attr, sanitized))
    }
    dom.config = config

    return {
      dom,
      update: (updatedNode) => {
        if (updatedNode.type !== initialNode.type)
          return false
        bindAttrs(updatedNode)
        return true
      },
      stopEvent: (e) => {
        if (e.target instanceof HTMLInputElement)
          return true
        return false
      },
      selectNode: () => {
        dom.selected = true
      },
      deselectNode: () => {
        dom.selected = false
      },
      destroy: () => {
        dom.remove()
      },
    }
  }
})

withMeta(imageBlockView, {
  displayName: 'NodeView<image-block>',
  group: 'ImageBlock',
})
