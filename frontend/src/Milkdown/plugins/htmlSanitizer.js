import { $remark } from '@milkdown/utils';
import { visit } from 'unist-util-visit';

const replacementForHtmlNode = (node) => {
    const raw = typeof node.value === 'string' ? node.value.trim() : '';
    if (!raw) {
        return [];
    }

    const normalized = raw.toLowerCase();
    if (normalized === '<br>' || normalized === '<br/>' || normalized === '<br />') {
        return [
            {
                type: 'text',
                value: '  \n',
            },
        ];
    }

    const iframeMatch = /<iframe[^>]*src=["']([^"']+)["'][^>]*>(?:<\/iframe>)?/i.exec(raw);
    if (iframeMatch) {
        const url = iframeMatch[1];
        return [
            {
                type: 'link',
                url,
                title: null,
                children: [
                    {
                        type: 'text',
                        value: url,
                    },
                ],
            },
        ];
    }

    return [];
};

const replacementForBreakNode = () => [
    {
        type: 'text',
        value: '  \n',
    },
];

export const htmlSanitizer = $remark(
    'htmlSanitizer',
    () => () => (tree) => {
        visit(tree, (node, index, parent) => {
            if (!parent || typeof index !== 'number') {
                return;
            }

            if (node.type === 'html') {
                const replacementNodes = replacementForHtmlNode(node);
                parent.children.splice(index, 1, ...replacementNodes);
                return index + replacementNodes.length;
            }

            if (node.type === 'break') {
                const replacementNodes = replacementForBreakNode();
                parent.children.splice(index, 1, ...replacementNodes);
                return index + replacementNodes.length;
            }
        });
    }
);

