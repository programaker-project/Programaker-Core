import { ToolboxDescription } from '../base_toolbox_description';
import { UI_ICON } from '../definitions';
import { SimpleButtonOnClick, SimpleButtonRenderer } from './renderers/simple_button';
import { SimpleOutputOnClick, SimpleOutputRenderer } from './renderers/simple_output';

export const UiToolboxDescription: ToolboxDescription = [
    {
        id: 'basic_UI',
        name: 'Basic UI',
        blocks: [
            {
                icon: UI_ICON,
                type: 'ui_flow_block',
                'id': 'simple_button',
                renderer: SimpleButtonRenderer,
                onclick: SimpleButtonOnClick,
                outputs: [
                    {
                        type: "pulse",
                    },
                ]
            },
            {
                icon: UI_ICON,
                type: 'ui_flow_block',
                'id': 'simple_output',
                renderer: SimpleOutputRenderer,
                onclick: SimpleOutputOnClick,
                inputs: [
                    {
                        type: "any",
                    },
                ]
            },
        ]
    }
];
