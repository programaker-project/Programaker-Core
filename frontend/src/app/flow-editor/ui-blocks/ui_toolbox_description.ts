import { ToolboxDescription } from '../base_toolbox_description';
import { UI_ICON } from '../definitions';
import { SimpleButtonRenderer, SimpleButtonOnClick } from './renderers/simple_button';

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
        ]
    }
];
