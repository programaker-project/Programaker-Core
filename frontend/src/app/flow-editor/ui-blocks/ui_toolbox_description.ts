import { ToolboxDescription } from '../base_toolbox_description';
import { UI_ICON } from '../definitions';
import { SimpleButtonBuilder } from './renderers/simple_button';
import { SimpleOutputBuilder } from './renderers/simple_output';
import { ResponsivePageBuilder } from './renderers/responsive_page';

export const UiToolboxDescription: ToolboxDescription = [
    {
        id: 'basic_UI',
        name: 'Basic UI',
        blocks: [
            {
                icon: UI_ICON,
                type: 'ui_flow_block',
                id: 'simple_button',
                builder: SimpleButtonBuilder,
                outputs: [
                    {
                        type: "pulse",
                    },
                ]
            },
            {
                icon: UI_ICON,
                type: 'ui_flow_block',
                id: 'simple_output',
                builder: SimpleOutputBuilder,
                inputs: [
                    {
                        type: "any",
                    },
                ]
            },
            {
                icon: UI_ICON,
                type: 'ui_flow_block',
                subtype: 'container_flow_block',
                id: 'responsive_page_holder',
                builder: ResponsivePageBuilder,
            }
        ]
    }
];
