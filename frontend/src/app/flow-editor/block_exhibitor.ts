import { BlockManager } from './block_manager';
import { EnumValue } from './enum_direct_value';
import { Area2D, FlowBlock, InputPortDefinition, OutputPortDefinition, MessageType } from './flow_block';
import { DirectValue } from './direct_value';
import { uuidv4 } from './utils';

const SvgNS = "http://www.w3.org/2000/svg";
export type BlockGenerator = (manager: BlockManager, blockId: string) => FlowBlock;

export class BlockExhibitor implements BlockManager {
    private baseElement: HTMLElement;

    private element: SVGSVGElement;
    private block: FlowBlock;

    public static FromGenerator(generator: BlockGenerator, baseElement: HTMLElement) {
        const ex = new BlockExhibitor(baseElement);
        ex.init(generator);
        return ex;
    }

    private constructor(baseElement: HTMLElement) {
        this.baseElement = baseElement;
    }

    // Block manager interface
    onIoSelected(_block: FlowBlock,
                 _type: 'in'|'out',
                 _index: number,
                 _definition: InputPortDefinition | OutputPortDefinition,
                 _port_center: {x: number, y: number},
                ): void {
        // Do nothing
    }

    onInputsChanged(_block: FlowBlock,
                    _input_num: number,
                   ): void {
        // Do nothing
    }

    onSelectRequested(_block: FlowBlock,
                      _previous_value: string,
                      _values: EnumValue[],
                      _value_dict: {[key:string]: EnumValue},
                      _update: (new_value: string) => void) : void {
        // Do nothing
    }

    onRequestEdit(_block: DirectValue, _type: MessageType, _update: (value: string) => void): void {
        // Do nothing
    }

    onDropdownExtended(_block: FlowBlock,
                       _slot_id: string,
                       _previous_value: string,
                       _current_rect: Area2D,
                       _update: (new_value: string) => void,
                      ): void {
        console.warn('Dropdown extension not implemented on block exhibitor')
    }

    // Block exhibitor management
    private init(generator: BlockGenerator) {
        this.element = document.createElementNS(SvgNS, 'svg');
        this.element.setAttribute('class', 'block_renderer block_exhibitor');
        this.baseElement.appendChild(this.element);

        this.block = generator(this, uuidv4());
        this.block.render(this.element, {});

        const area = this.block.getBodyArea();

        this.block.moveBy({x: 5, y: 5}); // Move slightly, to allow all of the block shadow in

        // Move block into view
        this.element.style.width = area.width + 10 + 'px';
        this.element.style.height = area.height + 10 + 'px';
    }

    public getElement(): SVGSVGElement {
        return this.element;
    }

    public getInnerElementRect(): Area2D {
        return this.block.getBodyElement().getBoundingClientRect();
    }
}
