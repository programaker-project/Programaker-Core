import { OnIOSelected, OnInputsChanged, OnDropdownExtended } from './flow_block';

export interface BlockManager {
    onIoSelected: OnIOSelected;
    onInputsChanged: OnInputsChanged;
    onDropdownExtended: OnDropdownExtended;

}
