import { OnRequestEdit } from './direct_value';
import { OnSelectRequested } from './enum_direct_value';
import { OnDropdownExtended, OnInputsChanged, OnIOSelected } from './flow_block';

export interface BlockManager {
    onIoSelected: OnIOSelected;
    onInputsChanged: OnInputsChanged;
    onDropdownExtended: OnDropdownExtended;
    onRequestEdit: OnRequestEdit;
    onSelectRequested: OnSelectRequested;
}
