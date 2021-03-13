export type VisibilityEnum = 'public' | 'shareable' | 'private';

export class ProgramMetadata {
    id: string;
    name: string;
    enabled: boolean;
    visibility: VisibilityEnum;
    type: string;
    bridges_in_use: string[];
}

export type ProgramType = 'scratch_program' | 'flow_program' | 'spreadsheet_program';
export type OwnerType = 'user' | 'group';

export class ProgramContent extends ProgramMetadata {
    type: ProgramType;
    parsed: any;
    orig: any;
    owner: string;
    owner_full: { type: OwnerType, id: string};
    checkpoint?: any;

    "readonly"?: boolean;
    can_admin?: boolean;

    constructor (metadata: ProgramMetadata, parsed: any, orig: any, type: ProgramType) {
      super();

      this.id = metadata.id;
      this.name = metadata.name;

      this.parsed = parsed;
      this.orig = orig;
      this.type = type;
    }
}

export class ScratchProgram extends ProgramContent {
    constructor(metadata: ProgramMetadata, parsed: any, orig: any) {
       super(metadata, parsed, orig, 'scratch_program');
    }
}

export class FlowProgram extends ProgramContent {
    constructor(metadata: ProgramMetadata, parsed: any, orig: any) {
        super(metadata, parsed, orig, 'flow_program');
    }
}

export class SpreadsheetProgram extends ProgramContent {
    constructor(metadata: ProgramMetadata, parsed: any, orig: any) {
        super(metadata, parsed, orig, 'spreadsheet_program');
    }
}

export interface ProgramLogEntry {
    program_id: string,
    thread_id: string | 'none',
    user_id: string | 'none',
    block_id: string | undefined,
    severity: 'error' | 'debug' | 'warning',
    event_data: any,
    event_message: string,
    event_time: number,
};

export type ProgramInfoUpdate = { type: "program_log" | "debug_log", value: ProgramLogEntry };

export type ProgramEditorEventType = 'blockly_event' | 'flow_event' | 'cursor_event' | 'save_checkpoint' | 'add_editor' | 'remove_editor' | 'ready';

export type ProgramEditorEventValue = { type: ProgramEditorEventType, value: any, save?: boolean };

export type ProgramEditorEvent = { type: "editor_event", value: ProgramEditorEventValue };
