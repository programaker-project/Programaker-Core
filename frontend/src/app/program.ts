export class ProgramMetadata {
    id: string;
    name: string;
    link: string;
}

export type ProgramType = 'scratch_program';

export class ProgramContent extends ProgramMetadata {
    program_type: ProgramType;
    program_content: any;
}
