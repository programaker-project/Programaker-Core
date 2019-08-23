export class ProgramMetadata {
    id: string;
    name: string;
    link: string;
}

export type ProgramType = 'scratch_program';

export class ProgramContent extends ProgramMetadata {
    type: ProgramType;
    parsed: any;
    orig: any;
    owner: string;

    constructor (metadata: ProgramMetadata, parsed: any, orig: any, type: ProgramType) {
      super();

      this.id = metadata.id;
      this.link = metadata.link;
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
