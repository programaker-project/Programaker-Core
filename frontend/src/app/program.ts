export class Program {
    id: number;
    name: string;
}

export class ProgramExample {
    id: number;
    text: string;
}

export class UnpersistedProgramExample extends ProgramExample {
    failed: boolean;
}
