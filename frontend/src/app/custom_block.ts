export class BlockArgument {
    type: string;
    default_value: string;
};

export class CustomBlock {
    function_name: string;
    message: string;
    arguments: [BlockArgument];
};