class Service {
    id: string;
    name: string;
    link: string;
}

class AvailableService extends Service {
    enabled: boolean;
}

class RequestInput {
    name: string;
    defaultValue: string;
}

class Request {
    method: string;
    url: string;
    inputs: RequestInput[];
    headers: RequestInput[];
}

interface ServiceEnableTagEntry {
    type: 'tag';
    tag: string;
    properties: { [key: string]: any; };
    content: ServiceEnableEntry[];
};

interface ServiceEnableTextEntry {
    type: 'text' | 'console';
    value: string;
};

type ServiceEnableEntry = ServiceEnableTextEntry | ServiceEnableTagEntry;
type ServiceEnableType = 'message' | 'form';

interface ServiceEnableMessage {
    type: ServiceEnableType;
    value: {
        form: ServiceEnableEntry[]
    },
    metadata?: any,
}


type ServiceEnableHowTo = ServiceEnableMessage;

export {
    Service,
    AvailableService,
    RequestInput,
    Request,

    ServiceEnableHowTo,
    ServiceEnableMessage,
    ServiceEnableEntry,
    ServiceEnableType,
};
