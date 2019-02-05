class Service {
    id: number;
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

interface ServiceEnableFormTagEntry {
    type: 'tag';
    tag: string;
    properties: {[key: string]: any; };
    content: ServiceEnableFormEntry[];
};

interface ServiceEnableFormTextEntry {
    type: 'text' | 'console';
    value: string;
};

type ServiceEnableFormEntry = ServiceEnableFormTextEntry | ServiceEnableFormTagEntry;

interface ServiceEnableScriptedForm {
    type: 'message';
    value: {
        form: ServiceEnableFormEntry[]
    }
}


type ServiceEnableHowTo = ServiceEnableScriptedForm;

export {
    Service,
    AvailableService,
    RequestInput,
    Request,

    ServiceEnableHowTo,
    ServiceEnableScriptedForm,
    ServiceEnableFormEntry,
};
