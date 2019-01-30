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

interface ServiceEnableFormEntry {
    type: 'text' | 'console';
    value: string;
};

interface ServiceEnableScriptedForm {
    type: 'scripted-form';
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
