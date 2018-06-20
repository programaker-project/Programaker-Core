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

type ServiceEnableMethod = 'external';

class ServiceExtraTelegramInfo {
    token: string;
    bot_name: string;
}

class ServiceEnableHowTo extends Service {
    method: ServiceEnableMethod;
    extra: ServiceExtraTelegramInfo;
}

export {
    Service,
    AvailableService,
    RequestInput,
    Request,
    ServiceEnableHowTo,
    ServiceEnableMethod,
    ServiceExtraTelegramInfo,
};
