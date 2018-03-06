export class Channel {
    activated: boolean;
}

export class TelegramChannel extends Channel {
    token: string;
}
