import { Subscribable } from 'rxjs';

type Pusher<T> = { push(value: T): void; };
export type Synchronizer<T> = Subscribable<T> & Pusher<T>;
