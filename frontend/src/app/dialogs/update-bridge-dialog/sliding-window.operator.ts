import { pipe } from "rxjs";
import { scan } from "rxjs/operators";

export const slidingWindow = (maxSize: number) =>
    pipe(
        scan((acc, val) => {
            if (acc.length >= maxSize) {
                acc.splice(maxSize - 1);
            }

            acc.unshift(val);

            return acc;
        }, [])
    );
