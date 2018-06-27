import { Pipe, PipeTransform } from '@angular/core';
import { GetTypeOfJson, JSONType } from './json';

@Pipe({
    name: 'SummarizeJSON',
})

export class SummarizeJSON implements PipeTransform {
    transform(element) {
        const jtype = GetTypeOfJson(element);
        switch (jtype) {
        case JSONType.Null:
        case JSONType.Boolean:
        case JSONType.Number:
            return element

        case JSONType.String:
            {
                if (element.length > 10) {
                    element = element.substr(0, 10) + '…';
                }

                return element;
            }

        case JSONType.List:
            return [this.transform(element[0])];

        case JSONType.Map:
            {
                const transformed = {};
                for (const key in element) {
                    if (!element.hasOwnProperty(key)) {
                        continue;
                    }

                    transformed[key] = this.transform(element[key]);
                }

                return transformed;
            }
        }
    }
}
