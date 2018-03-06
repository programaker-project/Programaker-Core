import { Pipe } from '@angular/core';
import { GetTypeOfJson, JSONType } from './json';

@Pipe({
    name: 'SummarizeJSON',
})

export class SummarizeJSON {
    transform(element) {
        var jtype = GetTypeOfJson(element);
        switch(jtype){
        case JSONType.Null:
        case JSONType.Boolean:
        case JSONType.Number:
            return element

        case JSONType.String:
            {
                if (element.length > 10) {
                    element = element.substr(0, 10) + "…";
                }

                return element;
            }

        case JSONType.List:
            return [this.transform(element[0])];

        case JSONType.Map:
            {
                var transformed = {};
                for (var key in element){
                    transformed[key] = this.transform(element[key]);
                }

                return transformed;
            }
        }
    }
}
