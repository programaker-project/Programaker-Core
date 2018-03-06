import { DomSanitizer, SafeHtml } from '@angular/platform-browser';

import { GetTypeOfJson, JSONType } from './json';
import { Pipe } from '@angular/core';

@Pipe({
    name: 'SelectFromJSON',
})

export class SelectFromJSON {
    private sanitizer: DomSanitizer;

    constructor(sanitizer: DomSanitizer) {
        this.sanitizer = sanitizer;
    }

    set_object_class(html, jtype) {
        switch(jtype){
        case JSONType.Null:
            html.setAttribute("class", "tagged-json-type-null");
            break;

        case JSONType.Boolean:
            html.setAttribute("class", "tagged-json-type-boolean");
            break;

        case JSONType.Number:
            html.setAttribute("class", "tagged-json-type-number");
            break;

        case JSONType.String:
            html.setAttribute("class", "tagged-json-type-string");
            break;

        case JSONType.List:
            html.setAttribute("class", "tagged-json-type-list");
            break;

        case JSONType.Map:
            html.setAttribute("class", "tagged-json-type-map");
            break;
        }

    }


    indent(element, depth) {
        for(var i = 0; i < depth; i++){
            var indent = document.createElement("span");
            indent.setAttribute("class", "tagged-json-indentation");
            indent.innerHTML = "&nbsp;&nbsp;&nbsp;&nbsp;";
            element.appendChild(indent);
        }
    }


    tag(element, depth) {
        var jtype = GetTypeOfJson(element);
        var outer_object = document.createElement("span");
        var json_object = document.createElement("span");

        this.set_object_class(json_object, jtype);

        switch(jtype){
        case JSONType.Null:
        case JSONType.Boolean:
        case JSONType.Number:
            {
                var text_node = document.createTextNode(element);
                json_object.appendChild(text_node);
                break;
            }

        case JSONType.String:
            {
                if (element.length > 100) {
                    element = element.substr(0, 50) + "…";
                }

                var text_node = document.createTextNode(element);
                json_object.appendChild(text_node);
                break;
            }

        case JSONType.List:
            {
                var first_wrapper = document.createElement("div");
                first_wrapper.setAttribute("class", "tagged-json-list-element tagged-json-list-first");

                var first = this.tag(element[0], depth + 1);
                this.indent(first_wrapper, depth);
                first.setAttribute("key", "0");

                first_wrapper.appendChild(first);
                json_object.appendChild(first_wrapper);
                break;
            }

        case JSONType.Map:
            {
                for (var key in element){
                    var json_key_value = document.createElement("div");
                    json_key_value.setAttribute("class", "tagged-json-map-key-value-pair");
                    this.indent(json_key_value, depth);
                    json_key_value.setAttribute("key", key);

                    {
                        var json_key = document.createElement("span");
                        json_key.setAttribute("class", "tagged-json-map-key");
                        var json_key_object = this.tag(key, depth + 1);
                        json_key.appendChild(json_key_object);
                        json_key_value.appendChild(json_key);

                        json_key.setAttribute("onclick", 'window.AddServiceComponent.addElement(this)');
                    }

                    {
                        var json_value = document.createElement("span");
                        json_value.setAttribute("class", "tagged-json-map-value");
                        var json_value_object = this.tag(element[key], depth + 1);
                        json_value.appendChild(json_value_object);
                        json_key_value.appendChild(json_value);
                    }


                    json_object.appendChild(json_key_value);
                }

                this.indent(json_object, depth - 1);

                break;
            }
        }

        outer_object.appendChild(json_object);
        return outer_object;
    }


    transform(item): SafeHtml {
        var tagged = this.tag(item, 1);
        tagged.setAttribute("key", "");
        return this.sanitizer.bypassSecurityTrustHtml(tagged.outerHTML);
    }
}
