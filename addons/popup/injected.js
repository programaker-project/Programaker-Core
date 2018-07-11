(function(){
    console.log("DEBUG " + document.location);

    var selection = window.getSelection();
    var selected = selection.anchorNode;
    var node = selected;

    var ascension = [];
    while (node !== document){
        ascension.push(node);
        console.log("DEBUG " + node + " " + node.tagName + " "  + node.id + " "  + node.classList);
        node = node.parentNode;
    }

    console.log("DEBUG " + ascension);

    var validField = (function (field){
        // Void field
        if (((field + "") === "") || (field === undefined) || (field === null)){
            return false;
        }

        field += "";

        // Fields not containing ':'
        return field.indexOf(":") == -1;
    });

    var route = [];
    var selector = "";

    for (var i = ascension.length - 1; i >= 0; i--){
        var step = ascension[i];

        console.log("DEBUG " + i + " " + step);
        if (false && validField(step.id)){
            route.push({id: step.id});
            console.log("DEBUG " + "--> {id: " + step.id + " }");

            selector += "#" + step.id + " ";
        }
        else if ((validField(step.classList)) && (step.classList.length > 0)){
            // @TODO better logic, decision trees?
            route.push({class: step.classList[0]});
            console.log("DEBUG " + "--> {class: " + step.classList[0] + " }");

            selector += "." + step.classList[0] + " ";
        }
        else if (validField(step.tagName)) {
            route.push({tag: step.tagName});
            console.log("DEBUG " + "--> {tag: " + step.tagName + " }");

            selector += step.tagName.toLowerCase() + " ";
        }
    }


    console.log("DEBUG " + selector);
    console.log("DEBUG " + "Testing...");
    var elements = document.querySelectorAll(selector);
    console.log("DEBUG " + elements.length + " elements");

    var nth = "";
    var expected = "";
    for (var i = 0; i < elements.length; i++){
        console.log("DEBUG " + elements[i] + " => "
                    + selection.containsNode(elements[i], true));

        if (selection.containsNode(elements[i], true)){
            nth = ", \"nth\": " + (i + 1);
            console.log(elements[i].textContent);
            expected = elements[i].textContent;
            break;
        }
    }


    var expected_image = "";
    var nth_image = "";
    var images = document.querySelectorAll(selector + " img");
    console.log("DEBUG " + images.length + " images");
    for (var i = 0; i < images.length; i++){
        if (selection.containsNode(images[i])){
            nth_image = ", \"nth_image\": " + (i + 1);
            expected_image = ", \"expected_image\": \"" + images[i].src + "\"";
            break;
        }
    }


    console.log("DATA {\"selector\": \"" + selector.trim() + "\","
                + "\"url\":  \"" + document.location + "\","
                + "\"images\": " + (expected_image.length > 0) + ","
                + "\"expected\": \"" + expected.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
                + expected_image
                + nth
                + nth_image
                + "}");

})();
