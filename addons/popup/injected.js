(function(){
    try {
        const build_xpath = (node) => {
            console.log(node);
            if (node.parentElement === null) {
                return `/${node.tagName.toLowerCase()}`;
            }

            const parent = node.parentElement;
            const sameLevel = Array.from(parent.childNodes);
            const sameType = sameLevel.filter((e) => e.tagName === node.tagName);
            const index = sameType.indexOf(node);

            return (build_xpath(parent) + '/'
                    + `${node.tagName.toLowerCase()}[${index + 1}]`);
        }

        const selection = window.getSelection();
        const selected = selection.anchorNode;

        console.log(selected);
        const xpath = build_xpath(selected);

        const data = {
            selector_type: "xpath_v1",
            value: xpath,
        };

        console.log(JSON.stringify({ data }));

        const value = document.evaluate( xpath, document, null, XPathResult.ANY_TYPE, null ).iterateNext();
        console.log("Value:", value);
    }
    catch (e){
        console.error(e);
    }
})();
