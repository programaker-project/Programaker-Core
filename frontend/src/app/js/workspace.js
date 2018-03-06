(function(){
    var workspace;

    function onInit(){
        startBlocks();
        document.getElementById('run_bot_button').onclick = onRunBot;
        load_initial(workspace);
    }

    function onRunBot() {
        var token = document.getElementById('telegram_bot_token').value;

        var xml = Blockly.Xml.workspaceToDom(workspace);
        var content = Blockly.Xml.domToPrettyText(xml);

        var http = new XMLHttpRequest();
        var url = "/api/bot_orders/set";
        http.open("POST", url, true);

        //Send the proper header information along with the request
        http.setRequestHeader("Content-type", "application/json");

        http.onreadystatechange = (function() {//Call a function when the state changes.
            if(http.readyState == 4 && http.status == 200) {
                console.log(http.responseText);
            }
        });
        http.send(JSON.stringify({"commands": content, "token": token}));
    }

    function startBlocks(){
        var rtl = false;
        var toolbox = null;
        var side = 'bottom';
        var soundsEnabled = false;

        workspace = Blockly.inject('workspace', {
            comments: false,
            disable: false,
            collapse: true,
            media: '../media/',
            readOnly: false,
            rtl: rtl,
            scrollbars: false,
            toolbox: toolbox,
            toolboxPosition: side == 'top' || side == 'start' ? 'start' : 'end',
            horizontalLayout: side == 'top' || side == 'bottom',
            sounds: soundsEnabled,
            zoom: {
                controls: true,
                wheel: true,
                startScale: 0.75,
                maxScale: 4,
                minScale: 0.25,
                scaleSpeed: 1.1
            },
            colours: {
                fieldShadow: 'rgba(255, 255, 255, 0.3)',
                dragShadowOpacity: 0.6
            }
        });
    }

    window.onload = onInit;
})();
