(function(){
    var workspace;

    function onInit(){
        startBlocks();
        document.getElementById('run_bot_button').onclick = onRunBot;
        load_initial(workspace);
    }

    function onRunBot() {
        var xml = Blockly.Xml.workspaceToDom(workspace);
        console.log(Blockly.Xml.domToPrettyText(xml));
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
