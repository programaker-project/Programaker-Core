-module(automate_rest_api_renderer).

-export([ render_page/2
        ]).

%%====================================================================
%% API functions
%%====================================================================
-spec render_page(binary(), _) -> iolist().
render_page(ProgramId, Contents) ->
    [ render_page_header(Contents)
    , render_page_body(Contents)
    , render_page_footer(ProgramId, Contents)
    ].


%%====================================================================
%% Internal functions
%%====================================================================
render_page_header(Contents) ->
    Title = unicode:characters_to_binary("<TODO> have configurable titles ¯\\_(ツ)_/¯"),
    [ <<"<!DOCTYPE html>\n">>
    , <<"<html><head><meta charset=\"UTF-8\">\n">>
    , <<"<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1, user-scalable=0'>">>
    , <<"<title>">>, html_escape(Title), <<"</title>">>
    , render_styles()
    , <<"</head>\n<body>\n">>
    ].

render_page_body(Contents) ->
    render_element(Contents).

render_page_footer(ProgramId, Contents) ->
    [ render_scripts(ProgramId, Contents)
    , <<"\n</html>">>
    ].

html_escape(Str) ->
    mochiweb_html:escape(Str).

%%====================================================================
%% Element rendering
%%====================================================================
render_element(#{ <<"cut_type">> := CutType
                , <<"groups">> := Groups
                }) ->
    [ <<"<div class='">>, CutType, <<"-cut'>">>
    , GroupRendering = lists:map(fun render_element/1, Groups)
    , <<"</div>">>
    ];

render_element(E=#{ <<"widget_type">> := <<"simple_button">>
                  , <<"id">> := WidgetId
                  }) ->
    [ <<"<div class=widget-container><button class='widget simple_button' id='elem-">>
    , WidgetId
    , <<"'>">>
    , html_escape(maps:get(<<"text">>, E, "Click me!"))
    , <<"</button></div>">>
    ];

render_element(#{ <<"widget_type">> := <<"simple_debug_output">>
                , <<"id">> := WidgetId
                }) ->
    Contents = <<"- No content yet -">>,
    [ <<"<div class=widget-container><div class='widget simple_debug_output' id='elem-">>, WidgetId, <<"'><div>">>
    , Contents
    , <<"</div></div></div>">>
    ].

%%====================================================================
%% Auxiliary sections
%%====================================================================
render_styles() ->
    [ <<"<style>">>
    , <<"* { margin: 0; padding: 0 } ">>
    , <<"body { height: 100vh; text-align: center; } ">>
    , <<".horizontal-cut { width: 100%; height: 100%; display: flex; box-sizing: border-box; justify-content: space-evenly; } ">>
    , <<".vertical-cut { height: 100%; display: flex; flex-flow: column; box-sizing: border-box; justify-content: space-evenly; } ">>
    , <<".simple_debug_output { color: #fc4; background-color: #222; margin: auto; display: flex; justify-content: center; flex-direction: column; width: 100%; height: 100%; } ">>
    , <<".widget-container { width: 100%; height: 100%; display: flex; } ">>
    , <<".widget { width: 100%; height: 100%; } ">>
    , <<"</style>">>
    ].

render_scripts(ProgramId, Contents) ->
    ScriptContents = wire_components(Contents),
    [ <<"<script type='text/javascript'>">>
    , render_connection_block_start(ProgramId)
    , ScriptContents
    , render_connection_block_end()
    , <<"\n</script>">>
    ].

wire_components(#{ <<"cut_type">> := CutType
                 , <<"groups">> := Groups
                 }) ->
    lists:map(fun wire_components/1, Groups);

wire_components(#{ <<"widget_type">> := <<"simple_button">>
                 , <<"id">> := WidgetId
                 }) ->
    [ "document.getElementById('elem-", WidgetId ,"').onclick = (function() {\n"
    , "websocket.send(JSON.stringify({\n"
    , "    type: 'ui-event',\n"
    , "    value: {\n"
    , "    action: 'activated',\n"
    , "    block_type: 'simple_button',\n"
    , "    block_id: '", WidgetId, "'\n"
    , "}}));\n"
    , "});\n"
    ];

wire_components(#{ <<"widget_type">> := <<"simple_debug_output">>
                        , <<"id">> := WidgetId
                        }) ->
    [ "link_widget('", WidgetId, "');"].

render_connection_block_start(ProgramId) ->
    Url = ["/api/v0/programs/by-id/", ProgramId, "/ui-events"],
    [ "\n(function(){ \n"
    , "var listeners = {};"
    , "var link_widget = (function(id){ listeners[id] = (function(data){var e = document.getElementById('elem-' + id); e.innerText = data.values[0];}); });"
    , "var dispatch = (function(data) { \n"
    , "  var id = data.subkey.split('.')[1]; if (listeners[id]) { listeners[id](data); } \n"
    , "    else { console.warn('Received event for unknown widget:', data.subkey); } });\n"
    , "var ws_url = (document.location.origin + '", Url, "').replace(/^http/, 'ws');\n"
    , "console.log('Connecting to websocket on', ws_url);\n"
    , "var websocket = new WebSocket(ws_url);\n"
    , "websocket.onmessage = (function(ev) {\n"
    ,  "  var parsed = JSON.parse(ev.data);\n"
    ,  "  dispatch(parsed);\n"
    , "});\n"

    , "websocket.onclose = (function() { console.warn('Connection closed'); });\n"
    , "websocket.onerror = (function(err) { console.error(err); });\n"
    , "websocket.onopen = (function() { console.log('Connection opened'); \n"
    , "  websocket.send(JSON.stringify({ type: 'AUTHENTICATION', value: { token: 'ANONYMOUS' }}));\n"
    , "});\n"
    ].

render_connection_block_end() ->
    <<"})();">>.
