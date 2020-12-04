-module(automate_rest_api_renderer).

-export([ render_page/2
        ]).

-define(DEFAULT_TITLE, <<"Page title">>).

%%====================================================================
%% API functions
%%====================================================================
-spec render_page(binary(), _) -> iolist().
render_page(ProgramId, Page) ->
    {ok, Values} = automate_storage:get_widget_values_in_program(ProgramId),
    [ render_page_header(Page)
    , render_page_body(Page, ProgramId, Values)
    , render_page_footer(ProgramId, Page)
    ].


%%====================================================================
%% Internal functions
%%====================================================================
render_page_header(Page) ->
    Title = unicode:characters_to_binary("<TODO> have configurable titles ¯\\_(ツ)_/¯"),
    [ <<"<!DOCTYPE html>\n">>
    , <<"<html><head><meta charset=\"UTF-8\">\n">>
    , <<"<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1, user-scalable=0'>">>
    , <<"<title>">>, html_escape(maps:get(<<"title">>, Page, ?DEFAULT_TITLE)), <<"</title>">>
    , render_styles()
    , <<"</head>\n<body>\n">>
    ].

render_page_body(#{ <<"value">> := Contents }, ProgramId, Values) ->
    render_element(Contents, ProgramId, Values).

render_page_footer(ProgramId, #{ <<"value">> := Contents }) ->
    [ render_scripts(ProgramId, Contents)
    , <<"\n</html>">>
    ].

raw_to_html(Bin) when is_binary(Bin) ->
    html_escape(Bin);
raw_to_html([Contained]) ->
    raw_to_html(Contained);
raw_to_html(X) ->
    raw_to_html(list_to_binary(io_lib:format("~w", [X]))).

html_escape(Str) ->
    mochiweb_html:escape(Str).

%%====================================================================
%% Element rendering
%%====================================================================
render_element(null, _ProgramId, _Values) ->
    [<<"<div class='vbox'>&#x1F6A7; Work in progress &#x1F6A7;</div>">>
    ];

render_element(E=#{ <<"cut_type">> := CutType
                  , <<"groups">> := Groups
                  }, ProgramId, Values) ->

    ElementBackground = case E of
                            #{ <<"background">> := #{ <<"type">> := <<"color">>
                                                    , <<"value">> := Color
                                                    }} ->
                                [ "background-color:"
                                , Color %% TODO: Validate that the color is a correct one.
                                ];
                            _ -> []
                        end,

    [ <<"<div class='">>, CutType, <<"' ">>
    , "style='", ElementBackground, "' "
    ,  ">"
    , GroupRendering = lists:map(fun(E) -> render_element(E, ProgramId, Values) end, Groups)
    , <<"</div>">>
    ];

render_element(E=#{ <<"widget_type">> := <<"simple_button">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, _Values) ->
    [ <<"<div class=widget-container><button class='widget simple_button' id='elem-">>
    , WidgetId
    , <<"'>">>
    , html_escape(maps:get(<<"text">>, E, "Click me!"))
    , <<"</button></div>">>
    ];

render_element(E=#{ <<"widget_type">> := <<"fixed_text">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, _Values) ->
    ElementStyle = get_text_element_style(E),
    [ <<"<div class=widget-container><div class='widget fixed_text' id='elem-">>
    , WidgetId
    , <<"'">>
    , " style='", ElementStyle, "'"
    , <<">">>
    , html_escape(maps:get(<<"text">>, E, "Right click me to edit this text!"))
    , <<"</div></div>">>
    ];

render_element(E=#{ <<"widget_type">> := Type= <<"dynamic_text">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, Values) ->
    Contents = raw_to_html(maps:get(<<"text">>, E,
                                    maps:get(<<Type/binary, ".", WidgetId/binary>>, Values,
                                             <<"- No content yet -">>))),
    ElementStyle = get_text_element_style(E),
    [ <<"<div class=widget-container><div class='widget dynamic_text' id='elem-">>, WidgetId, <<"'">>
    , " style='", ElementStyle, "'"
    , <<"><div>">>
    , Contents
    , <<"</div></div></div>">>
    ];


render_element(E=#{ <<"widget_type">> := <<"fixed_image">>
                  , <<"id">> := WidgetId
                  }, ProgramId, _Values) ->
    ImgUrl = get_image_url(E, ProgramId),
    [ <<"<div class=widget-container>">>
    , "<img class='widget' src='", ImgUrl, "'/>"
    , <<"</div>">>
    ].


%%====================================================================
%% Auxiliary sections
%%====================================================================
render_styles() ->
    [ <<"<style>">>
    , <<"* { margin: 0; padding: 0 } ">>
    , <<"body { height: 100vh; text-align: center; } ">>
    , <<".hbox { width: 100%; height: 100%; display: flex; box-sizing: border-box; justify-content: space-evenly; } ">>
    , <<".vbox { width: 100%; height: 100%; display: flex; flex-flow: column; box-sizing: border-box; justify-content: space-evenly; } ">>
    , <<".dynamic_text { color: #fc4; background-color: #222; margin: auto; display: flex; justify-content: center; flex-direction: column; width: 100%; height: 100%; } ">>
    , <<".widget-container { width: 100%; height: 100%; display: flex; } ">>
    , <<".widget { margin: 0 auto; width: max-content; height: max-content; padding: 1ex; } ">>
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


wire_components(null) ->
    [];
wire_components(#{ <<"widget_type">> := <<"fixed_text">>
                 }) ->
    [];
wire_components(#{ <<"widget_type">> := <<"fixed_image">>
                 }) ->
    [];

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

wire_components(#{ <<"widget_type">> := <<"dynamic_text">>
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

get_text_element_style(E) ->
    [ get_text_element_font_size_style(E)
    , get_text_element_text_color_style(E)
    , get_text_element_background_color_style(E)
    ].

get_text_element_font_size_style(#{ <<"settings">> := #{ <<"text">> := #{ <<"fontSize">> := #{ <<"value">> := Value } } } }) ->
    [ "font-size: ", integer_to_binary(Value), "px;"
    ];
get_text_element_font_size_style(_) ->
    [].

get_text_element_text_color_style(#{ <<"settings">> := #{ <<"text">> := #{ <<"color">> := #{ <<"value">> := Value } } } }) ->
    [ "color: ", Value, ";"
    ];
get_text_element_text_color_style(_) ->
    [].

get_text_element_background_color_style(#{ <<"settings">> := #{ <<"bg">> := #{ <<"type">> := <<"color">>
                                                                             , <<"value">> := Value } } }) ->
    [ "background-color: ", Value, ";"
    ];
get_text_element_background_color_style(#{ <<"settings">> := #{ <<"bg">> := #{ <<"type">> := <<"transparent">> } } }) ->
    "background: transparent;";
get_text_element_background_color_style(_) ->
    [].

get_image_url(#{ <<"settings">> := #{ <<"body">> := #{ <<"image">> := #{ <<"id">> := ImgId } } } }, ProgramId) ->
    [ "/api/v0/programs/by-id/"
    , ProgramId
    , "/assets/by-id/"
    , ImgId
    ];
get_image_url(_, _) ->
    []. %% TODO: Add a default image?
