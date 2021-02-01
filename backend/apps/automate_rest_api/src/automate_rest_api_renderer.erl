-module(automate_rest_api_renderer).

-export([ render_page/4
        ]).

-define(DEFAULT_TITLE, <<"Page title">>).

%%====================================================================
%% API functions
%%====================================================================
-spec render_page(binary(), _, cowboy_req:req(), page | element) -> iolist().
render_page(ProgramId, Page, Req, RenderAs) ->
    {ok, Values} = automate_storage:get_widget_values_in_program(ProgramId),
    [ render_page_header(Page, RenderAs)
    , render_page_body(Page, ProgramId, Values, Req)
    , render_page_footer(ProgramId, Page, RenderAs)
    ].


%%====================================================================
%% Internal functions
%%====================================================================
render_page_header(Page, page) ->
    [ <<"<!DOCTYPE html>\n">>
    , <<"<html><head><meta charset=\"UTF-8\">\n">>
    , <<"<meta name='viewport' content='width=device-width, initial-scale=1.0, maximum-scale=1, user-scalable=0'>">>
    , <<"<title>">>, html_escape(maps:get(<<"title">>, Page, ?DEFAULT_TITLE)), <<"</title>">>
    , render_styles(page)
    , <<"</head>\n<body>\n">>
    ];
render_page_header(_Page, element) ->
    [ "<div class='programaker-element'>"
    , render_styles(element)
    ].

render_page_body(#{ <<"value">> := Contents }, ProgramId, Values, Req) ->
    render_element(Contents, ProgramId, Values, Req).

render_page_footer(ProgramId, #{ <<"value">> := Contents }, page) ->
    [ render_scripts(ProgramId, Contents)
    , <<"\n</html>">>
    ];

render_page_footer(ProgramId, #{ <<"value">> := Contents }, element) ->
    [ render_scripts(ProgramId, Contents)
    , "\n</div>"
    ].

raw_to_html(Bin) when is_binary(Bin) ->
    html_escape(Bin);
raw_to_html([Contained]) ->
    raw_to_html(Contained);
raw_to_html(X) ->
    raw_to_html(list_to_binary(io_lib:format("~w", [X]))).

html_escape(Str) ->
    Lines = binary:split(Str, <<"\n">>, [global]),
    EscapedLines = lists:map(fun mochiweb_html:escape_attr/1, Lines),
    lists:join("<br/>", EscapedLines).

%%====================================================================
%% Element rendering
%%====================================================================
render_element(null, _ProgramId, _Values, _Req) ->
    [<<"<div class='vbox'>&#x1F6A7; Work in progress &#x1F6A7;</div>">>
    ];

render_element(E=#{ <<"cut_type">> := CutType
                  , <<"groups">> := Groups
                  }, ProgramId, Values, Req) ->

    ElementBackground = case E of
                            #{ <<"settings">> := #{ <<"bg">> := #{ <<"type">> := <<"color">>
                                                                         , <<"value">> := Color
                                                                         }}} ->
                                [ "background-color:"
                                , Color %% TODO: Validate that the color is a correct one.
                                ];
                            _ -> []
                        end,

    [ <<"<div class='">>, CutType, <<"' ">>
    , "style='", ElementBackground, "' "
    ,  ">"
    , "<div class='inner-box'>"
    , lists:map(fun(El) -> render_element(El, ProgramId, Values, Req) end, Groups)
    , <<"</div></div>">>
    ];

render_element(E=#{ <<"container_type">> := <<"simple_card">>
                  , <<"content">> := Content
                  }, ProgramId, Values, Req) ->
    ElementBackground = case E of
                            #{ <<"settings">> := #{ <<"bg">> := #{ <<"type">> := <<"color">>
                                                                         , <<"value">> := Color
                                                                         }}} ->
                                [ "background-color:"
                                , Color %% TODO: Validate that the color is a correct one.
                                ];
                            _ -> []
                        end,

    [ "<div class='simple_card'>"
    , "<div class='inner-box' "
    , "style='", ElementBackground, "' "
    , ">"
    , case Content of
          null -> "";
          _ -> render_element(Content, ProgramId, Values, Req)
      end
    , <<"</div></div>">>
    ];

render_element(E=#{ <<"container_type">> := <<"link_area">>
                  , <<"content">> := Content
                  }, ProgramId, Values, Req) ->
    Target = case E of
                 #{ <<"settings">> := #{ <<"target">> := #{ <<"link">> := #{ <<"value">> := Link }
                                                          }}} ->
                     Link; %% TODO: Validate link types
                 _ -> "#"
             end,
    [ "<a class='link_area' href='", Target, "'>"
    , "<div class='inner-box' "
    , ">"
    , case Content of
          null -> "";
          _ -> render_element(Content, ProgramId, Values, Req)
      end
    , <<"</div></a>">>
    ];

render_element(E=#{ <<"widget_type">> := <<"simple_button">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, _Values, _Req) ->
    [ <<"<div class=widget-container><button class='widget simple_button' id='elem-">>
    , WidgetId
    , <<"'>">>
    , html_escape(maps:get(<<"text">>, E, "Click me!"))
    , <<"</button></div>">>
    ];

render_element(E=#{ <<"widget_type">> := <<"fixed_text">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, _Values, _Req) ->
    ElementStyle = get_text_element_style(E),
    [ <<"<div class=widget-container><div class='widget text fixed_text' id='elem-">>
    , WidgetId
    , <<"'">>
    , " style='", ElementStyle, "'"
    , <<">">>
    , get_fixed_text_content(E)
    , <<"</div></div>">>
    ];

render_element(E=#{ <<"widget_type">> := Type= <<"text_box">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, Values, _Req) ->
    Contents = raw_to_html(maps:get(<<"text">>, E,
                                    maps:get(<<Type/binary, ".", WidgetId/binary>>, Values,
                                             <<"">>))),
    ElementStyle = get_text_element_style(E),
    [ <<"<div class=widget-container><input type='text' class='widget text text_box' id='elem-">>, WidgetId, <<"'">>
    , " style='", ElementStyle, "'"
    , <<" placeholder=\"">>
    , Contents
    , <<"\" /></div>">>
    ];

render_element(E=#{ <<"widget_type">> := Type= <<"dynamic_text">>
                  , <<"id">> := WidgetId
                  }, _ProgramId, Values, _Req) ->
    Contents = raw_to_html(maps:get(<<"text">>, E,
                                    maps:get(<<Type/binary, ".", WidgetId/binary>>, Values,
                                             <<"- No content yet -">>))),
    ElementStyle = get_text_element_style(E),
    [ <<"<div class=widget-container><div class='widget text dynamic_text' id='elem-">>, WidgetId, <<"'">>
    , " style='", ElementStyle, "'"
    , <<"><div>">>
    , Contents
    , <<"</div></div></div>">>
    ];


render_element(E=#{ <<"widget_type">> := <<"fixed_image">>
                  , <<"id">> := _WidgetId
                  }, ProgramId, _Values, Req) ->
    ImgUrl = get_image_url(E, ProgramId, Req),
    [ <<"<div class=widget-container>">>
    , "<img class='widget' src='", ImgUrl, "'/>"
    , <<"</div>">>
    ];

render_element(E=#{ <<"widget_type">> := <<"horizontal_separator">>
                  , <<"id">> := _WidgetId
                  }, _ProgramId, _Values, _Req) ->
    [ "<hr "
    , case E of
          #{ <<"settings">> := #{ <<"body">> := #{ <<"widthTaken">> := #{ <<"value">> := Width } } } } ->
              [ "class='size-", Width, "' " ];
          _ ->
              []
      end
    , "/>"
    ].


%%====================================================================
%% Auxiliary sections
%%====================================================================
render_styles(RenderAs) ->
    MaterialShadow = "0 3px 1px -2px rgba(0,0,0,.2),0 2px 2px 0 rgba(0,0,0,.14),0 1px 5px 0 rgba(0,0,0,.12)",

    {Root, RootStyle} = case RenderAs of
                            page -> { "",
                                      [ "body { height: 100vh; text-align: center; } "
                                      , " body {"
                                      ,   "font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Roboto,Helvetica Neue,Arial,Noto Sans,sans-serif,Apple Color Emoji,Segoe UI Emoji,Segoe UI Symbol,Noto Color Emoji;"
                                      ,   "font-size: 1rem;"
                                      ,   "font-weight: 400;"
                                      ,   "line-height: 1.5;"
                                      ,   "color: #212529;"
                                      ,   "text-align: left; }"
                                      , " * { margin: 0; padding: 0; box-sizing: border-box; } "
                                      ]};
                            element ->
                                RootEl = "div.programaker-element ",
                                { RootEl,
                                  [ RootEl, "* { margin: 0; padding: 0; box-sizing: border-box; } "
                                  , RootEl, "{ text-align: center; } "
                                  ]}
                        end,

    [ <<"<style>">>
    , RootStyle
    , Root, <<".hbox { justify-content: space-evenly; } ">>
    , Root, <<".vbox { flex-flow: column; justify-content: space-evenly; } ">>
    , Root, <<".dynamic_text { color: #fc4; background-color: #222; margin: auto; display: flex; justify-content: center; flex-direction: column; width: 100%; height: 100%; } ">>
    , Root, ".fixed_text { max-width: 50em; overflow-wrap: anywhere; } "
    , Root, <<".widget-container { width: 100%; height: 100%; display: flex; } ">>
    , Root, ".hbox > .inner-box > .widget-container { display: inline flex; width: max-content; }"
    , Root, <<".widget { margin: 0 auto; padding: 1ex; } ">>
    , Root, "hr { width: calc(50% - 2px); margin: 1ex auto; border: 1px solid #aaa; } "
    , Root, "hr.size-short { width: calc(min(100%, 20ex) - 2px); } "
    , Root, "hr.size-full { width: calc(100% - 2px); } "
    , Root, ".hbox > .inner-box { margin: 0 auto; width: max-content; max-width: 100%; text-align: center; }"
    , Root, ".hbox > .inner-box > .vbox, .hbox > .inner-box > .simple_card { display: inline-flex; vertical-align: top; max-width: 100%; }"
    , Root, "a.link_area { display: inline-flex; text-decoration: inherit; }"
    , Root, ".simple_card { margin: 0 auto; width: max-content; }"
    , Root, ".simple_card > .inner-box { margin: 1ex; padding: 1ex; border-radius: 4px; box-shadow: ", MaterialShadow, "; min-width: 20ex; min-height: 8ex; }"
    , Root, ".simple_card > .inner-box > .vbox { margin: auto; }"
    , Root, ".simple_card > .inner-box > .widget-container > .widget { margin: auto; }"
    , Root, "img { max-width: 75vw; max-height: 75vh; }" % Set some baseline to image sizes
    , Root, "font a { color: inherit; }"
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
wire_components(#{ <<"widget_type">> := <<"horizontal_separator">>
                 }) ->
    [];
wire_components(#{ <<"container_type">> := _
                 , <<"content">> := Content
                 }) ->
    wire_components(Content);

wire_components(#{ <<"cut_type">> := _CutType
                 , <<"groups">> := Groups
                 }) ->
    lists:map(fun wire_components/1, Groups);

wire_components(#{ <<"widget_type">> := <<"text_box">>
                 , <<"id">> := WidgetId
                 }) ->
    [ "addCollectable('", WidgetId, "', function() {  return document.getElementById('elem-", WidgetId, "').value });"
    , "document.getElementById('elem-", WidgetId ,"').onkeyup = (function() {\n"
    , "websocket.send(JSON.stringify({\n"
    , "    type: 'ui-event',\n"
    , "    value: {\n"
    , "    action: 'changed',\n"
    , "    block_type: 'text_box',\n"
    , "    block_id: '", WidgetId, "',\n"
    , "    data: collectData(),\n"
    , "}}));\n"
    , "});\n"
    ];


wire_components(#{ <<"widget_type">> := <<"simple_button">>
                 , <<"id">> := WidgetId
                 }) ->
    [ "addCollectable('", WidgetId, "', function() {  return document.getElementById('elem-", WidgetId, "').innerText });"
    , "document.getElementById('elem-", WidgetId ,"').onclick = (function() {\n"
    , "websocket.send(JSON.stringify({\n"
    , "    type: 'ui-event',\n"
    , "    value: {\n"
    , "    action: 'activated',\n"
    , "    block_type: 'simple_button',\n"
    , "    block_id: '", WidgetId, "',\n"
    , "    data: collectData(),\n"
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
    , "var collectables = [];\n"
    , "var addCollectable = (function(id, item) { collectables.push([id, item]) });\n"
    , "var collectData = (function() { var result = {}; for (var coll of collectables) { result[coll[0]] = coll[1](); }; return result; } );\n"
    ].

render_connection_block_end() ->
    <<"})();">>.

%% Element attribute management
get_text_element_style(E) ->
    [ get_text_element_font_size_style(E)
    , get_text_element_font_weight_style(E)
    , get_text_element_text_color_style(E)
    , get_text_element_background_color_style(E)
    , get_text_element_underline_color_style(E)
    ].

get_text_element_text_color_style(#{ <<"settings">> := #{ <<"text">> := #{ <<"color">> := #{ <<"value">> := Value } } } }) ->
    [ "color: ", Value, ";"
    ];
get_text_element_text_color_style(_) ->
    [].

get_text_element_font_size_style(#{ <<"settings">> := #{ <<"text">> := #{ <<"fontSize">> := #{ <<"value">> := Value } } } }) ->
    [ "font-size: ", responsive_font_size(Value), ";"
    ];
get_text_element_font_size_style(_) ->
    [].

get_text_element_font_weight_style(#{ <<"settings">> := #{ <<"text">> := #{ <<"fontWeight">> := #{ <<"value">> := Value } } } }) ->
    [ "font-weight: ", font_weight_to_css(Value), ";"
    ];
get_text_element_font_weight_style(_) ->
    [].

get_text_element_background_color_style(#{ <<"settings">> := #{ <<"bg">> := #{ <<"type">> := <<"color">>
                                                                             , <<"value">> := Value } } }) ->
    [ "background-color: ", Value, ";"
    ];
get_text_element_background_color_style(#{ <<"settings">> := #{ <<"bg">> := #{ <<"type">> := <<"transparent">> } } }) ->
    "background: transparent;";
get_text_element_background_color_style(_) ->
    [].

get_image_url(#{ <<"settings">> := #{ <<"body">> := #{ <<"image">> := #{ <<"id">> := ImgId } } } }, ProgramId, Req) ->
    ImagePath = [ "/api/v0/programs/by-id/"
                , ProgramId
                , "/assets/by-id/"
                , ImgId
                ],
    BaseChanges = #{path => ImagePath, qs => undefined },
    Changes = case automate_configuration:get_backend_api_info() of
                  undefined ->
                      BaseChanges;
                  BackendInfo ->
                      maps:merge(BackendInfo, BaseChanges)
    end,
    cowboy_req:uri(Req, Changes);

get_image_url(_, _, _) ->
    []. %% TODO: Add a default image?

get_text_element_underline_color_style(#{ <<"underline">> := <<"none">>}) ->
    "text-decoration: none;";
get_text_element_underline_color_style(#{ <<"underline">> := #{ <<"color">> := Color}}) ->
    ["text-decoration: underline; text-decoration-color: ", Color, ";"];
get_text_element_underline_color_style(#{ <<"underline">> := <<"default">>}) ->
    "";
get_text_element_underline_color_style(_) ->
    "".


get_fixed_text_content(#{ <<"content">> := Content }) ->
    formatted_text_to_html(Content);
get_fixed_text_content(E) ->
    html_escape(maps:get(<<"text">>, E, "Right click me to edit this text!")).

formatted_text_to_html(FT) ->
    lists:map(fun formatted_element_to_html/1, FT).

formatted_element_to_html(#{ <<"type">> := <<"text">>
                           , <<"value">> := Text
                           }) ->
    html_escape(Text);
formatted_element_to_html(E=#{ <<"type">> := <<"link">>
                             , <<"target">> := Target
                             , <<"contents">> := Contents
                             , <<"open_in_tab">> := OpenInTab
                             }) ->
    [ "<a href='", mochiweb_html:escape(Target)
    , "' rel='noopener noreferrer'", case OpenInTab of true -> " target='_blank'"; _ -> "" end
    , " style='", get_text_element_style(E), "'"
    , ">"
    , formatted_text_to_html(Contents)
    , "</a>"
    ];
formatted_element_to_html(#{ <<"type">> := <<"text-color">>
                           , <<"color">> := Color
                           , <<"contents">> := Contents
                           }) ->
    [ "<font style='color:", mochiweb_html:escape(Color), ";' >"
    , formatted_text_to_html(Contents)
    , "</font>"
    ];
formatted_element_to_html(#{ <<"type">> := <<"format">>
                           , <<"format">> := Format
                           , <<"contents">> := Contents
                           }) ->
    Tag = format_to_tag(Format),
    [ "<", Tag, ">"
    , formatted_text_to_html(Contents)
    , "</", Tag, ">"
    ].

format_to_tag(<<"bold">>) ->
    "b";
format_to_tag(<<"italic">>) ->
    "i";
format_to_tag(<<"underline">>) ->
    "u".



%% Attribute translation
font_weight_to_css(<<"normal">>) ->
    "normal";
font_weight_to_css(<<"bold">>) ->
    "bold";
font_weight_to_css(<<"light">>) ->
    "300";
font_weight_to_css(<<"super-light">>) ->
    "100";
font_weight_to_css(<<"super-bold">>) ->
    "900".

%% Set limits to font size
responsive_font_size(FontSizeInPx) ->
    MinSize = "0.5rem",
    MaxSize = "10vw",
    [ "clamp(", MinSize
    , ", ", integer_to_binary(FontSizeInPx), "px"
    , ", ", MaxSize, ")"
    ].
