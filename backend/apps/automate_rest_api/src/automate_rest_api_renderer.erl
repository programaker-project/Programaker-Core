-module(automate_rest_api_renderer).

-export([ render_page/1
        ]).

%%====================================================================
%% API functions
%%====================================================================
-spec render_page(_) -> iolist().
render_page(Contents) ->
    [ render_page_header(Contents)
    , render_page_body(Contents)
    , render_page_footer(Contents)
    ].


%%====================================================================
%% Internal functions
%%====================================================================
render_page_header(_Contents) ->
    Title = unicode:characters_to_binary("<TODO> have configurable titles ¯\\_(ツ)_/¯"),
    [ <<"<!DOCTYPE html>\n">>
    , <<"<html><head><meta charset=\"UTF-8\">\n">>
    , <<"<title>">>, html_escape(Title), <<"</title>">>
    , render_styles()
    , <<"</head>\n<body>\n">>
    ].

render_page_body(Contents) ->
    render_element(Contents).

render_page_footer(_Contents) ->
    <<"\n</html>">>.

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

render_element(#{ <<"widget_type">> := <<"simple_button">>
                , <<"id">> := WidgetId
                }) ->
    [ <<"<button class='simple_button' id='elem-">>
    , WidgetId
    , <<"'>Click me!</button>">>
    ];

render_element(#{ <<"widget_type">> := <<"simple_debug_output">>
                , <<"id">> := WidgetId
                }) ->
    Contents = <<"- No content yet -">>,
    [ <<"<div class='simple_debug_output' id='elem-">>, WidgetId, <<"'><div class=content>">>
    , Contents
    , <<"</div></div>">>
    ].

%%====================================================================
%% Auxiliary sections
%%====================================================================
render_styles() ->
    [ <<"<style>">>
    , <<"* { margin: 0; padding: 0 } ">>
    , <<"body { height: 100vh; text-align: center; } ">>
    , <<".horizontal-cut { width: 100%; height: 100%; display: flex; } ">>
    , <<".vertical-cut { height: 100%; display: flex; flex-flow: column; } ">>
    , <<".simple_debug_output { margin: auto; height: 100%; display: flex; justify-content: center; flex-direction: column; } ">>
    , <<".content { } ">>
    , <<"button { width: max-content; height: max-content; margin: auto; } ">>
    , <<"</style>">>
    ].
