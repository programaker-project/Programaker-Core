%%%-------------------------------------------------------------------
%% @doc automate_template_engine interface.
%% @end
%%%-------------------------------------------------------------------

-module(automate_template_engine).

-behaviour(supervisor).

%% API
-export([ list_templates_from_user_id/1
        , create_template/3
        ]).

-define(BACKEND, automate_template_engine_mnesia_backend).

%%====================================================================
%% API functions
%%====================================================================
-spec list_templates_from_user_id(binary()) -> {ok, [map()]}.
list_templates_from_user_id(UserId) ->
    ?BACKEND:list_templates_from_user_id(UserId).


-spec create_template(binary(), binary(), [any()]) -> {ok, binary()}.
create_template(UserId, TemplateName, TemplateContent) ->
    ?BACKEND:create_template(UserId, TemplateName, TemplateContent).
