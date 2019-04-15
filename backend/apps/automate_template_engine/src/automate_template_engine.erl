%%%-------------------------------------------------------------------
%% @doc automate_template_engine interface.
%% @end
%%%-------------------------------------------------------------------

-module(automate_template_engine).

%% API
-export([ list_templates_from_user_id/1
        , create_template/3
        , delete_template/2
        , update_template/4
        , get_template/2

        , match/4
        ]).

-define(MATCHING, automate_template_engine_matching).
-define(BACKEND, automate_template_engine_mnesia_backend).
-include("records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec list_templates_from_user_id(binary()) -> {ok, [#template_entry{}]}.
list_templates_from_user_id(UserId) ->
    ?BACKEND:list_templates_from_user_id(UserId).


-spec create_template(binary(), binary(), [any()]) -> {ok, binary()}.
create_template(UserId, TemplateName, TemplateContent) ->
    ?BACKEND:create_template(UserId, TemplateName, TemplateContent).


-spec delete_template(binary(), binary()) -> ok | {error, binary()}.
delete_template(UserId, TemplateId) ->
    ?BACKEND:delete_template(UserId, TemplateId).


-spec update_template(binary(), binary(), binary(), [any()]) -> ok | {error, binary()}.
update_template(UserId, TemplateId, TemplateName, TemplateContent) ->
    ?BACKEND:update_template(UserId, TemplateId, TemplateName, TemplateContent).

-spec get_template(binary(), binary()) -> {ok, #template_entry{}} | {error, binary()}.
get_template(UserId, TemplateId) ->
    ?BACKEND:get_template(UserId, TemplateId).

match(UserId, Thread, TemplateId, InputValue) ->
    ?MATCHING:match(UserId, Thread, TemplateId, InputValue).
