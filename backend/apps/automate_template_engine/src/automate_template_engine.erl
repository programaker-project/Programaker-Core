%%%-------------------------------------------------------------------
%% @doc automate_template_engine interface.
%% @end
%%%-------------------------------------------------------------------

-module(automate_template_engine).

%% API
-export([ list_templates/1
        , create_template/3
        , delete_template/2
        , update_template/4
        , get_template/2

        , match/4
        ]).

-define(MATCHING, automate_template_engine_matching).
-define(BACKEND, automate_template_engine_mnesia_backend).
-include("records.hrl").
-include("../../automate_bot_engine/src/program_records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec list_templates(owner_id()) -> {ok, [#template_entry{}]}.
list_templates(Owner) ->
    ?BACKEND:list_templates(Owner).


-spec create_template(owner_id(), binary(), [any()]) -> {ok, binary()}.
create_template(Owner, TemplateName, TemplateContent) ->
    ?BACKEND:create_template(Owner, TemplateName, TemplateContent).


-spec delete_template(owner_id(), binary()) -> ok | {error, binary()}.
delete_template(Owner, TemplateId) ->
    ?BACKEND:delete_template(Owner, TemplateId).


-spec update_template(owner_id(), binary(), binary(), [any()]) -> ok | {error, binary()}.
update_template(Owner, TemplateId, TemplateName, TemplateContent) ->
    ?BACKEND:update_template(Owner, TemplateId, TemplateName, TemplateContent).

-spec get_template(owner_id(), binary()) -> {ok, #template_entry{}} | {error, binary()}.
get_template(Owner, TemplateId) ->
    ?BACKEND:get_template(Owner, TemplateId).

-spec match(owner_id(), #program_thread{}, binary(), binary()) -> {ok, #program_thread{}, any()} | {error, not_found}.
match(Owner, Thread, TemplateId, InputValue) ->
    ?MATCHING:match(Owner, Thread, TemplateId, InputValue).
