-module(automate_rest_api_backend).

%% API exports
-export([ register_user/1
        , verify_registration_with_code/1
        , send_recovery_mail/1
        , check_password_reset_verification_code/1
        , reset_password/2

        , login_user/1
        , get_user/1
        , generate_token_for_user/1
        , is_valid_token/1
        , is_valid_token_uid/1
        , create_monitor/2
        , lists_monitors_from_username/1
        , create_program/3
        , get_program/1
        , get_program/2
        , get_program_logs/1
        , lists_programs_from_username/1
        , update_program/3
        , update_program_by_id/2
        , list_services_from_username/1
        , get_services_metadata/2
        , get_service_enable_how_to/2

        , update_program_metadata/3
        , update_program_metadata/2
        , delete_program/2
        , delete_program/1

        , create_service_port/2
        , list_custom_blocks_from_username/1
        , register_service/4
        , send_oauth_return/2
        , list_bridges/1
        , list_available_connections/1
        , list_established_connections/1
        , callback_bridge/3
        , bridge_function_call/4

        , create_custom_signal/2

        , create_template/3
        , delete_template/2
        , update_template/4
        , get_template/2
        ]).

%% Definitions
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_service_registry/src/records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_template_engine/src/records.hrl").

-define(URLS, automate_rest_api_utils_urls).

%%====================================================================
%% API functions
%%====================================================================
-spec register_user(#registration_rec{}) -> {ok, continue | wait_for_mail_verification } | {error, _}.
register_user(Reg=#registration_rec{ username=Username }) ->
    case automate_storage_utils:validate_username(Username) of
        false ->
            {error, invalid_username};
        true ->
            case automate_mail:is_enabled() of
                false ->
                    register_user_instantly(Reg);
                true ->
                    register_user_require_validation(Reg)
            end
    end.

verify_registration_with_code(RegistrationCode) ->
    automate_storage:verify_registration_with_code(RegistrationCode).

-spec send_recovery_mail(binary()) -> ok | {error, _}.
send_recovery_mail(Email) ->
    case automate_mail:is_enabled() of
        false ->
            {error, mail_recovery_not_supported};
        true ->
            case automate_storage:get_user_from_mail_address(Email) of
                {ok, #registered_user_entry{ id=UserId, username=Username }} ->
                    case automate_storage:create_recovery_verification(UserId) of
                        {ok, Code} ->
                            {ok, _Url} = automate_mail:send_password_recovery_verification(Username, Email, Code),
                            ok;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec login_user(#login_rec{}) -> {ok, {binary(), binary()}} | {error, {_, _} | atom() | binary()}.
login_user(#login_rec{ password=Password
                     , username=Username
                     }) ->
    case automate_storage:login_user(Username, Password) of
        { ok, {Token, UserId} } ->
            { ok, {Token, UserId} };
        { error, Reason } ->
            { error, Reason }
    end.

check_password_reset_verification_code(VerificationCode) ->
    automate_storage:check_password_reset_verification_code(VerificationCode).

-spec reset_password(binary(), binary()) -> ok | {error, _}.
reset_password(VerificationCode, Password) ->
    automate_storage:reset_password(VerificationCode, Password).

get_user(UserId) ->
    automate_storage:get_user(UserId).

generate_token_for_user(UserId) ->
    automate_storage:generate_token_for_user(UserId).

is_valid_token(Token) when is_binary(Token) ->
    case automate_storage:get_session_username(Token, true) of
        { ok, Username } ->
            {true, Username};
        { error, session_not_found } ->
            false;
        { error, Reason } ->
            automate_logging:log_api(error, ?MODULE, {error_retrieving_session, Reason}),
            false
    end.

is_valid_token_uid(Token) when is_binary(Token) ->
    case automate_storage:get_session_userid(Token, true) of
        { ok, UserId } ->
            {true, UserId};
        { error, session_not_found } ->
            false;
        { error, Reason } ->
            automate_logging:log_api(error, ?MODULE, {error_retrieving_session, Reason}),
            false
    end.

-spec create_monitor(binary(), #monitor_descriptor{}) -> {ok, {binary(), binary()}}.
create_monitor(Username, #monitor_descriptor{ type=Type, name=Name, value=Value }) ->
    case automate_storage:create_monitor(Username, #monitor_entry{ type=Type
                                                                 , name=Name
                                                                 , value=Value
                                                                 , id=none %% ID generated by the storage
                                                                 , owner=none
                                                                 }) of
        { ok, MonitorId } ->
            { ok, { MonitorId, Name } }
    end.

-spec lists_monitors_from_username(binary()) -> {'ok', [ #monitor_metadata{} ] }.
lists_monitors_from_username(Username) ->
    case automate_storage:lists_monitors_from_username(Username) of
        {ok, Monitors} ->
            {ok, [#monitor_metadata{ id=Id
                                   , name=Name
                                   , link=generate_url_for_monitor_name(Username, Name)
                                   }
                  || {Id, Name} <- Monitors]}
    end.

create_program(Username, ProgramName, ProgramType) ->
    case automate_storage:create_program(Username, ProgramName, ProgramType) of
        { ok, ProgramId } ->
            { ok, { ProgramId
                  , ProgramName
                  , generate_url_for_program_name(Username, ProgramName)
                  , ProgramType
                  } }
    end.

get_program(Username, ProgramName) ->
    case automate_storage:get_program(Username, ProgramName) of
        {ok, ProgramData} ->
            {ok, program_entry_to_program(ProgramData)};
        X ->
            X
    end.

get_program(ProgramId) ->
    case automate_storage:get_program_from_id(ProgramId) of
        {ok, ProgramData} ->
            {ok, program_entry_to_program(ProgramData)};
        X ->
            X
    end.

get_program_logs(ProgramId) ->
    case automate_storage:get_logs_from_program_id(ProgramId) of
        {ok, Logs} ->
            {ok, Logs};
        X ->
            X
    end.

-spec lists_programs_from_username(binary()) -> {'ok', [ #program_metadata{} ] }.
lists_programs_from_username(Username) ->
    case automate_storage:lists_programs_from_username(Username) of
        {ok, Programs} ->
            {ok, lists:map(fun({ProgramId, ProgramName, Enabled, ProgramType}) ->
                                   #program_metadata{ id=ProgramId
                                                    , name=ProgramName
                                                    , link=generate_url_for_program_name(Username, ProgramName)
                                                    , enabled=Enabled
                                                    , type=ProgramType
                                                    }
                           end, Programs)}
    end.

update_program(Username, ProgramName,
               #program_content{ orig=Orig
                               , parsed=Parsed
                               , type=Type }) ->

    case automate_storage:update_program(Username, ProgramName,
                                         #stored_program_content{ orig=Orig
                                                                , parsed=Parsed
                                                                , type=Type }) of
        { ok, ProgramId } ->
            automate_bot_engine_launcher:update_program(ProgramId);
        { error, Reason } ->
            {error, Reason}
    end.

update_program_by_id(ProgramId,
                     #program_content{ orig=Orig
                                     , parsed=Parsed
                                     , type=Type }) ->

    case automate_storage:update_program_by_id(ProgramId,
                                               #stored_program_content{ orig=Orig
                                                                      , parsed=Parsed
                                                                      , type=Type }) of
        { ok, ProgramId } ->
            automate_bot_engine_launcher:update_program(ProgramId);
        { error, Reason } ->
            {error, Reason}
    end.

update_program_metadata(Username, ProgramName,
                        Metadata=#editable_user_program_metadata{program_name=NewProgramName}) ->

    case automate_storage:update_program_metadata(Username,
                                                  ProgramName,
                                                  Metadata) of
        { ok, _ProgramId } ->
            {ok, #{ <<"link">> => generate_url_for_program_name(Username, NewProgramName) }};
        { error, Reason } ->
            {error, Reason}
    end.

-spec update_program_metadata(binary(), #editable_user_program_metadata{}) -> ok | {error, binary()}.
update_program_metadata(ProgramId, Metadata=#editable_user_program_metadata{}) ->

    case automate_storage:update_program_metadata(ProgramId, Metadata) of
        { ok, _ProgramId } ->
            ok;
        { error, Reason } ->
            {error, Reason}
    end.


-spec delete_program(binary(), binary()) -> ok | {error, any()}.
delete_program(Username, ProgramName) ->
    case automate_storage:delete_program(Username, ProgramName) of
        {ok, ProgramId} ->
            {ok, _} = automate_bot_engine_launcher:stop_program(ProgramId),
            ok;
        X ->
            X
    end.

-spec delete_program(binary()) -> ok | {error, any()}.
delete_program(ProgramId) ->
    case automate_storage:delete_program(ProgramId) of
        ok ->
            {ok, _} = automate_bot_engine_launcher:stop_program(ProgramId),
            ok;
        X ->
            X
    end.


-spec list_services_from_username(binary()) -> {'ok', [ #service_metadata{} ]} | {error, term(), binary()}.
list_services_from_username(Username) ->
    {ok, Owner} = automate_storage:get_userid_from_username(Username),
    case automate_service_registry:get_all_services_for_user(Owner) of
        {ok, Services} ->
            {ok, get_services_metadata(Services, Owner)};
        E = {error, _, _} ->
            E
    end.

get_services_metadata(Services, Owner) ->
    lists:filter(fun (V) ->
                         V =/= none
                 end,
                 lists:map(fun ({K, V}) -> get_service_metadata(K, V, Owner) end,
                           maps:to_list(Services))).


-spec get_service_enable_how_to(binary(), binary()) -> {ok, map() | none} | {error, not_found}.
get_service_enable_how_to(Username, ServiceId) ->
    case get_platform_service_how_to(Username, ServiceId) of
        {ok, HowTo} ->
            {ok, HowTo};
        {error, not_found} ->
            automate_logging:log_api(error, ?MODULE, "Unknown platform service required"),
            {error, not_found}
    end.


-spec create_service_port(binary(), binary()) -> {ok, {binary(), binary()}}.
create_service_port(Username, ServicePortName) ->
    {ok, Owner} = automate_storage:get_userid_from_username(Username),
    {ok, ServicePortId } = automate_service_port_engine:create_service_port(Owner, ServicePortName),
    {ok, {?URLS:bridge_control_url(ServicePortId), ServicePortId}}.

-spec list_custom_blocks_from_username(binary()) -> {ok, map()}.
list_custom_blocks_from_username(Username) ->
    {ok, Owner} = automate_storage:get_userid_from_username(Username),
    automate_service_port_engine:list_custom_blocks(Owner).


-spec register_service(binary(), binary(), map(), binary()) -> {ok, any} | {error, binary()}.
register_service(Username, ServiceId, RegistrationData, ConnectionId) ->
    {ok, Owner} = automate_storage:get_userid_from_username(Username),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId),
    {ok, _Result} = automate_service_registry_query:send_registration_data(Module, Owner, RegistrationData,
                                                                           #{<<"connection_id">> => ConnectionId}).

-spec send_oauth_return(binary(), binary()) -> ok | {error, term()}.
send_oauth_return(ServicePortId, Qs) ->
    case automate_service_port_engine:send_oauth_return(Qs, ServicePortId) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec list_bridges(binary()) -> {ok, [#service_port_entry_extra{}]}.
list_bridges(Username) ->
    {ok, Owner} = automate_storage:get_userid_from_username(Username),
    {ok, _ServicePorts} = automate_service_port_engine:get_user_service_ports(Owner).

-spec list_available_connections(owner_id()) -> {ok, [{#service_port_entry{}, #service_port_configuration{}}]}.
list_available_connections(Owner) ->
    case automate_service_registry:get_all_services_for_user(Owner) of
        {ok, Services} ->
            EnabledServices = lists:filtermap(fun({ _ServiceId, #{ module := Module } }) ->
                                                      case automate_service_port_engine:is_module_connectable_bridge(Owner, Module) of
                                                          false -> false;
                                                          {false, _BridgeData} ->
                                                              false;
                                                          {true, BridgeData} ->
                                                              { true, BridgeData }
                                                      end
                            end, maps:to_list(Services)),
            {ok, EnabledServices};
        E = {error, _, _} ->
            E
    end.

-spec list_established_connections(binary()) -> {ok, [#user_to_bridge_connection_entry{}]}.
list_established_connections(UserId) ->
    automate_service_port_engine:list_established_connections({user, UserId}).

callback_bridge(UserId, BridgeId, Callback) ->
    automate_service_port_engine:callback_bridge(UserId, BridgeId, Callback).

-spec bridge_function_call(owner_id(), binary(), binary(), any()) -> {ok, map()} |{ error, term()}.
bridge_function_call(UserId, BridgeId, FunctionName, Arguments) ->
    automate_service_port_engine:call_service_port(BridgeId, FunctionName, Arguments, UserId, #{}).

%% Custom signals
-spec create_custom_signal(owner_id(), binary()) -> {ok, binary()}.
create_custom_signal(Owner, SignalName) ->
    automate_storage:create_custom_signal(Owner, SignalName).

%% Templates
-spec create_template(owner_id(), binary(), [any()]) -> {ok, binary()}.
create_template(Owner, TemplateName, TemplateContent) ->
    automate_template_engine:create_template(Owner, TemplateName, TemplateContent).

-spec delete_template(owner_id(), binary()) -> ok | {error, binary()}.
delete_template(Owner, TemplateId) ->
    automate_template_engine:delete_template(Owner, TemplateId).

-spec update_template(owner_id(), binary(), binary(), [any()]) -> ok | {error, binary()}.
update_template(Owner, TemplateId, TemplateName, TemplateContent) ->
    automate_template_engine:update_template(Owner, TemplateId, TemplateName, TemplateContent).

-spec get_template(owner_id(), binary()) -> {ok, #template_entry{}} | {error, binary()}.
get_template(Owner, TemplateId) ->
    automate_template_engine:get_template(Owner, TemplateId).

%%====================================================================
%% Internal functions
%%====================================================================
register_user_instantly(#registration_rec{ email=Email
                                         , password=Password
                                         , username=Username
                                         }) ->
    case automate_storage:create_user(Username, Password, Email, ready) of
        { ok, _UserId } ->
            { ok, continue };
        { error, Reason } ->
            { error, Reason }
    end.

register_user_require_validation(#registration_rec{ email=Email
                                                  , password=Password
                                                  , username=Username
                                                  }) ->
    case automate_storage:create_user(Username, Password, Email, mail_not_verified) of
        { ok, UserId } ->
            case automate_storage:create_mail_verification_entry(UserId) of
                {ok, MailVerificationCode} ->
                     case automate_mail:send_registration_verification(Username, Email, MailVerificationCode) of
                         { ok, _Url } ->
                             { ok, wait_for_mail_verification };
                         {error, Reason} ->
                             automate_storage:delete_user(UserId),
                             {error, Reason}
                     end;
                {error, Reason} ->
                    automate_storage:delete_user(UserId),
                    {error, Reason}
            end;
        { error, Reason } ->
            { error, Reason }
    end.

get_service_metadata(Id
                    , #{ name := Name
                       , description := _Description
                       , module := Module
                       }
                    , Owner) ->
    try automate_service_registry_query:is_enabled_for_user(Module, Owner) of
        {ok, Enabled} ->
            #service_metadata{ id=Id
                             , name=Name
                             , link=?URLS:service_id_url(Id)
                             , enabled=Enabled
                             }
    catch X:Y ->
            automate_logging:log_api(error, ?MODULE, io_lib:format("Error getting service metadata ~p:~p", [X, Y])),
            none
    end.

generate_url_for_program_name(Username, ProgramName) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/programs/~s", [Username, ProgramName]))).

generate_url_for_monitor_name(Username, MonitorName) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/monitors/~s", [Username, MonitorName]))).

program_entry_to_program(#user_program_entry{ id=Id
                                            , owner=Owner
                                            , program_name=ProgramName
                                            , program_type=ProgramType
                                            , program_parsed=ProgramParsed
                                            , program_orig=ProgramOrig
                                            , enabled=Enabled
                                            , last_upload_time=LastUploadTime
                                            }) ->
    {OwnerType, OwnerId} = Owner,
    #user_program{ id=Id
                 , owner=#{ type => OwnerType, id => OwnerId }
                 , program_name=ProgramName
                 , program_type=ProgramType
                 , program_parsed=ProgramParsed
                 , program_orig=ProgramOrig
                 , enabled=Enabled
                 , last_upload_time=LastUploadTime
                 }.

-spec get_platform_service_how_to(binary(), binary()) -> {ok, map() | none} | {error, not_found}.
get_platform_service_how_to(Username, ServiceId)  ->
    {ok, Owner} = automate_storage:get_userid_from_username(Username),
    case automate_service_registry:get_service_by_id(ServiceId) of
        E = {error, not_found} ->
            E;
        {ok, #{ module := Module }} ->
            automate_service_registry_query:get_how_to_enable(Module, Owner)
    end.
