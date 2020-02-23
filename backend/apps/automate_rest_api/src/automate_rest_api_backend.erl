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
        , create_program/1
        , get_program/2
        , update_program_tags/3
        , update_program_status/3
        , get_program_tags/2
        , get_program_logs/1
        , stop_program_threads/2
        , lists_programs_from_username/1
        , update_program/3
        , list_services_from_username/1
        , get_service_enable_how_to/2

        , update_program_metadata/3
        , delete_program/2

        , create_service_port/2
        , list_custom_blocks_from_username/1
        , register_service/3
        , send_oauth_return/2
        , list_bridges/1
        , list_available_connections/1
        , delete_bridge/2
        , callback_bridge/3
        , bridge_function_call/4

        , create_custom_signal/2
        , list_custom_signals_from_user_id/1

        , create_template/3
        , list_templates_from_user_id/1
        , delete_template/2
        , update_template/4
        , get_template/2
        ]).

%% Definitions
-include("./records.hrl").
-include("../../automate_service_registry/src/records.hrl").
-include("../../automate_storage/src/records.hrl").
-include("../../automate_service_port_engine/src/records.hrl").
-include("../../automate_template_engine/src/records.hrl").

%%====================================================================
%% API functions
%%====================================================================
-spec register_user(#registration_rec{}) -> {ok, continue | wait_for_mail_verification } | {error, _}.
register_user(Reg) ->
    case automate_mail:is_enabled() of
        false ->
            register_user_instantly(Reg);
        true ->
            register_user_require_validation(Reg)
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
            io:format("Error getting session: ~p~n", [Reason]),
            false
    end.

is_valid_token_uid(Token) when is_binary(Token) ->
    case automate_storage:get_session_userid(Token, true) of
        { ok, UserId } ->
            {true, UserId};
        { error, session_not_found } ->
            false;
        { error, Reason } ->
            io:format("Error getting session: ~p~n", [Reason]),
            false
    end.

-spec create_monitor(binary(), #monitor_descriptor{}) -> {ok, {binary(), binary()}}.
create_monitor(Username, #monitor_descriptor{ type=Type, name=Name, value=Value }) ->
    case automate_storage:create_monitor(Username, #monitor_entry{ type=Type
                                                                 , name=Name
                                                                 , value=Value
                                                                 , id=none %% ID generated by the storage
                                                                 , user_id=none
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

create_program(Username) ->
    ProgramName = generate_program_name(),
    case automate_storage:create_program(Username, ProgramName) of
        { ok, ProgramId } ->
            { ok, { ProgramId
                  , ProgramName
                  , generate_url_for_program_name(Username, ProgramName) } }
    end.

get_program(Username, ProgramName) ->
    case automate_storage:get_program(Username, ProgramName) of
        {ok, ProgramData} ->
            {ok, program_entry_to_program(ProgramData)};
        X ->
            X
    end.

update_program_tags(Username, ProgramName, Tags) ->
    case automate_storage:register_program_tags(ProgramName, Tags) of
        ok ->
            ok;
        { error, Reason } ->
            {error, Reason}
    end.

update_program_status(Username, ProgramName, Status) ->
    case automate_bot_engine:change_program_status(Username, ProgramName, Status) of
        ok ->
            ok;
        { error, Reason } ->
            { error , Reason }
    end.

get_program_tags(Username, ProgramId) ->
    case automate_storage:get_tags_program_from_id(ProgramId) of
        {ok, Tags} ->
            {ok, Tags};
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

stop_program_threads(UserId, ProgramId) ->
    case automate_bot_engine:stop_program_threads(UserId, ProgramId) of
        ok ->
            ok;
        { error, Reason } ->
            {error, Reason}
    end.

-spec lists_programs_from_username(binary()) -> {'ok', [ #program_metadata{} ] }.
lists_programs_from_username(Username) ->
    case automate_storage:lists_programs_from_username(Username) of
        {ok, Programs} ->
            {ok, [#program_metadata{ id=ProgramId
                                   , name=ProgramName
                                   , link=generate_url_for_program_name(Username, ProgramName)
                                   , enabled=Enabled
                                   }
                  || {ProgramId, ProgramName, Enabled} <- Programs]}
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


-spec delete_program(binary(), binary()) -> ok | {error, any()}.
delete_program(Username, ProgramName) ->
    case automate_storage:delete_program(Username, ProgramName) of
        {ok, ProgramId} ->
            {ok, _} = automate_bot_engine_launcher:stop_program(ProgramId),
            ok;
        X ->
            X
    end.


-spec list_services_from_username(binary()) -> {'ok', [ #service_metadata{} ]} | {error, term(), binary()}.
list_services_from_username(Username) ->
    {ok, UserId} = automate_storage:get_userid_from_username(Username),
    case  automate_service_registry:get_all_services_for_user(UserId) of
        {ok, Services} ->
            {ok, get_services_metadata(Services, Username)};
        E = {error, _, _} ->
            E
    end.


-spec get_service_enable_how_to(binary(), binary()) -> {ok, map() | none} | {error, not_found}.
get_service_enable_how_to(Username, ServiceId) ->
    case get_platform_service_how_to(Username, ServiceId) of
        {ok, HowTo} ->
            {ok, HowTo};
        {error, not_found} ->
            %% TODO: Implement user-defined services
            io:format("Error: non platform service required~n"),
            {error, not_found}
    end.


-spec create_service_port(binary(), binary()) -> {ok, binary()}.
create_service_port(Username, ServicePortName) ->
    {ok, UserId} = automate_storage:get_userid_from_username(Username),
    {ok, ServicePortId } = automate_service_port_engine:create_service_port(UserId, ServicePortName),
    {ok, generate_url_for_service_port(UserId, ServicePortId)}.

-spec list_custom_blocks_from_username(binary()) -> {ok, map()}.
list_custom_blocks_from_username(Username) ->
    {ok, UserId} = automate_storage:get_userid_from_username(Username),
    automate_service_port_engine:list_custom_blocks(UserId).


-spec register_service(binary(), binary(), map()) -> {ok, any} | {error, binary()}.
register_service(Username, ServiceId, RegistrationData) ->
    {ok, UserId} = automate_storage:get_userid_from_username(Username),
    {ok, #{ module := Module }} = automate_service_registry:get_service_by_id(ServiceId, UserId),
    {ok, _} = automate_service_registry_query:send_registration_data(Module, UserId, RegistrationData).

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
    {ok, UserId} = automate_storage:get_userid_from_username(Username),
    {ok, _ServicePorts} = automate_service_port_engine:get_user_service_ports(UserId).

-spec list_available_connections(binary()) -> {ok, [#service_port_entry_extra{}]}.
list_available_connections(UserId) ->
    {ok, _ServicePorts} = automate_service_port_engine:get_user_service_ports(UserId).

-spec delete_bridge(binary(), binary()) -> ok | {error, binary()}.
delete_bridge(UserId, BridgeId) ->
    automate_service_port_engine:delete_bridge(UserId, BridgeId).

callback_bridge(UserId, BridgeId, Callback) ->
    automate_service_port_engine:callback_bridge(UserId, BridgeId, Callback).

-spec bridge_function_call(binary(), binary(), binary(), any()) -> {ok, map()} |{ error, term()}.
bridge_function_call(UserId, BridgeId, FunctionName, Arguments) ->
    automate_service_port_engine:call_service_port(BridgeId, FunctionName, Arguments, UserId, #{}).

%% Custom signals
-spec create_custom_signal(binary(), binary()) -> {ok, binary()}.
create_custom_signal(UserId, SignalName) ->
    automate_storage:create_custom_signal(UserId, SignalName).

-spec list_custom_signals_from_user_id(binary()) -> {ok, [#custom_signal_entry{}]}.
list_custom_signals_from_user_id(UserId) ->
    automate_storage:list_custom_signals_from_user_id(UserId).

%% Templates
-spec create_template(binary(), binary(), [any()]) -> {ok, binary()}.
create_template(UserId, TemplateName, TemplateContent) ->
    automate_template_engine:create_template(UserId, TemplateName, TemplateContent).

-spec list_templates_from_user_id(binary()) -> {ok, [#template_entry{}]}.
list_templates_from_user_id(UserId) ->
    automate_template_engine:list_templates_from_user_id(UserId).

-spec delete_template(binary(), binary()) -> ok | {error, binary()}.
delete_template(UserId, TemplateId) ->
    automate_template_engine:delete_template(UserId, TemplateId).

-spec update_template(binary(), binary(), binary(), [any()]) -> ok | {error, binary()}.
update_template(UserId, TemplateId, TemplateName, TemplateContent) ->
    automate_template_engine:update_template(UserId, TemplateId, TemplateName, TemplateContent).

-spec get_template(binary(), binary()) -> {ok, #template_entry{}} | {error, binary()}.
get_template(UserId, TemplateId) ->
    automate_template_engine:get_template(UserId, TemplateId).

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
                         { ok, Url } ->
                             io:format("Url: ~p~n", [Url]),
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

get_services_metadata(Services, Username) ->
    lists:filter(fun (V) ->
                         V =/= none
                 end,
                 lists:map(fun ({K, V}) -> get_service_metadata(K, V, Username) end,
                           maps:to_list(Services))).

get_service_metadata(Id
                    , #{ name := Name
                       , description := _Description
                       , module := Module
                       }
                    , Username) ->
    try automate_service_registry_query:is_enabled_for_user(Module, Username) of
        {ok, Enabled} ->
            #service_metadata{ id=Id
                             , name=Name
                             , link=generate_url_for_service_id(Username, Id)
                             , enabled=Enabled
                             }
    catch X:Y ->
            io:fwrite("Error getting service metadata ~p:~p~n", [X, Y]),
            none
    end.

generate_url_for_service_id(Username, ServiceId) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/services/id/~s", [Username, ServiceId]))).

%% *TODO* generate more interesting names.
generate_program_name() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

generate_url_for_program_name(Username, ProgramName) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/programs/~s", [Username, ProgramName]))).

generate_url_for_monitor_name(Username, MonitorName) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/monitors/~s", [Username, MonitorName]))).

generate_url_for_service_port(UserId, ServicePortId) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/id/~s/bridges/id/~s/communication", [UserId, ServicePortId]))).


program_entry_to_program(#user_program_entry{ id=Id
                                            , user_id=UserId
                                            , program_name=ProgramName
                                            , program_type=ProgramType
                                            , program_parsed=ProgramParsed
                                            , program_orig=ProgramOrig
                                            , enabled=_Enabled
                                            }) ->
    #user_program{ id=Id
                 , user_id=UserId
                 , program_name=ProgramName
                 , program_type=ProgramType
                 , program_parsed=ProgramParsed
                 , program_orig=ProgramOrig
                 }.

-spec get_platform_service_how_to(binary(), binary()) -> {ok, map() | none} | {error, not_found}.
get_platform_service_how_to(Username, ServiceId)  ->
    {ok, UserId} = automate_storage:get_userid_from_username(Username),
    case automate_service_registry:get_service_by_id(ServiceId, UserId) of
        E = {error, not_found} ->
            E;
        {ok, #{ module := Module }} ->
            automate_service_registry_query:get_how_to_enable(Module, #{ user_id => UserId, user_name => Username})
    end.
