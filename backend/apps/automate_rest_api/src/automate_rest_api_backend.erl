-module(automate_rest_api_backend).

%% API exports
-export([ register_user/1
        , login_user/1
        , is_valid_token/1
        , create_program/1
        , get_program/2
        , lists_programs_from_username/1
        , update_program/3
        , list_services_from_username/1
        , get_service_enable_how_to/2
        ]).

%% Definitions
-include("./records.hrl").
-include("../../automate_storage/src/records.hrl").

%%====================================================================
%% API functions
%%====================================================================
register_user(#registration_rec{ email=Email
                               , password=Password
                               , username=Username
                               }) ->
    case automate_storage:create_user(Username, Password, Email) of
        { ok, UserId } ->
            Url = generate_url_from_userid(UserId),
            io:format("Url: ~p~n", [Url]),
            { ok, Url };
        { error, Reason } ->
            { error, Reason }
    end.

login_user(#login_rec{ password=Password
                     , username=Username
                     }) ->
    case automate_storage:login_user(Username, Password) of
        { ok, Token } ->
            { ok, Token };
        { error, Reason } ->
            { error, Reason }
    end.

is_valid_token(Token) when is_binary(Token) ->
    case automate_storage:get_session_username(Token) of
        { ok, Username } ->
            {true, Username};
        { error, session_not_found } ->
            false;
        { error, Reason } ->
            io:format("Error getting session: ~p~n", [Reason]),
            false
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

-spec lists_programs_from_username(string()) -> {'ok', [ #program_metadata{} ] }.
lists_programs_from_username(Username) ->
    case automate_storage:lists_programs_from_username(Username) of
        {ok, Programs} ->
            {ok, [#program_metadata{ id=ProgramId
                                   , name=ProgramName
                                   , link=generate_url_for_program_name(Username, ProgramName)
                                   }
                  || {ProgramId, ProgramName} <- Programs]}
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

-spec list_services_from_username(string()) -> {'ok', [ #service_metadata{} ]}.
list_services_from_username(Username) ->
    {ok, get_telegram_services_from_username(Username)}.


get_service_enable_how_to(Username, ServiceId) ->
    case get_platform_service_how_to(Username, ServiceId) of
        {ok, HowTo} ->
            {ok, HowTo};
        {error, not_found} ->
            %% TODO: Implement user-defined services
            io:format("[Error] Non platform service required~n"),
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
generate_url_from_userid(UserId) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s", [UserId]))).

%% *TODO* generate more interesting names.
generate_program_name() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).

generate_url_for_program_name(Username, ProgramName) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/programs/~s", [Username, ProgramName]))).


program_entry_to_program(#user_program_entry{ id=Id
                                            , user_id=UserId
                                            , program_name=ProgramName
                                            , program_type=ProgramType
                                            , program_parsed=ProgramParsed
                                            , program_orig=ProgramOrig
                                            }) ->
    #user_program{ id=Id
                 , user_id=UserId
                 , program_name=ProgramName
                 , program_type=ProgramType
                 , program_parsed=ProgramParsed
                 , program_orig=ProgramOrig
                 }.

-spec get_telegram_services_from_username(string()) -> [ #service_metadata{} ].
get_telegram_services_from_username(Username) ->
    DefaultId = automate_bot_engine_telegram:get_platform_id(),
    DefaultName = automate_bot_engine_telegram:get_platform_name(),
    case automate_bot_engine_telegram:is_enabled() of
        true ->
            {ok, HasEnabled} = automate_bot_engine_telegram:user_has_enabled_platform(Username),
            [ #service_metadata{ id=DefaultId
                               , name=DefaultName
                               , link=generate_url_from_service(Username, DefaultId)
                               , enabled=HasEnabled
                               } ];
        false ->
            []
    end.

-spec generate_url_from_service(binary(), binary()) -> binary().
generate_url_from_service(Username, ServiceId) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s/services/~s", [Username, ServiceId]))).

-spec get_platform_service_how_to(binary(), binary()) -> {ok, #service_enable_how_to{}} | {error, any()}.
get_platform_service_how_to(Username, ServiceId)  ->
    TelegramPlatformId = automate_bot_engine_telegram:get_platform_id(),

    case ServiceId of
        TelegramPlatformId ->
            get_telegram_platform_enable_how_to(Username);

        _ ->
            {error, not_found}
    end.

-spec get_telegram_platform_enable_how_to(binary()) -> {ok, #service_enable_how_to{}}.
get_telegram_platform_enable_how_to(Username) ->
    {ok, RegistrationToken} = automate_bot_engine_telegram:get_registration_token(Username),
    BotName = automate_bot_engine_telegram:get_bot_name(),
    case get_telegram_services_from_username(Username) of
        [ Service | _] ->
            {ok, #service_enable_how_to{ service=Service
                                       , method='external'
                                       , extra=#service_enable_extra_telegram{ token=RegistrationToken
                                                                             , bot_name=BotName
                                                                             }
                                       }}
    end.
