-module(automate_service_user_registration_backend_mnesia).

-export([ start_link/0
        , gen_registration_token/2
        , get_registration_token/2
        , get_info_from_registration_token/1
        ]).

-include("records.hrl").
-include("../../automate_storage/src/records.hrl").
-define(SERVICE_REGISTRATION_TOKEN_TABLE, automate_service_registration_token_table).

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
    Nodes = [node()],

    %% Service registration token table
    ok = case mnesia:create_table(?SERVICE_REGISTRATION_TOKEN_TABLE,
                                  [ {attributes, record_info(fields, service_registration_token)}
                                  , { disc_copies, Nodes }
                                  , { record_name, service_registration_token }
                                  , { type, set }
                                  ]) of
             { atomic, ok } ->
                 ok;
             { aborted, { already_exists, _ }} ->
                 ok
         end,
    ok = mnesia:wait_for_tables([?SERVICE_REGISTRATION_TOKEN_TABLE], 
                                automate_configuration:get_table_wait_time()),
    ignore.


-spec get_registration_token(binary(), binary()) -> {ok, binary()} | { error, not_found }.
get_registration_token(Username, ServiceId) ->
    Transaction = fun() ->
                          case automate_storage:get_userid_from_username(Username) of
                              {ok, UserId} ->
                                  TokenMatchHead = #service_registration_token{ token='$1'
                                                                              , service_id='$2'
                                                                              , user_id='$3'
                                                                              },

                                  %% Check that neither the id, username or email matches another
                                  GuardService = {'==', '$2', ServiceId},
                                  GuardUserId = {'==', '$3', UserId},
                                  TokenGuard = {'andthen', GuardService, GuardUserId},
                                  TokenResultColumn = '$1',
                                  TokenMatcher = [{TokenMatchHead, [TokenGuard], [TokenResultColumn]}],

                                  case mnesia:select(?SERVICE_REGISTRATION_TOKEN_TABLE, TokenMatcher) of
                                      [Token] ->
                                          { ok, Token };
                                      [] ->
                                          {error, not_found}
                                  end;
                              {error, no_user_found} ->
                                  {error, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec gen_registration_token(binary(), binary()) -> {ok, binary()}.
gen_registration_token(Username, ServiceId) ->
    Token = generate_id(),

    Transaction = fun() ->
                          case automate_storage:get_userid_from_username(Username) of
                              {ok, UserId} ->
                                  TokenRegistration = #service_registration_token{ token=Token
                                                                                 , service_id=ServiceId
                                                                                 , user_id=UserId
                                                                                 },
                                  ok = mnesia:write(?SERVICE_REGISTRATION_TOKEN_TABLE, TokenRegistration, write),
                                  {ok, Token};
                              {error, no_user_found} ->
                                  {ok, not_found}
                          end
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, Result } ->
            Result;
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.

-spec get_info_from_registration_token(binary()) -> {ok, #service_registration_token{}} | {error, not_found}.
get_info_from_registration_token(Token) ->
    Transaction = fun () ->
                          mnesia:read(?SERVICE_REGISTRATION_TOKEN_TABLE, Token)
                  end,
    case mnesia:transaction(Transaction) of
        { atomic, [] } ->
            {error, not_found};
        { atomic, [Result = #service_registration_token{}] } ->
            {ok, Result};
        { aborted, Reason } ->
            {error, mnesia:error_description(Reason)}
    end.




%%====================================================================
%% Internal functions
%%====================================================================
generate_id() ->
    binary:list_to_bin(uuid:to_string(uuid:uuid4())).
