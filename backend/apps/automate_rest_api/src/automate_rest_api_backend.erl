-module(automate_rest_api_backend).

%% API exports
-export([register_user/1]).

%% Definitions
-include("./records.hrl").

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

%%====================================================================
%% Internal functions
%%====================================================================
generate_url_from_userid(UserId) ->
    binary:list_to_bin(lists:flatten(io_lib:format("/api/v0/users/~s", [UserId]))).
