-module(automate_bot_engine_variables).

%% API
-export([ resolve_argument/1
        ]).

-define(SERVER, ?MODULE).
-include("../../automate_storage/src/records.hrl").
-include("program_records.hrl").
-include("instructions.hrl").

%%%===================================================================
%%% API
%%%===================================================================
resolve_argument(#{ ?TYPE := ?VARIABLE_CONSTANT
                  , ?VALUE := Value
                  }) ->
    {ok, Value}.
