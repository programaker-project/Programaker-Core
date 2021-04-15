-module(automate_rest_api_utils).
-export([ read_body/1
        , send_json_output/2
        , send_json_format/1
        , stream_body_to_file/3
        , stream_body_to_file_hashname/3
        , copy_asset/3
        , user_picture_path/1
        , user_has_picture/1
        , user_picture_modification_time/1
        , group_picture_path/1
        , group_has_picture/1
        , group_picture_modification_time/1
        , get_bridges_on_program_id/1
        , get_owner_asset_directory/1
        , is_public/1

        , start_metrics/2
        , end_metrics/1
        , end_metrics_with_error/2
        ]).

-include("../../automate_common_types/src/types.hrl").
-define(HASH_ALGORITHM, sha3_256).

read_body(Req0) ->
    read_body(Req0, <<>>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

send_json_output(Output, Req) ->
    Res1 = cowboy_req:set_resp_body(Output, Req),
    Res2 = cowboy_req:delete_resp_header(<<"content-type">>, Res1),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res2).

send_json_format(Req) ->
    Res1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Res1).

stream_body_to_file(Req, Path, FileName) ->
    TmpPath = tmp_path(),
    ok = filelib:ensure_dir(TmpPath),
    {ok, File} = file:open(TmpPath, [write, raw]),
    try multipart(Req, File, #{}, FileName, nohash) of
        Res ->
            ok = filelib:ensure_dir(Path),
            ok = movefile(TmpPath, Path),
            Res
    after
        case filelib:is_file(TmpPath) of
            true ->
                ok = file:close(File),
                ok = file:delete(TmpPath);
            false ->
                ok
        end
    end.

stream_body_to_file_hashname(Req, Path, FileKey) ->
    TmpPath = tmp_path(),
    ok = filelib:ensure_dir(TmpPath),
    {ok, File} = file:open(TmpPath, [write, raw]),
    try multipart(Req, File, #{}, FileKey, hash) of
        {ok, #{ FileKey := {Hash, FileType} }, Req1} ->
            Id = url_safe_base64_encode(Hash),
            FullPath = list_to_binary(io_lib:format("~s/~s", [Path, Id])),
            ok = filelib:ensure_dir(FullPath),
            ok = movefile(TmpPath, FullPath),
            {ok, {Id, FileType}, Req1}
    after
        case filelib:is_file(TmpPath) of
            true ->
                ok = file:close(File),
                ok = file:delete(TmpPath);
            false ->
                ok
        end
    end.

copy_asset(FromOwner, ToOwner, AssetId) ->
    FromDir = get_owner_asset_directory(FromOwner),
    FromPath = list_to_binary([FromDir, "/", AssetId]),
    ToDir = get_owner_asset_directory(ToOwner),
    ToPath = list_to_binary([ToDir, "/", AssetId]),

    ok = filelib:ensure_dir(ToPath),

    {ok, _BytesCopied} = file:copy(FromPath, ToPath),
    ok.

user_has_picture(UserId) ->
    filelib:is_file(user_picture_path(UserId)).

user_picture_modification_time(UserId) ->
    case filelib:last_modified(user_picture_path(UserId)) of
        0 -> {error, not_found};
        Date -> { ok, Date }
    end.

-spec user_picture_path(binary()) -> binary().
user_picture_path(UserId) ->
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s/picture", [automate_configuration:asset_directory("public/users/")
                                                   , UserId
                                                   ]))).

group_has_picture(GroupId) ->
    filelib:is_file(group_picture_path(GroupId)).

-spec group_picture_path(binary()) -> binary().
group_picture_path(GroupId) ->
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s/picture", [automate_configuration:asset_directory("public/groups/")
                                                   , GroupId
                                                   ]))).

group_picture_modification_time(GroupId) ->
    case filelib:last_modified(group_picture_path(GroupId)) of
        0 -> {error, not_found};
        Date -> { ok, Date }
    end.

-spec get_owner_asset_directory(owner_id()) -> binary().
get_owner_asset_directory({OwnerType, OwnerId}) ->
    OwnerTypeDir = case OwnerType of
                       user -> "users";
                       group -> "groups"
                   end,
    list_to_binary(io_lib:format("~s/~s/_assets",
                                 [automate_configuration:asset_directory("public/" ++ OwnerTypeDir ++ "/" )
                                 , OwnerId
                                 ])).


-spec is_public(user_program_visibility()) -> boolean().
is_public(Visibility) ->
    case Visibility of
        public -> true;
        shareable -> true;
        private -> false
    end.
%% Auxiliary
movefile(Source, Target) ->
    case file:rename(Source, Target) of
        ok ->
            ok;
        {error, exdev} -> %% Source and target on different devices
            {ok, _BytesCopied} = file:copy(Source, Target),
            ok = file:delete(Source)
    end.

tmp_path() ->
    BackupName = atom_to_list(?MODULE) ++ "/" ++ integer_to_list(erlang:phash2(make_ref())),
    BackupDir = filename:basedir(user_cache, "automate"),
    BackupDir ++ "/" ++ BackupName.

multipart(Req0, File, Data, ToSave, Options) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req} ->
            {ReqCont, NewData} = case cow_multipart:form_data(Headers) of
                                     {data, FieldName} ->
                                         {ok, Body, Req2} = cowboy_req:read_part_body(Req),
                                         {Req2, Data#{ FieldName => Body }};
                                     {file, ToSave, _FileName, FileType} ->
                                         InfoAcc = case Options of
                                                       nohash -> 0;
                                                       hash -> {hash, crypto:hash_init(?HASH_ALGORITHM)}
                                                   end,
                                         {ok, Req2, Result} = stream_body_content_to_file(Req, File, InfoAcc),
                                         {Req2, Data#{ ToSave => {Result, FileType} }};
                                     {file, _, _, _} ->
                                         {Req, Data}
                                 end,
            multipart(ReqCont, File, NewData, ToSave, Options);
        {done, Req} ->
            {ok, Data, Req}
    end.

stream_body_content_to_file(Req0, File, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, Data, Req} ->
            ok = file:write(File, Data),
            NextData = case Acc of
                           X when is_number(X) -> X + size(Data);
                           {hash, HashAcc} ->
                               crypto:hash_final(crypto:hash_update(HashAcc, Data))
                       end,
            {ok, Req, NextData};
        {more, Data, Req} ->
            ok = file:write(File, Data),
            NextData = case Acc of
                           X when is_number(X) -> X + size(Data);
                           {hash, HashAcc} ->
                               {hash, crypto:hash_update(HashAcc, Data)}
                       end,
            stream_body_content_to_file(Req, File, NextData)
    end.

get_bridges_on_program_id(ProgramId) ->
    {ok, Program} = automate_storage:get_program_from_id(ProgramId),
    {ok, Bridges} = automate_bot_engine:get_bridges_on_program(Program),
    Bridges.

%% Note that this is not going to be decoded (although the replacements are reversible)
url_safe_base64_encode(Bin) ->
    RawB64 = base64:encode(Bin),
    %% Replace elements with specific URL semantics
    S1 = binary:replace(RawB64, <<"/">>, <<"-">>, [global]),
    S2 = binary:replace(S1, <<"+">>, <<"_">>, [global]),
    S2.

%%====================================================================
%% Metrics
%%====================================================================
-type user_agent_bucket() :: google_apps_script | other.
-record(metrics_data, { user_agent_bucket :: user_agent_bucket()
                      , start_time :: integer()
                      , endpoint :: atom()
                      }).

get_user_agent_bucket(undefined) ->
    other;
get_user_agent_bucket(UserAgent) ->
    case binary:matches(UserAgent, <<"Google-Apps-Script">>) of
        [] -> other;
        _ -> google_apps_script
    end.

start_metrics(Req, Endpoint) ->
    #metrics_data{ user_agent_bucket=get_user_agent_bucket(cowboy_req:header(<<"user-agent">>, Req))
                 , start_time=erlang:monotonic_time()
                 , endpoint=Endpoint
                 }.

end_metrics(#metrics_data{ user_agent_bucket=Bucket
                         , start_time=StartTime
                         , endpoint=Endpoint
                         }) ->
    EndTime = erlang:monotonic_time(),
    TimeElapsed = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
    prometheus_histogram:observe(default, automate_api_latency, [Endpoint, Bucket, ok], TimeElapsed).

end_metrics_with_error(#metrics_data{ user_agent_bucket=Bucket
                                    , start_time=StartTime
                                    , endpoint=Endpoint
                                    }, Error) ->
    EndTime = erlang:monotonic_time(),
    TimeElapsed = erlang:convert_time_unit(EndTime - StartTime, native, millisecond),
    prometheus_histogram:observe(default, automate_api_latency, [Endpoint, Bucket, Error], TimeElapsed).
