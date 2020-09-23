-module(automate_rest_api_utils).
-export([ read_body/1
        , send_json_output/2
        , send_json_format/1
        , stream_body_to_file/3
        , user_picture_path/1
        , user_has_picture/1
        , group_picture_path/1
        , group_has_picture/1
        , get_bridges_on_program_id/1
        ]).

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
    try multipart(Req, File, #{}, FileName) of
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

user_has_picture(UserId) ->
    filelib:is_file(user_picture_path(UserId)).

-spec user_picture_path(binary()) -> binary().
user_picture_path(UserId) ->
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s", [automate_configuration:asset_directory("public/users/")
                                           , UserId
                                           ]))).

group_has_picture(GroupId) ->
    filelib:is_file(group_picture_path(GroupId)).

-spec group_picture_path(binary()) -> binary().
group_picture_path(GroupId) ->
    binary:list_to_bin(
      lists:flatten(io_lib:format("~s/~s", [automate_configuration:asset_directory("public/groups/")
                                           , GroupId
                                           ]))).



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

multipart(Req0, File, Data, ToSave) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req} ->
            {ReqCont, NewData} = case cow_multipart:form_data(Headers) of
                                     {data, FieldName} ->
                                         {ok, Body, Req2} = cowboy_req:read_part_body(Req),
                                         {Req2, Data#{ FieldName => Body }};
                                     {file, ToSave, _FileName, _FileType} ->
                                         {ok, Req2, Result} = stream_body_content_to_file(Req, File, 0),
                                         {Req2, Data#{ ToSave => Result }};
                                     {file, _, _, _} ->
                                         {Req, Data}
                                 end,
            multipart(ReqCont, File, NewData, ToSave);
        {done, Req} ->
            {ok, Data, Req}
    end.

stream_body_content_to_file(Req0, File, Size) ->
    case cowboy_req:read_part_body(Req0) of
        {ok, Data, Req} ->
            ok = file:write(File, Data),
            {ok, Req, Size + size(Data)};
        {more, Data, Req} ->
            ok = file:write(File, Data),
            stream_body_content_to_file(Req, File, Size + size(Data))
    end.

get_bridges_on_program_id(ProgramId) ->
    {ok, Program} = automate_storage:get_program_from_id(ProgramId),
    {ok, Bridges} = automate_bot_engine:get_bridges_on_program(Program),
    Bridges.
