%%%-------------------------------------------------------------------
%% @doc automate_configuration module
%% @end
%%%-------------------------------------------------------------------

-module(automate_mail).

-export([ is_enabled/0
        , send_registration_verification/3
        , send_password_recovery_verification/3
        ]).

-define(APPLICATION, automate_mail).

-define(DEFAULT_PLATFORM_NAME, "PrograMaker").

%%====================================================================
%% Utils functions
%%====================================================================
-spec is_enabled() -> boolean().
is_enabled() ->
    case application:get_env(?APPLICATION, mail_gateway, none) of
        none ->
            false;
        _ ->
            true
    end.

-spec send_registration_verification(binary(), binary(), binary()) -> {ok, binary()} | {error, any()}.
send_registration_verification(ReceiverName, ReceiverMail, Code) ->
    {ok, MailGateway} = application:get_env(?APPLICATION, mail_gateway),
    case MailGateway of
        { test_ets, EtsTable } ->
            send_registration_verification_through_ets(ReceiverName, ReceiverMail, Code, EtsTable);
        MailGateway ->
            send_registration_verification_through_mail(ReceiverName, ReceiverMail, Code, MailGateway)
    end.

%% For debugging
%% Note that for this to work, the ETS table must be public.
send_registration_verification_through_ets(ReceiverName, ReceiverMail, Code, EtsTable) ->
    Url = case application:get_env(?APPLICATION, registration_verification_url_pattern) of
              {ok, UrlPattern} ->
                  binary:list_to_bin(
                    lists:flatten(io_lib:format(UrlPattern, [Code])));
              undefined ->
                  none
          end,
    true = ets:insert(EtsTable, [{ ReceiverMail, ReceiverName, Code, Url }]),
    {ok, Url}.

-spec send_registration_verification_through_mail(binary(), binary(), binary(), binary()) -> {ok, binary()} | {error, any()}.
send_registration_verification_through_mail(ReceiverName, ReceiverMail, Code, MailGateway) ->
    {ok, Sender} = application:get_env(?APPLICATION, registration_verification_sender),
    PlatformName = application:get_env(?APPLICATION, platform_name, ?DEFAULT_PLATFORM_NAME),
    {ok, UrlPattern} = application:get_env(?APPLICATION, registration_verification_url_pattern),
    Url = binary:list_to_bin(
            lists:flatten(io_lib:format(UrlPattern, [Code]))),

    Subject = binary:list_to_bin(
                lists:flatten(
                  io_lib:format(
                    "Welcome to ~s!", [PlatformName]))),

    Message = binary:list_to_bin(
                lists:flatten(
                  io_lib:format(
                    "Hi, ~s! Welcome to PrograMaker!"
                    "<br/><br/>"
                    "<b><a href=\"~s\">Click here to finish activating your account.</a></b>"
                    "<br/><br/>"
                    "Greetings, the ~s team."
                    "<br/><br/>"
                    "PD: Note that you can respond to this mail address if you have any issue with the platform :)"
                   , [ ReceiverName, Url, PlatformName ]))),

    R = httpc:request(post,
                      { MailGateway
                      , []
                      , "application/json"
                      , jiffy:encode(
                          #{ sender => Sender
                           , receiver => ReceiverMail
                           , subject => Subject
                           , message => Message
                           , content_type => <<"text/html">>
                           })
                      }, [], []),

    case R of
        {ok, { _StatusLine, _Headers, Body }} ->
            Response = jiffy:decode(Body, [return_maps]),
            case Response of
                #{ <<"success">> := true} ->
                    {ok, Url};
                #{ <<"message">> := Msg } ->
                    {error, Msg };
                _ ->
                    {error, Response }
            end;
        {error, Reason} ->
            { error, Reason }
    end.

-spec send_password_recovery_verification(binary(), binary(), binary()) -> {ok, binary()}.
send_password_recovery_verification(ReceiverName, ReceiverMail, Code) ->
    {ok, Sender} = application:get_env(?APPLICATION, password_reset_verification_sender),
    PlatformName = application:get_env(?APPLICATION, platform_name, ?DEFAULT_PLATFORM_NAME),
    {ok, MailGateway} = application:get_env(?APPLICATION, mail_gateway),
    {ok, UrlPattern} = application:get_env(?APPLICATION, password_reset_verification_url_pattern),
    Url = binary:list_to_bin(
            lists:flatten(io_lib:format(UrlPattern, [Code]))),

    Subject = binary:list_to_bin(
                lists:flatten(
                  io_lib:format(
                    "Password reset for ~s", [PlatformName]))),

    Message = binary:list_to_bin(
                lists:flatten(
                  io_lib:format(
                    "Hi!"
                    "<br/><br/>"
                    "Someone requested a password reset for the ~s account \"~s\". You can use this link to reset your password (you can ignore it safely if it was not you)."
                    "<br/><br/>"
                    "<b><a href=\"~s\">Click here to reset your account password.</a></b>"
                    "<br/><br/>"
                    "Greetings, the ~s team."
                    "<br/><br/>"
                    "PD: Note that you can respond to this mail address if you have any issue with the platform :)"
                   , [ PlatformName, ReceiverName, Url, PlatformName ]))),

    R = httpc:request(post,
                      { MailGateway
                      , []
                      , "application/json"
                      , jiffy:encode(
                          #{ sender => Sender
                           , receiver => ReceiverMail
                           , subject => Subject
                           , message => Message
                           , content_type => <<"text/html">>
                           })
                      }, [], []),

    case R of
        {ok, { _StatusLine, _Headers, Body }} ->
            Response = jiffy:decode(Body, [return_maps]),
            case Response of
                #{ <<"success">> := true} ->
                    {ok, Url};
                #{ <<"message">> := Msg } ->
                    {error, Msg };
                _ ->
                    {error, Response }
            end;
        {error, Reason} ->
            { error, Reason }
    end.
