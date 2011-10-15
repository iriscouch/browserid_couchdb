% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_browserid).
-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

-export([handle_id_req/1]).

-import(couch_httpd, [header_value/2, send_method_not_allowed/2]).

%% TODO: 
%% * Set SSL options so we verify the providers cert
%% * Possibly auto-create user doc in browserid_authentication_handler/1
%% * Do something sane with providers other than browserid.org

% hash_if_required looks for hash_secret in browserid section of config
% If it exist and isn't an empty string, uses it as hmac key according to code from
% http://stackoverflow.com/questions/4193543/erlang-calculating-hmac-sha1-example/4202361#4202361
% If missing or empty (i.e. admin doesn't want to hash usernames), simply returns the argument
% (this is the default)
% If you use hash_secret, make sure the string is long enough and cryptographically random
% Tip: use one of the strings from https://api.wordpress.org/secret-key/1.1/ :)

hash_if_required(Email) -> ok
    , Hashkey = couch_config:get("browserid", "hash_secret", undefined)
    , case Hashkey
        of undefined -> ok
            , Email
        ; Key -> ok
            , <<Mac:160/integer>> = crypto:sha_mac(?l2b(Key),Email)
            , ?l2b(lists:flatten(io_lib:format("~40.16.0b", [Mac])))
        end
    .

handle_id_req(#httpd{method='GET'}=Req) -> ok
    , case code:priv_dir(browserid_couchdb)
        of {error, bad_name} -> ok
            , Message = <<"Cannot find browserid helper files">>
            , ?LOG_ERROR("~s", [Message])
            , couch_httpd:send_json(Req, 500, {[{error, Message}]})
        ; Priv_dir -> ok
            , send_from_dir(Req, Priv_dir)
        end
    ;

% Login handler with Browser ID.
handle_id_req(#httpd{method='POST'}=Req) -> ok
    , case couch_config:get("browserid", "enabled")
        of "true" -> ok
            , handle_id_req(enabled, Req)
        ; _ -> ok
            % Browserid is disabled in the config.
            , throw({error, browserid_not_enabled})
        end
    ;

handle_id_req(_Req) ->
    % Send 405
    not_implemented.

handle_id_req(enabled, #httpd{method='POST', mochi_req=MochiReq}=Req) ->
    ReqBody = MochiReq:recv_body(),
    Form = case MochiReq:get_primary_header_value("content-type") of
        % content type should be json
        "application/x-www-form-urlencoded" ++ _ ->
            mochiweb_util:parse_qs(ReqBody);
        "application/json" ++ _ ->
            {Pairs} = ?JSON_DECODE(ReqBody),
            lists:map(fun({Key, Value}) ->
              {?b2l(Key), ?b2l(Value)}
            end, Pairs);
        _ ->
            []%couch_httpd:send_json(Req, 406, {error, method_not_allowed})
    end,
    Assertion = couch_util:get_value("assertion", Form, ""),
    Audience = MochiReq:get_header_value("host"),
    case verify_id(Assertion, Audience) of
    {error, _Reason} ->
        % Send client an error response, couch_util:send_err ...
        not_implemented;
    {ok, Verified_obj} -> ok
        , send_good_id(Req, Verified_obj)
    end.


verify_id(Assertion, Audience) -> ok
    % TODO: Verify without depending on the Mozilla crutch verification web service.
    %       * https://wiki.mozilla.org/Identity/Verified_Email_Protocol
    %       * https://github.com/mozilla/browserid/tree/master/verifier
    , case couch_config:get("browserid", "verify_url", undefined)
        of undefined -> ok
            % Bad config.
            , throw({missing_config_value, "Required config httpd/browserid_verify_url"})
        ; VerifyURL -> ok
            , verify_id_with_crutch(VerifyURL, Assertion, Audience)
        end
    .


verify_id_with_crutch(VerifyURL, Assertion, Audience) ->
    VerifyQS = "assertion=" ++ Assertion ++ "&audience=" ++ Audience,

    %% VerifyURL = ?l2b(couch_util:get_value("verify_url", Form,
    %%                                       "https://browserid.org/verify")),
    %% case ibrowse_lib:parse_url(VerifyUrl) of
    %% #url{protocol = https} ->
    %%     % Maybe don't allow non-https providers?

    ?LOG_DEBUG("Verifying browserid audience: ~s", [Audience]),
    ?LOG_DEBUG("Verifying browserid assertion: ~s", [Assertion]),
    % Verify ALL the things!

    {ok, Worker} = ibrowse:spawn_link_worker_process(VerifyURL),
    case ibrowse:send_req_direct(
        Worker, VerifyURL,
        [{content_type, "application/x-www-form-urlencoded"}],
        post, VerifyQS) of
    {error, Reason} ->
        ?LOG_INFO("Error on browserid request: ~p", [Reason]),
        {error, Reason};
    {ibrowse_req_id, _ReqId} ->
        % Will this ever be streaming?
        % Would that mean chunked-encoding?
        % Maybe support that
        not_implemented;

    {ok, Code, Headers, Body} -> ok
        , case list_to_integer(Code)
            of Err_code when Err_code =/= 200 -> ok
                , ?LOG_DEBUG("Bad response from verification service:\n~p\n~p\n~p", [Err_code, Headers, Body])
                , {error, bad_verification_check}
            ; 200 -> ok
                , {Resp} = ?JSON_DECODE(Body)
                , ?LOG_DEBUG("Verification service response:\n~p", [Resp])
                , Status = couch_util:get_value(<<"status">>, Resp, undefined)
                , Email  = couch_util:get_value(<<"email">> , Resp, undefined)
                , case {Status, Email}
                    of {<<"okay">>, Email} when Email =/= undefined -> ok
                        , Newresp = [ {<<"status">>,<<"okay">>}
                                    , {<<"email">>,hash_if_required(Email)}
                                    , {<<"audience">>,couch_util:get_value(<<"audience">> , Resp, undefined)}
                                    , {<<"valid-until">>,couch_util:get_value(<<"valid-until">> , Resp, undefined)}
                                    , {<<"issuer">>,couch_util:get_value(<<"issuer">> , Resp, undefined)}
                                    ]
                        , ?LOG_DEBUG("BrowserID verified: ~p", [Newresp])
                        , {ok, Newresp}
                    ; _ -> ok
                        , ?LOG_DEBUG("Failed verification from ~s:\n~p", [VerifyURL, Resp])
                        , throw({error, failed_verification}) % TODO: 4xx response
                    end
            end
    end.


send_good_id(Req, Verified_obj) -> ok
    , case couch_util:get_value(<<"email">>, Verified_obj)
        of undefined -> ok
            , ?LOG_ERROR("Email address not in verification object: ~p", [Verified_obj])
            , throw({error, bad_verification})
        ; Email -> ok
            , send_good_id(Req, Verified_obj, Email)
        end
    .

send_good_id(Req, Verified_obj, Email) -> ok
    % Create or update user auth doc with access token
    , case update_or_create_user_doc(Email, Verified_obj)
        of {ok, #doc{deleted=false}=User_doc} -> ok
            , send_good_id(Req, Verified_obj, Email, User_doc)
        ; Error -> ok
            , ?LOG_ERROR("Fail to update or create user doc for ~s: ~p", [Email, Error])
            , couch_httpd:send_json(Req, 403, [], {[{error, <<"Unable to update doc">>}]})
        end
    .

send_good_id(Req, Verified_obj, Email, User_doc) -> ok
    % Set the authenticated session cookie.
    , Secret = ?l2b(ensure_cookie_auth_secret())

    , {User_body} = User_doc#doc.body
    , Salt       = couch_util:get_value(<<"salt">> , User_body, <<"">>)
    , User_name  = couch_util:get_value(<<"name">> , User_body, null)
    , User_roles = couch_util:get_value(<<"roles">>, User_body, [])
    , User_ctx   = {[ {<<"name">> , User_name}
                    , {<<"roles">>, User_roles}
                    ]}

    % Sanity check
    , case User_name
        of Email -> ok
        ; _ -> ok
            , ?LOG_ERROR("User document ~s does not match email: ~s", [User_doc#doc.id, Email])
            , throw({error, bad_user_document})
        end

    , CurrentTime = make_cookie_time()
    , Cookie = cookie_auth_cookie(Req, ?b2l(User_name), <<Secret/binary, Salt/binary>>, CurrentTime)
    % , ?LOG_INFO("=========\nCookie\n~p", [Cookie])

    , Headers = [Cookie]
    , Response_obj = couch_util:json_apply_field({<<"userCtx">>, User_ctx}, {Verified_obj})
    , couch_httpd:send_json(Req, 200, Headers, Response_obj)
    .


update_or_create_user_doc(Email, _Verified_obj) -> ok
    % TODO: Perhaps accumulate Verified_obj.issuer or .audience into the user doc?

    , User_db = ?l2b(couch_config:get("couch_httpd_auth", "authentication_db", "_users"))
    , Doc_id = <<"org.couchdb.user:", Email/binary>>

    , Admin = #user_ctx{roles=[<<"_admin">>]}
    , {ok, Db} = couch_db:open_int(User_db, [ {user_ctx,Admin} ])

    % Add a user doc if necessary.
    , {Action, Doc} = case couch_db:open_doc_int(Db, Doc_id, [])
        of {ok, #doc{deleted=false}=User_doc} -> ok
            , ?LOG_DEBUG("Found existing doc for user: ~s", [Doc_id])
            , {Body} = User_doc#doc.body
            , Is_browserid = couch_util:get_value(<<"browserid">>, Body, false)
            , Roles        = couch_util:get_value(<<"roles">>    , Body, [])
            , Is_bid_role  = lists:member(<<"browserid">>, Roles)

            , case {Is_browserid, Is_bid_role}
                of {true, true} -> ok
                    % Looks good. Nothing to do.
                    , {noop, User_doc}
                ; _ -> ok
                    , ?LOG_DEBUG("Adding browserid flag and role to existing user: ~s", [Doc_id])
                    , Bid_roles = [<<"browserid">> | Roles]

                    , With_role = couch_util:json_apply_field({<<"roles">>, Bid_roles}, {Body})
                    , With_bid  = couch_util:json_apply_field({<<"browserid">>, true}, With_role)
                    , {update, User_doc#doc{ body = With_bid }}
                end
        ; _ -> ok
            , ?LOG_DEBUG("Creating new user from BrowserID login: ~s", [Email])
            , Salt = couch_uuids:random()
            , New_doc = #doc{ id = Doc_id
                            , body = {[ {<<"_id">>  , Doc_id}
                                      , {<<"type">> , <<"user">>}
                                      , {<<"name">> , Email}
                                      , {<<"roles">>, [ <<"browserid">> ]}
                                      , {<<"salt">> , Salt}
                                      , {<<"browserid">>, true} % XXX
                                      ]}
                            }
            , {update, New_doc}
        end

    , case Action
        of noop -> ok
            , ?LOG_DEBUG("Existing user has used BrowserID before: ~s", [Doc_id])
            , {ok, Doc}
        ; update -> ok
            , ?LOG_DEBUG("Updating user doc:\n~p", [Doc])
            , case couch_db:update_doc(Db, Doc, [])
                of {ok, Rev} -> ok
                    , ?LOG_DEBUG("Updated ~s for BrowserID:\n~p", [Doc_id, Rev])
                    , {ok, Doc}
                ; Update_error -> ok
                    , ?LOG_ERROR("Failed to update ~s: ~p", [Doc_id, Update_error])
                    , {error, Update_error}
                end
        end
    .

%
% Utilities
%

send_from_dir(Req, Dir) -> ok
    , couch_httpd_misc_handlers:handle_utils_dir_req(Req, Dir)
    .


% These are not exported from couch_httpd_auth.
ensure_cookie_auth_secret() ->
    case couch_config:get("couch_httpd_auth", "secret", nil) of
        nil ->
            NewSecret = ?b2l(couch_uuids:random()),
            couch_config:set("couch_httpd_auth", "secret", NewSecret),
            NewSecret;
        Secret -> Secret
    end.

make_cookie_time() ->
    {NowMS, NowS, _} = erlang:now(),
    NowMS * 1000000 + NowS.

cookie_auth_cookie(Req, User, Secret, TimeStamp) ->
    SessionData = User ++ ":" ++ erlang:integer_to_list(TimeStamp, 16),
    Hash = crypto:sha_mac(Secret, SessionData),
    mochiweb_cookies:cookie("AuthSession",
        couch_util:encodeBase64Url(SessionData ++ ":" ++ ?b2l(Hash)),
        [{path, "/"}, cookie_scheme(Req)]).

cookie_scheme(#httpd{mochi_req=MochiReq}) ->
    case MochiReq:get(scheme) of
        http -> {http_only, true};
        https -> {secure, true}
    end.

% vim: sts=4 sw=4 et
