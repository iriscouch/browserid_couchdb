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

-export([browserid_authentication_handler/1, handle_id_req/1]).

-import(couch_httpd, [header_value/2, send_method_not_allowed/2]).

%% TODO: 
%% * Set SSL options so we verify the providers cert
%% * Possibly auto-create user doc in browserid_authentication_handler/1
%% * Do something sane with providers other than browserid.org

browserid_authentication_handler(#httpd{mochi_req=MochiReq}=Req) ->
    case MochiReq:get_cookie_value("BrowserIDAssertion") of
    undefined -> Req;
    [] -> Req;
    Cookie ->
        % This format could be changed still
        % Not sure what's good
        [_User, _TimeStr, _Assertion] = try
            string:tokens(?b2l(couch_util:decodeBase64Url(Cookie)), ":")
        catch
            _:_Error ->
                Reason = <<"Malformed browserid cookie. "
                           "Please clear your cookies.">>,
                throw({bad_request, Reason})
        end,
        % Verify cookie expire, check auth cache, etc
        % Possibly call verify/2 if not cached
        % Set the user context on Req
       Req
    end.


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
handle_id_req(#httpd{method='POST', mochi_req=MochiReq}=Req) ->
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
        % tilgovi: Set the cookie, send a redirect?
        % JasonSmith: Does a bear wear a funny hat?
        , ?LOG_DEBUG("Verified:\n~p", [Verified_obj])
        , couch_httpd:send_json(Req, 200, {Verified_obj})
    end;

handle_id_req(_Req) ->
    % Send 405
    not_implemented.


verify_id(Assertion, Audience) -> ok
    % TODO: Verify without depending on the Mozilla crutch verification web service.
    %       * https://wiki.mozilla.org/Identity/Verified_Email_Protocol
    %       * https://github.com/mozilla/browserid/tree/master/verifier
    , case couch_config:get("httpd", "browserid_verify_url", undefined)
        of undefined -> ok
            % Bad config.
            , throw({missing_config_value, "Required config httpd/browserid_verify_url"})
        ; VerifyURL -> ok
            , verify_id_with_crutch(VerifyURL, Assertion, Audience)
        end
    .


verify_id_with_crutch(VerifyURL, Assertion, Audience) ->
    VerifyQS = "?assertion=" ++ Assertion ++ "&audience=" ++ Audience,

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
        Worker, VerifyURL ++ VerifyQS,
        [{content_type, "application/json"}], get) of
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
                , case couch_util:get_value(<<"status">>, Resp)
                    of <<"okay">> -> ok
                        , ?LOG_DEBUG("BrowserID verified: ~p", [Resp])
                        , {ok, Resp}
                    ; _Not_ok -> ok
                        , ?LOG_DEBUG("Failed verification from ~s:\n~p", [VerifyURL, Resp])
                        , {error, failed_verification}
                    end
            end
    end.


%
% Utilities
%

send_from_dir(Req, Dir) -> ok
    , couch_httpd_misc_handlers:handle_utils_dir_req(Req, Dir)
    .

% vim: sts=4 sw=4 et
