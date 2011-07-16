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

-export([authentication_handler/1, handle_id_req/2]).

-import(couch_httpd, [header_value/2, send_method_not_allowed/2]).

%% TODO: 
%% * Set SSL options so we verify the providers cert
%% * Possibly auto-create user doc in authentication_handler/1
%% * Do something sane with providers other than browserid.org

authentication_handler(#httpd{mochi_req=MochiReq}=Req) ->
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

% session handler
% Login handler with Browser ID.
handle_id_req(#httpd{method='POST', mochi_req=MochiReq}=Req, VerifyURL) ->
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
    case verify_id(VerifyURL, Assertion, Audience) of
    {error, Reason} ->
        % Send client an error response, couch_util:send_err ...
        not_implemented;
    ok ->
        % Set the cookie, send a redirect?
        not_implemented
    end;
handle_id_req(Req, _VerifyURL) ->
    % Send 405
    not_implemented.

verify_id(VerifyURL, Assertion, Audience) ->
    VerifyQS = "?assertion=" ++ Assertion ++ "&audience=" ++ Audience,

    %% VerifyURL = ?l2b(couch_util:get_value("verify_url", Form,
    %%                                       "https://browserid.org/verify")),
    %% case ibrowse_lib:parse_url(VerifyUrl) of
    %% #url{protocol = https} ->
    %%     % Maybe don't allow non-https providers?

    ?LOG_DEBUG("Verifying browserid assertion: ~s", [Assertion]),
    % Verify ALL the things!

    {ok, Worker} = ibrowse:spawn_link_worker_process(VerifyURL),
    case ibrowse:send_req_direct(
        Worker, VerifyURL ++ VerifyQS,
        [{content_type, "application/json"}], get) of
    {error, Reason} ->
        ?LOG_INFO("Error on browserid request: ~p", [Reason]),
        {error, Reason};
    {ibrowse_req_id, ReqId} ->
        % Will this ever be streaming?
        % Would that mean chunked-encoding?
        % Maybe support that
        not_implemented;
    {ok, Code, Headers, Body} ->
        case list_to_integer(Code) of
        200 ->
            {Resp} = ?JSON_DECODE(Body),
            ?LOG_DEBUG("Verification response from ~s: ~p", [VerifyURL, Resp]),
            case couch_util:get_value(<<"status">>, Resp) of
            <<"okay">> ->
                ok
            end
        % Handle other cases
        end
    end.
