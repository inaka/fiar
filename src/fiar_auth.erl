-module(fiar_auth).
-author('euen@inakanetworks.com').

-export([
         check_auth/1,
         current_user/1,
         credentials/1
        ]).

-define(AUTH_HEADER, <<"Basic realm=\"FiaR\"">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_auth(Req) ->
  case current_user(Req) of
    undefined ->
      {not_authenticated, ?AUTH_HEADER, Req};
    notfound ->
      {not_authenticated, ?AUTH_HEADER, Req};
    User ->
      {authenticated, User, Req}
  end.

current_user(Req) ->
  case credentials(Req) of
    undefined ->
      undefined;
    {Username, Pass} ->
      fiar_user_repo:get(Username, Pass)
  end.

credentials(Req) ->
  try cowboy_req:parse_header(<<"authorization">>, Req) of
    {ok, {<<"basic">>, Credentials}, _} ->
      Credentials;
    {ok, undefined, _} ->
        case cowboy_req:cookie(<<"auth">>, Req) of
          {undefined, _} -> undefined;
          {Token, _} -> 
            parse_cookie_auth(Token)
        end
      % lager:info("fiar_notify_users_handler: user ~p, pass ~p", [User, Pass]),
  catch
    _:Exception ->
      ErrorMsg = "error trying to check auth: ~p~n\tStack: ~p~n",
      lager:warning(ErrorMsg, [Exception, erlang:get_stacktrace()]),
      throw(Exception)
  end.

parse_cookie_auth(Token) ->
  try base64:decode(Token) of
    UserPass -> 
      case binary:split(UserPass, <<":">>) of
        [User, Pass] -> {User, Pass};
        _ -> undefined
      end
  catch
    _:_ -> undefined
  end.
