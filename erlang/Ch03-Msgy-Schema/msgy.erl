-module(msgy).
-export([main/0]).
-include("msgy.hrl").

main() ->
  %% Setup our repositories
  {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),

  %% Create and save users
  Joe = #user{user_id="joeuser",
              nick_name="Joe User"},

  Marleen = #user{user_id="marleenmgr",
                  nick_name="Marleen Manager"},

  user_repository:save_user(Pid, Joe),
  user_repository:save_user(Pid, Marleen),

  %% Create new Msg, post to timelines
  Msg = msg_repository:create_msg(Marleen#user.user_id, Joe#user.user_id, 'private', "Welcome to the company!"),
  timeline_repository:post_msg(Pid, Msg),


  %% Get Joe's inbox for today, get first message
  {TodaysDate,_} = calendar:now_to_universal_time(erlang:now()),
  JoesInboxToday = timeline_repository:get_timeline(Pid, Joe#user.user_name, inbox, TodaysDate),

  JoesFirstMessage = msg_repository:get_msg(Pid, hd(JoesInboxToday#timeline.msgs)),

  io:format("From: ~s~nMsg : ~s~n~n", [JoesFirstMessage#msg.sender, JoesFirstMessage#msg.text]),
  ok.
