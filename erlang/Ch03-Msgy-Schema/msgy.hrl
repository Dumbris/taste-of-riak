-type key_string() :: nonempty_string().
-type msg_type() :: 'inbox' | 'sent'.
%% 'text' - message to channel or other user
%% 'data' - encoded data
%% 'invitation' - channel invitation, in case of public message - broadcast aka spam
%% 'query' - query to add myteam
%% 'reply' - allow or decline query to add myteam
%% 'peerupdate' - update status, nickname, turn off video audio
%% 'modupdate' - update user status, nickname, turn off video audio by moderator
-type private_msg_type() :: 'text' | 'data' | 'invitation' | 'query' | 'reply'.
-type public_msg_type() :: 'text' | 'data' | 'invitation' | 'peerupdate' | 'modupdate'.
-type user_id() :: nonempty_string().
-type user_pic() :: string().
-type channel_id() :: nonempty_string().
-type nick_name() :: nonempty_string().
-type datetimestamp() :: nonempty_string().
-type text() :: nonempty_string().
-type year() :: non_neg_integer().
-type month() :: non_neg_integer().
-type day() :: non_neg_integer().
-type hour() :: non_neg_integer().
-type min() :: non_neg_integer().
-type sec() :: non_neg_integer().
-type ms() :: non_neg_integer().
-type date() :: {year(), month(), day()}.

%Type describe user, info must be enougth to dislpay message with out query user info
-record(user, {user_name :: user_id(),
               nick_name :: nick_name(),
               user_pic  :: user_pic()
}).
%Channel messages storage for query use channel_timeline bucket
-record(channel_msg, {
    channel_id :: channel_id(),
    sender :: #user{},
    msg_type :: public_msg_type(),
    created :: datetimestamp(),
    text :: nonempty_string()}).
%Private message storage, note in key we should not include channel_id
-record(private_msg, {
    channel_id :: channel_id(),
    sender :: #user{},
    recipient :: #user{},
    msg_type :: private_msg_type(),
    created :: datetimestamp(),
    text :: nonempty_string()}).
%Keep unreaded private messages for user
-record(inbox, {user_id :: user_id(),
    msgs = [] :: [key_string()]}).
%For history query
-record(channel_timeline, {channel_id :: channel_id(),
                   msgs = [] :: [key_string()]}).

-type user() :: #user{}.
-type channel_msg() :: #channel_msg{}.
-type private_msg() :: #private_msg{}.
-type channel_timeline() :: #channel_timeline{}.

-define(CANNEL_MSG_BUCKET, <<"ChannelMsgs">>).
-define(PRIVATE_MSG_BUCKET, <<"PrivateMsgs">>).
-define(CHANNEL_TIMELINE_BUCKET, <<"ChannelTimelines">>).
-define(INBOX, "Inbox").
%Message content types
-define(TEXT, "Text").
-define(DATA, "Data").
-define(INVITATION, "Invitation").
-define(QUERY, "Query").
-define(REPLY, "Reply").
-define(MODUPDATE, "Modupdate").
-define(PEERUPDATE, "Peerupdate").

-export_type([user/0, channel_msg/0, private_msg/0, channel_timeline/0, key_string/0, public_msg_type/0, private_msg_type/0,
			  user_id/0, nick_name/0, datetimestamp/0, text/0,
			  email/0, year/0, month/0, day/0, date/0]).
