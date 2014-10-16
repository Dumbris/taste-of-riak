-type key_string() :: nonempty_string().
-type msg_type() :: 'inbox' | 'sent'.
-type content_type() :: 'private' | 'invitation' | 'add_query' | 'reply_query'.
-type user_id() :: nonempty_string().
-type nick_name() :: nonempty_string().
-type datetimestamp() :: nonempty_string().
-type text() :: nonempty_string().
-type email() :: nonempty_string().
-type year() :: non_neg_integer().
-type month() :: non_neg_integer().
-type day() :: non_neg_integer().
-type hour() :: non_neg_integer().
-type min() :: non_neg_integer().
-type sec() :: non_neg_integer().
-type ms() :: non_neg_integer().
-type date() :: {year(), month(), day()}.

-record(user, {user_name :: user_id(),
               full_name :: nick_name(),
               email :: email()}).

-record(msg, {sender :: user_id(),
              recipient :: user_id(),
              content_type :: content_type(),
              created :: datetimestamp(), 
              text :: nonempty_string()}).

-record(timeline, {owner :: user_id(),
                   msg_type :: msg_type(), 
                   msgs = [] :: [key_string()]}).

-type user() :: #user{}.
-type msg() :: #msg{}.
-type timeline() :: #timeline{}.

-define(USER_BUCKET, <<"Users">>).
-define(MSG_BUCKET, <<"Msgs">>).
-define(TIMELINE_BUCKET, <<"Timelines">>).
-define(INBOX, "Inbox").
-define(SENT, "Sent").

-export_type([user/0, msg/0, timeline/0, key_string/0, msg_type/0,
			  user_id/0, nick_name/0, datetimestamp/0, text/0,
			  email/0, year/0, month/0, day/0, date/0]).
