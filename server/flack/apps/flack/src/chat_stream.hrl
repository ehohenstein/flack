
-record(user_joined, {
    chat :: binary(),
    username :: binary(),
    user_id :: binary(),
    timestamp :: binary(),
    sequence :: integer()
}).

-record(chat_message, {
    chat_name :: binary(),
    user_id :: undefined | binary(),
    mime_type :: binary(),
    message :: binary(),
    timestamp :: undefined | binary(),
    sequence :: undefined | integer()
}).

-record(user_left, {
    chat :: binary(),
    user_id :: binary(),
    timestamp :: binary(),
    sequence :: integer()
}).

