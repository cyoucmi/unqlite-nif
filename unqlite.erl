-module(unqlite).
-export([load/0,init/1,set/2,get/1, del/1, commit/0, rollback/0, init_cursor/0, cursor_first_entry/1,cursor_last_entry/1,cursor_valid_entry/1,cursor_prev_entry/1,cursor_next_entry/1,cursor_entry/1]).

load()->
	unqlite_nif:load().

init(UnqliteDbFile) when is_atom(UnqliteDbFile)->
	unqlite_nif:init(UnqliteDbFile).


set(KeyBin, ValueBin) when is_binary(KeyBin), is_binary(ValueBin)->
    unqlite_nif:set(KeyBin, ValueBin).

get(KeyBin) when is_binary(KeyBin)->
    unqlite_nif:get(KeyBin).

del(KeyBin) when is_binary(KeyBin)->
    unqlite_nif:del(KeyBin).

commit()->
    unqlite_nif:commit().

rollback()->
    unqlite_nif:rollback().



init_cursor()->
    unqlite_nif:init_cursor().

cursor_first_entry(Cursor)->
    unqlite_nif:cursor_first_entry(Cursor).

cursor_last_entry(Cursor)->
    unqlite_nif:cursor_last_entry(Cursor).


cursor_valid_entry(Cursor)->
    unqlite_nif:cursor_valid_entry(Cursor).

cursor_prev_entry(Cursor)->
    unqlite_nif:cursor_prev_entry(Cursor).


cursor_next_entry(Cursor)->
    unqlite_nif:cursor_next_entry(Cursor).

cursor_entry(Cursor)->
    unqlite_nif:cursor_entry(Cursor).

