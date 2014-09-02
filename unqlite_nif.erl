-module(unqlite_nif).
-export([load/0,init/1,set/2,get/1, del/1, commit/0, rollback/0, init_cursor/0, cursor_first_entry/1,cursor_last_entry/1,cursor_valid_entry/1,cursor_prev_entry/1,cursor_next_entry/1,cursor_entry/1]).

load()->
	erlang:load_nif("./unqlite_nif", 0).

init(_)->
	"unqlite_nif library not loaded".

set(_,_)->
	"unqlite_nif library not loaded".


get(_)->
	"unqlite_nif library not loaded".

del(_)->
	"unqlite_nif library not loaded".

commit()->
	"unqlite_nif library not loaded".


rollback()->
	"unqlite_nif library not loaded".


init_cursor()->
	"unqlite_nif library not loaded".

cursor_first_entry(_)->
	"unqlite_nif library not loaded".

cursor_last_entry(_)->
	"unqlite_nif library not loaded".


cursor_valid_entry(_)->
	"unqlite_nif library not loaded".

cursor_prev_entry(_)->
	"unqlite_nif library not loaded".


cursor_next_entry(_)->
	"unqlite_nif library not loaded".

cursor_entry(_)->
	"unqlite_nif library not loaded".


