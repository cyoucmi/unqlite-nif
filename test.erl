-module(test).
-export([test/0,test1/0]).


test()->
     unqlite:load(),
     unqlite:init('/root/unqlite_test.db').
	 
test1()->
	unqlite:get(<<"aa">>).


