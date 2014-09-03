-module(test).
-compile([export_all]).

-define(N, 1000000).

-record(ets_data, {key, value}).


init()->
     unqlite:load(),
     unqlite:init('/root/unqlite_test.db').
	 

get_string(Length) ->
    lists:foldl(fun(_, Acc) ->
			[random:uniform(255) | Acc]
                end, [], lists:seq(1, Length)).



make_ets_data()->
    ets:new(ets_data, [named_table, set, {keypos, #ets_data.key}, public]),
    insert_ets_data(?N).

insert_ets_data(0)->
    ok;
insert_ets_data(N)->
    Key = get_string(10),
    Value = get_string(100),
    ets:insert(ets_data, #ets_data{
            key = binary:list_to_bin(Key),
            value = binary:list_to_bin(Value)}),
    insert_ets_data(N-1).

insert_to_unqlite()->
    insert_to_unqlite1(ets:first(ets_data), ?N).

insert_to_unqlite1('$end_of_table', _)->
    ok;
insert_to_unqlite1(KeyBin, N)->
    [#ets_data{value = ValueBin}] = ets:lookup(ets_data, KeyBin),
    unqlite:set(KeyBin, ValueBin),
    if 
        N rem 20000 =:= 0 ->
            unqlite:commit();
        true->
            ok
    end,
    insert_to_unqlite1(ets:next(ets_data, KeyBin), N-1).


check_data_equal()->
    DbCursor = unqlite:init_cursor(),
    unqlite:cursor_first_entry(DbCursor),
    check_data_equal1(DbCursor).

check_data_equal1(DbCursor)->
    case unqlite:cursor_valid_entry(DbCursor) of
        true->
            {DbKeyBin, DbValuBin} = unqlite:cursor_entry(DbCursor),
            [#ets_data{value = DbValuBin}] = ets:lookup(ets_data, DbKeyBin),
            ets:delete(ets_data, DbKeyBin),
            unqlite:cursor_next_entry(DbCursor),
            check_data_equal1(DbCursor);
        false->
            io:format("~p~n", [ets:tab2list(ets_data)])
        end.

%% timer:tc(fun()->test:make_ets_data()end). 
%% timer:tc(fun()->test:insert_to_unqlite()end). 
%%  timer:tc(fun()->test:check_data_equal()end). 
%%







