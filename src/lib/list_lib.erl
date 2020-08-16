%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 列表库
%%% @end
%%% Created : 05. 一月 2020 13:18
%%%-------------------------------------------------------------------
-module(list_lib).
-author("huangzaoyi").

%% API
-export([
    keyfind/4, keyfind/6,
    keystore/6,
    shuffle/1,
    rand/1,
    keyrand/1,
    nrand/2,
    nrand2/2,
    nkeyrand/2,
    page/3,
    desc_sort/1,
    desc_keysort/2,
    add_sort/2,
    add_desc_sort/2,
    keysorts/2,
    set_idx/2,
    set_idx2/1
]).

%% @doc lists:keyfind加一个默认值
-spec keyfind(term(), pos_integer(), list(), term()) -> term().
keyfind(Key, Pos, List, Default) when is_list(List) ->
    case lists:keyfind(Key, Pos, List) of
        false ->
            Default;
        Val ->
            Val
    end.

%% @doc 在二层列表中获取一个值
-spec keyfind(term(), pos_integer(), term(), pos_integer(), list(), term()) -> term().
keyfind(Key, Pos, Key2, Pos2, List, Default) when is_list(List) ->
    ValList = keyfind(Key, Pos, List, []),
    keyfind(Key2, Pos2, ValList, Default).

%% @doc 在二层列表中保存一个值
-spec keystore(term(), pos_integer(), term(), pos_integer(), list(), term()) -> list().
keystore(Key, Pos, Key2, Pos2, List, Val) when is_list(List) ->
    ValList = keyfind(Key, Pos, List, []),
    NewValList = lists:keystore(Key2, Pos2, ValList, Val),
    lists:keystore(Key, Pos, List, NewValList).

%% @doc 打乱列表中的元素
-spec shuffle(list()) -> list().
shuffle(List = [_]) ->
    List;
shuffle(List = [_ | _]) ->
    Len = length(List),
    List1 = [{rand:uniform(Len * 10), X} || X <- List],
    List2 = lists:keysort(1, List1),
    [E || {_, E} <- List2].

%% @doc 随机取出列表中的一项元素
-spec rand(list()) -> term().
rand([I]) -> I;
rand(List = [_ | _]) ->
    N = rand:uniform(length(List)),
    lists:nth(N, List).

%% @doc 按概率取出列表中的一项元素
-spec keyrand(list()) -> term().
keyrand([{I, _N}]) -> I;
keyrand(List = [_ | _]) ->
    Num = lists:sum([N || {_I, N} <- List]),
    Top = rand:uniform(Num),
    keyrand(List, Top, 0).
keyrand([{I, _R}], _Top, _N) ->
    I;
keyrand([{I, R} | _List], Top, N) when R + N >= Top ->
    I;
keyrand([{_I, R} | List], Top, N) ->
    keyrand(List, Top, N + R).

%% @doc 多次随机取出列表中的一项元素
-spec nrand(list(), pos_integer()) -> list().
nrand(List, N) when is_integer(N), N > 0 ->
    [rand(List) || _ <- lists:seq(1, N)].

%% @doc 多次随机不放回取出列表中的一项元素
-spec nrand2(list(), pos_integer()) -> list().
nrand2(List, N) ->
    nrand2(List, N, []).
nrand2(_List, 0, Acc) ->
    Acc;
nrand2(List, N, Acc) when is_integer(N), N > 0 ->
    I = rand(List),
    NewList = lists:delete(I, List),
    nrand2(NewList, N - 1, [I | Acc]).

%% @doc 多次按概率取出列表中的一项元素
-spec nkeyrand(list(), pos_integer()) -> list().
nkeyrand(List, N) when is_integer(N), N > 0 ->
    [keyrand(List) || _ <- lists:seq(1, N)].

%% @doc 分页获取数据
-spec page(list(), pos_integer(), pos_integer()) -> {list(), pos_integer()}.
page(List, Pos, Len) ->
    NewList = lists:sublist(List, Pos, Len),
    Total = length(NewList),
    {NewList, Pos + Total}.

%% @doc 降序排序
-spec desc_sort(list()) -> list().
desc_sort(List) ->
    lists:reverse(lists:sort(List)).

%% @doc 按键降序排序
-spec desc_keysort(pos_integer(), list()) -> list().
desc_keysort(Pos, List) ->
    lists:reverse(lists:keysort(Pos, List)).

%% 3
%% 1 2 3 4 5
%% @doc 在有序的列表中添加一项
-spec add_sort(term(), list()) -> list().
add_sort(I, []) ->
    [I];
add_sort(I, List) when is_list(List) ->
    add_sort(I, List, []).
add_sort(I, [], Acc) ->
    lists:reverse(Acc) ++ [I];
add_sort(I, [H | List], Acc) when I < H ->
    lists:reverse(Acc) ++ [I, H] ++ List;
add_sort(I, [H | List], Acc) ->
    add_sort(I, List, [H | Acc]).

%% 3
%% 5 4 3 2 1
%% @doc 在有序的降序列表中添加一项
-spec add_desc_sort(term(), list()) -> list().
add_desc_sort(I, []) ->
    [I];
add_desc_sort(I, List) when is_list(List) ->
    add_desc_sort(I, List, []).
add_desc_sort(I, [], Acc) ->
    lists:reverse(Acc) ++ [I];
add_desc_sort(I, [H | List], Acc) when I > H ->
    lists:reverse(Acc) ++ [I, H] ++ List;
add_desc_sort(I, [H | List], Acc) ->
    add_desc_sort(I, List, [H | Acc]).

%% @doc 按多个键排序一个列表
-spec keysorts(list(), list()) -> list().
keysorts(PosList, List) ->
    lists:sort(fun(I1, I2) -> compare(I1, I2, PosList) end, List).

%% 按照pos比较大小
compare(_I1, _I2, []) ->
    true;
compare(I1, I2, [{desc, Pos} | List]) ->
    E1 = erlang:element(Pos, I1),
    E2 = erlang:element(Pos, I2),
    if
        E1 > E2 ->
            true;
        E1 < E2 ->
            false;
        true ->
            compare(I1, I2, List)
    end;
compare(I1, I2, [Pos | List]) ->
    E1 = erlang:element(Pos, I1),
    E2 = erlang:element(Pos, I2),
    if
        E1 > E2 ->
            false;
        E1 < E2 ->
            true;
        true ->
            compare(I1, I2, List)
    end.

%% @doc 设置索引
-spec set_idx(list(), pos_integer()) -> list().
set_idx(List, Pos) ->
    set_idx(List, Pos, 1, []).
set_idx([], _Pos, _Idx, Acc) ->
    lists:reverse(Acc);
set_idx([I | List], Pos, Idx, Acc) ->
    NewI = erlang:setelement(Pos, I, Idx),
    set_idx(List, Pos, Idx + 1, [NewI | Acc]).

%% @doc 设置索引
-spec set_idx2(list()) -> list().
set_idx2(List) ->
    set_idx2(List, 1, []).
set_idx2([], _Idx, Acc) ->
    lists:reverse(Acc);
set_idx2([I | List], Idx, Acc) ->
    NewI = erlang:append_element(I, Idx),
    set_idx2(List, Idx + 1, [NewI | Acc]).

