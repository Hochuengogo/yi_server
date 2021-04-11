%%%-------------------------------------------------------------------
%%% @author huangzaoyi
%%% @copyright (C) 2021, <COMPANY>
%%% @doc 列表库
%%%
%%% @end
%%% Created : 07. 3月 2021 7:33 下午
%%%-------------------------------------------------------------------
-module(list_util).
-author("huangzaoyi").

%% API
-export([
    keyfind/4, keyfind/6,
    keystore/6,
    shuffle/1,
    rand/1,
    rand_more/2,
    rand_more2/2,
    keyrand/2,
    keyrand_more/3,
    keyrand_more2/3,
    page/3,
    sort/2,
    keysort/3,
    keysorts/2,
    add_sort/3,
    add_keysort/3,
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
shuffle(List) ->
    Len = length(List),
    NewList = [{util:rand(Len * 100), X} || X <- List],
    [E || {_, E} <- lists:keysort(1, NewList)].

%% @doc 随机取出列表中的一项元素
-spec rand(list()) -> term().
rand([I]) -> I;
rand(List = [_ | _]) ->
    N = util:rand(length(List)),
    lists:nth(N, List).

%% @doc 随机放回取出多项
-spec rand_more(list(), pos_integer()) -> list().
rand_more(List = [_ | _], Num) ->
    [rand(List) || _ <- lists:seq(1, Num)].

%% @doc 随机不放回取出多项 如果num大于列表的长度，则会返回列表长度项数据
-spec rand_more2(list(), pos_integer()) -> list().
rand_more2(List = [_ | _], Num) ->
    do_rand_more2(List, Num, []).
do_rand_more2([], _Num, Acc) ->
    Acc;
do_rand_more2(_List, Num, Acc) when Num =< 0 ->
    Acc;
do_rand_more2(List, Num, Acc) ->
    I = rand(List),
    do_rand_more2(lists:delete(I, List), Num - 1, [I | Acc]).

%% @doc 按概率取出列表中的一项元素
-spec keyrand(pos_integer(), list()) -> term().
keyrand(_Pos, [I]) ->
    I;
keyrand(Pos, List = [_ | _]) ->
    Total = lists:sum([element(Pos, I) || I <- List]),
    Top = util:rand(Total),
    do_keyrand(Pos, List, Top, 0).
do_keyrand(_Pos, [I], _Top, _Sum) ->
    I;
do_keyrand(Pos, [I | List], Top, Sum) ->
    Pro = element(Pos, I),
    NewSum = Pro + Sum,
    case Top =< NewSum of
        true ->
            I;
        _ ->
            do_keyrand(Pos, List, Top, NewSum)
    end.

%% @doc 按概率放回取出列表中多项
-spec keyrand_more(pos_integer(), list(), pos_integer()) -> list().
keyrand_more(Pos, List = [_ | _], Num) ->
    [keyrand(Pos, List) || _ <- lists:seq(1, Num)].

%% @doc 按概率不放回取出列表中多项 如果num大于列表的长度，则会返回列表长度项数据
-spec keyrand_more2(pos_integer(), list(), pos_integer()) -> list().
keyrand_more2(Pos, List = [_ | _], Num) ->
    do_keyrand_more2(Pos, List, Num, []).
do_keyrand_more2(_Pos, [], _Num, Acc) ->
    Acc;
do_keyrand_more2(_Pos, _List, Num, Acc) when Num =< 0 ->
    Acc;
do_keyrand_more2(Pos, List, Num, Acc) ->
    I = keyrand(Pos, List),
    do_keyrand_more2(Pos, lists:delete(I, List), Num - 1, [I | Acc]).

%% @doc 分页获取数据
-spec page(list(), pos_integer(), pos_integer()) -> {list(), pos_integer()}.
page(List, Start, Len) ->
    SubList = lists:sublist(List, Start, Len),
    Total = length(SubList),
    {SubList, Start + Total}.

%% @doc 排序
-spec sort(list(), asc | desc) -> list().
sort(List, asc) ->
    lists:sort(List);
sort(List, desc) ->
    lists:reverse(lists:sort(List)).

%% @doc 按键排序
-spec keysort(pos_integer(), list(), asc | desc) -> list().
keysort(Pos, List, asc) ->
    lists:keysort(Pos, List);
keysort(Pos, List, desc) ->
    lists:reverse(lists:keysort(Pos, List)).

%% @doc 在有序的列表中添加一项
-spec add_sort(term(), list(), asc | desc) -> list().
add_sort(I, [], _Flag) ->
    [I];
add_sort(I, List, Flag) when is_list(List) andalso (Flag == asc orelse Flag == desc) ->
    do_add_sort(I, List, Flag, []).
do_add_sort(I, [], _Flag, Acc) ->
    lists:reverse([I | Acc]);
do_add_sort(I, [H | List], Flag, Acc) when I < H andalso Flag == asc ->
    lists:reverse([H, I | Acc]) ++ List;
do_add_sort(I, [H | List], Flag, Acc) when I > H andalso Flag == desc ->
    lists:reverse([H, I | Acc]) ++ List;
do_add_sort(I, [H | List], Flag, Acc) ->
    do_add_sort(I, List, Flag, [H | Acc]).

%% @doc 在有序的列表中添加一项
-spec add_keysort(term(), list(), list()) -> list().
add_keysort(I, [], _PosList) ->
    I;
add_keysort(I, List, PosList) ->
    do_add_keysort(I, List, PosList, []).
do_add_keysort(I, [], _PosList, Acc) ->
    lists:reverse([I | Acc]);
do_add_keysort(I, [H | List], PosList, Acc) ->
    case compare(I, H, PosList) of
        true ->
            lists:reverse([H, I | Acc]) ++ List;
        _ ->
            do_add_sort(I, List, PosList, [H | Acc])
    end.

%% @doc 按多个键排序一个列表
-spec keysorts([{asc, pos_integer()} | {desc, pos_integer()}], list()) -> list().
keysorts(PosList, List) ->
    lists:sort(fun(I1, I2) -> compare(I1, I2, PosList) end, List).

%% 按照pos比较大小
compare(_I1, _I2, []) ->
    false;
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
compare(I1, I2, [{asc, Pos} | List]) ->
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

