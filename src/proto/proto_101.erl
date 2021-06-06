-module(proto_101).
-export([pack/3, unpack/3]).


pack(10100, req, {#{id:=Val_id_1,name:=Val_name_2},Val_list_6}) ->
    <<Val_id_1:32,(proto_core:pack_string(Val_name_2))/binary,(length(Val_list_6)):16,(list_to_binary([<<Val_id_4:32,(proto_core:pack_string(Val_name_5))/binary>>||{Val_id_4,Val_name_5}<-Val_list_6]))/binary>>;
pack(10100, res, {}) ->
    <<>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10100, req, Bin) ->
    {Val_id_1,Bin_1} = proto_core:unpack_uint32(Bin),
    {Val_name_2,Bin_2} = proto_core:unpack_string(Bin_1),
    {Val_list_6,Bin_6} = proto_core:unpack_array(Bin_2,
    fun(Bin_3) ->
        {Val_id_4,Bin_4} = proto_core:unpack_uint32(Bin_3),
        {Val_name_5,Bin_5} = proto_core:unpack_string(Bin_4),
        {{Val_id_4,Val_name_5},Bin_5}
    end),
    _ = Bin_6,
    {#{id=>Val_id_1,name=>Val_name_2},Val_list_6};
unpack(10100, res, Bin) ->
    _ = Bin,
    {};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
