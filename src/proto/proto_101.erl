-module(proto_101).
-export([pack/3, unpack/3]).


pack(10100, req, #{test_map2:=#{name:=Val_name_4}=Val_test_map2_5}=Val_test_map_6) ->
    <<(maps:get(id,Val_test_map_6,0)):32,(proto_core:pack_string((maps:get(name,Val_test_map_6,<<>>))))/binary,(maps:get(id,Val_test_map2_5,0)):32,(proto_core:pack_string(Val_name_4))/binary>>;
pack(10100, res, {}) ->
    <<>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10100, req, Bin) ->
    {Val_id_1,Bin_1} = proto_core:unpack_uint32(Bin),
    {Val_name_2,Bin_2} = proto_core:unpack_string(Bin_1),
    {Val_id_3,Bin_3} = proto_core:unpack_uint32(Bin_2),
    {Val_name_4,Bin_4} = proto_core:unpack_string(Bin_3),
    _ = Bin_4,
    #{id=>Val_id_1,name=>Val_name_2,test_map2=>#{id=>Val_id_3,name=>Val_name_4}};
unpack(10100, res, Bin) ->
    _ = Bin,
    {};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
