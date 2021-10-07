-module(proto_102).
-export([pack/3, unpack/3]).


pack(10200, req, {}) ->
    <<>>;
pack(10200, res, {Val_type_1,Val_code_2,Val_args_3,Val_msg_4}) ->
    <<Val_type_1:8,Val_code_2:32,(length(Val_args_3)):16,(list_to_binary([<<(proto_core:pack_string(Val__args_3))/binary>>||Val__args_3<-Val_args_3]))/binary,(proto_core:pack_string(Val_msg_4))/binary>>;

pack(10201, req, {}) ->
    <<>>;
pack(10201, res, {Val_type_1,Val_code_2,Val_args_3,Val_msg_4}) ->
    <<Val_type_1:8,Val_code_2:32,(length(Val_args_3)):16,(list_to_binary([<<(proto_core:pack_string(Val__args_3))/binary>>||Val__args_3<-Val_args_3]))/binary,(proto_core:pack_string(Val_msg_4))/binary>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10200, req, Bin) ->
    _ = Bin,
    {};
unpack(10200, res, Bin) ->
    {Val_type_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_code_2,Bin_2} = proto_core:unpack_uint32(Bin_1),
    {Val_args_3,Bin_3} = proto_core:unpack_array(Bin_2,fun proto_core:unpack_string/1),
    {Val_msg_4,Bin_4} = proto_core:unpack_string(Bin_3),
    _ = Bin_4,
    {Val_type_1,Val_code_2,Val_args_3,Val_msg_4};

unpack(10201, req, Bin) ->
    _ = Bin,
    {};
unpack(10201, res, Bin) ->
    {Val_type_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_code_2,Bin_2} = proto_core:unpack_uint32(Bin_1),
    {Val_args_3,Bin_3} = proto_core:unpack_array(Bin_2,fun proto_core:unpack_string/1),
    {Val_msg_4,Bin_4} = proto_core:unpack_string(Bin_3),
    _ = Bin_4,
    {Val_type_1,Val_code_2,Val_args_3,Val_msg_4};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
