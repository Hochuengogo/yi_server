-module(proto_101).
-export([pack/3, unpack/3]).


pack(10100, req, Val_data_3) ->
    <<(length(Val_data_3)):16,(list_to_binary([<<(proto_core:pack_string(Val_key_1))/binary,(proto_core:pack_string(Val_val_2))/binary>>||{Val_key_1,Val_val_2}<-Val_data_3]))/binary>>;
pack(10100, res, {Val_code_1,Val_id_2,Val_srv_id_3}) ->
    <<Val_code_1:8,Val_id_2:32,(proto_core:pack_string(Val_srv_id_3))/binary>>;

pack(10101, req, {Val_id_1,Val_srv_id_2}) ->
    <<Val_id_1:32,(proto_core:pack_string(Val_srv_id_2))/binary>>;
pack(10101, res, {Val_code_1,Val_id_2,Val_srv_id_3}) ->
    <<Val_code_1:8,Val_id_2:32,(proto_core:pack_string(Val_srv_id_3))/binary>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10100, req, Bin) ->
    {Val_data_3,Bin_4} = proto_core:unpack_array(Bin,
    fun(Bin_1) ->
        {Val_key_1,Bin_2} = proto_core:unpack_string(Bin_1),
        {Val_val_2,Bin_3} = proto_core:unpack_string(Bin_2),
        {{Val_key_1,Val_val_2},Bin_3}
    end),
    _ = Bin_4,
    Val_data_3;
unpack(10100, res, Bin) ->
    {Val_code_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_id_2,Bin_2} = proto_core:unpack_uint32(Bin_1),
    {Val_srv_id_3,Bin_3} = proto_core:unpack_string(Bin_2),
    _ = Bin_3,
    {Val_code_1,Val_id_2,Val_srv_id_3};

unpack(10101, req, Bin) ->
    {Val_id_1,Bin_1} = proto_core:unpack_uint32(Bin),
    {Val_srv_id_2,Bin_2} = proto_core:unpack_string(Bin_1),
    _ = Bin_2,
    {Val_id_1,Val_srv_id_2};
unpack(10101, res, Bin) ->
    {Val_code_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_id_2,Bin_2} = proto_core:unpack_uint32(Bin_1),
    {Val_srv_id_3,Bin_3} = proto_core:unpack_string(Bin_2),
    _ = Bin_3,
    {Val_code_1,Val_id_2,Val_srv_id_3};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
