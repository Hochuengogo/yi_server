-module(proto_100).
-export([pack/3, unpack/3]).


pack(10000, req, Val_data_3) ->
    <<(length(Val_data_3)):16,(list_to_binary([<<(proto_core:pack_string(Val_key_1))/binary,(proto_core:pack_string(Val_val_2))/binary>>||{Val_key_1,Val_val_2}<-Val_data_3]))/binary>>;
pack(10000, res, {Val_code_1,Val_msg_2,Val_role_info_9}) ->
    <<Val_code_1:8,(proto_core:pack_string(Val_msg_2))/binary,(length(Val_role_info_9)):16,(list_to_binary([<<Val_id_3:32,(proto_core:pack_string(Val_srv_id_4))/binary,(proto_core:pack_string(Val_name_5))/binary,Val_lev_6:16,Val_sex_7:8,Val_career_8:8>>||{Val_id_3,Val_srv_id_4,Val_name_5,Val_lev_6,Val_sex_7,Val_career_8}<-Val_role_info_9]))/binary>>;

pack(10001, req, {}) ->
    <<>>;
pack(10001, res, {Val_timestamp_1}) ->
    <<Val_timestamp_1:32>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10000, req, Bin) ->
    {Val_data_3,Bin_4} = proto_core:unpack_array(Bin,
    fun(Bin_1) ->
        {Val_key_1,Bin_2} = proto_core:unpack_string(Bin_1),
        {Val_val_2,Bin_3} = proto_core:unpack_string(Bin_2),
        {{Val_key_1,Val_val_2},Bin_3}
    end),
    _ = Bin_4,
    Val_data_3;
unpack(10000, res, Bin) ->
    {Val_code_1,Bin_1} = proto_core:unpack_uint8(Bin),
    {Val_msg_2,Bin_2} = proto_core:unpack_string(Bin_1),
    {Val_role_info_9,Bin_10} = proto_core:unpack_array(Bin_2,
    fun(Bin_3) ->
        {Val_id_3,Bin_4} = proto_core:unpack_uint32(Bin_3),
        {Val_srv_id_4,Bin_5} = proto_core:unpack_string(Bin_4),
        {Val_name_5,Bin_6} = proto_core:unpack_string(Bin_5),
        {Val_lev_6,Bin_7} = proto_core:unpack_uint16(Bin_6),
        {Val_sex_7,Bin_8} = proto_core:unpack_uint8(Bin_7),
        {Val_career_8,Bin_9} = proto_core:unpack_uint8(Bin_8),
        {{Val_id_3,Val_srv_id_4,Val_name_5,Val_lev_6,Val_sex_7,Val_career_8},Bin_9}
    end),
    _ = Bin_10,
    {Val_code_1,Val_msg_2,Val_role_info_9};

unpack(10001, req, Bin) ->
    _ = Bin,
    {};
unpack(10001, res, Bin) ->
    {Val_timestamp_1,Bin_1} = proto_core:unpack_uint32(Bin),
    _ = Bin_1,
    {Val_timestamp_1};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
