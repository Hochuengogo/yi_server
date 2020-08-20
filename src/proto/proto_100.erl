-module(proto_100).
-export([pack/3, unpack/3]).


pack(10000, req, {}) ->
    <<>>;
pack(10000, res, {Val_time_1}) ->
    <<Val_time_1:32>>;

pack(Code, _Flag, _Data) ->
    {error, {error_bad_code, Code}}.


unpack(10000, req, Bin) ->
    <<>> = Bin,
    {};
unpack(10000, res, Bin) ->
    {Val_time_1,Bin_1} = proto_core:unpack_uint32(Bin),
    <<>> = Bin_1,
    {Val_time_1};

unpack(Code, _Flag, _Bin) ->
    {error, {error_bad_code, Code}}.
