-module(piperl_util).
-export([decode_ubf_stream/1,
         decode_ubf_stream/2
        ]).

%% assumes F returns a binary
-type stream() :: fun(() -> binary()).
-type ubf() :: any().
-type cont() :: list().

-spec decode_ubf_stream(stream()) -> {ubf(),cont()}.
decode_ubf_stream(F) when is_function(F) ->
  Str = get_chunk(F),
  decode_ubf_stream(F,Str,false).

-spec decode_ubf_stream(stream(),cont()) -> {ubf(),cont()}.
decode_ubf_stream(F,[]) when is_function(F) ->
  Str = get_chunk(F),
  decode_ubf_stream(F,Str,false);
decode_ubf_stream(F,Str) when is_function(F), is_list(Str) ->
  decode_ubf_stream(F,Str,false).
decode_ubf_stream(F,Str,UbfCont) ->
  R = if UbfCont == false -> ubf:decode(Str);
         true -> ubf:decode(Str,UbfCont)
      end,
  case R of
    {done,Data,Excess} -> {Data,Excess};
    UbfCont2-> decode_ubf_stream(F,get_chunk(F),UbfCont2)
  end.

get_chunk(F) ->
  binary_to_list(F()).
