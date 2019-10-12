-module(faster_xml).

-include("crates.hrl").

-export([
    parse/1,
    parse_file/1
]).

-on_load(init/0).

init() ->
    erlang:load_nif(?crate_faster_xml_nif, 0).


parse(Bin) ->
    parse(self(), Bin).

parse(_Pid, _Bin) ->
    erlang:nif_error(nif_not_loaded).

parse_file(FName) ->
    {ok, Bin} = file:read_file(FName),
    parse(Bin).