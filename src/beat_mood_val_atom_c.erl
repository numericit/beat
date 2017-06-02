-module(beat_mood_val_atom_c).
-export([handle/3]).

-behaviour(beat_mood).

-include("beat.hrl").

handle(_Path, ?V(Line, string, [Char]), State) ->
    {{char, Line, Char}, State};
handle(_Path, _Ast, _State) ->
    next.
