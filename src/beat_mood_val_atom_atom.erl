-module(beat_mood_val_atom_atom).
-export([handle/3]).

-behaviour(beat_mood).

-include("beat.hrl").

handle(_Path, ?V(Line, string, AtomStr), State) ->
    {{atom, Line, list_to_atom(AtomStr)}, State};
handle(_Path, _Ast, _State) ->
    next.
