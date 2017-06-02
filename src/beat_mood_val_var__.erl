-module(beat_mood_val_var__).
-export([handle/3]).

-behaviour(beat_mood).

handle(_Path, _Ast, State) ->
    {'beat compiler ignore', State}.
