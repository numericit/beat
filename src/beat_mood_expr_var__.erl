-module(beat_mood_expr_var__).
-export([handle/3]).

-behaviour(beat_mood).

handle(_Path, _Ast, State) ->
    {'beat compiler ignore', State}.
