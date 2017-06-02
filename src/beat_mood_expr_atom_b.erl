-module(beat_mood_expr_atom_b).
-export([handle/3]).

-behaviour(beat_mood).

-include("beat.hrl").

% binary comprehension
handle(_Path, ?E(Line, 'for', {Qualifiers, Body}), State) ->
    {Items, EBody, State1} = beat_to_erl:lc_to_ast(Line, Qualifiers, Body, State),
    R = {bc, Line, EBody, Items},
    {R, State1};
handle(_Path, _, _State) ->
    next.

