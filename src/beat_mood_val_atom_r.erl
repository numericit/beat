-module(beat_mood_val_atom_r).
-export([handle/3]).

-behaviour(beat_mood).

-include("beat.hrl").

% #r.<atom> <atom>
handle([?Atom(r), ?Atom(RecordName)], ?Atom(Line, Field), State) ->
    R = {record_index, Line, RecordName, {atom, Line, Field}},
    {R, State};
% #r.<atom>.<atom> <var>
handle([?Atom(r), ?Atom(RecordName), ?Atom(Field)], ?Var(Line, RecordVar), State) ->
    R = {record_field, Line, {var, Line, RecordVar}, RecordName, {atom, Line, Field}},
    {R, State};
% #r.<atom> <var>#<map>
handle([?Atom(r), ?Atom(RecordName)], ?S(Line, map, {Var, KVs}), State) ->
    {EVar, State1} = beat_to_erl:ast_to_ast(Var, State),
    {Items, State2} = beat_to_erl:state_map(fun to_record_field/2, KVs, State1),
    R = {record, Line, EVar, RecordName, Items},
    {R, State2};
% #r.<atom> <map>
handle([?Atom(r), ?Atom(RecordName)], ?S(Line, map, KVs), State) ->
    {Items, State1} = beat_to_erl:state_map(fun to_record_field/2, KVs, State),
    R = {record, Line, RecordName, Items},
    {R, State1};
handle(_Path, _Ast, _State) ->
    next.

to_record_field({kv, Line, Key, Val}, State) ->
    {{EKey, EVal}, State1} = beat_to_erl:kv_to_ast(Key, Val, State),
    R = {record_field, Line, EKey, EVal},
    {R, State1};
to_record_field(Other, State) ->
    Line = element(2, Other),
    State1 = beat_to_erl:add_error(State, bad_record_field_init, Line,
                       beat_to_erl:expected_got("initialization", {ast, Other})),
    {{atom, Line, error}, State1}.

