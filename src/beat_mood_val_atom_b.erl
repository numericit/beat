-module(beat_mood_val_atom_b).
-export([handle/3]).

-behaviour(beat_mood).

-include("beat.hrl").

handle(_Path, ?S(Line, map, TSList), State) ->
    type_specifiers_to_ast(Line, TSList, State);
handle(_Path, _Ast, _State) ->
    next.

type_specifiers_to_ast(Line, TSList, State) ->
    {RFields, State1} = lists:mapfoldl(fun to_bin_element/2, State, TSList),
    R = {bin, Line, RFields},
    {R, State1}.

parse_bin_key_element(?Var(Line, Name)) -> {ok, {var, Line, Name}};
parse_bin_key_element(?Int(Line, Value)) -> {ok, {integer, Line, Value}};
parse_bin_key_element({val, Line, bstring, Val}) ->
    {ok, {bin, Line, [{bin_element, Line, {string, Line, Val}, default, default}]}};
parse_bin_key_element(Other) -> {error, {invalid_bin_key, Other}}.

% if value is the _ var assume defaults
to_bin_element({kv, Line, Key, ?Var(VLine, '_')}, State) ->
    to_bin_element({kv, Line, Key, ?S(VLine, map, [])}, State);
% if value is an atom assume it's the type
to_bin_element({kv, Line, Key, ?Atom(ALine, AName)}, State) ->
    to_bin_element({kv, Line, Key, ?S(ALine, map,
                                       [{kv, ALine, ?Atom(ALine, type),
                                         ?Atom(ALine, AName)}])}, State);
% if value is an integer assume it's the size
to_bin_element({kv, Line, Key, Size=?Int(SLine, _SVal)}, State) ->
    to_bin_element({kv, Line, Key, ?S(SLine, map,
                                       [{kv, SLine, ?Atom(SLine, size),
                                         Size}])}, State);
to_bin_element({kv, Line, Key, ?S(_MapLine, map, Fields)}, State) ->
    case parse_bin_key_element(Key) of
        {ok, EKey} ->
            InitialState =  {bin_element, Line, EKey, default, default},
            parse_bin_element_fields(Line, Fields, State, InitialState);
        {error, _Reason} ->
            State1 = beat_to_erl:add_error(State, invalid_bin_type_specifier_field, Line,
                                         beat_to_erl:expected_got("binary string, var or integer", Key)),
            {{atom, Line, error}, State1}
    end;

to_bin_element(Other, State) ->
    Line = element(2, Other),
    State1 = beat_to_erl:add_error(State, invalid_bin_type_specifier_value, Line,
                       beat_to_erl:expected_got("_, integer or map", Other)),
    {{atom, Line, error}, State1}.

bin_element_size(Line, NewSize, T, State, {BeType, BeLine, BeName, _OldSize, Params}) ->
    {ENewSize, State1} = beat_to_erl:ast_to_ast(NewSize, State),
    NewBinElement = {BeType, BeLine, BeName, ENewSize, Params},
    parse_bin_element_fields(Line, T, State1, NewBinElement).

parse_bin_element_fields(_Line, [], State, BinElement) ->
    {BinElement, State};

parse_bin_element_fields(Line, [{kv, _Line, ?Atom(size), ?Var(_)=NewSize}|T],
                         State, BinElement) ->
    bin_element_size(Line, NewSize, T, State, BinElement);
parse_bin_element_fields(Line, [{kv, _Line, ?Atom(size), ?V(_, integer, _Size)=NewSize}|T],
                         State, BinElement) ->
    bin_element_size(Line, NewSize, T, State, BinElement);

parse_bin_element_fields(Line, [{kv, _Line, ?Atom(unit), ?V(_, integer, Unit)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params})
                        when Unit >= 1 andalso Unit =< 256 ->
    {NewParams, State1} = add_bin_element_param(Params, {unit, Unit}, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(type), ?Atom(Type)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [integer, float, binary, bytes, bitstring, bits, utf8, utf16, utf32],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Type, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(endianness), ?Atom(Endianness)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [big, little, native],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Endianness, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(sign), ?Atom(Sign)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [signed, unsigned],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Sign, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [Other|T], State, BinElement) ->
    Msg = "one of val (var), size (integer), type (atom), sign (atom), endianness (atom), unit (1..256)",
    OtherLine = element(2, Other),
    State1 = beat_to_erl:add_error(State, invalid_bin_type_specifier_field, OtherLine,
                       beat_to_erl:expected_got(Msg, {ast, Other})),
    parse_bin_element_fields(Line, T, State1, BinElement).

add_bin_element_param(default, Param, State) ->
    add_bin_element_param([], Param, State);
add_bin_element_param(L, Param, State) ->
    {[Param|L], State}.

add_bin_element_param(Line, Params, Param, ValidValues, State) ->
    IsInValues = lists:member(Param, ValidValues),
    if IsInValues -> add_bin_element_param(Params, Param, State);
       true ->
           Msg = io_lib:format("one of ~p", [ValidValues]),
           State1 = beat_to_erl:add_error(State, invalid_bin_type_specifier_value, Line,
                              beat_to_erl:expected_got(Msg, Param)),
           {Params, State1}
    end.


