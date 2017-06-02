-module(beat_erl_macro).
-export([macro_defs/1, expand_macro/2, expand_macro/3, parse_to_include/1,
        call_macro/3]).

macro_defs(Path) ->
    {ok, _Tokens, Macros} = aleppo:process_file(Path, [{return_macros, true}]),
    {ok, Macros}.

parse_to_include(Path) ->
    {ok, _Tokens, Macros} = aleppo:process_file(Path, [{return_macros, true}]),
    {ok, Ast} = epp:parse_file(Path, []),
    {ok, remove_eof(Ast, []), Macros}.

expand_macro(Macros, Name) ->
    expand_macro(Macros, Name, #{}).

get_macro(Macros, Name) ->
    case dict:find(Name, Macros) of
        {ok, Macro} ->
            {MacroArgs, Tokens} = case Macro of
                                      {_MArgs, _MTokens} = R -> R;
                                      MTokens when is_list(MTokens) -> {[], MTokens}
                                  end,
            {ok, MacroArgs, Tokens};
        error -> {error, {macro_not_found, Name}}
    end.

call_macro(Macros, MacroName, []) ->
    expand_macro(Macros, MacroName, #{});
call_macro(Macros, MacroName, Args) ->
    MacroArity = length(Args),
    MacroKey = {MacroName, MacroArity},
    case get_macro(Macros, MacroKey) of
        {ok, MacroArgVarNames, _Tokens} ->
            MacroArgNames = unwrap_macro_arg_var_names(MacroArgVarNames),
            WrappedArgs = [{ast, Arg} || Arg <- Args],
            CallArgsList = lists:zip(MacroArgNames, WrappedArgs),
            CallArgs = maps:from_list(CallArgsList),
            expand_macro(Macros, MacroKey, CallArgs);
        {error, _Reason} = Error -> Error
    end.

expand_macro(Macros, Name, Args) ->
    case get_macro(Macros, Name) of
        {ok, _MacroArgs, Tokens} ->
            case expand_macros(Macros, Tokens, wrap_out_args(Args)) of
                {ok, ETokens, Refs} -> parse_tokens(ETokens, Refs);
                {error, _Reason} = Error -> Error
            end;
        {error, _Reason} = Error -> Error
    end.

expand_macros(Macros, Macro, Args) ->
    expand_macros(Macros, Macro, Args, [], #{}).

expand_macros(_Macros, [], _Args, Accum, Refs) ->
    {ok, lists:reverse(Accum), Refs};
expand_macros(Macros, [{var, _, VarName}=VarAst|T], Args, Accum, Refs) ->
    case maps:get(VarName, Args, undefined) of
        undefined ->
            expand_macros(Macros, T, Args, [VarAst|Accum], Refs);
        % when called from outside vars are ast trees, so we need to put
        % a reference disguised as a var so it compiles from tokens to ast
        % after this processing and then we look for the fake vars with refs
        % and we put the ast trees there
        %
        {ast, Value} ->
            {RefVar, NewRefs} = add_ref_var(Refs, Value),
            expand_macros(Macros, T, Args, [RefVar|Accum], NewRefs);

        % when replacing a macro inside a macro the arguments are tokens,
        % this means we have to insert the tokens (reversed) now because
        % we are working on tokens at this level
        {lex, Value} ->
            expand_macros(Macros, T, Args, lists:reverse(Value) ++ Accum, Refs)
    end;
expand_macros(Macros, [{macro, {var, _, MacroName}}|T], Args, Accum, Refs) ->
    case get_macro(Macros, MacroName) of
        {ok, [], Tokens} ->
            expand_macros(Macros, T, Args, lists:reverse(Tokens) ++ Accum, Refs);
        {error, _Reason} = Error -> Error
    end;

expand_macros(Macros, [{macro_string, {var, MacroInfo, VarName}}|T], Args, Accum, Refs) ->
    case maps:get(VarName, Args, undefined) of
        undefined -> {error, {var_not_defined, VarName}};
        {ast, Value} ->
            StrAst = pp_form(MacroInfo, Value),
            expand_macros(Macros, T, Args, [StrAst|Accum], Refs);
        {lex, Tokens} ->
            case parse_aleppo_tokens(Tokens) of
                {ok, [AbsForm]} ->
                    StrAst = pp_form(MacroInfo, AbsForm),
                    expand_macros(Macros, T, Args, [StrAst|Accum], Refs);
                {error, _Reason} = Error -> Error
            end
    end;

expand_macros(Macros, [{macro, {var, _, MacroName}, MacroArgs}|T], Args, Accum, Refs) ->
    MacroArity = length(MacroArgs),
    case get_macro(Macros, {MacroName, MacroArity}) of
        {ok, MacroArgVarNames, Tokens} ->
            MacroArgNames = unwrap_macro_arg_var_names(MacroArgVarNames),
            CallArgsList = lists:zip(MacroArgNames, resolve_vars(MacroArgs, Args)),
            CallArgs = maps:from_list(CallArgsList),

            case expand_macros(Macros, Tokens, CallArgs, [], #{}) of
                {ok, ExpandedInnerMacro, InnerRefs} ->
                    NewRefs = maps:merge(Refs, InnerRefs),
                    NewAccum = lists:reverse(ExpandedInnerMacro) ++ Accum,
                    expand_macros(Macros, T, Args, NewAccum, NewRefs);
                {error, _Reason} = Error -> Error
            end;
        {error, _Reason} = Error -> Error
    end;

expand_macros(Macros, [H|T], Args, Accum, Refs) ->
    expand_macros(Macros, T, Args, [H|Accum], Refs).

unwrap_macro_arg_var_names(Vars) ->
    lists:map(fun ([{var, _, Name}]) -> Name end, Vars).

resolve_vars(Items, Args) ->
    lists:map(fun ([{var, _, Name}=Ast]) -> maps:get(Name, Args, {lex, [Ast]});
                  (Other) -> {lex, Other}
              end, Items).

add_ref_var(Refs, Value) ->
    Ref = make_ref(),
    NewRefs = maps:put(Ref, Value, Refs),
    {{var, 1, Ref}, NewRefs}.

parse_aleppo_tokens(Tokens) ->
    {ok, Tokens1} = aleppo:process_tokens(Tokens ++ [{dot, 1}]),
    erl_parse:parse_exprs(remove_eof(Tokens1, [])).

parse_tokens(Tokens, Refs) ->
    case parse_aleppo_tokens(Tokens) of
        {ok, AstWithRefs} ->
            AstReplaced = replace_ast_refs(AstWithRefs, Refs),
            {ok, AstReplaced};
        {error, _Reason} = Error -> Error
    end.

replace_ast_refs(Ast, Refs) ->
    Walker = fun (State, {var, _, Val}) when is_reference(Val) ->
                     {maps:get(Val, Refs), State};
                 (State, Other) -> {Other, State}
             end,
    {Node, _NewState} = ast_walk:exprs(Ast, Walker, no_state),
    Node.

remove_eof([], Accum) -> lists:reverse(Accum);
remove_eof([{eof, _}|T], Accum) -> remove_eof(T, Accum);
remove_eof([H|T], Accum) -> remove_eof(T, [H|Accum]).

wrap_out_args(Args) ->
    maps:map(fun (_K, {lex, _}=V) -> V;
                 (_K, {ast, _}=V) -> V;
                 (_K, V) -> {ast, V}
             end, Args).

pp_form(MacroInfo, Value) ->
    Line = proplists:get_value(line, MacroInfo, 1),
    Str = lists:flatten(erl_pp:expr(Value)),
    {string, [{line, Line}], Str}.
