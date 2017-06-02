-module(beat).
-export([main/0, main/1, compile/2, compile/3, to_code/1, to_code/2,
         to_raw_lex/1, to_lex/1, to_ast/1,
         to_erl/1, to_erl_ast/1, to_erl_ast/2,
         to_mod/1,
         pprint/1, print_errors/2]).

-export([str_to_ast/1]).

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, unicode:characters_to_list(Content, utf8)};
        Other -> Other
    end.

with_file_content(Path, Fn) ->
    case read_file(Path) of
        {ok, Content} -> Fn(Content);
        Other -> Other
    end.

to_raw_lex(Path) -> with_file_content(Path, fun str_to_raw_lex/1).
to_lex(Path)     -> with_file_content(Path, fun str_to_lex/1).
to_ast(Path)     -> with_file_content(Path, fun str_to_ast/1).
to_erl_ast(Path) -> to_erl_ast(Path, #{}).
to_erl_ast(Path, Opts) ->
    with_file_content(Path, fun (Str) -> str_to_erl_ast(Str, Path, Opts) end).

to_mod(Path) ->
    case to_erl_ast(Path) of
        {ok, {Ast, State}} ->
            ModAtomName = get_module_name(Path),
            ToMod = fun () ->
                            ExportAttr = beat_util:get_export_attr_ast(State),
                            FnAttrs = beat_util:get_fn_attribute(State),
                            ModAttr = {attribute, 1, module, ModAtomName},
                            FileAttr = {attribute, 1, file, {Path, 1}},
                            FilteredFnAttrs = remove_emptish_fn_attrs(FnAttrs, []),
                            case ExportAttr of
                                {attribute, _ExportLine, export, []} ->
                                    {ok, [FileAttr, ModAttr|(FilteredFnAttrs ++ Ast)]};
                                _ ->
                                    {ok, [FileAttr, ModAttr, ExportAttr|(FilteredFnAttrs ++ Ast)]}
                            end
                    end,
            format_errors_or(ModAtomName, State, ToMod);
        Other -> Other
    end.

pprint(Path) ->
    case to_ast(Path) of
        {ok, Ast} -> io:format("~s", [beat_pp:print(Ast)]);
        Other -> io:format("Error: ~p", [Other])
    end.

to_erl(Path) ->
    case to_mod(Path) of
        {ok, [_|Mod]} -> erl_prettypr:format(erl_syntax:form_list(Mod));
        Other -> Other
    end.

to_code(Path) ->
    to_code(Path, []).

to_code(Path, Opts) ->
    case to_mod(Path) of
        {ok, Ast} ->
            case compile:forms(Ast, [return, strong_validation|Opts]) of
                {error, _Errors, _Warnings}=Error -> Error;
                error -> {error, [{error, compile_forms_error}], []};
                {ok, _ModuleName0, ValidationInfo} ->
                    Warnings0 = case ValidationInfo of
                        [] -> [];
                        [{_FileName0, W0}] -> W0
                    end,
                    case compile:forms(Ast, Opts) of
                        {ok, ModuleName, Code} -> {ok, ModuleName, Code, Warnings0};
                        {ok, ModuleName, Code, Warnings} ->
                            {ok, ModuleName, Code, Warnings0 ++ Warnings};
                        error -> {error, [{error, compile_forms_error}], []};
                        {error, _Errors, _Warnings}=Error -> Error
                    end
            end;
        Other -> Other
    end.

compile(Path, DestPath) ->
    compile(Path, DestPath, []).

compile(Path, DestPath, Opts) ->
    case to_code(Path, Opts) of
        {ok, ModuleName, Code, Warnings} ->
            BeamPath = filename:join(DestPath, get_module_beam_name(Path)),
            case bin_to_file(Code, BeamPath) of
                error -> {error, [{file_write_error, BeamPath}], []};
                {error, enoent} -> {error, [{file_write_error, BeamPath}], []};
                ok -> {ok, [{warnings, Warnings}, {module_name, ModuleName}]}
            end;
        Other -> Other
    end.

from_erl(Path) -> epp:parse_file(Path, [], []).

str_to_raw_lex(String) -> beat_lexer:string(String).

str_to_lex(String) ->
    try beat_lexer:string(String) of
        {ok, Tokens, Endline} ->
            CleanTokens = clean_tokens(Tokens),
            {ok, CleanTokens, Endline};
        {eof, Endline} -> {error, {Endline, beat_lexer, {eof, Endline}}};
        {error, Error} -> {error, Error};
        {error, Error, _} -> {error, Error}
    catch
        throw:{error, Reason} ->
            {error, Reason}
    end.

str_to_ast(Str) ->
    case str_to_lex(Str) of
        {ok, Tokens, _NewLine} -> beat_parser:parse(Tokens);
        Other -> Other
    end.

% to force escriptize to load the default extension modules
force_extensions_load() ->
    beat_mood_expr_atom_b:handle(nil, nil, nil),
    beat_mood_expr_var__:handle(nil, nil, nil),
    beat_mood_val_atom_atom:handle(nil, nil, nil),
    beat_mood_val_atom_b:handle(nil, nil, nil),
    beat_mood_val_atom_c:handle(nil, nil, nil),
    beat_mood_val_atom_i:handle(nil, nil, nil),
    beat_mood_val_atom_m:handle(nil, nil, nil),
    beat_mood_val_atom_r:handle(nil, nil, nil),
    beat_mood_val_var__:handle(nil, nil, nil).

str_to_erl_ast(String, Path, Opts) ->
    Module = get_module_name(Path),
    force_extensions_load(),
    case str_to_ast(String) of
        {ok, Ast} -> {ok, beat_to_erl:to_erl(Ast, Module, Path, Opts)};
        Other -> Other
    end.

% cli utilities

print({ok, Data}) ->
    print(Data);
print({error, _}=Error) ->
    Reason = beat_error:normalize(Error),
    io:format("error:~s~n", [Reason]);
print(Data) ->
    try io:format("~s~n", [Data]) catch
        _:_ -> io:format("~p~n", [Data])
    end.

% command line interface

main() ->
    main([]).

main([]) ->
    io:format("Usage:~n"),  
    io:format("\tbeat beam <file> [<outdir>]: compile to beam bytecode~n"),
    io:format("\tbeat erl <file>: print erlang code~n"),  
    io:format("\t--~n"),
    io:format("\tbeat rawlex <file>: print raw tokens~n"),
    io:format("\tbeat lex <file>: print normalized tokens~n"),
    io:format("\tbeat ast <file>: print beat ast~n"),
    io:format("\tbeat erlast <file>: print erlang ast~n"),
    io:format("\t~n"),
    %%io:format("\tbeat mod     <file>: print erlang absform with module attributes~n"),
    %%io:format("\tbeat erl2ast <file.erl>: print erlang absform <file.erl>~n"),
    %%io:format("\tbeat erl2erl <file.erl>: parse and pretty print erlang <file.erl>~n"),
    %%io:format("\tbeat pprint <file>: pretty print code from <file> (experimental)~n"),
    ok;
main(["rawlex", File]) ->
    print(to_raw_lex(File));
main(["lex", File]) ->
    print(to_lex(File));
main(["ast", File]) ->
    print(to_ast(File));
main(["mod", File]) ->
    print(to_mod(File));
main(["erlast", File]) ->
    print(to_erl_ast(File));
main(["erl", File]) ->
    print(to_erl(File));
main(["erl2ast", File]) ->
    print(from_erl(File));
main(["erl2erl", File]) ->
    {ok, Ast} = from_erl(File),
    print(erl_prettypr:format(erl_syntax:form_list(Ast)));
main(["beam", File]) ->
    main(["beam", File, "."]);
main(["shell"]) ->
    user_drv:start(["tty_sl -c -e", {beat_repl, start, []}]);
main(["beam", File, OutputDir]) ->
    case compile(File, OutputDir, [debug_info]) of
        {ok, CompileInfo} ->
            print_errors(proplists:get_value(warnings, CompileInfo, []), "warnings");
        {error, Errors, Warnings} ->
            print_errors(Errors, "errors"),
            print_errors(Warnings, "warnings"),
            ok;
        Other ->
            print(Other)
    end;
main(["pprint", File]) ->
    pprint(File);
main(Opts) ->
    io:format("Invalid arguments: \"~p\"~n", [Opts]),
    main([]).

% private

clean_tokens(Tokens) -> clean_tokens(Tokens, []).

clean_tokens([], Accum) -> lists:reverse(Accum);
% remove newline after colon, comma and semicolon
clean_tokens([{colon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{semicolon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{sep, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after logical ops
clean_tokens([{bool_or, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_orr, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_xor, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_and, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_andd, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after ops
clean_tokens([{add_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{mul_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{comp_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{concat_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{send_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bin_shift, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bin_and, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bin_or, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{assign, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{coloneq, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{cons_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after open [ ( and {
clean_tokens([{open, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{open_list, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{open_map, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after arrows
clean_tokens([{arrow, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{arrowend, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{larrow, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{larrowend, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after try, catch, after, receive, begin, when
clean_tokens([{'try', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'catch', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'after', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'receive', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'begin', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'when', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'fn', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'fn', _}=H1, {var, _, _}=H2, {nl, _, _}|T], Accum) -> clean_tokens([H1, H2|T], Accum);
clean_tokens([{'fn', _}=H1, {atom, _, _}=H2, {nl, _, _}|T], Accum) -> clean_tokens([H1, H2|T], Accum);

clean_tokens([{'def', _}=H1, {atom, _, _}=H2, {nl, _, _}|T], Accum) -> clean_tokens([H1, H2|T], Accum);

% remove newline before end and @
clean_tokens([{nl, _, _}, {'end', _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {'case', _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline before closing ] ) and }
clean_tokens([{nl, _, _}, {close, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {close_list, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {close_map, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline before arrows
clean_tokens([{nl, _, _}, {arrow, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {arrowend, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {larrow, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {larrowend, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove duplicated endlines
clean_tokens([{nl, _, _}, {nl, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove last endline
clean_tokens([{nl, _, _}], Accum) -> clean_tokens([], Accum);
clean_tokens([H|T], Accum) -> clean_tokens(T, [H|Accum]).

get_module_str_name(Path) ->
    BaseName = filename:basename(Path),
    filename:rootname(BaseName).

get_module_name(Path) ->
    list_to_atom(get_module_str_name(Path)).

get_module_beam_name(Path) ->
    ModuleNameStr = get_module_str_name(Path),
    string:concat(ModuleNameStr, ".beam").

bin_to_file(Bin, Path) ->
    to_file(Bin, Path, [binary, write]).

to_file(Data, Path, Mode) ->
    case file:open(Path, Mode) of
        {ok, Device} ->
            file:write(Device, Data),
            file:close(Device),
            ok;
        Error -> Error
    end.

format_errors_or(_Module, #{errors:=[]}, Fn) -> Fn();
format_errors_or(Module, #{errors:=Errors}, _Fn) ->
    ErrorsFirstToLast = lists:reverse(Errors),
    {error, {beat, Module, beat_error:to_string(Module, ErrorsFirstToLast)}}.

% remove fn attributes that only contain the public or no attributes
remove_emptish_fn_attrs([], Accum) ->
    lists:reverse(Accum);
remove_emptish_fn_attrs([{attribute, _ALine, fn_attrs, {_FName, _FArity, []}}|T], Accum) ->
    remove_emptish_fn_attrs(T, Accum);
remove_emptish_fn_attrs([{attribute, _ALine, fn_attrs, {_FName, _FArity, [{[public], {[], []}}]}}|T], Accum) ->
    remove_emptish_fn_attrs(T, Accum);
remove_emptish_fn_attrs([H|T], Accum) ->
    remove_emptish_fn_attrs(T, [H|Accum]).

print_errors([], _Prefix) -> ok;
print_errors(Errors, Prefix) ->
    lists:foreach(fun (Error) ->
                          Reason = beat_error:normalize(Error),
                          io:format("~s:~n~s~n", [Prefix, Reason])
                  end, Errors).
