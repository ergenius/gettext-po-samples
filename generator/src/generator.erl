-module(generator).
%% -*- coding: utf-8 -*-
%% Copyright (c) 2022, Madalin Grigore-Enescu <github@ergenius.com> <www.ergenius.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% API exports
-export([main/1]).

-include_lib("kernel/include/file.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc escript entry point
main(Args) ->

    Arguments = args_parse(Args),
    case erlang:length(Arguments) of
        0 -> help();
        _ -> action(Arguments)
    end.

%% @doc Output help and halt
help() ->

    io:format("generator [-h|--help|help] [dir=<full path to project directory>]"
    "\n\nWhere:"
    "\n\n[-h|--help|help]"
    "\nOptional. Show this help message."
    "\n\ndir=<full path to project directory>"
    "\nMandatory. Specify the absolute path to the directory where the input data subdirectory exist and output files will be generated.\n"),
    erlang:halt(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% args
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse escript arguments and convert them into a proplist
args_parse(Args) -> args_parse(Args, []).
args_parse([H|T], Acum) ->
    {Key, Value} = args_parse_key_value(H),
    args_parse(T, [{Key, Value} | Acum]);
args_parse([], Acum) -> lists:reverse(Acum).

%% @doc Convert one escript argument into {Key, Value}
%% If the argument doesn't have a value, {Arg, undefined} is returned.
args_parse_key_value(Arg) -> args_parse_key_value(Arg, []).
args_parse_key_value([$=|T], Key) -> {lists:reverse(Key), T};
args_parse_key_value([H|T], Key) -> args_parse_key_value(T, [H|Key]);
args_parse_key_value([], Key) -> {lists:reverse(Key), []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Handle script actions
action(Arguments) ->

    %% help
    case (proplists:is_defined("-h", Arguments) or
        proplists:is_defined("--help", Arguments) or
        proplists:is_defined("help", Arguments)) of
        true -> help();
        false -> ok
    end,

    %% Generate
    case proplists:get_value("dir", Arguments, undefined) of
        Dir when erlang:is_list(Dir), erlang:length(Dir) > 0 ->
            action_generate(Dir);
        _ -> help()
    end.

%% @doc Generate files
action_generate(Dir) ->

    case filelib:is_dir(Dir) of
        true -> ok;
        false ->
            io:format("Specified dir directory could not be found!"),
            help()
    end,

    %% Process languages
    {ok, [CldrLanguages]} = file:consult(filename:join([Dir, "generator", "data", "languages.config"])),
    ok = generate(CldrLanguages, Dir),

    %% Done!
    erlang:halt(0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Generate PO files
generate(CldrLanguages, Dir) ->

    {ok, PotTemplate} = file:read_file(filename:join([Dir, "generator", "data", "template.pot"])),

    %% Delete all existing files first
    {ok, FilesToDelete} = dir_list_files(filename:join([Dir, "po"]), [regular], [".po", ".pot"]),
    delete_files(FilesToDelete),

    %% Create POT file
    MsgstrN = <<"msgstr[0]=\"\"\nmsgstr[1]=\"\"">>,
    Bin = pot_template_replace(PotTemplate, <<"en">>, <<"2">>, <<"n != 1">>, <<"English">>, <<"">>, MsgstrN),
    ok = write_utf8_file(filename:join([Dir, "po", "template.pot"]), Bin),

    %% Create PO files
    {ok, ByFormula} = generate_po(CldrLanguages, PotTemplate, Dir),

    %% Write /data/plural-forms
    ByFormulaList = formula_orddict_to_list(ByFormula),
    PluralFormsEterm = erlang:iolist_to_binary([unicode:encoding_to_bom(utf8), lists:map(fun(Term) -> io_lib:format("~tp.~n", [Term]) end, [ByFormulaList])]),
    file:write_file(filename:join([Dir, "data", "plural-forms.eterm"]), PluralFormsEterm),

    PluralFormsJson = jason:encode({"languagesByFormula", ByFormulaList}),
    file:write_file(filename:join([Dir, "data", "plural-forms.json"]), PluralFormsJson).

%% @doc Generate all PO files
generate_po(L, Template, Dir) -> generate_po(L, Template, Dir, orddict:new()).
generate_po([{Language, Specs}|T], Template, Dir, ByFormula) ->

    io:format("~nProcessing language: ~p", [{Language, Specs}]),

    LanguageName = proplists:get_value("name", Specs),
    Formula = proplists:get_value("formula", Specs),
    Nplurals = proplists:get_value("nplurals", Specs),
    Cases = proplists:get_value("cases", Specs),
    _Territory = proplists:get_value("territory", Specs),
    _BaseLanguage = proplists:get_value("baseLanguage", Specs),

    BinPORevisionDate = utils_po_revision_date(),
    BinLanguage = erlang:list_to_binary(Language),
    BinLanguageName = erlang:list_to_binary(LanguageName),
    BinFormula = erlang:list_to_binary(Formula),
    BinNplurals = erlang:integer_to_binary(Nplurals),
    BinMsgstrN = generate_po_msgstr_n(Cases),
    BinPo = pot_template_replace(Template, BinLanguage, BinNplurals, BinFormula, BinLanguageName, BinPORevisionDate, BinMsgstrN),
    ok = write_utf8_file(filename:join([Dir, "po", Language++".po"]), BinPo),

    %% Add language by formula to dictionary
    ByFormulaKey = {Nplurals, Cases, Formula},
    ByFormula1 = case orddict:find(ByFormulaKey, ByFormula) of
                     {'ok', ByformulaValue} ->
                         orddict:store(ByFormulaKey, lists:append(ByformulaValue, [Language]), ByFormula);
                     _ -> orddict:store(ByFormulaKey, [Language], ByFormula)
                   end,

    generate_po(T, Template, Dir, ByFormula1);
generate_po([], _Template, _Dir, ByFormula) -> {ok, ByFormula}.

%% @doc Generate msgstr[n] from cases
generate_po_msgstr_n(Cases) -> generate_po_msgstr_n(Cases, 0, "", []).
generate_po_msgstr_n([H|T], Index, NL, Acum) ->
    Msgstr = erlang:iolist_to_binary([NL, "# ", H, "\nmsgstr[", erlang:integer_to_binary(Index), "] \"\""]),
    generate_po_msgstr_n(T, Index+1, "\n", [Msgstr | Acum]);
generate_po_msgstr_n([], _Index, _NL, Acum) -> erlang:iolist_to_binary(lists:reverse(Acum)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Write utf8 file
write_utf8_file(Filename, Bin) ->
    file:write_file(Filename, Bin).

%% @doc Replace fragments in template
pot_template_replace(PotTemplate, Language, Nplurals, Plural, LanguageTeam, PORevisionDate, MsgstrN) ->

%% "Language: {{Language}}\n"
%% "Content-Type: text/plain; charset=UTF-8\n"
%% "MIME-Version: 1.0\n"
%% "Content-Transfer-Encoding: 8bit\n"
%% "Plural-Forms: nplurals={{Nplurals}}; plural=({{Plural}});\n"
%% "Language-Team: {{LanguageTeam}}\n"
%% {{PORevisionDate}}
    PotTemplate1 = binary:replace(PotTemplate, <<"{{language}}">>, Language),
    PotTemplate2 = binary:replace(PotTemplate1, <<"{{nplurals}}">>, Nplurals),
    PotTemplate3 = binary:replace(PotTemplate2, <<"{{plural}}">>, Plural),
    PotTemplate4 = binary:replace(PotTemplate3, <<"{{language-team}}">>, LanguageTeam),
    PotTemplate5 = binary:replace(PotTemplate4, <<"{{po-revision-date}}\n">>, PORevisionDate),
    binary:replace(PotTemplate5, <<"{{msgstr[n]}}">>, MsgstrN, [global]).

%% @doc Generate PO Revision Date
utils_po_revision_date() ->
    TS = os:timestamp(),
    {{Year,Month,Day},{Hour,Minute,_Second}} = calendar:now_to_universal_time(TS),
    erlang:list_to_binary(lists:flatten(io_lib:format("\"PO-Revision-Date: ~4..0w-~2..0w-~2..0w ~2..0w:~2..0w+0000\\n\"\n",[Year, Month, Day, Hour, Minute]))).

%% @doc Convert orddict to list that can be converted into a JSON
formula_orddict_to_list(Orddict) -> formula_orddict_to_list(orddict:to_list(Orddict), []).
formula_orddict_to_list([{{Nplurals, Cases, Formula}, Languages}|T], Acum) ->
    formula_orddict_to_list(T, [[
        {"nplurals", Nplurals},
        {"formula", Formula},
        {"cases", Cases},
        {"languages", Languages}] | Acum]);
formula_orddict_to_list([], Acum) -> lists:reverse(Acum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% dir_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc List files into the directory with the specified extension
dir_list_files(Dir, Types, Extensions) ->
    case file:list_dir(Dir) of
        {ok, Filenames} -> dir_list_files_r(Dir, Filenames, Types, Extensions);
        Error -> Error
    end.
dir_list_files_r(Dir, Filenames, Types, Extensions) -> dir_list_files_r(Dir, Filenames, Types, Extensions, []).
dir_list_files_r(Dir, [Filename|T], Types, Extensions, Acum) ->
    FullFilename = filename:join(Dir, Filename),
    case file:read_file_info(FullFilename, [{time, posix}]) of
        {ok, FileInfo} ->
            case dir_list_files_match(FullFilename, FileInfo, Types, Extensions) of
                true ->
                    dir_list_files_r(Dir, T, Types, Extensions, [{FullFilename, FileInfo}, Acum]);
                false -> dir_list_files_r(Dir, T, Types, Extensions, Acum)
            end;
        Error -> Error
    end;
dir_list_files_r(_Dir, [], _Types, _Extensions, Acum) -> {ok, lists:flatten(Acum)}.

dir_list_files_match(Filename, #file_info{type = Type}, Types, Extensions) when
    erlang:is_list(Types), erlang:is_list(Extensions) ->
    case lists:member(Type, Types) of
        true -> lists:member(filename:extension(Filename), Extensions);
        _ -> false
    end;
dir_list_files_match(_Filename, #file_info{type = Type}, Types, _Extensions) when
    erlang:is_list(Types) -> lists:member(Type, Types);
dir_list_files_match(Filename, _FileInfo, _Types, Extensions) when
    erlang:is_list(Extensions) -> lists:member(filename:extension(Filename), Extensions).

delete_files([{Filename,_}|T]) ->
    file:delete(Filename),
    delete_files(T);
delete_files([]) -> ok.
