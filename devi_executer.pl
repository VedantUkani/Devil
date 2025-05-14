:- ['devi_tokenizer'].  
:- ['devi_parser'].  
:- ['devi_evaluator'].  

run :-
    current_prolog_flag(argv, Args),
    (   Args = [File, '--debug'] ->
        (   check_devi_extension(File) ->
            debug_process_file(File),
	halt
        ;   write('Error: Only .devi files are allowed!'), nl, halt(1)
        )
    ;   Args = [File] ->
        (   check_devi_extension(File) ->
            process_file(File),
	halt
        ;   write('Error: Only .devi files are allowed!'), nl, halt(1)
        )
    ;   write('Usage: ./devil filename.devi [--debug]'), nl, halt(1)
    ).

% Tokenize, parse, and evaluate the file
process_file(File) :-
    read_file_to_string(File, FileContent, []),
    tokenize_input(FileContent, ListOfTokens),
    program(ParsedTree, ListOfTokens, []),
    write('Program Output:\n'),
    write('-------------\n'),
    program_eval(ParsedTree, _),
    successful_flag.  % Call successful_flag

% Debug version of process_file
debug_process_file(File) :-
    read_file_to_string(File, FileContent, []),
    write('\n[DEBUG] File Content:\n'),
    write(FileContent),
    nl,  % Newline after file content
    write('\n[DEBUG] Tokenizing...\n'),
    tokenize_input(FileContent, ListOfTokens),
    write('[DEBUG] Token List:\n'),
    write(ListOfTokens),
    nl,  % Newline after token list
    (program(ParsedTree, ListOfTokens, []) -> 
        write('[DEBUG] Parsed Tree:\n'),
        write(ParsedTree),
        nl,  % Newline after parsed tree
        write('[DEBUG] Evaluating Program...\n'),
        write('Program Output:\n'),
        write('-------------\n'),
        program_eval(ParsedTree, _),
        successful_flag  % Call successful_flag 
    ;   write('[ERROR] Parsing failed!\n'),
        halt(1)
    ).

successful_flag :-
    ansi_format([bold,fg(green)], 'SUCCESSFUL', []).

% Check if the file has a .devi extension
check_devi_extension(File) :-
    file_name_extension(_, Extension, File),
    Extension = 'devi'.

:- initialization(run).

