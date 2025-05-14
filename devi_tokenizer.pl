% Tokenizer definition
tokenize_input(Input, Tokens) :-
    string_codes(Input, Codes),    % Convert string to character codes (works for both strings and atoms)
    tokenize_character_codes(Codes, Tokens).

tokenize_character_codes([], []):-!.  

tokenize_character_codes([32|Rest], Tokens) :-    % 32 is the ASCII code for space
    tokenize_character_codes(Rest, Tokens),!.
tokenize_character_codes([10|Rest], Tokens) :-    % 10 is the ASCII code for newline
    tokenize_character_codes(Rest, Tokens),!.
tokenize_character_codes([9|Rest], Tokens) :-     % 9 is the ASCII code for tab
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for `#DEVI` and `#`
tokenize_character_codes(Codes, ['#DEVI'|Tokens]) :-
    phrase(devil_start_token, Codes, Rest),
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for floating-point numbers
tokenize_character_codes(Codes, [Value|Tokens]) :-
    phrase(floating_token(Value), Codes, Rest),
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for integers
tokenize_character_codes(Codes, [Value|Tokens]) :-
    phrase(integer_token(Value), Codes, Rest),
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for strings
tokenize_character_codes([34 | Rest], [String | Tokens]) :-  % 34 is ASCII for `"`
    extract_string(Rest, StringChars, Remaining),
    append([34], StringChars, TempString),
    append(TempString,[34],FinalString),
    atom_chars(String,FinalString),
    tokenize_character_codes(Remaining, Tokens),!.

% Tokenization for identifiers
tokenize_character_codes(Codes, [Token|Tokens]) :-
    phrase(identifier_token(Token), Codes, Rest),
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for comparison operators
tokenize_character_codes([61, 61 | Rest], ['==' | Tokens]) :-  % '=='
    tokenize_character_codes(Rest, Tokens),!.
tokenize_character_codes([60, 61 | Rest], ['<=' | Tokens]) :-  % '<='
    tokenize_character_codes(Rest, Tokens),!.
tokenize_character_codes([62, 61 | Rest], ['>=' | Tokens]) :-  % '>='
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for ASCII operators and keywords
tokenize_character_codes([Char|Rest], [Token|Tokens]) :-
    map_ascii_operator(Char, Token),           
    tokenize_character_codes(Rest, Tokens),!.

% Tokenization for keywords (including 'if', 'else', 'while', etc.)
tokenize_character_codes(Codes, [Keyword|Tokens]) :-
    phrase(keyword_definition(Keyword), Codes, Rest),
    tokenize_character_codes(Rest, Tokens).

tokenize_character_codes([_|Rest], Tokens) :- 
    tokenize_character_codes(Rest, Tokens).

% Floating-point token definition
floating_token(Value) -->
    integer_digit_sequence(WholePart),
    ".", 
    integer_digit_sequence(FractionalPart), 
    { 
        WholePart \= [],
        FractionalPart \= [],
        append(WholePart, [46|FractionalPart], NumChars), % 46 is ASCII for '.'
        string_codes(NumString, NumChars), % Convert character codes to a string
        number_string(Value, NumString) % Convert the string to a number
    }.

% Integer token definition
integer_token(Value) --> 
    integer_digit_sequence(Digits), { 
        Digits \= [], 
        number_chars(Value, Digits) 
    }.

integer_digit_sequence([Digit|Rest]) --> 
    [Digit], { char_type(Digit, digit) }, integer_digit_sequence(Rest).

integer_digit_sequence([Digit]) --> 
    [Digit], { char_type(Digit, digit) }.

integer_digit_sequence([]) --> [].

% Extract string until the next quotation mark
extract_string([34 | Rest], [], Rest).  % End of string (closing `"` found)
extract_string([Char | Rest], [Char | StringChars], Remaining) :-
    Char \= 34,                        % Ensure it's not a closing `"`
    extract_string(Rest, StringChars, Remaining).

% Identifier token definition
identifier_token(Token) -->
    identifier_character_sequence(Chars), { 
        Chars \= [], 
        atom_chars(Token, Chars) 
    }.

% Modified identifier character sequence
identifier_character_sequence([Char|Rest]) -->
    [Char], { char_type(Char, alpha); Char = 95 }, % First character: letter or '_'
    identifier_character_sequence_rest(Rest).

% Rest of the identifier can include letters, digits, and underscores
identifier_character_sequence_rest([Char|Rest]) -->
    [Char], { char_type(Char, alnum); Char = 95 }, 
    identifier_character_sequence_rest(Rest).

identifier_character_sequence_rest([]) --> [].

% Tokens for `#DEVI` and `#`
devil_start_token --> "#DEVI".

% ASCII operator mappings
map_ascii_operator(40, '(').    % ASCII code for '('
map_ascii_operator(41, ')').    % ASCII code for ')'
map_ascii_operator(123, '{').   % ASCII code for '{'
map_ascii_operator(125, '}').   % ASCII code for '}'
map_ascii_operator(61, '=').    % ASCII code for '='
map_ascii_operator(59, ';').    % ASCII code for ';'
map_ascii_operator(43, '+').    % ASCII code for '+'
map_ascii_operator(45, '-').    % ASCII code for '-'
map_ascii_operator(42, '*').    % ASCII code for '*'
map_ascii_operator(47, '/').    % ASCII code for '/'
map_ascii_operator(60, '<').    % ASCII code for '<'
map_ascii_operator(62, '>').    % ASCII code for '>'
map_ascii_operator(35, '#').    % ASCII code for '#'
map_ascii_operator(63, '?').    % ASCII code for '?'
map_ascii_operator(58, ':').    % ASCII code for ':'

% Keyword mappings
keyword_definition(if) --> "if".
keyword_definition(endif) --> "endif".               
keyword_definition(else) --> "else".                 
keyword_definition(while) --> "while".  
keyword_definition(endwhile) --> "endwhile".               
keyword_definition(not) --> "not".   
keyword_definition(and) --> "and".                   
keyword_definition(or) --> "or".                                   
keyword_definition(integer) --> "integer". 
keyword_definition(float) --> "float".                         
keyword_definition(string) --> "string".             
keyword_definition(boolean) --> "boolean".           
keyword_definition(for) --> "for".     
keyword_definition(endfor) --> "endfor".               
keyword_definition(print) --> "print". 