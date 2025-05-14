% Program entry point
program(parsetree(X)) --> ['#DEVI'], parse(X), ['#'].

% Parse a block consisting of declarations and commands
parse(block(Decls, Comms)) --> declare(Decls), comm(Comms),!.   % Declarations followed by commands
parse(block(Decls)) --> declare(Decls),!.                      % Only declarations
parse(block(Comms)) --> comm(Comms),!.                         % Only commands

% Declarations
declare(dtree(X, Y)) --> variable_declaration(X), declare(Y).  % Declaration of multiple variables
declare(dtree(X)) --> variable_declaration(X).                % Declaration of a single variable

% Variable declaration with or without initialization
variable_declaration(declare(Type, Var, Value)) --> datatype(Type), id(Var), [=], expr(Value).
variable_declaration(declare(Type, Var)) --> datatype(Type), id(Var). 

% Assignment statement assigning an expression to a variable
assignment(assign(Var, Value)) --> id(Var), [=], expr(Value).

% Command parsing
comm(command(X, Y)) --> cb(X), comm(Y).            % Command with multiple statements
comm(command(X)) --> cb(X).                        % Command with a single statement

% Command building
cb(X) --> assignment(X).    
cb(X) --> variable_declaration(X).  
cb(print(X)) --> [print], ['#'], expr(X), ['#'].
cb(if(X, Y, Z)) --> [if], ['('], cond(X),[')'], ['{'], comm(Y), ['}'], [else], ['{'], comm(Z), ['}'], [endif]. % If command
cb(while(X, Y)) --> [while], ['('], cond(X), [')'], ['{'], comm(Y), ['}'], [endwhile]. % While loop command
cb(for(X, Y, Z, W)) --> [for], ['('], declare(X), ['#'], cond(Y), ['#'], assignment(Z), [')'], ['{'],comm(W),['}'], [endfor]. % For loop
cb(ternary(V, Y, Z)) --> cond(V), ['?'],['{'], expr(Y), ['}'],[':'], ['{'],expr(Z),['}']. % Ternary expression

% Condition parsing
cond(X) --> boolean_expr(X).  % boolean 
cond(cond(X, Op, Y)) --> expr(X), relational_op(Op), expr(Y).
cond(boolNot(X)) --> [not], ['('], cond(X), [')'].                   % Negation condition
cond(booland(X,Op,Y,Z)) --> expr(X), relational_op(Op), expr(Y), [and], cond(Z). % Logical AND
cond(boolor(X,Op,Y,Z)) --> expr(X), relational_op(Op), expr(Y), [or], cond(Z). % Logical OR

% Expression parsing for arithmetic operations
expr(X) --> boolean_expr(X).
expr(X) --> id(X).                                % Expression as identifier
expr(X) --> number(X).
expr(X) --> string_literal(X).% Data type definition
expr(ternary(V, Y, Z)) --> ['('],cond(V),[')'], ['?'],['{'], expr(Y), ['}'],[':'], ['{'],expr(Z),['}']. % Ternary expression
expr(X) --> ['('], expr(X), [')'].  
expr(add(X, Y)) --> id(X), [+], expr(Y).                % Addition with identifier
expr(add(X, Y)) --> number(X), [+], expr(Y).               % Addition with number
expr(sub(X, Y)) --> id(X), [-], expr(Y).                % Subtraction with identifier
expr(sub(X, Y)) --> number(X), [-], expr(Y).               % Subtraction with number
expr(mul(X, Y)) --> id(X), [*], expr(Y).                % Multiplication with identifier
expr(mul(X, Y)) --> number(X), [*], expr(Y).               % Multiplication with number
expr(div(X, Y)) --> id(X), [/], expr(Y).                % Division with identifier
expr(div(X, Y)) --> number(X), [/], expr(Y).               % Division with number

boolean_expr(bool(true)) --> [true].         % Literal true
boolean_expr(bool(false)) --> [false].       % Literal false

datatype(datatype(integer)) --> [integer].
datatype(datatype(string)) --> [string].
datatype(datatype(boolean)) --> [boolean].
datatype(datatype(float)) --> [float].

relational_op(<) --> [<].
relational_op(>) --> [>].
relational_op(==) --> [==].
relational_op(<=) --> [<=].
relational_op(>=) --> [>=].

id(var(Var)) --> [Var], { 
    atom(Var), 
    atom_chars(Var, [FirstChar | Rest]), 
    (char_type(FirstChar, alpha) ; FirstChar = '_'), 
    all_alnum_or_underscore(Rest) 
}.

% Match string literals surrounded by double quotes
string_literal(string(Str)) -->
    [Atom],
    { atom(Atom),                              % Ensure it's an atom
      atom_chars(Atom, Chars),                 % Break the atom into characters
      Chars = ['"' | Rest],                    % Check if it starts with a quote
      append(StringChars, ['"'], Rest),        % Ensure it ends with a quote
      atom_chars(Str, StringChars)             % Extract content between quotes
    }.

number(num(Num)) --> [Num], { number(Num) }.

% Helper for checking valid variable names
all_alnum_or_underscore([]).
all_alnum_or_underscore([Char | Rest]) :-
    (char_type(Char, alnum) ; Char == '_'),
    all_alnum_or_underscore(Rest).