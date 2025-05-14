
% Start of Program
program --> ['#DEVI'], statement_list, ['#'].

% A statement can be any of the defined statement types.
statement_list --> statement, statement_list.
statement_list --> [].

statement --> variable_declaration.
statement --> assignment.
statement --> print_statement.
statement --> for_statement.
statement --> if_statement.
statement --> while_statement.
statement --> ternary_expression.

% Declaring Variable with Datatype
variable_declaration --> datatype, variable.
variable_declaration --> datatype, variable, [=], expression.

% Assignment statement that assigns an expression to an identifier.
assignment --> variable, [=], expression.

% Print Statement
print_statement --> [print], ['#'], expression, ['#'].  

for_statement --> [for], ['('], variable_declaration, ['#'], condition, ['#'], assignment, [')'], block, [endfor].

% If statement with a condition and optional else block.
if_statement --> [if], ['('], condition, [')'], ['{'], statement_list, ['}'], else_block, [endif].
else_block --> [else], ['{'], statement_list, ['}'].
else_block --> [].

% While statement that continues as long as the condition is true.
while_statement --> [while], ['('], condition, [')'], ['{'], statement_list, ['}'], [endwhile].

% Ternary conditional expression.
ternary_expression --> variable, [=],['('],condition,[')'], ['?'], ['{'],expression,['}'], [':'], ['{'],expression,['}'].

% Block of statements enclosed in curly braces.
block --> ['{'], statement_list, ['}'].

% Condition can be a logical expression or a comparison between two expressions.
condition --> boolean.
condition --> expression, comparison_operator, expression.
condition --> [not],['('],condition,[')'].

% Expression can be a digit, identifier, or a combination of identifiers and operators.
expression --> value.
expression --> variable.
expression --> string.
expression --> boolean.
expression --> ['('], expression, [')']. 
expression --> variable, arithmetic_operator, expression.

string --> double_quote, character_phrase, double_quote.

character_phrase --> character, character_phrase.
character_phrase --> character.

character --> lower_case | upper_case | digit | symbol.

% Boolean
boolean --> [true].
boolean --> [false].

% Variable rules
variable --> lower_case, variable.         % e.g., aVar123
variable --> upper_case, variable.         % e.g., AVar
variable --> variable, digit, variable.    % e.g., var1name
variable --> variable, digit, variable.    % e.g., var1
variable --> ['_'], variable.              % e.g., _var
variable --> lower_case.                   % e.g., x
variable --> upper_case.                   % e.g., X

% Value rule
value --> float | integer

float --> integer, ['.'], integer.
float --> integer.

integer --> digit, integer.
integer --> digit.

% Data Type
datatype --> [integer] | [string] | [boolean] | [float].

% Comparison operator
comparison_operator --> [==].
comparison_operator --> [<].
comparison_operator --> [>].
comparison_operator --> [<=].
comparison_operator --> [>=].

% Arithmetic operators
arithmetic_operator --> [+] | [-] | [*] | [/].

double_quote --> ['\"'].

digit --> ['0'] | ['1'] | ['2'] | ['3'] | ['4'] | ['5'] | ['6'] | ['7'] | ['8'] | ['9'].

lower_case --> ['a'] | ['b'] | ['c'] | ['d'] | ['e'] | ['f'] | ['g'] | ['h'] | ['i'] | ['j'] | ['k'] | ['l'] | ['m'] | ['n'] | ['o'] | ['p'] | ['q'] | ['r'] | ['s'] | ['t'] | ['u'] | ['v'] | ['w'] | ['x'] | ['y'] | ['z'].

upper_case --> ['A'] | ['B'] | ['C'] | ['D'] | ['E'] | ['F'] | ['G'] | ['H'] | ['I'] | ['J'] | ['K'] | ['L'] | ['M'] | ['N'] | ['O'] | ['P'] | ['Q'] | ['R'] | ['S'] | ['T'] | ['U'] | ['V'] | ['W'] | ['X'] | ['Y'] | ['Z'].

symbol --> [' '] | ['!'] | ['\"'] | ['#'] | ['$'] | ['%'] | ['&'] | ['\''] | ['('] | [')'] | ['*'] | ['+'] | [','] | ['-'] | ['.'] | ['/'] | [':'] | [';'] | ['<'] | ['='] | ['>'] | ['?'] | ['@'] | ['['] | ['\\'] | [']'] | ['^'] | ['_'] | ['`'] | ['{'] | ['|'] | ['}'] | ['~'].





