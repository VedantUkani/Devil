% Program Evaluation starts with the main program block enclosed by '#DEVI' and '#'
program_eval(parsetree(Block), FinalEnv) :- 
    block_eval(Block, [], FinalEnv).

% Block Evaluation handles declarations and commands within the block
block_eval(block(Decls, Comms), Env, FinalEnv) :-
    declarations_eval(Decls, Env, TempEnv),
    commands_eval(Comms, TempEnv, FinalEnv).
block_eval(block(Decls), Env, FinalEnv) :-
    declarations_eval(Decls, Env, FinalEnv).
block_eval(block(Comms), Env, FinalEnv) :-
    commands_eval(Comms, Env, FinalEnv).

% Declarations Evaluation
declarations_eval(dtree(Declaration, Rest), Env, FinalEnv) :-
    variable_declaration_eval(Declaration, Env, TempEnv),
    declarations_eval(Rest, TempEnv, FinalEnv).
declarations_eval(dtree(Declaration), Env, FinalEnv) :-
    variable_declaration_eval(Declaration, Env, FinalEnv).

% Variable Declaration Evaluation
variable_declaration_eval(declare(datatype(Type), var(Var), Expr), Env, FinalEnv) :-
    expression_eval(Expr, Env, Value),
    update_env(Env, Var, data(Type, Value), FinalEnv).
variable_declaration_eval(declare(datatype(Type), var(Var)), Env, FinalEnv) :-
    update_env(Env, Var, data(Type, undefined), FinalEnv).

% Helper for Environment Update
update_env([], Var, Value, [Var=Value]).
update_env([Var=data(Type,_) | Rest], Var, Value, [Var=data(Type,Value) | Rest]).
update_env([H | Rest], Var, Value, [H | UpdatedRest]) :-
    update_env(Rest, Var, Value, UpdatedRest).

% Commands Evaluation
commands_eval(command(Command, Rest), Env, FinalEnv) :-
    command_eval(Command, Env, TempEnv),
    commands_eval(Rest, TempEnv, FinalEnv).
commands_eval(command(Command), Env, FinalEnv) :-
    command_eval(Command, Env, FinalEnv).

% Command Evaluation
command_eval(assign(var(Var), Expr), Env, FinalEnv) :-
    expression_eval(Expr, Env, Value),
    update_env(Env, Var, Value, FinalEnv).

command_eval(declare(datatype(Type), var(Var), Expr), Env, FinalEnv) :-
    expression_eval(Expr, Env, Value),
    update_env(Env, Var, data(Type, Value), FinalEnv).

command_eval(declare(datatype(Type), var(Var)), Env, FinalEnv) :-
    update_env(Env, Var, data(Type, undefined), FinalEnv).

command_eval(print(Expr), Env, Env) :-
    expression_eval(Expr, Env, Value),
    writeln(Value).

command_eval(if(Cond, ThenBlock, _), Env, FinalEnv) :-
    condition_eval(Cond, Env, true), !,
    commands_eval(ThenBlock, Env, FinalEnv).

command_eval(if(_, _, ElseBlock), Env, FinalEnv) :-
    commands_eval(ElseBlock, Env, FinalEnv).

command_eval(while(Cond, Body), Env, FinalEnv) :-
    condition_eval(Cond, Env, true), !,
    commands_eval(Body, Env, TempEnv),
    command_eval(while(Cond, Body), TempEnv, FinalEnv).

command_eval(while(_, _), Env, Env).

command_eval(for(Init, Cond, Update, Body), Env, FinalEnv) :-
    declarations_eval(Init, Env, TempEnv1),
    for_loop_eval(Cond, Update, Body, TempEnv1, FinalEnv).

% For Loop Evaluation
for_loop_eval(Cond, Update, Body, Env, FinalEnv) :-
    condition_eval(Cond, Env, true), !,
    commands_eval(Body, Env, TempEnv1),
    command_eval(Update, TempEnv1, TempEnv2),
    for_loop_eval(Cond, Update, Body, TempEnv2, FinalEnv).

for_loop_eval(_, _, _, Env, Env).

% Condition Evaluation
condition_eval(bool(true), _, true).

condition_eval(bool(false), _, false).

condition_eval(cond(Left, Op, Right), Env, Result) :-
    expression_eval(Left, Env, LValue),
    expression_eval(Right, Env, RValue),
    compare_op(Op, LValue, RValue, Result).

condition_eval(booland(Cond1, Cond2), Env, Result) :-
    condition_eval(Cond1, Env, Val1),
    condition_eval(Cond2, Env, Val2),
    and_op(Val1, Val2, Result).

condition_eval(boolor(Cond1, Cond2), Env, Result) :-
    condition_eval(Cond1, Env, Val1),
    condition_eval(Cond2, Env, Val2),
    or_op(Val1, Val2, Result).

% Logical AND
and_op(true, true, true).
and_op(_, _, false).

% Logical OR
or_op(false, false, false).
or_op(_, _, true).

% Expression Evaluation
expression_eval(ternary(Cond, ThenExpr, _), Env, Value) :-
    condition_eval(Cond, Env, true), !,            % Evaluate condition; if true
    expression_eval(ThenExpr, Env, Value).         % Evaluate 'then' expression
expression_eval(ternary(_, _, ElseExpr), Env, Value) :-
    expression_eval(ElseExpr, Env, Value).         % Evaluate 'else' expression

expression_eval(add(Left, Right), Env, Value) :-
    expression_eval(Left, Env, LValue),
    expression_eval(Right, Env, RValue),
    Value is LValue + RValue.
expression_eval(sub(Left, Right), Env, Value) :-
    expression_eval(Left, Env, LValue),
    expression_eval(Right, Env, RValue),
    Value is LValue - RValue.
expression_eval(mul(Left, Right), Env, Value) :-
    expression_eval(Left, Env, LValue),
    expression_eval(Right, Env, RValue),
    Value is LValue * RValue.
expression_eval(div(Left, Right), Env, Value) :-
    expression_eval(Left, Env, LValue),
    expression_eval(Right, Env, RValue),
    Value is LValue / RValue.
expression_eval(var(Var), Env, Value) :-
    memberchk(Var=data(_,Value), Env).
expression_eval(num(Value), _, Value).
expression_eval(string(Str), _, Str).
expression_eval(bool(true), _, true).
expression_eval(bool(false), _, false).
% Comparison Operators
compare_op(<, L, R, true) :- L < R.
compare_op(<, _, _, false).
compare_op(>, L, R, true) :- L > R.
compare_op(>, _, _, false).
compare_op(==, L, R, true) :- 
    (   boolean(L), boolean(R)
    ->  L = R   % Use logical equality for booleans
    ;   L =:= R  % Use arithmetic equality for numbers
    ).
compare_op(==, _, _, false).
compare_op(<=, L, R, true) :- L =< R.
compare_op(<=, _, _, false).
compare_op(>=, L, R, true) :- L >= R.
compare_op(>=, _, _, false).

boolean(true).
boolean(false).
