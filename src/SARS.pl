:- table boolean/3, expression/3, term/3.

%to Process the program and parse
program(t_program(Program)) -->['begin'], block(Program), ['end'].

%to process the block 
block(t_blk(K)) --> ['{'], block_section(K), ['}']. 
block_section(t_blk(K, L)) --> statements(K), block_section(L).
block_section(t_blk(K)) --> statements(K).


%to process different types of statement
statements(t_stms(Statement)) --> declaration(Statement), [;].
statements(t_stms(Statement)) --> assignment(Statement), [;].
statements(t_stms(Statement)) --> expression(Statement), [;].
statements(t_stms(Statement)) --> boolean(Statement), [;].
statements(t_stms(Statement)) --> print(Statement), [;].
statements(t_stms(Statement)) --> ifcondition(Statement).
statements(t_stms(Statement)) --> ternarycondition(Statement), [;].
statements(t_stms(Statement)) --> forloop(Statement).
statements(t_stms(Statement)) --> whileloop(Statement).
statements(t_stms(Statement)) --> forrange(Statement).
statements(t_stms(Statement)) --> iterator(Statement), [;].

%to process var declaration
declaration(t_decint(int, X, Y)) --> ['int'], identifier(X), ['='], expression(Y).
declaration(t_decstr(string, X, Y)) --> ['string'], identifier(X), ['='], string(Y).
declaration(t_decbool(bool, X, true)) --> ['bool'], identifier(X), [=], ['true'].
declaration(t_decbool(bool, X, false)) --> ['bool'], identifier(X), [=], ['false'].
declaration(t_declare(X, Y)) --> type(X), identifier(Y).


%to process assignment operations
assignment(t_assignmnt(X, Y)) --> identifier(X), ['='], expression(Y).
assignment(t_assignmnt(X, Y)) --> identifier(X), ['='], boolean(Y).

%to process datatype
type(int) --> ['int'].
type(string) --> ['string'].
type(bool) --> ['bool'].

%to parse whileloop
whileloop(t_whileloop(A, B)) --> ['while'], ['('], (condition(A);boolean(A)), [')'], block(B).

%to parse forloop
forloop(t_forloop(W, X, Y, Z)) --> ['for'], ['('], declaration(W), [';'], (condition(X);boolean(X)), [';'], iterator(Y), [')'], block(Z).
forloop(t_forloop(W, X, Y, Z)) --> ['for'], ['('], declaration(W), [';'], (condition(X);boolean(X)), [';'], assignment(Y), [')'], block(Z).
forloop(t_forloop(W, X, Y, Z)) --> ['for'], ['('], assignment(W), [';'], (condition(X);boolean(X)), [';'], iterator(Y), [')'], block(Z).
forloop(t_forloop(W, X, Y, Z)) --> ['for'], ['('], assignment(W), [';'], (condition(X);boolean(X)), [';'], expression(Y), [')'], block(Z).

%to parse for_in_range
forrange(t_for_in_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], num(C), [')'], block(D).
forrange(t_for_in_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], identifier(C), [')'], block(D).
forrange(t_for_in_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], num(B), [':'], identifier(C), [')'], block(D).
forrange(t_for_in_range(A, B, C, D)) --> ['for'], identifier(A), ['in'], ['range'], ['('], identifier(B), [':'], num(C), [')'], block(D).

%to parse if condition
ifcondition(t_if_condition(A, B)) --> ['if'], ['('], (condition(A);boolean(A)), [')'], block(B).
ifcondition(t_if_condition(A, B, C)) --> ['if'], ['('], (condition(A);boolean(A)), [')'], block(B), ['else'], block(C).

%to parse ternary condition
ternarycondition(t_ternary_condition(A, B, C)) --> (condition(A);boolean(A)), ['?'], statements(B), [':'], statements(C).

%to parse the boolean expression 
boolean(true) --> ['true'].
boolean(false) --> ['false'].
boolean(t_bool_NOT(X)) --> ['not'],['('], boolean(X), [')'].
boolean(t_bool_NOT(X)) --> ['not'],['('], condition(X), [')'].
boolean(t_bool_AND(X, Y)) --> boolean(X), ['and'], boolean(Y).
boolean(t_bool_AND(X, Y)) --> condition(X), ['and'], condition(Y).
boolean(t_bool_OR(X, Y)) --> boolean(X), ['or'], boolean(Y).
boolean(t_bool_OR(X, Y)) --> condition(X), ['or'], condition(Y).

%to parse print statements
print(t_print(X)) --> ['print'], identifier(X).
print(t_print(X)) --> ['print'], num(X).
print(t_print(X)) --> ['print'], string(X).

%to parse condition checks
condition(t_condition(X, Y, Z)) --> expression(X), compareoperator(Y), expression(Z).
condition(t_condition(X, Y, Z)) --> string(X), compareoperator(Y), string(Z).
condition(t_condition(X, Y, Z)) --> identifier(X), compareoperator(Y), string(Z).

%to parse comparison operator
compareoperator(==) --> ['=='].
compareoperator('!=') --> ['!='].
compareoperator(>) --> ['>'].
compareoperator(<) --> ['<'].
compareoperator(>=) --> ['>='].
compareoperator(<=) --> ['<='].

%to parse addition ,subtraction,multiplication and division
expression(t_add(X, Y)) --> expression(X), ['+'], term(Y).
expression(t_sub(X, Y)) --> expression(X), ['-'], term(Y).
expression(X) --> term(X).
term(t_mult(X, Y)) --> term(X), ['*'], term(Y).
term(t_div(X, Y)) --> term(X), ['/'], term(Y).
term(X) --> ['('], expression(X), [')'].
term(X) --> num(X).
term(X) --> identifier(X).

%to parse unary increment and decrement operation
iterator(t_incre(X)) --> identifier(X), ['+'], ['+'] .
iterator(t_decre(X)) --> identifier(X), ['-'], ['-'].

%to parse number, identifier, and string
num(t_num(Y)) --> [Y], {number(Y)}.
identifier(identifier(Y)) --> [Y], {atom(Y)}.
string(Y) --> onlystring(Y).
onlystring(t_str(Y)) --> [Y], {atom(Y)}.

check_type(Val, Temp) :- string(Val), Temp = string.
check_type(Val, Temp) :- integer(Val), Temp = int.
check_type(Val, Temp) :- (Val = true ; Val = false), Temp = bool.

not(true, false).
not(false, true).

and(false, _, false).
and(_, false, false).
and(true, true, true).

or(true, _, true).
or(_, true, true).
or(false, false, false).

%lookup predicate to find the corresponding values from the States

lookup(Identifier, [(Category, Identifier, P_holder)|], P_holder).
lookup(Identifier, [_|Tail], P_holder) :- 
    lookup(Identifier, Tail, P_holder).

lookup_category(Identifier, [_|Tail], P_holder) :- 
    lookup_category(Identifier, Tail, P_holder).

lookup_category(Identifier, [(Category,Identifier,X)|], Category).

%update predicate to update the value of  identifier

update(Category, Identifier, Val, [], [(Category, Identifier, Val)]).
update(Category, Identifier, Val, [(Category, Identifier, _)|Tail], [(Category, Identifier, Val)|Tail]).
update(Category, Identifier, Val, [Head|Tail], [Head|Rest]) :- 
    update(Category, Identifier, Val, Tail, Rest).

%to evaluate program
eval_program(t_program(Program), Final_State) :- 
    eval_blk(Program, [], Final_State), !.

%to evaluate different blocks
eval_blk(t_blk(K), State, Final_State) :- 
    eval_blk_section(K, State, Final_State).
eval_blk_section(t_blk(K, L), State, Final_State) :- 
    eval_stms(K, State, S1), 
    eval_blk_section(L, S1, Final_State).
eval_blk_section(t_blk(K), State, Final_State) :- 
    eval_stms(K, State, Final_State).

%to evaluate different statements
eval_stms(t_stms(Statement), State, Final_State) :- 
    eval_declare(Statement, State, Final_State);
    eval_assign(Statement, State, Final_State);
    eval_bool(Statement, State, Final_State, _Val);
    eval_print(Statement, State, Final_State).
    %eval_if(Statement, State, Final_State);
    %eval_while(Statement, State, Final_State);
    %eval_for_loop(Statement, State, Final_State);
    %eval_for_in_range(Statement, State, Final_State);
    %eval_ternary_cond(Statement, State, Final_State);
    %eval_iterate(Statement, State, Final_State).

%to evaluate different types of declarations
eval_declare(t_declare(X, Y), State, New_State):- 
    eval_tree(Y, Identifier),
    update(X, Identifier, _, State, New_State).
eval_declare(t_decint(int, Y, Z), State, New_State):- 
    eval_tree(Y, Identifier),
    eval_expr(Z, State, S1, Val),
    update(int, Identifier, Val, S1, New_State).
eval_declare(t_decstr(string, Y, Z), State, New_State):- 
    eval_tree(Y, Identifier),
    eval_str(Z, State, NewEnv1, Val),
    update(string, Identifier, Val, NewEnv1, New_State).
eval_declare(t_decbool(bool, Y, true), State, New_State):- 
    eval_tree(Y, Identifier),
    update(bool, Identifier, true, State, New_State).
eval_declare(t_decbool(bool, Y, false), State, New_State):- 
    eval_tree(Y, Identifier),
    update(bool, Identifier, false, State, New_State).

    %to evaluate assignment operations
eval_assign(t_assignmnt(X, Y), State, New_State) :- 
    eval_expr(Y, State, S1, Val),
    check_type(Val, T),
    eval_tree(X, Identifier),
    lookup_category(Identifier, S1, T1),
    T =@= T1,
    update(T, Identifier, Val, S1, New_State).
eval_assign(t_assignmnt(X, Y), State, New_State) :- 
    eval_str(Y, State, State, Val),
    check_type(Val, T),
    eval_tree(X, Identifier),
    lookup_category(Identifier, State, T1),
    T =@= T1,
    update(T, Identifier, Val, State, New_State).
eval_assign(t_assignmnt(X, Y), State, New_State) :- 
   eval_bool(Y, State, State, Val),
    check_type(Val, T),
    eval_tree(X, Identifier),
   lookup_category(Identifier, State, T1),
    T =@= T1,
    update(T, Identifier, Val, State, New_State).



%to evaluate boolean conditions
eval_bool(true, _Env1, _NewEnv, true).
eval_bool(false, _Env1, _NewEnv,false).
eval_bool(t_bool_NOT(B), State, New_State, Val) :- 
    (eval_bool(B, State, New_State, V1);eval_cond(B, State, New_State, V1)), 
    not(V1, V2), 
    Val = V2.
eval_bool(t_bool_AND(X, Y), State, New_State, Val) :- 
    eval_bool(X, State, New_State, V1),
    eval_bool(Y, State, New_State, V2),
    and(V1, V2, Val).
eval_bool(t_bool_AND(X, Y), State, New_State, Val) :- 
    eval_cond(X, State, New_State, V1),
    eval_cond(Y, State, New_State, V2), 
    and(V1, V2, Val).
eval_bool(t_bool_OR(X, Y), State, New_State, Val) :- 
    eval_bool(X, State, New_State, V1),
    eval_bool(Y, State, New_State, V2),
    or(V1, V2, Val).
eval_bool(t_bool_OR(X, Y), State, New_State, Val) :- 
    eval_cond(X, State, New_State, V1),
    eval_cond(Y, State, New_State, V2),
    or(V1, V2, Val).

%to evaluate conditional operations
eval_cond(t_condition(X, ==, Y), State, New_State, Val) :- 
    eval_expr(X, State, New_State, V1),
    eval_expr(Y, State, New_State, V2),
    (( V1 =:= V2, Val = true); ( \+(V1 =:= V2), Val = false)).
eval_cond(t_condition(X, '!=', Y), State, New_State, Val) :- 
    eval_expr(X, State, New_State, V1),
    eval_expr(Y, State, New_State, V2),
    (( V1 =\= V2, Val = true);( \+(V1 =\= V2), Val = false)).
eval_cond(t_condition(X, '>', Y), State, New_State, Val) :-
    eval_expr(X, State, New_State, V1),
    eval_expr(Y, State, New_State, V2),
    (( V1 > V2, Val = true);( \+(V1 > V2), Val = false)).
eval_cond(t_condition(X, '<', Y), State, New_State, Val) :- 
    eval_expr(X, State, New_State, V1),
    eval_expr(Y, State, New_State, V2),
    (( V1 < V2, Val = true);( \+(V1 < V2), Val = false)).
eval_cond(t_condition(X, '>=', Y), State, New_State, Val) :- 
    eval_expr(X, State, New_State, V1),
    eval_expr(Y, State, New_State, V2),
    (( V1 >= V2, Val = true);( \+(V1 >= V2), Val = false)).
eval_cond(t_condition(X, '<=', Y), State, New_State, Val) :- 
    eval_expr(X, State, New_State, V1),
    eval_expr(Y, State, New_State, V2),
    (( V1 =< V2, Val = true);( \+(V1 =< V2), Val = false)).
eval_cond(t_condition(X, ==, Y), State, New_State, Val) :- 
    eval_str(X, State, New_State, V1),
    eval_str(Y, State, New_State, V2),
    ((V1 = V2, Val = true);(\+(V1 = V2), Val = false)).
eval_cond(t_condition(X,'!=',Y), State, New_State, Val) :-
    eval_str(X, State, New_State, V1),
    eval_str(Y, State, New_State, V2),
    ((V1 = V2, Val = false);(\+(V1 = V2), Val = true)).
eval_cond(t_condition(X,'>',Y), State, New_State,_Val) :- 
    eval_str(X, State, New_State,_V1),
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,'<',Y), State, New_State,_Val) :- 
    eval_str(X, State, New_State,_V1),
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,'>=',Y), State, New_State,_Val) :- 
    eval_str(X, State, New_State,_V1),
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,'<=',Y), State, New_State,_Val) :- 
    eval_str(X, State, New_State,_V1),
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,==,Y), State, New_State, Val) :-
    eval_tree(X,Identifier),
    lookup(Identifier, State, V1),
    check_type(V1,T),
    T=string,
    eval_str(Y, State, New_State, V2),
    ((V1 =@= V2, Val = true);(\+(V1 =@= V2), Val = false)).
eval_cond(t_condition(X,'!=',Y), State, New_State, Val) :- 
    eval_tree(X,Identifier),
    lookup(Identifier, State, V1),
    check_type(V1,T),
    T=string,
    eval_str(Y, State, New_State, V2),
    ((V1 = V2, Val = false);(\+(V1 = V2), Val = true)).
eval_cond(t_condition(X,'>',Y), State, New_State,_Val) :- 
    eval_tree(X,Identifier),
    lookup(Identifier, State, V1),
    check_type(V1,T),
    T=string,
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,'<',Y), State, New_State,_Val) :- 
    eval_tree(X,Identifier),
    lookup(Identifier, State, V1),
    check_type(V1,T),
    T=string,
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,'>=',Y), State, New_State,_Val) :- 
    eval_tree(X,Identifier),
    lookup(Identifier, State, V1),
    check_type(V1,T),
    T=string,
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").
eval_cond(t_condition(X,'<=',Y), State, New_State,_Val) :- 
    eval_tree(X,Identifier),
    lookup(Identifier, State, V1),
    check_type(V1,T),
    T=string,
    eval_str(Y, State, New_State,_V2),
    write("Operation not allowed").