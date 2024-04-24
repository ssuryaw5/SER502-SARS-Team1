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