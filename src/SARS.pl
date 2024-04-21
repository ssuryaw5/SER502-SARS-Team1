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