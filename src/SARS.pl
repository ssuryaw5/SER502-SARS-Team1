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
statements(t_stms(Statement)) --> printstatements(Statement), [;].
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