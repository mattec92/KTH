txt_file_pre(F, N) :-
    decompose_file_name(F, _, P, '.txt'),
    name(P, N).

valid_proof_file(F) :-
    txt_file_pre(F, N),
    prefix("valid", N).

invalid_proof_file(F) :-
    txt_file_pre(F, N),
    prefix("invalid", N).

filter(_,[],[]).
filter(P, [A0|As0], As) :-
    (
        call(P, A0) -> As = [A0|As1]
    ;
        As = As1
    )
    , filter(P, As0, As1).

run_all_tests(ProgramToTest) :-
    catch(consult(ProgramToTest),
          B,
          (write('Could not consult \"'), write(ProgramToTest),
           write('\": '), write(B), nl, halt)),
    working_directory(D),
    directory_files(D, L),
    filter(valid_proof_file, L, V),
    all_valid_ok(V),
    filter(invalid_proof_file, L, I),
    all_invalid_ok(I),
    halt.

all_valid_ok([]).
all_valid_ok([Test | Remaining]) :-
    write(Test), 
    (verify(Test), write(' passed');
    write(' failed. The proof is valid but your program rejected it!')),
    nl, all_valid_ok(Remaining).

all_invalid_ok([]).
all_invalid_ok([Test | Remaining]) :-
    write(Test), 
    (\+verify(Test), write(' passed');
    write(' failed. The proof is invalid but your program accepted it!')),
    nl, all_invalid_ok(Remaining).
