% FLP Logicky projekt - Rubikova kostka
% Martin Risa  (xrisam00)
% 4.2.0420

% cte radky ze standardniho vstupu, konci na LF nebo EOF 
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_), %atom_codes(C,[Cd]),
		[C|LL] = L).

%testuje znak na EOF nebo LF
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


%rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


%vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

read_rubik(
    [
        % up
        [[C01,C02,C03]],[[C04,C05,C06]],[[C07,C08,C09]],        
        % front,right,back,left
        [[C11,C12,C13],[C51,C52,C53],[C31,C32,C33],[C41,C42,C43]],
        [[C14,C15,C16],[C54,C55,C56],[C34,C35,C36],[C44,C45,C46]],
        [[C17,C18,C19],[C57,C58,C59],[C37,C38,C39],[C47,C48,C49]],
        % down
        [[C21,C22,C23]],[[C24,C25,C26]],[[C27,C28,C29]] 
    ],
    [
        [C01,C02,C03,C04,C05,C06,C07,C08,C09], %up
        [C11,C12,C13,C14,C15,C16,C17,C18,C19], %front
        [C51,C52,C53,C54,C55,C56,C57,C58,C59], %right
        [C31,C32,C33,C34,C35,C36,C37,C38,C39], %back
        [C41,C42,C43,C44,C45,C46,C47,C48,C49], %left
        [C21,C22,C23,C24,C25,C26,C27,C28,C29]  %down
    ]
).

check_solved(
    [
        [C01,C02,C03,C04,C05,C06,C07,C08,C09], %up
        [C11,C12,C13,C14,C15,C16,C17,C18,C19], %front
        [C21,C22,C23,C24,C25,C26,C27,C28,C29], %down
        [C31,C32,C33,C34,C35,C36,C37,C38,C39], %back
        [C41,C42,C43,C44,C45,C46,C47,C48,C49], %left
        [C51,C52,C53,C54,C55,C56,C57,C58,C59]  %right
    ]):-
    C01 == C02, C01 == C03, C01 == C04, C01 == C05, C01 == C06, C01 == C07, C01 == C08, C01 == C09,
    C11 == C12, C11 == C13, C11 == C14, C11 == C15, C11 == C16, C11 == C17, C11 == C18, C11 == C19,
    C21 == C22, C21 == C23, C21 == C24, C21 == C25, C21 == C26, C21 == C27, C21 == C28, C21 == C29,
    C31 == C32, C31 == C33, C31 == C34, C31 == C35, C31 == C36, C31 == C37, C31 == C38, C31 == C39,
    C41 == C42, C41 == C43, C41 == C44, C41 == C45, C41 == C46, C41 == C47, C41 == C48, C41 == C49,
    C51 == C52, C51 == C53, C51 == C54, C51 == C55, C51 == C56, C51 == C57, C51 == C58, C51 == C59.

solve_nice(Cube,Steps,MaxDepth,CurrentDepth) :-
		MaxDepth @>= CurrentDepth,
		(
            (solve_awful(Cube,[],Steps,0,CurrentDepth))
            ;
            (Depth is CurrentDepth + 1,
            solve_nice(Cube, Steps, MaxDepth, Depth))
		).    
   
start :-
    prompt(_, ''),
    read_lines(LL),
    split_lines(LL,S),
    read_rubik(S,Cube),
    
    (check_solved(Cube) -> write_rubik(Cube), halt;
    (solve_nice(Cube, Steps, 20, 1) ->   
    
    write_rubik(Cube),
    write_steps(Steps),
    
    halt; halt)).
                             
solve_awful(Cube,PrevSteps,Steps,Depth,ReachedDepth):-
    Depth =< ReachedDepth,
(
    (
        rotate_clockwise_back(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_counter_clockwise_back(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))    )
    ;
    (
        rotate_clockwise_down(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_counter_clockwise_down(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_clockwise_front(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_counter_clockwise_front(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_clockwise_left(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_counter_clockwise_left(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_clockwise_right(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_counter_clockwise_right(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_clockwise_up(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth))
    )
    ;
    (
        rotate_counter_clockwise_up(Cube,MovedCube),
        append(PrevSteps,[MovedCube],NewSteps),
        (check_solved(MovedCube) -> Steps = NewSteps; 
        NewDepth is Depth + 1,
        (solve_awful(MovedCube,NewSteps,Steps,NewDepth,ReachedDepth) -> true; false)
        )
    )
).

rotate_clockwise_up(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			Down
		],
		[
			[E7,E4,E1,E8,E5,E2,E9,E6,E3],
			[B1,B2,B3,A4,A5,A6,A7,A8,A9],
			[C1,C2,C3,B4,B5,B6,B7,B8,B9],
			[D1,D2,D3,C4,C5,C6,C7,C8,C9],
			[A1,A2,A3,D4,D5,D6,D7,D8,D9],
			Down
		]).

rotate_counter_clockwise_up(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			Down
		],
		[
			[E3,E6,E9,E2,E5,E8,E1,E4,E7],
			[D1,D2,D3,A4,A5,A6,A7,A8,A9],
			[A1,A2,A3,B4,B5,B6,B7,B8,B9],
			[B1,B2,B3,C4,C5,C6,C7,C8,C9],
			[C1,C2,C3,D4,D5,D6,D7,D8,D9],
			Down
		]).

rotate_clockwise_down(
		[
			Up,
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			Up,
			[D1,D2,D3,A4,A5,A6,A7,A8,A9],
			[A1,A2,A3,B4,B5,B6,B7,B8,B9],
			[B1,B2,B3,C4,C5,C6,C7,C8,C9],
			[C1,C2,C3,D4,D5,D6,D7,D8,D9],
			[F7,F4,F1,F8,F5,F2,F9,F6,F3]
		]).

rotate_counter_clockwise_down(
		[
			Up,
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			Up,
			[A1,A2,A3,A4,A5,A6,B7,B8,B9],
			[B1,B2,B3,B4,B5,B6,C7,C8,C9],
			[C1,C2,C3,C4,C5,C6,D7,D8,D9],
			[D1,D2,D3,D4,D5,D6,A7,A8,A9],
			[F3,F6,F9,F2,F5,F8,F1,F4,F7]
		]).

rotate_clockwise_front(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			Back,
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[E1,E2,E3,E4,E5,E6,D3,D6,D9],
			[A7,A4,A1,A8,A5,A2,A9,A6,A3],
			[E7,B2,B3,E8,B5,B6,E9,B8,B9],
			Back,
			[D1,D2,F1,D4,D5,F2,D7,D8,F3],
			[B7,B4,B1,F4,F5,F6,F7,F8,F9]
		]).

rotate_counter_clockwise_front(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			Back,
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[E1,E2,E3,E4,E5,E6,B1,B4,B7],
			[A3,A6,A9,A2,A5,A8,A1,A4,A7],
			[F3,B2,B3,F2,B5,B6,F1,B8,B9],
			Back,
			[D1,D2,E9,D4,D5,E8,D7,D8,E7],
			[D3,D6,D9,F4,F5,F6,F7,F8,F9]
		]).
		
rotate_clockwise_right(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			Left,
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[E1,E2,A3,E4,E5,A6,E7,E8,A9],
			[A1,A2,F3,A4,A5,F6,A7,A8,F9],
			[B7,B4,B1,B8,B5,B2,B9,B6,B3],
			[E9,C2,C3,E6,C5,C6,E3,C8,C9],
			Left,
			[F1,F2,C7,F4,F5,C4,F7,F8,C1]
		]).

rotate_counter_clockwise_right(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			Left,
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[E1,E2,C7,E4,E5,C4,E7,E8,C1],
			[A1,A2,E3,A4,A5,E6,A7,A8,E9],
			[B3,B6,B9,B2,B5,B8,B1,B4,B7],
			[F9,C2,C3,F6,C5,C6,F3,C8,C9],
			Left,
			[F1,F2,A3,F4,F5,A6,F7,F8,A9]
		]).

rotate_clockwise_left(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			Right,
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[C9,E2,E3,C6,E5,E6,C3,E8,E9],
			[E1,A2,A3,E4,A5,A6,E7,A8,A9],
			Right,
			[C1,C2,F7,C4,C5,F4,C7,C8,F1],
			[D7,D4,D1,D8,D5,D2,D9,D6,D3],
			[A1,F2,F3,A4,F5,F6,A7,F8,F9]
		]).

rotate_counter_clockwise_left(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			[A1,A2,A3,A4,A5,A6,A7,A8,A9],
			Right,
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[A1,E2,E3,A4,E5,E6,A7,E8,E9],
			[F1,A2,A3,F4,A5,A6,F7,A8,A9],
			Right,
			[C1,C2,E7,C4,C5,E4,C7,C8,E1],
			[D3,D6,D9,D2,D5,D8,D1,D4,D7],
			[C9,F2,F3,C6,F5,F6,C3,F8,F9]
		]).

rotate_clockwise_back(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			Front,
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[B3,B6,B9,E4,E5,E6,E7,E8,E9],
			Front,
			[B1,B2,F9,B4,B5,F8,B7,B8,F7],
			[C7,C4,C1,C8,C5,C2,C9,C6,C3],
			[E3,D2,D3,E2,D5,D6,E1,D8,D9],
			[F1,F2,F3,F4,F5,F6,D1,D4,D7]
		]).

rotate_counter_clockwise_back(
		[
			[E1,E2,E3,E4,E5,E6,E7,E8,E9],
			Front,
			[B1,B2,B3,B4,B5,B6,B7,B8,B9],
			[C1,C2,C3,C4,C5,C6,C7,C8,C9],
			[D1,D2,D3,D4,D5,D6,D7,D8,D9],
			[F1,F2,F3,F4,F5,F6,F7,F8,F9]
		],
		[
			[D7,D4,D1,E4,E5,E6,E7,E8,E9],
			Front,
			[B1,B2,E1,B4,B5,E2,B7,B8,E3],
			[C3,C6,C9,C2,C5,C8,C1,C4,C7],
			[F7,D2,D3,F8,D5,D6,F9,D8,D9],
			[F1,F2,F3,F4,F5,F6,B9,B6,B3]
]).

write_steps([]).
write_steps([H|T]) :-
	writef("\n"), writef("\n"), write_rubik(H), write_steps(T).
    
write_rubik(
		[	
			[C01,C02,C03,C04,C05,C06,C07,C08,C09], %up
			[C11,C12,C13,C14,C15,C16,C17,C18,C19], %front
            [C51,C52,C53,C54,C55,C56,C57,C58,C59], %right
            [C31,C32,C33,C34,C35,C36,C37,C38,C39], %back
            [C41,C42,C43,C44,C45,C46,C47,C48,C49], %left
			[C21,C22,C23,C24,C25,C26,C27,C28,C29]  %down
		]) :-
	writef("%w%w%w\n%w%w%w\n%w%w%w\n", [C01,C02,C03,C04,C05,C06,C07,C08,C09]), %up
	writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [C11,C12,C13,C51,C52,C53,C31,C32,C33,C41,C42,C43]),
	writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [C14,C15,C16,C54,C55,C56,C34,C35,C36,C44,C45,C46]),
	writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [C17,C18,C19,C57,C58,C59,C37,C38,C39,C47,C48,C49]),
	writef("%w%w%w\n%w%w%w\n%w%w%w", [C21,C22,C23,C24,C25,C26,C27,C28,C29]). %down
	
	