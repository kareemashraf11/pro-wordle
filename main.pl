main:-
	write("Welcome to Pro-Wordle"),nl,
	write("----------------------"),nl,nl,
	build_kb,
	play.
	
build_kb:-
	write("Please enter a word and its category on separate lines:"),
	nl,
	read(Word),
	(Word=done,nl,write("Done building the words database..."),nl;read(Category),assert(word(Word,Category)),build_kb).
	
play:-
	write("The available categories are: "),
	categories(List),
	write(List),
	nl,
	get_category(Category),
	get_length(Len),
	Guesses is Len+1,
	write("Game started. You have "),
	write(Guesses),
	write(" guesses."),
	nl,
	pick_word(Word,Len,Category),
	guess(Guesses,Len,Word).
	
guess(Guesses,L,W):-
	Guesses==1,
	read(Word),
	((Word=W,write("You won!"));write("You lost!")).
	
guess(Guesses,L,W):-
	Guesses>1,
	nl,
	write("Enter a word composed of "),
	write(L),
	write(" letters:"),
	nl,
	read(Word),
	(
	Word=W, write("You Won!")
	;
	Guesses>1,
	word(Word,_),
	atom_length(Word,L),
	atom_chars(Word,L1),
	atom_chars(W,L2),
	correct_letters(L1,L2,Res),
	correct_positions(L1,L2,Res2),
	write("Correct letters are:  "),
	write(Res),
	nl,
	write("Correct letters in correct positions are:  "),
	write(Res2),
	nl,
	Guesses1 is Guesses-1,
	write("Remaining Guesses are "),
	write(Guesses1),
	nl,
	guess(Guesses1,L,W)
	;
	\+atom_length(Word,L),
	write("Word is not composed of 5 letters. Try again."),
	nl,
	write("Remaining Guesses are "),
	write(Guesses),
	nl,
	guess(Guesses,L,W)
	;
	\+word(Word,_),
	write("Word is not in the knowledge base. Try again."),
	nl,
	write("Remaining Guesses are "),
	write(Guesses),
	nl,
	guess(Guesses,L,W)
	).

is_category(Category):-
	word(_,Category).

categories(List):-
	add_categories([],List).
add_categories(Acc,List):-
	word(_,Category),
	\+member(Category,Acc),
	append(Acc,[Category],Acc1),
	add_categories(Acc1,List),
	!.
add_categories(Acc,List):-
	List=Acc,
	!.
	
available_length(Len):-
	word(Word,_),
	atom_length(Word,Len),
	!.
	
pick_word(Word,Len,Category):-
	word(Word,Category),
	atom_length(Word,Len).

correct_letters(L1,L2,CL):-
	intersect(L1,L2,CL).
intersect(L1,L2,L3) :-
    helper(L1,L2,[],L3).
helper([],_,L3,L3).
helper([X1|T1],L2,A,L3) :-
    member(X1,L2),
	\+member(X1,A),
	append(A,[X1],A1),
    helper(T1,L2,A1,L3),!.
helper([_|T1],L2,A,L3) :-
    helper(T1,L2,A,L3),!.

correct_positions([],_,[]).
correct_positions([H|T1],[H|T2],[H|T3]):-
    correct_positions(T1,T2,T3).
correct_positions([H1|T1],[H2|T2],PL):-
	H1\=H2,
	correct_positions(T1,T2,PL).

get_category(Category):-
	write("Choose a category:"),
	nl,
	read(C),
	categories(List),
	(member(C,List),Category=C;write("This category does not exist."),nl,get_category(Category)).
	
get_length(Len):-
	write("Choose a length:"),
	nl,
	read(L),
	(available_length(L),Len=L;write("There are no words of this length."),nl,get_length(Len)).