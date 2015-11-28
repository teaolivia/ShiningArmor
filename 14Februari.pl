/* facts */

:-dynamic(room/1).
room(X,[bedroom,castle,armory,dragon_treasury]).

side_to_side(bedroom,castle).
side_to_side(castle,bedroom).
side_to_side(castle,armory).
side_to_side(armory,castle).
side_to_side(armory,dragon_treasury).
side_to_side(dragon_treasury,armory).

:-dynamic(satriadi/1).
satriadi(castle).

:-dynamic(onroom/2).
onroom(bed,bedroom).
onroom(armor,castle).
onroom(shield,castle).
onroom(maps,castle).
onroom(desk,armory).
onroom(sword,armory).
onroom(princess,dragon_treasury).

left(bedroom,castle).
left(castle,armory).
left(armory,dragon_treasury).

swordsharp(no).

/* change inventory from list to dynamic facts */
:-dynamic(inventory/1).
inventory(maps).

:-dynamic(take/1).

/* rule */
start:- nl,write('Welcome to Tio''s World where everything is made up and nothing holds an importance!'), nl,
		write('Your job is to find Princess for Tio the Knight in Shining Armor by exploring this nonsense (and very small) world!'), nl,
		write('You can explore by using command:'), nl,
		write('- look'),nl,
		write('- sleeping'),nl,
		write('- readmap'),nl,
		write('- goto(place)'),nl,
		write('- take(object)'),nl,
		write('- sharpen(object)'),nl,
		write('- quit'),nl,nl,
		command_loop.
		/* fixed start */

command_loop:-
  repeat,
  get_command(X),
  do(X),
  
  (princessfound; X == quit).
/* added "command_loop" rule for looping the prolog to receive input until the user type quit. */

do(look) :- look,!.
do(sleeping) :- sleeping,!.
do(readmap) :- readmap,!.
do(goto(X)) :- goto(X),!.
do(take(X)) :- take(X),!.
do(sharpen(X)) :- sharpen(X),!.
do(quit) :- quit,!.
/* added "do" rule to for respective rule */


princessfound :- 	have(princess),
					write('Congratulation Tio has found his true love'),nl.
/* the condition to quit the loop */


look :- satriadi(X),
		respond(['You are in ',X]),
		write('You can see: '),
		forall(onroom(Y,X), writespace(Y)), nl,
		write('Your inventory: '),
		forall(inventory(Z), writespace(Z)), nl,nl.
		/* fixed look */

sleeping :- satriadi(bedroom),
			write('Have a good night O Tio, Knight in Shining Armor'),nl.
			/* fixed sleeping */
sleeping :- satriadi(X), respond(['You can''t sleep in ',X]).


have(X) :- 	inventory(X).
			/* check whether X exist in inventory */

readmap :- (have(maps) -> write('You open the wonderful map and see whats inside'),nl,
			write('dragon_treasury | armory | castle | bedroom | '),nl,nl;
			write('You can''t read map because you don''t have it'),nl,nl).
			/* fixed readmap */

goto(X) :- 	room(X),
			satriadi(Y),
			can_go(X,Y),
			retract(satriadi(Y)), asserta(satriadi(X)),
			look.
			/* fixed goto */

can_go(X,Y) :-	side_to_side(X,Y),!.
can_go(X,Y) :-	respond(['You can''t get to ',X,' from ',Y]),fail.
				/* can_go used to do a checking when goto */

sharpen(X) :-	X == sword, have(sword),!.
sharpen(X) :-	write('You can''t sharpen it'), fail.
				/* sharpen return yes setiap kali variabel sword dimasukkan */
				/* untuk variabel lain selian sword, sudah bisa */

take(X) :-	onroom(X, Room),
			satriadi(Room),
 	    	retract(onroom(X, Room)),
 			assertz(inventory(X)),
        	nl. 
        	/* fixed take X and put it in inventory */

take(X) :-	X = princess,
			room(X,[H|T]),
			satriadi(dragon_treasury),
			swordsharp(yes),
			have(shield),
			have(armor),
			write('Congratulation Tio has found his true love').
			/* take(princess) not working in dragon_treasury */
			/* work on the other location */

inventory :- 	have(X),
				write('Your inventory: '),nl,
				list_inventory.
inventory :-	have(X),
				write(' '),nl.

list_inventory :- 	have(X),
					tab(2),write(X),nl,
					fail.
list_inventory.

get_command(C):-
  readlist(L), 
  command(X,L,[]),    % call the grammar for command       
  C =.. X,!.          % make the command list a structure
get_command(_):-
  respond(['I don''t understand, try again or type help']),fail.

command([Pred,Arg]) --> verb(Type,Pred), noun(Type,Arg).
command([Pred]) --> verb(intran,Pred).

respond([]) :-	write('.'),nl,nl.
respond([H|T]) :-	write(H),
					respond(T).

verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

tran_verb(take) --> [take].
tran_verb(sharpen) --> [sharpen].

intran_verb(inventory) --> [inventory].
intran_verb(look) --> [look].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(sleeping) --> [sleeping].
intran_verb(readmap) --> [readmap].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> noun(Type,Noun).


noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C2) :-         % if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until 
  restword(C1,Cs,C2),       % we have all the word     
  name(W, [NewC|Cs]).       % then make it an atom
readword(C,W,C2) :-         % otherwise
  get0(C1),       
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).

in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).

quit :- write('So it turns out you''re not the chosen one'),
		halt.
		/* if terminate, exit from prolog */

writespace(X) :- write(X), tab(1).
				/* for writing with white space */

isroom(X) :- member(X,room([H,T])).
			/* for returning list of room member(s) */