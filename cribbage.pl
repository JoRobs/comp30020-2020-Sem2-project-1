/*
Name:         Joshua Robinson
StudentID:    999565
This file provides some useful tools for the game of Cribbage, including a 
predicate for determining hand values, and a predicate for choosing hands.

This file contains 2 important predicates, hand_value/3 and select_hand/3, 
their modes being hand_value(+Hand,+Startcard,-Value) and 
select_hand(+Cards,-Hand,-Cribcards), which can be used to improve a user's 
abilities in a game of Cribbage. The author assumes the reader is aware of the 
game of Cribbage and it's rules. 

hand_value/3 can be used to calculate, given a valid hand in Cribbage and a 
valid Startcard, the value of that hand during the "Show". It does so by 
calculating the points scored by 5 methods, 15s, card pairs, card runs, flushes 
and "One for his nob". To facilitate this, the Card Utilities section of the 
file provides predicates for card ranks, values and counts in a list of cards.
The Value is the sum of points scored by each of the 5 methods.

select_hand/3 can be used to determine the 4 card hand with the highest 
expected value from a set of more than 4 cards, Hand is the set of 4 cards to 
use as the hand and Cribcards is the rest. It does this by enumerating every 
possible 4 card combination from the set of cards, then calculates the hand 
value of that set given every possible startcard, the average of these values 
is the expected value. The hand with the highest expected value is then chosen.
*/

/*
------------------------------------Types-----------------------------------
Primary structures used
----------------------------------------------------------------------------
*/

%Represents a standard playing card's suit
suit(diamonds).
suit(clubs).
suit(spades).
suit(hearts).

%Represents a standard playing card's rank
rank(ace).
rank(2).
rank(3).
rank(4).
rank(5).
rank(6).
rank(7).
rank(8).
rank(9).
rank(10).
rank(jack).
rank(queen).
rank(king).

%Represents a standard playing card
card(R, S):- rank(R), suit(S).

/*
------------------------------Major Predicates------------------------------
The functionality of the file
----------------------------------------------------------------------------
*/

/*
hand_value(+Hand, +Startcard, -Value) Holds when Value is the number of points
a particular Hand and Startcard combination would receive during the show phase
of cribbage. Hand is a list of card/2 and Startcard is a card/2 while Value is
an Integer, the Hand is expected to be 4 cards long, although higher numbers of 
cards still work, although the flushes calculation checks if every card is of 
the same suit.
*/
hand_value(Hand, Startcard, Value):-
    card_vals([Startcard|Hand], Vals),
    msort(Vals, SortedVals),
    get_15(SortedVals, P15),
    card_ranks([Startcard|Hand], Ranks),
    msort(Ranks, SortedRanks),
    val_counts(SortedRanks, Rank_Counts),
    get_pair(Rank_Counts, PPair),
    get_run(Rank_Counts, PRun),
    get_flushes(Hand,Startcard, PFlush),
    get_nob(Hand, Startcard, PNob),
    Value is P15 + PPair + PRun + PFlush + PNob.

/*
get_15s(+Vals, -Points) Holds when Points is the number of points Cards would 
recieve from the 15s rule of cribbage
*/
get_15(Vals, Points):-
    findall(Comb, add_to_n(Vals, Comb, 15), CombList),
    length(CombList, Count),
    Points is Count * 2.
    

/*
get_pairs(+Val_Counts, -Points) Holds when Points is 2 times the number of pair 
combinations in Val_Counts. Val_Counts is a list of Value-Count pairs where 
Value is the value of the rank of a card determined by rank_val/2 and count is 
the number of times that value appear in the hand. Assumes Count =< 4.
*/
get_pair(Val_Counts, Points):-
    get_pair_aux(Val_Counts, 0, N),
    Points is N*2.
get_pair_aux([], N, N).
get_pair_aux([_-C|T], A0, N):-
    A1 is A0 + ((C-1)*(C) // 2),
    get_pair_aux(T, A1, N).

/*
get_run(+Rank_Counts, -PRun) Holds when PRun is the number of points scored for
runs in a game of cribbage. Rank_Counts is a list of Rank-Count pairs for a 
hand of cards, where Rank is the numerical orderering of cards 
based on their value, and Counts are the number of times that rank appears in 
the hand. Assumes Rank_Counts is sorted by Rank. 

Will return the run that scores the most points, not the run that is longest. 
assuming the hand is at most 5 cards, this isn't an issue as there is only ever 
one run per hand.
*/
get_run([(R-C)|Rs], P):-
    R0 is R - 1,
    bagof(P, get_run_aux([(R-C)|Rs], R0, 0, 1, P), Ps),
    max_member(P, Ps).

get_run_aux([], _, L, M, T):-
    L >= 3,
    T is L * M;
    L < 3,
    T=0.
get_run_aux([(R1-C)|Rs], R01, L0, M0, T):-
    (R1 is R01 + 1
        ->  L1 is L0 + 1,
            M1 is M0 * C,
            get_run_aux(Rs, R1, L1, M1, T)
        ;   R02 is R1-1,
            (get_run_aux([], R02, L0, M0, T);
            get_run_aux([(R1-C)|Rs], R02, 0, 1, T))
    ).
    
    
/*
get_flushes(+Startcard, +Hand, -Points) Holds when Points is the number of 
points obtained from flushes in the game of cribbage given a starting card 
and hand. Startcard is a card/2 and Hand is a list of card/2.
*/
get_flushes([card(_, S)|H], Startcard, P):-
    get_flushes_aux(H, Startcard, S, P).

/*
Points are hard coded, 4 points for hand flush,
5 points total for startcard as well
*/
get_flushes_aux([],card(_, S), S, 5).
get_flushes_aux([],card(_, S1), S2, 4):- \+ S1 = S2.
get_flushes_aux([card(_,S)|H], Start, S, P):-get_flushes_aux(H, Start, S, P).
get_flushes_aux([card(_,S1)|_], _, S2, 0):- \+ S1 = S2.


/*
get_nob(+Cards, +Startcard, -Points) Holds when Cards contains a jack of the 
same suit as Startcard and Points is 1, otherwise when Points is 0.
*/
get_nob([], _, 0).
get_nob([card(R, S)|T], card(Startcard_Rank, Startcard_Suit), P):-
    (S = Startcard_Suit, R=jack
        ->  P=1
        ;   get_nob(T, card(Startcard_Rank, Startcard_Suit), P)
    ).

/*
select_hand(+Cards, -Hand, -Cribcards) Cards is a list of card/2, 
determines all possible Startcards, then finds all possible 4 card Hands of 
Cards, determines the hand value for each Hand-Startcard combination and 
calculates the average per hand, chooses the hand with highest average value as 
Hand and puts the rest of Cards into Cribcards
*/
select_hand(Cards, Hand, Cribcards):-
    setof(card(R,V), card(R,V), Allcards),
    subtract(Allcards, Cards, Startcards),
    setof(Hand-Value, 
          get_exp_hand_val(Cards, Startcards, Hand, Value), 
          All_Hands),
    unzip(All_Hands, Hands, Values),
    max_member(Max, Values),
    get_first_index(Max, Values, MaxIndex),
    nth0(MaxIndex, Hands, Hand), 
    subtract(Cards, Hand, Cribcards).

get_exp_hand_val(Cards, Startcards, Hand, ExpValue):-
    comb_of_len(4, Cards, Hand), 
    findall(Value, 
            (member(Startcard, Startcards), hand_value(Hand, Startcard, Value)),
            Values),
    sumlist(Values, Sum),
    length(Values, NHands),
    ExpValue is Sum/NHands.
/*
-----------------------------General Utilities------------------------------
Predicates that work generally
----------------------------------------------------------------------------
*/

/*
add_to_n(+L1, -L2, +A, +N) Holds when the sum of elements in L2 is N, 
and elements in L2 are a combination of elements in L1.
*/
add_to_n(L1, L2, N):-add_to_n_aux(L1,L2,0,N).
add_to_n_aux([],[],N,N).
add_to_n_aux([X|T1], [X|T2], A0,N):-
    A1 is A0+X,
    A1 =< N,
    add_to_n_aux(T1, T2, A1, N).
add_to_n_aux([_|T1],T2, A0,N):-
    add_to_n_aux(T1, T2, A0, N).

/*
comb_of_len(+N, +L1, -L2) Holds when elements in the list L2 are a combination 
of elements in the list L1 of length N.
*/
comb_of_len(N, L1, L2):-
    comb_of_len_aux(N, L1, [], L2).
comb_of_len_aux(N, [], L,L):-
    length(L,N).
comb_of_len_aux(N, [H|T], AL, L):-
    (length(AL, N)
    ->  L=AL
    ;   comb_of_len_aux(N, T, [H|AL],L);
        comb_of_len_aux(N, T, AL,L)
    ).

/*
get_first_index(+E, +L, -I) Holds when I is the index of the first occurrance 
of E in list L, fails if E is not present in L
*/
get_first_index(E, L, I):-
    get_first_index_aux(E, L, 0, I).
get_first_index_aux(E, [E|_], I, I).
get_first_index_aux(E, [H|T], A0, I):-
    E =\= H, A1 is A0+1,
    get_first_index_aux(E, T, A1, I).

/*
val_counts(+List, -Counts) holds when Counts is a list of element-count 
pairs of the elementsof List. Assumes equivalent elements in list are adjacent.
*/
val_counts([H|T], Counts):-
    val_counts_aux([H|T], H, 0, Counts).
val_counts_aux([], Prev, A, [Prev-A]).
val_counts_aux([Prev|Es], Prev, A0, Counts):-
    A1 is A0+1,
    val_counts_aux(Es, Prev, A1, Counts).
val_counts_aux([E|Es], Prev, A, [Prev-A|Counts]):-
    E =\= Prev,
    val_counts_aux(Es, E, 1, Counts).

/*
unzip(?L1, ?L2, ?L3) Holds when L1 is a list where the nth element is E2-E3 
where E2 is the nth element in L2 and E3 is the nth element in L3 for all n 
less than the length of either list. L1, L2 and L3 must be the same length.
*/
unzip([], [], []).
unzip([P-R|Ls], [P|Ps], [R|Rs]):-unzip(Ls, Ps, Rs).

/*
--------------------------------Card Utilities------------------------------
Cribbage specific predicates
----------------------------------------------------------------------------
*/


%Represents the value of a Rank when calculating 15s in hand_value/3
rank_val(ace, 1).
rank_val(jack, 10).
rank_val(queen, 10).
rank_val(king, 10).
rank_val(Rank, Rank):-integer(Rank).

%Represents the ordering of Ranks for Runs in hand_value/3
rank_order(ace, 1).
rank_order(2,2).
rank_order(3,3).
rank_order(4,4).
rank_order(5,5).
rank_order(6,6).
rank_order(7,7).
rank_order(8,8).
rank_order(9,9).
rank_order(10,10).
rank_order(jack, 11).
rank_order(queen, 12).
rank_order(king, 13).


/*
card_vals(+Cards, -Vals) Holds when Vals is the list of the integer 
values of Cards
*/
card_vals([],[]).
card_vals([card(V1,_)|T1], L):-
    rank_val(V1,V2),
    L=[V2|T2],
    card_vals(T1,T2).

/*
card_ranks(+Cards, -Ranks) Holds when Ranks is the list of the integer 
ranks of Cards
*/
card_ranks([],[]).
card_ranks([card(R,_)|T1], L):-
    rank_order(R,RO),
    L=[RO|T2],
    card_ranks(T1,T2).