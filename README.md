# comp30020-2020-Sem2-project-1
The one about Cribbage in Prolog

###Description
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

###Instructions
Can be run using SWI-Prolog by loading the file through [cribbage]. or consult(cribbage).
