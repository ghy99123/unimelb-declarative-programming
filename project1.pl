% Author  : Hanyi Gao <hanyig1@student.unimelb.edu.au>
% Project : Cribbage, COMP90048
% Purpose : Perform the major parts of a cribbage playing program.

/*
In this project, we need to perform part of a card game called Cribbage and
implement two predicates:
1. hand_value(Hand, Startcard, Value):
holds when Hand and Startcard are ground and Value is the points the player
gets for certain combinations of cards based on some rules

2. select_hand(Cards, Hand, Cribcards):
holds when Cards is ground. Hand is a list of the 4 cards remained in player's 
hand that has the maximum expected value, and Cribcards are a list of the cards
that should not kept.
*/


%****************************Part 1-hand_value/3*******************************%
/* 
definition of hand_value/3 
To get the points, we just calculate the points for each rule, and then add them.
The cards will be sorted first, and we also get the frequency of the same suit 
to make the calculation much easier.
Note: 1. get all the 5 cards including 4 Hand cards and 1 start card
      2. get the ranks of the cards
      3. get the frequency of a certain rank in a card list
*/
hand_value(Hand, Startcard, Value) :- 
  all_hand(Hand, Startcard, All),                % [Note 1]
  take_ranks(All, RankListV1, RankListV2),       % [Note 2]
  msort(RankListV1, SortedHand),
  get_card_freq(SortedHand, CardFreqV1),         % [Note 3]
  fifteens(RankListV2, Value1),
  pairs(SortedHand, Value2), 
  runs(CardFreqV1, Value3), 
  flushes(Hand, Startcard, Value4), 
  one_for_his_nob(Hand, Startcard, Value5),
  Value is (Value1 + Value2 + Value3 + Value4 + Value5).


% Gets the list of the 5 cards(4 hand cards + 1 start card). 
all_hand(Hand, Startcard, All) :- append(Hand, [Startcard], All).

/* 
Gets lists of two version that contain the rank of a card list.
The ranks are all numbers. i.e. ace should be seen as 1.
*/
take_ranks([], [], []).
take_ranks([card(Rank, _) | Rest], RankListV1, RankListV2) :- 
    (   integer(Rank)
    ->  take_ranks(Rest, RestV1, RestV2),
        RankListV1 = [Rank | RestV1],
        RankListV2 = [Rank | RestV2]
    ;   convert_v1(Rank, NumV1),
        convert_v2(Rank, NumV2),
        take_ranks(Rest, RestV1, RestV2),
        RankListV1 = [NumV1 | RestV1],
        RankListV2 = [NumV2 | RestV2]
    ).

/*
Convert card rank to a certain number.
There are two versions since for the rule '15s', jack, queen and king should be
seen as the value of 10, while for the other rules, they can be seen as 11, 12
and 13 respectively.
*/
convert_v1(ace, 1).
convert_v1(jack, 11).
convert_v1(queen, 12).
convert_v1(king, 13).

convert_v2(ace, 1).
convert_v2(jack, 10).
convert_v2(queen, 10).
convert_v2(king, 10).

% Get the frequency of a certain rank in a card list
get_card_freq(Lin, Lout) :- get_card_freq(Lin, [], Lout, 1).
get_card_freq([Num], List, Res, Freq) :- 
    append(List, [card_freq(Num, Freq)], Res).
get_card_freq([Num, NextNum | Rest], List, Res, Freq) :-
    (   Num = NextNum
    ->  Freq2 is (Freq + 1),
        get_card_freq([NextNum | Rest], List, Res, Freq2)
    ;   append(List, [card_freq(Num, Freq)], CurList),
        get_card_freq([NextNum | Rest], CurList, Res, 1)
    ).
   

% Rule1: Check how many points are scored based on the rule '15s'.
fifteens(Lin, Value) :- 
    (   bagof(SubScore, fifteens_helper(Lin, SubScore), List),
        List \= []
    ->  sumlist(List, Value)
    ;   Value = 0
    ).
/*
Gets all the combination of cards that add to 15 and the score to each 
combination. In particular, the score to each comination is always 2.
*/
fifteens_helper(Lin, SubScore):-
    subset(Lin, Lout),
    sumlist(Lout, Val),
    Val is 15,
    SubScore is 2.
% Gets the subset Lists of a given list.
subset([], []).
subset([H|T], [H|T1]) :- 
    subset(T,T1).
subset([_|T],T1) :-
    subset(T, T1).


% Rule2: Check how many points are scored based on the rule 'Pairs'.
pairs(SortedNumList, Value) :- pairs(SortedNumList, 1, Value).
pairs([], _, 0).
pairs([Head|Rest], CurLen, Value) :-
    (   Rest = [Head|_]
    ->  Len is (CurLen + 1),
        pairs(Rest, Len, Value)
    ;   pairs(Rest, 1, FormerValue),
        pairs_helper(CurLen, LatterValue),
        Value is (FormerValue + LatterValue) 
    ).
% pairs_helper/2 is a helper term to determine the score for n of a kind,
% where n can be 1, 2, 3 or 4
pairs_helper(1, 0).
pairs_helper(2, 2).
pairs_helper(3, 6).
pairs_helper(4, 12).


% Rule3: Check how many points are scored based on the rule 'Runs'.
runs([card_freq(Num, Freq) | Rest], Value) :-   
    (   runs_helper([card_freq(Num, Freq) | Rest], [card_freq(Num, Freq)], Res),
        length(Res, Len), Len >= 3
    ->  combination([card_freq(Num, Freq) | Rest], N),
        Value is (Len * N)
    ;   Value = 0
    ).
   
runs_helper([card_freq(_, _)], List, Res) :- Res = List.
runs_helper([card_freq(Num, _), card_freq(NextNum, Freq2) | Rest], List, Res) :-
    (   NextNum is (Num + 1)
    ->  append(List, [card_freq(NextNum, Freq2)], CurList),
        runs_helper([card_freq(NextNum, Freq2) | Rest], CurList, Res)
    ;   (   length(List, Len), Len < 3
        ->  runs_helper([card_freq(NextNum, Freq2) | Rest], 
                        [card_freq(NextNum, Freq2)], Res)
        ;   Res = List
        )
    ).

combination([], 1).
combination([card_freq(_, Freq) | Rest], Total) :- 
    combination(Rest, SubTotal),
    Total is (Freq * SubTotal).


% Rule4: Check how many points are scored based on the rule 'Flushes'.
flushes(Hand, Startcard, Value) :- 
    (   same_suit(Hand)    
    ->  append(Hand, [Startcard], All),
        (  same_suit(All)
        ->  Value = 5
        ;   Value = 4
        )
    ;   Value = 0
    ).
% Check if all the cards in a list are of the same suit.
same_suit([_]).
same_suit([card(_, Suit), card(_, Suit) | Cards]) :- 
    same_suit([card(_, Suit) | Cards]).


% Rule5: Check how many points are scored based on the rule 'One for his nob'.
one_for_his_nob(Hand, card(_, Suit), Value) :- 
    (   member(card(jack, Suit), Hand)
    ->  Value = 1
    ;   Value = 0
    ).


%***************************Part 2-select_hand/3*******************************%
/* 
definition of select_hand/3 
To get the set of hand cards that has the highest expected value, we need to get
the expected value of all possible hand cards and simply gets the one that has
the highest score.
*/
select_hand(Cards, Hand, Cribcards) :-
    bagof(expect(PossibleHand, Value), 
          select_hand_helper(Cards, PossibleHand, Value), List),
    max_value(List, Hand, 0, _),
    complement_set(Cards, Hand, Cribcards).

/*
To calculate the expected value, we need to first calculate the value for every
possible hand card and the expected value is the average number of all these values.
*/
select_hand_helper(Cards, PossibleHand, Value) :-
    subset(Cards, PossibleHand),
    length(PossibleHand, 4),
    bagof(Score, expected_value(Cards, PossibleHand, Score), Values),
    average(Values, Value).

% Get the average value of a list of numbers.
average(List, Value) :- 
    length(List, Len), 
    sumlist(List, Val), 
    Value is (Val / Len).

% Get the hand cards that has the maximum expected value
max_value([], Hand, _, CurHand) :- Hand = CurHand.
max_value([expect(PossibleHand, Value) | Rest], Hand, CurMax, CurHand) :-
    ( Value > CurMax
    -> max_value(Rest, Hand, Value, PossibleHand)
    ;  max_value(Rest, Hand, CurMax, CurHand)
    ).

/*
complmet_set(+Set, +Subset, -ComplementSet).
Get the list of elements that belong to Set but does not belong to Subset. 
*/
complement_set(Cards, Hand, Cribcards) :- 
    complement_set(Cards, Hand, Cribcards, []).
complement_set([], _, Cribcards, PreList) :- Cribcards = PreList.
complement_set([E | Rest], Hand, Cribcards, PreList) :-
    (   member(E, Hand)
    ->  complement_set(Rest, Hand, Cribcards, PreList)
    ;   append(PreList, [E], CurList),
        complement_set(Rest, Hand, Cribcards, CurList)
    ).    

/*
Generates all possible start cards and gets the value of hand cards with each 
start card.
*/
expected_value(Cards, PossibleHand, Score) :-
    card_generate(Cards, All),
    member(StartCard, All),
    hand_value(PossibleHand, StartCard, Score).

% Generate a list of all 52 cards.
card_generate(Cards, AllStartCards) :- 
    RankList = [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king],
    SuitList = [clubs, diamonds, hearts, spades],
    card_generate(Cards, AllStartCards, RankList, SuitList, []).
card_generate(_, AllStartCards, [], _, PreList) :- AllStartCards = PreList.
card_generate(Cards, AllStartCards, [Rank | Rest], SuitList, PreList) :-
    fixed_rank_cards(Cards, Rank, SuitList, PreList, CurList),
    card_generate(Cards, AllStartCards, Rest, SuitList, CurList).

/* 
Gernerate cards for fixed rank. e.g. for the rank of ace, we need to generate 
card(ace, clubs), card(ace, diamonds), card(ace, hearts) and card(ace, spades)
and add them to the current list that is generating all the cards.
*/
fixed_rank_cards(_, _, [], PreList, Lout) :- Lout = PreList.
fixed_rank_cards(Cards, Rank, [Suit | Rest], PreList, Lout) :-
    (   member(card(Rank, Suit), Cards)
    ->  fixed_rank_cards(Cards, Rank, Rest, PreList, Lout)
    ;   append(PreList, [card(Rank, Suit)], CurList),
        fixed_rank_cards(Cards, Rank, Rest, CurList, Lout)
    ).





