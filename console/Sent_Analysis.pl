% Predicate for the program interface
sentiment_analysis_interface :-
	nl,writeln(" ############  ############  ############"),
	writeln("  	|P|r|o|l|o|g| |S|e|n|t|i|m|e|n|t| |A|n|a|l|y|s|i|s|   "),
	writeln(" ############  ############  ############ "),
    repeat,
    nl,
    write('1. Analyze a sentence'),
    nl,
    write('2. Documentation'),
	nl,
    write('3. Exit'),
    nl,
    write('Enter your choice (1-3): '),
    get_single_char(Choice),
    process_choice(Choice).

% Predicate to process the user's choice
process_choice(49) :-  % ASCII code for '1'
    nl,
	write('############  ############  ############'),
	nl,nl,
    write('Enter a sentence: '),
    read_line(Sentence),
    nl,
    write('Analyzing the sentence...'),
    nl,
    % Perform sentiment analysis on the sentence
    analyze_sentence(Sentence),
	!,  % Cut to prevent backtracking
    nl,
    write('Press Enter to continue...'),
    read_line(_),
    nl,
    sentiment_analysis_interface.


process_choice(50) :-  % ASCII code for '2'
	nl,
	cd('c:/users/utente/desktop/uni/computer science'),
	print_file_content('cons_doc.txt'),
	nl,
	write('Press Enter to continue...'),
    read_line(_),
    nl,
    sentiment_analysis_interface.


process_choice(51) :-  % ASCII code for '3'
    nl,
    write('Exiting...'),
    nl,
    !.
	
	
print_file_content(File) :-
    open(File, read, Stream),
    read_and_print_lines(Stream),
    close(Stream).

read_and_print_lines(Stream) :-
    \+ at_end_of_stream(Stream),
    read_line_to_string(Stream, Line),
    writeln(Line),
    read_and_print_lines(Stream).

read_and_print_lines(_).  % Base case to stop recursion when the end of the file is reached.



% Predicate to read a line of input as a list of characters
read_line(Line) :-
    read_line_to_codes(user_input, Codes),
    atom_codes(Atom, Codes),
    atom_chars(Atom, Line).

% Predicate to perform sentiment analysis on the sentence
analyze_sentence(Sentence) :-

    % Split the sentence into words
    split_string(Sentence, " ", "", Words),
	% Display the lists
    write('Original list of words: '),
    write(Words),
    nl,
	

	% IF Words IS EMPTY 
	(   Words = [] 
    ;   Words = ['']  % List contains only one element being 'space'
    ;   Words = [""]  % List contains only one element being 'enter'
    ->  write('Please enter at least one word.')
	; 
	
	(
    % Remove stopwords from the list of words
    remove_stopwords(Words, FilteredWords),
    write('List of words without stopwords: '),
    write(FilteredWords),
    nl,
	
	
	% Retrieve the list labels for each word
    get_labels(FilteredWords, Labels),
    write('List of labels: '),
    write(Labels),
    nl,
	
	% Apply the intensifiers to the list of labels
    apply_intensifier(Labels, LabelWeights, NewLabels),
	merge_intensifier(LabelWeights,WeightsAfterInt),
	write('List of labels after intensifiers: '),
    write(NewLabels),
    nl,
	write('List of weights after intensifiers: '),
    write(WeightsAfterInt),
    nl,
	
	% Remove the intensifiers from the list of words
    remove_intensifier(FilteredWords,Labels, NoIntWords),
	write('List of words after intensifiers: '),
    write(NoIntWords),
    nl,
	
	% Calculate the neighbours influence
	label_scores(NewLabels,WordNei),
	write('List of weights influenced: '),
    write(WordNei),
    nl,
	
	% Retrieve the list of Pos-Neg scores for each word
    get_scores(NoIntWords, Scores),
    write('List of scores: '),
    write(Scores),
    nl,
	
	
	% METTERE CONTROLLO CHE SE Scores è vuoto viene restituito 0 e / o detto che non è riconosciuta come parola
	
	
	% Calcultate scores based upon the neighbours influence
	calculate_scores(Scores, WordNei, NeighbScores),
	write('List of scores calculated after neighbours: '),
	write(NeighbScores),
	nl,
	
	
	
	
	% Multiply weights and new scores
	scoresANDweights(NeighbScores,WeightsAfterInt,MultipliedScores),
	write('List of scores multiplied with intensifier and negated weights: '),
	write(MultipliedScores),
	nl,
	
	
	% Apply negations to the remaining words
	apply_negations(WeightsAfterInt,MultipliedScores,NegatedScores),
	write('List of scores after negations with intensifier and negated weights: '),
	write(NegatedScores),
	nl,
	
	% Compute final score
	final_score(NegatedScores,FinalScore),
	write('Final Score: '),
	write(FinalScore),
	nl,
	
	printSentiment(FinalScore))).
	
	
	% A possible thing to do will be to repeat this step of applying intensifiers and remove 
	% it from the list another time, but it need to update the code

	

	
	
	


% Predicate to remove stopwords from a list of words
remove_stopwords([], []).
remove_stopwords([Word|Rest], Filtered) :-
    atom_chars(Atom, Word),
    stopword(Atom),
    !,
    remove_stopwords(Rest, Filtered).
remove_stopwords([Word|Rest], [Word|FilteredRest]) :-
    remove_stopwords(Rest, FilteredRest).

% Predicate to retrieve scores for a list of words
get_scores([], []).
get_scores([Word|Words], [Scores|Result]) :-
    get_scores_single(Word, Scores),
    get_scores(Words, Result).

% Predicate to convert to lo lower case
to_lowercase(Word, LowercaseWord) :-
    atom_codes(Word, Codes),
    maplist(to_lower, Codes, LowercaseCodes),
    atom_codes(LowercaseWord, LowercaseCodes).

to_lower(Upper, Lower) :-
    Upper >= 65, Upper =< 90,
    !,
    Lower is Upper + 32.
to_lower(Other, Other).

% Predicate to retrieve scores for a single word
get_scores_single(Word, Scores) :-
    to_lowercase(Word, LowercaseWord),
    findall(PositiveScore-NegativeScore,
        (synset(_, PositiveScore, NegativeScore, Words), member(LowercaseWord, Words)),
        Scores).


% Predicate to retrieve labels for a list of words
get_labels([], []).
get_labels([Word|Words], [Label|Result]) :-
    atom_chars(Atom, Word),
    get_label(Atom, Label),
    get_labels(Words, Result).

% Predicate to retrieve a label for a single word
get_label(Word, Label) :-
    indicator(Word, pos),
    Label = 'pos',
    !.
get_label(Word, Label) :-
    indicator(Word, neg),
    Label = 'neg',
    !.
get_label(Word, Label) :-
    negation(Word),
    Label = 'n',
    !.
get_label(Word, Label) :-
    intensifier(Word, Score),
    atomic_list_concat(['in-', Score], Label),
    !.
get_label(_, 'neut').



% Predicate to apply intensifier to the list of label
apply_intensifier([], [], []).
apply_intensifier(['n' | Rest], [0 | IntensifiedRest],['n' | NoIntRest]) :-
    apply_intensifier(Rest, IntensifiedRest, NoIntRest).
apply_intensifier([X | Rest], IntensifiedLabels , NoIntLabels) :-
    (
        member(X, ['pos', 'neg', 'neut']),
		apply_intensifier(Rest, RemainingIntensified, RemainingNoInt),
		IntensifiedLabels = [1 | RemainingIntensified],
		NoIntLabels = [X | RemainingNoInt]
    ;
        atom_concat('in-', _, X),
        atom_concat('in-', NumberAtom, X),
        atom_number(NumberAtom, Number),
		apply_intensifier(Rest, RemainingIntensified, NoIntLabels),
        IntensifiedLabels = [Number | RemainingIntensified]
    ).




% Predicate to merge word and intensifier
merge_intensifier([], []).
    
merge_intensifier([X | Rest], Merged) :-
    (   
		(	
			X = 1,
		
		(
			Rest = [Y | _] ->				
				(
					% The next element is not the last in the list
					Y \= 1,Y \= 0,	% Next element is an intensifier
					merge_intensifier(Rest, RemainingMerge),
					Merged = [Y | RemainingMerge]
					;
					(Y = 0;Y = 1),
					merge_intensifier(Rest, RemainingMerge),
					Merged = [1 | RemainingMerge]
				)
				;
				
				(
					% The next element is the last in the list
					Rest \= 1,Rest \= 0,Rest \= [],	% Last element is an intensifier
					Merged = Rest 
					;
					Rest = 1,
					Merged = [1,1]
					
					;
					Rest = 0,
					Merged = [1,0]
					
				)
				
		);
			X = 1,
			Rest = [],
			Merged = [1]	
    )
	;
        (	X = 0,
				
		(
			Rest = [_ | _] ->
				(	merge_intensifier(Rest, RemainingMerge),
					Merged = [0 | RemainingMerge]			
				)
				;
				(
					% The next element is the last in the list
					Rest \= 1,Rest \= 0,	% Last element is an intensifier
					Merged = 0 
					;
					Rest =1,
					Merged = [0,1]
					;
					Rest = 0,
					Merged = [0,0]
				)
		);
		X = 0,
		Rest = [],
		Merged = [0]
		)
    ;
        (	(X \= 1, X \= 0),
		
		(
			Rest = [Y | Rest1] ->
				(	

					% The next element is not the last in the list
					Y = 1,	% Next element is 1 
					merge_intensifier(Rest1, RemainingMerge),
					Merged = [X | RemainingMerge]
					;
					Y=0,
					merge_intensifier(Rest1, RemainingMerge),
					Merged = [0 | RemainingMerge]
					;
					Y \= 1, Y \= 0,
					merge_intensifier(Rest, Merged)
				)
				;
				(
					% The next element is the last in the list
					Rest = 1,	% Next element is 1 
					Merged = X 
					
					;
					Rest = 0,
					Merged = 0 
					
					;
					Merged = []		
				
	
				)			
		);
		(X \= 1, X \= 0),
		Rest = [],
		Merged = []
    )
	).






% Predicate to remove intensifiers from the list of words
remove_intensifier([], [], []).
remove_intensifier([Word | Words], [Label | Labels], FilteredWords) :-
    (   atom_concat('in-', _, Label) ->
        remove_intensifier(Words, Labels, FilteredWords)
    ;
        FilteredWords = [Word | RemainingFilteredWords],
        remove_intensifier(Words, Labels, RemainingFilteredWords)
    ).



% Predicate to compute the "influence" of neighbours to each word
check_label('n', -1) :-!.
check_label('pos', 1) :- !.
check_label('neg', -1) :- !.
check_label('neut', 0) :- !.
%check_label(Label, -1) :- sub_atom(Label, 0, 4, _, 'neg-'), !.
%check_label(Label, 1) :- sub_atom(Label, 0, 4, _, 'pos-'), !.
check_label(_, 0):-!.


compute_scores([], []) :- !.
compute_scores([Label | RestLabels], Scores) :-
    compute_scores([Label | RestLabels], 0, Scores).

compute_scores([], _, []) :- !.
compute_scores([Label], PrevScore, [[PrevScore, CurrScore, NextScore]]) :-
    (
        check_label(Label, CurrScore) -> 
		true;
        Label = 0
    ),
    NextScore = 0.

compute_scores([Label | RestLabels], PrevScore, [[PrevScore, CurrScore, NextScore] | RestScores]) :-
    (
        check_label(Label, CurrScore) -> 
		true
		;
        Label = 0
    ),
    RestLabels = [NextLabel | _],
    check_label(NextLabel, NextScore),
    compute_scores(RestLabels, CurrScore, RestScores).


% Predicate to sum the triple of neighbours
sum_scores([], []).
sum_scores([[Score1, Score2, Score3] | RestTriples], [Score | RestScores]) :-
    Sum is Score1 + Score2 + Score3,
    (
        Sum > 1 ->
        Score = 1
    ;
        Sum < -1 ->
        Score = -1
    ;
        Score = Sum
    ),
    sum_scores(RestTriples, RestScores).

% Prediacate to calculate the neighbours influence
label_scores([],[]).
label_scores(Labels, LabelScores) :-
		compute_scores(Labels, Scores),
		sum_scores(Scores, LabelScores).

% Predicate to calcultate scores based upon the neighbours influence
calculate_scores([], [], []).
calculate_scores([S | Scores], [W | WordNei], [N | NeighbScores]):-
	(
		(	
			W = 1,
			maxPos(S,N)
		)
		;
		(	
			W = -1,	
			maxNeg(S,N)
			)
		;
		(
			W = 0,
			avgSc(S,N)
		)
		),
	calculate_scores(Scores,WordNei,NeighbScores).

% Base case: Empty list, no positive values
maxPos([], 0).

% Case: Positive score found, update maxPos with the current positive value
maxPos([Pos-_|Rest], MaxPos) :-
    Pos > 0,
    maxPos(Rest, RestMax),
    MaxPos is max(Pos, RestMax).

% Case: Negative score found, continue searching for maxPos
maxPos([_|Rest], MaxPos) :-
    maxPos(Rest, MaxPos).

% Base case: Empty list, no negative values
maxNeg([], 0).

% Case: Negative score found, update maxNeg with the current negative value
maxNeg([_-Neg|Rest], MaxNeg) :-
    Neg > 0,
    maxNeg(Rest, RestMax),
    MaxNeg is max(Neg, RestMax).

% Case: Positive score found, continue searching for maxNeg
maxNeg([_|Rest], MaxNeg) :-
    maxNeg(Rest, MaxNeg).

% Calculate the average of positive scores
averagePosNeg(Scores, AveragePos, AverageNeg) :-
    separateScores(Scores, PositiveScores, NegativeScores),
	
	sum_list(PositiveScores, PSum),
	length(Scores, Count),
	(Count=0;PSum=0 -> AveragePos is 0;
	AveragePos is PSum / Count),
	
	sum_list(NegativeScores, NSum),
	(Count=0;NSum=0 -> AverageNeg is 0;
	AverageNeg is NSum / Count).
	
	
	
%   calculateAverage(PositiveScores, PosAverage),
%    calculateAverage(NegativeScores, NegAverage),
%    AveragePos is PosAverage - NegAverage.
%    AveragePos is PosAverage.


separateScores([], [], []).
separateScores([PScore-NScore |Rest], [PScore|Positives], [NScore|Negatives]) :-
    separateScores(Rest, Positives, Negatives).


avgSc(Scores,Avg):-
	averagePosNeg(Scores,AvgPos,AvgNeg),
	Avg is AvgPos-AvgNeg.
	
	
scoresANDweights([], [], []).
scoresANDweights([N | NeighbScores], [W | WeightsAfterInt],[M | MultipliedScores]) :-
	M is N * W,
	scoresANDweights(NeighbScores,WeightsAfterInt,MultipliedScores).


% Prediacate to apply the nagtion to the list of scores 
apply_negations([], [], []).
apply_negations([W | WeightsAfterInt],[M | MultipliedScores],[N | NegatedScores]):-
	(WeightsAfterInt = [_ | RestW] ->
		(			
			MultipliedScores = [Z | RestM],
			W = 0 ->
				(apply_negations(RestW,RestM,NegatedScores),
				N is Z * (-1))
			;
				(apply_negations(WeightsAfterInt,MultipliedScores,NegatedScores),
				 N is M)
		)
		;
		(
			W = 0 ->
				(N = MultipliedScores * (-1),NegatedScores=[])
			;
				(N = M,NegatedScores = [])		
		)
	).

% Compute final score
final_score([], 0).
final_score([H | T], FinalScore) :-
    final_score(T, RestSum),
    FinalScore is H + RestSum.

printSentiment(FinalScore):-
	(FinalScore > 0.3,write('Your sentence sentiment is positive  :)'),!);
	(FinalScore < -0.3,write('Your sentence sentiment is negative  :('),!);
	(write('Your sentence sentiment is neutral :|'),!).

% Main predicate to handle the program flow
main :-
    catch(
        sentiment_analysis_interface,
        Exception,
        handle_exception(Exception)
    ).


% Handler predicate to handle exceptions
handle_exception(Error) :-
	nl,write('Error: '),write(Error),nl,
    writeln('It seems an error has occured. You may have:'),
	writeln('- Entered no words.'),
	writeln('- Entered plural words.'),
	writeln('- Entered unaccepted words.'),
	writeln('- Entered unknown words or characters.'),
	writeln('- Entered punctuation marks.'),
	writeln('- Entered consecutive spaces.'),
	writeln('Please view the documentation and make sure to type correctly the words in the sentence.').

% Main entry point of the program
:- initialization(main).

% Import the dictionary 
:- consult('sent_wordnet.pl').

% Import the dictionary of modifiers 
:- consult('sent_modifiers.pl').
