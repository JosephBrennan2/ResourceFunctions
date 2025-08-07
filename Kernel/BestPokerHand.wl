BeginPackage["ResourceFunctions`BestPokerHand`"];
$Suits;
$Ranks;
$Cards;
$HoleCardCombinations;
CardRankToIndex;
ValidHandQ;
OnePairQ;
TwoPairQ;
ThreeOfAKindQ;
StraightQ;
FlushQ;
FullHouseQ;
QuadsQ;
StraightFlushQ;
HandRank;
TieBreaker;
HandStrength;
ShowCardGraphicHelp;
BestPokerHand;
Begin["`Private`"]
Clear[
	$Suits,$Ranks,$Cards,$HoleCardCombinations,CardRankToIndex,ValidHandQ,OnePairQ,TwoPairQ,ThreeOfAKindQ,StraightQ,FlushQ,FullHouseQ,QuadsQ,
	StraightFlushQ,HandRank,TieBreaker,HandStrength,ShowCardGraphicHelp,BestPokerHand
]

$Suits = {"\[ClubSuit]", "\[DiamondSuit]", "\[HeartSuit]", "\[SpadeSuit]"};
$Ranks = {"2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A"};
(* 52-card deck. *)
$Cards =
Flatten[
	Table[
		StringJoin[rank, suit],
		{rank, Reverse[$Ranks]},
		{suit, $Suits}
	],
	1
];
(* 1326 possible pairs of hole cards. *)
$HoleCardCombinations = Subsets[$Cards, {2}];

(* Numeric score of card rank. *)
CardRankToIndex[rank_String] :=
Switch[
	rank,
	"A", 14,
	"K", 13,
	"Q", 12,
	"J", 11,
	"T", 10,
	_,
	ToExpression[rank]
];

(* Ensure no repeating cards, and all cards are valid. *)
ValidHandQ[hand_List] :=
AllTrue[
	StringPart[hand, 1],
	MemberQ[
		$Ranks,
		#
	] &
] &&
AllTrue[
	StringPart[hand, 2],
	MemberQ[
		$Suits,
		#
	] &
] &&
Max[
	Counts[hand]
] <= 1;

(* Functions that check they type of hand that has been made. *)
OnePairQ[hand_List] :=
ValidHandQ[hand] &&
	Max[
		Counts[
			Select[
				Counts[
					StringPart[hand, 1]
				],
				# == 2 &
			]
		]
	] == 1;
TwoPairQ[hand_List] :=
ValidHandQ[hand] &&
	Length[
		Select[
			Counts[
				StringPart[hand, 1]
			],
			# == 2 &
		]
	] == 2;
ThreeOfAKindQ[hand_List] :=
ValidHandQ[hand] &&
Max[
	Counts[
	StringPart[hand, 1]
	]
] == 3;
StraightQ[hand_List] :=
ValidHandQ[hand] &&
	Length[hand] >= 5 &&
		Union[
			Differences[
				Sort[
					Map[
						CardRankToIndex,
						StringPart[hand, 1]
					]
				]
			]
		] == {1} || 
		Sort[Map[CardRankToIndex, StringPart[hand, 1]]] == {2, 3, 4, 5, 14};
FlushQ[hand_List] :=
ValidHandQ[hand] &&
Max[
	Counts[
		StringPart[hand, 2]
	]
] == 5;
FullHouseQ[hand_List] :=
ValidHandQ[hand] &&
	Values[
		Sort[
			Counts[
				StringPart[hand, 1]
			]
		]
	] == {2, 3};
QuadsQ[hand_List] :=
ValidHandQ[hand] &&
Max[
	Counts[
		StringPart[hand, 1]
	]
] == 4;
StraightFlushQ[hand_List] :=
ValidHandQ[hand] &&
StraightQ[hand] &&
FlushQ[hand];

(* Ranking of hand types. *)
HandRank[hand_List] :=
Which[
	StraightFlushQ[hand], 9,
	QuadsQ[hand], 8,
	FullHouseQ[hand], 7,
	FlushQ[hand], 6,
	StraightQ[hand], 5,
	ThreeOfAKindQ[hand], 4,
	TwoPairQ[hand], 3,
	OnePairQ[hand], 2,
	True, 1
];
HandRank[{}] := Nothing;
(* Settle tie-breakers with 'kickers'. *)
TieBreaker[hand_List] :=
Module[
	{
	ranks, freqs, sorted
	},
	ranks = ReverseSort[CardRankToIndex[StringPart[#, 1]] & /@ hand];
	If[
		ranks === {14, 5, 4, 3, 2},
		{1, 5, 4, 3, 2},
		freqs = Tally[ranks];
		sorted = ReverseSortBy[freqs, {Last[#], First[#]} &]; (* Last is multiplicity of card rank, First is the card rank itself. *)
		sorted[[All, 1]]
	]
];
(* Prepend HandRank to the tie-breaker list. *)
HandStrength[hand_List] :=
PadRight[
	Prepend[
		TieBreaker[hand],
		HandRank[hand]
	],
   	Length[hand] + 1,
   	"0"
];

(* Convert hole card format to be used for PlayingCardGraphic. *)
ShowCardGraphicHelp[holeCards_] :=
Module[
	{formatCards},
	formatCards =
	Map[
		ReplaceAll[#, {str_ :> {CardRankToIndex[StringPart[str, 1]] /. {14 -> 1}, StringPart[str, 2]}}] &,
		holeCards
	];
	If[
		Length[formatCards] == 1,
		Sequence @@ formatCards[[1]],
		formatCards
	]
];

(* Configure options. *)
Options[BestPokerHand] = {"ShowGraphic" -> False};
BestPokerHand[cards_List, opts : OptionsPattern[]] := BestPokerHand[cards, 1, opts] (* Able to use options without specifying n=1. *)

(* Case for 0, 1, 2, or 3 community cards. *)
BestPokerHand[cards_List /; Length[cards] <= 3 && ValidHandQ[cards], n_ : 1, opts : OptionsPattern[]] :=
Module[
	{possibleHands, rankedHands, sortedRankedHands, strongestHands},
	possibleHands =
	Map[
		# -> Join[#, cards] &,
		Select[$HoleCardCombinations, ContainsNone[#, cards] &]
	];
	rankedHands =(* Merge hands with the same ranking. *)
	Merge[
		MapApply[
			Function[{key, value}, HandStrength[value] -> key],
			possibleHands
		],
		Identity
	];
	sortedRankedHands =
	Reverse[
		KeySort[
			rankedHands
		]
	];
	strongestHands =
	Values[
		Take[
			MapApply[
				Or,
				sortedRankedHands
			],
			n
		]
	];
	If[
		Length[strongestHands] == 1,
		If[
			OptionValue["ShowGraphic"],
			If[
				Head[strongestHands[[1]]] === Or,
				Or @@ (ResourceFunction["PlayingCardGraphic"][#, "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}] & /@ (ShowCardGraphicHelp /@ (List @@ (strongestHands[[1]])))),
				ResourceFunction["PlayingCardGraphic"][ShowCardGraphicHelp[strongestHands[[1]]], "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}]
		],
		strongestHands[[1]]
		],
		If[
			OptionValue["ShowGraphic"],
			Map[
				If[
					Head[#] === Or,
						Or @@ (ResourceFunction["PlayingCardGraphic"][#, "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}] & /@ (ShowCardGraphicHelp /@ (List @@ (#)))),
						ResourceFunction["PlayingCardGraphic"][ShowCardGraphicHelp[#], "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}]
				] &,
				strongestHands
			],
			strongestHands
		]
	]
];
(* Case for 4 community cards. *)
BestPokerHand[cards_List /; Length[cards] == 4 && ValidHandQ[cards], n_ : 1, opts : OptionsPattern[]] :=
Module[
	{useOneHoleCard, useTwoHoleCards, possibleHands, rankedHands, sortedRankedHands, strongestHands},
	useOneHoleCard =
	Table[
		{h} -> Join[{h}, cards],
		{h, Complement[$Cards, cards]}
	];
	useTwoHoleCards =(* Keep only best-performing instance of each 'both hole cards' combination. *)
	Map[
		First[
			ReverseSortBy[
				Table[
					# -> Join[#, c],
					{c, Subsets[cards, {3}]}
				],
				HandStrength[Values[#]] &
			]
		] &,
		Select[$HoleCardCombinations, ContainsNone[#, cards] &]
	];
	possibleHands = Join[useOneHoleCard, useTwoHoleCards];
	rankedHands =
	Merge[
		MapApply[
			Function[{key, value}, HandStrength[value] -> key],
	  		possibleHands
		],
		Identity
	];
	sortedRankedHands =
		Reverse[
			KeySort[
				rankedHands
			]
		];
	strongestHands =
		Values[
			Take[
				MapApply[
					Or,
					sortedRankedHands
				],
				n
			]
		];
		If[
			Length[strongestHands] == 1,
			If[
				OptionValue["ShowGraphic"],
				If[
					Head[strongestHands[[1]]] === Or,
					Or @@ (ResourceFunction["PlayingCardGraphic"][#, "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}] & /@ (ShowCardGraphicHelp /@ (List @@ (strongestHands[[1]])))),
					ResourceFunction["PlayingCardGraphic"][ShowCardGraphicHelp[strongestHands[[1]]], "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}]
				],
			strongestHands[[1]]
			],
		If[
			OptionValue["ShowGraphic"],
			Map[
				If[
					Head[#] === Or,
					Or @@ (ResourceFunction["PlayingCardGraphic"][#, "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}] & /@ (ShowCardGraphicHelp /@ (List @@ (#)))),
					ResourceFunction["PlayingCardGraphic"][ShowCardGraphicHelp[#], "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}]
				] &,
				strongestHands
			],
			strongestHands
		]
	]
];
(* Case for 5 community cards. *)
BestPokerHand[cards_List /; Length[cards] == 5 && ValidHandQ[cards], n_ : 1, opts : OptionsPattern[]] :=
Module[
	{
		useZeroHoleCards, useOneHoleCard, useTwoHoleCards, possibleHands, rankedHands, sortedRankedHands, strongestHands
	},
	useZeroHoleCards = {{} -> cards};
	useOneHoleCard = (* Keep only best-performing instance of each 'single hole card' combination. *)
	Map[
		First[
			ReverseSortBy[
				Table[
					{#} -> Join[{#}, c],
					{c, Subsets[cards, {4}]}
				],
			HandStrength[Values[#]] &
			]
		] &,
		Complement[$Cards, cards]
	];
	useTwoHoleCards = (* Keep only best-performing instance of each 'both hole cards' combination. *)
	Map[
		First[
			ReverseSortBy[
				Table[
					# -> Join[#, c],
					{c, Subsets[cards, {3}]}
				],
				HandStrength[Values[#]] &
			]
		] &,
		Select[$HoleCardCombinations, ContainsNone[#, cards] &]
	];
	possibleHands = Join[useZeroHoleCards, useOneHoleCard, useTwoHoleCards];
	rankedHands =
	Merge[
		MapApply[
			Function[{key, value}, HandStrength[value] -> key ],
			possibleHands
		],
		Identity
	];
	sortedRankedHands =
	Reverse[
		KeySort[
			rankedHands
		]
	];
	strongestHands =
	Values[
		Take[
			MapApply[
				Or,
				sortedRankedHands
			],
			n
		]
	];
	If[
		Length[strongestHands] == 1,
		If[
			OptionValue["ShowGraphic"],
			If[
				Head[strongestHands[[1]]] === Or,
				Or @@ (ResourceFunction["PlayingCardGraphic"][#, "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}] & /@ (ShowCardGraphicHelp /@ (List @@ (strongestHands[[1]])))),
				ResourceFunction["PlayingCardGraphic"][ShowCardGraphicHelp[strongestHands[[1]]], "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}]
			],
			strongestHands[[1]]
		],
		If[
			OptionValue["ShowGraphic"],
			Map[
				If[
					Head[#] === Or,
					Or @@ (ResourceFunction["PlayingCardGraphic"][#, "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}] & /@ (ShowCardGraphicHelp /@ (List @@ (#)))),
					ResourceFunction["PlayingCardGraphic"][ShowCardGraphicHelp[#], "CardSpreadAngle" -> 0, "CardOffset" -> {0.5, 0}]
				] &,
				strongestHands
			],
			strongestHands
		]
	]
];
BestPokerHand[cards_List /; Length[cards] > 5, n_ : 1, opts : OptionsPattern[]] := Message[BestPokerHand::len, cards];
BestPokerHand::len = "The argument `1` must contain 5 or fewer elements.";
BestPokerHand[cards_List /; ! ValidHandQ[cards], n_ : 1, opts : OptionsPattern[]] := Message[BestPokerHand::invalid, cards];
BestPokerHand::invalid = "The argument `1` is not a valid list of community cards.";

End[]
EndPackage[]