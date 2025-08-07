SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "ResourceFunctions`ScrabbleScore`",
    "ResourceFunctions`BestPokerhand`",
    "ResourceFunctions`MakeChainmail`"
  (* .m File Names. *)
};