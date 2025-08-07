SetDirectory[ParentDirectory @ DirectoryName @ $InputFileName];

Get[#]& /@ {
    "ResourceFunctions`ScrabbleScore`",
    "ResourceFunctions`BestPokerhand`",
    "ResourceFunctions`MakeChainmail`"
  (* .wl File Names. *)
};