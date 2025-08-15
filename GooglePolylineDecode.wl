BeginPackage["ResourceFunctions`GooglePolylineDecode`"];
GooglePolylineDecode;
Begin["`Private`"]
Clear[GooglePolylineDecode];
GooglePolylineDecode[polyline_String] :=
Module[
	{
		chars, ends, starts, groups, fiveBitChunks, unsignedValues,  signedValues, coordinates, lats, lons, preciseLats, preciseLons, geo
	},
	chars = ToCharacterCode[polyline] - 63;
	ends = Flatten[Position[chars, _?(# < 32 &)]]; (* A value < 32 indicates a chunk with the continuation bit set to 0 (i.e., the last chunk). *)
	If[
		ends == {},
		Return[Message[GooglePolylineDecode::invalid, polyline]]
	];
	starts = Prepend[Most[ends] + 1, 1];
	groups = MapThread[Take[chars, {#1, #2}] &, {starts, ends}];
	fiveBitChunks = Map[BitAnd[#, 31] &, groups]; (* Extract 5-bit chunks by removing continuation bit. *)
	unsignedValues =
	Map[
		Sum[#[[k]]*2^(5*(k - 1)), {k, 1, Length[#]}] &,
		fiveBitChunks
	];(* Combine chunks by shifting each to its position. *)
	signedValues =
	Map[
		If[
			BitAnd[#, 1] == 1,
			BitNot[BitShiftRight[#, 1]],
			BitShiftRight[#, 1]
		] &,
		unsignedValues
	];(* Convert to signed value. *)
	coordinates = Map[SetAccuracy[#/10.^5, 6] &, signedValues];
	If[
		Length[coordinates] == 1,
		Return[N[coordinates[[1]], {Infinity, 6}]],
		If[
			OddQ[Length[coordinates]] && Length[coordinates] > 1,
			Return[Message[GooglePolylineDecode::invalid, polyline]],
			{lats, lons} = {Accumulate[coordinates[[1 ;; All ;; 2]]], 
			Accumulate[coordinates[[2 ;; All ;; 2]]]};
			preciseLats = Map[If[# == 0, 0, N[#, {Infinity, 6}]] &, lats];
			preciseLons = Map[If[# == 0, 0, N[#, {Infinity, 6}]] &, lons];
			geo = GeoPosition /@ Transpose[{preciseLats, preciseLons}];
			If[
				Length[geo] == 1,
				Return[geo[[1]]],
				Return[geo]
			]
		]
	]
];
GooglePolylineDecode::invalid = "The argument `1` is an incomplete polyline string.";

End[]
EndPackage[]