BeginPackage["ResourceFunctions`GooglePolylineEncode`"];
EncodeCoordinate;
EncodePosition;
GooglePolylineEncode;
Begin["`Private`"]
Clear[EncodeCoordinate, EncodePosition, GooglePolylineEncode];
EncodeCoordinate[coord_Real] :=
Module[
	{
		roundCoord, posBinaryDigits, invertPosDigits, binaryDigits, shiftVal, binaryShiftedList, list, chunks, trimChunks, contChunks, decVals
	},
	If[
		coord == 0.,
		Return["?"]
	];
	roundCoord = Round[coord*10^5];
	posBinaryDigits = IntegerDigits[Abs[roundCoord], 2, 32]; (* Find binary digits of absolute value. *)
	If[
		coord < 0,
		invertPosDigits = BitXor[posBinaryDigits, Table[1, {32}]]; (* Invert each digit. *)
		binaryDigits = IntegerDigits[FromDigits[invertPosDigits, 2] + 1, 2, 32]; (*	Add 1 to result. *)
		shiftVal = BitShiftLeft[FromDigits[binaryDigits, 2], 1]; (* Bit-shift one to the left. *)
		binaryShiftedList = IntegerDigits[shiftVal, 2, 32]; (* Find binary digits. *)
		list = BitXor[binaryShiftedList, Table[1, {32}]], (* Invert each digit. *)
		shiftVal = BitShiftLeft[roundCoord, 1];
		list = IntegerDigits[shiftVal, 2, 32]
	];
	chunks = Map[Reverse, Partition[Reverse[list], 5]]; (* Break into 5-bit chunks starting from RHS*)
	trimChunks =
	With[
		{n = LengthWhile[Reverse[chunks], AllTrue[#, # == 0 &] &]},
		Take[chunks, Length[chunks] - n]
	]; (* Remove all-zero chunks. *)
	contChunks =
	MapIndexed[
		If[
			#2[[1]] < Length[trimChunks],
			IntegerDigits[BitOr[FromDigits[#1, 2], 32], 2],
			#1
		] &,(* OR with 32 (0x20) if not the last chunk. *)
		trimChunks
	];
	decVals = Plus[FromDigits[#, 2], 63] & /@ contChunks;
	StringJoin[FromCharacterCode /@ decVals]
];

EncodePosition[{lat_Real, lon_Real}] := StringJoin[EncodeCoordinate /@ {lat, lon}];
EncodePosition[{lat_Quantity, lon_Quantity}] := StringJoin[EncodeCoordinate /@ {QuantityMagnitude[lat], QuantityMagnitude[lon]}];

(* A single location. *)
GooglePolylineEncode[location_Entity] := GooglePolylineEncode[GeoPosition[location]];
GooglePolylineEncode[GeoPosition[{lat_Real, lon_Real}]] := GooglePolylineEncode[{lat, lon}];
GooglePolylineEncode[{lat_?NumericQ, lon_?NumericQ}] := EncodePosition[{N[lat], N[lon]}];
GooglePolylineEncode[{lat_Quantity, lon_Quantity}] := EncodePosition[{N[QuantityMagnitude[lat]], N[QuantityMagnitude[lon]]}];

(* Multiple {lat,lon} pairs. *)
GooglePolylineEncode[list : {__Entity}] := GooglePolylineEncode[GeoPosition /@ list]
GooglePolylineEncode[list : {__GeoPosition}] := GooglePolylineEncode[list /. GeoPosition[{lat_, lon_}] :> {lat, lon}];
GooglePolylineEncode[list : {__List}] :=
Module[
	{startPos, latDiffs, lonDiffs},
	startPos = First[list];
	latDiffs = Differences[list[[All, 1]]];
	lonDiffs = Differences[list[[All, 2]]];
	StringJoin[
		Map[
			EncodePosition[#] &,
			Prepend[Transpose[{latDiffs, lonDiffs}], startPos]
		]
	]
];

End[]
EndPackage[]