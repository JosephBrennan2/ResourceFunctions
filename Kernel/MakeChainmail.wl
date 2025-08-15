BeginPackage["ResourceFunctions`MakeChainmail`"];
europeanGrid;
europeanConnectors;
european4In1Chainmail;
squareGrid;
squareConnectors;
square4In1Chainmail;
japaneseGrid;
japaneseConnectors;
japaneseDiagonals1;
japaneseDiagonals2;
japanese6In1Chainmail;
singleOctahedron;
octahedralGrid;
octahedralChainmail;
MakeChainmail;
Begin["`Private`"]
Clear[
	europeanGrid, europeanConnectors, european4In1Chainmail, 
	squareGrid, squareConnectors, square4In1Chainmail,
	japaneseGrid, japaneseConnectors, japaneseDiagonals1, japaneseDiagonals2, japanese6In1Chainmail,
	singleOctahedron, octahedralGrid, octahedralChainmail,
	MakeChainmail
];

(*European 4-in-1*)
europeanGrid[pos_, rows_, cols_, ds_, \[Theta]_] :=
Flatten[
	Table[
		With[
			{center = pos + {i*ds, j*ds/2, 0}},
			Rotate[
			Torus[center, {4/5, 1}], \[Theta], {1, 0, 0}, {0, 0, 0} + center]
			],
		{i, 0, rows - 1},
		{j, 0, cols - 1}
	],
	1
];
europeanConnectors[pos_, rows_, cols_, ds_, \[Theta]_] :=
Flatten[
	Table[
		With[
			{center = {ds/2, ds/4, 0} + {i*ds, j*ds/2, 0}},
			Rotate[Torus[pos + center, {4/5, 1}], -\[Theta], {1, 0, 0}, pos + center]
		],
		{i, 0, rows - 2},
		{j, 0, cols - 2}
	],
	1
];
european4In1Chainmail[pos_, rows_, cols_, ds_, \[Theta]_, opts : OptionsPattern[Graphics3D]] :=
Graphics3D[
	{
		europeanGrid[pos, rows, cols, ds, \[Theta]],
		europeanConnectors[pos, rows, cols, ds, \[Theta]]
	},
	opts
];

(*Square 4-in-1*)
Clear[squareGrid, squareConnectors, square4In1Chainmail]
squareGrid[pos_, rows_, cols_, ds_, \[Theta]_] :=
Flatten[
	Table[
		With[
			{center = pos + {i*ds, j*ds/2, 0}},
			Rotate[
				Tube[
					Plus[center, #] & /@ {{-1, -1, 0}, {1, -1, 0}, {1, 1, 0}, {-1, 1, 0}, {-1, -1, 0}},
					0.2
				],
				\[Theta],
				{1, 0, 0},
				center
			]
		],
		{i, 0, rows - 1},
		{j, 0, cols - 1}
	],
   	1
];
squareConnectors[pos_, rows_, cols_, ds_, \[Theta]_] :=
Flatten[
	Table[
		With[
	 		{center = pos + {ds/2, ds/4, 0} + {i*ds, j*ds/2, 0}},
	 		Rotate[
	  			Tube[
	   				Plus[center, #] & /@ {{-1, -1, 0}, {1, -1, 0}, {1, 1, 0}, {-1, 1, 0}, {-1, -1, 0}},
	   				0.2
	   			],
	  			-\[Theta],
	  			{1, 0, 0},
	  			center
	  		]
	 	],
		{i, 0, rows - 2},
		{j, 0, cols - 2}
	],
   	1
];
square4In1Chainmail[pos_, rows_, cols_, ds_, \[Theta]_, opts : OptionsPattern[Graphics3D]] :=
Graphics3D[
	{
		squareGrid[pos, rows, cols, ds, \[Theta]], squareConnectors[pos, rows, cols, ds, \[Theta]]
	},
	opts
];

(*Japanese 6-in-1*)
japaneseGrid[pos_, rows_, cols_, ds_] :=
Flatten[
	Table[
		With[
			{center = pos + {0, If[EvenQ[i], 0, ds/2], 0} + {ds*i, ds*j, 0}},
     		Torus[center, {4/5, 1}]
     	],
    	{i, 0, rows - 1},
    	{j, 0, cols - 1}
    ],
  	1
];
japaneseConnectors[pos_, rows_, cols_, ds_] :=
Table[
   	With[
		{center = pos + {0, If[EvenQ[i], 0, ds/2], 0} + {i*ds, (j + 1/2)*ds, 0}},
    	Rotate[
    		Torus[center, {4/5, 1}],
     		\[Pi]/2,
    		{0, 1, 0},
     		center
    	]
    ],
   {i, 0, rows - 1},
   {j, 0, cols - 2}
];
japaneseDiagonals1[pos_, rows_, cols_, ds_] :=
Table[
	With[
		{center = pos + {0, If[EvenQ[i], 0, ds/2], 0} + {(i + 1/2)*ds, (j + 1/4)*ds, 0}},
    	Rotate[
    		Rotate[
      			Torus[center, {4/5, 1}],
      			\[Pi]/2,
      			{1, 0, 0},
      			center
      		],
     		\[Pi]/6,
     		{0, 0, 1},
     		center
     	]
    ],
   {i, 0, rows - 2},
   {j, 0, If[EvenQ[i], cols - 1, cols - 2]}
];
japaneseDiagonals2[pos_, rows_, cols_, ds_] :=
Table[
	With[
    	{center = pos + {0, If[EvenQ[i], ds/2, 0], 0} + {(i + 1/2)*ds, (j + 1/4)*ds, 0}},
    	Rotate[
     		Rotate[
      			Torus[center, {4/5, 1}],
      			\[Pi]/2,
      			{1, 0, 0},
      			center
      		],
     		-(\[Pi]/6),
     		{0, 0, 1},
     		center
     	]
	],
   {i, 0, rows - 2},
   {j, 0, If[EvenQ[i], cols - 2, cols - 1]}
];
japanese6In1Chainmail[pos_, rows_, cols_, ds_, opts : OptionsPattern[Graphics3D]] :=
Graphics3D[
	{
    	japaneseGrid[pos, rows, cols, ds],
    	japaneseConnectors[pos, rows, cols, ds],
    	japaneseDiagonals1[pos, rows, cols, ds],
    	japaneseDiagonals2[pos, rows, cols, ds]
    },
   opts
];

(*Octahedral*)
singleOctahedron[pos_, i_, j_] :=
Module[
	{octaVerts, trussEdges, verts, xRot, yRot, finalRotation, octa},
   	octaVerts = {{1, 0, 0}, {-1, 0, 0}, {0, 1, 0}, {0, -1, 0}, {0, 0, 1}, {0, 0, -1}};
   	trussEdges = {{1, 3}, {1, 4}, {1, 5}, {1, 6}, {2, 3}, {2, 4}, {2, 5}, {2, 6}, {3, 5}, {3, 6}, {4, 5}, {4, 6}};
   	xRot = If[EvenQ[i], Pi/4, 0];
   	yRot = If[EvenQ[j], Pi/4, 0];
   	finalRotation = RotationMatrix[yRot, {0, 1, 0}] . RotationMatrix[xRot, {1, 0, 0}];
   	verts = (pos + finalRotation . #) & /@ octaVerts;
   	octa = 
	Table[
		Tube[
			{verts[[trussEdges[[k, 1]]]], verts[[trussEdges[[k, 2]]]]},
       		0.05
		], 
		{k, Length[trussEdges]}
	]
];
octahedralGrid[pos_, rows_, cols_, ds_] :=
Flatten[
	Table[
   		With[
     		{center = pos + {i*ds, j*ds, 0}},
     		If[EvenQ[i] && EvenQ[j], Nothing, singleOctahedron[center, i, j]]
     	],
    	{i, 0, rows - 1}, {j, 0, cols - 1}
	],
   	1
];
octahedralChainmail[pos_, rows_, cols_, ds_, opts : OptionsPattern[Graphics3D]] :=
Graphics3D[
	octahedralGrid[pos, rows, cols, ds],
   	opts
];

MakeChainmail[] := MakeChainmail["European4In1", {0, 0, 0}, 3, 3, 2.5, \[Pi]/8];
MakeChainmail["European4In1", opts : OptionsPattern[Graphics3D]] := MakeChainmail["European4In1", {0, 0, 0}, 3, 3, 2.5, \[Pi]/8, opts];
MakeChainmail["Square4In1", opts : OptionsPattern[Graphics3D]] := MakeChainmail["Square4In1", {0, 0, 0}, 3, 3, 2.5, \[Pi]/10, opts];
MakeChainmail["Japanese6In1", opts : OptionsPattern[Graphics3D]] := MakeChainmail["Japanese6In1", {0, 0, 0}, 3, 3, 2.5, 0, opts];
MakeChainmail["Japanese6In1", pos_List : {0, 0, 0}, rows_Integer : 3, cols_Integer : 3, opts : OptionsPattern[Graphics3D]] := MakeChainmail["Japanese6In1", pos, rows, cols, 2.5, 0, opts];
MakeChainmail["Octahedral", opts : OptionsPattern[Graphics3D]] := MakeChainmail["Octahedral", {0, 0, 0}, 3, 3, 1.6, 0, opts];
MakeChainmail["Octahedral", pos_List : {0, 0, 0}, rows_Integer : 3, cols_Integer : 3, opts : OptionsPattern[Graphics3D]] := MakeChainmail["Octahedral", pos, rows, cols, 1.6, 0, opts];
MakeChainmail[type_String : "European4In1", pos_List : {0, 0, 0}, rows_Integer : 3, cols_Integer : 3, ds_ : 2.5, \[Theta]_ : \[Pi]/8,  opts : OptionsPattern[Graphics3D]] :=
Which[
	type === "European4In1",
	european4In1Chainmail[pos, rows, cols, ds, \[Theta], opts],
	type === "Square4In1",
	square4In1Chainmail[pos, rows, cols, ds, \[Theta], opts],
	type === "Japanese6In1",
	japanese6In1Chainmail[pos, rows, cols, ds, opts],
	type === "Octahedral",
	octahedralChainmail[pos, rows, cols, ds, opts]
];

End[]
EndPackage[]