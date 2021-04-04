(* ::Package:: *)

(* ::Title:: *)
(*Voronoize*)


(* ::Section:: *)
(*Package Header*)


BeginPackage["ImageArt`Voronoize`"]


(* ::Section:: *)
(*Function Definition*)


(* ::Subsubsection:: *)
(*Usage statement*)


Voronoize::usage="Voronoize[image] creates an image with Voronoi mesh effect."


(* ::Subsection:: *)
(*Private Context*)


Begin["Private`"]


(* ::Subsubsection:: *)
(*Attributes & Options*)


SetAttributes[Voronoize, ReadProtected];


Options[Voronoize] = {
	"CellSize" -> 10,
	"CellSmudge" -> 0,
	"CellBoundaryStyle" -> Directive[Black, Opacity[0.2]],
	"ColorFunction" -> "GrayTones",
	ImageSize -> 300
};


(* ::Subsubsection:: *)
(*Definition*)


Voronoize[image_, OptionsPattern[]] := Module[
	{gImg, data, cellSize, cellSmudge, cellBoundaryStyle, colorFunction, imageSize},
	
	(* Options assignment *)
	cellSize = OptionValue["CellSize"];
	cellSmudge = OptionValue["CellSmudge"];
	cellBoundaryStyle = OptionValue["CellBoundaryStyle"];
	colorFunction = OptionValue["ColorFunction"];
	imageSize = OptionValue[ImageSize];
	
	(* Convert the image to grayscale *)
	gImg = ColorConvert[ImageResize[image, 300], "GrayScale"];
	
	(*Extract image data (pixel values) along with pixel indices *)
	data = MapIndexed[Append[#2, #1]&, ImageData[gImg], {2}];
	
	(* 
		Image generation
		Reference: https://mathematica.stackexchange.com/questions/8507/artistic-image-vectorization/8525#8525
	*)
	
	Rotate[
		ListDensityPlot[
			(
				MapThread[
					Append, 
					{3 RandomReal[{-1, 1}, {Length[#], 2}], ConstantArray[0, Length[#]]}
				] + #
			)& @ Flatten[Transpose @ data, 1][[1;;-1;;cellSize]], 
			InterpolationOrder -> cellSmudge,
			ColorFunction -> colorFunction,
			BoundaryStyle -> cellBoundaryStyle,
			Frame -> False,
			PlotRangePadding -> 0,
			ImageSize -> imageSize,
			AspectRatio -> Automatic
		],
	-Pi/2]
]


(* ::Section:: *)
(*Package Footer*)


End[]


EndPackage[]
