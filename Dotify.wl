(* ::Package:: *)

(* ::Title:: *)
(*Dotify*)


(* ::Section:: *)
(*Package Header*)


BeginPackage["ImageArt`Dotify`"]


(* ::Section:: *)
(*Function Definition*)


(* ::Subsubsection:: *)
(*Usage Statement*)


Dotify::usage="Dotify[image] Dotifies an Image"


(* ::Subsection:: *)
(*Private Context*)


Begin["Private`"]


(* ::Subsubsection:: *)
(*Attributes & Options*)


SetAttributes[Dotify, ReadProtected];


Options[Dotify] = {
	"DotCount" -> 50,
	"DotRadius" -> 1/2,
	Background -> White
}


(* ::Subsubsection:: *)
(*Definition*)


Dotify[image_, OptionsPattern[]] := Module[
	{dotCount, dotRadius, background, pixels},
	
	(* Option assignment *)
	{dotCount, dotRadius, background} = 
		OptionValue[#]& /@ {
			"DotCount", 
			"DotRadius", 
			Background
		};
	
	(* Getting individual pixel values *)
	pixels = Transpose[ImageData[ImageResize[image, dotCount], DataReversed -> True]];
	
	(*
		Mapping pixel values with colors and their position with center of dots.
		Reference: https://mathematica.stackexchange.com/questions/106165/reproduce-image-effect-in-mathematica
	*)
	Graphics[
		MapIndexed[
			{RGBColor @@ #1, Disk[#2, dotRadius]}&, 
			pixels, 
			{2}
		], 
		Background -> background,
		PlotRangePadding -> None
	]
	
]


(* ::Section:: *)
(*Package Footer*)


End[]


EndPackage[]
