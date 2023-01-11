(* ::Package:: *)

(* ::Subsubsection:: *)
(*Copyright*)


(* ::Text:: *)
(*Copyright 2017-2018, Kasper J. Larsen and Robbert Rietkerk*)
(*This program is distributed under the terms of the GNU General Public License.*)


(* ::Text:: *)
(*This program is free software: you can redistribute it and/or modify*)
(*    it under the terms of the GNU General Public License as published by*)
(*    the Free Software Foundation, either version 3 of the License, or*)
(*    (at your option) any later version.*)
(**)
(*    This program is distributed in the hope that it will be useful,*)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of*)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the*)
(*    GNU General Public License for more details.*)
(**)
(*    You should have received a copy of the GNU General Public License*)
(*    along with this program.  If not, see <http://www.gnu.org/licenses/>.*)


(* ::Subsubsection::Closed:: *)
(*Begin*)


BeginPackage["MultivariateResidues`"];

MultivariateResidue::usage="\
MultivatiateResidue[Num,{d[1],\[Ellipsis],d[n]},{z[1]\[Rule]z[1,1],\[Ellipsis],z[n]\[Rule]z[n,1]}] calculates the residue of Num/(d[1]\[CenterEllipsis]d[n]) at the point (z[1],\[Ellipsis],z[n])=(z[1,1],\[Ellipsis],z[n,1]).\n\
MultivatiateResidue[Num,{d[1],\[Ellipsis],d[n]},{z[1],\[Ellipsis],z[n]},{{z[1,1],\[Ellipsis],z[n,1]},{z[1,2],\[Ellipsis],z[n,2]},\[Ellipsis]}] calculates the residue of Num/(d[1]\[CenterEllipsis]d[n]) at the points (z[1],\[Ellipsis],z[n]) \[Epsilon] {(z[1,1],\[Ellipsis],z[n,1]),(z[1,2],\[Ellipsis],z[n,2]),\[Ellipsis]}.\n\
The following options can be given:\n\
Method - ''TransformationFormula''(default),''QuotientRingDuality'' - the method used to calculate the multivariate residue\n\
MonomialOrder - Lexicographic(default),DegreeLexicographic,DegreeReverseLexicographic - the criterion used for monomial ordering\n\
CoefficientDomain - Automatic(default),Rationals,RationalFunctions,InexactNumbers - the type of objects assumed to be coefficients of monomials\n\
FindMinimumDelta - True(default),False - whether to look for the smallest power \[Delta] in the method ''QuotientRingDuality''.";

(* Advanced General Options *)
$MultiResInputChecks=True;
$MultiResInputChecks::usage="Check user input (default:True)."

$MultiResInternalChecks=True;
$MultiResInternalChecks::usage="Perform internal checks (default:True)."

$MultiResUseSingular=False;
$MultiResUseSingular::usage="Use Singular for computations of Groebner basis and polynomial division (default:False). Note: Please specify the path to Singular in $MultiResSingularPath";

$MultiResSingularPath="/usr/bin/Singular";
$MultiResSingularPath::usage="The path to Singular (default:''/usr/bin/Singular''). Note: $MultiResUseSingular must be set to True in order to have effect.";

FindMinimumDelta::usage="Option of MultivariateResidue when using the method ''QuotientRingDuality''";

GlobalResidue::usage="Instruction to compute the global residue directly.";

GlobalResidueTheoremCPn::usage="\
GlobalResidueTheoremCPn[Num,{d[1],\[Ellipsis],d[n]},{z[1],\[Ellipsis],z[n]}] produces linear relations among residues computed at various locations, \n\
that follow from the Global Residue Theorem (see Section 5 of the accompanying paper).\n\
The output takes the form {{Ideal[1],{Variety[1],Residues[1]}}, {Ideal[2],{Variety[2],Residues[2]}}, ...}, where\n\
Ideal[i] is a list of n+1 denominator factors in terms of homogeneous coordinates w[0],w[1],...,w[n] (in the compactification \!\(\*SuperscriptBox[\(\[DoubleStruckCapitalC]\[DoubleStruckCapitalP]\), \(n\)]\) of \!\(\*SuperscriptBox[\(\[DoubleStruckCapitalC]\), \(n\)]\)),\n\
Variety[i] is a list of N[i] poles (with N[i] an integer), each of which is a point in \!\(\*SuperscriptBox[\(\[DoubleStruckCapitalC]\), \(n + 1\)]\),\n\
Residues[i] is a list of N[i] residues of Ideal[i] computed at the N[i] poles in Variety[i].\n\
Only the non-vanishing terms in the linear relations (i.e. only the non-vanishing residues) are displayed.";

w::usage="Homogeneous coordinate appearing in the output of GlobalResidueTheoremCPn.";


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*CheckInput*)


CheckInput::num="The numerator is identically zero. Returning a vanishing residue.";
CheckInput::den="The denominator factors `1` are independent of (one or more) of the variables `2`. Returning a vanishing residue.";
CheckInput::ideal="The ideal `1` is not zero-dimensional. Returning a vanishing residue.";
CheckInput::poles="The list of poles `1` does not have the correct form {{z1[0],...,zn[0]},{z1[1],...,zn[1]},...}";

CheckInput[num_,den_List,vars_List,poles_List]:=Module[
	{null},
	
	(* A null result is a list of zeros, one for each pole *)
	null=Table[0,{Length[poles]}];

	(* If num is identically zero, then the residue is zero *)
	If[num===0,
		Message[CheckInput::num];
		Throw[null];
	];

	(* If den doesn't depend on all vars, then the residue is zero *)
	If[Length[Intersection[vars,Variables[den]]]=!=Length[vars],
		Message[CheckInput::den,den,vars];
		Throw[null];
	];
	
	(* More generally: If the ideal generated by den is not zero-dimensional, then the residue is not defined *)
	If[DeleteDuplicates[Length/@Quiet[Solve[#==0&/@den,vars]]]=!={Length[vars]},
		Message[CheckInput::ideal,AngleBracket@@den];
		Throw[null];
	];

	(* If poles does not have the form {{...},{...},...} (list of lists), then return Null *)
	If[(DeleteDuplicates[Head/@poles]=!={List}||DeleteDuplicates[Length/@poles]=!={Length[vars]})&&poles=!={GlobalResidue},
		Message[CheckInput::poles,poles];
		Throw[Null];
	];


]


(* ::Subsubsection::Closed:: *)
(*SingularInterface*)


TranslateMonomialOrder={DegreeReverseLexicographic->"dp",DegreeLexicographic->"Dp",Lexicographic->"lp"};

Relabel[Vars_List,LabelName_String]:=Thread[Vars -> (ToExpression[LabelName<>ToString[#]]&/@Range[Length[Vars]]) ];

RoundBrackets[str_String]:=StringReplace[str,{"{"->"(","}"->")"}];

ListInStringForm[lst_List]:=Module[{L},
L=ToString[InputForm[lst]];
L=StringReplace[L,"^"~~a:NumberString~~"/"~~b:NumberString:>"^"~~ToString[a]~~"*1/"~~ToString[b]];
L=StringReplace[L,{"{"->"","}"->""}];
Return[L]
];


MultivariateResidue::nosingular="Singular has not been found in the specified path $MultiResSingularPath=`1`.";


Options[SingularFindMinimumDelta]={
	MonomialOrder->Inherited
};

SingularFindMinimumDelta[Ideal_List,J_List,Vars_List,OptionsPattern[]]:=Module[
{Parameters,RelabelVars,RelabelParameters,RelabelAll,
IdealInStringForm,JInStringForm,ModulusAndConstants,RingVars,SingularMonomialOrderings,
Input,SingularResult,SingularResultInMathematicaForm,time},

If[FileNames[$MultiResSingularPath]==={},Message[MultivariateResidue::nosingular,$MultiResSingularPath];Abort[]];

Parameters=Complement[Variables[Ideal],Vars];
RelabelVars=Relabel[Vars,"z"];
RelabelParameters=Relabel[Parameters,"c"];
RelabelAll=Join[RelabelVars,RelabelParameters];

ModulusAndConstants=RoundBrackets[ToString[Prepend[Parameters/.RelabelParameters,0]]];
RingVars=RoundBrackets[ToString[Vars/.RelabelVars]];
SingularMonomialOrderings=OptionValue[MonomialOrder]/.TranslateMonomialOrder;

IdealInStringForm=ListInStringForm[Ideal/.RelabelAll];
JInStringForm=ListInStringForm/@(J/.RelabelAll);

Input=StringJoin[
"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
"option(redSB);",
"option(returnSB);",
"ideal I = "<>IdealInStringForm<>";",
"ideal GB = std(I);",
	Sequence@@Table[
"ideal J"<>ToString[i]<>" = "<>JInStringForm[[i]]<>";"
	,{i,1,Length[J]}],
"ideal M = intersect("<>ListInStringForm[Table[ToExpression["J"<>ToString[i]],{i,1,Length[J]}]]<>");",
"int delta = 1;",
"while ( size(reduce(M^delta,GB,1))>0 ) {delta=delta+1;} ",
"delta;",
"exit;"
];

(*  Run Singular  *)
SingularResult=ReadList[StringJoin[
"!",$MultiResSingularPath," --no-rc --no-stdlib --no-warn --no-shell --no-tty --quiet --execute='",Input,"'"
],"String"];
SingularResult=ToExpression[SingularResult//First];

(*  Convert Singular's output to Mathematica notation  *)
SingularResultInMathematicaForm=SingularResult/.(Reverse/@RelabelAll);
Return[SingularResultInMathematicaForm];
];


Options[SingularPartitionOfUnity]={
	MonomialOrder->Inherited
};

SingularPartitionOfUnity[Ideal_List,Lagrange_List,delta_Integer,Vars_List,OptionsPattern[]]:=Module[
{Parameters,RelabelVars,RelabelParameters,RelabelAll,
IdealInStringForm,LagrangeInStringForm,ModulusAndConstants,RingVars,SingularMonomialOrderings,
Input,SingularResult,SingularResultInMathematicaForm,time},

If[FileNames[$MultiResSingularPath]==={},Message[MultivariateResidue::nosingular,$MultiResSingularPath];Abort[]];

Parameters=Complement[Variables[Ideal],Vars];
RelabelVars=Relabel[Vars,"z"];
RelabelParameters=Relabel[Parameters,"c"];
RelabelAll=Join[RelabelVars,RelabelParameters];

ModulusAndConstants=RoundBrackets[ToString[Prepend[Parameters/.RelabelParameters,0]]];
RingVars=RoundBrackets[ToString[Vars/.RelabelVars]];
SingularMonomialOrderings=OptionValue[MonomialOrder]/.TranslateMonomialOrder;

IdealInStringForm=ListInStringForm[Ideal/.RelabelAll];
LagrangeInStringForm=ListInStringForm[Lagrange/.RelabelAll];

Input=StringJoin[
"proc PowerReduce (poly P, int pow, ideal Gb)",
"{",
"  list R;",
"  R[1] = reduce(P,Gb);",
"  for (int i=2; i<=pow; i=i+1)",
"  {",
"    R[i] = reduce(R[i-1]*R[1],Gb);",
"  }",
"  return(R[pow])",
"}",
"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
"option(redSB);",
"ideal I = "<>IdealInStringForm<>";",
"ideal GB = std(I);",
"int delta = "<>ToString[delta]<>";",
"list L = "<>LagrangeInStringForm<>";",
"proc PartitionOfUnity (poly P)",
"{",
"  poly R = P;",
"  R = 1-PowerReduce(R,delta,GB);",
"  R = 1-PowerReduce(R,delta,GB);",
"  return(R);",
"}",
"list E = apply(L, PartitionOfUnity);",
"string(E);",
"exit;"
];
(*  Run Singular  *)
SingularResult=ReadList[StringJoin[
"!",$MultiResSingularPath," --no-rc --no-stdlib --no-warn --no-shell --no-tty --quiet --execute='",Input,"'"
],"String"];
SingularResult="{"<>SingularResult[[1]]<>"}";
SingularResult=ToExpression[SingularResult];

(*  Convert Singular's output to Mathematica notation  *)
SingularResultInMathematicaForm=SingularResult/.(Reverse/@RelabelAll);
Return[SingularResultInMathematicaForm];
];


Options[SingularUnivariatePolynomials]={
	MonomialOrder->Inherited
};

SingularUnivariatePolynomials[Ideal_List,Vars_List,OptionsPattern[]]:=Module[
{Parameters,RelabelVars,RelabelParameters,RelabelAll,
ModulusAndConstants,RingVars,SingularMonomialOrderings,
IdealInStringForm,eliminates,
Input,SingularResult,SingularResultInMathematicaForm},

If[FileNames[$MultiResSingularPath]==={},Message[MultivariateResidue::nosingular,$MultiResSingularPath];Abort[]];

Parameters=Complement[Variables[Ideal],Vars];
RelabelVars=Relabel[Vars,"x"];
RelabelParameters=Relabel[Parameters,"c"];
RelabelAll=Join[RelabelVars,RelabelParameters];

ModulusAndConstants=RoundBrackets[ToString[Prepend[Parameters/.RelabelParameters,0]]];
RingVars=RoundBrackets[ToString[Vars/.RelabelVars]];
SingularMonomialOrderings=OptionValue[MonomialOrder]/.TranslateMonomialOrder;

IdealInStringForm=ListInStringForm[Ideal/.RelabelAll];

eliminates=Table[Drop[ToString/@(Vars/.RelabelVars),{i}],{i,1,Length[Vars]}];
eliminates=Riffle[#,"*"]&/@eliminates;
eliminates=StringJoin@@#&/@eliminates;

(*  Create input file for Singular  *)
If[Length[Ideal]>1,
	Input=StringJoin[
	"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
	"option(redSB);",
	"ideal I = "<>IdealInStringForm<>";",
	"list L ="<>StringJoin@@Riffle[("eliminate(I,"<>#<>")[1]")&/@eliminates,","]<>";",
	"string(L);",
	"exit;"
	];
];

If[Length[Ideal]==1,
	Input=StringJoin[
	"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
	"option(redSB);",
	"ideal I = "<>IdealInStringForm<>";",
	"string(std(I));",
	"exit;"
	];
];

(*  Run Singular  *)
SingularResult=ReadList[StringJoin[
"!",$MultiResSingularPath," --no-rc --no-stdlib --no-warn --no-shell --no-tty --quiet --execute='",Input,"'"
],"String"];
SingularResult="{"<>SingularResult[[1]]<>"}";
SingularResult=ToExpression[SingularResult];

(*  Convert Singular's output to Mathematica notation  *)
SingularResultInMathematicaForm=SingularResult/.(Reverse/@RelabelAll);
Return[SingularResultInMathematicaForm];
];


Options[SingularGroebnerBasis]={
	MonomialOrder->Inherited
};

SingularGroebnerBasis[Ideal_List,Vars_List,OptionsPattern[]]:=Module[
{Parameters,RelabelVars,RelabelParameters,RelabelAll,
IdealInStringForm,FactorsInStringForm,ModulusAndConstants,RingVars,SingularMonomialOrderings,
Input,SingularResult,SingularResultInMathematicaForm,time},

If[FileNames[$MultiResSingularPath]==={},Message[MultivariateResidue::nosingular,$MultiResSingularPath];Abort[]];

Parameters=Complement[Variables[Ideal],Vars];
RelabelVars=Relabel[Vars,"z"];
RelabelParameters=Relabel[Parameters,"c"];
RelabelAll=Join[RelabelVars,RelabelParameters];

ModulusAndConstants=RoundBrackets[ToString[Prepend[Parameters/.RelabelParameters,0]]];
RingVars=RoundBrackets[ToString[Vars/.RelabelVars]];
SingularMonomialOrderings=OptionValue[MonomialOrder]/.TranslateMonomialOrder;

IdealInStringForm=ListInStringForm[Ideal/.RelabelAll];

Input=StringJoin[
"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
"option(redSB);",
"ideal I = "<>IdealInStringForm<>";",
"string(std(I));",
"exit;"
];

(*  Run Singular  *)
SingularResult=ReadList[StringJoin[
"!",$MultiResSingularPath," --no-rc --no-stdlib --no-warn --no-shell --no-tty --quiet --execute='",Input,"'"
],"String"];
SingularResult="{"<>SingularResult[[1]]<>"}";
SingularResult=ToExpression[SingularResult];

(*  Convert Singular's output to Mathematica notation  *)
SingularResultInMathematicaForm=SingularResult/.(Reverse/@RelabelAll);
Return[SingularResultInMathematicaForm];
];


Options[SingularPolynomialRemainder]={
	MonomialOrder->Inherited
};

SingularPolynomialRemainder[poly_,groebnerbasis_List,Vars_List,OptionsPattern[]]:=Module[
{Parameters,RelabelVars,RelabelParameters,RelabelAll,
polyInStringForm,GBInStringForm,FactorsInStringForm,ModulusAndConstants,RingVars,SingularMonomialOrderings,
Input,SingularResult,SingularResultInMathematicaForm,time},

Parameters=Complement[Union[Variables[poly],Variables[groebnerbasis]],Vars];
RelabelVars=Relabel[Vars,"z"];
RelabelParameters=Relabel[Parameters,"c"];
RelabelAll=Join[RelabelVars,RelabelParameters];

ModulusAndConstants=RoundBrackets[ToString[Prepend[Parameters/.RelabelParameters,0]]];
RingVars=RoundBrackets[ToString[Vars/.RelabelVars]];
SingularMonomialOrderings=OptionValue[MonomialOrder]/.TranslateMonomialOrder;

polyInStringForm=ListInStringForm[{poly}/.RelabelAll];
GBInStringForm=ListInStringForm[groebnerbasis/.RelabelAll];

Input=StringJoin[
"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
"option(redSB);",
"ideal GB = "<>GBInStringForm<>";",
"reduce("<>polyInStringForm<>",GB);",
"exit;"
];

(*  Run Singular  *)
SingularResult=ReadList[StringJoin[
"!",$MultiResSingularPath," --no-rc --no-stdlib --no-warn --no-shell --no-tty --quiet --execute='",Input,"'"
],"String"];
SingularResult=ToExpression[SingularResult//First];

(*  Convert Singular's output to Mathematica notation  *)
SingularResultInMathematicaForm=SingularResult/.(Reverse/@RelabelAll);
Return[SingularResultInMathematicaForm];
];


Options[SingularDetA]={
	MonomialOrder->Inherited
};

SingularDetA[Ideal_List,Factors_List,Vars_List,OptionsPattern[]]:=Module[
{Parameters,RelabelVars,RelabelParameters,RelabelAll,
IdealInStringForm,FactorsInStringForm,ModulusAndConstants,RingVars,SingularMonomialOrderings,
Input,SingularResult,SingularResultInMathematicaForm,time},

If[FileNames[$MultiResSingularPath]==={},Message[MultivariateResidue::nosingular,$MultiResSingularPath];Abort[]];

Parameters=Complement[Variables[Ideal],Vars];
RelabelVars=Relabel[Vars,"z"];
RelabelParameters=Relabel[Parameters,"c"];
RelabelAll=Join[RelabelVars,RelabelParameters];

ModulusAndConstants=RoundBrackets[ToString[Prepend[Parameters/.RelabelParameters,0]]];
RingVars=RoundBrackets[ToString[Vars/.RelabelVars]];
SingularMonomialOrderings=OptionValue[MonomialOrder]/.TranslateMonomialOrder;

IdealInStringForm=ListInStringForm[Ideal/.RelabelAll];
FactorsInStringForm=ListInStringForm[Factors/.RelabelAll];

Input=StringJoin[
"ring R = "<>ModulusAndConstants<>","<>RingVars<>","<>SingularMonomialOrderings<>";",
"ideal I = "<>IdealInStringForm<>";",
"ideal F = "<>FactorsInStringForm<>";",
"list L = lift(I,F);",
(*"det(L[1]);",*)
"reduce(det(L[1]),std(F));",
"exit;"
];

(*  Run Singular  *)
SingularResult=ReadList[StringJoin[
"!",$MultiResSingularPath," --no-rc --no-stdlib --no-warn --no-shell --no-tty --quiet --execute='",Input,"'"
],"String"];
SingularResult=ToExpression[SingularResult//First];

(*  Convert Singular's output to Mathematica notation  *)
SingularResultInMathematicaForm=SingularResult/.(Reverse/@RelabelAll);
Return[SingularResultInMathematicaForm];
];


(* ::Subsubsection::Closed:: *)
(*Leading Term*)


LT[polynomial_,(variables_List:{}),(ordering_String:"Lexicographic")]:=Module[{vars,mono},
	If[Intersection[variables,Variables[polynomial]]==={},Return[polynomial]];
	If[variables==={},vars=Variables[polynomial],vars=variables];
	mono=MonomialList[polynomial,vars,ordering];
	mono=First[mono];
	If[Head[mono]===Plus,
		Print["Error in LT[",polynomial,",",variables,"]!  mono = ",mono];
		Abort[]
	];
	Return[mono]
]

LC[polynomial_,(variables_List:{}),(ordering_String:"Lexicographic")]:=Module[{lt,lc},
	lt=LT[polynomial,variables,ordering];
	lc=lt/.(#->1&/@variables);
	Return[lc]
]

LM[poly_,(var_List:{}),(ord_String:"Lexicographic")]:=LT[poly,var,ord]/LC[poly,var,ord]

NormalizeLC[expr_,vars_:{}]:=Module[{lc},
	lc=LC[expr,vars];
	If[lc===1,Return[expr],Return[Apart[expr/lc]]];
]


(* ::Subsubsection::Closed:: *)
(*MultivariateResidues*)


MultivariateResidue::mthd="Value of option Method -> ''`1`'' is not valid.";
MultivariateResidue::cdom="Value of option CoefficientDomain -> `1` is not valid.";
MultivariateResidue::mono="Value of option MonomialOrder -> `1` is not valid.";
MultivariateResidue::find="Value of option FindMinimumDelta -> `1` is not valid.";

Options[MultivariateResidue]={
	Method->"TransformationFormula",	
	MonomialOrder->Lexicographic,
	CoefficientDomain->RationalFunctions,
	FindMinimumDelta->True
};

MultivariateResidue[num_,den_List,rules_List,opts:OptionsPattern[]]:=First[MultivariateResidue[num,den,First/@rules,{Last/@rules},opts]]

MultivariateResidue[num_,den_List,vars_List,poles_List,opts:OptionsPattern[]]:=Module[{explicitopts},
	(* Check options *)
	If[And@@(OptionValue[Method]=!=#&/@{"TransformationFormula","QuotientRingDuality"}),
		Message[MultivariateResidue::mthd,OptionValue[Method]];
		Return[Null];
	];
	If[And@@(OptionValue[CoefficientDomain]=!=#&/@{Rationals,RationalFunctions,InexactNumbers}),
		Message[MultivariateResidue::cdom,OptionValue[CoefficientDomain]];
		Return[Null];
	];
	If[And@@(OptionValue[MonomialOrder]=!=#&/@{Lexicographic,DegreeLexicographic,DegreeReverseLexicographic}),
		Message[MultivariateResidue::mono,OptionValue[MonomialOrder]];
		Return[Null];
	];
	If[And@@(OptionValue[FindMinimumDelta]=!=#&/@{True,False}),
		Message[MultivariateResidue::find,OptionValue[FindMinimumDelta]];
		Return[Null];
	];
	
	explicitopts={
		Method->OptionValue[Method],
		MonomialOrder->OptionValue[MonomialOrder],
		CoefficientDomain->OptionValue[CoefficientDomain],
		FindMinimumDelta->OptionValue[FindMinimumDelta]
	};

	(* Select the desired algorithm *)
	If[OptionValue[Method]==="TransformationFormula",
		Return[MultivariateResidue1[num,den,vars,poles,Sequence@@FilterRules[explicitopts,Options[MultivariateResidue1]]]];
	];
	If[OptionValue[Method]==="QuotientRingDuality",
		Return[MultivariateResidue2[num,den,vars,poles,Sequence@@FilterRules[explicitopts,Options[MultivariateResidue2]]]];
	];

	(* Otherwise return Null result *)
	Return[Null]
];



(* ::Subsubsection::Closed:: *)
(*MultivariateResidues1 (TransformationFormula)*)


MultivariateResidue::convmat="Error! Conversion matrix between f and g is incorrect. Cannot proceed to calculate the residue w.r.t. the ideal `1`.";
MultivariateResidue::detA="The conversion matrix between f and g has vanishing determinant, so the residue is zero.";
MultivariateResidue::poleord="Error in determining the order of the poles: the given root `1` seems to be absent in the ideal. Cannot proceed to calculate the residue w.r.t. the ideal `2`. Hint: if the given roots are simplified with certain assumptions, then please specify those assumptions in $Assumptions.";

Options[MultivariateResidue1]={
	MonomialOrder->Inherited,
	CoefficientDomain->Inherited
};


MultivariateResidue1[num_,den_List,vars_List,poles_List,opts:OptionsPattern[]]:=Catch[Module[
{MonOrd,CoefDom,n,x,X,h,f,param,Jacs,poleQ,g,gb,lc,B,C,DetA,zeros,DenFactors,DenFactorsCoef,ProductOfGs,PoleFactors,OrderOfPoles,PoleMonomial,ListOfDerivatives,t,Res,pos},

If[$MultiResInputChecks===True,
	CheckInput[num,den,vars,poles];
];
MonOrd=OptionValue[MonomialOrder];
CoefDom=OptionValue[CoefficientDomain];

n=Length[vars];
x=X/@Range[n];
h=num/.Thread[vars->x];
f=den/.Thread[vars->x];
param=Complement[Variables[f],x];

(* If the residue is non-degenerate, compute via Jacobian *)
Jacs=Det/@((D[den,#]&/@vars)/.(Thread[vars->#]&/@poles));
Jacs=Jacs//Simplify;
If[Count[Jacs,0]===0,
	Res=(num/.(Thread[vars->#]&/@poles))/Jacs;
	poleQ=den/.(Thread[vars->#]&/@poles)//Simplify;
	poleQ=DeleteDuplicates/@poleQ;
	poleQ=If[#==={0},1,0]&/@poleQ;
	Res=Res*poleQ;
	Return[Res];
];

If[$MultiResUseSingular===True,
(* USE SINGULAR begin =============================== *)
g=SingularUnivariatePolynomials[f,x,MonomialOrder->Lexicographic];
g=Collect[g,x];
DetA=SingularDetA[f,g,x,MonomialOrder->MonOrd];
DetA=DetA//Together;
(* USE SINGULAR end =============================== *)
,
(* USE MATHEMATICA begin =============================== *)

(* Compute univariate polynomials g *)
g=First[GroebnerBasis[f,x,#,CoefficientDomain->CoefDom,MonomialOrder->EliminationOrder,Sort->True]]&/@Table[Drop[x,{j}],{j,1,n}];
g=Collect[g,x];

(* Compute Groebnerbasis and conversionmatrix, such that gb = C.f *)
If[($VersionNumber>=11)===True,
	{gb,C}=GroebnerBasis`BasisAndConversionMatrix[f,x,{},CoefficientDomain->CoefDom,MonomialOrder->Lexicographic];
	,
	{gb,C}=conversionMatrix[f,x,CoefficientDomain->CoefDom,MonomialOrder->Lexicographic];
];

(* Write univariate polynomials in terms of Groebner basis, such that g = B.gb *)
B=First/@PolynomialReduce[g,gb,x,CoefficientDomain->CoefDom,MonomialOrder->Lexicographic];
(* Now we have that: g = (B.C).f === A.f. Compute Det[A] = Det[B.C] *)
If[Length[DeleteDuplicates[Dimensions[B]]]==1&&Length[DeleteDuplicates[Dimensions[C]]]==1,
	(* If B and C are square matrices then compute the product of the determinants *)
	DetA=Det[B]*Det[C];
	,
	(* Otherwise compute the determinant of the product *)
	DetA=Det[B.C];
];
If[$MultiResInternalChecks===True,
	(*  Check to verify that Amatrix.Ideal = factor  *)
	(* Analytically *)
	(*
	If[DeleteDuplicates[Simplify[B.C.f-g]]=!={0},
		Message[MultivariateResidue::convmat,AngleBracket@@den]
		Throw[Null];
	];
	*)
	(* Numerically *)
	zeros=(B.C.f-g)/.(Rule[#,RandomReal[{7,13},WorkingPrecision->500]]&/@Join[x,param]);
	zeros=DeleteDuplicates[Chop[zeros,0.001]];
	If[zeros=!={0},
		(* Try once more -- could have been unlucky with choice of random numbers *)
		zeros=(B.C.f-g)/.(Rule[#,RandomReal[{7,13},WorkingPrecision->500]]&/@Join[x,param]);
		zeros=DeleteDuplicates[Chop[zeros,0.001]];
		If[zeros=!={0},
			Message[MultivariateResidue::convmat,AngleBracket@@den]
			Throw[Null];
		];
	];
];
DetA=DetA//Together;
(* USE MATHEMATICA end =============================== *)
];

(* If the determinant vanishes, then the residue will vanish for any variety *)
If[DetA===0,
	Message[MultivariateResidue::detA];
	Throw[Table[0,{Length[poles]}]];
];

(* List denominator factors with their powers, after the transformation *)
DenFactors=Times@@g;
DenFactors=FactorList[DenFactors];
(* If higher powers of any variable appears in the factorlist, then fix this! *)
If[Or@@(!FreeQ[DenFactors,Power[#,_]]&/@x),
	(* First factorize the base factors *)
	DenFactors=Join[
		DenFactors[[1;;1]],
		{Times@@(Flatten[Solve[#[[1]]==0,Intersection[Variables[#[[1]]],x]]]/.{Rule[a_,b_]:>a-b}),#[[2]]}&/@DenFactors[[2;;-1]]
	];
	(* Then split the base factors containing products into separate base factors *)
	DenFactors=DenFactors/.({t_Times,n_}:>Sequence@@({#,n}&/@(List@@t)));
	(* Add up powers of equal base factors that may have been produced *)
	DenFactors={#[[1,1]],Total[Last/@#]}&/@GatherBy[DenFactors,First];
];

(* Extract the product of leading coefficients *)
DenFactorsCoef=Times@@((LC[#[[1]],x]^#[[2]])&/@DenFactors);

(* Normalize all leading coefficients to one *)
DenFactors={NormalizeLC[#[[1]],x],#[[2]]}&/@DenFactors;
DenFactors=DenFactors//Simplify;

(* Construct ProductGs = Times@@g which now has the form: Const*(x1-a1)*(x1-b1)*...*(x2-a2)*(x2-b2)*...*(xn-an)*(xn-bn)*... *)
ProductOfGs=DenFactorsCoef*(Times@@((First[#]^Last[#])&/@DenFactors));

(* Compute residue at each variety *)
Do[
	(* If not all original denominator factors f vanish on the pole, then the corresponding residue is zero *)
	If[Simplify[f/.Thread[x->poles[[p]]]]=!=ConstantArray[0,n],
		Res[p]=0;
		Continue[]
	];
	
	PoleFactors=Simplify[x-poles[[p]]];

	(*  Order of each univariate pole  *)
	OrderOfPoles=Table[Select[DenFactors,Simplify[First[#]-PoleFactors[[j]]]===0&],{j,1,n}];

	If[DeleteDuplicates[Length/@OrderOfPoles]=!={1},
		Message[MultivariateResidue::poleord,poles[[p,1]],AngleBracket@@den];
		Throw[Null];
	];
	OrderOfPoles=#[[1,2]]&/@OrderOfPoles;

	(*  Pole-containing part of the denominator  *)
	PoleMonomial=Times@@Thread[Power[PoleFactors,OrderOfPoles]];
	(* Construct list of derivatives *)
	ListOfDerivatives=Thread[{x,OrderOfPoles-ConstantArray[1,n]}];
	(* Find a convenient order of variables to take derivatives *)
	pos=Transpose[{Range[n],poles[[p]],OrderOfPoles}];
	pos=SortBy[pos,{
		If[#[[2]]===0,0,1],(* first the variables that are set to zero;  otherwise... *)
		If[Variables[#[[2]]]==={},0,1],(* first the variables that are set to a number (free of parameters);  otherwise... *)
		#[[3]],(* first the variables with a low pole order;  otherwise... *)
		ByteCount[#[[2]]](* first the variables with a small expression size *)
		}&];
	pos=First/@pos;
	
	(* Take derivatives *)
	t[0]=Together[PoleMonomial/ProductOfGs]*DetA*h;
	Do[
		t[i]=D[
			t[i-1],
			ListOfDerivatives[[ pos[[i]] ]]
		]/.(x[[ pos[[i]] ]]->poles[[p, pos[[i]] ]])//Together;
	,{i,1,n}
	];
	
	Res[p]=t[n]/Product[(OrderOfPoles[[i]]-1)!,{i,1,n}];
	
,{p,1,Length[poles]}
];

Return[Table[Res[p],{p,1,Length[poles]}]]
];
]


(* Finds a Groebner basis wrt lex order, and a conversion matrix. *)
conversionMatrix[polys_,vars_,opts___] := Module[
{time, coords,aa, pmat, len , newpolys, extrapoly, gbPOT, gb, convmat, fvar, rvars},

len= Length[polys];
coords = Array[aa, len + 1];
fvar = First[coords];
rvars = Rest[coords];
pmat = Transpose[Join[{polys}, IdentityMatrix[len]]];
newpolys = pmat.coords;

(* Groebner basis of a set of vectors wrt POT order *)
extrapoly = Flatten[Table[coords[[j]]*coords[[k]], {j, len+1}, {k, j, len+1}]];
gbPOT = GroebnerBasis[Join[newpolys, extrapoly], Join[coords, vars], opts];
(* remove the elements in the basis that contain an element of extra *)
gbPOT = Flatten[gbPOT/.Map[(# :> {}) &, extrapoly]];
gbPOT = Collect[gbPOT, cvars];


gb = gbPOT /. Join[{fvar -> 1}, Thread[rvars -> 0]] /. (0 :> Sequence[]);
convmat = Select[gbPOT, ! FreeQ[#, fvar] &] /. fvar -> 0;

Return[{gb, convmat /. Thread[rvars -> Table[UnitVector[len, j], {j, len}]]}]
]


(* ::Subsubsection::Closed:: *)
(*MultivariateResidues2 (QuotientRingDuality)*)


MultivariateResidue::delta="Value of \[Delta] has increased beyond \[Delta]=100. Continuing with the option FindMinumumDelta\[Rule]False.";
MultivariateResidue::inv="Matrix is not invertible! Try setting CoefficientDomain\[Rule]RationalFunctions.";
MultivariateResidue::unity="Partition of unity does not sum to one.";
MultivariateResidue::empty="None of the poles `1` are points in the variety of the ideal `2`. Residue is zero.";
MultivariateResidue::canb="Construction of canonical basis was unsuccessful.";
MultivariateResidue::missroot="Error: the given roots `1` seem to be absent in the ideal. Hint: if the given roots are simplified with certain assumptions, then please specify those assumptions in $Assumptions.";

Options[MultivariateResidue2]={
	MonomialOrder->Inherited,
	CoefficientDomain->Inherited,
	FindMinimumDelta->Inherited,
	ReturnGlobalResidue->False
};

MultivariateResidue2[num_,den_,vars_,poles_,opts:OptionsPattern[]]:=Catch[Module[
{t,MonOrd,CoefDom,Minimum\[Delta],
Vars,x,Num,Ideal,Variety,
GBLex,PolRemainderLex,
GB,CanonicalBasis,DualBasis,CanonicalBasisExponentVectors,
FinishCoefficientList,MCan,CanonicalToDualConversionMatrix,
M0,\[Mu],ProjectionVectors,i0,j0,L,\[Lambda],g,
MGenerators,GeneratorsOfIntersection,\[Delta],MatrixOfP,ListOfMultiplicities,
eigenvalues,projectedvariety,
\[Lambda]Max,DiagonalizabilityTest,PreGeneratorsOfIntersection,e,NumRemainder,\[CapitalLambda]0,\[CapitalLambda],Res,ResList,
Alist,A,cr,eq0,eq1,J,Lagrange,elist,
PolyPowRem,PolyRem,PolyRemLex,zeros,missingroots
},


If[$MultiResInputChecks===True,
	CheckInput[num,den,vars,poles];
];

MonOrd=OptionValue[MonomialOrder];
CoefDom=OptionValue[CoefficientDomain];
Minimum\[Delta]=OptionValue[FindMinimumDelta];

Vars=x/@Range[1,Length[vars]];
Num=num/.Thread[vars->Vars];
Ideal=den/.Thread[vars->Vars];

If[poles==={GlobalResidue},
	Variety={GlobalResidue};
];

If[poles=!={GlobalResidue},
	(* Compute the entire variety internally. At the end, connect the results to the user-supplied poles *)
	Variety=DeleteDuplicates[Map[Last,Quiet[Solve[#==0&/@den,vars]],{2}]//Simplify];
	missingroots=Select[poles//Simplify,!MemberQ[Variety,#]&];
	If[missingroots=!={},
	Message[MultivariateResidue::missroot,missingroots];
	Return[Null];
	];
	If[Intersection[poles//Simplify,Variety]==={},
		zeros=Flatten[Table[poles[[i]]-Variety[[j]],{i,1,Length[poles]},{j,1,Length[Variety]}],1];
		zeros=zeros/.(Rule[#,RandomReal[{7,13},WorkingPrecision->500]]&/@Variables[zeros]);
		zeros=DeleteDuplicates/@Chop[zeros,0.001];
		If[!MemberQ[zeros,{0}],
			Message[MultivariateResidue::empty,poles,AngleBracket@@den];
			Return[Table[0,{Length[poles]}]];
		];
	];
];


GBLex=Simplify[GroebnerBasis[Ideal,Vars,MonomialOrder->Lexicographic,CoefficientDomain->CoefDom]];
If[MonOrd===Lexicographic,
	GB=GBLex,
	GB=Simplify[GroebnerBasis[Ideal,Vars,MonomialOrder->MonOrd,CoefficientDomain->CoefDom]]
];

PolyRemLex[poly_]:=PolyRemainder[poly,GBLex,Vars,MonomialOrder->Lexicographic];
PolyRem[poly_]:=PolyRemainder[poly,GB,Vars,MonomialOrder->MonOrd];
PolyPowRem[poly_,power_]:=PolyPowerRemainder[poly,power,GB,Vars,MonomialOrder->MonOrd];


(* Compute canonical and dual bases *)
{CanonicalBasis,DualBasis}=CanonicalAndDualBases[Ideal,Vars,MonOrd,GB];
CanonicalBasisExponentVectors=#[[1,1]]&/@CoefficientRules[CanonicalBasis,Vars,MonOrd];
FinishCoefficientList[x_]:=If[Head[x]===List,0,x];

(*  Decompose 1 in the dual basis of \[DoubleStruckCapitalC][Subscript[x, 1],...,Subscript[x, n]]/Ideal by first decomposing 1 in the 
   canonical basis and then converting the result into the dual basis.  *)
Alist=Table[A[i],{i,1,Length[DualBasis]}];
cr=CoefficientRules[DualBasis.Alist,Vars,MonOrd];
eq0=(Last/@Select[cr,First[#]=!=Table[0,{Length[Vars]}]&]);
eq1=(Last/@Select[cr,First[#]===Table[0,{Length[Vars]}]&]);
If[Length[CanonicalBasis]>50,
	(* for ''large'' systems of equations, first perform polynomial division of ''1'' wrt groebner basis of ''zeros'' *)
	eq0=GroebnerBasis[eq0,Alist,MonomialOrder->MonOrd,CoefficientDomain->CoefDom];
	eq1=(Last/@PolynomialReduce[eq1,eq0,Alist]);
];
eq0=0==#&/@eq0;
eq1=1==#&/@eq1;
\[Mu]=Alist/.First[Solve[Join[eq0,eq1],Alist]];

(* When asked for the Global Residue *)
If[poles==={GlobalResidue},
	e[1]=1;
	Goto[ComputeResidue];
];

(*  Compute Lagrangian interpolation polynomials  *)
ProjectionVectors[x_]:=Tuples[Range[-x,x],Length[vars]];
Do[i0=i1;
	j0=1;
	While[Length[Union[Simplify[ProjectionVectors[i1][[j0]].Vars/.Thread[Vars->#]&/@Variety]]]=!=Length[Variety],
		If[j0>=Length[ProjectionVectors[i1]],Break[]];
		j0++
	];
	If[Length[Union[Simplify[ProjectionVectors[i1][[j0]].Vars/.Thread[Vars->#]&/@Variety]]]===Length[Variety],Break[]]
,{i1,1,10}];

L[X_]:=ProjectionVectors[i0][[j0]].X;
\[Lambda][X_,j1_]:=L[X-Variety[[j1]]];

Do[
	g[p]=Factor[Product[\[Lambda][Vars,j1],{j1,Drop[Range[Length[Variety]],{p}]}]/Product[\[Lambda][Variety[[p]],j1],{j1,Drop[Range[Length[Variety]],{p}]}]]
,{p,1,Length[Variety]}];



If[Minimum\[Delta]===False,Goto[DetermineRootMultiplicity2]];
(*  Determine root multiplicity  (1)  *)
Label[DetermineRootMultiplicity1];


(*  References:
[IVA] = David Cox, John Little, Donal O\[CloseCurlyQuote]Shea: "Ideals, Varieties, and Algorithms"
 [UAG] = David Cox, John Little, Donal O\[CloseCurlyQuote]Shea: "Using Algebraic Geometry"  *)

(*  The aim of the code below is to compute the smallest possible \[Delta] satisfying part a of Lemma (2.3)
   in Section 4.2 of [UAG]. Namely, the desired partition functions Subscript[e, i] associated with each distinct
  pole Variety[[i]] can then be constructed as Subscript[e, i] = 1 - ((1 - g[i]^\[Delta])^\[Delta]).  *)

(*  In the below notation, Subscript[M, i] denotes the ideal of the variety {Variety[[i]]}. As stated below 
   Definition (2.1) in Section 4.2 of [UAG], Subscript[M, i] has
   the generators {z[1]-Variety[[i,1]],...,z[n]-Variety[[n,1]]}.  *)
Do[MGenerators[p]=Vars-Variety[[p]],{p,1,Length[Variety]}];


(* Compute delta with Singular *)
If[$MultiResUseSingular===True,
	J=Table[MGenerators[p],{p,1,Length[Variety]}];
	\[Delta]=SingularFindMinimumDelta[Ideal,J,Vars,MonomialOrder->MonOrd];
	Goto[ComputePartitionFunctions];
];


(*  The below code uses the algorithm described below Theorem 11 in Section 4.3 of [IVA] to find a list
of generators of the ideal Subscript[M, 1]\[Intersection]...\[Intersection]Subscript[M, Length[Variety]].  *)
GeneratorsOfIntersection[0,0]=MGenerators[1];
Do[
	GeneratorsOfIntersection[0,j]=Factor[Select[GroebnerBasis[
		Join[t GeneratorsOfIntersection[0,j-1],(1-t)MGenerators[j+1]],
		Insert[Vars,t,1],
		MonomialOrder->Lexicographic
	],FreeQ[#,t]&]];
,{j,1,Length[Variety]-1}];



GeneratorsOfIntersection[1]=Union[PolyRem/@GeneratorsOfIntersection[0,Length[Variety]-1]];

If[Length[GeneratorsOfIntersection[1]]>1,
	GeneratorsOfIntersection[1]=Select[GeneratorsOfIntersection[1],#=!=0&];
];
(*  The below code finds the smallest \[Delta] such that J^\[Delta] \[SubsetEqual] Ideal.
 g[\[Delta]] is the list of generators of J^\[Delta] (cf. Proposition 6 of Section 4.3 of [IVA]).
 The statement that J^\[Delta] \[SubsetEqual] Ideal is equivalent to the statement that all elements of g[\[Delta]] have 
  a vanishing remainder after polynomial division by (a Gr\[ODoubleDot]bner basis of) Ideal 
  (cf. Exercise 2 of Section 1.4 of [IVA]).  *)
\[Delta]=1;
While[GeneratorsOfIntersection[\[Delta]]=!={0},
	\[Delta]++;

	If[\[Delta]>100,
		Message[MultivariateResidue::delta];
		Goto[DetermineRootMultiplicity2];
	];
	
	PreGeneratorsOfIntersection[\[Delta]]=Table[
		GeneratorsOfIntersection[1][[i]]*GeneratorsOfIntersection[\[Delta]-1][[j]]
	,{i,1,Length[GeneratorsOfIntersection[1]]}
	,{j,i,Length[GeneratorsOfIntersection[\[Delta]-1]]}]//Flatten//DeleteDuplicates;
	
	
	GeneratorsOfIntersection[\[Delta]]=DeleteDuplicates[PolyRem/@PreGeneratorsOfIntersection[\[Delta]]];
	
];
If[Minimum\[Delta]===True,Goto[ComputePartitionFunctions]];




(*  Determine root multiplicity  (2)  *)
Label[DetermineRootMultiplicity2];

MatrixOfP[P_]:=Module[{ImageOfP},
	ImageOfP[P,z_]:=Module[{rem},
		rem=PolyRem[P*z];
		FinishCoefficientList/@(CanonicalBasisExponentVectors/.CoefficientRules[rem,Vars,MonOrd])
	];
	ImageOfP[P,#]&/@CanonicalBasis
];

If[Length[CanonicalBasis]>50,
	eigenvalues=Simplify[Solve[CharacteristicPolynomial[MatrixOfP[L[Vars]],x]==0,x]];
	ListOfMultiplicities=Tally[Last/@First/@eigenvalues];
	\[Lambda]Max=Max[Last/@ListOfMultiplicities];
	\[Delta]=\[Lambda]Max;
,	
	eigenvalues=Simplify[Eigenvalues[MatrixOfP[L[Vars]]]];
	projectedvariety=Simplify[L/@Variety];
	ListOfMultiplicities=Count[eigenvalues,#]&/@projectedvariety;
	\[Lambda]Max=Max[ListOfMultiplicities];
	\[Delta]=\[Lambda]Max;
];


Label[ComputePartitionFunctions];
(*  Having determined the Lagrangian interpolation polynomials g[i] and the "multiplicity" \[Delta] according to
   Lemma (2.3) in Section 4.2 of [UAG], we now construct the partition functions e[i] as the remainder
  of 1-(1-g[i]^\[Delta])^\[Delta] after polynomial division over (a Gr\[ODoubleDot]bner basis of) the ideal.  *)


If[$MultiResUseSingular===True,
	Lagrange=Table[g[p],{p,1,Length[Variety]}];
	elist=SingularPartitionOfUnity[Ideal,Lagrange,\[Delta],Vars,MonomialOrder->MonOrd];
	Do[e[p]=elist[[p]],{p,1,Length[Variety]}];
	If[Simplify[Sum[e[p],{p,1,Length[Variety]}]]=!=1,
		Message[MultivariateResidue::unity];
		Throw[Null];
	];
	Goto[ComputeResidue];
];


If[$MultiResInternalChecks===True,
	(* METHOD 1 *)
	Do[
		e[p]=PolyRem[1-PolyPowRem[1-PolyPowRem[g[p],\[Delta]],\[Delta]]];
	,{p,1,Length[Variety]}];
	If[Simplify[Sum[e[p],{p,1,Length[Variety]}]]=!=1,
		Message[MultivariateResidue::unity];
		Throw[Null];
	];
	,
	(* METHOD 2 *)
	Do[
		e[p]=PolyRem[1-PolyPowRem[1-PolyPowRem[g[p],\[Delta]],\[Delta]]];
	,{p,1,Length[Variety]-1}];
	e[Length[Variety]]=1-Sum[e[p],{p,1,Length[Variety]-1}];
];


Label[ComputeResidue];

Do[NumRemainder[p]=PolyRem[Num e[p]],{p,1,Length[Variety]}];

(*  Decompose NumRemainder[p] in the canonical basis of \[DoubleStruckCapitalC][Subscript[x, 1],...,Subscript[x, n]]/Ideal  *)
Do[
	\[CapitalLambda]0[p]=FinishCoefficientList/@(CanonicalBasisExponentVectors/.CoefficientRules[NumRemainder[p],Vars,MonOrd]);
	
	If[Simplify[Expand[NumRemainder[p]-\[CapitalLambda]0[p].CanonicalBasis]]=!=0,
		Message[MultivariateResidue::canb];
		Throw[Null];
	];
	\[CapitalLambda][p]=Factor[\[CapitalLambda]0[p]];
,{p,1,Length[Variety]}];

Do[Res[p]=Factor[Dot[\[CapitalLambda][p],\[Mu]]],{p,1,Length[Variety]}];

(* When asked for the Global Residue *)
If[poles==={GlobalResidue},
	Return[Res[1]]
];

ResList=Table[Res[p],{p,1,Length[Variety]}];
(* Connect the Residues on each Variety to the requested Residues on the user-supplied poles *)
ResList=Extract[ResList,Position[Variety,#]]&/@Simplify[poles]/.{{}->{0}}//Flatten;

Return[ResList]

];
];


(* Reduce polynomial p *)
PolyRemainder[p_,gb_List,vars_List,opts__]:=Last[PolynomialReduce[p,gb,vars,opts]];

(* Reduce power of polynomial p^n *)
PolyPowerRemainder[p_,n_Integer,gb_List,vars_List,opts__]:=Module[{base2,pRed,time},
	base2=Reverse[IntegerDigits[n,2]];
	pRed[1]=PolyRemainder[p,gb,vars,opts];
	If[n>1,
		Do[pRed[2^i]=PolyRemainder[pRed[2^(i-1)]^2,gb,vars,opts];,{i,1,Length[base2]-1}];
		pRed[n]=DeleteCases[base2*Table[pRed[2^(i-1)],{i,1,Length[base2]}],0];
		pRed[n]=If[pRed[n]==={},0,Times@@pRed[n]];
		pRed[n]=PolyRemainder[pRed[n],gb,vars,opts];
	];
	Return[pRed[n]]
];


(*  Compute canonical and dual bases of \[DoubleStruckCapitalC][Subscript[x, 1], ..., Subscript[x, n]]/Ideal  *)
CanonicalAndDualBases[ideal_,vars_,MonOrd_,GB_]:=Module[
{Vars,x,y,Ideal,Gx,Gy,LeadingTerms,LeadTermExponentVectors,MaxExponent,BasisExponentVectors,
CanonicalBasisBeforeSorting,CanonicalBasis,CanonicalBasisExponentVectors,BezoutianMatrix,
BezoutianDeterminant,BezDetRemainderX,BezDetRemainderY,BezDetRemainder,FinishCoefficientList,
DualBasisY,DualBasisX,DualBasis},

Vars=x/@Range[1,Length[vars]];
Ideal=ideal/.Thread[vars->Vars];
Gx=GB/.Thread[vars->Vars];

(*  Generate a Gr\[ODoubleDot]bner basis of the ideal, and create a copy for the later purpose of polynomial division of
   the Bezoutian determinant.  *)
Gy=Gx/.Table[x[k]->y[k],{k,1,Length[Vars]}];

(*  Generate a list of the leading term of each Gr\[ODoubleDot]bner basis element  *)
LeadingTerms=MonomialList[#,Vars,MonOrd][[1]]&/@Gx;

(*  Generate the exponent vector for each element of LeadingTerms  *)
LeadTermExponentVectors=CoefficientRules[#,Vars][[1,1]]&/@LeadingTerms;

MaxExponent=Max[Union@@LeadTermExponentVectors];

(*  For each element of LeadTermExponentVectors, say {Subscript[m, 1],...,Subscript[m, Length[Vars]]}, enumerate the points 
  within {0,...,MaxExponent}^Length[Vars]  whose i'th component is strictly less than Subscript[m, i]
  for at least one i=1,...,Length[Vars]. Then take the intersection of the resulting sets.  *)
BasisExponentVectors=Intersection@@Table[Select[Tuples[Range[0,MaxExponent],{Length[Vars]}],Or@@Thread[Less[#,LeadTermExponentVectors[[i]]]]&],{i,1,Length[Gx]}];

(*  Compute the canonical linear basis of \[DoubleStruckCapitalC][Subscript[x, 1],...,Subscript[x, n]]/Ideal  *)
CanonicalBasisBeforeSorting=Times@@Thread[Power[Vars,#]]&/@BasisExponentVectors;
CanonicalBasis=MonomialList[Plus@@CanonicalBasisBeforeSorting,Vars,MonOrd];

CanonicalBasisExponentVectors=#[[1,1]]&/@CoefficientRules[CanonicalBasis,Vars,MonOrd];

(*  Generate Bezoutian matrix and determinant  *)
BezoutianMatrix=Table[Simplify[1/(x[j]-y[j]) ((Ideal[[i]]/.Table[x[k]->y[k],{k,1,j-1}])-(Ideal[[i]]/.Table[x[k]->y[k],{k,1,j}]))],{i,1,Length[Vars]},{j,1,Length[Vars]}];

BezoutianDeterminant=Det[BezoutianMatrix];

(*  Perform polynomial division of the Bezoutian determiant over R \[CircleTimes] R, where R=(\[DoubleStruckCapitalC][Subscript[x, 1],...,Subscript[x, n]]/Ideal)  *)
BezDetRemainderX=PolyRemainder[BezoutianDeterminant,Gx,Vars,MonomialOrder->MonOrd];
BezDetRemainderY=PolyRemainder[BezDetRemainderX,Gy,Vars/.Table[x[k]->y[k],{k,1,Length[Vars]}],MonomialOrder->MonOrd];

BezDetRemainder=Expand[BezDetRemainderY];

(*  Construct the dual basis of \[DoubleStruckCapitalC][Subscript[x, 1],...,Subscript[x, n]]/Ideal as the expansion coefficients of the 
   Bezoutian determinant in the canonical basis. *)
FinishCoefficientList[x_]:=If[Head[x]===List,0,x];

DualBasisY=FinishCoefficientList/@(CanonicalBasisExponentVectors/.CoefficientRules[BezDetRemainder,Vars,MonOrd]);

DualBasisX=DualBasisY/.Table[y[k]->x[k],{k,1,Length[Vars]}];

DualBasis=PolyRemainder[#,Gx,Vars,MonomialOrder->MonOrd]&/@DualBasisX;

{CanonicalBasis,DualBasis}/.Thread[Vars->vars]
];


(* ::Subsubsection:: *)
(*GlobalResidueTheoremCPn*)


Quiet[Get["Combinatorica`"]];

GlobalResidueTheoremCPn[Num_,Ideal_,Vars_]:=Module[
{i,ProjectivizeVars,VarsOfPatch,NormalizePoint,NumeratorDegree,VectorOfDenominatorDegrees,ProjectivizedIdeal,RescalingPower,ProjectivizedNumerator,ListOfDivisors,ProjectivizedIdealFactors,PartitionsOfDivisors,IndicesOfDiscreteDivisorIntersections,PatchVars,IntersectionOfDivisors,PolesInGRT,ResiduesInGRT,PatchLabel,PatchLabels,ResidueAtPole,PositionsOfNonVanishingResidues},

ProjectivizeVars=Table[Vars[[i]]->w[i]/w[0],{i,1,Length[Vars]}];
Do[(*loop over i*)
VarsOfPatch[i]=Drop[Table[w[j],{j,0,Length[Vars]}],{i+1}]
,{i,0,Length[Vars]}
];


NormalizePoint[x_]:=Module[{FirstNonVanishingEntry},

FirstNonVanishingEntry=Min[Flatten[Position[#=!=0&/@x,True]]];
Return[Simplify[x/x[[FirstNonVanishingEntry]]/.w[FirstNonVanishingEntry-1]->1]];
];

NumeratorDegree=Max[Total[#[[1]]]&/@CoefficientRules[Num,Vars]];

VectorOfDenominatorDegrees=Table[Max[Total[#[[1]]]&/@CoefficientRules[i,Vars]],{i,Ideal}];

ProjectivizedIdeal=Factor[(Ideal/.ProjectivizeVars)Table[w[0]^i,{i,VectorOfDenominatorDegrees}]];

(*  \[Omega] on patch Subscript[U, i] of CP^n  *)
(*  ((-1)^iNum)/(w[0]^(Length[Vars]+1+NumeratorDegree-Total[VectorOfDenominatorDegrees])Times@@ProjectivizedIdeal)  *)

RescalingPower=Length[Vars]+1+NumeratorDegree-Total[VectorOfDenominatorDegrees];

If[
RescalingPower<0,
ProjectivizedNumerator=Expand[w[0]^(-RescalingPower+NumeratorDegree)(Num/.ProjectivizeVars)],
ProjectivizedNumerator=Expand[w[0]^(NumeratorDegree)(Num/.ProjectivizeVars)]
];

ProjectivizedIdealFactors=Select[Power@@#&/@FactorList[Times@@ProjectivizedIdeal],!FreeQ[#,w]&];

If[
RescalingPower>0,
ListOfDivisors=Join[{w[0]^RescalingPower},ProjectivizedIdealFactors],ListOfDivisors=ProjectivizedIdealFactors
];

PartitionsOfDivisors=Map[Times@@#&,KSetPartitions[ListOfDivisors,Length[Vars]],{2}];

IndicesOfDiscreteDivisorIntersections={};


Monitor[Do[(*Loop over i*)


PatchVars=Reverse[Subsets[Table[w[j],{j,0,Length[Vars]}],{Length[Vars]}]];
(* For a given partition find the divisors and their intersection *)
Quiet[
IntersectionOfDivisors[i]=Union[
Flatten[
Table[
Solve[PartitionsOfDivisors[[i]]==0&&PatchVars[[pv]][[1;;pv-1]]==0,PatchVars[[pv]]]
,{pv,1,Length[PatchVars]}
]
,1]];
];

If[
Union[Length/@IntersectionOfDivisors[i]]=!={Length[Vars]},
Continue[]
];

IndicesOfDiscreteDivisorIntersections=Join[IndicesOfDiscreteDivisorIntersections,{i}];

PolesInGRT[i]=SortBy[DeleteDuplicates[NormalizePoint[Table[w[j],{j,0,Length[Vars]}]/.#]&/@IntersectionOfDivisors[i]],Reverse];

ResiduesInGRT[i]={};

PatchLabels=(Flatten[Position[#,1]][[1]]-1)&/@PolesInGRT[i];
PolesInGRT[i]=Transpose[{PolesInGRT[i],PatchLabels}];
PolesInGRT[i]=GatherBy[PolesInGRT[i],Last];

Do[(* loop over PolesInGRT[i] *)
PatchLabel=PoleSet[[1,2]];

ResidueAtPole=MultivariateResidue[
(-1)^PatchLabel*ProjectivizedNumerator/.w[PatchLabel]->1,PartitionsOfDivisors[[i]]/.w[PatchLabel]->1,
VarsOfPatch[PatchLabel],
Drop[#,{PatchLabel+1}]&/@First/@PoleSet
];
ResidueAtPole=Factor/@ResidueAtPole;

ResiduesInGRT[i]=Join[ResiduesInGRT[i],ResidueAtPole];

,{PoleSet,PolesInGRT[i]}
];
PolesInGRT[i]=Flatten[Map[First,PolesInGRT[i],{2}],1];
PositionsOfNonVanishingResidues[i]=Flatten[Position[#=!=0&/@ResiduesInGRT[i],True]];

,{i,1,Length[PartitionsOfDivisors]}
],
ToString[i]<>"/"<>ToString[Length[PartitionsOfDivisors]]
];

Return[Table[
{
PartitionsOfDivisors[[i]],
{
PolesInGRT[i][[PositionsOfNonVanishingResidues[i]]],
ResiduesInGRT[i][[PositionsOfNonVanishingResidues[i]]]
}
}
,{i,IndicesOfDiscreteDivisorIntersections}
]];
];


(* ::Subsubsection::Closed:: *)
(*End*)


End[];
EndPackage[];
Print["MultivariateResidues, Kasper J. Larsen and Robbert Rietkerk (2017-2018)"];
