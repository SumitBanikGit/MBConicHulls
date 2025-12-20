(* ::Package:: *)

(* ::Section:: *)
(*Package Initialization*)


(* ::Subsection::Closed:: *)
(*Load Dependencies*)


BeginPackage["MBConicHulls`"];


Quiet[Needs["Combinatorica`"];]
Block[{Print}, Get["MultivariateResidues`"];]
Off[SymbolName::sym];


(* ::Subsection:: *)
(*Command Usage*)


MBRep::"usage"="MBRep[PreFac_,IntVar_,MBVar_,MBArg_,MBNumber->1,Substitute->{},TakeLimit->{},CanonicalTransform->True,PolygammaToGamma->True] inputs the MB integral in a form that can be processed by the package where :\n\
PreFac - is the prefactor of the MB integral.\n\
IntVar - takes in the list of integration variables or the list of integration variables and their contour lines in case of MB with straight contours.\n\
MBVar - takes in the list of parameters, where MBVar[[i]] is raised to the power IntVar[[i]].\n\
MBArg - is a list of the form {{numerator},{denominator}}, where the elements of {numerator} and {denominator} are the arguments of the gamma functions in the numerator and denominator of the MB integrand, respectively. For polygamma functions (allowed only in the numerator), we input it as a list, with the first element being the order and the second being the argument of the function.  \n\
Substitute - is an optional parameter , to substitute the values of parameters appearing in the MB integrand. It is by default {}.\n\
TakeLimit - is an optional parameter, which specifies the limits on parameters to be taken while transforming a straight to a non-straight MB. It is by default {}. \n\
CanonicalTransform - is an optional parameter, which specifies whether to transform the input MB into a canonical MB. Its default value is True.
PolygammaToGamma - is an optional parameter, which specifies whether to rewrite numerator polygamma functions whose poles are split by straight contour as derivatives of gamma functions in the input MB. Its default value is True.
";
ResolveMB::"usage"="ResolveMB[MBRepOut_,MBNumber->1,MaxSolutions->Infinity,PrintSolutions->True,MasterSeries->True]  returns the type of integral (degenerate or nondegenerate) and the total number of conic hulls, and then goes on to display the list of relevant intersecting conic hulls and the sets of poles for each series representation along with the master series characteristic list and variables for the degenerate case. The function arguments are: \n\
MBRepOut - is the output of the MBRep[] function. \n\
MBNumber:: is an optional parameter which specifies the number of the MB to be solved among the ones returned by MBRep[]. Its default value is 1. \n\
MaxSolutions - is an optional parameter which specifies the maximum number of series solutions of the MB integral that one wishes to evaluate. Its default value is Infinity. \n\
PrintSolutions - is an optional parameter which specifies whether to print the list of possible solutions along with their list of poles or not. Its default value is True.\n\
MasterSeries - is an optional parameter which specifies whether to compute the master series for each of the series solutions found or not. Its default value is True.
";
EvaluateSeries::"usage"="EvaluateSeries[ResolveMBOut_, MBParaSub_, SeriesNum_,PrintSeries->True] calculates and prints the series expression of the series representation SeriesNum. The function arguments are: \n\
ResolveMBOut - is the output of the ResolveMB[] or TriangulateMB[] function. \n\
MBParaSub - is a list of substitutions to be made to the parameters in the arguments of the gamma functions of the integrand. \n\
SeriesNum - is the number, as enumerated in the output of ResolveMB[] or TriangulateMB[], of the series for which we wish to calculate residues. \n\
PrintSeries - is an optional parameter, which specifies whether to print the derived analytic solution or not. Its default value is True.
";
SumAllSeries::"usage"="SumAllSeries[EvaluateSeriesOut_,MBVarSub_,SumLim_,RunInParallel-> Bool,NumericalPrecision -> PositiveIntegers] numerically sums the series of the particular series representation evaluated by EvaluateSeries[] and prints the result. The function arguments are: \n\
EvaluateSeriesOut - is the output of the EvaluateSeries[] function. \n\
MBVarSub - is a list of substitutions to provide numerical values to the parameters in MBVar. \n\
SumLim - is the upper limit of all the summation indices of the residue sum. \n\
RunInParallel - is an optional parameter indicating whether the sum has to be performed using Mathematica's parallel processing functionality or not. The default value is False. \n\
NumericalPrecision - is an optional parameter of SumAllSeries[], which determines the precision of the numerical value of the series representation. The default MachinePrecision.";
n::"usage"="Summation Indices";
\[Iota]::"usage"="Dummy Indices";
\[Omicron]::"usage"="Dummy Indices";
Substitute::"usage"=" is an optional parameter of MBRep[], to substitute the values of parameters appearing in the MB integrand. It is by default {}.";
TakeLimit::"usage"=" is an optional parameter of MBRep[], which specifies the limits on parameters to be taken while transforming a straight to a non-straight MB. It is by default {}.";
RunInParallel::"usage"=" is an optional parameter of SumAllSeries[], indicating whether the sum to be performed using Mathematica's parallel processing functionality. It is by default False.";
NumericalPrecision::"usage"=" is an optional parameter of SumAllSeries[], which determines the precision of the numerical value of the series representation. It is by default MachinePrecision.";

CrossCheckSolutions::"usage"=" It will be updated in future. ";
TriangulateMB::"usage"="TriangulateMB[MBRepOut_,MBNumber->1,MasterSeries->True,MaxSolutions->Infinity,PrintSolutions->True,ShortestOnly->False,TopComPath-> /usr/local/bin/
,TopComParallel->True,Cardinality->None,MaxCardinality->None,SolutionSummary->False,QuickSolve->False]  first computes the point configuration associated to the MB integral. It then calls TOPCOM to find all the possible triangulations and prints the set of poles for each possible series solutions.  The function arguments are: \n\
MBRepOut - is the output of the MBRep[] function. \n\
MBNumber:: is an optional parameter which specifies the number of the MB to be solved among the ones returned by MBRep[]. Its default value is 1. \n\
MaxSolutions - is an optional parameter which specifies the maximum number of series solutions of the MB integral that one wishes to evaluate. Its default value is Infinity. \n\
MasterSeries - is an optional parameter which specifies whether to compute the master series for each of the series solutions found or not. Its default value is True. \n\
TopComParallel - is an optional parameter which specifies whether to run TOPCOM in parallel or not. Its default value is True. \n\
Cardinality - is an optional parameter which specifies the length of solutions which have to be considered. Its default value is None. \n\
MaxCardinality - is an optional parameter which specifies the maximum length of the solutions to be considered. Its default value is None. \n\
SolutionSummary - is an optional parameter which specifies whether to only print a summary of possible solutions along with their cardinality. Its default value is False. \n\
QuickSolve - is an optional parameter which specifies whether to find only the quickest possible solution. This is useful for higher-fold MB integrals. Its default value is False. \n\
TopComPath - is an optional parameter which specifies the path to the TOPCOM executables. Its default value is /usr/local/bin/. \n\
PrintSolutions - is an optional parameter which specifies whether to print the list of possible solutions along with their list of poles or not. Its default value is True. \n\
ShortestOnly - is an optional parameter which specifies whether to only print the solution with shortest number of sets of poles or not. Its default value is False. \n\
";
CanonicalTransform::"usage"=" is an optional parameter of MBRep[] which specifies whether to transform the input MB into a canonical MB. Its default value is True.";
PolygammaToGamma::"usage"=" is an optional parameter of MBRep[] which specifies whether to rewrite numerator polygamma functions whose poles are split by straight contour as derivatives of gamma functions in the input MB. Its default value is True.";
MaxSolutions::"usage"=" is an optional parameter of TriangulateMB[] and ResolveMB[] which specifies the maximum number of series solutions of the MB integral that one wishes to evaluate. Its default value is Infinity.";
MBNumber::"usage"=" is an optional parameter of TriangulateMB[] and ResolveMB[] which specifies the number of the MB to be solved among the ones returned by MBRep[]. Its default value is 1.";
MasterSeries::"usage"=" is an optional parameter of TriangulateMB[] and ResolveMB[] which specifies whether to compute the master series for each of the series solutions found or not. Its default value is True.";
TopComParallel::"usage"="is an optional parameter of TriangulateMB[] which specifies whether to run TOPCOM in parallel or not. Its default value is True.";
Cardinality::"usage"="is an optional parameter of TriangulateMB[] which specifies the length of solutions which have to be considered. Its default value is None.";
MaxCardinality::"usage"="is an optional parameter of TriangulateMB[] which specifies the maximum length of the solutions to be considered. Its default value is None.";
SolutionSummary::"usage"="is an optional parameter of TriangulateMB[] which specifies whether to only print a summary of possible solutions along with their cardinality. Its default value is False.";
QuickSolve::"usage"="is an optional parameter of TriangulateMB[] which specifies whether to find only the quickest possible solution. This is useful for higher-fold MB integrals. Its default value is False.";
TopComPath::"usage"="is an optional parameter of TriangulateMB[] which specifies the path to the TOPCOM executables. Its default value is /usr/local/bin/.";
PrintSolutions::"usage"="is an optional parameter of TriangulateMB[] and ResolveMB[] which specifies whether to print the list of possible solutions along with their list of poles or not. Its default value is True.";
PrintSeries::"usage"="is an optional parameter of EvaluateSeries[] which specifies whether to print the derived analytic solution or not. Its default value is True.";
ShortestOnly::"usage"="is an optional parameter of TriangulateMB[] which specifies whether to only print the solution with shortest number of sets of poles or not. Its default value is False.";


(* ::Section::Closed:: *)
(*Internal Modules*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*Straight -> Non-Straight & Transforming MB*)


RemoveCommonGamma[NumDen_]:=Module[{Counter=1,CurrentNum,Num=Reverse[NumDen[[1]]],Den=NumDen[[2]],RemovePos},
While[Counter<=Length@Num,CurrentNum=Num[[Counter]];
If[MemberQ[Den,CurrentNum],Den=DeleteElements[Den,1->{CurrentNum}];Num=DeleteElements[Num,1->{CurrentNum}],Counter++];
];
Return[{Reverse@Num,Den}]];


StraightToNonStraight[PreFactor_,IntVar_,MBVar_,MBIntegrand_,Contour_,TakeLimit_]:=Module[{Num=MBIntegrand[[1]],
Den=MBIntegrand[[2]],NumContour,NumContourLim,NegativeNumPos,NegativeNum,NegativeNumGammaVal,NewPrefactor=PreFactor,MBArgGamma,MBArg,
PGLabel,NumGamma,DenGamma,NumPG,NumFlat,DenFlat,NegativeNumPosGamma,NegativeNumPosPG,ReturnList,NegativeNumPGVal,
NegativeNumGamma,NegativeNumPG,NegativeNumPGOrder,PGProd,PGProdList,ReturnListFinal},
PGLabel=Position[Num,_List]//Flatten;
NumGamma=Num//Select[#,Head[#]=!=List&]&;
DenGamma=Den//Select[#,Head[#]=!=List&]&;
NumPG=Num//Select[#,Head[#]===List&]&;
NumFlat=Num//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=Den//Flatten//Select[#,Head[#]=!=Integer&]&;

NumContour=NumFlat/.Table[IntVar[[i]]->Contour[[i]],{i,Length@IntVar}];
NumContourLim=NumContour/.TakeLimit;
If[And@@(!Element[#,NonPositiveIntegers]&/@NumContourLim),Nothing,Print["Contour ill-defined! It passes through one or more poles of the MB integral."]; Abort[]];

NegativeNumPos=Table[If[NumContourLim[[i]]<0,i ,Nothing,Nothing],{i,Length@NumFlat}];
NegativeNumPosGamma=Complement[NegativeNumPos,PGLabel];
NegativeNumGamma=NumFlat[[NegativeNumPosGamma]];
NegativeNumPosPG=Complement[NegativeNumPos,NegativeNumPosGamma];
NegativeNumPG=NumFlat[[NegativeNumPosPG]];
NegativeNumPGOrder=First@#&/@(Num[[NegativeNumPosPG]]);

NegativeNumGammaVal=Abs[IntegerPart/@NumContourLim[[NegativeNumPosGamma]]];
NegativeNumPGVal=Abs[IntegerPart/@NumContourLim[[NegativeNumPosPG]]];
Do[{NumGamma,DenGamma,NewPrefactor}=TransformMBGamma[NegativeNumGammaVal[[i]],NegativeNumGamma[[i]],NumGamma,DenGamma,NewPrefactor],{i,Length@NegativeNumGamma}];
If[Length@NegativeNumGamma>0,NumGamma=SortBy[NumGamma,Length[MonomialList@#]&]];

MBArgGamma=RemoveCommonGamma[{NumGamma,DenGamma}];
MBArg={Join[MBArgGamma[[1]],Num[[Complement[PGLabel,NegativeNumPosPG]]]],MBArgGamma[[2]]};
ReturnList={{NewPrefactor,IntVar,MBVar,MBArg}};

If[Length@NegativeNumPosPG>0,
PGProd=Product[TransformMBPG[NegativeNumPGVal[[i]],NegativeNumPGOrder[[i]],NegativeNumPG[[i]]],{i,Length@NegativeNumPosPG}];
PGProdList=PGProd//Expand//MonomialList;
ReturnList=Table[{ReturnList[[1,1]]*FactorTermsList[PGProdList[[i]]][[1]],ReturnList[[1,2]],ReturnList[[1,3]],{Join[ReturnList[[1,4,1]],PGtoList[FactorTermsList[PGProdList[[i]]][[2]]]],ReturnList[[1,4,2]]}},{i,Length@PGProdList}];
];

Return[ReturnList];
]


SplitPG[expr_]:=If[Head[expr]===Times,List@@expr,{expr}]

PGtoList[x_]:=Module[{PGSplit=SplitPG[x],PGList},
PGList=PGSplit/. PolyGamma[n_,z_]^m_:>Table[{n,z},m]/. PolyGamma[n_,z_]:>{{n,z}};
Return[Join@@PGList]
]


TransformMBGamma[n_,z_,Num_,Den_,Prefactor_]:=Module[{NewNum=DeleteElements[Num,1->{z}],NewDen,NewPrefactor},
NewNum=Join[NewNum,{-n-z,1+n+z}];NewDen=Join[Den,{1-z}];NewPrefactor=Prefactor*(-1)^(n+1);
Return[{NewNum,NewDen,NewPrefactor}]];


TransformMBPG[n_,m_,z_]:=Module[{PGExpand},
PGExpand=PolyGamma[m,1+n+z]+(-1)^(m+1) PolyGamma[m,-n-z]+(-1)^m PolyGamma[m,1-z];
Return[PGExpand]];


(* ::Input::Initialization::Plain:: *)
CanonicalMB[STNS_]:=Module[{Num=STNS[[4,1]],Den=STNS[[4,2]],Prefactor=STNS[[1]],MBVar=STNS[[3]],IntVarNew=STNS[[2]],Dim=Length@STNS[[2]],IntVarPos,NewPos,STNSReturn,y,IntVarTrans,AllPosSol,Sub,IntVarNewSub,VarPow,MBVarNew,DetFac=1},

IntVarTrans=Subscript[y, #]&/@Range@Dim;
AllPosSol=Subsets[Range@Length@Num,{Length@IntVarNew}];
Sub=Do[
Sub=Solve[Num[[i]]==-IntVarTrans,IntVarNew];
If[Length@Sub!=0,DetFac=(Coefficient[Num[[i]],#]&/@IntVarNew)//Det//Abs;Return[Sub//Flatten]],{i,AllPosSol}];

If[Length@Sub===0,Print["MB Representation can't be transformed into canonical form!"];Abort[]];

Num=Num/.Sub/.Table[IntVarTrans[[i]]->IntVarNew[[i]],{i,Dim}];
Den=Den/.Sub/.Table[IntVarTrans[[i]]->IntVarNew[[i]],{i,Dim}];
IntVarNewSub=IntVarNew/.Sub;
MBVarNew=Table[VarPow=Coefficient[IntVarNewSub,IntVarTrans[[i]]];
Times@@(MBVar^VarPow)
,{i,Dim}];
Prefactor=Prefactor*DetFac^-1*Times@@Power[MBVar,IntVarNewSub/.Table[i->0,{i,IntVarTrans}]];
Return[{Prefactor,IntVarNew,MBVarNew,{Num,Den}}]
]


ReArrangeMB[STNS_]:=Module[{Num=STNS[[4,1]],IntVarPos,IntVarNew=STNS[[2]],NewPos,STNSReturn},
IntVarPos=Table[First@Position[Num,i,1],{i,-IntVarNew}]//Flatten;
NewPos=Join[IntVarPos,Complement[Range[Length@Num],IntVarPos]];
Num=Num[[NewPos]];
STNSReturn=ReplacePart[STNS,{4,1}->Num];
Return[STNSReturn]
]


(* ::Subsection::Closed:: *)
(*Finding Building Blocks, Poles and Conic Hulls*)


positionDuplicates[l_]:=Module[{list=l},GatherBy[Range@Length[list],list[[#]]&]];
MBRepDim[MBRepVars_]:=(Length[MBRepVars[[2]]])
MergeNumDen[MBRepVars_]:=(Flatten[MBRepVars[[4]]]) 
NumLength[MBRepVars_]:=(Length[MBRepVars[[4,1]]])
MBVar[MBRepVars_]:=MBRepVars[[3]]


MakeCof[MBRepVars_]:=Module[{NumDen=MergeNumDen[MBRepVars],IntVar=MBRepVars[[2]]},
Table[Coefficient[NumDen[[i]],IntVar],{i,0,Length[NumDen]}]]


MakeCofDum[MBRepVars_,DumVarOnly_]:=Module[{NumDen=MergeNumDen[MBRepVars]},
Table[Coefficient[NumDen[[i]],DumVarOnly],{i,Length[NumDen]}]]


FindAllComb[MBRepVars_]:=Module[{NumberNum=NumLength[MBRepVars],Dim=MBRepDim[MBRepVars]},Subsets[Range@NumberNum,{Dim}]]


FindAllGoodComb[MBRepVars_]:=(*FindAllGoodComb[MBRepVars]=*)Module[{Cof,Num=MBRepVars[[4,1]],
AllComb,IntVar=MBRepVars[[2]],RR=x/@Range[MBRepDim[MBRepVars]],RetTup=1,SGN={},
Comb={},ConHull={},B={},R={},Poles={},j,NumDenIntCof,Degenerate,Msg1,Delta,NumCof,DenCof,ProportionalQ,AnyProportionalQ,
NumGamma,DenGamma,NumFlat,DenFlat,MBRepVarsFlatten,MBRepVarsGamma,NumGammaCof,DenGammaCof,NumDenIntGammaCof},

NumGamma=MBRepVars[[4,1]]//Select[#,Head[#]=!=List&]&;
DenGamma=MBRepVars[[4,2]]//Select[#,Head[#]=!=List&]&;
NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
MBRepVarsFlatten= ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];
MBRepVarsGamma= ReplacePart[MBRepVars,{{4,1}->NumGamma,{4,2}->DenGamma}];
Cof=MakeCof[MBRepVarsFlatten];
AllComb=FindAllComb[MBRepVars];


Monitor[
Do[
SGN=Insert[SGN,Sign[Det[Extract[Rest[Cof],#]&/@i]],RetTup];
 If[SGN[[RetTup]]!= 0, 
Comb=Insert[Comb,i,RetTup];
ConHull=Insert[ConHull,ConicHullRegion[{Cof[[1]]},Extract[Rest[Cof],#]&/@i],RetTup];
B=Insert[B,Refine[RegionMember[ConHull[[RetTup]], RR], RR\[Element] Reals]/.{GreaterEqual->Greater,LessEqual->Less},RetTup];
R=Insert[R,ImplicitRegion[B[[RetTup]],Evaluate[RR]],RetTup];
j=1;
Poles=Insert[Poles,Flatten[IntVar/.Solve[And@@{NumFlat[[#]]==-Subscript[n, j++]}&/@i,IntVar]],RetTup];
RetTup++] ,
{i,AllComb}],ProgressIndicator[Flatten[Position[AllComb,i]][[1]],{1,Length@AllComb}]];

Cof=Rest[MakeCof[MBRepVarsGamma]];
NumGammaCof=IntVar . Cof[[#]]&/@Range[Length@NumGamma];
DenGammaCof=IntVar . Cof[[#]]&/@Range[Length@NumGamma+1,Length@Cof];
NumDenIntGammaCof=Join[NumGammaCof,-DenGammaCof];
Delta=Total@NumDenIntGammaCof;

ProportionalQ[x1_,y1_]:=With[{x=Coefficient[x1,IntVar],y=Coefficient[y1,IntVar]},If[x . y/(Norm[x]Norm[y])===1,Return[True],Return[False]]];
Which[ FullSimplify[Delta]===0,
Degenerate=True ; Msg1=StringForm["Degenerate case with `1` conic hulls",--RetTup],
AnyProportionalQ=Catch[Do[If[ProportionalQ[Delta,i],Throw[True],Continue[]],{i,NumGammaCof}];Throw[False]];
AnyProportionalQ,Degenerate=False; Msg1=StringForm["Degenerate case with \[CapitalDelta] proportional to one or more coefficient vectors and associated with `1` conic hulls",--RetTup],
True,
Degenerate=False;Msg1=StringForm["Non-Degenerate case with `1` conic hulls",--RetTup]] ;
Return[{RetTup,Comb,Poles,R,B,NumDenIntGammaCof,Degenerate,RR,Msg1,SGN}]
]


(* ::Subsection::Closed:: *)
(*Intersection of Conic Hulls and solutions*)


FindIntersection[MBRepVarsF_,RequestedSeries_,MasterSeriesQ_]:=(*FindIntersection[MBRepVarsF,RequestedSeries,MasterSeriesQ]=*)Module[{MBRepVars=MBRepVarsF[[{1,2,3,4}]],
IntVar=MBRepVarsF[[2]],SeriesRepCount=0,
Counter=0,Branch=1,Dim=MBRepDim[MBRepVarsF],F={},Reg,Labels,NextLabel,IntersectingLabel,
LargestSubsetQ,CrossProd,AllPosSub=Subsets[Range[MBRepDim[MBRepVarsF]],{MBRepDim[MBRepVarsF]-1}], MasterCH ={},
MasterCHNorVec ={},MasterCHBasVec ={},MasterPoles ={},MasterCharList={},MasterSeriesVar ={},Msg2={},j,RepeatedTermLabel,
temp,DupTuplesList,UniqueTuples,CombLabels,RetTup,Comb,Poles,R,B,NumDenIntGammaCof,Degenerate,RR,Msg1,SGN,LMasterCHNorVec,
NumFlat,DenFlat,MBRepVarsFlatten},

NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
MBRepVarsFlatten= ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];

{RetTup,Comb,Poles,R,B,NumDenIntGammaCof,Degenerate,RR,Msg1,SGN}=FindAllGoodComb[MBRepVars];
Print@Msg1;
MBRepVars=MBRepVarsFlatten;

Poles=Expand@Poles;

B=Sort/@B;
DupTuplesList=positionDuplicates@B;
UniqueTuples=First/@DupTuplesList;
RetTup=Length@UniqueTuples;
R=R[[UniqueTuples]];
B=B[[UniqueTuples]];


While[True,
If[Branch==1,
If[++Counter<= RetTup,Reg=R[[Counter]];Labels={Counter};Branch=2;NextLabel=Counter+1;Continue[],Break[]]];

If[SeriesRepCount>= RequestedSeries,Break[]];

IntersectingLabel=Catch[Do[If[RegionDisjoint[Reg,R[[i]]],Continue[],Throw[i]],{i,NextLabel,RetTup}];Throw[0]];
If[IntersectingLabel==0,
LargestSubsetQ=Catch[Do[If[SubsetQ[F[[i]],UniqueTuples[[Labels]]],Throw[False]],{i,SeriesRepCount}];Throw[True]];

If[LargestSubsetQ,
CombLabels=Sort[Flatten[DupTuplesList[[Labels]]]];
++SeriesRepCount;

If[MasterSeriesQ,

Which[Degenerate&&(Dim>=2),

MasterCH=Insert[MasterCH,FullSimplify[And@@B[[Labels]]],-1];


MasterCHNorVec=Insert[MasterCHNorVec,temp=MasterCH[[SeriesRepCount]];Table[Coefficient[Subtract@@(temp[[i]]),Evaluate@RR],{i,Length[temp]}],-1];

LMasterCHNorVec=Sort[Last@MasterCHNorVec];
If[Length@temp>Dim, MasterCHNorVec=ReplacePart[MasterCHNorVec,-1->LMasterCHNorVec[[Flatten[Position[#,Except[0,_?NumericQ],1,1]&/@Last@QRDecomposition@Transpose@LMasterCHNorVec]]]]];


MasterCHBasVec=Insert[MasterCHBasVec,Table[CrossProd=Cross@@(MasterCHNorVec[[SeriesRepCount]][[i]]);
If[ MasterCH[[SeriesRepCount]] [[Complement[Range[Dim],i]]]/.(RR[[#]]-> CrossProd[[#]]&/@Range[Dim]),CrossProd,-CrossProd],{i,AllPosSub}],-1];
j=1;
MasterPoles=Insert[MasterPoles,Flatten@Solve[And@@(Table[IntVar . MasterCHBasVec[[SeriesRepCount]][[i]]==-Subscript[n, j++],{i,Dim}]),IntVar],-1];
MasterCharList=Insert[MasterCharList,Join[NumDenIntGammaCof/.MasterPoles[[SeriesRepCount]],Subscript[n, #]&/@Range[Dim]],-1];
 RepeatedTermLabel={};

Do[If[MemberQ[RepeatedTermLabel,i]||MemberQ[RepeatedTermLabel,j],Continue[]];If[FullSimplify[Total@MasterCharList[[SeriesRepCount]][[{i,j}]]]===0,RepeatedTermLabel=Union[RepeatedTermLabel,{i,j}]],{i,1,Length@MasterCharList[[SeriesRepCount]]-1},{j,i+1,Length@MasterCharList[[SeriesRepCount]]}];
MasterCharList[[SeriesRepCount]]=Delete[MasterCharList[[SeriesRepCount]],Table[{RepeatedTermLabel[[i]]},{i,Length[RepeatedTermLabel]}]]//FullSimplify;
MasterSeriesVar=Insert[MasterSeriesVar,Table[Times@@((MBVar@MBRepVars)^Coefficient[IntVar/.MasterPoles[[SeriesRepCount]],Subscript[n, i]]),{i,Dim}],-1]

,Degenerate&&(Dim===1),
MasterCharList=Insert[MasterCharList,Subscript[n,1],-1];
If[ContainsOnly[SGN[[CombLabels]],{-1}],MasterSeriesVar=Insert[MasterSeriesVar,MBVar@MBRepVars,-1],MasterSeriesVar=Insert[MasterSeriesVar,1/MBVar@MBRepVars,-1]];

,!Degenerate,
Nothing]
];

PrintTemporary["Number of Series Solution Found :: ", SeriesRepCount];
F=Insert[F,CombLabels,-1]];
Branch--;
If[Branch==1,Continue[]];
NextLabel=Last@Labels+1;
Labels=Most[Labels];
Reg=RegionIntersection@@R[[Labels]],

Labels=Insert[Labels,IntersectingLabel,-1];Branch++;
Reg=RegionIntersection[Reg,R[[IntersectingLabel]]];NextLabel=IntersectingLabel+1]];

Return[{F,Comb,Poles,Msg1,Msg2,SGN,Length@F,MBRepVars,MasterCharList,MasterSeriesVar,Degenerate}]]


(* ::Subsection::Closed:: *)
(*Finding Point-Configuration & Running TOPCOM*)


FindMBAMatrix[MBRepOut_]:=Module[{A,IdentityA,Num=MBRepOut[[4,1]],IntVar=MBRepOut[[2]],NumCof,MBFold,ATrim},
PrintTemporary["Finding A matrix"];
MBFold=Length@IntVar;
NumCof=Table[Coefficient[Num[[i]],IntVar],{i,MBFold+1,Length[Num]}];
IdentityA=IdentityMatrix[(Length@NumCof)];
A=ArrayFlatten[{{NumCof, IdentityA}}] ;

NumCof=DeleteDuplicates@NumCof;
IdentityA=IdentityMatrix[(Length@NumCof)];
ATrim=ArrayFlatten[{{NumCof, IdentityA}}] ;

PrintTemporary["Done!"];
Return[{A,ATrim,Length@Num}]]


FindTriangulations[Amat_,TopcomPath_,TopComOptions_,TriangulationsNum_,CardMsg_,TPCommand_]:=Module[{TriInSize,TriSize,
Count,ShellInput,List1,List2,List3,List4,List5,ListS,Atri},
SetDirectory[NotebookDirectory[]];
CreateFile["InputTOPCOM.dat",OverwriteTarget->True];
CreateFile["Triangulations.txt",OverwriteTarget->True];
Export["InputTOPCOM.dat",ToString[Amat,FormatType->InputForm]];
Export["Triangulations.txt","TestData"];
Block[{Print},RunProcess[$SystemShell,"StandardOutput",
"tr \'{}\' \'\[\]\' < \""<>NotebookDirectory[]<>"InputTOPCOM.dat\" | tee \""<>NotebookDirectory[]<>"InputTOPCOM.dat\""
];
];

If[Head[TriangulationsNum]===Integer,
Count=ToString@TriangulationsNum;

Which[StringTake[$OperatingSystem,3]==="Mac"

,RunProcess[$SystemShell,"StandardOutput",TopcomPath<>TPCommand<>TopComOptions<>" < \""<>NotebookDirectory[]<>"InputTOPCOM.dat\" | head -n "<>Count<>" > \""<>NotebookDirectory[]<>"Triangulations.txt\""];

,StringTake[$OperatingSystem,3]==="Uni"

,ShellInput=TopcomPath<>TPCommand<>TopComOptions<>" < "<>NotebookDirectory[]<>"InputTOPCOM.dat | head -n "<>Count<>" > "<>NotebookDirectory[]<>"Triangulations.txt";
Export["RunTopcom.txt",ShellInput,OverwriteTarget->True];
RenameFile["RunTopcom.txt","RunTopcom.sh",OverwriteTarget->True];
Run["sh RunTopcom.sh"];
];
,
Block[{Print},RunProcess[$SystemShell,"StandardOutput",TopcomPath<>TPCommand<>TopComOptions<>" < \""<>NotebookDirectory[]<>"InputTOPCOM.dat\" | tee \""<>NotebookDirectory[]<>"Triangulations.txt\""];];
];

TriSize=FileByteCount[NotebookDirectory[]<>"Triangulations.txt"];
TriInSize=FileByteCount[NotebookDirectory[]<>"InputTOPCOM.dat"];
If[TriSize===0&&TriInSize>0,
Print["No triangulations found "<>CardMsg<>"."];Abort[]];

PrintTemporary["Extracting Results"];
Atri=Import[NotebookDirectory[]<>"Triangulations.txt"];

If[TPCommand=="points2alltriangs"&&TriInSize>0,
List1 = Map[First, StringPosition[Atri, "="]]+2;
ListS =Map[First, StringPosition[Atri, ";"]]-1;
List3=Table[{List1[[i]],ListS[[i]]},{i,Length@List1}];
List4=With[{AtriW=Atri},ToExpression[ParallelMap[StringJoin[StringPart[AtriW,Range[Sequence@@#]]]&,List3]]+1];
];

If[TPCommand=="points2placingtriang"&&TriInSize>0,List4={ToExpression@Atri+1}];


PrintTemporary["Done!"];
Return[List4]
];


(* ::Subsection::Closed:: *)
(*Analytic Series using MultivariateResidues*)


SingOrder[x_,DumVar_]:=Module[{DumVarOnly,DumVarPow,DumVarPowList,Var,DumVarPresent},
DumVarOnly=(Most[#]&/@DumVar)//Flatten;
DumVarPow=Table[If[Length[DumVar[[i]]]==2,{DumVar[[i,1]]->1},{DumVar[[i,1]]->1,DumVar[[i,2]]->DumVar[[i,3]]}],{i,Length@DumVar}];
DumVarPowList=Join@@DumVarPow;
Var=Variables[x];
DumVarPresent=Select[Var,MemberQ[DumVarOnly,#]&];
DumVarPow=Total[DumVarPresent/.DumVarPowList]+1;
Return[DumVarPow]
]


FindSingFactors[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_]:=(*FindSingFactors[MBRepVars,FSeriesSol,Comb,Poles,SGN]=*)Module[{TempCond,PositiveDefinite,
NumberNum=NumLength[MBRepVars],NumDen=MergeNumDen[MBRepVars],Dim=MBRepDim[MBRepVars],DivConCond={},SingFacLabels={},AssociatePoles={},IntVar=MBRepVars[[2]],
ConGamma,NumDenSub,NumDenSubD,NumDenSubN,DivGamma={},ConDivGamma={},SumVar,DivCond,ConCond,SingComb,NonSingComb,PoleSubstitute},

SumVar=Subscript[n, #]&/@Range[Dim];
PositiveDefinite=And@@((#>=0)&/@SumVar);
ConGamma=Complement[Range[NumberNum],Union@@(Extract[Comb,#]&/@FSeriesSol)];
Do[
PoleSubstitute=IntVar[[#]]-> Poles[[i]][[#]]&/@Range[Dim];
NumDenSub=Together[NumDen/.PoleSubstitute//Simplify];
NumDenSubD=Denominator@NumDenSub;
NumDenSubN=Numerator@NumDenSub;
DivGamma={};
ConDivGamma={};

Do[If[Resolve[Exists[Evaluate[SumVar],Evaluate[SumVar]\[Element]Integers,Evaluate@NumDenSubN[[j]]<= 0&&Mod[Evaluate@NumDenSubN[[j]],Evaluate@NumDenSubD[[j]]]==0&&Evaluate@PositiveDefinite]],If[Resolve[Exists[Evaluate[SumVar],Evaluate[SumVar]\[Element]Integers,Evaluate@NumDenSubN[[j]]>0 ||Mod[Evaluate@NumDenSubN[[j]],Evaluate@NumDenSubD[[j]]]!= 0 && Evaluate@PositiveDefinite]],ConDivGamma=Insert[ConDivGamma,j,-1],DivGamma=Insert[DivGamma,j,-1]]],{j,Complement[Range[Length[NumDen]],Comb[[i]],ConGamma]}];
Do[DivCond=True;ConCond=True;SingComb=Union[DivGamma,o];If[MemberQ[SingFacLabels,Sort[Union[Comb[[i]],SingComb]]],Continue[]];NonSingComb=Complement[ConDivGamma,o];
Do[DivCond=And[DivCond,FullSimplify[Evaluate@NumDenSubN[[k]]<= 0&&Mod[Evaluate@NumDenSubN[[k]],Evaluate@NumDenSubD[[k]]]==0 && Evaluate@PositiveDefinite,Evaluate[SumVar]\[Element]Integers]],{k,SingComb}];
Do[ConCond=And[ConCond,FullSimplify[Evaluate@NumDenSubN[[l]]>0 ||Mod[Evaluate@NumDenSubN[[l]],Evaluate@NumDenSubD[[l]]]!= 0  && Evaluate@PositiveDefinite,Evaluate[SumVar]\[Element]Integers]],{l,NonSingComb}];
If[TempCond=FullSimplify[DivCond&&ConCond&&Evaluate@PositiveDefinite,Evaluate[SumVar]\[Element]Integers];Resolve[Exists[Evaluate[SumVar],Evaluate[SumVar]\[Element]Integers,TempCond]],DivConCond=Insert[DivConCond,TempCond,-1];SingFacLabels=Insert[SingFacLabels,Sort[Union[Comb[[i]],SingComb]],-1];AssociatePoles=Insert[AssociatePoles,Poles[[i]],-1],Nothing,Print["Error 3 ::",DivCond&&ConCond]]
,{o,Subsets[ConDivGamma]}]
,{i,FSeriesSol}];

Return[{SingFacLabels,AssociatePoles,DivConCond}];
]


GroupSingFactors[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_]:=(*GroupSingFactors[MBRepVars,FSeriesSol,Comb,Poles,SGN]=*)Module[{GroupFacLabels={},NumberNum=NumLength[MBRepVars],Dim=MBRepDim[MBRepVars],
Cof=Rest@MakeCof[MBRepVars],SingFacLabels,OnlyNumLabels,CommonLabelsPos,LabelPos,LabelInsert,CounterTuple,CounterTupleSets,IdealCheck,
SingCombSum,SingCombSum1,ErrorIndicator,GroupCounter,CommonLabels,RepeatedCombLabel,co,F1={},AssociatePoles,DivConCond,SignFacsLabelsNum,
AbortQ=True},


PrintTemporary["Finding Singular Factors"];

While[AbortQ,
AbortQ=False;
CheckAbort[

{SingFacLabels,AssociatePoles,DivConCond}=FindSingFactors[MBRepVars,FSeriesSol,Comb,Poles,SGN];

,AbortQ=True]];

PrintTemporary["Done!"];



PrintTemporary["Grouping Singular Factors"];
If[Dim===1,
GroupFacLabels=Table[{{Intersection[i, Range@NumberNum]}}, {i, SingFacLabels}];

Return[{SingFacLabels,AssociatePoles,DivConCond,GroupFacLabels}]];


Do[
OnlyNumLabels=Table[If[p<= NumberNum,p,Nothing],{p,SingFacLabels[[d]]}];

SingCombSum=Table[If[SubsetQ[OnlyNumLabels,Comb[[f]]],If[SGN[[f]]==1,Comb[[f]],ReplacePart[Comb[[f]],{1-> Comb[[f]][[2]],2->Comb[[f]][[1]]}]],Nothing],{f,FSeriesSol}];

SingCombSum1=SingCombSum;
co=0;
While[
F1={};
ErrorIndicator=0;
GroupCounter=1;
F1=Insert[F1,{#}&/@SingCombSum[[1]],-1];
SingCombSum1=Rest[SingCombSum1];
While[Length[SingCombSum1]!= 0,

Do[CommonLabels=Intersection[Flatten[F1[[GroupCounter]]],SingCombSum1[[j]]];
If[Length[CommonLabels]==Dim-1,
CommonLabelsPos=Table[First[Flatten[Position[F1[[GroupCounter]],i]]],{i,CommonLabels}];
If[DuplicateFreeQ[CommonLabelsPos],

LabelPos=First[Complement[Range[Dim],CommonLabelsPos]];
LabelInsert=Complement[SingCombSum1[[j]],CommonLabels];
CounterTuple=ReplacePart[F1[[GroupCounter]],LabelPos-> LabelInsert];

If[Length[CounterTuple]>= 2,
CounterTuple=ReplacePart[CounterTuple,{1-> CounterTuple[[2]],2->CounterTuple[[1]]}]];

CounterTupleSets=Tuples[CounterTuple];

IdealCheck=Product[Det[Cof[[i]]],{i,CounterTupleSets}];
(* Checking zero-dimensional ideal or not *)
If[IdealCheck!= 0,
Break[],If[j===Length@SingCombSum1,ErrorIndicator=1;Break[]]],

If[j===Length@SingCombSum1,ErrorIndicator=1;Break[]]],

If[j===Length@SingCombSum1,ErrorIndicator=1;Break[]]],{j,1,Length[SingCombSum1]}

];

If[ErrorIndicator==1,Break[]];
F1[[GroupCounter]]=ReplacePart[F1[[GroupCounter]],LabelPos-> Sort[Union[F1[[GroupCounter]][[LabelPos]],LabelInsert]]];
SingCombSum1=Join[SingCombSum1,CounterTupleSets];

RepeatedCombLabel={};Do[If[MemberQ[RepeatedCombLabel,i]||MemberQ[RepeatedCombLabel,j],Continue[]];If[ContainsExactly[SingCombSum1[[i]],SingCombSum1[[j]]]&&Signature[SingCombSum1[[i]]]*Signature[SingCombSum1[[j]]]==-1,RepeatedCombLabel=Union[RepeatedCombLabel,{i,j}]],{i,1,Length[SingCombSum1]-1},{j,i+1,Length[SingCombSum1]}];
SingCombSum1=Delete[SingCombSum1,Table[{RepeatedCombLabel[[i]]},{i,Length[RepeatedCombLabel]}]];

If[ContainsExactly[Flatten[F1[[GroupCounter]]],OnlyNumLabels],If[Length[SingCombSum1]!= 0,++GroupCounter;F1=Insert[F1,Table[{i},{i,SingCombSum1[[1]]}],-1];SingCombSum1=Rest[SingCombSum1]]]];

ErrorIndicator>0, If[++co>Length@SingCombSum!,Print["No Grouping Possible"];Break[]];SingCombSum=Reverse[NextPermutation[SingCombSum]];SingCombSum1=SingCombSum];

GroupFacLabels=Insert[GroupFacLabels,Table[F1[[i]],{i,GroupCounter}],-1] 
,{d,Length[SingFacLabels]}];
PrintTemporary["Done!"];

Return[{SingFacLabels,AssociatePoles,DivConCond,GroupFacLabels}];

]



CalculateResidue[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_,DumVar_]:=(*CalculateResidue[MBRepVars,FSeriesSol,Comb,Poles,SGN]=*)Module[{IntVar=MBRepVars[[2]],
Dim=MBRepDim[MBRepVars],Cof,NumDenFlat,AssociatePoles,SingFacLabels,DivConCond,GroupFacLabels,SeriesNumber=0,NumberNum=NumLength[MBRepVars],
  NumDenIntPart, GenReflection,NumDenPoleSub,NumDenPoleShift,GenRefSub,Integrand,IntegrandSub,NumeratorFac,DenSingFac,TransformedVar,GenReflectionPG,
  TransformMatrix,IntVarSwap,GbBasis,OverallFac,NumFacReduce,NumFacMonoList,RefinedFac,SeriesCof ,series={},PolyGammaLabel,PolyGammaOrder,
  SeriesCondition,Msg3={},seriestemp,ContourSign,SignSGN,SignFacsLabelsNum,NumGamma,DenGamma,NumFlat,DenFlat,MBRepVarsFlatten,NumPolyGamma,
  DumVarOnly=(Most[#]&/@DumVar)//Flatten,PolesDumZero,DumCof,NumDenDumPart,NumDenIntDumPart,DumVarZero,DenSingFacGammaAll,DumVarSequence,DenSingFacDumGammaAll
  ,DumVarD,NumeratorFac1,NumeratorFac2,NumeratorFac3,DenSingPow,IntegrandDerLim},

DumVarZero=Table[i->0,{i,DumVarOnly}];
DumVarSequence=Table[If[Length[DumVar[[i]]]==2,ConstantArray[DumVar[[i,1]],1],Join[ConstantArray[DumVar[[i,1]],1],ConstantArray[DumVar[[i,2]],DumVar[[i,3]]]]],{i,Length@DumVar}];
DumVarSequence=Join@@DumVarSequence;
PolyGammaLabel=Position[MBRepVars[[4,1]],_List]//Flatten;
PolyGammaOrder=First[#]&/@MBRepVars[[4,1]][[PolyGammaLabel]];
NumGamma=MBRepVars[[4,1]]//Select[#,Head[#]=!=List&]&;
DenGamma=MBRepVars[[4,2]]//Select[#,Head[#]=!=List&]&;
NumPolyGamma=MBRepVars[[4,1]]//Select[#,Head[#]===List&]&;
NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
NumDenFlat=Join[NumFlat,DenFlat];
MBRepVarsFlatten=ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];
Cof=Rest@MakeCof[MBRepVarsFlatten];
DumCof=MakeCofDum[MBRepVarsFlatten,DumVarOnly];

MBRepVarsFlatten= MBRepVarsFlatten/.DumVarZero;
PolesDumZero= Poles/.DumVarZero;

(*Print["MBRepVarsFlatten ", MBRepVarsFlatten];
Print["FSeriesSol ", FSeriesSol];
Print["Comb ", Comb];
Print["PolesDumZero ", PolesDumZero];
Print["SGN ", SGN];
Print["Cof ", Cof];
Print["DumCof ", DumCof];
Print["DumVarZero ", DumVarZero];
Print["DumVar ", DumVar];*)


{SingFacLabels,AssociatePoles,DivConCond,GroupFacLabels}=GroupSingFactors[MBRepVarsFlatten,FSeriesSol,Comb,PolesDumZero,SGN];

(*Print["SingFacLabels ", SingFacLabels];
Print["AssociatePoles ", AssociatePoles];
Print["DivConCond ", DivConCond];
Print["GroupFacLabels ", GroupFacLabels];
Print["IntVar ", IntVar];*)

PrintTemporary["Computing residues"];

SeriesCondition={};
GenReflection[z_,n_]:=((-1)^(-n) Gamma[1-z]Gamma[1+z])/Gamma[1-n-z]; (* Generalized Relflection Formula for Gamma*)
GenReflectionPG[m_,z_,n_]:=z^(m+1) PolyGamma[m,1+z]+ (-1)^(m+1) z^(m+1) PolyGamma[m,1-z] +(-1)^(m+1) Gamma[1+m]+(-1)^m z^(m+1) PolyGamma[m,1-n-z]; (* Generalized Relflection Formula for PolyGamma*)
NumDenIntPart=Total[Cof[[#]]*IntVar]&/@Range[Length@NumDenFlat];
NumDenDumPart=Total[DumCof[[#]]*DumVarOnly]&/@Range[Length@NumDenFlat];
NumDenIntDumPart=NumDenIntPart+NumDenDumPart;
(*Print["NumDenIntPart ", NumDenIntPart];
Print["NumDenDumPart ", NumDenDumPart];
Print["NumDenIntDumPart ", NumDenIntDumPart];*)
Monitor[
Do[

NumDenPoleSub=NumDenFlat/.(IntVar[[#]]-> AssociatePoles[[k]][[#]]&/@Range@Dim)/.DumVarZero//Simplify;
NumDenPoleShift=NumDenFlat/.(IntVar[[#]]->IntVar[[#]]+ AssociatePoles[[k]][[#]]&/@Range@Dim)//Simplify;
GenRefSub=Table[If[!MemberQ[PolyGammaLabel,j],Gamma@NumDenPoleShift[[j]]-> GenReflection[NumDenIntDumPart[[j]],NumDenPoleSub[[j]]],PolyGamma[First[MBRepVars[[4,1]][[j]]],NumDenPoleShift[[j]]]->1],{j,SingFacLabels[[k]]}];
(*Print["NumDenPoleSub ", NumDenPoleSub];
Print["GenRefSub ", GenRefSub];*)

Integrand=MBRepVars[[1]]* ( (Times@@Gamma/@NumGamma) (Times@@(PolyGamma@@#&/@NumPolyGamma)) Times@@(MBRepVars[[3]]^MBRepVars[[2]]))/Times@@(Gamma/@DenGamma);

(*IntegrandDerLim = Integrand;
Do[
DumVarD=DumVarSequence[[s]];
IntegrandDerLim = D[IntegrandDerLim,DumVarD];
,{s,Length@DumVarSequence}];
IntegrandDerLim =IntegrandDerLim//Limit[#,DumVarZero]&;*)

IntegrandSub=Simplify[Integrand/.(IntVar[[#]]->IntVar[[#]]+ AssociatePoles[[k]][[#]]&/@Range@Dim)]/.GenRefSub;
NumeratorFac=Product[If[l>NumberNum,NumDenIntDumPart[[l]],1],{l,SingFacLabels[[k]]}];
NumeratorFac=NumeratorFac*Product[If[MemberQ[PolyGammaLabel,l],GenReflectionPG[First[MBRepVars[[4,1]][[l]]],NumDenIntPart[[l]],NumDenPoleSub[[l]]],1],{l,SingFacLabels[[k]]}];
DenSingFacGammaAll= Table[If[l<=NumberNum,NumDenIntDumPart[[l]],Nothing],{l,SingFacLabels[[k]]}];
DenSingFacDumGammaAll = Select[ DenSingFacGammaAll,ContainsAny[DumVarOnly,Variables[#]]&];
DenSingPow =ConstantArray[-1,Length@DenSingFacDumGammaAll];



(*Print["Integrand ", Integrand];
Print["IntegrandDerLim ", IntegrandDerLim];*)


(*Print["Integrand Shifted ", Integrand/.(IntVar[[#]]->IntVar[[#]]+ AssociatePoles[[k]][[#]]&/@Range@Dim)];
Print["IntegrandSub ", IntegrandSub];
Print["IntegrandSubLim ", IntegrandSub//Limit[#,DumVarZero]&];
Print["NumeratorFac ", NumeratorFac];
Print["DenSingFacGammaAll ", DenSingFacGammaAll];
Print["DenSingFacDumGammaAll ", DenSingFacDumGammaAll];

Print["DenSingPow ", DenSingPow];
Print["DumVarSequence ", DumVarSequence];*)

Do[
DumVarD=DumVarSequence[[s]];
NumeratorFac1= D[NumeratorFac,DumVarD]*Product[Power[i,Boole[MemberQ[Variables[i],DumVarD]]],{i,DenSingFacDumGammaAll}];
NumeratorFac2= NumeratorFac*Expand[D[IntegrandSub,DumVarD]/IntegrandSub]*Product[Power[i,Boole[MemberQ[Variables[i],DumVarD]]],{i,DenSingFacDumGammaAll}];
NumeratorFac3= NumeratorFac*Sum[ DenSingPow[[j]]* D[DenSingFacDumGammaAll[[j]],DumVarD]*Product[Power[i,Boole[MemberQ[Variables[i],DumVarD]]],{i,Delete[DenSingFacDumGammaAll,j]}], {j,Length@DenSingFacDumGammaAll}];

NumeratorFac= NumeratorFac1 + NumeratorFac2 + NumeratorFac3;
DenSingPow =Table[DenSingPow[[i]]-Boole[MemberQ[Variables[DenSingFacDumGammaAll[[i]]],DumVarD]],{i,Length@DenSingFacDumGammaAll}];

(*Print["NumeratorFac1 ", NumeratorFac1//Limit[#,DumVarZero]&];
Print["NumeratorFac2 ", NumeratorFac2//Limit[#,DumVarZero]&];
Print["NumeratorFac3 ", NumeratorFac3//Limit[#,DumVarZero]&];
Print["DenSingPow ", DenSingPow];*)
,{s,Length@DumVarSequence}];

IntegrandSub= Limit[IntegrandSub,DumVarZero];
NumeratorFac = Limit[NumeratorFac,DumVarZero]//Expand;


(*Print["NumeratorFac ", NumeratorFac];*)

Do[

DenSingFac=Table[Product[ If[!MemberQ[PolyGammaLabel,a],Power[NumDenIntPart[[a]],SingOrder[NumDenIntDumPart[[a]],DumVar]],Power[NumDenIntPart[[a]],First[MBRepVars[[4,1]][[a]]]+1]] ,{a,b[[c]]}],{c,Dim}];

(*Print["DenSingFac ", DenSingFac];*)
TransformedVar={};
TransformMatrix={};
Do[
IntVarSwap=ReplacePart[IntVar,{i->IntVar[[Dim]],Dim->IntVar[[i]]}];
GbBasis=ResourceFunction["ExtendedGroebnerBasis"][DenSingFac,IntVarSwap];
TransformedVar=Insert[TransformedVar,GbBasis[[1]][[1]],-1];

TransformMatrix=Insert[TransformMatrix,GbBasis[[2]][[1]],-1],{i,Dim}];

OverallFac=FactorTermsList[Times@@TransformedVar][[1]];
TransformedVar=Numerator[TransformedVar/OverallFac]; 
NumFacReduce=PolynomialReduce[Expand[Det[TransformMatrix]*NumeratorFac],Evaluate@TransformedVar,Evaluate@IntVar][[2]];
NumFacMonoList=If[NumFacReduce=!=0,MonomialList[NumFacReduce],{}];
RefinedFac={};
SeriesCof={};

Do[
RefinedFac=Insert[RefinedFac,Table[Denominator[FactorTermsList[NumFacMonoList[[i]]][[2]]/TransformedVar[[j]]],{j,Dim}],-1];
SeriesCof=Insert[SeriesCof,Numerator[NumFacMonoList[[i]]/Times@@TransformedVar],-1],
{i,Length@NumFacMonoList}];


If[Dim===1, 
SignFacsLabelsNum=Select[Flatten[SingFacLabels],MemberQ[Range@NumberNum,#]&];
SignSGN=SGN[[SignFacsLabelsNum]];
Which[ContainsOnly[SignSGN,{-1}],ContourSign=-1,ContainsOnly[SignSGN,{1}],ContourSign=1,True,Print["Some Issues!"]],ContourSign=1];
seriestemp=(OverallFac)^(-1) Sum[MultivariateResidue[SeriesCof[[j]]*ContourSign*IntegrandSub,RefinedFac[[j]],Table[IntVar[[i]]-> 0,{i,Dim}]],
{j,Length@RefinedFac}]//Simplify;
If[seriestemp===0,Continue[],SeriesNumber++;series=Insert[series,seriestemp,-1]];
SeriesCondition=Insert[SeriesCondition,DivConCond[[k]],-1];

Msg3= Insert[Msg3,StringForm["Series Number `1` :: `2` valid for `3`",SeriesNumber, series[[SeriesNumber]],
 If[SeriesCondition[[SeriesNumber]],And@@(Subscript[n, #]>= 0&/@Range[Dim]),Nothing,SeriesCondition[[SeriesNumber]]]],-1];

,{b,GroupFacLabels[[k]]}]
,{k,Length@DivConCond}]
,ProgressIndicator[k,{1,Length@DivConCond}]];

PrintTemporary["Done!"];


Return[{SeriesNumber,series,Msg3,Dim,SeriesCondition}]]


(* ::Section::Closed:: *)
(*External Modules*)


(* ::Subsection::Closed:: *)
(*Module to Input MB*)


Options[MBRep]={Substitute->{},TakeLimit->{},CanonicalTransform->True, PolygammaToGamma -> True};

MBRep[PreFacRaw_,IntVar_,MBVar_,MBArg_,OptionsPattern[]]:=Module[{MBArgNew=(RemoveCommonGamma@MBArg)/.OptionValue[Substitute],
Contour,TimeTaken,IntVarNew,ContourString,AllVariables,PreFac=PreFacRaw/.OptionValue[Substitute],
STNSReturn,RetTup,Comb,Poles,R,B,NumDenIntCof,Degenerate,RR,Msg1,SGN,Num,IntVarPos,NewPos,MBNumArg,ContourSub,DumVar={},TakeLimitAll,MBDenArg,DumVarOnly},

TimeTaken=AbsoluteTiming[

If[Length@IntVar=!=Length@MBVar,Print["The number of Integration variables and MB variables should be same."]; Abort[];];
If[((MBArgNew[[2]]//Flatten//Length)-(MBArgNew[[2]]//Length))=!=0,Print["PolyGamma in the denominator is not supported."]; Abort[];];

Which[ContainsOnly[Head@#&/@IntVar,{Rule}],
IntVarNew=First@#&/@IntVar;
Contour=Last@#&/@IntVar; 
ContourString=Table[StringJoin[{ToString[Re[ToString[IntVarNew[[i]],StandardForm]],StandardForm]," = ",ToString[Contour[[i]],StandardForm]}],{i,Length@IntVarNew}];
Print["Straight Contour : ",ContourString];
AllVariables=Variables[Flatten[MBArgNew/.OptionValue[TakeLimit]]];
If[Length@AllVariables>Length@IntVarNew,Print["Please provide the values of the following parameters : ",Complement[AllVariables,IntVarNew]];Abort[]];

If[OptionValue[PolygammaToGamma],
MBNumArg=MBArgNew[[1]];
MBDenArg=MBArgNew[[2]];
ContourSub=IntVar;
Do[
If[Head[MBNumArg[[i]]]===List,
If[(MBNumArg[[i,2]]/.OptionValue[TakeLimit]/.ContourSub)<0,
Which[MBNumArg[[i,1]]===0,AppendTo[MBDenArg,MBNumArg[[i,2]]];AppendTo[DumVar,{Subscript[\[Iota], i],0}];MBNumArg=ReplacePart[MBNumArg,{i->Subscript[\[Iota], i]+MBNumArg[[i,2]]}],
MBNumArg[[i,1]]>0,AppendTo[MBDenArg,MBNumArg[[i,2]]+Subscript[\[Omicron], i]];AppendTo[DumVar,{Subscript[\[Iota], i],Subscript[\[Omicron], i],MBNumArg[[i,1]]}];MBNumArg=ReplacePart[MBNumArg,{i->Subscript[\[Iota], i]+Subscript[\[Omicron], i]+MBNumArg[[i,2]]}]];
];
];
,{i,Length@MBNumArg}];
MBArgNew= ReplacePart[MBArgNew,{1->MBNumArg,2->MBDenArg}];
MBArgNew=RemoveCommonGamma@MBArgNew;
];

DumVarOnly=(Most[#]&/@DumVar)//Flatten;
TakeLimitAll=Join[OptionValue[TakeLimit],Table[i->0,{i,DumVarOnly}]];

STNSReturn=StraightToNonStraight[PreFac,IntVarNew,MBVar,MBArgNew,Contour,TakeLimitAll],

ContainsNone[Head@#&/@IntVar,{Rule}],
Print["Non-Straight Contours."];
IntVarNew=IntVar;
STNSReturn={{PreFac,IntVar,MBVar,MBArgNew}},

True,
Print["Input not in correct format! Please see the documentation."];
Abort[]];

If[Length@STNSReturn>1,Print[Length@STNSReturn," MB integrals after transforming from straight to non-straight contour"]];

If[OptionValue@CanonicalTransform,
PrintTemporary["Transforming into canonical form"];
STNSReturn=Table[CanonicalMB[i],{i,STNSReturn}]//FullSimplify;
STNSReturn=Table[ReArrangeMB[i],{i,STNSReturn}];
];
];
If[Length@DumVar>0,
Print["Transforming PolyGamma function whose poles are split by the straight contour into Gamma function"];;
Print["Introduced Dummy variable : ", DumVarOnly]];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
AppendTo[STNSReturn,DumVar];
Return[Join[STNSReturn,{}]];
];

MBRep[X___]:=If[Length@List@X!=4,Print["Four arguments expected, but ",Length@List@X," given."];Abort[]]


(* ::Subsection::Closed:: *)
(*Module to find solutions using Conic Hull Approach*)


Options[ResolveMB]={MaxSolutions->Infinity,PrintSolutions->True,MasterSeries->True,MBNumber->1};

ResolveMB[MBRepIn_,OptionsPattern[]]:=Module[{
msg1, FindIntersectionReturn,MBRepVars,IntVar,
MBRepVarsFlatten,NumFlat,DenFlat,NumGamma,DenGamma,NumCof,DenCof,NumDenIntCof,Delta,ProportionalQ,AnyProportionalQ,RetTup,
MaxRequestedSeries=OptionValue[MaxSolutions],TimeTaken,LengthMessage = "",LengthList,LengthTally,
Dim,Degenerate,F,FSort,Comb,Poles,CombLabels,MasterCharList={},MasterSeriesVar ={},Msg2={},RearrangePos,
PrintSol=OptionValue@PrintSolutions,MasterSeriesQ=OptionValue@MasterSeries,MBRepInput=Most[MBRepIn],DumVar=MBRepIn[[-1]]},

If[OptionValue@MBNumber>Length@MBRepInput, Print["The series number to be evaluated is greater than the total number of series solution found."];Abort[]];
If[Length@MBRepInput>1, Print["Solving MB Number : ",OptionValue@MBNumber]];

MBRepVars=MBRepInput[[OptionValue@MBNumber]];
Dim=MBRepDim[MBRepInput[[OptionValue@MBNumber]]];
IntVar=MBRepInput[[OptionValue@MBNumber]][[2]];

NumGamma=MBRepVars[[4,1]]//Select[#,Head[#]=!=List&]&;
DenGamma=MBRepVars[[4,2]]//Select[#,Head[#]=!=List&]&;
NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
MBRepVarsFlatten= ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];

NumCof=Coefficient[#,IntVar]&/@NumGamma;
DenCof=Coefficient[#,IntVar]&/@DenGamma;


TimeTaken=AbsoluteTiming[

If[!PrintSol,MasterSeriesQ=False];
FindIntersectionReturn=FindIntersection[MBRepVars,MaxRequestedSeries,MasterSeriesQ];
{F,Comb,Poles,msg1,Msg2,MasterCharList,MasterSeriesVar,Degenerate}=FindIntersectionReturn[[{1,2,3,4,5,9,10,11}]];

FSort=Sort[F, Length[#1] < Length[#2] &]//Sort;
RearrangePos=Flatten[Position[F,#]&/@FSort];
If[MasterSeriesQ&&Degenerate,
MasterCharList=MasterCharList[[RearrangePos]];
MasterSeriesVar=MasterSeriesVar[[RearrangePos]];
];

Do[
CombLabels=FSort[[SeriesRepCount]];
Which[Degenerate&&(Dim>=2)&&MasterSeriesQ,
Msg2=Append[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Cardinality `2`. Intersecting Conic Hulls `3`. Set of poles :: `4` with master series characteristic list and variables `5`."
,SeriesRepCount,Length@CombLabels,Subscript[C,Sequence@@Comb[[#]]]&/@CombLabels,Expand[Poles[[#]]]&/@CombLabels,Join[{MasterCharList[[SeriesRepCount]]},{MasterSeriesVar[[SeriesRepCount]]}]]] 

,Degenerate&&(Dim===1)&&MasterSeriesQ,
Msg2=Append[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Cardinality `2`. Intersecting Conic Hulls `3`. Set of poles :: `4` with master series characteristic list and variables `5`."
,SeriesRepCount,Length@CombLabels,Subscript[C,Sequence@@Comb[[#]]]&/@CombLabels,Expand[Poles[[#]]]&/@CombLabels,Join[{MasterCharList[[SeriesRepCount]]},{MasterSeriesVar[[SeriesRepCount]]}]]]

,True,
    Msg2=Append[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Cardinality `2`. Intersecting Conic Hulls `3`. Set of poles :: `4`.",
SeriesRepCount,Length@CombLabels,Subscript[C,Sequence@@Comb[[#]]]&/@CombLabels,Expand[Poles[[#]]]&/@CombLabels]];
    ];
    
    ,{SeriesRepCount,Length@FSort}];

LengthList=Length[#]&/@(FindIntersectionReturn[[1]]);
LengthTally=Tally[LengthList];
LengthTally=Sort[LengthTally,#1[[1]]<#2[[1]]&];
Do[LengthMessage = LengthMessage<>"Cardinality "<>ToString[LengthTally[[i]][[1]]]<>":: Solution found "<>ToString[LengthTally[[i]][[2]]]<>".\n", {i, Length@LengthTally}];

Print["Found ",Length@Msg2," series solutions."];
Print[LengthMessage];

If[PrintSol,
Do[Print[Msg2[[i]]]; Print[""],{i,Length[Msg2]}]]];

Print["Time Taken ",TimeTaken[[1]]," seconds"];

FindIntersectionReturn=ReplacePart[FindIntersectionReturn,{1->FSort,8->MBRepInput[[OptionValue@MBNumber]]}];
AppendTo[FindIntersectionReturn,DumVar];

Return[FindIntersectionReturn]]


(* ::Subsection::Closed:: *)
(*Module to find solutions using Triangulation Approach*)


Options[TriangulateMB]={MasterSeries->True,MaxSolutions->Infinity,PrintSolutions->True,ShortestOnly->False,TopComPath->"/usr/local/bin/"
,TopComParallel->True,Cardinality->None,MaxCardinality->None,SolutionSummary->False,QuickSolve->False,MBNumber->1};

TriangulateMB[MBRepIn_,OptionsPattern[]]:=Module[{A,NumLength,MBNumberVal=OptionValue@MBNumber,
Triangulation,AllTriangulations={},RegMsg="regular",CardMsg="",Msg2={},MBRepVars,
MaxRequestedSeries=OptionValue[MaxSolutions],TimeTaken,TopComOptions="",
SGN={},ConHull={},B={},R={},Poles={},RetTup=1,Comb,NumDenIntCof,Degenerate,RR,Msg1,Dim,
MasterCH ={},MasterCHNorVec ={},MasterCHBasVec ={},MasterPoles ={},MasterCharList={},MasterSeriesVar ={},temp,
Labels,CrossProd,j,AllPosSub,RepeatedTermLabel,IntVar,MasterSeriesMsg={},RequestCard=False,
RequestMaxCard=False,LMasterCHNorVec,LengthMessage="",LengthList,LengthTally,
MSeries=OptionValue@MasterSeries,MaxSol=OptionValue@MaxSolutions,PrintSol=OptionValue@PrintSolutions,
ShortOnly=OptionValue@ShortestOnly,RegOnly=True,Card=OptionValue@Cardinality,MaxCard=OptionValue@MaxCardinality,
QSolve=OptionValue@QuickSolve,SolSummary=OptionValue@SolutionSummary,TPCommand="points2alltriangs",
ProportionalQ,AnyProportionalQ,Cof,Num,NumCof,DenCof,Delta,ResolveMBOut,BreakTri=False,ATrim,
NumGamma,DenGamma,NumFlat,DenFlat,MBRepVarsFlatten,NumGammaCof,DenGammaCof,NumDenIntGammaCof,MBRepVarsGamma,
MBRepInput=Most[MBRepIn],DumVar=MBRepIn[[-1]],CheckCommand},

If[OptionValue@MBNumber>Length@MBRepInput, Print["The series number to be evaluated is greater than the total number of series solution found."];Abort[]];
If[Length@MBRepInput>1, Print["Solving MB Number : ",MBNumberVal]];

CheckCommand="which "<>OptionValue[TopComPath]<>"points2triangs";
If[StringLength[RunProcess[$SystemShell,"StandardOutput",CheckCommand]]===0, Print["We could not find TOPCOM executables at ",OptionValue[TopComPath]];Abort[]];

MBRepVars=MBRepInput[[MBNumberVal]][[{1,2,3,4}]];
Dim=MBRepDim[MBRepInput[[MBNumberVal]]];
AllPosSub=Subsets[Range[MBRepDim[MBRepInput[[MBNumberVal]]]],{Dim-1}];
IntVar=MBRepInput[[MBNumberVal]][[2]];
Num=MBRepInput[[MBNumberVal]][[4,1]];


NumGamma=MBRepVars[[4,1]]//Select[#,Head[#]=!=List&]&;
DenGamma=MBRepVars[[4,2]]//Select[#,Head[#]=!=List&]&;
NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
MBRepVarsFlatten= ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];
MBRepVarsGamma= ReplacePart[MBRepVars,{{4,1}->NumGamma,{4,2}->DenGamma}];
Cof=MakeCof[MBRepVarsFlatten];


TimeTaken=AbsoluteTiming[

If[Head@Card!=Symbol&&Card\[NotElement]PositiveIntegers,Print["Cardinality can only be set to non-negative integers"];Abort[]];
If[Head@MaxCard!=Symbol&&MaxCard\[NotElement]PositiveIntegers,Print["MaxCardinality can only be set to non-negative integers"];Abort[]];

If[SolSummary,PrintSol=False;MSeries=False;ShortOnly=False];

If[QSolve&&(Head[Card]===Integer||Head[MaxCard]===Integer),Print["Cardinality or Maxcardinality can't be set during QuickRun. Ignoring them!"]];

{A,ATrim,NumLength}=FindMBAMatrix[MBRepVarsFlatten];

Print["The associated A-matrix for this MB integral is ",ATrim//MatrixForm];

If[Length@ATrim===1,Print["Triangulation Method currently works only when the A-matrix is not a row-matrix!"];
Print["Switching to the Conic Hull Method."];
ResolveMBOut=ResolveMB[MBRepInput,MaxSolutions->MaxSol,MBNumber->MBNumberVal];
BreakTri=True;
Return@ResolveMBOut;];
If[QSolve,TPCommand="points2placingtriang",

{RetTup,Comb,Poles,R,B,NumDenIntCof,Degenerate,RR,Msg1,SGN}=FindAllGoodComb[MBRepVars];
 Print@Msg1;
 TPCommand="points2alltriangs";
 ];

A=Transpose@A;

PrintTemporary["Running Topcom"];

If[(Head[Card]===Integer||Head[MaxCard]===Integer)&&(!RegOnly),Print["Cannot set cardinality or maxcardinality for non-regular triangulations!"];Abort[]];
If[Head[Card]===Integer&&Head[MaxCard]===Integer,Print["Cannot set cardinality and maxcardinality simultaneously!"];Abort[]];
If[RegOnly,TopComOptions=TopComOptions<>" --regular",TopComOptions=TopComOptions<>" --nonregular";RegMsg="non-regular"];
If[OptionValue@TopComParallel,TopComOptions=TopComOptions<>" --parallelenumeration --workbuffercontrol"];
If[Head[Card]===Integer&&RegOnly,RequestCard=True;TopComOptions=TopComOptions<>" --cardinality "<>ToString[Card];CardMsg=" with cardinality "<>ToString[Card]];
If[Head[MaxCard]===Integer&&RegOnly,RequestMaxCard=True;TopComOptions=TopComOptions<>" --maxcardinality "<>ToString[MaxCard];CardMsg=" with maximum cardinality "<>ToString[MaxCard]];


If[RequestCard,PrintTemporary["Checking Cardinality"]];
If[RequestMaxCard,PrintTemporary["Checking MaxCardinality"]];

While[Length@AllTriangulations===0,
AllTriangulations=FindTriangulations[A,OptionValue@TopComPath,TopComOptions,MaxRequestedSeries,CardMsg,TPCommand];
];
PrintTemporary["Done!"];

PrintTemporary["Sorting Triangulations"];
AllTriangulations=Sort[AllTriangulations,Length[#1]<Length[#2]&];
PrintTemporary["Done!"];

Print["Found ",Length@AllTriangulations," "<>RegMsg<>" triangulations"<>CardMsg<>"."];

If[ShortOnly,AllTriangulations=AllTriangulations[[{1}]]];

If[!RequestCard&&!QSolve,
Print["The shortest series solution found is of length ",Length[First@AllTriangulations],"."]];

If[QSolve,Print["Solution found of length ",Length[First@AllTriangulations],"."]];

LengthList=Length[#]&/@(AllTriangulations);
LengthTally=Tally[LengthList];
Do[LengthMessage = LengthMessage<>"Cardinality "<>ToString[LengthTally[[i]][[1]]]<>":: Solution found "<>ToString[LengthTally[[i]][[2]]]<>".\n", {i, Length@LengthTally}];
If[!RequestCard,Print[LengthMessage]];


If[QSolve,
PrintTemporary["Deriving associated poles."];
Comb=Sort[Complement[Range@NumLength,#]&/@AllTriangulations[[1]]];
RR=x/@Range[MBRepDim[MBRepVarsFlatten]];
Monitor[
Do[
SGN=Insert[SGN,Sign[Det[Extract[Rest[Cof],#]&/@i]],RetTup];

If[MSeries,
ConHull=Insert[ConHull,ConicHullRegion[{Cof[[1]]},Extract[Rest[Cof],#]&/@i],RetTup];
B=Insert[B,Refine[RegionMember[ConHull[[RetTup]], RR], RR\[Element] Reals]/.{GreaterEqual->Greater,LessEqual->Less},RetTup];
R=Insert[R,ImplicitRegion[B[[RetTup]],Evaluate[RR]],RetTup];
,ConHull={};
B={};
R={}];

j=1;
Poles=Insert[Poles,Flatten[IntVar/.Solve[And@@{NumFlat[[#]]==-Subscript[n, j++]}&/@i,IntVar]],RetTup];
RetTup++,
{i,Comb}];
,ProgressIndicator[Flatten[Position[Comb,i]][[1]],{1,Length@Comb}]];


Cof=Rest[MakeCof[MBRepVarsGamma]];
NumGammaCof=IntVar . Cof[[#]]&/@Range[Length@NumGamma];
DenGammaCof=IntVar . Cof[[#]]&/@Range[Length@NumGamma+1,Length@Cof];
NumDenIntGammaCof=Join[NumGammaCof,-DenGammaCof];
Delta=Total@NumDenIntGammaCof;



ProportionalQ[x1_,y1_]:=With[{x=Coefficient[x1,IntVar],y=Coefficient[y1,IntVar]},If[x . y/(Norm[x]Norm[y])===1,Return[True],Return[False]]];
Which[ FullSimplify[Delta]===0,
Degenerate=True ; Msg1=StringForm["Degenerate case"],
AnyProportionalQ=Catch[Do[If[ProportionalQ[Delta,i],Throw[True],Continue[]],{i,NumCof}];Throw[False]];
AnyProportionalQ,Degenerate=False; Msg1=StringForm["Degenerate case with \[CapitalDelta] proportional to one or more coefficient vectors"],
True,
Degenerate=False;Msg1=StringForm["Non-Degenerate case"]] ;

Print@Msg1;
PrintTemporary["Done!"];
];

PrintTemporary["Rearranging Triangulations"];

If[!SolSummary,
Triangulation=Sort@With[{AllTriangulationsW=AllTriangulations,CombW=Comb,NumLengthW=NumLength},
ParallelTable[Sort[(Position[CombW,Complement[Range@NumLengthW,#]]//Flatten//First)&/@AllTriangulationsW[[i]]],{i,Length@AllTriangulationsW}]];
];

PrintTemporary["Done!"];



If[MSeries&&!SolSummary&&Degenerate,

PrintTemporary["Finding Masters Series"];


Do[

Labels=Triangulation[[k]];
Which[Degenerate&&(Dim>=2),

MasterCH=Insert[MasterCH,FullSimplify[And@@B[[Labels]]],-1];
MasterCHNorVec=Insert[MasterCHNorVec,temp=MasterCH[[k]];Table[Coefficient[Subtract@@(temp[[i]]),Evaluate@RR],{i,Length[temp]}],-1];

LMasterCHNorVec=Sort[Last@MasterCHNorVec];
If[Length@temp>Dim, MasterCHNorVec=ReplacePart[MasterCHNorVec,-1->LMasterCHNorVec[[Flatten[Position[#,Except[0,_?NumericQ],1,1]&/@Last@QRDecomposition@Transpose@LMasterCHNorVec]]]]];

MasterCHBasVec=Insert[MasterCHBasVec,Table[CrossProd=Cross@@(MasterCHNorVec[[k]][[i]]);
If[ MasterCH[[k]] [[Complement[Range[Dim],i]]]/.(RR[[#]]-> CrossProd[[#]]&/@Range[Dim]),CrossProd,-CrossProd],{i,AllPosSub}],-1];


j=1;
MasterPoles=Insert[MasterPoles,Flatten@Solve[And@@(Table[IntVar . MasterCHBasVec[[k]][[i]]==-Subscript[n, j++],{i,Dim}]),IntVar],-1];
MasterCharList=Insert[MasterCharList,Join[NumDenIntCof/.MasterPoles[[k]],Subscript[n, #]&/@Range[Dim]],-1];
 RepeatedTermLabel={};

Do[If[MemberQ[RepeatedTermLabel,i]||MemberQ[RepeatedTermLabel,j],Continue[]];If[FullSimplify[Total@MasterCharList[[k]][[{i,j}]]]===0,RepeatedTermLabel=Union[RepeatedTermLabel,{i,j}]],{i,1,Length@MasterCharList[[k]]-1},{j,i+1,Length@MasterCharList[[k]]}];
MasterCharList[[k]]=Delete[MasterCharList[[k]],Table[{RepeatedTermLabel[[i]]},{i,Length[RepeatedTermLabel]}]]//FullSimplify;
MasterSeriesVar=Insert[MasterSeriesVar,Table[Times@@((MBVar@MBRepVarsFlatten)^Coefficient[IntVar/.MasterPoles[[k]],Subscript[n, i]]),{i,Dim}],-1];
MasterSeriesMsg=Append[MasterSeriesMsg,StringForm[" with master series characteristic list and variables `1`."
,Join[{MasterCharList[[k]]},{MasterSeriesVar[[k]]}]]] 

,Degenerate&&(Dim===1),
MasterCharList=Insert[MasterCharList,Subscript[n,1],-1];
If[ContainsOnly[SGN[[Labels]],{-1}],MasterSeriesVar=Insert[MasterSeriesVar,MBVar@MBRepVarsFlatten,-1],MasterSeriesVar=Insert[MasterSeriesVar,1/MBVar@MBRepVarsFlatten,-1]];

MasterSeriesMsg=Append[MasterSeriesMsg,StringForm["with master series characteristic list and variables `1`."
,Join[{MasterCharList[[k]]},{MasterSeriesVar[[k]]}]]]
];

,{k,Length@Triangulation}],

MasterSeriesMsg=ConstantArray[".",Length@Triangulation]

];

If[PrintSol,
Do[

AppendTo[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Cardinality `2`. Set of poles :: `3`",
i,Length@Triangulation[[i]],Expand[Poles[[#]]]&/@(Triangulation[[i]])]],
{i,Length@Triangulation}];

Do[
Print[Msg2[[i]],MasterSeriesMsg[[i]]]; Print[""],{i,Length[Msg2]}]];
];
If[BreakTri,Return[ResolveMBOut]];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
Return[{Triangulation,Comb,Poles,{},{},SGN,Length@Triangulation,MBRepVars,R,RetTup,B,MasterCharList,MasterSeriesVar,DumVar}]]


(* ::Subsection::Closed:: *)
(*Module to Cross-Check Solutions*)


CrossCheckSolutions[Solutions_]:=Module[{RegCond,TimeTaken,CheckSol=True,NonSolutionLabels,B=Solutions[[11]],RetTup=Solutions[[10]],R=Solutions[[9]],Reg,SolutionLabels=Solutions[[1]]},
TimeTaken=AbsoluteTiming[
Do[
RegCond=FullSimplify[And@@B[[SolutionLabels[[i]]]]]; 
Reg=RegionIntersection@@R[[SolutionLabels[[i]]]];
If[Head@Reg===EmptyRegion,CheckSol=False;Break[]];
NonSolutionLabels=Complement[Range@RetTup,SolutionLabels[[i]]];
Do[If[RegionDisjoint[Reg,R[[j]]],Continue[],CheckSol=False;Break[]],{j,NonSolutionLabels}];
If[CheckSol,Continue[],Break[]];
,{i,Length@SolutionLabels}];
If[CheckSol,Print[Style["Solution Consistent!",Green,Bold]],Print[Style["Solution Inconsistent!",Red,Bold]]];
];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
Return[RegCond];
]


(* ::Subsection::Closed:: *)
(*Module for Analytic Solution*)


Options[EvaluateSeries]={PrintSeries->True};

EvaluateSeries[ResMB_,MBParaSub_List,SeriesNum_Integer,OptionsPattern[]]:=Module[{CalculateResidueReturn,Msg3,SeriesNumber,TotalSeries=ResMB[[7]],
Msg4,TimeTaken,PrintSol=OptionValue@PrintSeries,DumVar=ResMB[[-1]]},

TimeTaken=AbsoluteTiming[

If[SeriesNum>TotalSeries,Print["The series number to be evaluated is greater than the total number of series solution found."];Abort[]];

CalculateResidueReturn=CalculateResidue@@(Join[{ResMB[[8]],ResMB[[1]][[SeriesNum]]}/.MBParaSub,ResMB[[{2,3,6}]]/.MBParaSub,ResMB[[{-1}]]]);
SeriesNumber=CalculateResidueReturn[[1]];
Msg3=CalculateResidueReturn[[3]];
Msg4=StringForm["The series solution is a sum of the following `` series.", SeriesNumber];
Print[Msg4];
If[PrintSol,
Do[Print[Msg3[[i]]]; Print[""],{i,SeriesNumber}]];
];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
If[$KernelCount==0,LaunchKernels[$ProcessorCount]];
Return[CalculateResidueReturn]
]


(* ::Subsection::Closed:: *)
(*Module to perform Numerical Summation*)


Options[SumAllSeries]={RunInParallel-> False,NumericalPrecision -> MachinePrecision};
SumAllSeries[SeriesInfo_,MBVarSub_,lim_,OptionsPattern[]]:=Module[{SeriesNumber=SeriesInfo[[1]],SeriesSub=SeriesInfo[[2]]/.MBVarSub,
SeriesCondition=SeriesInfo[[5]],Dim=SeriesInfo[[4]],sumlim,nseries={},SumCommand,TimeTaken},
TimeTaken=AbsoluteTiming[If[lim \[NotElement] NonNegativeIntegers,Print["Summation upper-limit should be a nonnegative integer"];Abort[]];
Which[OptionValue[RunInParallel]===False,SumCommand=Sum;Off[General::munfl];
 Off[General::stop],OptionValue[RunInParallel]===True,SumCommand=ParallelSum;ParallelEvaluate[Off[General::munfl]];
 ParallelEvaluate[Off[General::stop]]];
sumlim=Table[{Subscript[n, i],0,lim},{i,Dim}];
SetSharedVariable[nseries];
 Monitor[Do[
With[{CondExtract=SeriesCondition[[i]],
SeriesExtract=SeriesSub[[i]],RequestedPrecision=OptionValue[NumericalPrecision]},
nseries=Insert[nseries,SumCommand[If[CondExtract,N[SeriesExtract,RequestedPrecision]//Quiet,0],Evaluate[Sequence@@sumlim]],-1]],
{i,SeriesNumber}]

,ProgressIndicator[i,{1,SeriesNumber}]];

];

Print["Numerical Result: ",N[Total@nseries,OptionValue[NumericalPrecision]]//Quiet];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
(*New*)
Return[N[Total@nseries,OptionValue[NumericalPrecision]]//Quiet]
]


(* ::Section::Closed:: *)
(*End Message*)


End[];
EndPackage[];
Print["Last Updated: \!\(\*SuperscriptBox[\(12\), \(th\)]\) December, 2025"];
Print["Version 1.3.4 by S. Banik & S. Friot"];
