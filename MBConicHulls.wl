(* ::Package:: *)

BeginPackage["MBConicHulls`"];


Quiet[Needs["Combinatorica`"];]
Block[{Print}, Get["MultivariateResidues`"];]
Off[SymbolName::sym];


MBRep::"usage"="MBRep[PreFac_,IntVar_,MBVar_,MBArg_] inputs the MB integral in a form that can be processed by the package where :\n\
PreFac - is the prefactor of the MB integral.\n\
IntVar - takes in the list of integration variables.\n\
MBVar - takes in the list of parameters, where MBVar[[i]] is raised to the power IntVar[[i]].\n\
MBArg - is a list consisting of two sublists of the form {{numerator},{denominator}}, where the elements of {numerator} and {denominator} are the arguments of the gamma functions in the numerator and denominator of the MB integrand, respectively.";
ResolveMB::"usage"="ResolveMB[MBRepOut_,N_]  returns the type of integral (degenerate or nondegenerate) and the total number of conic hulls, and then goes on to display the list of relevant intersecting conic hulls and the sets of poles for each series representation along with the master series characteristic list and variables for the degenerate case. The function arguments are: \n\
MBRepOut - is the output of the MBRep[] function. \n\
N - is an optional parameter indicating the total number of series of the integral that one wishes to evaluate. If N is not specified, then all the non-zero series are shown.";
EvaluateSeries::"usage"="EvaluateSeries[ResolveMBOut_, MBParaSub_, SeriesNum_] calculates and prints the series expression of the series representation SeriesNum. The function arguments are: \n\
ResolveMBOut - is the output of the ResolveMB[] function. \n\
MBParaSub - is a list of substitutions to be made to the parameters in the arguments of the gamma functions of the integrand. \n\
SeriesNum - is the number, as enumerated in the output of ResolveMB[], of the series for which we wish to calculate residues.
";
SumAllSeries::"usage"="SumAllSeries[EvaluateSeriesOut_,MBVarSub_,SumLim_,RunInParallel-> Bool,NumericalPrecision -> PositiveIntegers] numerically sums the series of the particular series representation evaluated by EvaluateSeries[] and prints the result. The function arguments are: \n\
EvaluateSeriesOut - is the output of the EvaluateSeries[] function. \n\
MBVarSub - is a list of substitutions to provide numerical values to the parameters in MBVar. \n\
SumLim - is the upper limit of all the summation indices of the residue sum. \n\
RunInParallel -> Bool - is an optional parameter indicating whether the sum has to be performed using Mathematica's parallel processing functionality or not. The default value of Bool is False. \n\
NumericalPrecision -> PositiveIntegers - is an optional parameter of SumAllSeries[], which determines the precision of the numerical value of the series representation. The default value is MachinePrecision.";
n::"usage"="Summation Indices";
Substitute::"usage"="Substitute -> {} - is an optional parameter of MBRep[], to substitute the values of parameters appearing in the arguments of the gamma functions in the MB integrand";
TakeLimit::"usage"="Substitute -> {} - is an optional parameter of MBRep[], to substitute the values of parameters appearing in the arguments of the gamma functions in the MB integrand";
RunInParallel::"usage"="RunInParallel -> Bool - is an optional parameter of SumAllSeries[], indicating whether the sum to be performed using Mathematica's parallel processing functionality. The default value of Bool is False.";
NumericalPrecision::"usage"="NumericalPrecision -> PositiveIntegers - is an optional parameter of SumAllSeries[], which determines the precision of the numerical value of the series representation. The default value is MachinePrecision.";


Begin["`Private`"]


MBRep[PreFacRaw_,IntVar_,MBVar_,MBArg_,OptionsPattern[{Substitute->{},TakeLimit->{}}]]:=Module[{MBArgNew=(RemoveCommonGamma@MBArg)/.OptionValue[Substitute],
Contour,IntVarNew,ContourString,AllVariables,PreFac=PreFacRaw/.OptionValue[Substitute]},
If[Length@IntVar=!=Length@MBVar,Print["The number of Integration variables and MB variables should be same."]; Abort[];];

Which[ContainsOnly[Head@#&/@IntVar,{Rule}],
IntVarNew=First@#&/@IntVar;
Contour=Last@#&/@IntVar; 
ContourString=Table[StringJoin[{ToString[Re[ToString[IntVarNew[[i]],StandardForm]],StandardForm]," = ",ToString[Contour[[i]],StandardForm]}],{i,Length@IntVarNew}];
Print["Straight Contour : ",ContourString];
AllVariables=Variables[Flatten[MBArgNew/.OptionValue[TakeLimit]]];
If[Length@AllVariables>Length@IntVarNew,Print["Please provide the values of the following parameters : ",Complement[AllVariables,IntVarNew]];Abort[]];
Return[StraightToNonStraight[PreFac,IntVarNew,MBVar,MBArgNew,Contour,OptionValue[TakeLimit]]],

ContainsNone[Head@#&/@IntVar,{Rule}],
Print["Non-Straight Contours."];
Return[{PreFac,IntVar,MBVar,MBArgNew}],

True,
Print["Input not in correct format! Please see the documentation."]
];

];


MBRep[X___]:=If[Length@List@X!=4,Print["Four arguments expected, but ",Length@List@X," given."];Abort[]]


RemoveCommonGamma[NumDen_]:=Module[{Counter=1,CurrentNum,Num=Reverse[NumDen[[1]]],Den=NumDen[[2]],RemovePos},
While[Counter<=Length@Num,CurrentNum=Num[[Counter]];
If[MemberQ[Den,CurrentNum],Den=DeleteElements[Den,1->{CurrentNum}];Num=DeleteElements[Num,1->{CurrentNum}],Counter++];
];
Return[{Reverse@Num,Den}]];


StraightToNonStraight[PreFactor_,IntVar_,MBVar_,MBIntegrand_,Contour_,TakeLimit_]:=Module[{Num=MBIntegrand[[1]],
Den=MBIntegrand[[2]],NumContour,NumContourLim,NegativeNumPos,NegativeNum,NegativeNumVal,NewPrefactor=PreFactor,MBArg},

NumContour=Num/.Table[IntVar[[i]]->Contour[[i]],{i,Length@IntVar}];
NumContourLim=NumContour/.TakeLimit;
If[And@@(!Element[#,NonPositiveIntegers]&/@NumContourLim),Nothing,Print["Contour ill-defined! It passes through one or more poles of the MB integral."]; Abort[]];
NegativeNumPos=Table[If[NumContourLim[[i]]<0,i ,Nothing,Nothing],{i,Length@Num}];
NegativeNum=Num[[NegativeNumPos]];
NegativeNumVal=Abs[IntegerPart/@NumContourLim[[NegativeNumPos]]];
Do[{Num,Den,NewPrefactor}=TransformMB[NegativeNumVal[[i]],NegativeNum[[i]],Num,Den,NewPrefactor],{i,Length@NegativeNum}];
If[Length@NegativeNum>0,Num=SortBy[Num,Length[MonomialList@#]&]];
MBArg=RemoveCommonGamma[{Num,Den}];
Return[{NewPrefactor,IntVar,MBVar,MBArg}]
]


TransformMB[n_,z_,Num_,Den_,Prefactor_]:=Module[{NewNum=DeleteElements[Num,1->{z}],NewDen,NewPrefactor},
NewNum=Join[NewNum,{-n-z,1+n+z}];NewDen=Join[Den,{1-z}];NewPrefactor=Prefactor*(-1)^(n+1);
Return[{NewNum,NewDen,NewPrefactor}]];


positionDuplicates[l_]:=Module[{list=l},GatherBy[Range@Length[list],list[[#]]&]];
MBRepDim[MBRepVars_]:=(Length[MBRepVars[[2]]])
MergeNumDen[MBRepVars_]:=(Flatten[MBRepVars[[4]]]) 
Num[MBRepVars_]:=MBRepVars[[4,1]]
NumLength[MBRepVars_]:=(Length[MBRepVars[[4,1]]])
MBVar[MBRepVars_]:=MBRepVars[[3]]


MakeCof[MBRepVars_]:=Module[{NumDen=MergeNumDen[MBRepVars],IntVar=MBRepVars[[2]]},
Table[Coefficient[NumDen[[i]],IntVar],{i,0,Length[NumDen]}]]


FindAllComb[MBRepVars_]:=Module[{NumberNum=NumLength[MBRepVars],Dim=MBRepDim[MBRepVars]},Subsets[Range@NumberNum,{Dim}]]


FindAllGoodComb[MBRepVars_]:=Module[{Cof=MakeCof[MBRepVars],Num=MBRepVars[[4,1]],AllComb=FindAllComb[MBRepVars],IntVar=MBRepVars[[2]],RR=x/@Range[MBRepDim[MBRepVars]],RetTup=1,SGN={},
Comb={},ConHull={},B={},R={},Poles={},j,NumDenIntCof,Degenerate,Msg1,Delta,NumCof,DenCof,ProportionalQ,AnyProportionalQ},
Do[
SGN=Insert[SGN,Sign[Det[Extract[Rest[Cof],#]&/@i]],RetTup];
 If[SGN[[RetTup]]!= 0, 
Comb=Insert[Comb,i,RetTup];
ConHull=Insert[ConHull,ConicHullRegion[{Cof[[1]]},Extract[Rest[Cof],#]&/@i],RetTup];
B=Insert[B,Refine[RegionMember[ConHull[[RetTup]], RR], RR\[Element] Reals]/.{GreaterEqual->Greater,LessEqual->Less},RetTup];
R=Insert[R,ImplicitRegion[B[[RetTup]],Evaluate[RR]],RetTup];
j=1;
Poles=Insert[Poles,Flatten[IntVar/.Solve[And@@{Num[[#]]==-Subscript[n, j++]}&/@i,IntVar]],RetTup];
RetTup++] ,
{i,AllComb}];
Cof=Rest@Cof;
NumCof=IntVar . Cof[[#]]&/@Range[Length@Num];
DenCof=IntVar . Cof[[#]]&/@Range[Length@Num+1,Length@Cof];
NumDenIntCof=Join[NumCof,-DenCof];
Delta=Total@NumDenIntCof;
ProportionalQ[x1_,y1_]:=With[{x=Coefficient[x1,IntVar],y=Coefficient[y1,IntVar]},If[x . y/(Norm[x]Norm[y])===1,Return[True],Return[False]]];
Which[ FullSimplify[Delta]===0,
Degenerate=True ; Msg1=StringForm["Degenerate case with `1` conic hulls",--RetTup],
AnyProportionalQ=Catch[Do[If[ProportionalQ[Delta,i],Throw[True],Continue[]],{i,NumCof}];Throw[False]];
AnyProportionalQ,Degenerate=False; Msg1=StringForm["Degenerate case with \[CapitalDelta] proportional to one or more coefficient vectors and associated with `1` conic hulls",--RetTup],
True,
Degenerate=False;Msg1=StringForm["Non-Degenerate case with `1` conic hulls",--RetTup]] ;
Return[{RetTup,Comb,Poles,R,B,NumDenIntCof,Degenerate,RR,Msg1,SGN}]
]


FindIntersection[MBRepVars_,RequestedSeries_]:=Module[{IntVar=MBRepVars[[2]],RetTup,Comb,Poles,R,B,NumDenIntCof,Degenerate,RR,SeriesRepCount=0,
Counter=0,Branch=1,Dim=MBRepDim[MBRepVars],F={},Reg,Labels,NextLabel,IntersectingLabel,
LargestSubsetQ,CrossProd,AllPosSub=Subsets[Range[MBRepDim[MBRepVars]],{MBRepDim[MBRepVars]-1}], MasterCH ={},
MasterCHNorVec ={},MasterCHBasVec ={},MasterPoles ={},MasterCharList={},MasterSeriesVar ={},Msg2={},j,RepeatedTermLabel,
temp,Msg1,SGN,DupTuplesList,UniqueTuples,CombLabels},

{RetTup,Comb,Poles,R,B,NumDenIntCof,Degenerate,RR,Msg1,SGN}=FindAllGoodComb[MBRepVars];
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

Which[Degenerate&&(Dim>=2),

MasterCH=Insert[MasterCH,FullSimplify[And@@B[[Labels]]],-1];
MasterCHNorVec=Insert[MasterCHNorVec,temp=MasterCH[[++SeriesRepCount]];Table[Coefficient[Subtract@@(temp[[i]]),Evaluate@RR],{i,Dim}],-1];
MasterCHBasVec=Insert[MasterCHBasVec,Table[CrossProd=Cross@@(MasterCHNorVec[[SeriesRepCount]][[i]]);
If[ MasterCH[[SeriesRepCount]] [[Complement[Range[Dim],i]]]/.(RR[[#]]-> CrossProd[[#]]&/@Range[Dim]),CrossProd,-CrossProd],{i,AllPosSub}],-1];
j=1;
MasterPoles=Insert[MasterPoles,Flatten@Solve[And@@(Table[IntVar . MasterCHBasVec[[SeriesRepCount]][[i]]==-Subscript[n, j++],{i,Dim}]),IntVar],-1];
MasterCharList=Insert[MasterCharList,Join[NumDenIntCof/.MasterPoles[[SeriesRepCount]],Subscript[n, #]&/@Range[Dim]],-1];
 RepeatedTermLabel={};

Do[If[MemberQ[RepeatedTermLabel,i]||MemberQ[RepeatedTermLabel,j],Continue[]];If[FullSimplify[Total@MasterCharList[[SeriesRepCount]][[{i,j}]]]===0,RepeatedTermLabel=Union[RepeatedTermLabel,{i,j}]],{i,1,Length@MasterCharList[[SeriesRepCount]]-1},{j,i+1,Length@MasterCharList[[SeriesRepCount]]}];
MasterCharList[[SeriesRepCount]]=Delete[MasterCharList[[SeriesRepCount]],Table[{RepeatedTermLabel[[i]]},{i,Length[RepeatedTermLabel]}]]//FullSimplify;
MasterSeriesVar=Insert[MasterSeriesVar,Table[Times@@((MBVar@MBRepVars)^Coefficient[IntVar/.MasterPoles[[SeriesRepCount]],Subscript[n, i]]),{i,Dim}],-1];
Msg2=Append[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Intersecting Conic Hulls `2`. The set of poles is :: `3` with master series characteristic list and variables `4`."
,SeriesRepCount,Subscript[C,Sequence@@Comb[[#]]]&/@CombLabels,Poles[[#]]&/@CombLabels,Join[{MasterCharList[[SeriesRepCount]]},{MasterSeriesVar[[SeriesRepCount]]}]]] 

,Degenerate&&(Dim===1),
MasterCharList=Insert[MasterCharList,Subscript[n,1],-1];
If[ContainsOnly[SGN[[CombLabels]],{-1}],MasterSeriesVar=Insert[MasterSeriesVar,MBVar@MBRepVars,-1],MasterSeriesVar=Insert[MasterSeriesVar,1/MBVar@MBRepVars,-1]];

Msg2=Append[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Intersecting Conic Hulls `2`. The set of poles is :: `3` with master series characteristic list and variables `4`."
,++SeriesRepCount,Subscript[C,Sequence@@Comb[[#]]]&/@CombLabels,Poles[[#]]&/@CombLabels,Join[{MasterCharList[[SeriesRepCount]]},{MasterSeriesVar[[SeriesRepCount]]}]]]

,!Degenerate,

Msg2=Append[Msg2,StringForm["\!\(\*StyleBox[\"Series\",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\" \",FontWeight->\"Bold\"]\)\!\(\*StyleBox[\"Solution\",FontWeight->\"Bold\"]\) `1` :: Intersecting Conic Hulls `2`. The set of poles is :: `3`.",
++SeriesRepCount,Subscript[C,Sequence@@Comb[[#]]]&/@CombLabels,Poles[[#]]&/@CombLabels]]];
PrintTemporary["Number of Series Solution Found :: ", SeriesRepCount];
F=Insert[F,CombLabels,-1]];
Branch--;
If[Branch==1,Continue[]];
NextLabel=Last@Labels+1;
Labels=Most[Labels];
Reg=RegionIntersection@@R[[Labels]],

Labels=Insert[Labels,IntersectingLabel,-1];Branch++;
Reg=RegionIntersection[Reg,R[[IntersectingLabel]]];NextLabel=IntersectingLabel+1]];
Return[{F,Comb,Poles,Msg1,Msg2,SGN,Length@F,MBRepVars}]]


FindSingFactors[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_]:=Module[{NumberNum=NumLength[MBRepVars],NumDen=MergeNumDen[MBRepVars],Dim=MBRepDim[MBRepVars],DivConCond={},SingFacLabels={},AssociatePoles={},IntVar=MBRepVars[[2]],ConGamma,NumDenSub,NumDenSubD,NumDenSubN,DivGamma={},ConDivGamma={},SumVar,DivCond,ConCond,SingComb,NonSingComb,PoleSubstitute},


SumVar=Subscript[n, #]&/@Range[Dim];
ConGamma=Complement[Range[NumberNum],Union@@(Extract[Comb,#]&/@FSeriesSol)];
Do[
PoleSubstitute=IntVar[[#]]-> Poles[[i]][[#]]&/@Range[Dim];
NumDenSub=Together[NumDen/.PoleSubstitute//Simplify];
NumDenSubD=Denominator@NumDenSub;
NumDenSubN=Numerator@NumDenSub;
DivGamma={};
ConDivGamma={};
Do[If[Resolve[Exists[Evaluate[SumVar],Evaluate[SumVar]\[Element]NonNegativeIntegers,Evaluate@NumDenSubN[[j]]<= 0&&Mod[Evaluate@NumDenSubN[[j]],Evaluate@NumDenSubD[[j]]]==0]],If[Resolve[Exists[Evaluate[SumVar],Evaluate[SumVar]\[Element]NonNegativeIntegers,Evaluate@NumDenSubN[[j]]>0 ||Mod[Evaluate@NumDenSubN[[j]],Evaluate@NumDenSubD[[j]]]!= 0 ]],ConDivGamma=Insert[ConDivGamma,j,-1],DivGamma=Insert[DivGamma,j,-1]]],{j,Complement[Range[Length[NumDen]],Comb[[i]],ConGamma]}];

Do[DivCond=True;ConCond=True;SingComb=Union[DivGamma,o];If[MemberQ[SingFacLabels,Sort[Union[Comb[[i]],SingComb]]],Continue[]];NonSingComb=Complement[ConDivGamma,o];

Do[DivCond=And[DivCond,FullSimplify[Evaluate@NumDenSubN[[k]]<= 0&&Mod[Evaluate@NumDenSubN[[k]],Evaluate@NumDenSubD[[k]]]==0,Evaluate[SumVar]\[Element]NonNegativeIntegers]],{k,SingComb}];

Do[ConCond=And[ConCond,FullSimplify[Evaluate@NumDenSubN[[l]]>0 ||Mod[Evaluate@NumDenSubN[[l]],Evaluate@NumDenSubD[[l]]]!= 0,Evaluate[SumVar]\[Element]NonNegativeIntegers]],{l,NonSingComb}];

If[Resolve[Exists[Evaluate[SumVar],Evaluate[SumVar]\[Element]NonNegativeIntegers,DivCond&&ConCond]],DivConCond=Insert[DivConCond,FullSimplify[DivCond&&ConCond,Evaluate[SumVar]\[Element]NonNegativeIntegers],-1];SingFacLabels=Insert[SingFacLabels,Sort[Union[Comb[[i]],SingComb]],-1];AssociatePoles=Insert[AssociatePoles,Poles[[i]],-1],Nothing,Print["Error 3 ::",DivCond&&ConCond]],{o,Subsets[ConDivGamma]}]
,{i,FSeriesSol}];

Return[{SingFacLabels,AssociatePoles,DivConCond}];
]


GroupSingFactors[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_]:=Module[{GroupFacLabels={},NumberNum=NumLength[MBRepVars],Dim=MBRepDim[MBRepVars],
Cof=Rest@MakeCof[MBRepVars],SingFacLabels,OnlyNumLabels,CommonLabelsPos,LabelPos,LabelInsert,CounterTuple,CounterTupleSets,IdealCheck,
SingCombSum,SingCombSum1,ErrorIndicator,GroupCounter,CommonLabels,RepeatedCombLabel,co,F1={},AssociatePoles,DivConCond,SignFacsLabelsNum},

{SingFacLabels,AssociatePoles,DivConCond}=FindSingFactors[MBRepVars,FSeriesSol,Comb,Poles,SGN];

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

Return[{SingFacLabels,AssociatePoles,DivConCond,GroupFacLabels}];

]



CalculateResidue[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_]:=Module[{IntVar=MBRepVars[[2]],Dim=MBRepDim[MBRepVars],Cof=Rest@MakeCof[MBRepVars],
NumDen=MergeNumDen[MBRepVars],AssociatePoles,SingFacLabels,DivConCond,GroupFacLabels,SeriesNumber=0,NumberNum=NumLength[MBRepVars],
  NumDenIntPart, GenReflection,NumDenPoleSub,NumDenPoleShift,GenRefSub,Integrand,IntegrandSub,NumeratorFac,DenSingFac,TransformedVar,
  TransformMatrix,IntVarSwap,GbBasis,OverallFac,NumFacReduce,NumFacMonoList,RefinedFac,SeriesCof ,series={},
  SeriesCondition,Msg3={},seriestemp,ContourSign,SignSGN,SignFacsLabelsNum},


{SingFacLabels,AssociatePoles,DivConCond,GroupFacLabels}=GroupSingFactors[MBRepVars,FSeriesSol,Comb,Poles,SGN];


SeriesCondition={};
GenReflection[x_,y_]:=((-1)^(-y) Gamma[1-x]Gamma[1+x])/Gamma[1-y-x]; (* Generalized Relflection Formula *)
NumDenIntPart=Total[Cof[[#]]*IntVar]&/@Range[Length@NumDen];

Do[
NumDenPoleSub=NumDen/.(IntVar[[#]]-> AssociatePoles[[k]][[#]]&/@Range@Dim)//Simplify;
NumDenPoleShift=NumDen/.(IntVar[[#]]->IntVar[[#]]+ AssociatePoles[[k]][[#]]&/@Range@Dim)//Simplify;
GenRefSub=Table[Gamma@NumDenPoleShift[[j]]-> GenReflection[NumDenIntPart[[j]],NumDenPoleSub[[j]]],{j,SingFacLabels[[k]]}];
Integrand=MBRepVars[[1]] ( ( Times@@Gamma/@MBRepVars[[4,1]])Times@@(MBRepVars[[3]]^MBRepVars[[2]]))/Times@@Gamma/@MBRepVars[[4,2]];
IntegrandSub=Simplify[Integrand/.(IntVar[[#]]->IntVar[[#]]+ AssociatePoles[[k]][[#]]&/@Range@Dim)]/.GenRefSub;
NumeratorFac=Product[If[l>NumberNum,NumDenIntPart[[l]],1],{l,SingFacLabels[[k]]}];

Do[
DenSingFac=Table[Product[NumDenIntPart[[a]],{a,b[[c]]}],{c,Dim}];
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

Do[RefinedFac=Insert[RefinedFac,Table[Denominator[FactorTermsList[NumFacMonoList[[i]]][[2]]/TransformedVar[[j]]],{j,Dim}],-1];
SeriesCof=Insert[SeriesCof,FactorTermsList[NumFacMonoList[[i]]/Times@@TransformedVar][[1]],-1],{i,Length@NumFacMonoList}];


If[Dim===1, 
SignFacsLabelsNum=Select[Flatten[SingFacLabels],MemberQ[Range@NumberNum,#]&];
SignSGN=SGN[[SignFacsLabelsNum]];
Which[ContainsOnly[SignSGN,{-1}],ContourSign=-1,ContainsOnly[SignSGN,{1}],ContourSign=1,True,Print["Some Issues!"]],ContourSign=1];

seriestemp=(OverallFac)^(-1) Sum[SeriesCof[[j]]*MultivariateResidue[ContourSign*IntegrandSub,RefinedFac[[j]],Table[IntVar[[i]]-> 0,{i,Dim}]],{j,Length@RefinedFac}]//Simplify;

If[seriestemp===0,Continue[],SeriesNumber++;series=Insert[series,seriestemp,-1]];
SeriesCondition=Insert[SeriesCondition,DivConCond[[k]],-1];

Msg3= Insert[Msg3,StringForm["Series Number `1` :: `2` valid for `3`",SeriesNumber, series[[SeriesNumber]], If[SeriesCondition[[SeriesNumber]],And@@(Subscript[n, #]>= 0&/@Range[Dim]),Nothing,SeriesCondition[[SeriesNumber]]]],-1];

,{b,GroupFacLabels[[k]]}],{k,Length@DivConCond}];

Return[{SeriesNumber,series,Msg3,Dim,SeriesCondition}]]


ResolveMB[MBRepInput__]:=Module[{msg1, msg2,t2,MBRepVars={MBRepInput}[[1]],MaxRequestedSeries,TimeTaken},
TimeTaken=AbsoluteTiming[If[Length@{MBRepInput}==1,MaxRequestedSeries=Infinity,MaxRequestedSeries={MBRepInput}[[2]]];
t2=FindIntersection[MBRepVars,MaxRequestedSeries];
{msg1,msg2}=t2[[{4,5}]];
Print[""];Print[msg1];
Print["Found ",Length@msg2," series solutions."];
Print[""];
Do[Print[msg2[[i]]]; Print[""],{i,Length[msg2]}]];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
Return[t2]]


EvaluateSeries[ResMB_,MBParaSub_List,SeriesNum_Integer]:=Module[{t3,msg3,SeriesNumber,TotalSeries=ResMB[[7]],msg4,TimeTaken},
TimeTaken=AbsoluteTiming[If[SeriesNum>TotalSeries,Print["The series number to be evaluated is greater than the total number of series solution found."];Abort[]];
t3=CalculateResidue@@(Join[{ResMB[[8]],ResMB[[1]][[SeriesNum]]},ResMB[[{2,3,6}]]]/.MBParaSub);
SeriesNumber=t3[[1]];
msg3=t3[[3]];
msg4=StringForm["The series solution is a sum of the following `` series.", SeriesNumber];
Print[msg4];
Do[Print[msg3[[i]]]; Print[""],{i,SeriesNumber}]];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
Return[t3]
]


Options[SumAllSeries]={RunInParallel-> False,NumericalPrecision -> MachinePrecision};
SumAllSeries[SeriesInfo_,MBVarSub_,lim_,OptionsPattern[]]:=Module[{SeriesNumber=SeriesInfo[[1]],SeriesSub=SeriesInfo[[2]]/.MBVarSub,
SeriesCondition=SeriesInfo[[5]],Dim=SeriesInfo[[4]],sumlim,nseries={},SumCommand,TimeTaken},
TimeTaken=AbsoluteTiming[If[lim \[NotElement] NonNegativeIntegers,Print["Summation upper-limit should be a nonnegative integer"];Abort[]];
Which[OptionValue[RunInParallel]===False,SumCommand=Sum;Off[General::munfl];
 Off[General::stop],OptionValue[RunInParallel]===True,SumCommand=ParallelSum;ParallelEvaluate[Off[General::munfl]];
 ParallelEvaluate[Off[General::stop]]];
sumlim=Table[{Subscript[n, i],0,lim},{i,Dim}];
SetSharedVariable[nseries];
 Do[
With[{CondExtract=SeriesCondition[[i]],
SeriesExtract=SeriesSub[[i]],RequestedPrecision=OptionValue[NumericalPrecision]},
nseries=Insert[nseries,SumCommand[If[CondExtract,N[SeriesExtract,RequestedPrecision]//Quiet,0],Evaluate[Sequence@@sumlim]],-1]],{i,SeriesNumber}]];
Print["Numerical Result: ",N[Total@nseries,OptionValue[NumericalPrecision]]//Quiet];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
(*New*)
Return[N[Total@nseries,OptionValue[NumericalPrecision]]//Quiet]
]


End[];
EndPackage[];
Print["Last Updated: \!\(\*SuperscriptBox[\(22\), \(th\)]\) December, 2022"];
Print["Version 1.1 by S. Banik, S. Friot"];
Print["Version 1.0 by B.Ananthanarayan, S.Banik, S.Friot, S.Ghosh"];



