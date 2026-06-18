(* ::Package:: *)

(* ::Section::Closed:: *)
(*Package Initialization*)


(* ::Subsection::Closed:: *)
(*Load Dependencies*)


BeginPackage["MBConicHulls`"];
If[$KernelCount===0,LaunchKernels[$ProcessorCount*2]];


Quiet[Needs["Combinatorica`"];]
Block[{Print}, Get["MultivariateResidues`"];]
Off[SymbolName::sym];


(* ::Subsection::Closed:: *)
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
EvaluateSeries::"usage"="EvaluateSeries[ResolveMBOut_, MBParaSub_, SeriesNum_,PrintSeries->True,RearrangeSeries->False,RunInParallel->True] calculates and prints the series expression of the series representation SeriesNum. The function arguments are: \n\
ResolveMBOut - is the output of the ResolveMB[] or TriangulateMB[] function. \n\
MBParaSub - is a list of substitutions to be made to the parameters in the arguments of the gamma functions of the integrand. \n\
SeriesNum - is the number, as enumerated in the output of ResolveMB[] or TriangulateMB[], of the series for which we wish to calculate residues. \n\
PrintSeries - is an optional parameter, which specifies whether to print the derived analytic solution or not. Its default value is True. \n\
RearrangeSeries - is an optional parameter, which specifies whether to rearrange the summation indices such that each index runs from 0 to Infinity. Its default value is False. \n\
RunInParallel - is an optional parameter, which specifies whether the residue computation is performed using Mathematica's parallel processing functionality. Its default value is True.
";
SumAllSeries::"usage"="SumAllSeries[EvaluateSeriesOut_,MBVarSub_,SumLim_,RunInParallel-> True,NumericalPrecision -> MachinePrecision] numerically sums the series of the particular series representation evaluated by EvaluateSeries[] and prints the result. The function arguments are: \n\
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
RunInParallel::"usage"=" is an optional parameter of SumAllSeries[], EvaluateSeries[], ResolveMB[] and TriangulateMB[], indicating whether the computation has to be performed using Mathematica's parallel processing functionality or not. In SumAllSeries[] it parallelises the summation of the series; in EvaluateSeries[] it parallelises the residue computation over the singular-factor groups; in ResolveMB[] and TriangulateMB[] it parallelises the evaluation of the conic-hull membership conditions, determinant signs and poles over the gamma-function tuples. It is by default True.";
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
RearrangeSeries::usage = "is an option of EvaluateSeries[] which specifies whether to rearrange the summation indices such that each index runs from 0 to \[Infinity]. Its default value is False.";
AdaptiveLimits::"usage"=" is an optional parameter of SumAllSeries[], which specifies whether each summation index of a rearranged series is truncated early once further terms cannot affect the requested NumericalPrecision; the user-given summation limit is never exceeded. If False, every index of a d-fold series is instead summed up to the deeper limit Round[(lim+1)^(MaxDim/d)]-1. It is by default False.";


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


MBCHPositiveInequality[expr_,rr_]:=Module[{e=Together[expr],den,vals,nz,g},
den=Denominator[e];
If[TrueQ[den>0],
e=Numerator[e],
If[TrueQ[den<0],
e=-Numerator[e],
Return[expr>0]]];
e=Expand[e];
vals=Join[Coefficient[e,rr],{e/.Thread[rr->0]}];
If[VectorQ[vals,IntegerQ],
nz=DeleteCases[Abs[vals],0];
If[nz=!={},
g=Apply[GCD,nz];
If[g>1,e=Expand[e/g]]]];
e>0
]


MBCHPositiveInequalities[coords_,rr_]:=And@@(MBCHPositiveInequality[#,rr]&/@coords)


FindAllGoodComb[MBRepVars_,ParallelQ_:True]:=Module[{Cof,Num=MBRepVars[[4,1]],
AllComb,IntVar=MBRepVars[[2]],RR=x/@Range[MBRepDim[MBRepVars]],RetTup,SGN={},
Comb={},ConHull={},B={},R={},Poles={},j,NumDenIntCof,Degenerate,Msg1,Delta,NumCof,DenCof,ProportionalQ,AnyProportionalQ,
NumGamma,DenGamma,NumFlat,DenFlat,MBRepVarsFlatten,MBRepVarsGamma,NumGammaCof,DenGammaCof,NumDenIntGammaCof,
CofR,Apex,NumConst,NVec,PerComb,AllRes,GoodPos},

NumGamma=MBRepVars[[4,1]]//Select[#,Head[#]=!=List&]&;
DenGamma=MBRepVars[[4,2]]//Select[#,Head[#]=!=List&]&;
NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
MBRepVarsFlatten= ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];
MBRepVarsGamma= ReplacePart[MBRepVars,{{4,1}->NumGamma,{4,2}->DenGamma}];
Cof=MakeCof[MBRepVarsFlatten];
AllComb=FindAllComb[MBRepVars];
CofR=Rest[Cof];Apex=Cof[[1]];
NumConst=NumFlat/.Thread[IntVar->ConstantArray[0,Length[IntVar]]];
NVec=Subscript[n,#]&/@Range[MBRepDim[MBRepVars]];

PerComb=With[{CofRW=CofR,ApexW=Apex,RRW=RR,NumConstW=NumConst,NVecW=NVec},
Function[i,Module[{sgn,gens,ginv,b,p},
gens=Extract[CofRW,#]&/@i;
sgn=Sign[Det[gens]];
If[sgn==0,{0,Null,Null},
ginv=Inverse[gens];
b=MBCHPositiveInequalities[(RRW-ApexW) . ginv,RRW];
p=Expand[ginv . (-NVecW[[Range[Length[i]]]]-NumConstW[[i]])];
{sgn,b,p}]]]];

AllRes=If[TrueQ[ParallelQ]&&$KernelCount>0,
ParallelMap[PerComb,AllComb,Method->"ItemsPerEvaluation"->Max[1,Ceiling[Length[AllComb]/(8 $KernelCount)]]],
PerComb/@AllComb];

(*The original loop inserted the zero determinant signs at the running position RetTup, which
packs all signs of the retained tuples at the front of SGN and the zeros behind them.*)
GoodPos=Flatten@Position[AllRes[[All,1]],1|-1,{1},Heads->False];
SGN=Join[AllRes[[GoodPos,1]],ConstantArray[0,Length[AllComb]-Length[GoodPos]]];
Comb=AllComb[[GoodPos]];
B=AllRes[[GoodPos,2]];
Poles=AllRes[[GoodPos,3]];
R=Table[ImplicitRegion[B[[j]],Evaluate[RR]],{j,Length[B]}];
RetTup=Length[GoodPos]+1;

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


FIIneqMatrix[cond_,RRv_]:=Module[{lst,rows,ca,mat},
lst=If[Head[cond]===And,List@@cond,{cond}];
If[AnyTrue[lst,Length[#]!=2&],Throw[$Failed,FIParseTag]];
rows=Switch[Head[#],Greater,#[[1]]-#[[2]],Less,#[[2]]-#[[1]],_,Throw[$Failed,FIParseTag]]&/@lst;
ca=Quiet@CoefficientArrays[rows,RRv];
If[Head[ca]=!=List||Length[ca]!=2||Normal[ca[[1]]]=!=ConstantArray[0,Length[rows]],Throw[$Failed,FIParseTag]];
mat=Normal[ca[[2]]];
If[!MatrixQ[mat,NumericQ],Throw[$Failed,FIParseTag]];
mat];

FIInteriorPoint[mat_]:=If[Length[mat]===Length[First[mat]]&&Det[mat]=!=0,
LinearSolve[mat,ConstantArray[1,Length[mat]]],
FIFeasible[mat]];

FIAllPosQ[v_]:=AllTrue[v,Positive];

FIFeasible[mat_]:=Module[{nv=Length[First[mat]],m=Length[mat],matN,xfl,xr,yfl,S,ns,cand,zsol},
matN=N[mat];

xfl=Quiet@LinearProgramming[ConstantArray[0.,nv],matN,ConstantArray[1.,m],-Infinity,Method->"Simplex"];
If[VectorQ[xfl,NumberQ],
xr=Rationalize[xfl,0];
If[FIAllPosQ[mat . xr],Return[xr]]];

yfl=Quiet@LinearProgramming[ConstantArray[0.,m],Join[Transpose[matN],{ConstantArray[1.,m]}],
Append[Table[{0.,0},{nv}],{1.,0}],0,Method->"Simplex"];
If[VectorQ[yfl,NumberQ],
S=Flatten@Position[yfl,_?(#>10.^-9&),{1},Heads->False];
If[0<Length[S]<=nv+3,
ns=NullSpace[Transpose[mat[[S]]]];
cand=SelectFirst[Join[ns,-ns],FIAllNonNegQ,None];
If[cand=!=None&&Total[cand]>0,Return[False]]]];
(*exact simplex (matrix form, variables (x,t), maximise t<=1 subject to mat.x>=t) decides the rest*)
zsol=Quiet@LinearProgramming[Append[ConstantArray[0,nv],-1],
Append[Join[mat,-ConstantArray[{1},m],2],Append[ConstantArray[0,nv],-1]],
Append[ConstantArray[0,m],-1],-Infinity,Method->"Simplex"];
If[!VectorQ[zsol,NumericQ],Throw[$Failed,FIParseTag]];
If[zsol[[-1]]>1/2,Most[zsol],False]];

FIAllNonNegQ[v_]:=AllTrue[v,NonNegative];


FindIntersection[MBRepVarsF_,RequestedSeries_,MasterSeriesQ_,ParallelQ_:True]:=Module[{MBRepVars=MBRepVarsF[[{1,2,3,4}]],
IntVar=MBRepVarsF[[2]],SeriesRepCount=0,
Counter=0,Branch=1,Dim=MBRepDim[MBRepVarsF],F={},Reg,Labels,NextLabel,IntersectingLabel,
LargestSubsetQ,CrossProd,AllPosSub=Subsets[Range[MBRepDim[MBRepVarsF]],{MBRepDim[MBRepVarsF]-1}], MasterCH ={},
MasterCHNorVec ={},MasterCHBasVec ={},MasterPoles ={},MasterCharList={},MasterSeriesVar ={},Msg2={},j,RepeatedTermLabel,
temp,DupTuplesList,UniqueTuples,CombLabels,RetTup,Comb,Poles,R,B,NumDenIntGammaCof,Degenerate,RR,Msg1,SGN,LMasterCHNorVec,
NumFlat,DenFlat,MBRepVarsFlatten,
FIfastQ,ConeMat,ConeInt,PairDis,PairIn,PairOK,StackW,FIPairInQ,FIPairDisQ,FIStackWitness,FIDisjointQ,CofLegacy,LegacyConeCondition},

NumFlat=MBRepVars[[4,1]]//Flatten//Select[#,Head[#]=!=Integer&]&;
DenFlat=MBRepVars[[4,2]]//Flatten//Select[#,Head[#]=!=Integer&]&;
MBRepVarsFlatten= ReplacePart[MBRepVars,{{4,1}->NumFlat,{4,2}->DenFlat}];

{RetTup,Comb,Poles,R,B,NumDenIntGammaCof,Degenerate,RR,Msg1,SGN}=FindAllGoodComb[MBRepVars,ParallelQ];
Print@Msg1;
MBRepVars=MBRepVarsFlatten;

Poles=Expand@Poles;

B=Sort/@B;
DupTuplesList=positionDuplicates@B;
UniqueTuples=First/@DupTuplesList;
RetTup=Length@UniqueTuples;
R=R[[UniqueTuples]];
B=B[[UniqueTuples]];
CofLegacy=MakeCof[MBRepVars];
LegacyConeCondition[lbl_]:=Refine[RegionMember[ConicHullRegion[{CofLegacy[[1]]},Extract[Rest[CofLegacy],#]&/@Comb[[UniqueTuples[[lbl]]]]],RR],RR\[Element]Reals]/.{GreaterEqual->Greater,LessEqual->Less};

FIfastQ=Catch[
ConeMat=Table[FIIneqMatrix[B[[i]],RR],{i,RetTup}];
ConeInt=Table[FIInteriorPoint[ConeMat[[i]]],{i,RetTup}];
If[!VectorQ[ConeInt[[#]],NumericQ]||!FIAllPosQ[ConeMat[[#]] . ConeInt[[#]]],Throw[$Failed,FIParseTag]]&/@Range[RetTup];
True,FIParseTag,$Failed&];
PairDis=Association[];PairIn=Association[];PairOK=Association[];StackW=Association[];
FIPairInQ[l_,i_]:=With[{key={l,i}},If[KeyExistsQ[PairIn,key],PairIn[key],PairIn[key]=FIAllPosQ[ConeMat[[l]] . ConeInt[[i]]]]];
FIPairDisQ[l_,i_]:=Module[{key=Sort[{l,i}],res},
If[KeyExistsQ[PairDis,key],Return[True]];
If[KeyExistsQ[PairOK,key],Return[False]];
res=Catch[FIFeasible[Join[ConeMat[[key[[1]]]],ConeMat[[key[[2]]]]]],FIParseTag,$Failed&];
If[res===False,PairDis[key]=True;True,PairOK[key]=res;False]];
FIStackWitness[lbls_]:=If[Length[lbls]==1,ConeInt[[lbls[[1]]]],Lookup[StackW,Key[lbls],None]];
FIDisjointQ[lbls_,i_,RegC_]:=Module[{w,res,pk},
If[AnyTrue[lbls,TrueQ[PairDis[Sort[{#,i}]]]&],Return[True]];
If[AllTrue[lbls,FIPairInQ[#,i]&],StackW[Append[lbls,i]]=ConeInt[[i]];Return[False]];
w=FIStackWitness[lbls];
If[w=!=None&&FIAllPosQ[ConeMat[[i]] . w],StackW[Append[lbls,i]]=w;Return[False]];
If[Length[lbls]>=3&&AnyTrue[lbls,FIPairDisQ[#,i]&],Return[True]];
res=Catch[FIFeasible[Join@@ConeMat[[Append[lbls,i]]]],FIParseTag,$Failed&];
If[res===$Failed,Return[RegionDisjoint[If[RegC===None,If[Length[lbls]===1,R[[First[lbls]]],RegionIntersection@@R[[lbls]]],RegC],R[[i]]]]];
If[res===False,
If[Length[lbls]==1,PairDis[Sort[{lbls[[1]],i}]]=True];
True,
StackW[Append[lbls,i]]=res;False]];

While[True,
If[Branch==1,
If[++Counter<= RetTup,Reg=If[FIfastQ===True,None,R[[Counter]]];Labels={Counter};Branch=2;NextLabel=Counter+1;Continue[],Break[]]];

If[SeriesRepCount>= RequestedSeries,Break[]];

IntersectingLabel=If[FIfastQ===True,
Catch[Do[If[FIDisjointQ[Labels,i,Reg],Continue[],Throw[i]],{i,NextLabel,RetTup}];Throw[0]],
Catch[Do[If[RegionDisjoint[Reg,R[[i]]],Continue[],Throw[i]],{i,NextLabel,RetTup}];Throw[0]]];
If[IntersectingLabel==0,
LargestSubsetQ=Catch[Do[If[SubsetQ[F[[i]],UniqueTuples[[Labels]]],Throw[False]],{i,SeriesRepCount}];Throw[True]];

If[LargestSubsetQ,
CombLabels=Sort[Flatten[DupTuplesList[[Labels]]]];
++SeriesRepCount;

If[MasterSeriesQ,

Which[Degenerate&&(Dim>=2),

MasterCH=Insert[MasterCH,If[Dim<=4,FullSimplify[And@@(LegacyConeCondition/@Labels)],If[FIfastQ===True,MBCHFastIntersection[B[[Labels]],RR,Join@@ConeMat[[Labels]]],MBCHFastIntersection[And@@B[[Labels]],RR]]],-1];

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
Reg=If[FIfastQ===True,None,RegionIntersection@@R[[Labels]]],

Labels=Insert[Labels,IntersectingLabel,-1];Branch++;
Reg=If[FIfastQ===True,None,RegionIntersection[Reg,R[[IntersectingLabel]]]];NextLabel=IntersectingLabel+1]];

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


FindTriangulations[Amat_,TopcomPath_,TopComOptions_,TriangulationsNum_,CardMsg_,TPCommand_]:=
Module[{dir,tag,fInRaw,fIn,fOut,fSh,cleanup,attempt,parse,validTriQ,validQ,sameLenQ,
        npts,Count,List4,try,maxTry=6},

dir=NotebookDirectory[];
SetDirectory[dir];

(* unique, shell-safe tag: OS pid (distinct per concurrent process) + per-call Unique symbol *)
tag=ToString[$ProcessID]<>"_"<>SymbolName[Unique["q"]];
fInRaw=dir<>"InputTOPCOMRaw_"<>tag<>".dat";
fIn   =dir<>"InputTOPCOM_"<>tag<>".dat";
fOut  =dir<>"Triangulations_"<>tag<>".txt";
fSh   =dir<>"RunTopcom_"<>tag<>".sh";

cleanup:=Quiet[DeleteFile/@Select[{fInRaw,fIn,fOut,fSh},FileExistsQ]];

(* a valid TOPCOM triangulation: nonempty, all simplices equal-size integer index sets in range *)
npts=Length[Amat];
sameLenQ[T_]:=Length[DeleteDuplicates[Length/@T]]===1;
validTriQ[T_]:=ListQ[T]&&T=!={}&&AllTrue[T,(VectorQ[#,IntegerQ]&&#=!={}&&Min[#]>=1&&Max[#]<=npts)&]&&sameLenQ[T];
validQ[L_]:=ListQ[L]&&L=!={}&&AllTrue[L,validTriQ];

(* parse raw TOPCOM text -> 1-indexed list of triangulations, or $Failed *)
parse[raw_]:=Quiet@Check[
  If[TPCommand=="points2alltriangs",
    Module[{L1,LS,L3},
      L1=Map[First,StringPosition[raw,"="]]+2;
      LS=Map[First,StringPosition[raw,";"]]-1;
      L3=Table[{L1[[i]],LS[[i]]},{i,Length@L1}];
      ToExpression[Table[StringTake[raw,L3[[i]]],{i,Length@L3}]]+1],
    {ToExpression@raw+1}],
  $Failed];

(* one full TOPCOM attempt -> parsed List4 or $Failed *)
attempt:=Module[{raw},
  Export[fInRaw,ToString[Amat,FormatType->InputForm]];
  Quiet[DeleteFile/@Select[{fIn,fOut},FileExistsQ]];
  Block[{Print},RunProcess[$SystemShell,"StandardOutput",
    "tr '{}' '[]' < \""<>fInRaw<>"\" > \""<>fIn<>"\""]];
  If[!FileExistsQ[fIn]||FileByteCount[fIn]===0,Return[$Failed]];
  If[Head[TriangulationsNum]===Integer,
    Count=ToString@TriangulationsNum;
    Which[
      StringTake[$OperatingSystem,3]==="Mac",
        Block[{Print},RunProcess[$SystemShell,"StandardOutput",
          TopcomPath<>TPCommand<>TopComOptions<>" < \""<>fIn<>"\" | head -n "<>Count<>" > \""<>fOut<>"\""]],
      StringTake[$OperatingSystem,3]==="Uni",
        Export[fSh,TopcomPath<>TPCommand<>TopComOptions<>" < \""<>fIn<>"\" | head -n "<>Count<>" > \""<>fOut<>"\"",OverwriteTarget->True];
        Run["sh \""<>fSh<>"\""]
    ],
    Block[{Print},RunProcess[$SystemShell,"StandardOutput",
      TopcomPath<>TPCommand<>TopComOptions<>" < \""<>fIn<>"\" | tee \""<>fOut<>"\""]]
  ];
  If[!FileExistsQ[fOut]||FileByteCount[fOut]===0,Return[$Failed]];
  parse[Import[fOut,"Text"]]];

CheckAbort[
  PrintTemporary["Extracting Results"];
  List4=$Failed; try=0;
  While[(!validQ[List4])&&try<maxTry, try++; List4=attempt];
  If[!validQ[List4],
    cleanup;
    Print["No valid triangulation"<>CardMsg<>" returned by TOPCOM after "<>ToString[maxTry]<>
      " attempts. Check that TopComPath is correct and TOPCOM runs."];
    Abort[]];
  PrintTemporary["Done!"];
  cleanup;
  List4
,
  cleanup; Abort[]]
];


(* ::Subsection::Closed:: *)
(*Analytic Series using MultivariateResidues*)


MBDecide[Expr_, Vars_] := Module[{Lin, IneqQ, DivAllQ, Atom, AndDec, Decide},
  Lin[G_] := Module[{C0, Cs}, C0 = G /. Thread[Vars -> 0]; Cs = Coefficient[G, #] & /@ Vars;
    If[VectorQ[Cs, NumericQ] && TrueQ[Expand[G - (C0 + Cs . Vars)] == 0], {C0, Cs}, $Failed]];
  IneqQ[G_, Op_] := Module[{Lc = Lin[G], C0, Cs}, If[Lc === $Failed || ! NumericQ[Lc[[1]]], Return[$Unknown]]; {C0, Cs} = Lc;
    Switch[Op, LessEqual, TrueQ[C0 <= 0] || AnyTrue[Cs, # < 0 &], Less, TrueQ[C0 < 0] || AnyTrue[Cs, # < 0 &],
     GreaterEqual, TrueQ[C0 >= 0] || AnyTrue[Cs, # > 0 &], Greater, TrueQ[C0 > 0] || AnyTrue[Cs, # > 0 &], _, $Unknown]];
  DivAllQ[L_, D_] := Module[{Lc = Lin[L], C0, Cs}, If[Lc === $Failed || ! IntegerQ[D], Return[$Unknown]]; {C0, Cs} = Lc;
    If[IntegerQ[C0] && VectorQ[Cs, IntegerQ], TrueQ[Mod[C0, D] == 0 && AllTrue[Cs, Mod[#, D] == 0 &]], $Unknown]];
  Atom[True] := True; Atom[False] := False;
  Atom[(H : (LessEqual | Less | GreaterEqual | Greater))[L_, 0]] := IneqQ[L, H];
  Atom[Equal[Mod[L_, D_], 0]] := Module[{Lc = Lin[L], C0}, If[Lc === $Failed, Return[$Unknown]]; C0 = Lc[[1]];
    If[! NumericQ[C0] || ! IntegerQ[C0], False, $Unknown]];
  Atom[Unequal[Mod[L_, D_], 0]] := Module[{Lc = Lin[L], C0, Da}, If[Lc === $Failed, Return[$Unknown]]; C0 = Lc[[1]];
    If[! NumericQ[C0] || ! IntegerQ[C0], True, Da = DivAllQ[L, D]; If[Da === True, False, If[Da === False, True, $Unknown]]]];
  Atom[_] := $Unknown;
  AndDec[Args_] := Module[{InEqs, ModEqs, L, Lc, C0, Le},
    InEqs = Cases[Args, (LessEqual | Less | GreaterEqual | Greater)[_, 0]];
    ModEqs = Cases[Args, Equal[Mod[_, _], 0]];
    Which[
     ModEqs === {} && Length[InEqs] === Length[Args],
       If[AllTrue[InEqs, TrueQ[(# /. Thread[Vars -> 0])] &], True, $Unknown],
     Length[InEqs] === 1 && Length[Args] === Length[InEqs] + Length[ModEqs] &&
       MatchQ[InEqs[[1]], LessEqual[_, 0]] && AllTrue[ModEqs, (#[[1, 1]] === InEqs[[1, 1]]) &],
       L = InEqs[[1, 1]]; Lc = Lin[L]; If[Lc === $Failed, Return[$Unknown]]; C0 = Lc[[1]];
       If[! NumericQ[C0] || ! IntegerQ[C0], Return[False]];
       Le = IneqQ[L, LessEqual];
       Which[Le === False, False, Le === True && AllTrue[ModEqs, DivAllQ[#[[1, 1]], #[[1, 2]]] === True &], True, True, $Unknown],
     True, $Unknown]];
  Decide[True] := True; Decide[False] := False;
  Decide[HoldPattern[Or[A__]]] := Module[{Rs = Atom /@ {A}}, If[AnyTrue[Rs, # === True &], True, If[AllTrue[Rs, # === False &], False, $Unknown]]];
  Decide[HoldPattern[And[A__]]] := AndDec[{A}];
  Decide[X : (LessEqual | Less | GreaterEqual | Greater | Equal | Unequal)[_, _]] := Atom[X];
  Decide[_] := $Unknown;
  Decide[Expr]];


SingOrder[x_,DumVar_]:=Module[{DumVarOnly,DumVarPow,DumVarPowList,Var,DumVarPresent},
DumVarOnly=(Most[#]&/@DumVar)//Flatten;
DumVarPow=Table[If[Length[DumVar[[i]]]==2,{DumVar[[i,1]]->1},{DumVar[[i,1]]->1,DumVar[[i,2]]->DumVar[[i,3]]}],{i,Length@DumVar}];
DumVarPowList=Join@@DumVarPow;
Var=Variables[x];
DumVarPresent=Select[Var,MemberQ[DumVarOnly,#]&];
DumVarPow=Total[DumVarPresent/.DumVarPowList]+1;
Return[DumVarPow]
]


FindSingFactors[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_]:=Module[{TempCond,PositiveDefinite,
NumberNum=NumLength[MBRepVars],NumDen=MergeNumDen[MBRepVars],Dim=MBRepDim[MBRepVars],DivConCond={},SingFacLabels={},AssociatePoles={},IntVar=MBRepVars[[2]],
ConGamma,NumDenSub,NumDenSubD,NumDenSubN,DivGamma={},ConDivGamma={},SumVar,DivCond,ConCond,SingComb,NonSingComb,PoleSubstitute,existsIntQ},
SumVar=Subscript[n, #]&/@Range[Dim];
PositiveDefinite=And@@((#>=0)&/@SumVar);
ConGamma=Complement[Range[NumberNum],Union@@(Extract[Comb,#]&/@FSeriesSol)];
existsIntQ[expr_] := Module[{b,dec,fi,val,time=5},
 b = expr /. HoldPattern[Subscript[n,_] >= 0] -> True;
 dec = MBDecide[b, SumVar];
 If[dec === True, Return[True]];
 If[dec === False, Return[False]];
 fi = TimeConstrained[Check[FindInstance[Evaluate[SumVar] \[Element] Integers && Evaluate[expr],SumVar, Integers],False]//Quiet,time,False];
 If[fi =!= {}, val=True, val=Resolve[Exists[Evaluate[SumVar], Evaluate[SumVar] \[Element] Integers, Evaluate[expr]]]];
 Return[val]];
Do[
PoleSubstitute=IntVar[[#]]-> Poles[[i]][[#]]&/@Range[Dim];
NumDenSub=Together[NumDen/.PoleSubstitute//Simplify];
NumDenSubD=Denominator@NumDenSub;NumDenSubN=Numerator@NumDenSub;
DivGamma={};ConDivGamma={};
Do[If[existsIntQ[NumDenSubN[[j]] <= 0 &&Mod[NumDenSubN[[j]], NumDenSubD[[j]]] == 0 &&PositiveDefinite],
If[existsIntQ[(NumDenSubN[[j]] > 0 &&PositiveDefinite)||(Mod[NumDenSubN[[j]], NumDenSubD[[j]]] != 0&&PositiveDefinite) ],
ConDivGamma=Insert[ConDivGamma,j,-1],DivGamma=Insert[DivGamma,j,-1]]],{j,Complement[Range[Length[NumDen]],Comb[[i]],ConGamma]}];
Do[DivCond=True;ConCond=True;SingComb=Union[DivGamma,o];If[MemberQ[SingFacLabels,Sort[Union[Comb[[i]],SingComb]]],Continue[]];NonSingComb=Complement[ConDivGamma,o];
DivCond = And@@(Table[NumDenSubN[[k]] <= 0 &&Mod[NumDenSubN[[k]], NumDenSubD[[k]]] == 0,{k, SingComb}]);
ConCond = And@@(Table[(NumDenSubN[[l]] > 0||Mod[NumDenSubN[[l]], NumDenSubD[[l]]] != 0),{l, NonSingComb}]);
TempCond = FullSimplify[DivCond && ConCond && PositiveDefinite,Evaluate[SumVar] \[Element] Integers];
If[existsIntQ[TempCond],DivConCond=Insert[DivConCond,TempCond,-1];SingFacLabels=Insert[SingFacLabels,Sort[Union[Comb[[i]],SingComb]],-1];AssociatePoles=Insert[AssociatePoles,Poles[[i]],-1],Nothing,Print["Error 3 ::",DivCond&&ConCond]]
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



CRDynamicParallelMap[workerIn_,nItems_,est_]:=Module[{maxC=Min[$ProcessorCount,$KernelCount],queue,act=Association[],res=ConstantArray[Null,nItems],r,id,rest},
queue=Reverse@Ordering[est];
While[queue=!={}&&Length[act]<maxC,
With[{kw=workerIn,kk=First[queue]},id=ParallelSubmit[kw[kk]]];
act[id]=First[queue];queue=Rest[queue]];
While[Length[act]>0,
{r,id,rest}=WaitNext[Keys[act]];
res[[act[id]]]=r;act=KeyDrop[act,{id}];
If[queue=!={},
With[{kw=workerIn,kk=First[queue]},id=ParallelSubmit[kw[kk]]];
act[id]=First[queue];queue=Rest[queue]]];
res]


CalculateResidue[MBRepVars_,FSeriesSol_,Comb_,Poles_,SGN_,DumVar_,ParallelQ_:False]:=Module[{IntVar=MBRepVars[[2]],
Dim=MBRepDim[MBRepVars],Cof,NumDenFlat,AssociatePoles,SingFacLabels,DivConCond,GroupFacLabels,SeriesNumber=0,NumberNum=NumLength[MBRepVars],
  NumDenIntPart,series={},SeriesCondition,NumGamma,DenGamma,NumFlat,DenFlat,MBRepVarsFlatten,NumPolyGamma,
  DumVarOnly=(Most[#]&/@DumVar)//Flatten,PolesDumZero,DumCof,NumDenDumPart,NumDenIntDumPart,DumVarZero,DumVarSequence,
  PolyGammaLabel,PolyGammaOrder,kWorker,resultPairs,IntegrandBase,kOrder,residueWeights,targetKernels,useParallelResidues},
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
{SingFacLabels,AssociatePoles,DivConCond,GroupFacLabels}=GroupSingFactors[MBRepVarsFlatten,FSeriesSol,Comb,PolesDumZero,SGN];
PrintTemporary["Computing residues"];
NumDenIntPart=Total[Cof[[#]]*IntVar]&/@Range[Length@NumDenFlat];
NumDenDumPart=Total[DumCof[[#]]*DumVarOnly]&/@Range[Length@NumDenFlat];
NumDenIntDumPart=NumDenIntPart+NumDenDumPart;
IntegrandBase=MBRepVars[[1]]*((Times@@Gamma/@NumGamma)(Times@@(PolyGamma@@#&/@NumPolyGamma)) Times@@(MBRepVars[[3]]^MBRepVars[[2]]))/Times@@(Gamma/@DenGamma);
IntegrandBase=IntegrandBase//ExpandAll;
With[{pMBRep41=MBRepVars[[4,1]],IntegrandW=IntegrandBase,IntVarW=IntVar,DimW=Dim,NumberNumW=NumberNum,PolyGammaLabelW=PolyGammaLabel,
 DumVarOnlyW=DumVarOnly,DumVarSequenceW=DumVarSequence,DumVarZeroW=DumVarZero,DumVarW=DumVar,
 NumDenFlatW=NumDenFlat,NumDenIntPartW=NumDenIntPart,NumDenIntDumPartW=NumDenIntDumPart,
 SGNW=SGN,SingFacLabelsW=SingFacLabels,AssociatePolesW=AssociatePoles,DivConCondW=DivConCond,GroupFacLabelsW=GroupFacLabels},
kWorker=Function[kk,Module[{GenReflection,GenReflectionPG,NumDenPoleSubL,NumDenPoleShiftL,GenRefSub,IntegrandSub,
 NumeratorFac,DenSingFacGammaAll,DenSingFacDumGammaAll,DenSingPow,DumVarD,NumeratorFac1,NumeratorFac2,NumeratorFac3,
 DenSingFac,TransformedVar,TransformMatrix,IntVarSwap,GbBasis,OverallFac,NumFacReduce,NumFacMonoList,RefinedFac,SeriesCof,
 SignFacsLabelsNum,SignSGN,ContourSign,seriestemp,out={},Mmat,detM},
 GenReflection[z_,n_]:=((-1)^(-n) Gamma[1-z]Gamma[1+z])/Gamma[1-n-z];
 GenReflectionPG[m_,z_,n_]:=z^(m+1) PolyGamma[m,1+z]+(-1)^(m+1) z^(m+1) PolyGamma[m,1-z]+(-1)^(m+1) Gamma[1+m]+(-1)^m z^(m+1) PolyGamma[m,1-n-z];
 NumDenPoleSubL=NumDenFlatW/.(IntVarW[[#]]->AssociatePolesW[[kk]][[#]]&/@Range@DimW)/.DumVarZeroW//Simplify;
 NumDenPoleShiftL=NumDenFlatW/.(IntVarW[[#]]->IntVarW[[#]]+AssociatePolesW[[kk]][[#]]&/@Range@DimW)//Simplify;
 GenRefSub=Table[If[!MemberQ[PolyGammaLabelW,j],Gamma@NumDenPoleShiftL[[j]]->GenReflection[NumDenIntDumPartW[[j]],NumDenPoleSubL[[j]]],PolyGamma[First[pMBRep41[[j]]],NumDenPoleShiftL[[j]]]->1],{j,SingFacLabelsW[[kk]]}];
 GenRefSub=GenRefSub//ExpandAll;
 IntegrandSub=ExpandAll@(Simplify[IntegrandW/.(IntVarW[[#]]->IntVarW[[#]]+AssociatePolesW[[kk]][[#]]&/@Range@DimW)])/.GenRefSub;
 NumeratorFac=Product[If[l>NumberNumW,NumDenIntDumPartW[[l]],1],{l,SingFacLabelsW[[kk]]}];
 NumeratorFac=NumeratorFac*Product[If[MemberQ[PolyGammaLabelW,l],GenReflectionPG[First[pMBRep41[[l]]],NumDenIntPartW[[l]],NumDenPoleSubL[[l]]],1],{l,SingFacLabelsW[[kk]]}];
 DenSingFacGammaAll=Table[If[l<=NumberNumW,NumDenIntDumPartW[[l]],Nothing],{l,SingFacLabelsW[[kk]]}];
 DenSingFacDumGammaAll=Select[DenSingFacGammaAll,ContainsAny[DumVarOnlyW,Variables[#]]&];
 DenSingPow=ConstantArray[-1,Length@DenSingFacDumGammaAll];
 Do[DumVarD=DumVarSequenceW[[s]];
  NumeratorFac1=D[NumeratorFac,DumVarD]*Product[Power[i,Boole[MemberQ[Variables[i],DumVarD]]],{i,DenSingFacDumGammaAll}];
  NumeratorFac2=NumeratorFac*Expand[D[IntegrandSub,DumVarD]/IntegrandSub]*Product[Power[i,Boole[MemberQ[Variables[i],DumVarD]]],{i,DenSingFacDumGammaAll}];
  NumeratorFac3=NumeratorFac*Sum[DenSingPow[[j]]*D[DenSingFacDumGammaAll[[j]],DumVarD]*Product[Power[i,Boole[MemberQ[Variables[i],DumVarD]]],{i,Delete[DenSingFacDumGammaAll,j]}],{j,Length@DenSingFacDumGammaAll}];
  NumeratorFac=NumeratorFac1+NumeratorFac2+NumeratorFac3;
  DenSingPow=Table[DenSingPow[[i]]-Boole[MemberQ[Variables[DenSingFacDumGammaAll[[i]]],DumVarD]],{i,Length@DenSingFacDumGammaAll}];
 ,{s,Length@DumVarSequenceW}];
 IntegrandSub=Limit[IntegrandSub,DumVarZeroW];
 NumeratorFac=Limit[NumeratorFac,DumVarZeroW]//Expand;
 Do[
  DenSingFac=Table[Product[If[!MemberQ[PolyGammaLabelW,a],Power[NumDenIntPartW[[a]],SingOrder[NumDenIntDumPartW[[a]],DumVarW]],Power[NumDenIntPartW[[a]],First[pMBRep41[[a]]]+1]],{a,b[[c]]}],{c,DimW}];
  If[DimW===1,SignFacsLabelsNum=Select[Flatten[SingFacLabelsW],MemberQ[Range@NumberNumW,#]&];SignSGN=SGNW[[SignFacsLabelsNum]];
   Which[ContainsOnly[SignSGN,{-1}],ContourSign=-1,ContainsOnly[SignSGN,{1}],ContourSign=1,True,Print["Some Issues!"]],ContourSign=1];
  Mmat=Table[Coefficient[DenSingFac[[c]],IntVarW[[jc]]],{c,DimW},{jc,DimW}];
  If[MatrixQ[Mmat,NumericQ]&&TrueQ[Expand[DenSingFac-Mmat . IntVarW]===ConstantArray[0,DimW]]&&Intersection[Flatten[b],PolyGammaLabelW]==={}&&(detM=Det[Mmat];detM=!=0),
   (* ---- SIMPLE first-order pole: direct residue (no Groebner / no MultivariateResidue / no Simplify) ---- *)
   seriestemp=(ContourSign*NumeratorFac*IntegrandSub/.Thread[IntVarW->0])/detM,
   (* ---- fallback: original Groebner + MultivariateResidue (UNCHANGED) ---- *)
   TransformedVar={};TransformMatrix={};
   Do[IntVarSwap=ReplacePart[IntVarW,{i->IntVarW[[DimW]],DimW->IntVarW[[i]]}];
    GbBasis=ResourceFunction["ExtendedGroebnerBasis"][DenSingFac,IntVarSwap];
    TransformedVar=Insert[TransformedVar,GbBasis[[1]][[1]],-1];
    TransformMatrix=Insert[TransformMatrix,GbBasis[[2]][[1]],-1],{i,DimW}];
   OverallFac=FactorTermsList[Times@@TransformedVar][[1]];
   TransformedVar=Numerator[TransformedVar/OverallFac];
   NumFacReduce=PolynomialReduce[Expand[Det[TransformMatrix]*NumeratorFac],Evaluate@TransformedVar,Evaluate@IntVarW][[2]];
   NumFacMonoList=If[NumFacReduce=!=0,MonomialList[NumFacReduce],{}];
   RefinedFac={};SeriesCof={};
   Do[RefinedFac=Insert[RefinedFac,Table[Denominator[FactorTermsList[NumFacMonoList[[i]]][[2]]/TransformedVar[[j]]],{j,DimW}],-1];
    SeriesCof=Insert[SeriesCof,Numerator[NumFacMonoList[[i]]/Times@@TransformedVar],-1],{i,Length@NumFacMonoList}];
   seriestemp=(OverallFac)^(-1) Sum[MultivariateResidues`MultivariateResidue[SeriesCof[[j]]*ContourSign*IntegrandSub,RefinedFac[[j]],Table[IntVarW[[i]]->0,{i,DimW}]],{j,Length@RefinedFac}]//Simplify
  ];
  If[seriestemp=!=0,AppendTo[out,{seriestemp,DivConCondW[[kk]]}]];
 ,{b,GroupFacLabelsW[[kk]]}];
 out]];
];
residueWeights=Max[Length[GroupFacLabels[[#]]],1]&/@Range[Length@DivConCond];
targetKernels=Min[If[$KernelCount>0,$KernelCount,$ProcessorCount*2],Max[Length@DivConCond,1]];
useParallelResidues=TrueQ[ParallelQ]&&targetKernels>=2&&Total[residueWeights]>=2 targetKernels;
If[useParallelResidues&&$KernelCount===0,LaunchKernels[targetKernels]];
useParallelResidues=useParallelResidues&&$KernelCount>0;
resultPairs=If[useParallelResidues,
  DistributeDefinitions@@(ToExpression/@Join[Names["MultivariateResidues`*"],Names["MultivariateResidues`Private`*"]]);
  DistributeDefinitions[SingOrder];
  ParallelEvaluate[ResourceFunction["ExtendedGroebnerBasis"]];
  Join@@CRDynamicParallelMap[kWorker,Length@DivConCond,residueWeights]
  ,
  Join@@Map[kWorker,Range[Length@DivConCond]]];
series=resultPairs[[All,1]];SeriesCondition=resultPairs[[All,2]];SeriesNumber=Length[resultPairs];
PrintTemporary["Done!"];
Return[{SeriesNumber,series,Dim,SeriesCondition}]]


(* ::Subsection::Closed:: *)
(*Rearrange Series*)


Options[TrivializeSeries] = {RunInParallel -> True};
TrivializeSeries::xi = "Could not find an interior generic xi.";
TrivializeSeries::nonunit = "Equality with no unit-coefficient variable (not handled).";

TrivializeSeries[RawSeries_, SeriesCond_, SumVars_, OptionsPattern[]] := Module[
  {TSToGE, TSPrimitive, TSPolyAb, TSVertices, TSRays, TSTriangulate, TSInteriorXi,
   TSFundPoints, TSReduceConstraints, TSDecompose, TSDisjointBool, TSProcessEqualities,
   TSCollectPowers, TSOne, Pairs, Res, ParRes},

  TSToGE[GreaterEqual[A_, B_]] := A - B;  TSToGE[LessEqual[A_, B_]] := B - A;
  TSToGE[Greater[A_, B_]] := A - B - 1;   TSToGE[Less[A_, B_]] := B - A - 1;

  TSPrimitive[G_] := Module[{Cc},
    If[AllTrue[G, # == 0 &], G, Cc = G*(LCM @@ (Denominator /@ G)); Cc/(GCD @@ Cc)]];

  TSPolyAb[GeList_, Vars_] := {
    Table[Coefficient[G, Vars[[J]]], {G, GeList}, {J, Length[Vars]}],
    Table[-(G /. Thread[Vars -> 0]), {G, GeList}]};

  TSVertices[AMat_, BVec_, Vars_] := Module[{Mc = Length[BVec], Dim = Length[Vars], Vtx, Out = {}},
    If[Dim == 0, Return[If[AllTrue[BVec, # <= 0 &], {{}}, {}]]];
    Do[If[MatrixRank[AMat[[S]]] == Dim, Vtx = LinearSolve[AMat[[S]], BVec[[S]]];
        If[VectorQ[Vtx] && AllTrue[AMat . Vtx - BVec, # >= 0 &], AppendTo[Out, Vtx]]],
      {S, Subsets[Range[Mc], {Dim}]}]; DeleteDuplicates[Out]];

  TSRays[AMat_, Vars_] := Module[{Mc = Length[AMat], Dim = Length[Vars], NS, Ry, Out = {}},
    If[Dim == 0, Return[{}]];
    If[Dim == 1, Do[Ry = {Sgn}; If[(Mc == 0 || AllTrue[AMat . Ry, # >= 0 &]) && ! AllTrue[Ry, # == 0 &], AppendTo[Out, Ry]], {Sgn, {1, -1}}]; Return[DeleteDuplicates[Out]]];
    Do[If[MatrixRank[AMat[[S]]] == Dim - 1, NS = NullSpace[AMat[[S]]];
        If[Length[NS] == 1, Do[Ry = TSPrimitive[Sgn*NS[[1]]];
             If[AllTrue[AMat . Ry, # >= 0 &] && ! AllTrue[Ry, # == 0 &], AppendTo[Out, Ry]],
            {Sgn, {1, -1}}]]], {S, Subsets[Range[Mc], {Dim - 1}]}]; DeleteDuplicates[Out]];

  TSTriangulate[Rays_, Normals_] := Module[{DEff = MatrixRank[Rays], Apex, Facets, Out = {}, Fr},
    If[Length[Rays] <= 1 || (Length[Rays] == DEff && MatrixRank[Rays] == DEff), Return[{Rays}]];
    Apex = Rays[[1]];
    Facets = DeleteDuplicates[Select[Table[Fr = Select[Rays, # . Nv == 0 &]; {Fr, Nv}, {Nv, Normals}],
        (#[[1]] =!= {} && MatrixRank[#[[1]]] == DEff - 1) &], #1[[1]] === #2[[1]] &];
    Do[If[Apex . F[[2]] != 0, Do[AppendTo[Out, Join[{Apex}, Sig]], {Sig, TSTriangulate[F[[1]], Normals]}]], {F, Facets}];
    DeleteDuplicates[Sort /@ Out]];

  TSInteriorXi[Gens_, Cones_] := Module[{NG = Length[Gens], WCand, GoodQ, Wsel},
    WCand = {Prime /@ Range[NG], (Prime /@ Range[NG]) + 1, Prime /@ (Range[NG] + 3),
             2 Prime /@ Range[NG] + 1, Prime /@ (2 Range[NG] + 1), Range[NG]^2 + 1};
    GoodQ[Wt_] := Module[{Xx = Wt . Gens}, AllTrue[Cones, AllTrue[Inverse[Transpose[#]] . Xx, # != 0 &] &]];
    Wsel = SelectFirst[WCand, GoodQ, $Failed]; If[Wsel === $Failed, $Failed, Wsel . Gens]];

  TSFundPoints[GHat_, OpenMask_] := Module[{GInv = Inverse[GHat], Umat, Smat, Vmat, Reps, T0, Tf},
    {Umat, Smat, Vmat} = SmithDecomposition[GHat];
    Reps = (Inverse[Umat] . #) & /@ Tuples[Range[0, # - 1] & /@ Diagonal[Smat]];
    (Module[{W0 = #}, T0 = GInv . W0;
       Tf = MapThread[If[#2, #1 - (Ceiling[#1] - 1), #1 - Floor[#1]] &, {T0, OpenMask}]; GHat . Tf] & /@ Reps)];

  TSReduceConstraints[GeList_, Vars_] := Module[{G, Dim = Length[Vars], AMat, BVec, FreeB = ConstantArray[{-Infinity, Infinity}, Length[Vars]]},
    G = DeleteDuplicates[GeList]; {AMat, BVec} = TSPolyAb[G, Vars];
    G[[Select[Range@Length@G, Function[K, Module[{Others = Delete[Range@Length@G, K], R},
       R = Quiet@LinearProgramming[ConstantArray[0, Dim], Join[AMat[[Others]], {-AMat[[K]]}], Join[BVec[[Others]], {-BVec[[K]] + 1}], FreeB];
       VectorQ[R, NumericQ]]]]]]];

  TSDecompose[GeList0_, Vars_] := Module[{GeList = TSReduceConstraints[GeList0, Vars], Dim = Length[Vars], AMat, BVec, Verts, Rays, Gens, NormalsH, Tris, Xi, Out = {}, GHat, Heights, OpenMask, FundP, VGen, RGen, RaysXY, WPt, HW, JStar},
    {AMat, BVec} = TSPolyAb[GeList, Vars]; Verts = TSVertices[AMat, BVec, Vars];
    If[Verts === {}, Return[{}]]; Rays = TSRays[AMat, Vars];
    Gens = Join[TSPrimitive[Append[#, 1]] & /@ Verts, TSPrimitive[Append[#, 0]] & /@ Rays];
    NormalsH = Join[MapThread[Append[#1, -#2] &, {AMat, BVec}], {UnitVector[Dim + 1, Dim + 1]}];
    Tris = TSTriangulate[Gens, NormalsH]; Xi = TSInteriorXi[Gens, Tris];
    If[Xi === $Failed, Message[TrivializeSeries::xi]; Return[$Failed]];
    Do[GHat = Transpose[Tri]; Heights = Last /@ Tri; OpenMask = (# < 0) & /@ (Inverse[GHat] . Xi);
     FundP = TSFundPoints[GHat, OpenMask]; VGen = Select[Range[Dim + 1], Heights[[#]] >= 1 &];
     RGen = Select[Range[Dim + 1], Heights[[#]] == 0 &]; RaysXY = (Most[Tri[[#]]]) & /@ RGen;
     Do[HW = Last[WPt]; Which[HW == 1, AppendTo[Out, {Most[WPt], RaysXY}],
        HW == 0, Do[If[Heights[[JStar]] == 1, AppendTo[Out, {Most[WPt] + Most[Tri[[JStar]]], RaysXY}]], {JStar, VGen}],
        True, Null], {WPt, FundP}], {Tri, Tris}]; Out];

  TSDisjointBool[Cond_] := Module[{Dnf, Clauses, NegAtom, T, Res2 = {}},
    If[FreeQ[Cond, Or], Return[{Cases[Cond, (Greater | Less | GreaterEqual | LessEqual | Equal | Unequal)[_, _], Infinity]}]];
    Dnf = BooleanConvert[Cond, "DNF"];
    Clauses = Which[Dnf === True, {{}}, Dnf === False, {}, Head[Dnf] === Or, (If[Head[#] === And, List @@ #, {#}] & /@ (List @@ Dnf)), Head[Dnf] === And, {List @@ Dnf}, True, {{Dnf}}];
    NegAtom = {GreaterEqual[A_, B_] :> A < B, LessEqual[A_, B_] :> A > B, Greater[A_, B_] :> A <= B, Less[A_, B_] :> A >= B, Equal[A_, B_] :> A != B, Unequal[A_, B_] :> A == B};
    Do[If[T == 1, AppendTo[Res2, Clauses[[1]]], Do[AppendTo[Res2, Join[Clauses[[T]], (# /. NegAtom) & /@ Combo]], {Combo, Tuples[Clauses[[;; T - 1]]]}]], {T, Length[Clauses]}]; Res2];

  TSProcessEqualities[Cond_, Summand_, Vars_] := Module[{Eqs, Elim = {}, Active = Vars, Ex, UC, Vv, Cv, Sol, CondR, SumR},
    Eqs = DeleteDuplicates[Cases[Cond, Equal[A_, B_] :> (A - B), Infinity]];
    Do[Ex = Expand[Eq //. Elim]; If[Ex === 0, Continue[]];
      UC = Select[Active, MemberQ[{1, -1}, Coefficient[Ex, #]] &];
      Which[UC =!= {}, Vv = First[UC]; Cv = Coefficient[Ex, Vv]; Sol = Vv -> Together[(Cv Vv - Ex)/Cv];
         Elim = Append[((#[[1]] -> (#[[2]] /. Sol)) & /@ Elim), Sol]; Active = DeleteCases[Active, Vv],
       AnyTrue[Active, Coefficient[Ex, #] =!= 0 &], Message[TrivializeSeries::nonunit]; Throw[$Failed],
       True, Null], {Eq, Eqs}];
    CondR = (Cond /. Equal[__] :> True) //. Elim; SumR = Summand //. Elim; {CondR, SumR, Active, Elim}];

  TSCollectPowers[S_, Vrs_] := Module[{Facs, PowQ, PowF, RestF, NDep, ConstP},
    Facs = If[Head[S] === Times, List @@ S, {S}];
    PowQ[f_] := If[MatchQ[f, Power[_, _]], ! FreeQ[f[[2]], Alternatives @@ Vrs], False];
    PowF = Select[Facs, PowQ]; RestF = Select[Facs, ! PowQ[#] &];
    NDep = Product[(Times @@ ((#[[1]]^Coefficient[#[[2]], Vrs[[I]]]) & /@ PowF))^Vrs[[I]], {I, Length[Vrs]}];
    ConstP = Times @@ ((#[[1]]^(#[[2]] /. Thread[Vrs -> 0])) & /@ PowF);
    (Times @@ RestF) ConstP NDep];

  TSOne[Summand0_, Cond0_, Vars0_] := Catch[Module[{FullCond, BoolPieces, Out = {}},
    FullCond = Cond0 && (And @@ ((# >= 0) & /@ Vars0)); BoolPieces = TSDisjointBool[FullCond];
    Do[Module[{ModAtoms, EqAtoms, IneqAtoms, Allowed},
       ModAtoms = Select[Piece, MatchQ[#, (Equal | Unequal)[Mod[_, _], _]] &];
       EqAtoms = Select[Piece, MatchQ[#, _Equal] && FreeQ[#, Mod] &];
       IneqAtoms = Select[Piece, MatchQ[#, (Greater | Less | GreaterEqual | LessEqual)[_, _]] && FreeQ[#, Mod] &];
       Allowed = Map[Function[At, Module[{Mf = At[[1, 2]], Rf = At[[2]]},
            If[Head[At] === Equal, {Mod[Rf, Mf]}, Complement[Range[0, Mf - 1], {Mod[Rf, Mf]}]]]], ModAtoms];
       Do[Module[{Fresh, ModEqs, VarsP, Cond, Summand, Vars, Elim, GeList, DPieces},
          Fresh = Table[Unique["TStau"], {Length@ModAtoms}];
          ModEqs = MapThread[Function[{At, Qv, Sr}, At[[1, 1]] == At[[1, 2]] Qv + Sr], {ModAtoms, Fresh, RC}];
          VarsP = Join[Vars0, Fresh];
          {Cond, Summand, Vars, Elim} = TSProcessEqualities[And @@ Join[EqAtoms, ModEqs, IneqAtoms], Summand0, VarsP];
          GeList = Join[TSToGE /@ Cases[Cond, (Greater | Less | GreaterEqual | LessEqual)[_, _], Infinity], Vars];
          DPieces = TSDecompose[GeList, Vars]; If[DPieces === $Failed, Throw[$Failed]];
          Do[Module[{Q = Length[Dp[[2]]], Sv, NewSv, MapN, ActRules, Relabel},
             Sv = Take[Vars, Q]; NewSv = Take[Vars0, Q];
             MapN = Dp[[1]] + If[Q == 0, 0, Sum[Sv[[L]] Dp[[2, L]], {L, Q}]];
             ActRules = Thread[Vars -> MapN]; Relabel = Thread[Sv -> NewSv];
             AppendTo[Out, {(Summand /. ActRules) /. Relabel, NewSv}]], {Dp, DPieces}]],
         {RC, Tuples[Allowed]}]], {Piece, BoolPieces}]; Out]];

  Pairs = Transpose[{RawSeries, SeriesCond}];
  If[TrueQ[OptionValue[RunInParallel]] && Length[RawSeries] > 1,
    If[$KernelCount === 0, Quiet@LaunchKernels[]];
    DistributeDefinitions[TrivializeSeries];
    ParRes = ParallelMap[TrivializeSeries[{#[[1]]}, {#[[2]]}, SumVars, RunInParallel -> False] &, Pairs, Method -> "FinestGrained"];
    If[! FreeQ[ParRes, $Failed], Return[$Failed]];
    {Join @@ ParRes[[All, 1]], Join @@ ParRes[[All, 2]]},
    Res = Flatten[Map[TSOne[#[[1]], #[[2]], SumVars] &, Pairs], 1];
    If[! FreeQ[Res, $Failed], Return[$Failed]];
    {MapThread[TSCollectPowers, {Res[[All, 1]], Res[[All, 2]]}], Res[[All, 2]]} /.
       {Gamma[a_] :> Gamma[ExpandAll@a], PolyGamma[a_] :> PolyGamma[ExpandAll@a],
        PolyGamma[b_, a_] :> PolyGamma[b, ExpandAll@a]}]];


(* ::Subsection::Closed:: *)
(*Master Series Helper*)


MBCHFastIntersection[conds_,vars_,mode_:Automatic]:=Module[{atoms,legacy,formRow,rowOfAtom,primitiveRow,inConeExactQ,inConeCertifiedQ,reduce,rows},
atoms=Cases[Unevaluated[conds],(_Less|_Greater),Infinity];
legacy=If[ListQ[conds],And@@conds,conds];
formRow[form_]:=Module[{f=Expand[Together[form]],row},row=Coefficient[f,vars];If[TrueQ[Together[f-row . vars]==0],row,$Failed]];
rowOfAtom[Less[a_,b_]]:=formRow[b-a];
rowOfAtom[Greater[a_,b_]]:=formRow[a-b];
primitiveRow[v_]:=Module[{w=Rationalize[Together[v],0],den,nz,g},den=LCM@@(Denominator/@w);
w=Expand[den w];nz=DeleteCases[Abs[w],0];
If[nz=!={},g=Apply[GCD,nz];If[g=!=0,w=w/g]];w];
inConeExactQ[row_,others_]:=Module[{sol},If[others==={},Return[False]];sol=Quiet@LinearProgramming[ConstantArray[0,Length[others]],Transpose[others],({#,0}&/@row),0,Method->"Simplex"];VectorQ[sol,NumberQ]];
inConeCertifiedQ[row_,others_]:=Module[{sol,rat},If[others==={},Return[False]];sol=Quiet@LinearProgramming[ConstantArray[0.,Length[others]],N@Transpose[others],({N@#,0}&/@row),0.,Method->"Simplex"];If[!VectorQ[sol,NumberQ],Return[False]];rat=Rationalize[Chop[sol,10^-9],10^-7];If[!VectorQ[rat,NonNegative],Return[False]];TrueQ[Transpose[others] . rat==row]];
reduce[aa_,rr_]:=Module[{pairs,redAtoms,redRows,i},If[Length[aa]<=40,Return[FullSimplify[legacy]]];If[MemberQ[rr,$Failed],Return[FullSimplify[legacy]]];pairs=DeleteDuplicatesBy[Transpose[{aa,primitiveRow/@rr}],Last];If[pairs==={},Return[True]];redAtoms=pairs[[All,1]];redRows=pairs[[All,2]];i=1;While[i<=Length[redRows],If[inConeCertifiedQ[redRows[[i]],Delete[redRows,i]],redRows=Delete[redRows,i];redAtoms=Delete[redAtoms,i],i++]];i=1;While[i<=Length[redRows],If[inConeExactQ[redRows[[i]],Delete[redRows,i]],redRows=Delete[redRows,i];redAtoms=Delete[redAtoms,i],i++]];And@@redAtoms];
Which[mode==="Rows",rowOfAtom/@atoms,ListQ[mode],If[Length[atoms]=!=Length[mode],FullSimplify[legacy],reduce[atoms,mode]],True,rows=rowOfAtom/@atoms;reduce[atoms,rows]]]


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
(*Module for the Conic Hull Approach*)


Options[ResolveMB]={MaxSolutions->Infinity,PrintSolutions->True,MasterSeries->True,MBNumber->1,RunInParallel->True};

ResolveMB[MBRepIn_,OptionsPattern[]]:=Module[{
msg1, FindIntersectionReturn,MBRepVars,IntVar,
MBRepVarsFlatten,NumFlat,DenFlat,NumGamma,DenGamma,NumCof,DenCof,NumDenIntCof,Delta,ProportionalQ,AnyProportionalQ,RetTup,
MaxRequestedSeries=OptionValue[MaxSolutions],TimeTaken,LengthMessage = "",LengthList,LengthTally,
Dim,Degenerate,F,FSort,Comb,Poles,CombLabels,MasterCharList={},MasterSeriesVar ={},Msg2={},RearrangePos,
PrintSol=OptionValue@PrintSolutions,MasterSeriesQ=OptionValue@MasterSeries,MBRepInput=Most[MBRepIn],DumVar=MBRepIn[[-1]]},

If[OptionValue@MBNumber>Length@MBRepInput, Print["The MB number to be evaluated is greater than the total number of series solution found."];Abort[]];
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
FindIntersectionReturn=FindIntersection[MBRepVars,MaxRequestedSeries,MasterSeriesQ,OptionValue@RunInParallel];
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
(*Module for the Triangulation Approach*)


Options[TriangulateMB]={MasterSeries->True,MaxSolutions->Infinity,PrintSolutions->True,ShortestOnly->False,TopComPath->"/usr/local/bin/"
,TopComParallel->True,Cardinality->None,MaxCardinality->None,SolutionSummary->False,QuickSolve->False,MBNumber->1,RunInParallel->True};

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
MBRepInput=Most[MBRepIn],DumVar=MBRepIn[[-1]],CheckCommand,TriConeRows},

If[OptionValue@MBNumber>Length@MBRepInput, Print["The MB number to be evaluated is greater than the total number of series solution found."];Abort[]];
If[Length@MBRepInput>1, Print["Solving MB Number : ",MBNumberVal]];


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

CheckCommand="which "<>OptionValue[TopComPath]<>"points2triangs";
If[StringLength[RunProcess[$SystemShell,"StandardOutput",CheckCommand]]===0, Print["TOPCOM executables not found at ",OptionValue[TopComPath],". Provide the correct location of executables using the ", Style["TopComPath", Bold]," option."];Abort[]];

If[Length@ATrim===1,Print["Triangulation Method currently works only when the A-matrix is not a row-matrix!"];
Print["Switching to the Conic Hull Method."];
ResolveMBOut=ResolveMB[MBRepIn,MaxSolutions->MaxSol,MBNumber->OptionValue@MBNumber];
BreakTri=True;
Return@ResolveMBOut;];
If[QSolve,TPCommand="points2placingtriang",

{RetTup,Comb,Poles,R,B,NumDenIntCof,Degenerate,RR,Msg1,SGN}=FindAllGoodComb[MBRepVars,OptionValue@RunInParallel];
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
NumDenIntCof=NumDenIntGammaCof=Join[NumGammaCof,-DenGammaCof];
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
Triangulation=With[{CombIdx=First/@PositionIndex[Comb],NumLengthW=NumLength},
Sort@Table[Sort[CombIdx[Complement[Range@NumLengthW,#]]&/@AllTriangulations[[i]]],{i,Length@AllTriangulations}]];
];

PrintTemporary["Done!"];



If[MSeries&&!SolSummary&&Degenerate,

PrintTemporary["Finding Masters Series"];

TriConeRows=MBCHFastIntersection[# ,RR,"Rows"]&/@B;

Do[

Labels=Triangulation[[k]];
Which[Degenerate&&(Dim>=2),

MasterCH=Insert[MasterCH,MBCHFastIntersection[B[[Labels]],RR,Join@@TriConeRows[[Labels]]],-1];
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
(*Module for Cross-Checks*)


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


Options[EvaluateSeries]={PrintSeries->True,RearrangeSeries->False,RunInParallel->True};

EvaluateSeries[ResMB_,MBParaSub_List,SeriesNum_Integer,OptionsPattern[]]:=Module[{CalculateResidueReturn,SeriesNumber,TotalSeries=ResMB[[7]],
Msg4,TimeTaken,PrintSol=OptionValue@PrintSeries,DumVar=ResMB[[-1]],Rearrange=OptionValue@RearrangeSeries,TrivializeSeriesOut,Dim,SumVars,SeriesCondition,
AllSeries,Msg3={},TrivialQ=False,SeriesLengthList,ParQ=TrueQ[OptionValue[RunInParallel]]},

TimeTaken=AbsoluteTiming[

If[SeriesNum>TotalSeries,Print["The series number to be evaluated is greater than the total number of series solution found."];Abort[]];

CalculateResidueReturn=CalculateResidue@@(Join[{ResMB[[8]],ResMB[[1]][[SeriesNum]]}/.MBParaSub,ResMB[[{2,3,6}]]/.MBParaSub,ResMB[[{-1}]],{ParQ}]);
SeriesNumber=CalculateResidueReturn[[1]];
AllSeries=CalculateResidueReturn[[2]];
Dim=CalculateResidueReturn[[3]];
SeriesCondition=CalculateResidueReturn[[4]];
SeriesCondition=Table[If[i,And@@(Subscript[n, #]>= 0&/@Range[Dim]),Nothing,i],{i,SeriesCondition}];

If[Rearrange,
SumVars=Subscript[n, #]&/@Range[Dim];
PrintTemporary["Rearranging the summation indices to write the series in canonical form."];
TrivializeSeriesOut=TrivializeSeries[AllSeries,SeriesCondition,SumVars];
If[MatchQ[TrivializeSeriesOut,{_List,_List}]&&FreeQ[TrivializeSeriesOut,$Failed]&&Length[TrivializeSeriesOut[[1]]]===Length[TrivializeSeriesOut[[2]]]&&Length[TrivializeSeriesOut[[1]]]>0&&AllTrue[TrivializeSeriesOut[[1]],Head[#]=!=List&]&&AllTrue[TrivializeSeriesOut[[2]],Head[#]===List&],
TrivialQ=True;
TrivializeSeriesOut=Transpose[SortBy[Transpose[TrivializeSeriesOut], Length[#[[2]]] &]];
SeriesLengthList=Tally[Length /@ TrivializeSeriesOut[[2]]]//Sort;
AllSeries=TrivializeSeriesOut[[1]];
SeriesNumber=Length@AllSeries;
SeriesCondition=And@@Thread[#>=0]&/@(TrivializeSeriesOut[[2]]);
CalculateResidueReturn=ReplacePart[CalculateResidueReturn,1->SeriesNumber];
CalculateResidueReturn=ReplacePart[CalculateResidueReturn,2->AllSeries];
CalculateResidueReturn=ReplacePart[CalculateResidueReturn,4->SeriesCondition],
Print["Note: this series cannot be rearranged into trivial (canonical) form; returning the non-rearranged series instead."];
TrivialQ=False;SeriesLengthList={}
];

];


Do[
Msg3= Insert[Msg3,StringForm["Series Number `1` :: `2` valid for `3`",i, AllSeries[[i]],SeriesCondition[[i]]],-1];
,{i,SeriesNumber}];

Msg4=StringForm["The series solution is a sum of the following `` series.", SeriesNumber];
Print[Msg4];

If[Rearrange, Do[Print["Found ",i[[2]]," series of ",i[[1]],"-fold."];,{i,SeriesLengthList}]];

If[PrintSol,
Do[Print[Msg3[[i]]]; Print[""],{i,SeriesNumber}]];
];
Print["Time Taken ",TimeTaken[[1]]," seconds"];

Return[Append[CalculateResidueReturn,TrivialQ]]
]


(* ::Subsection::Closed:: *)
(*Module for Numerical Summation*)


Options[SumAllSeries]={RunInParallel->True,NumericalPrecision->MachinePrecision,AdaptiveLimits->False};

SumAllSeries[SeriesInfo_,MBVarSub_,lim_,OptionsPattern[]]:=Module[{SeriesNumber=SeriesInfo[[1]],SeriesSub=SeriesInfo[[2]]/.MBVarSub,
SeriesCondition=SeriesInfo[[4]],Dim=SeriesInfo[[3]],nseries={},TimeTaken,TrivialQ=SeriesInfo[[5]],SeriesDimList,MaxDim,SumLimList,PieceValue,
PieceValueCond,PowerVals,CleanTinyImag,OkQ,LimListAll,TaskList,TaskOrder,EpsTrunc,AdaptiveQ=TrueQ[OptionValue[AdaptiveLimits]],RequestedPrecision=OptionValue[NumericalPrecision],FinalResult},

TimeTaken=AbsoluteTiming[

If[lim \[NotElement] NonNegativeIntegers,Print["Summation upper-limit should be a nonnegative integer"];Abort[]];

SeriesDimList=If[TrivialQ,Table[Which[i===True,0,Head[i]===And,Length[i],True,1],{i,SeriesCondition}],ConstantArray[Dim,SeriesNumber]];
MaxDim=Max[SeriesDimList];

EpsTrunc=10.^(-(If[OptionValue[NumericalPrecision]===MachinePrecision,$MachinePrecision,OptionValue[NumericalPrecision]]+1));
LimListAll=If[!TrivialQ,ConstantArray[ConstantArray[lim,Dim],SeriesNumber],
Table[Module[{DimVal=SeriesDimList[[k]],CapL,Pows,Mag},
If[DimVal==0,{},
CapL=Max[Round[(lim+1)^(MaxDim/DimVal)]-1,0];
If[!AdaptiveQ,ConstantArray[CapL,DimVal],
Pows=DeleteDuplicates@Cases[SeriesSub[[k]],(Power[b_,e_]/;FreeQ[b,Subscript[n, _]]&&!FreeQ[e,Subscript[n, _]]):>{b,e},{0,Infinity}];
Table[Mag=Abs[N[Times@@(Power[#[[1]],Coefficient[#[[2]],Subscript[n, j]]]&/@Pows)]]//Quiet;
Which[Mag==0.,0,Mag<1.,Min[Ceiling[Log[EpsTrunc]/Log[Mag]],lim],True,lim],{j,DimVal}]]]],{k,SeriesNumber}]];
SumLimList=Table[Table[{Subscript[n, j],0,LimListAll[[k,j]]},{j,SeriesDimList[[k]]}],{k,SeriesNumber}];

PowerVals=Function[{BaseIn,ExpVals,Prec},Module[{BaseN=N[BaseIn,Prec],ExpN=N[ExpVals,Prec],ExpRound,Tol},
Tol=If[Prec===MachinePrecision,10^-10,10^(-Min[20,Max[10,Floor[Prec/2]]])];
If[TrueQ[Chop[Im[BaseN]]==0]&&TrueQ[Re[BaseN]<0],
ExpRound=Round[ExpN];
If[Max[Abs[Flatten@{ExpN-ExpRound}]]<Tol,(-1)^ExpRound*Abs[BaseN]^ExpRound,BaseN^ExpN],
BaseN^ExpN]]];

OkQ=Function[x,NumberQ[x]||FreeQ[x,Indeterminate|DirectedInfinity|ComplexInfinity|Overflow|Underflow]];

PieceValue=Function[{TermIn,DimVal,LList,Prec},
Module[{VarSyms,VarPat,TermW,Pts,NVecs,VarSub,SpFns,SpSlots,BodyW,BodyFn,Args,ArgVals,SpVals,Res},
If[DimVal==0,N[TermIn,Prec]//Quiet,
If[Prec=!=MachinePrecision,Total[(Function[pt,Module[{v=N[TermIn/.Thread[(Subscript[n, #]&/@Range[DimVal])->pt],Prec]//Quiet},If[OkQ[v],v,0]]])/@Tuples[Range[0,#]&/@LList]],(
VarSyms=Table[Unique["MBnv"],{DimVal}];
VarPat=Alternatives@@VarSyms;
TermW=N[TermIn,Prec]/.Table[Subscript[n, j]->VarSyms[[j]],{j,DimVal}];
Pts=Tuples[Range[0,#]&/@LList];
NVecs=Transpose[Pts];
VarSub=Thread[VarSyms->NVecs];
SpFns=DeleteDuplicates@Cases[TermW,(Gamma[a_]/;!FreeQ[a,VarPat])|(PolyGamma[_,a_]/;!FreeQ[a,VarPat])|(Power[b_,e_]/;FreeQ[b,VarPat]&&!FreeQ[e,VarPat]),{0,Infinity}];
Args=DeleteDuplicates[Join[Cases[SpFns,Gamma[a_]:>a,{1}],Cases[SpFns,PolyGamma[_,a_]:>a,{1}]]];
ArgVals=AssociationThread[Args->(N[#/.VarSub,Prec]&/@Args)];
SpSlots=Table[Unique["MBsp"],{Length[SpFns]}];
BodyW=TermW/.Thread[SpFns->SpSlots];
BodyFn=Function[Evaluate[Join[VarSyms,SpSlots]],Evaluate[BodyW]];
SpVals=Function[s,Switch[Head[s],Gamma,Gamma[ArgVals[s[[1]]]],PolyGamma,PolyGamma[s[[1]],ArgVals[s[[2]]]],Power,N[s[[1]],Prec]^(s[[2]]/.VarSub)]]/@SpFns;
Res=(BodyFn@@Join[NVecs,SpVals])//Quiet;
If[ListQ[Res],If[Developer`PackedArrayQ[Res],Total[Res],Module[{bad=Flatten@Position[Res,x_/;!OkQ[x],{1},Heads->False]},If[bad=!={},Res[[bad]]=(Function[ix,Module[{v=N[TermIn/.Thread[(Subscript[n, #]&/@Range[DimVal])->Pts[[ix]]],Prec]//Quiet},If[OkQ[v],v,0.]]])/@bad];Total[Res]]],If[OkQ[Res],Length[Pts]*Res,0.]])]]]];

PieceValueCond=Function[{TermIn,CondIn,DimVal,LList,Prec},
Module[{VarSyms,VarPat,TermW,CondW,CondFn,Pts,GoodPts,NVecs,VarSub,SpFns,SpSlots,BodyW,BodyFn,Args,ArgVals,SpVals,Res},
If[DimVal==0,If[TrueQ[CondIn],N[TermIn,Prec]//Quiet,0],
VarSyms=Table[Unique["MBnv"],{DimVal}];
VarPat=Alternatives@@VarSyms;
TermW=TermIn/.Table[Subscript[n, j]->VarSyms[[j]],{j,DimVal}];
CondW=CondIn/.Table[Subscript[n, j]->VarSyms[[j]],{j,DimVal}];
CondFn=Function[Evaluate[VarSyms],Evaluate[CondW]];
Pts=Tuples[Range[0,#]&/@LList];
GoodPts=Pick[Pts,TrueQ/@(CondFn@@@Pts)];
If[GoodPts==={},If[Prec===MachinePrecision,0.,0],
If[Prec=!=MachinePrecision,Total[(Function[pt,Module[{v=N[TermIn/.Thread[(Subscript[n, #]&/@Range[DimVal])->pt],Prec]//Quiet},If[OkQ[v],v,0]]])/@GoodPts],
NVecs=Transpose[GoodPts];
VarSub=Thread[VarSyms->NVecs];
SpFns=DeleteDuplicates@Cases[TermW,(Gamma[a_]/;!FreeQ[a,VarPat])|(PolyGamma[_,a_]/;!FreeQ[a,VarPat])|(Power[b_,e_]/;FreeQ[b,VarPat]&&!FreeQ[e,VarPat]),{0,Infinity}];
Args=DeleteDuplicates[Join[Cases[SpFns,Gamma[a_]:>a,{1}],Cases[SpFns,PolyGamma[_,a_]:>a,{1}]]];
ArgVals=AssociationThread[Args->(N[#/.VarSub,Prec]&/@Args)];
SpSlots=Table[Unique["MBsp"],{Length[SpFns]}];
BodyW=N[TermW/.Thread[SpFns->SpSlots],Prec];
BodyFn=Function[Evaluate[Join[VarSyms,SpSlots]],Evaluate[BodyW]];
SpVals=Function[s,Switch[Head[s],Gamma,Gamma[ArgVals[s[[1]]]],PolyGamma,PolyGamma[s[[1]],ArgVals[s[[2]]]],Power,PowerVals[s[[1]],s[[2]]/.VarSub,Prec]]]/@SpFns;
Res=(BodyFn@@Join[NVecs,SpVals])//Quiet;
If[ListQ[Res],If[Developer`PackedArrayQ[Res],Total[Res],Module[{bad=Flatten@Position[Res,x_/;!OkQ[x],{1},Heads->False]},If[bad=!={},Res[[bad]]=(Function[ix,Module[{v=N[TermIn/.Thread[(Subscript[n, #]&/@Range[DimVal])->GoodPts[[ix]]],Prec]//Quiet},If[OkQ[v],v,0.]]])/@bad];Total[Res]]],If[OkQ[Res],Length[GoodPts]*Res,0.]]]]]]];

Which[OptionValue[RunInParallel]===False,
Off[General::munfl];Off[General::stop];
If[TrivialQ,
Monitor[nseries=Table[PieceValue[SeriesSub[[i]],SeriesDimList[[i]],LimListAll[[i]],RequestedPrecision],{i,SeriesNumber}],ProgressIndicator[i,{1,SeriesNumber}]],
Monitor[nseries=Table[PieceValueCond[SeriesSub[[i]],SeriesCondition[[i]],Dim,LimListAll[[i]],RequestedPrecision],{i,SeriesNumber}],ProgressIndicator[i,{1,SeriesNumber}]]],

OptionValue[RunInParallel]===True,
If[$KernelCount===0,LaunchKernels[]];
ParallelEvaluate[Off[General::munfl]];ParallelEvaluate[Off[General::stop]];
If[TrivialQ,
TaskList=Table[{BinarySerialize[SeriesSub[[i]]],SeriesDimList[[i]],LimListAll[[i]]},{i,SeriesNumber}];
TaskOrder=Reverse@Ordering[Table[Times@@(LimListAll[[i]]+1)*LeafCount[SeriesSub[[i]]],{i,SeriesNumber}]];
DistributeDefinitions[PieceValue,OkQ];
With[{RP=RequestedPrecision},
nseries=ParallelMap[Function[t,PieceValue[BinaryDeserialize[t[[1]]],t[[2]],t[[3]],RP]],TaskList[[TaskOrder]],Method->"FinestGrained"][[Ordering[TaskOrder]]]]
,
TaskList=Table[{BinarySerialize[SeriesSub[[i]]],BinarySerialize[SeriesCondition[[i]]],Dim,LimListAll[[i]]},{i,SeriesNumber}];
TaskOrder=Reverse@Ordering[Table[Times@@(LimListAll[[i]]+1)*LeafCount[SeriesSub[[i]]],{i,SeriesNumber}]];
DistributeDefinitions[PieceValueCond,PowerVals,OkQ];
With[{RP=RequestedPrecision},
nseries=ParallelMap[Function[t,PieceValueCond[BinaryDeserialize[t[[1]]],BinaryDeserialize[t[[2]]],t[[3]],t[[4]],RP]],TaskList[[TaskOrder]]][[Ordering[TaskOrder]]]]]]
];

CleanTinyImag=Function[{z,Prec},Module[{p,tol,zn},
p=If[Prec===MachinePrecision,$MachinePrecision,Prec];
tol=10^(-Max[8,p-4]);
zn=N[z,Prec]//Quiet;
If[NumberQ[zn]&&Abs[zn]!=0&&Abs[Im[zn]]<=tol*Abs[zn],Re[zn],zn]]];

FinalResult=CleanTinyImag[N[Total@nseries,OptionValue[NumericalPrecision]]//Quiet,OptionValue[NumericalPrecision]];

Print["Numerical Result: ",FinalResult];
Print["Time Taken ",TimeTaken[[1]]," seconds"];
(*New*)
Return[FinalResult]
]


(* ::Section::Closed:: *)
(*End Message*)


End[];
EndPackage[];
Print["Last Updated: \!\(\*SuperscriptBox[\(17\), \(th\)]\) June, 2026"];
Print["Version 1.0 by B. Ananthanarayan, S. Banik, S. Ghosh & S. Friot"];
Print["Version 1.7 by S. Banik & S. Friot"];
