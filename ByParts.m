(* ::Package:: *)

(* ::Title:: *)
(*By Parts*)


(* ::Text:: *)
(*:Author:*)
(*Joseph Elliston*)


(* ::Text:: *)
(*:Summary:*)
(*This package provides an easy interface for performing integrations by parts by moving derivatives, assuming that boundary terms may be neglected. *)


(* ::Text:: *)
(*:Context: *)
(*xAct`ByParts`*)


(* ::Text:: *)
(*:Copyright:*)
(*Copyright (C) University of Sussex 2013-2014*)


(* ::Text:: *)
(*:History:*)
(*Current version: v1.0.1 (2014.08.28)*)
(*v1.0 - 14.08.21: First release*)
(*See ByParts.history for incremental updates*)
(**)
(**)


(* ::Section::Closed:: *)
(*Preliminaries*)


(* ::Subsection::Closed:: *)
(*GPL*)


(* ::Text:: *)
(*Copyright (C) 2013-2014 University of Sussex*)
(**)
(*This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.*)
(**)
(*This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.*)
(**)
(*You should have received a copy of the GNU General Public License along with xAct; if not, write to the Free Software Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA. *)


(* ::Subsection::Closed:: *)
(*Version numbers and package dependencies*)


(* ::Text:: *)
(*Package version number:*)


xAct`ByParts`$Version={"1.0.1",Date[][[{1,2,3}]]};


(* ::Text:: *)
(*Expected version number for xTensor:*)


xAct`ByParts`$xTensorVersionExpected={"1.0.5",{2013,1,30}};


(* ::Text:: *)
(*In case the package is loaded multiple times, wipe the memory of all package symbols apart from the version numbers defined above.*)


With[
	{xAct`ByParts`Private`ByPartsSymbols=
		DeleteCases[
			Join[Names["xAct`ByParts`*"],Names["xAct`ByParts`Private`*"]],
			"$Version"|"xAct`ByParts`$Version"|"$xTensorVersionExpected"|"xAct`ByParts`$xTensorVersionExpected"
		]
	},
	Unprotect/@xAct`ByParts`Private`ByPartsSymbols;
	Clear/@xAct`ByParts`Private`ByPartsSymbols;
]


(* ::Text:: *)
(*Set this package to be the last package to load*)


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,
	xAct`xCore`Private`$LastPackage="xAct`ByParts`"
];


(* ::Text:: *)
(*Begin the package and load dependencies*)


BeginPackage["xAct`ByParts`",{"xAct`xCore`","xAct`xPerm`","xAct`xTensor`"}];


(* ::Text:: *)
(*Check version of xTensor:*)


If[
	Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],
	Message[General::versions,"xTensor",xAct`xTensor`$Version,$xTensorVersionExpected];
	Abort[]
];


(* ::Subsection::Closed:: *)
(*Output message after loading the package*)


Print[xAct`xCore`Private`bars];
Print["Package xAct`ByParts version ",$Version[[1]],", ",$Version[[2]]];
Print["Copyright (C) 2013-2014, University of Sussex, under the General Public License."];
Print["Written by Joseph Elliston."];


(* ::Text:: *)
(*We specify the context xAct`ByParts` to avoid overriding the Disclaimer of xCore, xPerm and xTensor. However we need to turn off the message General:shdw temporarily:*)


Off[General::shdw];
xAct`ByParts`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."];
On[General::shdw];


(* ::Text:: *)
(*If xAct`ByParts` is the last package read, then print the short GPL disclaimer:*)


If[xAct`xCore`Private`$LastPackage==="xAct`ByParts`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars]];


(* ::Subsection::Closed:: *)
(*Reduce and configure output*)


$DefInfoQ = False;
$UndefInfoQ = False;


(* ::Subsection::Closed:: *)
(*Detailed package description*)


$ByPartsDescription="The motivation for creating this package is that performing intergations by parts on large expressions can be tedious and error-prone. By automating this calculation we can also simplify the output from integration by parts and make all possible cancellations between terms without having to rely on being able to do this by eye. Furthermore, one can employ the ToCanonical function in xTensor to ensure that all terms are in a canonical form and therefore maximise these simplifications. 

Throughout we ignore total derivative terms under the implicit assumption that these are negligible on the boundary of the integration. This is not always the case for integations with a boundary and in such scenarios the user must take responsibility for ensuring that their variational problem is well posed and they must keep track of boundary terms where relevant. Nevertheless, most standard calculations to derive the local equations of motion are not plagued by such issues and in these cases one can apply this package without concern about boundary contributions.

For an arbitrary expression, there is no clear algorithmic procedure for making the integrations by parts because the desired goal is a function of user-preference. As a result, this package leaves the user in control of which integrations by parts are computed, and simply provides a nice interface for performing the calculation. 

This package is able to carry out integrations by parts on any term of the form: rest*Der1@Der2@...DerN@arg where Der1, Der2,... etc are either ParamD[indice] or derivatives der[indice] with CovDQ@der=True. These derivatives can therefore include any user-defined derivatives as well as the fiducial\.1d PD derivative. 'arg' is some general argument and 'rest' is some general multiplicative factor. 

We assume that each type of derivative 'DerX' commutes with any other derivative 'DerY' except when both are the same covariant derivative with different indices. This assumption requires that any covariant derivatives that will be commuted are defined on independent spaces. If this proves to be a restriction for a given calculation, then one can always expand out one or both of the covariant derivatives into partial derivatives.

If one wants to integate by parts with a derivative that is not ParamD or PD then it is first necessary to commute the derivative to the outside of any other derivatives of the same type. This commutation will yield new curvature terms. Alternatively, if one is integrating by parts with partial derivatives then it is necessary to first lower any contravariant indices using the SeparateMetric[] command.

The key function is the 'ByParts' function. This function contains the list of integrations that one needs to compute and these are executed sequentially. To specify a given integration by parts, one needs to provide the term to be integrated, the piece of that term containing the derivative to be moved and then the index of the derivative. This is explained in greater detail in the examples below. This method has been developed so that the user can copy and paste the expressions to perform the integrations speedily. 

'ByParts' is likely to be the only function that the user will use. However, it is certainly conceviable that the user may want to perform some user-defined simplification rules after each integration by parts. For this reason we provide the function 'AfterByParts' which, by default, does nothing. The user can modify this function to do whatever they like, such as setting derivatives of a particular term to zero.

The code that makes up the 'ByParts' function calls another function 'ByPartsKernel' to perform the integration by parts itself. The key role of 'ByPartsKernel' is to compare the output of the integration by parts to the input. If one of the outputs is equal to a negative integer times the input then this means that there is a symmetry in the expression and the output is modified accordingly. One can therefore view the 'ByParts' function as the code that takes a general sum of terms and a list of integrations and feeds these into the 'ByPartsKernel' function as appropriate, before collecting and simplifying the output.";


(* ::Input:: *)
(**)


(* ::Subsection::Closed:: *)
(*Acknowledgements*)


(* ::Text:: *)
(*My thanks go to David Seery and the University of Sussex for allowing me to devote so much time into writing this package. *)
(*I am also indebted to Guido Pettinari for patiently sharing some of his vast knowledge of Mathematica.*)
(*--Joe Elliston*)


(* ::Section::Closed:: *)
(*Public context*)


(* ::Subsection::Closed:: *)
(*Usage Messages*)


Unprotect[AfterByParts];
AfterByParts::usage = "AfterByParts[expr] is the function that acts on the result after each integration by parts in the 'ByParts' function. By default this function does nothing. It may be configured by the user to perform additional simplification routines as required.";

Unprotect[IntegrationListQ];
IntegrationListQ::usage = "IntegrationListQ[list] returns True if 'list' is of the correct form to be the second argument in the 'ByParts' function. Otherwise it returns False.";

Unprotect[InverseComposition];
InverseComposition::usage = "InverseComposition[expr] will output a list of the form {{Deriv1,Deriv2,Deriv3,...},Arg} where expr=Deriv1@Deriv2@Deriv3@...@Arg, where Deriv can be any covariant dderivative for which CovD returns True, or a parameter derivative ParamD. InverseComposition therefore strips derivatives off of the argument Arg and returns the result in a decomposed form. This process is essentailly the inverse of Mathematica's 'Composition' function, hence the name. For any argument that does not fit the description of being at least one derivative acting on an argument, the output will be {{},expr}.";

Unprotect[CovDToTop];
CovDToTop::usage = "CovDToTop[expr,CovD,indice] reorders covariant derivatives of type 'CovD' in 'expr' such that the derivative 'CovD[indice]' acts at the highest level with respect to any other derivatives of the same type. This uses the function CommuteCovDs to introduce the necessary curvature tensors. If CovD is PD or ParamD then this function does nothing.";
CovDToTop::noCD = "The derivative `1` is not present in the input expression.";
CovDToTop::noind = "The derivative `1` is not present in the input expression with an index `2`.";

Unprotect[ByParts];
ByParts::usage = "ByParts[expr, IntegrationsList] integrates 'expr' by parts using user-defined instructions, whilst presuming all boundary terms vanish. It is also assumed that all derivatives commute with the exception of covariant derivatives of the same type, where Riemann tensors are introduced as appropriate. The integration by parts itself is done by the function 'ByPartsKernel'. After each application of 'ByPartsKernel' the result is acted upon by the function 'AfterByParts' which may be set by the user, for example to simplify the resulting terms. By default 'AfterByParts' does nothing. Precisely which integrations are carried out is specified in the argument 'IntegrationsList' which takes the form {Part1,Part2,...} where each 'Part' details a specific integration by parts to compute, and these are computed sequentially. Each 'Part' must be of the form of a list of three further arguments {term, piece, indice} defined as: 
'term': 'expr' will in general be a sum of terms and 'term' is the particular term in this summation that is being integrated. If there is only one term in 'expr' then 'term' = 'expr'.
'piece': within the 'term' (as defined above) there will be a collection of derivatives acting on some argument and one of these derivatives will be the one to be moved. The argument and all of its derivatives together form the 'piece'. It is necessary to specify the piece precisely because the same derivative could appear multiple times in 'term' and it is then necessary to know precisely which derivative to move.
'indice': This is the indice of the derivative being moved, with the correct sign. For parameter derivative ParamD then this should just be the name of the parameter without any minus sign.

HOW TO USE THIS FUNCTION: After 'expr' is expanded into a sum of terms (using Expand[expr]) the user can copy any one of these terms and paste it into 'ByParts' as the 'term' argument above. Then the 'piece' can be copied from the 'term' and pasted as the next argument. Finally the indice can be written by hand. This copy-and-paste method makes the procedure simple to do. Note that the whole expression is updated after each integration by parts and so you are recommended to add successive integrations by parts piecewise. Note also that the ByParts function sometimes exhibits the curious and unexplained behaviour of seizing up and giving no result every other time of being executed. No incorrect result is ever generated, and the result can be made by stopping and restarting the evaluation of that cell.";

ByParts::both = "The first argument of ByParts contains contravariant partial derivatives (use SeparateMetric[] first to solve this problem) and the second argument is also of an incorrect form.";
ByParts::upPD = "The first argument of ByParts contains contravariant partial derivatives (use SeparateMetric[] first to solve this problem).";
ByParts::invlist = "The second argument of ByParts is not of the correct form.";
ByParts::nomatch = "Term `1` not found in the expression being integrated.";
ByParts::multimatch = "Term `1` found multiple times in the expression being integrated.";


(* ::Section::Closed:: *)
(*Private Context*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Debugging*)


(* ::Text:: *)
(*Define the variable InfoLevel to control the output of useful debugging messages. *)


(* ::Item:: *)
(*InfoLevel=0 --- All output messages are minimised.*)


(* ::Item:: *)
(*InfoLevel=1 --- Higher-level output messages are permitted.*)


(* ::Item:: *)
(*InfoLevel=2 --- All output messages are permitted.*)


InfoLevel=0;


(* ::Text:: *)
(*Also define the function InfoPrint[stuff,level] that prints out 'stuff' only if InfoLevel is sufficient.*)


SetNumberOfArguments[InfoPrint,2];
InfoPrint[expr_,level_]:=If[level<=InfoLevel,Print[expr];];


(* ::Subsection::Closed:: *)
(*AfterByParts*)


(* ::Input:: *)
(*?AfterByParts*)


SetNumberOfArguments[AfterByParts,1];
AfterByParts[expr_]:=expr;


(* ::Subsection::Closed:: *)
(*IntegrationListQ*)


(* ::Input:: *)
(*?IntegrationListQ*)


SetNumberOfArguments[IntegrationListQ,1];

IntegrationListQ[list_]:=
Module[{},
	TrueQ[
		(*Check that the overall head is a list*)
		Head@list===List
		&&
		(*Check the form of the sub-lists*)
		0==Count[MatchQ[#,{__,_,_}]&/@list,False]
		&&
		(*Check that the middle terms in each sublist have a derivative or ParamD head*)
		0==Count[MatchQ[#,{__,b_,_}/;(CovDQ@Head@Head@b||Head@Head@b===ParamD)]&/@list,False]
		&&
		(*Check that the terms 'b' are present in 'a' once and only once*)
		0==Count[If[#/.{a__,b_,_}:>True/;Length@Position[a,b]==1,True,False]&/@list,False]
		&&
		(*Check that the indice is valid, meaning that it is included within 'b'*)
		0==Count[
			((#/.{__,b_,c_}:>True/;Position[b,_?CovDQ[c]|ParamD[c,___]]=!={})/.{__,_,_}->False)&
			/@list,False
		]
	]
];


(* ::Subsection::Closed:: *)
(*InverseComposition*)


(* ::Input:: *)
(*?InverseComposition*)


SetNumberOfArguments[InverseComposition,1];

InverseComposition[expr_]:=
Module[{Arg,DerivList,DerRemover},
	(*Set default output values*)
	Arg=expr;
	DerivList={};
	(*Define DerRemover as the function that strips derivatives off Arg onion-style and appends these to DerivList*)
	DerRemover[arg_]:=
		If[MatchQ[arg,A_[B__][C_]/;(CovDQ@A||ParamD===A)],
			(*In this case thre is a derivative to strip off*)
			AppendTo[DerivList,Head@arg];
			First@arg,
			(*In this case there is no derivative so just return the same result*)
			arg
		];
	(*Apply DerRemover to Arg until no further change happens*)
	Arg=FixedPoint[DerRemover,Arg];
	(*Split ParamD derivatives into separate components*)
	DerivList=Flatten[DerivList//.ParamD[A_,B__]:>{ParamD[A],ParamD[B]}];
	(*output results*)
	{DerivList,Arg}
];



(* ::Subsection::Closed:: *)
(*CovDToTop*)


(* ::Input:: *)
(*?CovDToTop*)


(*In order that we can move derivatives on a scalar into the user-defined order 
we need to switch off the automatic commutation of derivatives on scalars*)
$CommuteCovDsOnScalars=False;
SetNumberOfArguments[CovDToTop,4];

(*There is nothing to be done if the derivative to be moved is PD or ParamD*)
CovDToTop[expr_,PD,_,rest_]:={expr,rest};
CovDToTop[expr_,ParamD,_,rest_]:={expr,rest};

(*The default for the fourth argument is an empty list*)
CovDToTop[expr_,CD_,ind_]:=CovDToTop[expr,CD,ind,{}];
CovDToTop[expr_,CD_,ind_,rest_]:=
Module[{tmp,ops,arg,CDlist,nonCDlist,newexpr,newderterm,newcurvterms},
	(*Decompose expr*)
	tmp=InverseComposition@expr;
	ops=First@tmp;
	InfoPrint["ops = "<>ToString@ops,2];
	arg=Last@tmp;
	InfoPrint["arg = "<>ToString@arg,2];
	(*Collect terms in ops that are of the same type as CD and those that are not, 
	preserving other orderings*)
	CDlist=Extract[ops,Position[ops,CD[_]]];
	InfoPrint["CDlist = "<>ToString@CDlist,2];
	nonCDlist=Delete[ops,Position[ops,CD[_]]];
	InfoPrint["nonCDlist = "<>ToString@nonCDlist,2];
	(*Use these lists to write expr with all CD derivatives together, called newexpr*)
	newexpr=Composition[Sequence@@CDlist][Composition[Sequence@@nonCDlist][arg]];
	InfoPrint["newexpr = "<>ToString@newexpr,2];
	(*Give a trivial output if:
	1) CD[ind] is not present in CDlist
	2) CD[ind] is the first element in CDlist
	3) CDlist only has one element
	output={expr,rest}*)
	If[(!MemberQ[CDlist,CD[ind],Infinity]||First@CDlist===CD[ind]||Length@CDlist==1),
		{expr,rest},
		(*Otherwise one needs to commute derivatives...*)
		(*Derivatives in CDlist that are later in the list than CD[ind] are removed*)
		CDlist=CDlist[[1;;First@Flatten@Position[CDlist,CD[ind]]]];
		InfoPrint["CDlist = "<>ToString@CDlist,2];
		(*Now permute the last two derivatives*)
		tmp=CommuteCovDs[newexpr,CD,{First[CDlist[[-1]]],First[CDlist[[-2]]]}]//ScreenDollarIndices//Expand;
		InfoPrint["tmp = "<>ToString@tmp,2];
		(*split the result into newcurvterms and newderterm*)
		newderterm=If[Head@tmp===Plus,Extract[tmp,First@Flatten@Position[tmp,CD[ind]]],tmp];
		InfoPrint["newderterm = "<>ToString@newderterm,2];
		newcurvterms=tmp-newderterm//Expand;
		InfoPrint["newcurvterms = "<>ToString@newcurvterms,2];
		newcurvterms=If[newcurvterms===0,{},If[Head@newcurvterms===Plus,List@@newcurvterms,{newcurvterms}]];
		InfoPrint["newcurvterms = "<>ToString@newcurvterms,2];
		(*Act the function recursively over the output*)
		CovDToTop[newderterm,CD,ind,Join[rest,newcurvterms]]
	]
]/;(Head@Head@expr===ParamD||CovDQ@Head@Head@expr)/;CovDQ@CD/;MemberQ[VBundlesOfCovD@CD,VBundleOfIndex@ind];


(* ::Subsection::Closed:: *)
(*ByPartsKernel*)


(* ::Text:: *)
(*ByPartsKernel is a private function within the ByParts package.*)


(* ::Input:: *)
(*ByPartsKernel::usage = "ByPartsKernel[rest,arg,der] is the lowest level function for performing integrations by parts. This function requires that the term being integrated has been decomposed into the form: term = rest*der[arg] where:*)
(*der = derivative we are moving *)
(*arg = the argument of the derivative that we are moving.*)
(*rest = the remainder of the expression (may involve other factors of arg or der[arg]).";*)


(* ::Text:: *)
(*ByPartsKernel analyses the output and identifies if any of the resulting terms equal a negative integer times the input. If this is the case then this means that there is symmetry in the input fuction. These degenerate factors are removed and the remaining terms divided by the appropriate integer factor.*)


SetNumberOfArguments[ByPartsKernel,3];

ByPartsKernel[rest_,arg_,der_]:=
Module[{ii,input,output,outputlist,number,degeneraciespositions},
	input=(rest*der[arg])//Simplification//NoScalar//Expand;
	output=(-der[rest]*arg)//Simplification//NoScalar//Expand;
	
	(*Now find the 'number' of degenerate terms in the output that are equal to (-1)*Integer times the input*)
	outputlist=If[Head[output]===Plus,List@@output,{output}];
	degeneraciespositions=Flatten@Position[IntegerQ/@(-outputlist/input),True];
	number=Total[-outputlist[[degeneraciespositions]]/input];
	
	(*These additional terms are removed by symmetry, and the other terms multiplied by 1/(number+1)*)
	output=Total[Delete[outputlist,degeneraciespositions]]/(number+1)//Simplify//Expand
]/;(CovDQ@Head@der||Head@der===ParamD);


(* ::Subsection::Closed:: *)
(*ByParts*)


(* ::Input:: *)
(*?ByParts*)


SetNumberOfArguments[ByParts,2];

ByParts[expr_,list_]:=Message[ByParts::both];
ByParts[expr_,list_?IntegrationListQ]:=Message[ByParts::upPD];
ByParts[expr_,list_]/;Position[expr,PD[_?UpIndexQ]]==={}:=Message[ByParts::invlist];

ByParts[expr_,list_?IntegrationListQ]:=
Module[{newexpr,loop,term,piece,indice,tmp,newexprlist,curvatureterms,newpiece,arg,der,rest},
	newexpr=expr//ScreenDollarIndices;
	(*Loop over the terms in 'list'*)
	For[loop=1,loop<=Length[list],loop++,
		(*Expand 'newexpr' and turn this into a list of terms*)
		newexprlist=If[Head[Expand[newexpr]]===Plus,List@@newexpr,{newexpr}];
		
		(*Isolate the pieces of newexpr for later use as list[[loop]]\[Rule]{term,piece,indice}*)
		term=list[[loop,1]];
		piece=list[[loop,2]];
		indice=list[[loop,3]];

		(*Check that there is only one matching term*)
		tmp=Flatten@Position[newexprlist,term];
		If[tmp==={},Message[ByParts::nomatch,term];Abort[]];
		If[Length@tmp>2,Message[ByParts::multimatch,term];Abort[]];
		(*Make newexpr equal to everything but the term being integrated*)
		newexpr=Total@Delete[newexprlist,{First@tmp}];
		InfoPrint["newexpr = "<>ToString@newexpr,2];

		(*Find the bits of term not involving 'piece' as 'rest', accounting for the 
		case where term involves a power of a scalar 'piece'*)
		rest=term/piece;
		InfoPrint["rest = "<>ToString@rest,2];

		(*Find 'der' as the name of the derivative to be moved*)
		piece/.A_[indice]@_:>(der=A[indice]);
		InfoPrint["der = "<>ToString@der,2];
		
		(*If we are moving a covariant derivative then commuting this to the outside will
		in general produce a list of curvature terms*)
		tmp=CovDToTop[piece,Head@der,indice];
		InfoPrint["tmp = "<>ToString@tmp,2];
		curvatureterms=Last@tmp; (*this is the list of curvature terms, which may be an empty list*)
		InfoPrint["curvatureterms = "<>ToString@curvatureterms,2];
		newpiece=First@tmp; (*this is the single term without any extra curvature factors*)
		InfoPrint["newpiece = "<>ToString@newpiece,2];
		
		(*Integrate newpiece by parts and add to newexpr*)
		tmp=InverseComposition[newpiece];
		InfoPrint["tmp = "<>ToString@tmp,2];
		arg=Composition[Sequence@@DeleteCases[First[tmp],der,{1},1]]@Last[tmp];
		InfoPrint["arg = "<>ToString@arg,2];
		newexpr=newexpr+ByPartsKernel[rest,arg,der];
		(*add the other curvature terms to newexpr*)
		newexpr=newexpr+Total[(rest*#)&/@curvatureterms];

		(*Simplify and output*)
		newexpr=newexpr//Simplification//NoScalar//Expand//ScreenDollarIndices;
		newexpr=AfterByParts[newexpr]//Expand;
		];
	newexpr
]/;Position[expr,PD[_?UpIndexQ]]==={};


(* ::Section::Closed:: *)
(*Finish Up*)


(* ::Text:: *)
(*End the private context*)


End[]


(* ::Text:: *)
(*Protect symbols*)


Protect[IntegrationListQ];
Protect[InverseComposition];
Protect[CovDToTop];
Protect[ByParts];


(* ::Text:: *)
(*End package*)


EndPackage[]
