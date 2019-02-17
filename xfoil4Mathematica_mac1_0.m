(* ::Package:: *)

BeginPackage["xfoilForMathematica`"]
	Xfoil::usage="Xfoil[input_] Returns {results}";
	XfoilHelp::usage="XfoilHelp[],\:8fd4\:56deXfoil\:7684\:4f7f\:7528\:8bf4\:660e";
Begin["`Private`"]
	Xfoil[input_]:=
	(
		(*change "wd" to your own directory!!!*)
		wd=NotebookDirectory[];
		If[FindFile[(wd<>"/xfoilForMathematica1_0.m")]==$Failed,
		Print[
"  First try? Please check the file name and 
the variable \"wd\" in the Function Xfoil[] 
in xfoilForMathematica.m file. Now the variable 
\"wd\" refers to:"];
		Print[Print[ToString[StringForm["``",wd]]];];
		Print["It should be the same with the directory of your current folder.\nFor more information please input\"XfoilHelp[]\""];
		Abort[];
		];
	(*Check the input data*)
		If[
			Length[input]>3 && Length[input]<6,
			1,
			Print["Please check your input!"];
			Abort[];
		];
		
	(*set the default values*)
		coord=If[input[[1]]==Null,"NACA0012",,input[[1]]];
		alpha=If[input[[2]]==Null,{0},,input[[2]]];
		re=If[input[[3]]==Null,1000000,,input[[3]]];
		mach=If[input[[4]]==Null,0.2,,input[[4]]];
		Print[{coord,alpha,re,mach}];
		If[Length[input]>4,
				If[input[[5]]==Null,"",,varargin={input[[5]]};];
				Print["The extra commands is: "];
				Print[varargin];
			,
				varargin="";
		];
		If[re<1000,Print["The Reynolds number must be more than 1000!"];Abort[];];
	(*default foil name*)
		foilName = "xfoil";
		
	(*default filenames*)
		fname = foilName;
		fileCoord = wd <> "/" <> foilName <> ".foil";
		
	(*Save coordinates*)
		If[StringQ[coord],(*Either a NACA string or a filename*)
			If[StringContainsQ[coord,"NACA",IgnoreCase -> True], (*Check if a NACA string*)
				foilName = coord;
			,(*Filename supplied*)
				fileCoord = coord;(*set coord file*)
			];
		,
			(*Write foil ordinate file*)
			If[FindFile[fileCoord]!=$Failed,DeleteFile[fileCoord];];
			fid = OpenWrite[fileCoord];
			If[FailureQ[fid],
				Print[ToString[StringForm["The file: `` was not created!",fileCoord]]];
				Abort[];
			,
				WriteString[fid,ToString[StringForm["``\n",foilName]]];
				WriteString[fid,ToString[StringForm["`1`   `1`\n",NumberForm[N[coord],{9,5}]]]];
				Close[fid];
			];
		];
		
	(*Write xfoil command file*)
		fid = OpenWrite[(wd<>"/"<>fname<>".inp")];
		If[FailureQ[fid],
			Print["The file: xfoil.inp was not created!"];
			Abort[];
		,
			If[StringQ[coord],
				If[StringContainsQ[coord,"NACA",IgnoreCase -> True], (*NACA string supplied*)
					WriteString[fid,("naca "<>StringTake[coord,{5,-1}]<>"\n")];
				,(*Filename supplied*)
					WriteString[fid,("load "<>fileCoord<>"\n")];
				];
			,(*Coordinates supplied, use the default filename*)
				WriteString[fid,("load "<>fileCoord<>"\n")];
			]
		];
		(*Extra Xfoil commands*)
		For[ii=1,ii<=Length[varargin],ii++,
			txt=varargin[[ii]];
			txt=StringReplace[txt,{" ","/","/"}->"\n"];
			WriteString[fid,(txt<>"\n\n")];
		];
		WriteString[fid,"\n\noper\n"];
		(*set Remolds and Mach*)
		WriteString[fid,ToString[StringForm["re ``\n",re]]];
		WriteString[fid,ToString[StringForm["mach ``\n",mach]]];
		(*Switch to viscous mode*)
		If[re>0,WriteString[fid,"visc\n"];];
		(*Polar accumulation*)
		WriteString[fid,"pacc\n\n\n"];
		(*Xfoil alpha calculations*)
		fileDump={};
		fileCpwr={};
		For[ii=1,ii<=Length[alpha],ii++,
		(*Individual output filenames*)
			fileDump=Insert[fileDump,(ToString[StringForm["``_a``_dump.dat",fname,NumberForm[alpha[[ii]],{6,3}]]]),-1];
			fileCpwr=Insert[fileCpwr,(ToString[StringForm["``_a``_cpwr.dat",fname,NumberForm[alpha[[ii]],{6,3}]]]),-1];
			WriteString[fid,ToString[StringForm["alfa ``\n",alpha[[ii]]]]];
			WriteString[fid,ToString[StringForm["dump ``\n",fileDump[[ii]]]]];
			WriteString[fid,ToString[StringForm["cpwr ``\n",fileCpwr[[ii]]]]];
		];
		(*Polar output filename*)
		filePwrt = ToString[StringForm["``_pwrt.dat",fname]];
		WriteString[fid,"pwrt\n"];
		WriteString[fid,ToString[StringForm["``\n",filePwrt]]];
		WriteString[fid,"plis\n"];
		(*PSAV f Write airfoil to plain coordinate file*)
		airfoil = ToString[StringForm["``.dat",coord]];
		(*airfoil = (wd<>"/"<>airfoil);*)
		If[FindFile[airfoil]!=$Failed,DeleteFile[airfoil];];
		WriteString[fid,"\nPSAV\n"];
		WriteString[fid,(airfoil<>"\n")];
		(*Close the program*)
		WriteString[fid,"\nquit\n"];
		Close[fid];
		
	(*execute xfoil*)
		inp = Import[(wd<>"/"<>fname<>".inp"),"String"];
		cmd = ("cd\n/Applications/Xfoil.app/Contents/Resources/xfoil\n"<>inp);
		(*Print[cmd];*)
		status = RunProcess[$SystemShell,All,cmd];
		If[status!=0,Print[ToString[StringForm["The Xfoil.app Exit with ``\n",status]]];Abort[];
		,Print["Sucessfully excuting xfoil!"]];
		(*clear file*)
		DeleteFile[(wd<>"/"<>fname<>".inp")];
		wd = $HomeDirectory;
		
	(*Read dump file*)
	(*#    s        x        y     Ue/Vinf    Dstar     Theta      Cf       H*)
		dump = {};
		foilS={};foilX={};foilY={};foilUeVinf={};foilDstar={};foilTheta={};foilCf={};foilH={};foilAlpha={};
		For[ii=1,ii<=Length[alpha],ii++,
			fid = (wd<>"/"<>fileDump[[ii]]);
			dump = Insert[dump,Import[fid],-1];
			DeleteFile[fid];
			(*store data*)
			wed = Length[dump[[-1,2]]];
			dump[[-1]]=Delete[dump[[-1]],1];
			dump[[-1]]=Transpose[dump[[-1]]];
			foilS = Insert[foilS,dump[[-1,1]],-1];
			foilX = Insert[foilX,dump[[-1,2]],-1];
			foilY = Insert[foilY,dump[[-1,3]],-1];
			foilUeVinf = Insert[foilUeVinf,dump[[-1,4]],-1];
			foilDstar = Insert[foilDstar,dump[[-1,5]],-1];
			foilTheta = Insert[foilTheta,dump[[-1,6]],-1];
			foilCf = Insert[foilCf,dump[[-1,7]],-1];
			If[wed>7,foilH = Insert[foilH,dump[[-1,8]],-1];,foilH = {,}];
			foilAlpha = Insert[foilAlpha,alpha[[ii]],-1];
		];
	(*Read cp file*)	
		cpwr = {};
		foilXcp={};foilCp={};
		For[ii=1,ii<=Length[alpha],ii++,
			fid = (wd<>"/"<>fileCpwr[[ii]]);
			cpwr = Insert[cpwr,Import[fid],-1];
			DeleteFile[fid];
			cpwr[[-1]]=Delete[cpwr[[-1]],1];
			cpwr[[-1]]=Transpose[cpwr[[-1]]];
			foilXcp = Insert[foilXcp,cpwr[[-1,1]],-1];
			foilCp = Insert[foilCp,cpwr[[-1,2]],-1];
		];
	(*Read polar file*)
		fid = (wd<>"/"<>filePwrt);
		pwrt = StringSplit[Import[fid,"String"],"\n"];
		DeleteFile[fid];
		(*store data*)
		polName = StringSplit[pwrt[[4]],{": ","                                       "}][[2]];
		polXtrfTop = NumberForm[ToExpression[StringSplit[pwrt[[8]],{"=","(top)","(bottom)"}][[2]]],{10,3}];
		polXtrfBottom = NumberForm[ToExpression[StringSplit[pwrt[[8]],{"=","(top)","(bottom)"}][[3]]],{10,3}];
		polMach = NumberForm[ToExpression[StringSplit[pwrt[[9]],{"=","Re","e","Ncrit"}][[2]]],{10,3}];
		temp = {ToExpression[StringSplit[pwrt[[9]],{"=","Re","e","Ncrit"}][[4]]],ToExpression[StringSplit[pwrt[[9]],{"=","Re","e","Ncrit"}][[5]]]};
		polRe = temp[[1]]*10^(temp[[2]]);
		polNcrit = NumberForm[ToExpression[StringSplit[pwrt[[9]],{"=","Re","e","Ncrit"}][[7]]],{10,3}];
		polAlpha = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[1]]],{10,3}];
		polCL = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[2]]],{10,4}];
		polCD = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[3]]],{10,5}];
		polCDp = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[4]]],{10,5}];
		polCm = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[5]]],{10,4}];
		polTopXtr = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[6]]],{10,4}];
		polBotXtr = NumberForm[ToExpression[StringSplit[pwrt[[13]]][[7]]],{10,4}];
	(*read the airfoil*)
		results3 = Transpose[Import[(wd<>"/"<>airfoil)]];
		DeleteFile[(wd<>"/"<>airfoil)];
	(*return the results!*)
		results1 = {
					polName,
					polXtrfTop,
					polXtrfBottom,
					polMach,
					polRe,
					polNcrit,
					polAlpha,
					polCL,
					polCD,
					polCDp,
					polCm,
					polTopXtr,
					polBotXtr
					};
		results2 = { 
					foilS,
					foilX,
					foilY,
					foilUeVinf,
					foilDstar,
					foilTheta,
					foilCf,
					foilH,
					foilAlpha,
					foilXcp,
					foilCp
					};
		results = {results1,results2,results3};
	(*remind*)
		Print[
"\:6210\:529f\:5b8c\:6210\:ff01\:ff01\:ff01\:ff08The program has successfully finished!!!\:ff09
\:7a0b\:5e8f\:5c06\:8f93\:51fa\:4e00\:4e2a\:5927\:578b\:6570\:7ec4results\:ff0c\:5305\:62ec:
					{results1,results2,results3}.
\:5176\:4e2d\:ff1a
	results1\:6570\:7ec4\:5305\:542b\:4e86\:8fd9\:4e9b\:5185\:5bb9\:ff1a\:ff08\:5047\:8bbe\:7528result\:6570\:7ec4\:6765\:63a5\:6536\:8fd4\:56de\:503c\:ff09
		\:5185\:5bb9\:540d(Contain)    \:503c(Value)    \:8c03\:7528\:683c\:5f0f(Using form)"];
		Print[ToString[StringForm["\t\t\tpolName\t\t``\t\tresult[[1,1]]",polName]]];
		Print[ToString[StringForm["\t\t\tpolXtrfTop\t\t ``\t\tresult[[1,2]]",polXtrfTop]]];
		Print[ToString[StringForm["\t\t\tpolXtrfBottom\t  ``\t\tresult[[1,3]]",polXtrfBottom]]];
		Print[ToString[StringForm["\t\t\tpolMach\t\t    ``\t\tresult[[1,4]]",polMach]]];
		Print[ToString[StringForm["\t\t\tpolRe\t\t\t``\t\tresult[[1,5]]",IntegerPart[polRe]]]];
		Print[ToString[StringForm["\t\t\tpolNcrit\t\t   ``\t\tresult[[1,6]]",polNcrit]]];
		Print[ToString[StringForm["\t\t\tpolAlpha\t\t ``\t\tresult[[1,7]]",polAlpha]]];
		Print[ToString[StringForm["\t\t\tpolCL\t\t    ``\t\tresult[[1,8]]",polCL]]];
		Print[ToString[StringForm["\t\t\tpolCD\t\t    ``\t\tresult[[1,9]]",polCD]]];
		Print[ToString[StringForm["\t\t\tpolCDp\t\t   ``\t\tresult[[1,10]]",polCDp]]];
		Print[ToString[StringForm["\t\t\tpolCm\t\t    ``\t\tresult[[1,11]]",polCm]]];
		Print[ToString[StringForm["\t\t\tpolTopXtr\t\t ``\t\tresult[[1,12]]",polTopXtr]]];
		Print[ToString[StringForm["\t\t\tpolBotXtr\t\t ``\t\tresult[[1,13]]",polBotXtr]]];
		Print[
"
	\:800cresults2\:6570\:7ec4\:5305\:542b\:4e86\:8fd9\:4e9b\:5185\:5bb9\:ff1a\:ff08\:5047\:8bbe\:7528result\:6570\:7ec4\:6765\:63a5\:6536\:8fd4\:56de\:503c\:ff09
		\:5185\:5bb9\:540d(Contain)    \:5c3a\:5bf8(Size)    \:8c03\:7528\:683c\:5f0f(Using form)
					       \:ff08\:5217x\:884c\:ff09"];
		Print[ToString[StringForm["\t\t\tfoilS\t\t\t  ``*``\t\tresult[[2,1]]",Length[foilS],Length[foilS[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilX\t\t\t  ``*``\t\tresult[[2,2]]",Length[foilX],Length[foilX[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilY\t\t\t  ``*``\t\tresult[[2,3]]",Length[foilY],Length[foilY[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilUeVinf\t     ``*``\t\tresult[[2,4]]",Length[foilUeVinf],Length[foilUeVinf[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilDstar\t\t  ``*``\t\tresult[[2,5]]",Length[foilDstar],Length[foilDstar[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilTheta\t\t  ``*``\t\tresult[[2,6]]",Length[foilTheta],Length[foilTheta[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilCf\t\t     ``*``\t\tresult[[2,7]]",Length[foilCf],Length[foilCf[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilH\t\t\t    ``*``\t\tresult[[2,8]]",Length[foilH],Length[foilH[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilAlpha\t\t    ``*``\t\tresult[[2,9]]",Length[foilAlpha],Length[foilAlpha[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilXcp\t\t    ``*``\t\tresult[[2,10]]",Length[foilXcp],Length[foilXcp[[1]]]]]];
		Print[ToString[StringForm["\t\t\tfoilCp\t\t     ``*``\t\tresult[[2,11]]",Length[foilCp],Length[foilCp[[1]]]]]];
		Print[ToString[StringForm["\:4ee5\:4e0a\:6570\:636e\:662f\:8fce\:89d2\:4e3a``\:7684\:6570\:636e",foilAlpha]]];
		Print[
"
	\:800cresults3\:6570\:7ec4\:662f\:7ffc\:578b\:5750\:6807\:ff1a\:ff08\:5047\:8bbe\:7528result\:6570\:7ec4\:6765\:63a5\:6536\:8fd4\:56de\:503c\:ff09
		\:5185\:5bb9\:540d(Contain)    \:5927\:5c0f(Size)    \:8c03\:7528\:683c\:5f0f(Using form)"];
		Print["        "ListLinePlot[Transpose[{results[[3,1]],results[[3,2]]}],AspectRatio->Automatic,ImageSize->Medium,Axes->False,ColorFunction->Black]];
		Print[ToString[StringForm["\t\t\tairfoilX\t\t\t  ``\t\tresult[[3,1]]",Length[results[[3,1]]]]]];
		Print[ToString[StringForm["\t\t\tairfoilY\t\t\t  ``\t\tresult[[3,2]]",Length[results[[3,1]]]]]];
	(*Never The End*)
		Return[results];
	);
	
	XfoilHelp[]:=
	(
		Print[
"      Xfoil\:51fd\:6570\:662f\:4e00\:4e2a\:7528\:6765\:8c03\:7528xfoilP4.exe\:7a0b\:5e8f\:ff0c\:5e76\:8fd4\:56de\:7ffc\:578b\:6570\:636e\:7684\:51fd\:6570\:ff0c\:800c\:8be5\:7a0b\:5e8f\:4e0e\:8be5.m\:6587\:4ef6\:4ee5\:53ca.nb\:6587\:4ef6\:6700\:597d\:653e\:5728\:4e00\:4e2a\:6587\:4ef6\:5939\:4e2d\:ff0c\:4ee5\:4f9b\:8c03\:7528\:3002\:5728\:8fd9\:4e2a.m\:6587\:4ef6\:7684Xfoil\:51fd\:6570\:6e90\:7801\:4e2d\:ff0c\:6709\:4e00\:4e2awd\:53d8\:91cf\:9700\:8981\:5f53\:524d.m\:6587\:4ef6\:4ee5\:53ca.nb\:6587\:4ef6\:7b49\:7684\:6587\:4ef6\:8def\:5f84\:76f8\:540c

      Xfoil\:51fd\:6570\:7684\:8f93\:5165\:662f\:4e00\:4e2a\:6570\:7ec4\:ff0c\:5b83\:7684\:957f\:5ea6\:4e3a4\:62165\:ff0c\:89c4\:5219\:5982\:4e0b\:ff1a
	   \:7b2c\:4e00\:9879\:662f\:4e00\:4e2a\:5b57\:7b26\:4e32\:ff0c\:8868\:793a\:7ffc\:578b\:540d\:79f0\:6216\:4f60\:7684\:7ffc\:578b\:5750\:6807\:6587\:4ef6\:76ee\:5f55\:ff08\:8fd9\:4e2a\:6587\:4ef6\:4e5f\:8981\:653e\:5728\:540c\:4e00\:6587\:4ef6\:5939\:4e2d\:ff09
	   \:7b2c\:4e8c\:9879\:662f\:4e00\:4e2a\:6570\:7ec4\:ff0c\:5373{n1,n2,\[Ellipsis]},\:8868\:793a\:4f60\:8981\:8ba1\:7b97\:7684\:8fce\:89d2\:5217\:8868\:ff08\:5373\:4f7f\:53ea\:6709\:4e00\:4e2a\:6570\:636e\:4e5f\:8981\:52a0{}\:ff09\:5f53\:7136\:ff0c\:4f60\:4e5f\:53ef\:4ee5\:4f7f\:7528\:5176\:4ed6\:65b9\:5f0f\:5b9a\:4e49\:7684\:6570\:7ec4\:ff08\:4e00\:7ef4\:77e9\:9635\:ff09
	   \:7b2c\:4e09\:9879\:662f\:4e00\:4e2a\:6570\:5b57\:ff0c\:8868\:793a\:96f7\:8bfa\:6570\:ff0c\:8fd9\:4e2a\:503c\:4e0d\:80fd\:5c0f\:4e8e1000.
	   \:7b2c\:56db\:9879\:662f\:4e00\:4e2a\:6570\:5b57\:ff0c\:8868\:793a\:9a6c\:8d6b\:6570.
	   \:7b2c\:4e94\:9879\:662f\:4e00\:4e2a\:5b57\:7b26\:4e32\:ff08\:4e5f\:53ef\:4ee5\:6ca1\:6709\:7b2c\:4e94\:9879\:ff09\:ff0c\:8868\:793a\:8c03\:7528xfoilP4.exe\:65f6\:6240\:9700\:7684\:989d\:5916\:547d\:4ee4\:ff0c\:547d\:4ee4\:4e4b\:95f4\:7528\:7a7a\:683c\:9694\:5f00\:3002

	  xfoil\:51fd\:6570\:4f1a\:8fd4\:56de\:4e00\:4e2a\:5927\:578b\:6570\:7ec4\:ff0c\:5982\:679c\:7a0b\:5e8f\:6b63\:786e\:8fd0\:884c\:ff0c\:8fd0\:884c\:5b8c\:6bd5\:540e\:4f1a\:6709\:8fd4\:56de\:503c\:8bf4\:660e\:3002\:5efa\:8bae\:7528\:8be5\:51fd\:6570\:4e3a\:4e00\:4e2a\:53d8\:91cf\:8d4b\:503c\:ff0c\:65b9\:4fbf\:540e\:7eed\:8c03\:7528\:3002\:4ee5\:4e0b\:5b9e\:4f8b\:4e2d\:9ed8\:8ba4\:4f7f\:7528result\:53d8\:91cf\:6765\:5b58\:50a8\:8fd4\:56de\:503c\:3002

	  \:73b0\:6709\:4e0b\:5217\:8f93\:5165\:5b9e\:4f8b\:ff1a
result = Xfoil[{\"NACA0012\",{10},1000000,0.03}];
result = Xfoil[{\"NACA0012\",{-10,10},1000000,0.03,\"panels n 330\"}];"
		];
	);
End[]
EndPackage[]



