Bin - design & runtime packages
Lib60 - compiled source for Delphi6  
Lib70 - compiled source for Delphi7
Src - not all source, for help only,but you can recompile.
Help- fds.chm
Demo- Demo program


Plans.
  Recompile help file.
  Fix TDimPager 

Use www.fxds.com/downloads.php for lates versions of Decision Cube.
Sorry for web design, but I must fill web site with info, then my brother Alex
can made nice design. Fds will free. Only source for FxBin,FxCache,FxMap,FxArray
will accessed for some fee.


10.02.2003
  Fix some bugs in FxExpr.

6.02.2003
  Added "TDecisionCube.OnCall" Event and property "TDecisionCube.Externals".
  I have write FxExpr untit. It implement complex expression with built-in functions
  Exp,Sqr,Sqrt,Ln,... and user-defined functions (see demo).
  I have upgrade Delphi 6 with Rtl2 update. 

27.01.2003
  I have remove dependencies of FxArray,FxBin,FxCache,FxMap from FxConsts unit.
  Now if you change FxConsts source you can recompile packages.
  TFxHtml class no more derived from CustomProducer.

22.01.2003
  drc files contain up-to-date information. You can use it for resource string
  translation. (Delphi help - Adding languages to project).
  Fix bugs. (Access violation when removing FxSource, ...).
  
07.01.2003
  Find return wrong Index when array have null values. Fixed.
  TInt64 array used LongInt instead Int64. Fixed.
  TFxGrid don't show plus or minus when next dim is drilled. Fixed.

01.01.2003
  Fixed some bugs in FxMap unit.
    (Exception in design-time when DecisionCube.Active=True and DataModule closing)

  Note: If you want change more then one property of TFxMapItem,
    then call TFxMapItems.BeginUpdate & TFxMapItems.EndUpdate.

  Attention!!!. Expressions in TFxMapItem.Expr use TFxMapItem.Name,
  not TFxMapItem.FieldName now!!!
  Expr  ::= ['-'] Factor { '+'|'-' Factor}
  Factor::= Term { '*'|'/' Term}
  Term  ::= IDENTIFIER|NUMBER| '(' Expr ')'
  IDENTIFIER - TFxMapItem.Name. Note: TFxMapItem must be Summary.

  I have remove DataSet Field check exception.
  If TFxMapItem can't find field in DataSet with FieldName then no more exception.
  This TFxMapItem is not activated.
  You can define some set of TFxMapItem's (dictionary) and then use some modifictions of SQL.

  Added property TFxMapItem.Tag

  TFmtBCD field must work now.
  Note: Decision Cube convert TFmtBCDField.Value to Currency type.
  

22.12.2002
  Add Export to Html - TFxHtml
  Added suport Null values in dimensions.
  Added property TFxMapItem.Visible:Boolean default True; This propert affect to Summaries only. 

7.12.2002
  Added unary '-' in expressions.
  Add TFxMapItem.Caption property!!!!!! You must initialize it.
  Fixed bug in TFmtBCD Field, it now must work.
  Implemented dimAverage 


28.11.2002
  Now Summaries support Null fields.
  Max,Min worked.
  All design-time components have been renamed.
  Rewritten core datacache algorithm.


01.07.2002(331)
Fix dimCount for non integer fields. Note: dimCount count not null fields in dataset, not in database
Fix WideString dimension type
New TBinTable layout algorithm
New fast fetch lookups algorithm
Add checks to lookup properties
LookupDataset auto-opened now
Removed all references to TClientDataSet. (datasnap)


16.06.2002
New super fast TBinTable. No more BDE, no more TClientDataSet.
dimCount now work. It count not null fields in dataset (see demo)
added support for TSQLTimeStampField (dbExpress)
While it not support dimAverage, but it will fixed soon.
You can use any sql for dataset, not only "...group by".
You can use any Field.FieldKind, not only fkData.
Fixed Bugs:
  Access Violation when removing TDecisionCube
Added help file.
Added comments to Demo.

03.06.2002
Closed:
Fixed bugs in Derived summary expression
More checks on TFxMap properties
Important - from this release:
  DecisionCube.Active property is implemented and
  this property replace DataSet.Open/Close logic.
  (see demo)

    

26.05.2002
Closed:
Units FxArray,FxCache,FxDb,FxStore,FxMap no more depend from FxConsts.
You can change source and recompile.
TDecisionCube.Refresh - deprecated;
TFxMapItem. (Alignment,BinType,Format,Name,Width) work in design mode
and in runtime you don't need call DecisionCube. Refresh to change this property.
See Demo.
  
26.05.2002 Fix memory leak in TFxMap.
26.05.2002 Fix Dimension Load Logic:
  DecisionCube. (MaxCells,MaxDimensions,MaxSummaries)
  TFxMapItem. (Active,ActiveFlag)
Load Logic - 
  1.Load all items with ActiveFlag=diActive
  2.Load all Summaries  with ActiveFlag=diAsNeeded and Active=True;
  3.Load all Dimensions with ActiveFlag=diAsNeeded and Active=True;
  4.Load all Summaries  with ActiveFlag=diAsNeeded and Active=False;
  5.Load all Dimensions with ActiveFlag=diAsNeeded and Active=False;
  Load stopped when Dims.Range*Sums>MaxCells, Dims>MaxDimensions,Sums>MaxSummaries
  Play in Demo with MaxCells.

15.05.2002 Fix Bug "Empty DataSet"
15.05.2002 Fix Bug in Domain Algorithm
15.05.2002 Fix Bug in Scan data values 
10.05.2002 Sort on Lookup Dimensions



What don't work:
binType - binSet


263

  