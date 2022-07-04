{Conditional defines that affect compilation}

{General define indicating use under C++ Builder}
{$IFDEF VER93}
  {$DEFINE CBuilder}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE CBuilder}
  {For BCB 3.0 package support.}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125}                                                      
  {$DEFINE CBuilder}
  {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER130}                                                      
  {$IFDEF BCB}
    {$DEFINE CBuilder}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFNDEF VER80}
 {$IFNDEF VER90}
  {$IFNDEF VER93}
    {$DEFINE VERSION3} { Delphi 3.0 or BCB 3.0 or higher }            
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VERSION3}
 {$IFNDEF VER100}     {Delphi 3}
  {$IFNDEF VER110}    {BCB 3}
   {$DEFINE VERSION4} {Delphi 4.0 or higher}                         
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER130}               { Delphi 5 & C++ Builder 5 }
  {$DEFINE VERSION5}       { Version 5 or greater }
 {$IFDEF BCB}                   { C++ Builder 5 }
  {$DEFINE CBuilder}
  {$ObjExportAll On}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER140}               { Delphi 6 & Bcb 6 }
  {$DEFINE VERSION6}       { Version 6 or greater }
 {$IFDEF BCB}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER150}               { Delphi 7 }
  {$DEFINE VERSION6}       { Version 6 or greater }
  {$DEFINE VERSION7}       { Version 7 or greater }  
 {$IFDEF BCB}
  {$DEFINE CBuilder}
  {$ObjExportAll On}
 {$ENDIF}
{$ENDIF}

{$IFDEF CBuilder)
  {$A+} {Word Align Data}
{$ELSE}
  {$IFDEF Win32}
    {$A+} {Word Align Data}
  {$ELSE}
    {$A-} {Byte Align Data}
  {$ENDIF}
{$ENDIF}

{$Q-} {Overflow Checking}
{$R-} {Range-Checking}
{$S-} {Stack-Overflow Checking}
{$V-} {Var-String Checking}

{$IFNDEF Win32}
  {$F-} {Force Far Calls}
  {$K-} {Smart Callbacks}
{$ENDIF}
