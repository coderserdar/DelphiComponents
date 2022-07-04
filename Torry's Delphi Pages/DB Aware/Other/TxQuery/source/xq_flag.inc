{**********************************************}
{   Compiling flags for TxQuery                }
{   Copyright (c) 1999-2002 by Alfonso Moreno  }
{**********************************************}

{$DEFINE TESTING}             { Remove for the finished program }
//{$DEFINE XQDEMO}              { Remove for the finished program }

{ ==== Determination of compiler and others (do NOT change) ==== }
{$IFDEF VER100}      // Delphi 3
{$DEFINE LEVEL3}
{$DEFINE DELPHI3}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER110}      // C++ Builder 3
{$DEFINE LEVEL3}
{$DEFINE BCB3}
{$DEFINE BCB}
{$ENDIF}

{$IFDEF VER120}      // Delphi 4
{$DEFINE LEVEL4}
{$DEFINE DELPHI4}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER125}      // C++ Builder 4
{$DEFINE LEVEL4}
{$DEFINE BCB4}
{$DEFINE BCB}
{$ENDIF}

{$IFDEF VER130}      // Delphi 5
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE DELPHI5}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER135}      // C++ Builder 5
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE BCB5}
{$DEFINE BCB}
{$ENDIF}

{$IFDEF VER140}      // Delphi 6
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE LEVEL6}
{$DEFINE DELPHI6}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER145}      // C++ Builder 6
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE LEVEL6}
{$DEFINE BCB6}
{$DEFINE BCB}
{$ENDIF}

{$IFDEF VER150}      // Delphi 7
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE LEVEL6}
{$DEFINE LEVEL7}
{$DEFINE DELPHI7}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER180}      // Delphi 10
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE LEVEL6}
{$DEFINE LEVEL7}
{$DEFINE DELPHI7}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER200}      // Delphi 12
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE LEVEL6}
{$DEFINE LEVEL7}
{$DEFINE DELPHI7}
{$DEFINE DELPHI}
{$ENDIF}

{$IFDEF VER210}      // Delphi 14
{$DEFINE LEVEL4}
{$DEFINE LEVEL5}
{$DEFINE LEVEL6}
{$DEFINE LEVEL7}
{$DEFINE DELPHI7}
{$DEFINE DELPHI}
{$ENDIF}

//***********************************************************************

{$IFDEF BCB}
{$ObjExportAll On}
{$ASSERTIONS ON}
{$ENDIF}

{$G+}

{$A-}

{$BOOLEVAL OFF}   // no complete boolean eval
{$IFDEF TESTING}
{$ASSERTIONS ON}
{$D+} {Enables and disables the generation of debug information}
{$L+} {Enables or disables the generation of local symbol information}
{$ELSE}
{$ASSERTIONS OFF}
{$D-} {Enables and disables the generation of debug information}
{$L-} {Enables or disables the generation of local symbol information}
{$ENDIF}

{==== Global fixed compiler options (do NOT change) ====}
{---Delphi 3---}
  {$IFDEF DELPHI3}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---Delphi 4---}
  {$IFDEF DELPHI4}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---Delphi 5---}
  {$IFDEF DELPHI5}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---Delphi 6---}
  {$IFDEF DELPHI6}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---C++Builder 3---}
  {$IFDEF BCB3}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---C++Builder 4---}
  {$IFDEF BCB4}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---C++Builder 5---}
  {$IFDEF BCB5}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}
{---C++Builder 6---}
  {$IFDEF BCB6}
  {$B- Incomplete boolean evaluation}
  {$H+ Long string support}
  {$J+ Writeable typed constants}
  {$P- No open string parameters}
  {$Q- No arithmetic overflow checking}
  {$R- No range checking}
  {$T- No type-checked pointers}
  {$V- No var string checking}
  {$X+ Extended syntax}
  {$Z1 Enumerations are word sized}
  {$ENDIF}

