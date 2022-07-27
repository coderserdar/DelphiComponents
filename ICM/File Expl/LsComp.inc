{======================================================================
   LsComp.inc
   ----------
      Compiler directives to be used by all Ls-Component units
      
      =======================================
      Delphi and C++ Builder Version List
      ---------------------------------------
      Delphi 1      = VER80    (Version  8.0)     
      Delphi 2      = VER90    (Version  9.0)
      C++ Builder 1 = VER93    (Version  9.3)
      Delphi 3      = VER100   (Version 10.0)
      C++ Builder 3 = VER110   (Version 11.0)   
      Delphi 4      = VER120   {Version 12.0)
      C++ Builder 4 = VER125   (Version 12.5)   
      Delphi 5 or
      C++ Builder 5 = VER130   (Version 13.0)
      Delphi 6 or
      C++ Builder 6 = VER140   (Version 14.0)
      Delphi 7      = VER150   (Version 15.0)
      ---------------------------------------

=======================================================================}

{$IFDEF WIN32}
  {$DEFINE D2_OR_HIGHER}
{$ENDIF}

{$IFNDEF VER80}  //If not D1
  {$DEFINE D2_OR_HIGHER}
  {$IFNDEF VER90}  //If not D2
    {$DEFINE BCB1_OR_HIGHER}
    {$IFNDEF VER93}  //If not BCB1
      {$DEFINE D3_OR_HIGHER}
      {$IFNDEF VER100}  //If not D3
        {$DEFINE BCB3_OR_HIGHER}
        {$IFNDEF VER110}  //IF not BCB3
          {$DEFINE D4_OR_HIGHER}
          {$IFNDEF VER120}  //If not D4
            {$DEFINE BCB4_OR_HIGHER}
            {$IFNDEF VER125}  //If not BCB4
              {$DEFINE D5_OR_HIGHER}
              {$IFNDEF VER130}  //If not D5
                {$DEFINE D6_OR_HIGHER}
                {$IFNDEF VER140}  //If not D6
                  {$DEFINE D7_OR_HIGHER}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE BCB_COMPILER}
{$ENDIF}
{$IFDEF VER110}
  {$DEFINE BCB_COMPILER}
  {$OBJEXPORTALL ON}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB_COMPILER}
  {$OBJEXPORTALL ON}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE BCB_COMPILER}    {D5 AND BCB5 use the same DCC32 compiler}
  {$OBJEXPORTALL ON}
{$ENDIF}

{== Code Generation Directives ==}

{$F-}    { Force Far Calls }
{$A+}    { Word Align Data }
{$U-}    { Pentium-Save FDIV }
{$K-}    { Smart Callbacks }
{$W-}    { Windows Stack Frame }


{== Runtime Errors ==}

{$IFOPT D+}
  {$R+}    { Range Checking - On - if compiled with Debug Information }
{$ELSE}
  {$R-}    { Range Checking - Off - if compiled without Debug Information }
{$ENDIF}
{$R-}    { Range Checking }
{$S-}    { Stack Checking }
{$I+}    { I/O Checking }
{$Q-}    { Overflow Checking }


{== Syntax Options ==}

{$V-}    { Strict Var-Strings }
{$B-}    { Complete Boolean Evaluation }
{$X+}    { Extended Syntax }
{$T-}    { Typed @ Operator }
{$P+}    { Open Parameters }
{$IFDEF D4_OR_HIGHER}
{$J+}    { Writeable Typed Constants }
{$ENDIF}

{== Miscellaneous Directives ==}

{$C MOVEABLE DEMANDLOAD DISCARDABLE}    { Code Segment Attribute }
{$G+}    { Delphi 1: 286 Instructions / Delphi 3 & later: Imported Data }
{$N+}    { Numeric Coprocessor }
{$Z-}    { Word Size Enumerated Types }

{$IFDEF WIN32}
{$H+}    { Long String Support }
{$ENDIF}

