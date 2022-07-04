{*******************************************************}
{                                                       }
{         include file inspired by the RX Lib           }
{         Copyright (c) 1996 AO ROSNO                   }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{*******************************************************}

{ Conditional defines that affect compilation }
{ ------------ !! DO NOT CHANGE DEFINES BELOW THIS LINE !! ------------ }

{$IFDEF VER93}  { Borland C++Builder 1.0 }
  {$DEFINE CBUILDER}
{$ENDIF}

{$IFDEF VER100} {Borland Delphi 3.0 }
  {$DEFINE D3}
{$ENDIF}

{$IFDEF VER110} { Borland C++Builder 3.0 }
  {$DEFINE CBUILDER}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER120} {Borland Delphi 4.0 }
  {$DEFINE D3}
  {$DEFINE D4}
{$ENDIF}

{$IFNDEF VER80}           { Delphi 1.0     }
 {$IFNDEF VER90}          { Delphi 2.0     }
  {$IFNDEF VER93}         { C++Builder 1.0 }
    {$DEFINE D3}          { Delphi 3.0 or higher }
    {$IFNDEF VER100}
      {$DEFINE V110}      { C++Builder 3.0 or higher }
      {$IFNDEF VER110}
        {$DEFINE D4}      { Delphi 4.0 or higher }
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

{$IFDEF CBUILDER}
  {$DEFINE USE_PROGRESSBAR}
  {$UNDEF USE_OLD_DBLISTS}
  {$IFNDEF VER93}
    {$DEFINE V110}        { C++Builder 3.0 or higher }
  {$ENDIF}
{$ENDIF}