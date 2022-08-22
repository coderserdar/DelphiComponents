
{*******************************************************}
{                                                       }
{     TinyDB Include File                               }
{     Copyright (c) 2000-2003 DayDream Software         }
{                                                       }
{*******************************************************}

{ TinyDB.INC is included in all TinyDB units so you can specify
  global compiler options here. TinyDB.INC is included before
  each units required compiler options and the options specified
  here can be overridden by hardcode options in the source files. }

// The Version of Delphi/BCB
{$I Compilers.inc}

// Use built-in version of ZLib
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion < 23.0}
    {in Delphi XE2 is used ZLib build-in for future use with 64-bit primary}
    {$DEFINE BUILT_ZLIB}

  {$ELSE}
    {$DEFINE As64BitCompatibility}
    {$DEFINE DXE2UP}
  {$IFEND}
{$ENDIF}

// Show Nag-Screen or not
{.$DEFINE TDB_SHOW_NAGSCREEN}
