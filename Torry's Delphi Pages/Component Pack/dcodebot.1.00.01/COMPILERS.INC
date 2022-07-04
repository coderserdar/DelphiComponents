//----------------------------------------------------------------------------------------------------------------------
// Include file to determine which compiler is currently being used to build the project/component.
// This file uses ideas from Brad Stowers DFS.inc file (www.delphifreestuff.com).
//
// Portions created by Mike Lischke are Copyright
// (C) 1999-2002 Dipl. Ing. Mike Lischke. All Rights Reserved.
//----------------------------------------------------------------------------------------------------------------------
// The following symbols are defined:
//
// D1    : Kylix/Delphi/BCB 1.x is the compiler.
// D1_UP : Kylix/Delphi/BCB 1.x or higher is the compiler.
// D2    : Kylix/Delphi 2.x or BCB 1.x is the compiler.
// D2_UP : Kylix/Delphi 2.x or higher, or BCB 1.x or higher is the compiler.
// D3    : Kylix/Delphi/BCB 3.x is the compiler.
// D3_UP : Kylix/Delphi/BCB 3.x or higher is the compiler.
// D4    : Kylix/Delphi/BCB 4.x is the compiler.
// D4_UP : Kylix/Delphi/BCB 4.x or higher is the compiler.
// D5    : Kylix/Delphi/BCB 5.x is the compiler.
// D5_UP : Kylix/Delphi/BCB 5.x or higher is the compiler.
// D6    : Kylix/Delphi/BCB 6.x is the compiler.
// D6_UP : Kylix/Delphi/BCB 6.x or higher is the compiler.
// D7    : Kylix/Delphi/BCB 7.x is the compiler.
// D7_UP : Kylix/Delphi/BCB 7.x or higher is the compiler.
//
// Only defined if Windows is the target:
// CPPB        : Any version of BCB is being used.
// CPPB_1      : BCB v1.x is being used.
// CPPB_3      : BCB v3.x is being used.
// CPPB_3_UP   : BCB v3.x or higher is being used.
// CPPB_4      : BCB v4.x is being used.
// CPPB_4_UP   : BCB v4.x or higher is being used.
// CPPB_5      : BCB v5.x is being used.
// CPPB_5_UP   : BCB v5.x or higher is being used.
//
// Only defined if Windows is the target:
// DELPHI      : Any version of Delphi is being used.
// DELPHI_1    : Delphi v1.x is being used.
// DELPHI_2    : Delphi v2.x is being used.
// DELPHI_2_UP : Delphi v2.x or higher is being used.
// DELPHI_3    : Delphi v3.x is being used.
// DELPHI_3_UP : Delphi v3.x or higher is being used.
// DELPHI_4    : Delphi v4.x is being used.
// DELPHI_4_UP : Delphi v4.x or higher is being used.
// DELPHI_5    : Delphi v5.x is being used.
// DELPHI_5_UP : Delphi v5.x or higher is being used.
// DELPHI_6    : Delphi v6.x is being used.
// DELPHI_6_UP : Delphi v6.x or higher is being used.
// DELPHI_7    : Delphi v7.x is being used.
// DELPHI_7_UP : Delphi v7.x or higher is being used.
//
// Only defined if Linux is the target:
// KYLIX       : Any version of Kylix is being used.
// KYLIX_1     : Kylix 1.x is being used.
// KYLIX_1_UP  : Kylix 1.x or higher is being used.
// KYLIX_2     : Kylix 2.x is being used.
// KYLIX_2_UP  : Kylix 2.x or higher is being used.
// KYLIX_3     : Kylix 3.x is being used.
// KYLIX_3_UP  : Kylix 3.x or higher is being used.
//
// Only defined if Linux is the target:
// QT_CLX      : Trolltech's QT library is being used.
//----------------------------------------------------------------------------------------------------------------------

{$IFDEF WIN32}
  {$IFDEF VER150}        
    {$DEFINE D7}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI_7}
  {$ENDIF}

  {$IFDEF VER140}
    {$DEFINE D6}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI_6}
  {$ENDIF}

  {$IFDEF VER130}
    {$DEFINE D5}
    {$IFDEF BCB}
      {$DEFINE CPPB}
      {$DEFINE CPPB_5}
    {$ELSE}
      {$DEFINE DELPHI}
      {$DEFINE DELPHI_5}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF VER125}
    {$DEFINE D4}
    {$DEFINE CPPB}
    {$DEFINE CPPB_4}
  {$ENDIF}

  {$IFDEF VER120}
    {$DEFINE D4}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI_4}
  {$ENDIF}

  {$IFDEF VER110}
    {$DEFINE D3}
    {$DEFINE CPPB}
    {$DEFINE CPPB_3}
  {$ENDIF}

  {$IFDEF VER100}
    {$DEFINE D3}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI_3}
  {$ENDIF}

  {$IFDEF VER93}
    {$DEFINE D2}
    {$DEFINE CPPB}
    {$DEFINE CPPB_1}
  {$ENDIF}

  {$IFDEF VER90}
    {$DEFINE D2}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI_2}
  {$ENDIF}

  {$IFDEF VER80}
    {$DEFINE D1}
    {$DEFINE DELPHI}
    {$DEFINE DELPHI_1}
  {$ENDIF}

  {$IFDEF DELPHI_2}
    {$DEFINE DELPHI_2_UP}
  {$ENDIF}

  {$IFDEF DELPHI_3}
    {$DEFINE DELPHI_2_UP}
    {$DEFINE DELPHI_3_UP}
  {$ENDIF}

  {$IFDEF DELPHI_4}
    {$DEFINE DELPHI_2_UP}
    {$DEFINE DELPHI_3_UP}
    {$DEFINE DELPHI_4_UP}
  {$ENDIF}

  {$IFDEF DELPHI_5}
    {$DEFINE DELPHI_2_UP}
    {$DEFINE DELPHI_3_UP}
    {$DEFINE DELPHI_4_UP}
    {$DEFINE DELPHI_5_UP}
  {$ENDIF}

  {$IFDEF DELPHI_6}
    {$DEFINE DELPHI_2_UP}
    {$DEFINE DELPHI_3_UP}
    {$DEFINE DELPHI_4_UP}
    {$DEFINE DELPHI_5_UP}
    {$DEFINE DELPHI_6_UP}
  {$ENDIF}

  {$IFDEF DELPHI_7}
    {$DEFINE DELPHI_2_UP}
    {$DEFINE DELPHI_3_UP}
    {$DEFINE DELPHI_4_UP}
    {$DEFINE DELPHI_5_UP}
    {$DEFINE DELPHI_6_UP}
    {$DEFINE DELPHI_7_UP}
  {$ENDIF}

  {$IFDEF CPPB_3}
    {$DEFINE CPPB_3_UP}
  {$ENDIF}

  {$IFDEF CPPB_4}
    {$DEFINE CPPB_3_UP}
    {$DEFINE CPPB_4_UP}
  {$ENDIF}

  {$IFDEF CPPB_5}
    {$DEFINE CPPB_3_UP}
    {$DEFINE CPPB_4_UP}
    {$DEFINE CPPB_5_UP}
  {$ENDIF}

  {$IFDEF CPPB_3_UP}
    {$OBJEXPORTALL ON}
  {$ENDIF}

{$ELSE (NOT WINDOWS)}
  {$DEFINE QT_CLX}

  {$DEFINE KYLIX}
  {$DEFINE KYLIX_1}
  {$DEFINE KYLIX_1_UP}

  {$IFDEF VER150}
    {$DEFINE D7}
    {$DEFINE KYLIX_3}
  {$ENDIF}

  {$IFDEF VER140}
    {$DEFINE D6}
    {$DEFINE KYLIX_2}
  {$ENDIF}

  {$IFDEF KYLIX_2}
    {$DEFINE KYLIX_2_UP}
  {$ENDIF}

  {$IFDEF KYLIX_3}
    {$DEFINE KYLIX_2_UP}
    {$DEFINE KYLIX_3_UP}
  {$ENDIF}

{$ENDIF}

// COMPILER DEFINES COMMON TO ALL PLATFORMS.
{$IFDEF D1}
  {$DEFINE D1_UP}
{$ENDIF}

{$IFDEF D2}
  {$DEFINE D1_UP}
  {$DEFINE D2_UP}
{$ENDIF}

{$IFDEF D3}
  {$DEFINE D1_UP}
  {$DEFINE D2_UP}
  {$DEFINE D3_UP}
{$ENDIF}

{$IFDEF D4}
  {$DEFINE D1_UP}
  {$DEFINE D2_UP}
  {$DEFINE D3_UP}
  {$DEFINE D4_UP}
{$ENDIF}

{$IFDEF D5}
  {$DEFINE D1_UP}
  {$DEFINE D2_UP}
  {$DEFINE D3_UP}
  {$DEFINE D4_UP}
  {$DEFINE D5_UP}
{$ENDIF}

{$IFDEF D6}
  {$DEFINE D1_UP}
  {$DEFINE D2_UP}
  {$DEFINE D3_UP}
  {$DEFINE D4_UP}
  {$DEFINE D5_UP}
  {$DEFINE D6_UP}
{$ENDIF}

{$IFDEF D7}
  {$DEFINE D1_UP}
  {$DEFINE D2_UP}
  {$DEFINE D3_UP}
  {$DEFINE D4_UP}
  {$DEFINE D5_UP}
  {$DEFINE D6_UP}
  {$DEFINE D7_UP}
{$ENDIF}

