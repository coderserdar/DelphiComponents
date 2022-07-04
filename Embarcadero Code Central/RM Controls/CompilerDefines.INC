{
  This Compiler definination file was created to help describe different versions
  of the Borland Compilers.  The verXX is what Borland uses to describe each
  different version of the compiler.  With this in mind this file defines the following:

     BDx - where x is the version of delphi.  eg. BD5 would be defined for Delphi 5
     BCBx - where x is the version of C++ Builder
     DELPHI_COMPILER - is defined for all Delphi versions of the compiler
     BCB_COMPILER - is defined for all C++ Builder versions of the compiler
     Unknown_Version - is defined if no compiler version is correctly matched to a verXX
     Dx_OR_HIGHER - where x is a Delphi version number.  Eg.  D4_OR_HIGHER would be
                    defined for all versions of the compiler from D4 onwards.
     BCBx_OR_HIGHER - where x is a C++ Builder version number.  Eg.  BCB4_OR_HIGHER would be
                    defined for all versions of the compiler from BCB4 onwards.


  The following is a quick reference to the verXX and the matching Borland product:

     Ver80  - Delphi 1
     Ver90  - Delphi 2                           Ver93  - C++ Builder 1
     Ver100 - Delphi 3                           Ver110 - C++ Builder 3
     Ver120 - Delphi 4                           Ver125 - C++ Builder 4
     Ver130 - Delphi 5                           Ver135 - C++ Builder 5
     Ver140 - Delphi 6

  Starting with D2 all compilers define WIN32 for the Win32 platform.  I would expect
  that a new define will appear with the addvent of Kylix.  Something like "LINUX"?

  D1 was the only 16-bit version of Delphi released.  The was never a 16-Bit version of BCB.
}

{*$DEFINE rmDebug}

{$IFDEF WIN32}
  {$DEFINE D2_OR_HIGHER}
{$ENDIF}

{$DEFINE UNKNOWN_COMPILER}
{$UNDEF RMVERSET}

{$IFDEF VER80} //DELPHI 1
  {$DEFINE BD1}
  {$DEFINE DELPHI_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE D1_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER90} //DELPHI 2
  {$DEFINE BD2}
  {$DEFINE DELPHI_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE D2_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER93} //BCB 1
  {$DEFINE BCB1}
  {$DEFINE BCB_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE BCB1_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER100} //DELPHI 3
  {$DEFINE BD3}
  {$DEFINE DELPHI_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE D3_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER110} //BCB 3
  {$DEFINE BCB3}
  {$DEFINE BCB_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE BCB3_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER120} //DELPHI 4
  {$DEFINE BD4}
  {$DEFINE DELPHI_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE D4_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125} //BCB 4
  {$DEFINE BCB4}
  {$DEFINE BCB_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE BCB4_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130} //DELPHI 5
  {$DEFINE BD5}
  {$DEFINE DELPHI_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE D5_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER135} //BCB5
  {$DEFINE BCB5}
  {$DEFINE BCB_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE BCB5_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140} //DELPHI 6
  {$DEFINE BD6}
  {$DEFINE DELPHI_COMPILER}
  {$UNDEF UNKNOWN_COMPILER}
  {$DEFINE RMVERSET}
{$ELSE}
  {$IFNDEF RMVERSET}
    {$DEFINE D6_OR_HIGHER}
  {$ENDIF}
{$ENDIF}

{$UNDEF RMVERSET}

{$B-} //Turn off Complete Boolean Evaluation
{$D-} //Turn off the damn Debug Info
