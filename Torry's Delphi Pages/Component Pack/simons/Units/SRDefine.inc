{ Include File für Simons Komponenten. Enthält Compiler-Direktiven für die
  verschiedenen Delphi-Versionen und Zielplattformen

  Include File for Simons components. Provides some compiler directives for
  the different Delphi versions and target platforms

  p.i.c.s.
  http://www.picsoft.de
  Simon Reinhardt
  reinhardt@picsoft.de                                                         }

{ A brief explanation of the defines in this unit

 SRDefines             : Always defined when this unit is included

 SR_Linux              : Compilation target is Linux
 SR_Windows            : Compilation target is Windows
 SR_Win16              : Compilation target is 16-bit Windows
 SR_Win32              : Compilation target is 32-bit Windows

 SR_Delphi             : To be compiled with Delphi
 SR_Delphi32           : To be compiled with 32-Bit-Delphi
 SR_Delphi1            : To be compiled with Delphi 1.0
 SR_Delphi2            : To be compiled with Delphi 2.0
 SR_Delphi3            : To be compiled with Delphi 3.0
 SR_Delphi4            : To be compiled with Delphi 4.0
 SR_Delphi5            : To be compiled with Delphi 5.0
 SR_Delphi6            : To be compiled with Delphi 6.0
 SR_Delphi7            : To be compiled with Delphi 7.0
 SR_Delphi8            : To be compiled with Delphi 7.0
 SR_Delphi9            : To be compiled with Delphi 7.0
 SR_Delphi2_Up         : To be compiled with Delphi 2.0 or higher
 SR_Delphi3_Up         : To be compiled with Delphi 3.0 or higher
 SR_Delphi4_Up         : To be compiled with Delphi 4.0 or higher
 SR_Delphi5_Up         : To be compiled with Delphi 5.0 or higher
 SR_Delphi6_Up         : To be compiled with Delphi 6.0 or higher
 SR_Delphi7_Up         : To be compiled with Delphi 7.0 or higher
 SR_Delphi8_Up         : To be compiled with Delphi 8.0 or higher
 SR_Delphi9_Up         : To be compiled with Delphi 2005 or higher

 SR_CBuilder           : To be compiled with C++Builder
 SR_CBuilder1          : To be compiled with C++Builder 1.0
 SR_CBuilder2          : To be compiled with C++Builder 2.0
 SR_CBuilder3          : To be compiled with C++Builder 3.0
 SR_CBuilder4          : To be compiled with C++Builder 4.0
 SR_CBuilder2_Up       : To be compiled with C++Builder 2.0 or higher
 SR_CBuilder3_Up       : To be compiled with C++Builder 3.0 or higher
 SR_CBuilder4_Up       : To be compiled with C++Builder 4.0 or higher

 SR_Kylix              : To be compiled with Kylix                             }

{$B-}

{$DEFINE SRDefines}

{$IFDEF WINDOWS}
  {$DEFINE SR_Win16}
  {$DEFINE SR_Windows}
{$ENDIF}
{$IFDEF WIN32}
  {$DEFINE SR_Win32}
  {$DEFINE SR_Windows}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE SR_Linux}
{$ENDIF}

{$IFDEF VER170}
  {$IFDEF SR_Linux}
    {$DEFINE SR_Kylix}
  {$ENDIF}
  {$IFDEF SR_Windows}
    {$DEFINE SR_Delphi9}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER160}
  {$IFDEF SR_Linux}
    {$DEFINE SR_Kylix}
  {$ENDIF}
  {$IFDEF SR_Windows}
    {$DEFINE SR_Delphi8}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$IFDEF SR_Linux}
    {$DEFINE SR_Kylix}
  {$ENDIF}
  {$IFDEF SR_Windows}
    {$DEFINE SR_Delphi7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$IFDEF SR_Linux}
    {$DEFINE SR_Kylix}
  {$ENDIF}
  {$IFDEF SR_Windows}
    {$DEFINE SR_Delphi6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER135}
  {$DEFINE SR_CBuilder5}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SR_Delphi5}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SR_CBuilder4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SR_Delphi4}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SR_CBuilder3}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE SR_Delphi3}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE SR_Delphi2}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE SR_CBuilder1}
{$ENDIF}

{$IFDEF VER80}
  {$DEFINE SR_Delphi1}
{$ENDIF}

{$IFDEF SR_Delphi1}
  {$DEFINE SR_Delphi}
{$ENDIF}

{$IFDEF SR_Delphi2}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
{$ENDIF}

{$IFDEF SR_Delphi3}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
{$ENDIF}

{$IFDEF SR_Delphi4}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
{$ENDIF}

{$IFDEF SR_Delphi5}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
  {$DEFINE SR_Delphi5_Up}
{$ENDIF}

{$IFDEF SR_Delphi6}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
  {$DEFINE SR_Delphi5_Up}
  {$DEFINE SR_Delphi6_Up}
{$ENDIF}

{$IFDEF SR_Delphi7}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
  {$DEFINE SR_Delphi5_Up}
  {$DEFINE SR_Delphi6_Up}
  {$DEFINE SR_Delphi7_Up}
{$ENDIF}

{$IFDEF SR_Delphi8}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
  {$DEFINE SR_Delphi5_Up}
  {$DEFINE SR_Delphi6_Up}
  {$DEFINE SR_Delphi7_Up}
  {$DEFINE SR_Delphi8_Up}
{$ENDIF}

{$IFDEF SR_Delphi9}
  {$DEFINE SR_Delphi}
  {$DEFINE SR_Delphi32}
  {$DEFINE SR_Delphi2_Up}
  {$DEFINE SR_Delphi3_Up}
  {$DEFINE SR_Delphi4_Up}
  {$DEFINE SR_Delphi5_Up}
  {$DEFINE SR_Delphi6_Up}
  {$DEFINE SR_Delphi7_Up}
  {$DEFINE SR_Delphi8_Up}
  {$DEFINE SR_Delphi9_Up}
{$ENDIF}

{$IFDEF SR_CBuilder1}
  {$DEFINE SR_CBuilder}
{$ENDIF}

{$IFDEF SR_CBuilder2}
  {$DEFINE SR_CBuilder}
  {$DEFINE SR_CBuilder2_Up}
{$ENDIF}

{$IFDEF SR_CBuilder3}
  {$DEFINE SR_CBuilder}
  {$DEFINE SR_CBuilder2_Up}
  {$DEFINE SR_CBuilder3_Up}
{$ENDIF}

{$IFDEF SR_CBuilder4}
  {$DEFINE SR_CBuilder}
  {$DEFINE SR_CBuilder2_Up}
  {$DEFINE SR_CBuilder3_Up}
  {$DEFINE SR_CBuilder4_Up}
{$ENDIF}
