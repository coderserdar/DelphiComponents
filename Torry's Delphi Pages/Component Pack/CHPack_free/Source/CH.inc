{ 

 Def_Win16         : Compilation target is 16-bit Windows
 Def_Win32         : Compilation target is 32-bit Windows

 Def_Delphi4       : To be compiled with Delphi 4.0
 Def_Delphi5       : To be compiled with Delphi 5.0
 Def_Delphi6       : To be compiled with Delphi 6.0
 Def_Delphi7	   : To be compiled with Delphi 7.0

}

{$B-}


{$IFDEF WIN32}
  {$DEFINE Def_Win32}
{$ELSE}
  {$DEFINE Def_Win16}
{$ENDIF}


{$IFDEF VER150}
  {$DEFINE Def_Delphi7}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE Def_Delphi6}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE Def_Delphi5}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE Def_Delphi4}
{$ENDIF}

