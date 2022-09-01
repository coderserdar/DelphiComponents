{$IFDEF CLR}
  {$UNSAFECODE ON}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE Delphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
{$ENDIF}

{$IFDEF VER160}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
  {$DEFINE Delphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
  {$DEFINE Delphi2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
  {$DEFINE Delphi2007}
  {$DEFINE Delphi2009}
{$ENDIF}

{$IFDEF VER210}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
  {$DEFINE Delphi2007}
  {$DEFINE Delphi2009}
  {$DEFINE Delphi2010}
{$ENDIF}

{$IFDEF VER220}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
  {$DEFINE Delphi2007}
  {$DEFINE Delphi2009}
  {$DEFINE Delphi2010}
  {$DEFINE DelphiXE}
{$ENDIF}

{$IFDEF VER230}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi8}
  {$DEFINE Delphi2005}
  {$DEFINE Delphi2006}
  {$DEFINE Delphi2007}
  {$DEFINE Delphi2009}
  {$DEFINE Delphi2010}
  {$DEFINE DelphiXE}
  {$DEFINE DelphiXE2}
{$ENDIF}


{$IFDEF Delphi6}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF Delphi7}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}
