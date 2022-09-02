{$OPTIMIZATION ON}
{$ALIGN ON}
{$STACKFRAMES OFF}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$TYPEDADDRESS OFF}
{$OPENSTRINGS ON}
{$LONGSTRINGS ON}
{$WRITEABLECONST ON}
{$ASSERTIONS OFF}

{$IFDEF VER140}
  {$DEFINE D6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE D7}
{$ENDIF}

{$IFDEF VER160}
  {$DEFINE DNET}
{$ENDIF}

{$IFDEF DNET}
  {$DEFINE D7}
{$ENDIF}

{$TYPEINFO ON}

//{$IFDEF VER210} //Rad Studio 2010
  {$DEFINE D7}
  {$DEFINE D2009}
  {$DEFINE D2010}
//{$ENDIF}

{$IFDEF VER220} //Rad Studio XE
  {$DEFINE D7}
  {$DEFINE D2009}
  {$DEFINE D2010}
  {$DEFINE XE}
{$ENDIF}

{$IFDEF D7}
  {$DEFINE D6}
{$ENDIF}

{$IFDEF D2009}
   {$POINTERMATH ON}
   //{$M+}
   //{$TYPEINFO ON}
{$ENDIF}

{$IFDEF D7}
{$IFNDEF DNET}
 {$WARN UNSAFE_CAST OFF}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_TYPE OFF}
{$ENDIF}
{$ENDIF}

{$IFDEF D7}
  {$DEFINE DPSOFT}
{$ENDIF}


{$DEFINE MYLATEST}

{WARN SYMBOL_PLATFORM OFF}
