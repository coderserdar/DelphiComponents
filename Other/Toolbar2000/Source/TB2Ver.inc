{ $jrsoftware: tb2k/Source/TB2Ver.inc,v 1.11 2008/09/13 21:06:45 jr Exp $ }

{ Determine Delphi/C++Builder version }
{$IFNDEF VER90}  { if it's not Delphi 2.0 }
  {$IFNDEF VER93}  { and it's not C++Builder 1.0 }
    {$DEFINE JR_D3}  { then it must be at least Delphi 3 or C++Builder 3 }
    {$IFNDEF VER100}  { if it's not Delphi 3.0 }
      {$IFNDEF VER120}  { Delphi 4/5's command line compiler doesn't like the ObjExportAll directive, so don't include it on Delphi 4/5 }
        {$IFNDEF VER130}
          {$ObjExportAll On}  { <- needed for compatibility with run-time packages in C++Builder 3+ }
        {$ENDIF}
      {$ENDIF}
      {$IFNDEF VER110}  { and it's not C++Builder 3.0 }
        {$DEFINE JR_D4}  { then it must be at least Delphi 4 or C++Builder 4 }
        {$IFNDEF VER120} {$IFNDEF VER125}  { if it's not Delphi 4 or C++Builder 4 }
          {$DEFINE JR_D5}  { then it must be at least Delphi 5 or C++Builder 5 }
          {$IFNDEF VER130}  { if it's not Delphi 5 or C++Builder 5 }
            {$DEFINE JR_D6}  { then it must be at least Delphi 6 or C++Builder 6 }
            {$IFNDEF VER140}  { if it's not Delphi 6 or C++Builder 6 }
              {$DEFINE JR_D7}  { then it must be at least Delphi 7 }
              {$IFNDEF VER150}  { if it's not Delphi 7 }
                {$DEFINE JR_D8}  { then it must be at least Delphi 8 }
                {$IFNDEF VER160}  { if it's not Delphi 8 }
                  {$DEFINE JR_D9}  { then it must be at least Delphi 9 (2005) }
                  {$IFNDEF VER170}  { if it's not Delphi 9 (2005) }
                    {$DEFINE JR_D10}  { then it must be at least Delphi 10 (2006) }
                    { Delphi 11 (2007) is an odd case: it defines VER180 and VER185 on Win32, and VER190 on .NET }
                    {$IFDEF VER185}  { if it's Win32 Delphi 11 (2007) exactly }
                      {$DEFINE JR_D11}  { then it must be at least Delphi 11 (2007) }
                    {$ENDIF}
                    {$IFNDEF VER180}  { if it's neither Delphi 10 (2006) nor Win32 Delphi 11 (2007) }
                      {$DEFINE JR_D11}  { then it must be at least Delphi 11 (2007) }
                      {$IFNDEF VER190}  { if it's not .NET Delphi 11 (2007) }
                        {$DEFINE JR_D12}  { then it must be at least Delphi 12 (2009) }
                      {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF} {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF JR_D6}
  {$IF SizeOf(Char) > 1}
    {$DEFINE JR_WIDESTR}  { defined if String type = WideString }
  {$IFEND}
  {$IF not Defined(CLR) and (SizeOf(Pointer) <> 4)}
    {$MESSAGE WARN 'This version of Toolbar2000 has not been tested on 64-bit Delphi for Win32'}
  {$IFEND}
{$ENDIF}

{$ALIGN ON}
{$BOOLEVAL OFF}
{$LONGSTRINGS ON}
{$TYPEDADDRESS OFF}
{$WRITEABLECONST ON}
{$IFDEF JR_D6}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
