// ---
{$UNDEF D3_UP}  // Delphi 3
{$UNDEF D4_UP}  // Delphi 4
{$UNDEF D5_UP}  // Delphi 5
{$UNDEF D6_UP}  // Delphi 6
{$UNDEF D7_UP}  // Delphi 7
{$UNDEF D8_UP}  // Delphi 8
{$UNDEF D9_UP}  // Delphi 2005
{$UNDEF D10_UP} // Delphi 2006
{$UNDEF D11_UP} // Delphi 2007
{$UNDEF D12_UP} // Delphi 2009
{$UNDEF DELPHI_NET}
// ---
{.$UNDEF _DEVEL_}
// ---
{$IFNDEF LINUX}
  {$IFNDEF CLR}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}
// ---
//{$IFDEF CONDITIONALEXPRESSIONS}
//  {$IF CompilerVersion >= 14.01} // Delphi 6 Up
//    {$DEFINE D6_UP}
//  {$IFEND}
//{$ENDIF}
// ---
{$IFNDEF VER80}
  {$IFNDEF VER90}
    {$IFNDEF VER93}
      {$DEFINE D3_UP} { Delphi 3.0 or higher }
      {$IFNDEF VER110}
        {$DEFINE D4_UP} { Delphi 4.0 or higher }
        {$IFNDEF VER120}
          {$DEFINE D5_UP}
          {$IFNDEF VER130}
            {$DEFINE D6_UP}   { Delphi 6.0 or higher }
            {$IFNDEF VER140}
              {$DEFINE D7_UP}   { Delphi 7.0 or higher }
              {$IFNDEF VER150}
                {$DEFINE D9_UP}   { Delphi 2005 (9) or higher }
                {$IFNDEF VER170}
                  {$DEFINE D10_UP}   { Delphi 2006 (10) or higher }
                  {$IFDEF VER185}
                    {$DEFINE D11_UP}   { Delphi 2007 (11) or higher }
                  {$ELSE}
                    {$IFNDEF VER180}
                       {$DEFINE D12_UP}   { Delphi 2009 (12) or higher }
                       {$IFNDEF VER200}
                          {$DEFINE D13_UP}   { Delphi 2010 (13) or higher }
                       {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
// ---
{$UNDEF _UNICODE_}
{$IFDEF D12_UP}
  {$DEFINE _UNICODE_}
{$ENDIF}
// ---
// ---
{$UNDEF _INLINE_}
{$IFDEF D9_UP}
  {$DEFINE _INLINE_}
{$ENDIF}
// ---
{$IFDEF D6_UP}
  {$WARN SYMBOL_PLATFORM OFF}    // Suppress .Net warnings
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_PLATFORM OFF}      // NOT certified for Kylix
  {$WARN UNIT_DEPRECATED OFF}    // NOT certified for Kylix
  {$WRITEABLECONST ON}
{$ENDIF}
// ---
{$IFDEF D7_UP}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
// ---
{$B-}
{$O-}
{$D+,L+}
