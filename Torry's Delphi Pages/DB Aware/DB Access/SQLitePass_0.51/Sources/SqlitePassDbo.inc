
{$IFDEF FPC}

 {$mode delphi}
 {$H+}
 {$T+}                        //Force Pointer Type conversion
 {$B-}
 {$DEFINE HasInline}          // Support function compiler inlining
 {$DEFINE HasftWideMemo}      // Support Unicode ftWideMemo
 {$DEFINE HasFmtBcd}          // Support for FmtBcd
 {$DEFINE HasftFixedWideChar} // Support for ftfixedWideChar
 {$DEFINE NeedSqlitePassWideStringUtils}

{$ELSE} // Not FPC ?

  {$IFDEF WIN32}
//# VER80 - Delphi 1
//# VER90 - Delphi 2
//# VER100 - Delphi 3
//# VER120 - Delphi 4
//# VER130 - Delphi 5
//# VER140 - Delphi 6
//# VER150 - Delphi 7
//# VER160 - Delphi 8
//# VER170 - Delphi 2005
//# VER180 - Delphi 2006
//# VER180 - Delphi 2007
//# VER185 - Delphi 2007

    {$IFDEF VER210}
      {$DEFINE Delphi2010}
      {$DEFINE Delphi2009}
      {$DEFINE HasInline}
      {$DEFINE HasftWideMemo} // Support Unicode ftWideMemo
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {$DEFINE HasftFixedWideChar} // Support for ftfixedWideChar
    {$ENDIF VER200}

    {$IFDEF VER200}
      {$DEFINE Delphi2009}
      {$DEFINE HasInline}
      {$DEFINE HasftWideMemo} // Support Unicode ftWideMemo
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {$DEFINE HasftFixedWideChar} // Support for ftfixedWideChar
    {$ENDIF VER200}

    {$IFDEF VER185}
      {$DEFINE Delphi2007}
      {$DEFINE Delphi2006}
      {$DEFINE HasInline}
      {$DEFINE HasftWideMemo} // Support Unicode ftWideMemo
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {.$DEFINE UseTNTComponents}
    {$ENDIF VER185}

    {$IFDEF VER180}
      {$DEFINE Delphi2007}
      {$DEFINE Delphi2006}
      {$DEFINE HasInline}
      {$DEFINE HasftWideMemo} // Support Unicode ftWideMemo
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {.$DEFINE UseTNTComponents}
    {$ENDIF ver180}

    {$IFDEF VER170}
      {$DEFINE Delphi8}
      {$DEFINE Delphi7}
      {$DEFINE Delphi6}
      {$DEFINE HasInline}
      {$DEFINE HasftWideMemo}   // Support Unicode ftWideMemo
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {.$DEFINE UseTNTComponents}
    {$ENDIF ver170}

    {$IFDEF VER160}
      {$DEFINE Delphi8}
      {$DEFINE Delphi7}
      {$DEFINE Delphi6}
      {$DEFINE HasftWideMemo}   // Support Unicode ftWideMemo
      {$DEFINE NeedSqlitePassWideStringUtils}
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {.$DEFINE UseTNTComponents}
    {$ENDIF}

    {$IFDEF VER150}
      {$DEFINE Delphi7}
      {$DEFINE Delphi6}
      {.$DEFINE HasftWideMemo} // Support Unicode ftWideMemo
      {$DEFINE NeedSqlitePassWideStringUtils}
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {.$DEFINE UseTNTComponents}
    {$ENDIF}

    {$IFDEF VER140}
      {$DEFINE Delphi6}
      {$DEFINE NeedSqlitePassUnicodeUtils}
      {$DEFINE NeedSqlitePassWideStringUtils}
      {$DEFINE HasFmtBcd}       // Support for FmtBcd
      {$DEFINE HasInt64Variant} // Support for ftLargeInt as Variant
      {.$DEFINE UseTNTComponents}
    {$ENDIF}

    {$IFDEF VER130}
      {$DEFINE Delphi5}
      {$DEFINE Delphi4}
      {$DEFINE NeedSqlitePassUnicodeUtils}
      {$DEFINE NeedSqlitePassWideStringUtils}
      {.$DEFINE UseTNTComponents}
    {$ENDIF}

    {$IFDEF VER120}
      {$DEFINE Delphi4}
      {$DEFINE NeedSqlitePassUnicodeUtils}
      {$DEFINE NeedSqlitePassWideStringUtils}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}


// TODO - CHECK COMPILER DIRECTIVES
{$IFDEF DEBUG_SQLITEPASS}
 {$Q+}  {- Overflow-Checking }
 {$R+}  {- Range-Checking }
 {$O-}  {- Optimisation }
 {$D+}  {+ Debug info }
{$ELSE}
 {.$B-} {- Complete Boolean Evaluation }
 {$Q-}  {- Overflow-Checking }
 {$R-}  {- Range-Checking }
 {.$V-} {- Var-String Checking }
 {.$T-} {- Typed @ operator }
 {$X+} {- Extended syntax }
 {$P+} {- Open string params }
 {$O+} {+ Optimisation }
{$ENDIF}

{$IFDEF WIN32}
 {.$J+} {- Writeable structured consts }
 {$H+} {- Use long strings by default }
{$ENDIF}
