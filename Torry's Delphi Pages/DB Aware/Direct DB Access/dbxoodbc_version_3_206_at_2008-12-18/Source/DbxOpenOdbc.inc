// Desirable Compiler options
{$B-} // Full boolean eval off (i.e. Short-cicuit boolean eval)

{ DbxOpenOdbc compiles under
 Delphi        V6.00, V6.01, V6.02, V7.00
 Kylix         V1(?), V2(?), V3(?)
 C++ Builder   V6(?)
}
{
CompilerVersion
(taken from http://www.blong.com/Tips/KylixTips.htm)
Delphi 5; Kylix 1; C++ Builder 5; or any earlier version: CompilerVersion NOT DEFINED
Delphi 6.00:   14.01
Delphi 6.01:   14.01
Delphi 6.02:   14.01
C++Builder 6:  14.01
Kylix 2:       14.10
Kylix 3:       14.50
Delphi 7:      15.00
}

{$UNDEF _K1UP_}
{$UNDEF _D6UP_}  // Delphi 6
{$UNDEF _K2UP_}
{$UNDEF _K3UP_}
{$UNDEF _D7UP_}  // Delphi 7
{$UNDEF _D9UP_}  // Delphi 2005
{$UNDEF _D10UP_} // Delphi 2006
{$UNDEF _D11UP_} // Delphi 2007
{$UNDEF _D12UP_} // Delphi 2009

{$UNDEF _DBX30_} // DBX 3.0
{$UNDEF _DBX40_} // DBX 4.0
{$UNDEF _DBXCB_} // DBX Callback

{$UNDEF _iOdbcApi_} // IOdbcApi Interface
{$UNDEF DynamicOdbcImport}
{$UNDEF UnloadApiCache}
{$UNDEF _INLINE_}
{$UNDEF _FASTMM_}
{$UNDEF _INT64_BUGS_FIXED_} // todo: need check for Delphi 2009

{$IFDEF CONDITIONALEXPRESSIONS}
  {$DEFINE _K1UP_}
  {$IF Declared(CompilerVersion)}
    {$IF CompilerVersion >= 14.01}
       // Delphi 6.00 and up
      {$DEFINE _D6UP_}
    {$IFEND}
    {$IF CompilerVersion >= 14.10}
      // Kylix 2 and up
      {$DEFINE _K2UP_}
    {$IFEND}
    {$IF CompilerVersion >= 14.50}
      // Kylix 3 and up
      // Kylix 3 supports something somewhere between D6 and D7
      {$DEFINE _K3UP_}
    {$IFEND}
    {$IF CompilerVersion >= 15.00}
      // Delphi 7.00 and up
      {$DEFINE _D7UP_}
    {$IFEND}
    {$IF CompilerVersion >= 17.00}
      // D9: Delphi 2005 and up
      {$DEFINE _D9UP_}
    {$IFEND}
    {$IF CompilerVersion >= 18.00}
      // D10: Delphi 2006 and up
      {$DEFINE _D10UP_}
    {$IFEND}
    {$IF CompilerVersion >= 18.50}
      // D11: Delphi 2007 and up
      {$DEFINE _D11UP_}
    {$IFEND}
    {$IF CompilerVersion >= 19.00}
      // D12: Delphi 2009 and up
      {$DEFINE _D12UP_}
    {$IFEND}
  {$IFEND} // of: $IF Declared(CompilerVersion)
{$ELSE}  // of: $IFDEF CONDITIONALEXPRESSIONS
  {$MESSAGE ERROR 'Compiler must be one of the following, or higher: Delphi 6; C++ Builder 6; Kylix 1'}
{$ENDIF}

// Optional:

{$IFDEF _D9UP_}
  {$DEFINE _INLINE_} // optional
{$ENDIF}

{$DEFINE DynamicOdbcImport} // optional: It allows to use odbc libraries directly, passing odbc manager.

{$IFDEF DynamicOdbcImport}
  {$DEFINE UnloadApiCache} // optional: allow reload odbc api library
{$ENDIF}

{$DEFINE _iOdbcApi_} // optional: OdbcApi.pas implemented IOdbcApi interface.

{$IFDEF _D10UP_}
  {$DEFINE _DBX30_}  // optional
{$ENDIF}

{$IFDEF _D11UP_}
// todo:
  {.$DEFINE _DBX40_}  // optional
{$ENDIF}

{$DEFINE _DBXCB_} // optional: callback support

{$DEFINE _FASTMM_} // optional
{$IFDEF _D10UP_}
  {.$UNDEF _FASTMM_} // optional
{$ENDIF}

{.$DEFINE _INT64_BUGS_FIXED_} // optional
// QC: 58681
//  + dbx3: Int64 Field type support, but:
//          Delphi not correctly supported Int64 (TLargeintField). See db.pas: "function TParam.GetDataSize: Integer;".
//          The Field it is impossible will change since it is impossible calculate parameter data size.
//          Int64 is mapped to Float.
//          When is connection options "coMapInt64ToBCD=1" then Int64 is mapped to BCD ( numeric(18,0) ).
//          Full-fledged support can be included only after corrections mistake in Delphi. If mistake will is corrected,
//          will possible include full-fledged support by means of directives _INT64_BUGS_FIXED_.

{$DEFINE _FIX_PostgreSQL_ODBC_}// fixed: PostgreSQL ODBC driver charset conversion bug.
                               //   Driver supported only utf8 charsets for odbc api
                               //   function "SQLBindParameter".

{$IFNDEF LINUX}
  {$DEFINE _ASA_MESSAGE_CALLBACK_} // optional
{$ENDIF}
