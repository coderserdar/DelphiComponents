// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  32148: mcmDefines.pas 
//
//    Rev 1.0    2014-02-03 00:31:44  mcm    Version: IMG 4.0
// Delphi 2  = VER90
// Builder   = VER93
// Delphi 3  = VER100
// Builder 3 = VER110
// Delphi 4  = VER120
// Builder 4 = VER125 or VER130 ?
// Delphi 5  = VER130
// Builder 5 = VER135
// Delphi 6 = VER140
// Builder 6 = VER1
// Delphi 7 = VER150
// Delphi 2005 = VER170
// Delphi 2006 = VER180
// Delphi 2007 = VER185
// Delphi 2009 = VER200
// Delphi 2010 = VER210
// Delphi XE   = VER220
// Delphi XE2  = VER230
// Delphi XE3  = VER240
// Delphi XE4  = VER250
// Delphi XE5  = VER260
// Delphi XE6  = VER270


{$B-} // Don't perform complete boolean evaluations.
{$IFDEF  VER90}  {$DEFINE DCB2_5}   {$ENDIF}
{$IFDEF  VER93}  {$DEFINE DCB2_5}   {$ENDIF}
{$IFDEF VER100}  {$DEFINE DCB3}
                 {$DEFINE DCB3_4}
                 {$DEFINE DCB2_5}
                 {$DEFINE DCB3_5}
                 {$DEFINE DCB3_6}   {$ENDIF} // DELPHI 3
{$IFDEF VER110}  {$DEFINE DCB3}
                 {$DEFINE DCB3_4}
                 {$DEFINE DCB2_5}
                 {$DEFINE DCB3_5}
                 {$DEFINE DCB3_6}   {$ENDIF} // BUILDER 3
{$IFDEF VER120}  {$DEFINE DCB3_4}
                 {$DEFINE DCB2_5}
                 {$DEFINE DCB3_5}
                 {$DEFINE DCB3_6}   {$ENDIF} // DELPHI 4
{$IFDEF VER125}  {$DEFINE DCB3_4}
                 {$DEFINE DCB2_5}
                 {$DEFINE DCB3_5}
                 {$DEFINE DCB3_6}   {$ENDIF} // BUILDER 4
{$IFDEF VER130}  {$DEFINE DCB2_5}
                 {$DEFINE DCB3_5}
                 {$DEFINE DCB3_6}   {$ENDIF} // DELPHI 5
{$IFDEF VER135}  {$DEFINE DCB2_5}
                 {$DEFINE DCB3_5}
                 {$DEFINE DCB3_6}   {$ENDIF} // BUILDER 5
{$IFDEF VER140}  {$DEFINE DCB3_6}   {$ENDIF} // DELPHI 6
{$IFDEF VER145}  {$DEFINE DCB3_6}   {$ENDIF} // BUILDER 6
{$IFDEF DCB3_6}  {$DEFINE LE_D2005} {$ENDIF} //
{$IFDEF VER150}  {$DEFINE LE_D2005} {$ENDIF} // DELPHI 7
{$IFDEF VER170}  {$DEFINE LE_D2005} {$ENDIF} // DELPHI 2005
{$IFDEF VER180}                     {$ENDIF} // DELPHI 2006
{$IFDEF VER185}                     {$ENDIF} // DELPHI 2007

{$IFDEF VER200}  {$DEFINE DCB_UNIC}
                 {$DEFINE GE_D2009} {$ENDIF} // DELPHI 2009
{$IFDEF VER210}  {$DEFINE GE_D2010}
                 {$DEFINE DCB_UNIC} {$ENDIF} // DELPHI 2010
{$IFDEF VER220}  {$DEFINE DCB_UNIC}
                 {$DEFINE GE_DXE}   {$ENDIF} // DELPHI XE
{$IFDEF VER230}  {$DEFINE DCB_UNIC}
                 {$DEFINE GE_DXE2}  {$ENDIF} // DELPHI XE2
{$IFDEF VER240}  {$DEFINE GE_DXE2}  {$ENDIF} // DELPHI XE3
{$IFDEF VER250}  {$DEFINE GE_DXE2}
                 {$DEFINE GE_DXE4}  {$ENDIF} // DELPHI XE4
{$IFDEF VER260}  {$DEFINE GE_DXE2}
                 {$DEFINE GE_DXE4}  {$ENDIF} // DELPHI XE5
{$IFDEF VER270}  {$DEFINE GE_DXE2}
                 {$DEFINE GE_DXE4}  {$ENDIF} // DELPHI XE6
{$IFDEF VER280}  {$DEFINE GE_DXE2}
                 {$DEFINE GE_DXE4}  {$ENDIF} // DELPHI XE7

{$IFDEF GE_DXE2} {$DEFINE DCB_UNIC}
                 {$DEFINE GE_DXE}   {$ENDIF} //
{$IFDEF GE_DXE}  {$DEFINE GE_D2010} {$ENDIF} //
{$IFDEF GE_D2010}{$DEFINE GE_D2009} {$ENDIF} //

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

