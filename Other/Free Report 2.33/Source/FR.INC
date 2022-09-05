
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Include file               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}


{$B-} {- Complete Boolean Evaluation }
{$R-} {- Range-Checking }
{$V-} {- Var-String Checking }
{$T-} {- Typed @ operator }
{$X+} {- Extended syntax }
{$P+} {- Open string params }
{$J+} {- Writeable structured consts }
{$H+} {- Use long strings by default }

{$IFDEF VER90}       // Delphi 2.0
  {$DEFINE Delphi2}
{$ENDIF}

{$IFDEF VER100}      // Delphi 3.0
  {$DEFINE Delphi3}
{$ENDIF}

{$IFDEF VER120}      // Delphi 4.0
  {$DEFINE Delphi4}
{$ENDIF}

{$IFDEF VER130}      // Delphi 5.0
  {$DEFINE Delphi4}
{$ENDIF}

{$IFDEF VER140}      // Delphi 6.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi6}
{$ENDIF}

{$IFDEF VER150}      // Delphi 7.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$WARN UNSAFE_CODE OFF}
{$ENDIF}

{$IFDEF VER170}      // Delphi 9.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$WARN UNSAFE_CODE OFF}
{$ENDIF}

{$IFDEF VER180}      // Delphi 10.0
  {$DEFINE Delphi4}
  {$DEFINE Delphi5}
  {$DEFINE Delphi6}
  {$DEFINE Delphi7}
  {$DEFINE Delphi9}
  {$DEFINE Delphi10}
  {$WARN UNSAFE_CODE OFF}
{$ENDIF}

{$IFDEF VER93}       // Borland C++ Builder 1.0
  {$DEFINE Delphi2}
{$ENDIF}

{$IFDEF VER110}      // Borland C++ Builder 3.0
  {$DEFINE Delphi3}
  {$ObjExportAll on}
{$ENDIF}

{$IFDEF VER125}      // Borland C++ Builder 4.0
  {$DEFINE Delphi4}
  {$ObjExportAll on}
{$ENDIF}

{$IFDEF VER130}      // Borland C++ Builder 5.0
 {$IFDEF BCB}
  {$ObjExportAll on}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER140}      // Borland C++ Builder 6.0
 {$IFDEF BCB}
  {$ObjExportAll on}
 {$ENDIF}
{$ENDIF}

{$IFDEF VER150}      // Borland C++ Builder 7.0
 {$IFDEF BCB}
  {$ObjExportAll on}
  {$WARN UNSAFE_CODE OFF}
 {$ENDIF}
{$ENDIF}

{$DEFINE BDE}

//{$DEFINE FREEREP2217READ}

//-------------------------Additional components----------------------------

//------------------- TeeChart component ----------------------------
{$IFNDEF Delphi2}
  {$DEFINE TeeChart}
{$ENDIF}

// TeeChart component is usually not shipped with Delphi 2.
// But if you have it, uncomment the following line:
//{$DEFINE TeeChart}

//------------------- JPEG images -----------------------------------
// JPEG images - only available in Delphi 3 and above.
// If you want to show jpeg images in "Picture" object, uncomment
// the following line:
//{$DEFINE JPEG}

//------------------- RX Rich 2.0 -----------------------------------
// RX RichEdit 2.0. You should install RX Library (2.60 or above) first.
//{$DEFINE RX}

//------------------- Interbase Objects (IBO) components ------------
// Allows using Interbase DB's with no BDE required.
// Note: this will exclude TfrDataStorage component from FR package.
//{$DEFINE IBO}

//------------------- Interbase Express (IBX) components ------------
// Allows using Interbase DB's in data manager with no BDE required.
//{$DEFINE IBX}

//------------------- ActiveX Data Objects (ADO) components ---------
// Allows using any DB's in data manager with no BDE required.
//{$DEFINE ADO}

//------------------- Script editor with syntax highlightning -------
// To use syntax highlightning in script editor, you should install
// TSyntaxMemo component by David Brock first.
//  (http://users.colloquium.co.uk/~dbrock/synmemo/synmemo.htm
//   e-mail: dbrock@cqm.co.uk)
//{$DEFINE SYNTMEMO}

//-------------------------------------------------------------------
{$IFDEF IBX}
 {$UNDEF BDE}
 {$UNDEF IBO}
 {$UNDEF ADO}
{$ENDIF}

{$IFDEF IBO}
 {$UNDEF BDE}
 {$UNDEF IBX}
 {$UNDEF ADO}
{$ENDIF}

{$IFDEF ADO}
 {$UNDEF BDE}
 {$UNDEF IBX}
 {$UNDEF IBO}
{$ENDIF}