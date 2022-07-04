(********************************************************************************)
(*                        UNIFIED INTERBASE (UIB)                               *)
(*                                                                              *)
(* The contents of this file are subject to the Mozilla Public License Version  *)
(* 1.1 (the "License"); you may not use this file except in compliance with the *)
(* License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ *)
(*                                                                              *)
(* Software distributed under the License is distributed on an "AS IS" basis,   *)
(* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for *)
(* the specific language governing rights and limitations under the License.    *)
(*                                                                              *)
(* Unit owner : Henri Gourvest <hgourvest@progdigy.com>                         *)
(* Contributor: Olivier Guilbaud <oguilb@free.fr>                               *)
(*                                                                              *)
(********************************************************************************)
(* $Id: jvuib.inc,v 1.12 2005/02/28 18:49:09 progdigy Exp $ *)
(********************************************************************************)

{$I jedi.inc}

{$IFDEF FREEBSD}
  {$DEFINE UNIX}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE UNIX}
{$ENDIF}

// *****************
// BEGIN USER DEFINE
// *****************

//----------
// FIREBIRD
//----------
{.$DEFINE FB102}
{.$DEFINE FB103}
{$DEFINE FB15}
{.$DEFINE FB20}
{.$DEFINE FBEMBED}

//----------
// INTERBASE
//----------
{.$DEFINE IB65}
{.$DEFINE IB7}
{.$DEFINE IB71}
{.$DEFINE IB75}

//----------
// YAFFIL
//----------
{.$DEFINE YF867}

// Enable Multithreading,
// Disabling Multithrading will made UIB faster.
{.$DEFINE UIBTHREADSAFE}

//----------
// Languages
//----------
{$DEFINE UIBLANG_EN}  // English
{.$DEFINE UIBLANG_FR} // French
{.$DEFINE UIBLANG_CZ} // Czech
{.$DEFINE UIBLANG_DE} // Deutch
{.$DEFINE UIBLANG_RU} // Russian
{.$DEFINE UIBLANG_ES} // Espanol
{.$DEFINE UIBLANG_TR} // Turkish

// use SYNEDIT (sql editor)
{$DEFINE HAVE_SYNEDIT}

// ***************
// END USER DEFINE
// ***************

{$IFDEF COMPILER7_UP}
 {$WARN UNSAFE_TYPE OFF}
 {$WARN UNSAFE_CODE OFF}
 {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFDEF FREEBSD}
  {$UNDEF UIBTHREADSAFE}
{$ENDIF}

{$IFDEF FPC}
  {$IFDEF FREEBSD}
    {$THREADING OFF}
  {$ELSE}
    {.$THREADING ON}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$DEFINE SET_TCP_NO_DELAY}
{$ENDIF}

// FIREBIRD

{$IFDEF FB20}
  {$DEFINE FIREBIRD}
  {$DEFINE FB20_UP}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE FB15ORYF867}
  {$DEFINE FB102ORYF867}
{$ENDIF}

{$IFDEF FB15}
  {$DEFINE FIREBIRD}
  {$DEFINE FB15_UP}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE FB15ORYF867}
  {$DEFINE FB102ORYF867}
{$ENDIF}

{$IFDEF FB103}
  {$DEFINE FIREBIRD}
  {$DEFINE FB103_UP}
  {$DEFINE FB102_UP}

  {$DEFINE FB102ORYF867}
{$ENDIF}

{$IFDEF FB102}
  {$DEFINE FIREBIRD}
  {$DEFINE FB102_UP}

  {$DEFINE FB102ORYF867}
{$ENDIF}

// INTERBASE

{$IFDEF IB75}
  {$DEFINE INTERBASE}
  {$DEFINE IB75_UP}
  {$DEFINE IB71_UP}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE IB65ORYF867}
{$ENDIF}

{$IFDEF IB71}
  {$DEFINE INTERBASE}
  {$DEFINE IB71_UP}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE IB65ORYF867}
{$ENDIF}

{$IFDEF IB7}
  {$DEFINE INTERBASE}
  {$DEFINE IB7_UP}
  {$DEFINE IB65_UP}

  {$DEFINE IB7ORFB15}
  {$DEFINE IB65ORYF867}
{$ENDIF}

{$IFDEF IB65}
  {$DEFINE INTERBASE}
  {$DEFINE IB65_UP}

  {$DEFINE IB65ORYF867}
{$ENDIF}

// YAFFIL

{$IFDEF YF867}
  {$DEFINE YAFFIL}
  {$DEFINE YF867_UP}

  {$DEFINE FB15ORYF867}
  {$DEFINE FB102ORYF867}
  {$DEFINE IB65ORYF867}
{$ENDIF}

////////////////////////

{$IFDEF INTERBASE}
  {$DEFINE INTERBASEORYAFFIL}
  {$DEFINE INTERBASEORFIREBIRD}
{$ENDIF}

{$IFDEF FIREBIRD}
  {$DEFINE INTERBASEORFIREBIRD}
  {$DEFINE FIREBIRDORYAFFIL}
{$ENDIF}

{$IFDEF YAFFIL}
  {$DEFINE INTERBASEORYAFFIL}
  {$DEFINE FIREBIRDORYAFFIL}
{$ENDIF}

{$IFDEF FPC}
  {$DEFINE SUPPORTS_WIDESTRING}
  {$DEFINE SUPPORTS_INTERFACE}
  {$DEFINE SUPPORTS_INT64}
  {$DEFINE SUPPORTS_DYNAMICARRAYS}
  {$DEFINE SUPPORTS_DEFAULTPARAMS}
  {$DEFINE SUPPORTS_OVERLOAD}
  {$ASMMODE Intel}
  {$UNDEF UseRegAsm}
  {$GOTO ON}
{$ENDIF}

{$IFDEF FB15_UP}
{$IFDEF MSWINDOWS}
  {$DEFINE DLLREGISTRY}
{$ENDIF}
{$ENDIF}

{.$IFDEF FPC}
  {$DEFINE PUREPASCAL}
{.$ENDIF}

{$IFDEF KYLIX}
  {$DEFINE PUREPASCAL}
{$ENDIF}

