(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

program BldBrows;
  {-Simple program to build all B-Tree Filer browsers via Make}

{$I BTDEFINE.INC}

{$IFDEF Win32}
uses
  Filer,
  VRec,
  FvcBrows;
{$ELSE}

{$IFNDEF Windows}
{$IFDEF Only_OPro}          { <-- This define is set by FILER.MAK }
{$DEFINE HasOpro}
{$ENDIF}
{$IFDEF Both_TPro_OPro}     { <-- This define is set by FILER.MAK }
{$DEFINE HasOpro}
{$ENDIF}
{$ENDIF}

uses
  Filer,
  Vrec,
  {$IFDEF Windows}
   {$IFDEF Ver80}
  FvcBrows;
   {$ELSE}
  WBrowser;
   {$ENDIF}
  {$ELSE}
  {$IFDEF WantTV}           { <-- This define is set by FILER.MAK }
  TVBrows,
  {$ENDIF}
  {$IFDEF HasOPro}
  OPBrow,
  {$ENDIF}
  Browser;
  {$ENDIF}
{$ENDIF}

begin
end.