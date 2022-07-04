
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
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsRegDb;
  {-registration unit for the data-aware Essentials components}

{$IFDEF Win32}
  {$IFNDEF Ver93}                                                      {!!.04}
    {$R ESREGDB.R32}
  {$ENDIF}
{$ELSE}
  {$R ESREGDB.R16}
{$ENDIF Win32}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
{$IFDEF AboveD6}                                                         {!!.11}
  DesignIntf,                                                        {!!.09}
  DesignEditors,                                                     {!!.09}
{$ELSE}
  dsgnintf,
{$ENDIF}
  Classes, Controls, Graphics, TypInfo,
  EsBase, EsLabel, EsCalc, EsEdCalc, EsCal, EsEdCal, EsDbEd;


procedure Register;


implementation


procedure Register;
begin
  RegisterComponents('Essentials 1',
    [TEsDbDateEdit,
     TEsDbNumberEdit]);
end;

end.
