
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

unit EsReg;
  {-registration unit for the Essentials components}

{$IFDEF Win32}
  {$IFNDEF Ver93}                                                      {!!.04}
    {$R ESREG.R32}
  {$ENDIF}
{$ELSE}
  {$R ESREG.R16}
{$ENDIF Win32}

interface

uses
{$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
{$IFDEF AboveD6}
  DesignIntf,                                                        {!!.09}
  DesignEditors,                                                     {!!.09}
{$ELSE}
  dsgnintf,
{$ENDIF}
  Classes, Controls, Graphics, TypInfo,  Buttons,
  EsBase, EsBase0, EsLabel, EsGrad, EsRollUp, EsCalc,
  EsEdCalc, EsCal, EsEdCal, EsClrCbx, EsMnuBtn, EsEdPop,
  EsTile, EsMarque, EsLabel0, EsDir, EsAbout0, EsWebPE;


procedure Register;


implementation


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TWinControl), TEsAttachedLabel, 'EsControl', TEsControlProperty);

  RegisterPropertyEditor(TypeInfo(TEsCustomSettings), TEsCustomLabel, 'CustomSettings', TEsCustomSettingsProperty);

  RegisterPropertyEditor(TypeInfo(string), TEsBase, 'Version', TEsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TEsCustomLabel, 'Version', TEsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TEsCustomRollUp, 'Version', TEsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TEsEdPopup, 'Version', TEsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TEsCustomColorComboBox, 'Version', TEsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TEsCustomTile, 'Version', TEsVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TEsCustomGradient, 'Version', TEsVersionProperty);


  {register web component editor}
  RegisterComponentEditor(TEsBase, TEsWebEditor);
  RegisterComponentEditor(TEsCustomRollUp, TEsWebEditor);
  RegisterComponentEditor(TEsEdPopup, TEsWebEditor);
  RegisterComponentEditor(TEsCustomColorComboBox, TEsWebEditor);
  RegisterComponentEditor(TEsCustomTile, TEsWebEditor);
  RegisterComponentEditor(TEsCustomGradient, TEsWebEditor);

  {register label component editor}
  RegisterComponentEditor(TEsCustomLabel, TEsLabelEditor);

  RegisterComponents('Essentials 1',
    [TEsLabel,
     TEsScrollingMarquee,
     TEsCalendar,
     TEsCalculator,
     TEsDateEdit,
     TEsNumberEdit,
     TEsMenuButton,
     TEsColorComboBox,
     TEsTile,
     TEsGradient,
     TEsRollUp
     ]);
end;

end.
