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
 * The Original Code is TurboPower XMLPartner
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
{*********************************************************}
{* XMLPartner: XpReg                                     *}
{*********************************************************}
{* XMLPartner: XML component registration                *}
{*********************************************************}

unit XpReg;

interface

procedure Register;

{$I XpDefine.inc}

implementation

uses
{$IFDEF MSWINDOWS}
{$IFDEF DCC6OrLater}                                                       {!!.56}
  DesignIntf,
  DesignEditors,
{$ELSE}
  dsgnintf,
{$ENDIF}
{$ENDIF}
{$IFDEF UsingCLX}
  QForms,
  XpAbout,
  XpQXSLPr,
  XpQFlBas,
  XpQFlHTM,
  XpQFlXML,
  {$IFNDEF LINUX}
    XpQFlRTF,
  {$ENDIF}
{$ELSE}
  Forms,
  XpAboutw,
  XpvXSLPr,
  XpvFlBas,
  XpvFlHTM,
  XpvFlPrt,
  XpvFlRTF,
  XpvFlXML,
{$ENDIF}
{$IFDEF LINUX}
  DesignIntf,
  DesignEditors,
{$ENDIF}
  Classes,
  XpBase,
  XpDom,
  XpParser;

type
  TXpVersionProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure Edit; override;
  end;

{===TXpVersionProperty===============================================}
function TXpVersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;
{--------}
procedure TXpVersionProperty.Edit;
var
  AboutBox : TXpAboutBox;
begin
  AboutBox := TXpAboutBox.Create(Application);
  try
    AboutBox.Caption := 'About XMLPartner Components';
    AboutBox.ProgramName.Caption := XpProductName;
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;
{====================================================================}

procedure Register;
begin
  { Register XMLPartner Professional components }
  RegisterComponents(XpProductName,
                     [
                      TXpParser,
                      TXpObjModel,
                      TXpXSLProcessor,
                      TXpFilterHTML,
{$IFNDEF LINUX}
{$IFNDEF UsingCLX}
                      TXpFilterPrint,
{$ENDIF}
                      TXpFilterRTF,
{$ENDIF}
                      TXpFilterText,
                      TXpFilterXML
                      ]);
  {Register the Version property editor. }
  RegisterPropertyEditor(TypeInfo(AnsiString), TXpParser, 'Version',
                         TXpVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TXpObjModel, 'Version',
                         TXpVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TXpXSLProcessor, 'Version',
                         TXpVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TXpFilterBase, 'Version',
                         TXpVersionProperty);
end;

end.
