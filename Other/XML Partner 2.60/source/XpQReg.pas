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
{* XMLPartner: XpQReg                                    *}
{*********************************************************}
{* XMLPartner: XML component registration - CLX          *}
{*********************************************************}

unit XpQReg;

interface

procedure Register;

{$I XpDefine.inc}
{$DEFINE UsingCLX}

implementation

uses
  DesignIntf,
  DesignEditors,
  QForms,
  XpAbout,
  XpQXSLPr,
  XpQFlBas,
  XpQFlHTM,
  XpQFlXML,
{$IFNDEF LINUX}
  XpQFlRTF,
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
                      TXpQXSLProcessor,
                      TXpFilterHTML,
{$IFNDEF LINUX}
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
  RegisterPropertyEditor(TypeInfo(AnsiString), TXpQXSLProcessor, 'Version',
                         TXpVersionProperty);
  RegisterPropertyEditor(TypeInfo(AnsiString), TXpFilterBase, 'Version',
                         TXpVersionProperty);
end;

end.
