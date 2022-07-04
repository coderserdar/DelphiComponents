{*********************************************************}
{* FlashFiler: Component Version Property Editor         *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
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

{$I ffdefine.inc}

unit ffclver;

interface

uses
  SysUtils,
  Classes,
  Controls,
  {$IFDEF DCC6OrLater}
  DesignIntf,
  DesignEditors;
  {$ELSE}
  DsgnIntf;
  {$ENDIF}

type
  TffVersionProperty = class(TStringProperty)
    public
      function GetAttributes: TPropertyAttributes; override;
      procedure Edit; override;
  end;

implementation

uses
  Forms,
  ffabout;

{===TffVersionProperty===============================================}
function TffVersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;
{--------}
procedure TffVersionProperty.Edit;
var
  AboutBox : TFFAboutBox;
begin
  AboutBox := TFFAboutBox.Create(Application);
  try
    AboutBox.Caption := 'About FlashFiler Components';
    AboutBox.ProgramName.Caption := 'FlashFiler 2';
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;
{====================================================================}

end.
