{*********************************************************}
{* Dialog to set range start and end values              *}
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

unit dgSetRng;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  ffllbase,
  ffdb,
  ubase,
  uelement,
  uentity;

type
  TdlgSetRange = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    paField1: TPanel;
    laField1: TLabel;
    edStart1: TEdit;
    edEnd1: TEdit;
    cbStartNull1: TCheckBox;
    cbEndNull1: TCheckBox;
    paField16: TPanel;
    laField16: TLabel;
    edStart16: TEdit;
    edEnd16: TEdit;
    cbStartNull16: TCheckBox;
    cbEndNull16: TCheckBox;
    paField15: TPanel;
    laField15: TLabel;
    edStart15: TEdit;
    edEnd15: TEdit;
    cbStartNull15: TCheckBox;
    cbEndNull15: TCheckBox;
    paField14: TPanel;
    laField14: TLabel;
    edStart14: TEdit;
    edEnd14: TEdit;
    cbStartNull14: TCheckBox;
    cbEndNull14: TCheckBox;
    paField13: TPanel;
    laField13: TLabel;
    edStart13: TEdit;
    edEnd13: TEdit;
    cbStartNull13: TCheckBox;
    cbEndNull13: TCheckBox;
    paField12: TPanel;
    laField12: TLabel;
    edStart12: TEdit;
    edEnd12: TEdit;
    cbStartNull12: TCheckBox;
    cbEndNull12: TCheckBox;
    paField11: TPanel;
    laField11: TLabel;
    edStart11: TEdit;
    edEnd11: TEdit;
    cbStartNull11: TCheckBox;
    cbEndNull11: TCheckBox;
    paField10: TPanel;
    laField10: TLabel;
    edStart10: TEdit;
    edEnd10: TEdit;
    cbStartNull10: TCheckBox;
    cbEndNull10: TCheckBox;
    paField9: TPanel;
    laField9: TLabel;
    edStart9: TEdit;
    edEnd9: TEdit;
    cbStartNull9: TCheckBox;
    cbEndNull9: TCheckBox;
    paField8: TPanel;
    laField8: TLabel;
    edStart8: TEdit;
    edEnd8: TEdit;
    cbStartNull8: TCheckBox;
    cbEndNull8: TCheckBox;
    paField7: TPanel;
    laField7: TLabel;
    edStart7: TEdit;
    edEnd7: TEdit;
    cbStartNull7: TCheckBox;
    cbEndNull7: TCheckBox;
    paField6: TPanel;
    laField6: TLabel;
    edStart6: TEdit;
    edEnd6: TEdit;
    cbStartNull6: TCheckBox;
    cbEndNull6: TCheckBox;
    paField5: TPanel;
    laField5: TLabel;
    edStart5: TEdit;
    edEnd5: TEdit;
    cbStartNull5: TCheckBox;
    cbEndNull5: TCheckBox;
    paField4: TPanel;
    laField4: TLabel;
    edStart4: TEdit;
    edEnd4: TEdit;
    cbStartNull4: TCheckBox;
    cbEndNull4: TCheckBox;
    paField3: TPanel;
    laField3: TLabel;
    edStart3: TEdit;
    edEnd3: TEdit;
    cbStartNull3: TCheckBox;
    cbEndNull3: TCheckBox;
    paField2: TPanel;
    laField2: TLabel;
    edStart2: TEdit;
    edEnd2: TEdit;
    cbStartNull2: TCheckBox;
    cbEndNull2: TCheckBox;
    paBottom: TPanel;
    CancelBtn: TButton;
    OKBtn: TButton;
    cbRangeStartKeyExclusive: TCheckBox;
    cbRangeEndKeyExclusive: TCheckBox;
    Bevel1: TBevel;
    procedure cbStartNull1Click(Sender: TObject);
    procedure cbEndNull1Click(Sender: TObject);
  private
    { Private declarations }
    procedure SetNumberOfFields(NumFields: Integer);
    function  GetStartNull(FieldIdx: Integer) : Boolean;
    function  GetEndNull(FieldIdx: Integer) : Boolean;
    function  GetStartValue(FieldIdx: Integer) : String;
    function  GetEndValue(FieldIdx: Integer) : String;
    procedure SetStartNull(FieldIdx: Integer; IsNull: Boolean);
    procedure SetEndNull(FieldIdx: Integer; IsNull: Boolean);
    procedure SetStartValue(FieldIdx: Integer; Value : String);
    procedure SetEndValue(FieldIdx: Integer; Value : String);
    procedure SetFieldName(FieldIdx: Integer; Value : String);
  public
    { Public declarations }
  end;

  TffRangeValuesForField = record
    StartNull,
    EndNull : Boolean;
    StartValue,
    EndValue : String[255];
  end;

  TffRangeValues = record
    Field: Array [1..ffcl_MaxIndexFlds] of TffRangeValuesForField;
    RangeStartKeyExclusive,
    RangeEndKeyExclusive : Boolean;
  end;


function SetRangeDlg(aTable : TffTable; var RangeValues : TffRangeValues): TModalResult;

implementation

{$R *.dfm}


function SetRangeDlg(aTable : TffTable; var RangeValues : TffRangeValues): TModalResult;
var
  FieldIdx : Integer;
begin
  with TdlgSetRange.Create(nil) do
  try
    for FieldIdx := Low(RangeValues.Field) to High(RangeValues.Field) do begin
      SetStartNull(FieldIdx, RangeValues.Field[FieldIdx].StartNull);
      SetEndNull(FieldIdx, RangeValues.Field[FieldIdx].EndNull);
      SetStartValue(FieldIdx, RangeValues.Field[FieldIdx].StartValue);
      SetEndValue(FieldIdx, RangeValues.Field[FieldIdx].EndValue);
    end;
    for FieldIdx := 1 to aTable.IndexFieldCount do
      SetFieldName(FieldIdx, aTable.IndexFields[FieldIdx-1].DisplayName);
    SetNumberOfFields(aTable.IndexFieldCount);
    cbRangeStartKeyExclusive.Checked := RangeValues.RangeStartKeyExclusive;
    cbRangeEndKeyExclusive.Checked := RangeValues.RangeEndKeyExclusive;
    Result := ShowModal;
    if Result=mrOK then begin
      for FieldIdx := Low(RangeValues.Field) to High(RangeValues.Field) do begin
        RangeValues.Field[FieldIdx].StartNull := GetStartNull(FieldIdx);
        RangeValues.Field[FieldIdx].EndNull := GetEndNull(FieldIdx);
        RangeValues.Field[FieldIdx].StartValue := GetStartValue(FieldIdx);
        RangeValues.Field[FieldIdx].EndValue := GetEndValue(FieldIdx);
      end;
      RangeValues.RangeStartKeyExclusive := cbRangeStartKeyExclusive.Checked;
      RangeValues.RangeEndKeyExclusive := cbRangeEndKeyExclusive.Checked;
    end;
  finally
    Free;
  end;
end;


{ TdlgSetRange }

procedure TdlgSetRange.SetNumberOfFields(NumFields: Integer);
var
  FieldIdx : Integer;
begin
  for FieldIdx := ffcl_MaxIndexFlds downto NumFields+1 do
    TPanel(FindComponent('paField'+IntToStr(FieldIdx))).Visible := False;
  ClientHeight := paBottom.Top + paBottom.Height;
end;

function TdlgSetRange.GetEndNull(FieldIdx: Integer): Boolean;
begin
  Result := TCheckBox(FindComponent('cbEndNull'+IntToStr(FieldIdx))).Checked;
end;

function TdlgSetRange.GetEndValue(FieldIdx: Integer): String;
begin
  Result := TEdit(FindComponent('edEnd'+IntToStr(FieldIdx))).Text;
end;

function TdlgSetRange.GetStartNull(FieldIdx: Integer): Boolean;
begin
  Result := TCheckBox(FindComponent('cbStartNull'+IntToStr(FieldIdx))).Checked;
end;

function TdlgSetRange.GetStartValue(FieldIdx: Integer): String;
begin
  Result := TEdit(FindComponent('edStart'+IntToStr(FieldIdx))).Text;
end;

procedure TdlgSetRange.SetEndNull(FieldIdx: Integer; IsNull: Boolean);
begin
  TCheckBox(FindComponent('cbEndNull'+IntToStr(FieldIdx))).Checked := IsNull;
end;

procedure TdlgSetRange.SetEndValue(FieldIdx: Integer; Value: String);
begin
  TEdit(FindComponent('edEnd'+IntToStr(FieldIdx))).Text := Value;
end;

procedure TdlgSetRange.SetStartNull(FieldIdx: Integer; IsNull: Boolean);
begin
  TCheckBox(FindComponent('cbStartNull'+IntToStr(FieldIdx))).Checked := IsNull;
end;

procedure TdlgSetRange.SetStartValue(FieldIdx: Integer; Value: String);
begin
  TEdit(FindComponent('edStart'+IntToStr(FieldIdx))).Text := Value;
end;

procedure TdlgSetRange.SetFieldName(FieldIdx: Integer; Value: String);
begin
  TLabel(FindComponent('laField'+IntToStr(FieldIdx))).Caption := Value;
end;

procedure TdlgSetRange.cbStartNull1Click(Sender: TObject);
var
  FieldIdx : String;
begin
  FieldIdx := Copy(TCheckBox(Sender).Name, 12, 2);
  TEdit(FindComponent('edStart'+FieldIdx)).Enabled := not TCheckBox(Sender).Checked;
end;

procedure TdlgSetRange.cbEndNull1Click(Sender: TObject);
var
  FieldIdx : String;
begin
  FieldIdx := Copy(TCheckBox(Sender).Name, 10, 2);
  TEdit(FindComponent('edEnd'+FieldIdx)).Enabled := not TCheckBox(Sender).Checked;
end;

end.
