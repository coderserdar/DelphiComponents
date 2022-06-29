{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: LanguageEditor.pas,v 1.8 2003/01/13 08:43:52 laa Exp $ }

unit LanguageEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Translator,
  StdCtrls, Buttons, ExtCtrls;

type

  TLangEditorType = (letAdd, letRename, letRemove);


  TdlgLangEditor = class(TForm)
    pnlRight: TPanel;
    pnlBottom: TPanel;
    GroupBox: TGroupBox;
    cmdOk: TBitBtn;
    cmdCancel: TBitBtn;
    lbl1: TLabel;
    edt1: TEdit;
    lbl2: TLabel;
    cbo1: TComboBox;
    lbl3: TLabel;
    cbo2: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure edt1Change(Sender: TObject);
    procedure cbo1Change(Sender: TObject);
    procedure cbo2Change(Sender: TObject);
    procedure cmdOkClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStrings : IEditableTranslatedStrings;
    FLangEditorType : TLangEditorType;
    procedure CheckOkEnabled;
  public
    function AddLanguage(Strings : IEditableTranslatedStrings) : Boolean;
    function RenameLanguage(Strings : IEditableTranslatedStrings) : Boolean;
    function RemoveLanguage(Strings : IEditableTranslatedStrings) : Boolean;

  end;

var
  dlgLangEditor: TdlgLangEditor;

implementation

{$R *.DFM}

uses Storages, DataElements, TranslatorFields;

const
  TOP1 = 20;
  TOP2 = 52;
  TOP3 = 84;

  HEIGHT2 = 124;
  HEIGHT3 = 156;

function TdlgLangEditor.AddLanguage(Strings : IEditableTranslatedStrings) : Boolean;
var
  i : Integer;
begin
  FLangEditorType := letAdd;
  FStrings := Strings;
  Self.Caption := 'Add New Language';

  lbl1.Caption := 'Add Language:';
  lbl2.Caption := 'At Position:';
  lbl3.Caption := 'Copy Properties from:';
  edt1.Top := TOP1;
  edt1.TabOrder := 0;
  cbo1.Top := TOP2;
  cbo1.TabOrder := 1;
  cbo2.Top := TOP3;
  cbo2.TabOrder := 2;
  Self.Height := HEIGHT3;

  for i := 0 to FStrings.LanguageCount - 1 do
  begin
    cbo1.Items.AddObject('Before ' + FStrings.Languages[i], FStrings.LanguageFields[i]);
    cbo2.Items.AddObject(FStrings.Languages[i], FStrings.LanguageFields[i]);
  end;
  cbo1.Items.AddObject('Last', nil);
  cbo2.Items.AddObject('None', nil);
  cbo1.ItemIndex := -1;
  cbo2.ItemIndex := -1;

  Result := (ShowModal = mrOk);
end;

function TdlgLangEditor.RenameLanguage(Strings : IEditableTranslatedStrings) : Boolean;
var
  i : Integer;
begin
  FLangEditorType := letRename;
  FStrings := Strings;
  Self.Caption := 'Rename Language';

  lbl1.Caption := 'Rename Language:';
  lbl2.Caption := 'New Name:';
  lbl3.Visible := False;
  cbo1.Top := TOP1;
  cbo1.TabOrder := 0;
  edt1.Top := TOP2;
  edt1.TabOrder := 1;
  cbo2.Visible := False;
  Self.Height := HEIGHT2;

  for i := 0 to FStrings.LanguageCount - 1 do
    cbo1.Items.AddObject(FStrings.Languages[i], FStrings.LanguageFields[i]);
  cbo1.ItemIndex := -1;

  Result := (ShowModal = mrOk);
end;

function TdlgLangEditor.RemoveLanguage(Strings : IEditableTranslatedStrings) : Boolean;
var
  i : Integer;
begin
  FLangEditorType := letRemove;
  FStrings := Strings;
  Self.Caption := 'Remove Language';

  lbl1.Caption := 'Remove Language:';
  lbl2.Visible := False;
  lbl3.Visible := False;
  edt1.Visible := False;
  cbo1.Top := TOP1;
  cbo2.Visible := False;
  Self.Height := HEIGHT2;

  for i := 0 to FStrings.LanguageCount - 1 do
    cbo1.Items.AddObject(FStrings.Languages[i], FStrings.LanguageFields[i]);
  cbo1.ItemIndex := -1;

  Result := (ShowModal = mrOk);
end;



procedure TdlgLangEditor.FormShow(Sender: TObject);
begin
  CheckOkEnabled;
end;

procedure TdlgLangEditor.CheckOkEnabled;
  function ComboLegal(ACombo : TComboBox) : Boolean;
  begin
    Result := (not ACombo.Visible) or (ACombo.ItemIndex >= 0);
  end;

  function TextLegal(AEdit : TEdit) : Boolean;
  begin
    Result := (not AEdit.Visible) or
              ((Trim(AEdit.Text) <> '') and
               (FStrings.IndexOfLanguage(Trim(AEdit.Text)) = ANYLANGUAGE));
  end;

begin
  cmdOk.Enabled := ComboLegal(cbo1) and
                   ComboLegal(cbo2) and
                   TextLegal(edt1);
end;

procedure TdlgLangEditor.edt1Change(Sender: TObject);
begin
  CheckOkEnabled;
end;

procedure TdlgLangEditor.cbo1Change(Sender: TObject);
begin
  CheckOkEnabled;
end;

procedure TdlgLangEditor.cbo2Change(Sender: TObject);
begin
  CheckOkEnabled;
end;

procedure TdlgLangEditor.cmdOkClick(Sender: TObject);
var
  idx1, idx2 : Integer;

begin
  try
    idx1 := cbo1.ItemIndex;

    case FLangEditorType of
      letAdd:
      begin
        idx2 := cbo2.ItemIndex;
        if idx2 >= FStrings.LanguageCount then
          idx2 := ANYLANGUAGE;
        FStrings.InsertLanguage(idx1, Trim(edt1.Text), idx2);
      end;

      letRename:
      begin
        FStrings.Languages[idx1] := Trim(edt1.Text);
      end;

      letRemove:
      begin
        FStrings.RemoveLanguage(idx1);
      end;

    end;
  except
    ModalResult := mrNone;
    raise;
  end;
end;

procedure TdlgLangEditor.FormDestroy(Sender: TObject);

begin

end;

end.

