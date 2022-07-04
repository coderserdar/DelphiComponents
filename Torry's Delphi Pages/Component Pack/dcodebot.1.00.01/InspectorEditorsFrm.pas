
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit InspectorEditorsFrm;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, InspectCtrls;

{ TInspectorEditorsForm }

type
  TInspectorEditorsForm = class(TForm)
    ListBox: TListBox;
    GroupBox: TGroupBox;
    KindComboBox: TComboBox;
    NameEdit: TEdit;
    KindLabel: TLabel;
    NameLabel: TLabel;
    ValueEdit: TEdit;
    ValueLabel: TLabel;
    OKButtopn: TButton;
    CancelButton: TButton;
    AddButton: TButton;
    RemoveButton: TButton;
    IndexLabel: TLabel;
    IndexEdit: TEdit;
    UpDown: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure ListBoxClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure ValueEditExit(Sender: TObject);
  private
    FInspector: TInspector;
    function GetEditors: TInspectorEditors;
    procedure SetEditors(Value: TInspectorEditors);
  public
    property Editors: TInspectorEditors read GetEditors write SetEditors;
  end;

function EditInspector(Inspector: TInspector): Boolean;

implementation

{$R *.DFM}

{ TInspectorEditorsForm }

procedure TInspectorEditorsForm.FormCreate(Sender: TObject);
begin
  ClientHeight := CancelButton.Top + 32;
  ClientWidth := ListBox.Left + CancelButton.Left + CancelButton.Width;
  GetEditorKindStrings(KindComboBox.Items);
  KindComboBox.ItemIndex := 0;
  FInspector := TInspector.Create(Self);
  with FInspector do
  begin
    Parent := Self;
    Visible := False;
  end;
end;

function TInspectorEditorsForm.GetEditors: TInspectorEditors;
begin
  Result := FInspector.Editors;
end;

procedure TInspectorEditorsForm.SetEditors(Value: TInspectorEditors);
var
  I: Integer;
begin
  if Value <> nil then
  begin
    FInspector.Editors.Assign(Value);
    with ListBox.Items do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Value.Count - 1 do
          Add(Value[I].Name);
      finally
        EndUpdate;
      end;
    end;
    UpDown.Max := ListBox.Items.Count - 1;
  end;
end;

procedure TInspectorEditorsForm.AddButtonClick(Sender: TObject);
var
  Kind: TEditorKind;
begin
  if Trim(Name) = '' then Exit;
  with KindComboBox do
    Kind := TEditorKind(Items.Objects[ItemIndex]);
  with Editors.Add(Kind) do
  begin
    Name := NameEdit.Text;
    try
      Text := ValueEdit.Text;
    except
      // ignore
    end;
  end;
  ListBox.Items.Add(NameEdit.Text);
  ListBox.ItemIndex := -1;
  ListBoxClick(nil);
end;

procedure TInspectorEditorsForm.ListBoxClick(Sender: TObject);
var
  Kind: TEditorKind;
begin
  UpDown.OnChangingEx := nil;
  if ListBox.ItemIndex > -1 then
  begin
    RemoveButton.Enabled := True;
    Kind := Editors[ListBox.ItemIndex].Kind;
    with KindComboBox do
      ItemIndex := Items.IndexOfObject(TObject(Kind));
    UpDown.Position := ListBox.ItemIndex;
    UpDown.Max := ListBox.Items.Count - 1;
    NameEdit.Text := Editors[ListBox.ItemIndex].Name;
    ValueEdit.Text := Editors[ListBox.ItemIndex].Text;
  end
  else
  begin
    RemoveButton.Enabled := False;
    UpDown.Position := 0;
    UpDown.Max := ListBox.Items.Count - 1;
    NameEdit.Text := '';
    ValueEdit.Text := '';
  end;
  UpDown.OnChangingEx := UpDownChangingEx;
end;

procedure TInspectorEditorsForm.RemoveButtonClick(Sender: TObject);
var
  PriorIndex: Integer;
begin
  if ListBox.ItemIndex > -1 then
  begin
    Editors[ListBox.ItemIndex].Free;
    PriorIndex := ListBox.ItemIndex;
    ListBox.Items.Delete(ListBox.ItemIndex);
    if PriorIndex = ListBox.Items.Count then
      Dec(PriorIndex);
    ListBox.ItemIndex := PriorIndex;
    ListBoxClick(nil);
  end;
end;

procedure TInspectorEditorsForm.UpDownChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  if (NewValue < 0) or (NewValue > ListBox.Items.Count - 1) then
    Exit;
  if ListBox.ItemIndex > -1 then
  begin
    Editors[ListBox.ItemIndex].Index := NewValue;
    ListBox.Items.Move(ListBox.ItemIndex, NewValue);
    ListBox.ItemIndex := NewValue;
    ListBox.OnClick := ListBoxClick;
  end
  else
    AllowChange := False;
end;

procedure TInspectorEditorsForm.ValueEditExit(Sender: TObject);
begin
	if ListBox.ItemIndex < 0 then Exit;
  if ListBox.Items[ListBox.ItemIndex] = NameEdit.Text then
	try
  	Editors[ListBox.ItemIndex].Text := ValueEdit.Text;
  except
		// ignore
	end;
end;

function EditInspector(Inspector: TInspector): Boolean;
begin
	with TInspectorEditorsForm.Create(Application) do
	try
  	Editors := Inspector.Editors;
    Result := ShowModal = mrOK;
    if Result then
      Inspector.Editors := Editors;
  finally
  	Free;
  end;
end;

end.
