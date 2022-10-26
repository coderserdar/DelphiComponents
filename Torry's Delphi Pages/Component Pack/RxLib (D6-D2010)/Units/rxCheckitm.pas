{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1995, 1996 AO ROSNO             }
{         Copyright (c) 1997, 1998 Master-Bank          }
{                                                       }
{ Patched by Polaris Software                           }
{*******************************************************}

unit rxCheckItm;

interface

{$I RX.INC}

uses 
  {$IFDEF WIN32} Windows,
  {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Messages,
  SysUtils, Classes, Controls, Forms, Menus, Graphics, StdCtrls, Dialogs,
  RXCtrls, ExtCtrls, rxPlacemnt,
  {$IFDEF RX_D6} DesignIntf, DesignEditors {$ELSE} DsgnIntf, rxPlacemnt {$ENDIF}; // Polaris

type

{ TCheckItemEditor }

  TCheckItemEditor = class(TForm)
  private
    FEdit: TEdit;
    FOkBtn: TButton;
    FCancelBtn: TButton;
    FComboBox: TComboBox;
    FEnableBox: TCheckBox;
    FMemo: TMemo;   // my addition
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TCheckItemsEditor }

  TCheckItemsEditor = class(TForm)
    Panel2: TPanel;
    DeleteBtn: TButton;
    NewBtn: TButton;
    EditBtn: TButton;
    Panel3: TPanel;
    CancelBtn: TButton;
    Panel1: TPanel;
    FormPlacement: TFormPlacement;
    OkBtn: TButton;
    Popup: TPopupMenu;
    cbGrayedItem: TMenuItem;
    cbCheckedItem: TMenuItem;
    cbUncheckedItem: TMenuItem;
    N2: TMenuItem;
    EnabledItem: TMenuItem;
    ClearBtn: TButton;
    CheckList: TRxCheckListBox;
    UpBtn: TButton;
    DownBtn: TButton;
    AddListBtn: TButton;
    procedure EditBtnClick(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure DeleteBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EnabledItemClick(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure AddListMenuClick(Sender: TObject);
    procedure cbGrayedItemClick(Sender: TObject);
    procedure cbCheckedItemClick(Sender: TObject);
    procedure cbUncheckedItemClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure CheckListClick(Sender: TObject);
    procedure UpDownBtnClick(Sender: TObject);
    procedure CheckListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure CheckListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure AddListBtnClick(Sender: TObject);
  private
    procedure CheckButtons;
  end;

{ CheckItems property editor }

  TCheckItemsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{$R *.DFM}

{$IFDEF WIN32}
 {$D-}
{$ENDIF}

uses 
  Consts, 
  {$IFDEF RX_D3} rxStrLEdit, 
  {$ELSE} StrEdit, {$ENDIF} 
  RxConst, rxVclUtils, rxBoxProcs;

{ TCheckItemsProperty }

function TCheckItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

procedure TCheckItemsProperty.Edit;
var
  Comp: TPersistent;
begin
  with TCheckItemsEditor.Create(Application) do
  try
    Comp := Self.GetComponent(0);
    if Comp is TComponent then
      Caption := TComponent(Comp).Name + '.' + Self.GetName
    else Caption := Self.GetName;
    if Comp is TRxCheckListBox then begin
      CheckList.AllowGrayed := TRxCheckListBox(Comp).AllowGrayed;
      CheckList.Sorted := TRxCheckListBox(Comp).Sorted;
      CheckList.CheckKind := TrxCheckListBox(Comp).CheckKind;
    end;
    CheckList.Items := TStrings(GetOrdValue);
    if ShowModal = mrOk then
       SetOrdValue(LongInt(CheckList.Items));
  finally
    Free;
  end;
end;

{ TCheckItemEditor }

constructor TCheckItemEditor.Create(AOwner: TComponent);
begin
{$IFDEF CBUILDER}
  inherited CreateNew(AOwner, 0);
{$ELSE}
  inherited CreateNew(AOwner);
{$ENDIF}
  { Form definitions }
  {Left := 354;
  Top := 338;}
  BorderStyle := bsDialog;
  Caption := 'Item editor';
  ClientHeight := 92 + 4 + 64 + 12;
  ClientWidth := 330;
  Font.Color := clWindowText;
  Font.Size := 8;
  Font.Name := 'MS Sans Serif';
  Font.Style := [];
  Scaled := True;
  Position := poScreenCenter;
  { FEdit }
  FEdit := TEdit.Create(Self);
  with FEdit do begin
    Parent := Self;
    Left := 8;
    Top := 12;
    Width := 313;
    Height := 21;
    TabOrder := 0;
  end;
  { FOkBtn }
  FOkBtn := TButton.Create(Self);
  with FOkBtn do begin
    Parent := Self;
    Left := 168;
    Top := 60 + 80;
    Width := 75;
    Height := 25;
    Caption := ResStr(SOKButton);
    Default := True;
    ModalResult := mrOk;
    TabOrder := 1;
  end;
  { FCancelBtn }
  FCancelBtn := TButton.Create(Self);
  with FCancelBtn do begin
    Parent := Self;
    Left := 246;
    Top := 60 + 80;
    Width := 75;
    Height := 25;
    Cancel := True;
    Caption := ResStr(SCancelButton);
    ModalResult := mrCancel;
    TabOrder := 2;
  end;
  { FCheckBox }
  FComboBox := TComboBox.Create(Self);
  with FComboBox do begin
    Parent := Self;
    Style := csDropDownList;
    Items.Add('Unchecked');
    Items.Add('Checked');
    Items.Add('Grayed');
    Left := 8;
    Top := 38;
    Width := 88;
    TabOrder := 3;
  end;
  { FEnableBox }
  FEnableBox := TCheckBox.Create(Self);
  with FEnableBox do begin
    Parent := Self;
    Left := 104;
    Top := 40;
    Width := 70;
    Height := 17;
    Caption := 'Enabled';
    State := cbChecked;
    TabOrder := 4;
  end;
  FMemo := TMemo.Create(Self);
  with FMemo do begin
    Parent := Self;
    Left:= 8 ;
    Width := Self.clientWidth - 16;
    Top := 62;
    Height := Self.ClientHeight - 108;
    WordWrap := false;
    ScrollBars := ssBoth;
    ShowHint := true;
    Hint := 'Enter hint here...';
    TabOrder := 5;
    BorderStyle := bsSingle;
  end;
end;

{ TCheckItemsEditor }

procedure TCheckItemsEditor.FormCreate(Sender: TObject);
begin
  with FormPlacement do begin
    UseRegistry := True;
    IniFileName := SDelphiKey;
  end;
  // commented, because HintMemo was lost somewhere
  // (* my addition *)
  // with HintMemo do Constraints.MinHeight := abs(Font.height)+8;
  //PanelHint.Constraints.MinHeight:= PanelHint.Height - HintMemo.Height + HintMemo.Constraints.MinHeight;
end;

procedure TCheckItemsEditor.CheckButtons;
begin
  DeleteBtn.Enabled := CheckList.ItemIndex >= 0;
  EditBtn.Enabled := DeleteBtn.Enabled;
  UpBtn.Enabled := CheckList.ItemIndex > 0;
  DownBtn.Enabled := (CheckList.ItemIndex < CheckList.Items.Count - 1)
    and (CheckList.ItemIndex >= 0);

  // commented, because HintMemo was lost somewhere
  // (* my addition *)
  // HintMemo.Lines.Text:=CheckList.ItemHint[CheckList.ItemIndex];
  // with HintMemo do Visible := Lines.Count > 0;
end;

procedure TCheckItemsEditor.EditBtnClick(Sender: TObject);
var
  I: Integer;
begin
  I := CheckList.ItemIndex;
  if I >= 0 then
    with TCheckItemEditor.Create(Application) do
    try         // ??? Why there's no scaling when <New> is pressed ?
      if Screen.PixelsPerInch <> 96 then begin { scale to screen res }
        ScaleBy(Screen.PixelsPerInch, 96);
        { The ScaleBy method does not scale the font well, so set the
          font back to the original info. }
        Font.Name := 'MS Sans Serif';
        Font.Size := 8;
        Left := (Screen.Width div 2) - (Width div 2);
        Top := (Screen.Height div 2) - (Height div 2);
      end;
      FEdit.Text := CheckList.Items[I];
      FMemo.Lines.Text := CheckList.ItemHint[I]; // my addition
      FComboBox.ItemIndex := Integer(CheckList.State[I]);
      FEnableBox.Checked := CheckList.EnabledItem[I];
      if ShowModal = mrOk then begin
        CheckList.Items[I] := FEdit.Text;
        CheckList.State[I] := TCheckBoxState(FComboBox.ItemIndex);
        CheckList.EnabledItem[I] := FEnableBox.Checked;
        CheckList.ItemHint[I] := FMemo.Lines.Text; // my addition
      end;
      Self.CheckList.ItemIndex := I;
      CheckButtons;  // my addition - for showing hint
    finally
      Free;
    end;
end;

procedure TCheckItemsEditor.NewBtnClick(Sender: TObject);
var
  Index: Integer;
begin
  with TCheckItemEditor.Create(Application) do
  try
    FEdit.Text := '';
    FMemo.Clear;   // my addition
    FComboBox.ItemIndex := Integer(clbDefaultState);
    FEnableBox.Checked := clbDefaultEnabled;
    if ShowModal = mrOk then begin
      Index := CheckList.Items.Add(FEdit.Text);
      CheckList.State[Index] := TCheckBoxState(FComboBox.ItemIndex);
      CheckList.EnabledItem[Index] := FEnableBox.Checked;
      CheckList.ItemHint[Index] := FMemo.Lines.Text; FMemo.Lines.Clear;// my addition
      CheckButtons;
    end;
  finally
    Free;
  end;
end;

procedure TCheckItemsEditor.DeleteBtnClick(Sender: TObject);
begin
  if CheckList.ItemIndex >= 0 then begin
    CheckList.Items.Delete(CheckList.ItemIndex);
    CheckButtons;
  end;
end;

procedure TCheckItemsEditor.FormShow(Sender: TObject);
begin
  CheckButtons;
end;

procedure TCheckItemsEditor.EnabledItemClick(Sender: TObject);
begin
  CheckList.EnabledItem[CheckList.ItemIndex] :=
    not CheckList.EnabledItem[CheckList.ItemIndex];
end;

procedure TCheckItemsEditor.PopupPopup(Sender: TObject);
var
  Enable: Boolean;
begin
  Enable := CheckList.ItemIndex >= 0;
  EnabledItem.Enabled := Enable;
  cbGrayedItem.Enabled := Enable;
  cbCheckedItem.Enabled := Enable;
  cbUncheckedItem.Enabled := Enable;
  cbGrayedItem.Checked := False;
  cbCheckedItem.Checked := False;
  cbUncheckedItem.Checked := False;
  if Enable then begin
    EnabledItem.Checked := CheckList.EnabledItem[CheckList.ItemIndex];
    case CheckList.State[CheckList.ItemIndex] of
      cbChecked: cbCheckedItem.Checked := True;
      cbUnchecked: cbUncheckedItem.Checked := True;
      cbGrayed: cbGrayedItem.Checked := True;
    end;
  end;
end;

procedure TCheckItemsEditor.AddListBtnClick(Sender: TObject);
var i,j:integer;
begin  
  j:=0;
  // commented because PopupStrings was lost somewhere
  {
  with PopupStrings do 
  begin
       for i:=0 to Items.Count -1 do
           if Items[i].Checked then 
		      begin j:=i; 
			    break; 
			  end;
       With Items[j] do 
	     Click;
  end; }
end;

procedure TCheckItemsEditor.AddListMenuClick(Sender: TObject);
var
  I: LongInt;
  Mode : integer;   // my addition
begin
  Mode := 0;
  if Sender is TComponent then Mode:=TComponent(Sender).Tag;
  with TStrEditDlg.Create(Application) do
    try
      case Mode of
        1: Caption := 'Append to '+Self.Caption;
        else Caption := Self.Caption;
      end;

      if Mode = 1 then if mrYes =
         MessageDlg('This will wipe out all ItemHints !!!'^M^J +
                    'Would You cancel this action'^M^J^I+
                    'and use <Edit> button instead?',mtConfirmation,[mbNo,mbYes],0)
         then Abort;

      if Sender is TMenuItem then TMenuItem(Sender).Checked := true;   
{$IFNDEF RX_D3}
      CodeWndBtn.Visible := False;
{$ENDIF}
      if Mode = 1 then
         Memo.Lines.Assign(CheckList.Items);

      if ShowModal = mrOk then begin
        if Mode = 1 then checkList.Items.Assign(Memo.Lines) else
        for I := 0 to Memo.Lines.Count - 1 do
          if Memo.Lines[I] <> '' then
            CheckList.Items.Add(Memo.Lines[I]);
        CheckButtons;
      end;
    finally
      Free;
    end;
end;

procedure TCheckItemsEditor.cbGrayedItemClick(Sender: TObject);
begin
  CheckList.State[CheckList.ItemIndex] := cbGrayed;
end;

procedure TCheckItemsEditor.cbCheckedItemClick(Sender: TObject);
begin
  CheckList.State[CheckList.ItemIndex] := cbChecked;
end;

procedure TCheckItemsEditor.cbUncheckedItemClick(Sender: TObject);
begin
  CheckList.State[CheckList.ItemIndex] := cbUnchecked;
end;

procedure TCheckItemsEditor.ClearBtnClick(Sender: TObject);
begin
  CheckList.Clear;
  // commented, cause HintMemo was lost somewhere
  //HintMemo.Lines.Clear;
end;

procedure TCheckItemsEditor.CheckListClick(Sender: TObject);
begin
  CheckButtons;
end;

procedure TCheckItemsEditor.UpDownBtnClick(Sender: TObject);
var
  OldIndex, NewIndex: Integer;
begin
  OldIndex := CheckList.ItemIndex;
  if Sender = UpBtn then NewIndex := OldIndex - 1
  else {if Sender = DownBtn then} NewIndex := OldIndex + 1;
  CheckList.Items.Move(OldIndex, NewIndex);
  CheckList.ItemIndex := NewIndex;
  CheckButtons;
end;

procedure TCheckItemsEditor.CheckListKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Incr: Integer;
begin
  case Key of
    VK_DELETE:
      if ssCtrl in Shift then begin
        DeleteBtnClick(nil);
        Key := 0;
      end;
    VK_INSERT:
      if Shift = [] then begin
        AddListBtnClick(nil);
        Key := 0;
      end;
    VK_DOWN, VK_UP:
      if (ssCtrl in Shift) then begin
        if Key = VK_DOWN then Incr := 1
        else Incr := -1;
        BoxMoveFocusedItem(CheckList, CheckList.ItemIndex + Incr);
        CheckButtons;
        Key := 0;
      end;
  end;
end;

procedure TCheckItemsEditor.CheckListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if Source = CheckList then begin
    BoxMoveFocusedItem(CheckList, CheckList.ItemAtPos(Point(X, Y), True));
    CheckButtons;
  end;
end;

procedure TCheckItemsEditor.CheckListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  BoxDragOver(CheckList, Source, X, Y, State, Accept, CheckList.Sorted);
  if State = dsDragLeave then CheckList.DragCursor := crDrag
  else if (State = dsDragEnter) and (CheckList.SelCount > 1) then
    CheckList.DragCursor := crMultiDrag;
end;

end.
