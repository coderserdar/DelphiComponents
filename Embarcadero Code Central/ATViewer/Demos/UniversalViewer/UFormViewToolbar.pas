unit UFormViewToolbar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Menus, ATxToolbarList;

type
  TFormViewToolbar = class(TForm)
    ListAvail: TListView;
    labAvail: TLabel;
    btnOk: TButton;
    btnCancel: TButton;
    ListCurrent: TListView;
    labCurrent: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    btnReset: TButton;
    btnUp: TButton;
    btnDown: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ListAvailSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ListCurrentSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormShow(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
  private
    { Private declarations }
    FButtons: TToolbarList;
    procedure UpdateLists;
  public
    { Public declarations }
  end;

function CustomizeToolbarDialog(AButtons: TToolbarList): boolean;


implementation

uses
  ATxMsgProc, ATxUtils;

{$R *.DFM}

procedure ListSelect(List: TListView; n: integer);
begin
  with List do
    begin
    if n>Items.Count-1 then Dec(n);
    if n>=0 then
      begin
      ItemFocused:= Items[n];
      Selected:= ItemFocused;
      Selected.MakeVisible(false);
      end;
    end;
end;

procedure ListSwapItems(List: TListView; n1, n2: integer);
var
  s: string;
  en: boolean;
begin
  with List do
    begin
    Items.BeginUpdate;

    s:= Items[n1].Caption;
    Items[n1].Caption:= Items[n2].Caption;
    Items[n2].Caption:= s;

    en:= Items[n1].Checked;
    Items[n1].Checked:= Items[n2].Checked;
    Items[n2].Checked:= en;

    Items.EndUpdate;
    end;
end;


function CustomizeToolbarDialog(AButtons: TToolbarList): boolean;
begin
  with TFormViewToolbar.Create(nil) do
    try
      AButtons.CopyTo(FButtons);
      Result:= ShowModal=mrOk;
      if Result then
        FButtons.CopyTo(AButtons);
    finally
      Release;
    end;
end;

procedure TFormViewToolbar.FormCreate(Sender: TObject);
begin
  FButtons:= TToolbarList.Create;
end;

procedure TFormViewToolbar.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FButtons);
end;

procedure TFormViewToolbar.UpdateLists;
var
  Index1, Index2: integer;
  i, N: integer;
  Rec: PToolbarButtonRec;
begin
  with ListAvail do
    if Assigned(Selected)
      then Index1:= Selected.Index else Index1:= 0;

  with ListCurrent do
    if Assigned(Selected)
      then Index2:= Selected.Index else Index2:= 0;

  ListAvail.Items.BeginUpdate;
  ListAvail.Items.Clear;
  for i:= 1 to cToolbarButtonsMax do
    begin
    if not FButtons.GetAvail(i, Rec) then Break;
    if (FButtons.IsAvailCurrent(i)) and
      (Rec^.FMenuItem.Caption<>'-') then Continue;
    with Rec^ do
        with ListAvail.Items.Add do
          begin
          if FMenuItem.Caption='-' then
            begin
            Caption:= sMsgButtonSeparator;
            ImageIndex:= -1;
            end
          else
            begin
            Caption:= GetToolbarButtonId(Rec^);
            ImageIndex:= FMenuItem.ImageIndex;
            end;
          Data:= pointer(i);
          end;
    end;
  ListSelect(ListAvail, Index1);
  ListAvail.Items.EndUpdate;

  ListCurrent.Items.BeginUpdate;
  ListCurrent.Items.Clear;
  for i:= 1 to cToolbarButtonsMax do
    begin
    if not FButtons.GetCurrent(i, N) then Break;
    if not FButtons.GetAvail(N, Rec) then Break;
    with Rec^ do
      with ListCurrent.Items.Add do
        begin
        if FMenuItem.Caption='-' then
          begin
          Caption:= sMsgButtonSeparator;
          ImageIndex:= -1;
          end
        else
          begin
          Caption:= GetToolbarButtonId(Rec^);
          ImageIndex:= FMenuItem.ImageIndex;
          end;
        Data:= pointer(N);
        end;
    end;
  ListSelect(ListCurrent, Index2);  
  ListCurrent.Items.EndUpdate;
end;


procedure TFormViewToolbar.btnAddClick(Sender: TObject);
var
  N: integer;
  OK: boolean;
begin
  with ListAvail do
    if Assigned(Selected) then
      begin
      with ListCurrent do
        if Assigned(Selected) then
          N:= Selected.Index+1
        else
          N:= 0;

      OK:= FButtons.AddCurrentAfter(integer(Selected.Data), N);

      UpdateLists;

      if OK then
        ListSelect(ListCurrent, N);
      end;
end;

procedure TFormViewToolbar.btnRemoveClick(Sender: TObject);
begin
  with ListCurrent do
    if Assigned(Selected) then
      begin
      FButtons.RemoveCurrent(Selected.Index+1);
      UpdateLists;
      end;
end;

procedure TFormViewToolbar.ListAvailSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnAdd.Enabled:= Assigned(ListAvail.Selected);
end;

procedure TFormViewToolbar.ListCurrentSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  btnRemove.Enabled:= Assigned(ListCurrent.Selected);
end;

procedure TFormViewToolbar.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewToolbar.inc}
  ListAvail.SmallImages:= FButtons.ImageList;
  ListCurrent.SmallImages:= FButtons.ImageList;
  UpdateLists;
end;

procedure TFormViewToolbar.btnUpClick(Sender: TObject);
begin
  with ListCurrent do
    if Assigned(Selected) then
      begin
      if FButtons.MoveCurrentUp(Selected.Index+1) then
        if Selected.Index>0 then
          ListSelect(ListCurrent, Selected.Index-1);
      UpdateLists;
      end;
end;

procedure TFormViewToolbar.btnDownClick(Sender: TObject);
begin
  with ListCurrent do
    if Assigned(Selected) then
      begin
      if FButtons.MoveCurrentDown(Selected.Index+1) then
        if Selected.Index<Items.Count-1 then
          ListSelect(ListCurrent, Selected.Index+1);
      UpdateLists;
      end;
end;

procedure TFormViewToolbar.btnResetClick(Sender: TObject);
begin
  FButtons.ResetCurrents;
  UpdateLists;
end;

end.
