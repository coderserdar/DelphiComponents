unit UFormViewToolList;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, ATxUserTools, TntDialogs;

type
  TFormViewToolList = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    GroupBox1: TGroupBox;
    List1: TListView;
    btnAdd: TButton;
    btnRemove: TButton;
    btnEdit: TButton;
    btnUp: TButton;
    btnDown: TButton;
    ImageList1: TImageList;
    OpenDialog1: TTntOpenDialog;
    procedure FormShow(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure List1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateList;
  public
    { Public declarations }
    Tools: TATUserTools;
  end;

implementation

uses
  ATxMsg, ATxMsgProc, ATxSProc, ATxUtils,
  UFormViewToolParams;

{$R *.DFM}

procedure ListSwapItems(List: TListView; n1, n2: integer);
var
  S: WideString;
begin
  with List do
    begin
    Items.BeginUpdate;

    S:= Items[n1].Caption;
    Items[n1].Caption:= Items[n2].Caption;
    Items[n2].Caption:= S;

    {
    S:= Items[n1].SubItems[0];
    Items[n1].SubItems[0]:= Items[n2].SubItems[0];
    Items[n2].SubItems[0]:= S;
    }

    Items.EndUpdate;
    end;
end;

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


procedure TFormViewToolList.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewToolList.inc}
  UpdateList;
end;


procedure TFormViewToolList.UpdateList;
var
  i: integer;
begin
  with List1 do
    begin
    Items.BeginUpdate;
    Items.Clear;

    ImageList1.Clear;

    for i:= Low(TATUserTools) to High(TATUserTools) do
      with Tools[i] do
        if FCaption<>'' then
          with Items.Add do
            begin
            Data:= pointer(i);
            ImageIndex:= AddCommandIcon(SExpandVars(FCommand), ImageList1);
            Caption:= FCaption;
            //SubItems.Add(FCommand);
            end;

    ListSelect(List1, 0);
    List1SelectItem(Self, nil, false);

    Items.EndUpdate;
    end;
end;

procedure TFormViewToolList.btnAddClick(Sender: TObject);
var
  Tool: TATUserTool;
begin
  if List1.Items.Count<High(TATUserTools) then
    with OpenDialog1 do
      begin
      InitialDir:= '';
      FileName:= '';
      if Execute then
        begin
        Tool.FCaption:= FGetFileDescription(FileName);
        Tool.FCommand:= FileName;
        Tool.FParams:= '"{FileName}"';
        Tool.FActions:= '';
        if ConfigureUserTool(Tool, Self) then
          begin
          AddUserTool(Tools, Tool);
          UpdateList;
          ListSelect(List1, List1.Items.Count-1);
          end;
        end;
      end;
end;

procedure TFormViewToolList.btnRemoveClick(Sender: TObject);
var
  n: integer;
begin
  with List1 do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      DeleteUserTool(Tools, integer(Selected.Data));
      UpdateList;
      ListSelect(List1, n);
      end;
end;

procedure TFormViewToolList.btnEditClick(Sender: TObject);
var
  n: integer;
begin
  with List1 do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      ConfigureUserTool(Tools[integer(Selected.Data)], Self);
      UpdateList;
      ListSelect(List1, n);
      end;
end;

procedure TFormViewToolList.btnUpClick(Sender: TObject);
var
  n, n1, n2: integer;
begin
  with List1 do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      if n>0 then
        begin
        n1:= integer(Selected.Data);
        n2:= integer(Items[n-1].Data);
        SwapUserTools(Tools[n1], Tools[n2]);
        UpdateList;
        ListSelect(List1, n-1);
        end;
      end;
end;

procedure TFormViewToolList.btnDownClick(Sender: TObject);
var
  n, n1, n2: integer;
begin
  with List1 do
    if Assigned(Selected) then
      begin
      n:= Selected.Index;
      if n<Items.Count-1 then
        begin
        n1:= integer(Selected.Data);
        n2:= integer(Items[n+1].Data);
        SwapUserTools(Tools[n1], Tools[n2]);
        UpdateList;
        ListSelect(List1, n+1);
        end;
      end;
end;


procedure TFormViewToolList.List1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  Sel: boolean;
begin
  Sel:= Assigned(List1.Selected);
  btnAdd.Enabled:= List1.Items.Count<High(TATUserTools);
  btnRemove.Enabled:= Sel;
  btnEdit.Enabled:= Sel;
  btnUp.Enabled:= Sel and (List1.Selected.Index>0);
  btnDown.Enabled:= Sel and (List1.Selected.Index<List1.Items.Count-1);
end;

procedure TFormViewToolList.FormCreate(Sender: TObject);
begin
  //Fix form font and ImageList
  //FixFormFont(Self.Font);
  FixImageList32Bit(ImageList1);
end;

end.
