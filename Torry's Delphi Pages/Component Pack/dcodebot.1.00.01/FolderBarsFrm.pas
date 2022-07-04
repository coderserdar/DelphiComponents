
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit FolderBarsFrm;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, BtnEdit, FolderCtrls, ImgList;

{ TFolderBarsForm }

type
  TFolderBarsForm = class(TForm)
    GroupBox: TGroupBox;
    CaptionEdit: TEdit;
    CaptionLabel: TLabel;
    OKButtopn: TButton;
    CancelButton: TButton;
    NewItemButton: TButton;
    RemoveButton: TButton;
    IndexLabel: TLabel;
    IndexEdit: TEdit;
    IndexSpin: TUpDown;
    TreeView: TTreeView;
    VisibleBox: TCheckBox;
    EnabledBox: TCheckBox;
    SelectedIndexLabel: TLabel;
    ImageIndexLabel: TLabel;
    NewBarButton: TButton;
    SelectedIndexEdit: TImageListEdit;
    ImageIndexEdit: TImageListEdit;
    procedure FormCreate(Sender: TObject);
    procedure NewItemButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure IndexSpinChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure NewBarButtonClick(Sender: TObject);
    procedure CaptionEditChange(Sender: TObject);
    procedure EnabledBoxClick(Sender: TObject);
    procedure VisibleBoxClick(Sender: TObject);
    procedure ImageIndexSpinChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure TreeViewEnter(Sender: TObject);
    procedure SelectedIndexSpinChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: Smallint;
      Direction: TUpDownDirection);
    procedure ImageIndexEditChange(Sender: TObject);
    procedure SelectedIndexEditChange(Sender: TObject);
  private
    FFolderBars: TFolderBars;
    FFolderBar: TFolderBar;
    FFolderItem: TFolderItem;
    FFolderImages: TCustomImageList;
    FItemImages: TCustomImageList;
    procedure SetFolderBars(Value: TFolderBars);
  public
    property FolderBars: TFolderBars read FFolderBars write SetFolderBars;
  end;

function EditFolderBars(Bars: TFolderBars): Boolean;

implementation

{$R *.DFM}

{ TFolderBarsForm }

procedure TFolderBarsForm.FormCreate(Sender: TObject);
begin
  ClientHeight := CancelButton.Top + 32;
  ClientWidth := TreeView.Left + CancelButton.Left + CancelButton.Width;
  FFolderBars := TFolderBars.Create(Self);
end;

procedure TFolderBarsForm.SetFolderBars(Value: TFolderBars);
var
  Node, Child: TTreeNode;
  I, J: Integer;
begin
  if Value <> nil then
  begin
  	if Value.Control <> nil then
    begin
    	FFolderImages :=	TCustomImageList(
      	Value.Control.Perform(CM_FOLDERIMAGES, 0, 0));
  		FItemImages := TCustomImageList(
      	Value.Control.Perform(CM_ITEMIMAGES, 0, 0));
    end;
    FFolderBars.Assign(Value);
    with TreeView.Items do
    begin
      BeginUpdate;
      Clear;
      for I := 0 to FFolderBars.Count - 1 do
      begin
        Node := TreeView.Items.AddChild(nil, FFolderBars.Items[I].Caption);
        Node.Data := FFolderBars.Items[I];
        for J := 0 to FFolderBars.Items[I].Items.Count - 1 do
        begin
          Child := TreeView.Items.AddChild(Node,
            FFolderBars.Items[I].Items[J].Caption);
          Child.Data := FFolderBars.Items[I].Items[J];
        end;
      end;
      EndUpdate;
    end;
  end;
end;

procedure TFolderBarsForm.IndexSpinChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
var
  Node: TTreeNode;
  OldIndex: Integer;
  Expanded: Boolean;
begin
  AllowChange := False;
  Node := TreeView.Selected;
  if Node = nil then Exit;
  if FFolderBar <> nil then
  begin
    AllowChange := (NewValue > -1) and (NewValue < FFolderBar.Collection.Count);
    if AllowChange then
    begin
      OldIndex := FFolderBar.Index;
      Expanded := Node.Expanded;
      FFolderBar.Index;
      FFolderBar.Index := NewValue;
      TreeView.OnChange := nil;
      if FFolderBar.Index = 0 then
        Node.MoveTo(nil, naAddFirst)
      else if FFolderBar.Index = FFolderBar.Collection.Count - 1 then
        Node.MoveTo(nil, naAdd)
      else if OldIndex > FFolderBar.Index then
        Node.MoveTo(Node.getPrevSibling, naInsert)
      else
        Node.MoveTo(Node.GetNextSibling.GetNextSibling, naInsert);
      if Expanded then
        Node.Expand(False);
      TreeView.OnChange := TreeViewChange;
    end;
  end
  else if FFolderItem <> nil then
  begin
    AllowChange := (NewValue > -1) and (NewValue < FFolderItem.Collection.Count);
    if AllowChange then
    begin
      OldIndex := FFolderItem.Index;
      FFolderItem.Index := NewValue;
      Node := Node.Parent;
      Node[FFolderItem.Index].Text := FFolderItem.Caption;
      Node[FFolderItem.Index].Data := FFolderItem;
      Node[OldIndex].Text := TFolderItem(FFolderItem.Collection.Items[OldIndex]).Caption;
      Node[OldIndex].Data := FFolderItem.Collection.Items[OldIndex];
      TreeView.OnChange := nil;
      TreeView.Selected := Node[FFolderItem.Index];
      TreeView.OnChange := TreeViewChange;
    end;
  end;
end;

function EditFolderBars(Bars: TFolderBars): Boolean;
begin
	with TFolderBarsForm.Create(Application) do
	try
  	FolderBars := Bars;
    Result := ShowModal = mrOK;
    if Result then
      Bars.Assign(FolderBars);
  finally
  	Free;
  end;
end;

procedure TFolderBarsForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  IndexSpin.OnChangingEx := nil;
  IndexEdit.OnChange := nil;
  ImageIndexEdit.OnChange := nil;
  SelectedIndexEdit.OnChange := nil;
  if (Node <> nil) and (TObject(Node.Data) is TFolderBar) then
  begin
    IndexSpin.OnChangingEx := nil;
    FFolderBar := TFolderBar(Node.Data);
    FFolderItem := nil;
    GroupBox.Caption := 'Folder Bar:';
    CaptionEdit.Text := FFolderBar.Caption;
    EnabledBox.Checked := True;
    EnabledBox.Enabled := False;
    VisibleBox.Checked := FFolderBar.Visible;
    ImageIndexEdit.Images := FFolderImages;
    ImageIndexEdit.ImageIndex := FFolderBar.ImageIndex;
    SelectedIndexLabel.Enabled := True;
    SelectedIndexEdit.Enabled := True;
    SelectedIndexEdit.Images := FFolderImages;
    SelectedIndexEdit.ImageIndex := FFolderBar.SelectedIndex;
    IndexSpin.Position := FFolderBar.Index;
    RemoveButton.Enabled := True;
    IndexSpin.OnChangingEx := IndexSpinChangingEx;
  end
  else if (Node <> nil) and (TObject(Node.Data) is TFolderItem) then
  begin
    FFolderItem := TFolderItem(Node.Data);
    FFolderBar := nil;
    GroupBox.Caption := 'Folder Item:';
    CaptionEdit.Text := FFolderItem.Caption;
    EnabledBox.Checked := FFolderItem.Enabled;
    EnabledBox.Enabled := True;
    VisibleBox.Checked := FFolderItem.Visible;
    ImageIndexEdit.Images := FItemImages;
    ImageIndexEdit.ImageIndex := FFolderItem.ImageIndex;
    IndexSpin.Position := FFolderItem.Index;
    SelectedIndexEdit.Images := nil;
    SelectedIndexEdit.ImageIndex := -1;
    SelectedIndexLabel.Enabled := False;
    SelectedIndexEdit.Enabled := False;
    RemoveButton.Enabled := True;
    IndexSpin.OnChangingEx := IndexSpinChangingEx;
  end
  else
  begin
    IndexSpin.OnChangingEx := nil;
    FFolderBar := nil;
    FFolderItem := nil;
    CaptionEdit.Text := '';
    EnabledBox.Checked := True;
    EnabledBox.Enabled := True;
    VisibleBox.Checked := True;
    ImageIndexEdit.Images := nil;
    ImageIndexEdit.ImageIndex := -1;
    SelectedIndexLabel.Enabled := True;
    SelectedIndexEdit.Enabled := True;
    SelectedIndexEdit.Images := nil;
    SelectedIndexEdit.ImageIndex := -1;
    NewItemButton.Enabled := False;
    RemoveButton.Enabled := False;
    IndexSpin.Position := 0;
  end;
  IndexSpin.OnChangingEx := IndexSpinChangingEx;
  ImageIndexEdit.OnChange := ImageIndexEditChange;
  SelectedIndexEdit.OnChange := SelectedIndexEditChange;
end;

procedure TFolderBarsForm.NewItemButtonClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  if Node = nil then Exit;
  if Node.Parent <> nil then Node := Node.Parent;
  FFolderItem := TFolderBar(Node.Data).Items.Add;
  FFolderItem.Caption := 'New Folder Item';
  Node := TreeView.Items.AddChild(Node, FFolderItem.Caption);
  Node.Data := FFolderItem;
  TreeView.Selected := Node;
end;

procedure TFolderBarsForm.NewBarButtonClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  FFolderBar := FFolderBars.Add;
  FFolderBar.Caption := 'New Folder Bar';
  Node := TreeView.Items.AddChild(nil, FFolderBar.Caption);
  Node.Data := FFolderBar;
  TreeView.Selected := Node;
end;

procedure TFolderBarsForm.RemoveButtonClick(Sender: TObject);
begin
  if TreeView.Selected <> nil then
  begin
    FFolderBar.Free;
    FFolderBar := nil;
    FFolderItem.Free;
    FFolderItem := nil;
    TreeView.Selected.Free;
  end;
end;

procedure TFolderBarsForm.CaptionEditChange(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  if Node = nil then Exit;
  if FFolderBar <> nil then
  begin
    FFolderBar.Caption := CaptionEdit.Text;
    Node.Text := FFolderBar.Caption;
  end
  else if FFolderItem <> nil then
  begin
    FFolderItem.Caption := CaptionEdit.Text;
    Node.Text := FFolderItem.Caption;
  end;
end;

procedure TFolderBarsForm.EnabledBoxClick(Sender: TObject);
begin
  if FFolderItem <> nil then
    FFolderItem.Enabled := EnabledBox.Checked;
end;

procedure TFolderBarsForm.VisibleBoxClick(Sender: TObject);
begin
  if FFolderBar <> nil then
    FFolderBar.Visible := VisibleBox.Checked
  else if FFolderItem <> nil then
    FFolderItem.Visible := VisibleBox.Checked;
end;

procedure TFolderBarsForm.ImageIndexSpinChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  if FFolderBar <> nil then
    FFolderBar.ImageIndex := NewValue
  else if FFolderItem <> nil then
    FFolderItem.ImageIndex := NewValue;
  AllowChange := True;
end;

procedure TFolderBarsForm.TreeViewEnter(Sender: TObject);
begin
  if (TreeView.Selected = nil) and (TreeView.Items.Count > 0) then
    TreeView.Selected := TreeView.Items[0];
end;

procedure TFolderBarsForm.SelectedIndexSpinChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint;
  Direction: TUpDownDirection);
begin
  if FFolderBar <> nil then
    FFolderBar.SelectedIndex := NewValue;
  AllowChange := True;
end;

procedure TFolderBarsForm.ImageIndexEditChange(Sender: TObject);
begin
  if FFolderBar <> nil then
    FFolderBar.ImageIndex := ImageIndexEdit.ImageIndex
  else if FFolderItem <> nil then
    FFolderItem.ImageIndex := ImageIndexEdit.ImageIndex;
end;

procedure TFolderBarsForm.SelectedIndexEditChange(Sender: TObject);
begin
  if FFolderBar <> nil then
    FFolderBar.SelectedIndex := SelectedIndexEdit.ImageIndex;
end;

end.
