//----------------------------------------------------------------------------
// Unit Name: pb_CategoriesForm
// Author:    Helli
// Date:      06.06.2003
// Purpose:
// History:
//----------------------------------------------------------------------------
//  Copyright © 2003 by Hellinger Software.  All Rights Reserved.
//----------------------------------------------------------------------------

unit pb_Categories;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, Registry, ComCtrls, ImgList;

type
  TPaletteCategories = class(TForm)
    MetaList: TTreeView;
    BtnAdd: TButton;
    BtnDel: TButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure MetaListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BtnAddClick(Sender: TObject);
    procedure MetaListEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure BtnDelClick(Sender: TObject);
    procedure MetaListChange(Sender: TObject; Node: TTreeNode);
    procedure MetaListEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure MetaListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MetaListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MetaListStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure MetaListEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
   FOtherStr: string;
   FOtherNode: TTreeNode;
   FDragging: Boolean;
   FScrolling: Boolean;
   function GetTreenode (const s: string): TTreeNode;
   function IsOther (Node: TTreeNode): Boolean;
   function IsDuplicate (Node: TTreeNode; s: string): Boolean;
   procedure Delay (ms: Cardinal);
  public
    procedure SaveCategories;
  end;


implementation

{$R *.dfm}

uses pb_master, pb_Common;


const cMetaCat =    17;
      cMetaCatSel = 18;
      cSubCat =     19;


procedure TPaletteCategories.Delay (ms: Cardinal);
var t: Cardinal;
begin
 t:= GetTickCount + ms;
 while GetTickCount < t do Application.ProcessMessages;
end;

function TPaletteCategories.GetTreenode (const s: string): TTreeNode;
var i: Integer;
begin
 Result:= nil;
 for i:= 0 to Pred (MetaList.Items.Count) do begin
  if MetaList.Items[i].Text = s then begin
   Result:= MetaList.Items[i];
   Exit;
  end;
 end;
end;

function TPaletteCategories.IsDuplicate (Node: TTreeNode; s: string): Boolean;
var tn, sn: TTreeNode;
begin
 Result:= False;
 if not Assigned (Node) then Exit;
 if Assigned (Node.Parent) then tn:= Node.Parent
                           else tn:= Node;
 if tn.HasChildren then begin
  sn:= tn.getFirstChild;
  repeat
   if Assigned (sn) and AnsiSameText (sn.Text, s) then begin
    Result:= True;
    Exit;
   end;
   sn:= tn.GetNextChild (sn);
  until (sn = nil);
 end;
end;


procedure TPaletteCategories.FormShow(Sender: TObject);
var sl, tabcat: TStringList;
    i, j: Integer;
    tn: TTreeNode;
begin
 // Sprache anpassen
 Caption:= GetLangStr (ltCfgCategory);
 BtnOk.Caption:= GetLangStr (ltOKCaption);
 BtnCancel.Caption:= GetLangStr (ltCancelCaption);
 BtnAdd.Caption:= GetLangStr (ltAddCaption);
 BtnDel.Caption:= GetLangStr (ltDelCaption);

 FOtherStr:= GetLangStr (ltDefCategory);
 FOtherNode:= nil;
 FScrolling:= False;
 FDragging:= False;

 // Kategorien
 MetaList.Items.BeginUpdate;
 tabcat:= TStringList.Create;
 sl:= TStringList.Create;
 sl.Sorted:= True;
 try
  // Erst die Hauptkategorien füllen
  Pbar.GetCategories (sl);
  sl.Add(FotherStr);
  for i:= 0 to Pred (sl.Count) do begin
   tn:= MetaList.Items.Add(nil, sl[i]);
   tn.ImageIndex:= cMetaCat;
   tn.SelectedIndex:= cMetaCatSel;
  end;
  // Subkategorien
  sl.Clear;
  sl.Assign (PBar.GetTabs);
  sl.Sorted:= True;
  for i:= 0 to Pred (sl.Count) do begin
   PBar.GetCategory (sl[i], tabcat);
   if tabcat.Count = 0 then  tabcat.Add (FOtherStr);
   for j:= 0 to Pred (tabcat.Count) do begin
    tn:= GetTreenode (tabcat[j]);
    if Assigned (tn) then begin
     if tn.ImageIndex = cMetaCat then begin
      tn:= MetaList.Items.AddChild(tn, sl[i]);
      tn.ImageIndex:= cSubCat;
      tn.SelectedIndex:= cSubCat;
     end;
    end;
   end;
  end;

 finally
  sl.Free;
  tabcat.Free;
  MetaList.Items.EndUpdate;
  FOtherNode:= GetTreeNode (FOtherStr);
 end;
end;


procedure TPaletteCategories.SaveCategories;
var i: Integer;
    sl: TStringList;
    s, s1, s2: string;
    r: TRegIniFile;
begin
 s:= '';
 sl:= TStringList.Create;
 r:= TRegIniFile.Create (cRegKey);
 try
  r.EraseSection (cRegCategory);
  for i:= 0 to Pred (MetaList.Items.Count) do begin
   if MetaList.Items[i].ImageIndex = cMetaCat then begin
    s:= MetaList.Items[i].Text;
   end else if (s <> FotherStr) then begin
    s1:= r.ReadString(cRegCategory, MetaList.Items[i].Text, '');
    if s1 = '' then s2:= s else s2:= s1 + ',' + s;
    r.WriteString (cRegCategory, MetaList.Items[i].Text, s2);
   end;
  end;
 finally
  r.Free;
  sl.Free;
 end;
end;

procedure TPaletteCategories.BtnAddClick(Sender: TObject);
var tn: TTreeNode;
begin
 MetaList.Items.BeginUpdate;
 tn:= MetaList.Items.Add(nil, GetLangStr (ltNewEntry));
 tn.ImageIndex:= cMetaCat;
 tn.SelectedIndex:= cMetaCatSel;
 MetaList.Items.EndUpdate;
 tn.EditText;
end;

procedure TPaletteCategories.BtnDelClick(Sender: TObject);
var s: string;
    t1, t2: TTreeNode;
begin
 MetaList.Items.BeginUpdate;
 if Assigned (MetaList.Selected) then begin
  if (MetaList.Selected.ImageIndex = 1) then begin
   s:= '"' + MetaList.Selected.Text + '"'#13#10 + GetLangStr (ltDeleteReally);
   if MessageDlg(s, mtConfirmation, [mbYes,mbNo], 0) = mrYes then begin
    // Alle Inhalts-Tabs nach "Sonstige" verschieben!
    t1:= MetaList.Selected.GetLastChild;
    while Assigned (t1) do begin
     t2:= t1;
     t1:= MetaList.Selected.GetPrevChild(t2);
     if not IsDuplicate (FOtherNode, t2.Text) then begin
      t2.MoveTo(FOtherNode, naAddChild);
     end else begin
      MetaList.Items.Delete(t2);
     end;
    end;
    MetaList.Items.Delete(MetaList.Selected);
   end;
  end else begin // nach FOther verschieben
   if not IsDuplicate (FOtherNode, MetaList.Selected.Text) then begin
    MetaList.Selected.MoveTo(FOtherNode, naAddChild);
   end else begin
    MetaList.Items.Delete(MetaList.Selected);
   end;
  end;
  FOtherNode.AlphaSort(false);
 end;
 MetaList.Items.EndUpdate;
end;

procedure TPaletteCategories.MetaListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var tn: TTreeNode;
begin
 Accept:= False;
 tn:= MetaList.GetNodeAt(x, y);
 if Assigned (tn) then  Accept:= not IsOther(tn);
 FScrolling:= False;
end;

procedure TPaletteCategories.MetaListEdited(Sender: TObject;
  Node: TTreeNode; var S: String);
begin
 MetaList.AlphaSort(False);
 BtnAdd.Enabled:= True;
end;

procedure TPaletteCategories.MetaListChange(Sender: TObject;
  Node: TTreeNode);
begin
 BtnDel.Enabled:= not IsOther(Node);
end;

function TPaletteCategories.IsOther(Node: TTreeNode): Boolean;
begin
 Result:= False;
 if Assigned (Node) then begin
  Result:= (Node = FOtherNode) or (Node.Parent = FOtherNode);
 end;
end;

procedure TPaletteCategories.MetaListEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
 AllowEdit:= (not IsOther(Node)) or (Node.ImageIndex = cMetaCat);
 BtnAdd.Enabled:= not AllowEdit;
end;

procedure TPaletteCategories.MetaListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var tn, sn, nn: TTreeNode;
    sl, sr: Byte;
begin
 FDragging:= False;
 FScrolling:= False;
 tn:= MetaList.GetNodeAt (X, Y);
 sn:= MetaList.Selected;
 if (not Assigned (tn)) or
    (tn = sn) or
    (tn = sn.Parent) or
    (sn.Level = 0) then begin
  MetaList.EndDrag (False);
  Exit;
 end;

 MetaList.Items.BeginUpdate;

 // Shift-Tasten gedrückt?
 sl:= Windows.GetKeyState(VK_LSHIFT);
 sr:= Windows.GetKeyState(VK_RSHIFT);

 if not IsDuplicate (tn, sn.Text) then begin
  if (sl < 128) and (sr < 128) then begin // Verschieben
   if tn.ImageIndex = cMetaCat then  sn.MoveTo(tn, naAddChild)
                               else  sn.MoveTo(tn, naAdd);
  end else begin
   if tn.ImageIndex = cMetaCat then begin
    nn:= MetaList.Items.AddChild(tn, sn.Text);
    nn.ImageIndex:= cSubCat;
    nn.SelectedIndex:= cSubCat;
   end else begin
    nn:= MetaList.Items.Add(tn, sn.Text);
    nn.ImageIndex:= cSubCat;
    nn.SelectedIndex:= cSubCat;
   end;
  end;
 end else begin
  MessageBeep (MB_ICONHAND);
 end;

 MetaList.Items.EndUpdate;
 MetaList.AlphaSort(true);
end;

procedure TPaletteCategories.MetaListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 case key of
  vk_F2:     if Assigned (MetaList.Selected) then begin
              if (not IsOther(MetaList.Selected)) and
                 (MetaList.Selected.ImageIndex = cMetaCat) then begin
               MetaList.Selected.EditText
              end;
             end;
  vk_Insert: BtnAddClick(Sender);
  vk_Delete: if BtnDel.Enabled then BtnDelClick (Sender);
 end;
end;


procedure TPaletteCategories.MetaListStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
 FDragging:= True;
 FScrolling:= False;
end;

procedure TPaletteCategories.MetaListEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
 FDragging:= False;
 FScrolling:= False;
end;

procedure TPaletteCategories.Label1MouseEnter(Sender: TObject);
begin
 if FDragging then begin

  FScrolling:= True;
  Repeat
   if ((Sender as TLabel).Name = 'Label1') then
     MetaList.Perform (WM_VSCROLL, SB_LINEUP, 0);
   if ((Sender as TLabel).Name = 'Label2') then
     MetaList.Perform (WM_VSCROLL, SB_LINEDOWN, 0);
   Delay (25);
  until not FScrolling;
 end;
end;

procedure TPaletteCategories.Label1MouseLeave(Sender: TObject);
begin
 FScrolling:= False;
end;

procedure TPaletteCategories.Button1Click(Sender: TObject);
begin
 SaveCategories;
end;

end.
