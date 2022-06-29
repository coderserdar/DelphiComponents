unit CakTreeView2;
// Common Archiver Kit (CAK) List View
// Common Interface for Compression/Decompression components.

//Copyright (C) Joseph Leung 2001 (lycj@yahoo.com)
//
//This library is free software; you can redistribute it and/or
//modify it under the terms of the GNU Lesser General Public
//License as published by the Free Software Foundation; either
//version 2.1 of the License, or (at your option) any later version.
//
//This library is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//Lesser General Public License for more details.
//
//You should have received a copy of the GNU Lesser General Public
//License along with this library; if not, write to the Free Software
//Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

// ver 0.1.1.0
// lastupdate 10.13.2003
// ver 0.1.1.0 - Icon load from res file
// ver 0.1.1.0 - Some update for CakTreeCombo2 support.

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, CakDir2, CakProcs, ImgList, ShellApi;

type
  TCAKTreeView2 = class(TTreeView)
  PRIVATE
    { Private declarations }
  // SysImageS: TImageList;
    MyImage  : TImageList;
  PROTECTED
    { Protected declarations }
  PUBLIC
    Passive, UpdCak : boolean;
    CakDir: TCakDir2;
    FTreeTopNode : string;
    DirNode: TTreeNode;
    procedure ReloadCAK;
    constructor Create(AOwner: TComponent); OVERRIDE;
    destructor Destroy; OVERRIDE;
    procedure add2tree(node: TTreeNode; Name, fullname: String);
    function getselectedpath: String;
    function GrabWindowPath : String;
    procedure setselectpath(path : string);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure SetCakdir(aCakdir : TCakdir2);
    { Public declarations }
  PUBLISHED
    property CakDir2 : TCakDir2 read CakDir write SetCakDir;
    property TreeTopNode: string READ FTreeTopNode WRITE FTreeTopNode;
    property PassiveMode : boolean read Passive write Passive;
    property UpdateCak : boolean read UpdCak write UpdCak;
    { Published declarations }
  end;

procedure Register;

implementation

procedure TCAKTreeView2.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
        if Assigned(CakDir) then
        if UpdateCak then
        Cakdir.List(GetSelectedPath+'*.*',(GetSelectedPath = ''));
        inherited Mousedown(Button,Shift,X,Y);
end;

function TCAKTreeView2.getselectedpath: String;
var
  k:         String;
  dummynode: ttreenode;
begin
  if not Assigned(Selected) then
        begin
          Result := '*.*';
          exit;
        end;
  if not selected.HasAsParent(Dirnode) then
    Result := ''
  else if selected = Dirnode then Result := ''
  else
  begin
    dummynode := selected;
    while not (dummynode = Dirnode) do
      begin
        k       := dummynode.Text + '\' + k;
      dummynode := dummynode.Parent;
    end;
    Result := k;
  end;
end;

procedure TCAKTreeView2.setselectpath(path : string);
var
  k:         String;
  dummynode: ttreenode;
  i,j        : integer;
function Lookfor(str : string) : integer;
var i : integer;
    node : Ttreenode;
begin
  Result := -1;
  for i := 0 to dummynode.Count -1 do
        begin
          node := dummynode.Item[i];
          if lowercase(node.Text) = lowercase(str) then
               begin
                Result := i;
                exit;
               end;
        end;
end;
begin
  dummynode := Dirnode;
  k := path;
  i := pos('\',k);
  while (k <> '') and (k <> '\') and (i <> 0) do
    begin
    j := LookFor(Copy(k,1,i-1));
    if j = -1 then
        begin
          Self.Selected := dummynode;
          exit;
        end else
    dummynode := dummynode.Item[j];
    k := Copy(k,i+1,length(k)-i);
    i := pos('\',k);
    end;
  Self.Selected := dummynode;
end;

procedure TCAKTreeView2.add2tree(node: TTreenode; Name, fullname: String);
var
  NewItem: TTreeNode;//TTReeNode;
  k, l:    String;
  i, j:    Integer;
begin
  k := ModifySlash(Name);

  if length(k) >= 1 then
    if k[1] = '\' then k := Copy(k, 2, length(k));
  if length(k) = 0 then exit;
  l := '';
  i := 0;
  begin
    while (i < length(k)) and (k[i] <> '\') do
    begin
      i := i + 1;
    end;
    if k[i] = '\' then
    begin
      l := Copy(k, i + 1, length(k) - i);
      k := Copy(k, 0, i - 1);
    end;
    j := 0;
    if (node.Count >= 1) and (j <= node.Count) then
      while (j < node.Count - 1) and (k <> node.Item[j].Text) do
      begin
        if assigned(node) then
          j := j + 1
        else
          exit;
      end;

    if node.Count > 0 then
      if k = node.Item[j].Text then
      begin
        if length(l) > 1 then
          add2tree(node.item[j], l, fullname);
        exit;
      end;

    NewItem      := Items.AddChild(node, k);
    Newitem.Text := k;
    Newitem.ImageIndex := 0;
    Newitem.SelectedIndex := 1;

    if length(l) > 1 then
      add2tree(newitem, l, fullname);
  end;
end;

procedure TCAKTreeView2.ReloadCAK;
var
  i: Integer;
begin

  items.Clear;
  DirNode      := items.Add(NIL, '{Dir View}');
  DirNode.Text := FTreeTopNode;//'{DirView}';
  DirNode.ImageIndex := 0;
  DirNode.SelectedIndex := 1;


  if not assigned(CakDir) then exit;
  if CakDir.DirectoryList.Count = 0 then exit;
  for i := 0 to CakDir.DirectoryList.Count - 1 do
    add2tree(Dirnode, CakDir.DirectoryList.strings[i],
      CakDir.DirectoryList.strings[i]);

  DirNode.Expand(False);
end;

function TCAKTreeView2.GrabWindowPath: String;
var
  Path: array [0..260] of Char;
begin
  GetWindowsDirectory(Path, Sizeof(Path));
  //Result := Appendslash(path);
  Result:= path;
end;


constructor TCAKTreeView2.Create(AOwner: TComponent);
var TmpBmp  : TBitmap;
begin
   inherited Create(AOwner);
   ReadOnly := true;
   MyImage := TImageList.Create(Self);
   MyImage.ShareImages := true;
   Passive := false;
   UpdCak := true;
   TmpBmp := TBitmap.Create;
   try
   TmpBmp.LoadFromResourceName(HInstance,'FOLDER_CLOSE_16');
   MyImage.Add(TmpBmp,nil);
   finally
   TmpBmp.Free;
   end;

   TmpBmp := TBitmap.Create;
   try
   TmpBmp.LoadFromResourceName(HInstance,'FOLDER_OPEN_16');
   MyImage.Add(TmpBmp,nil);
   finally
   TmpBmp.Free;
   end;

   Images := MyImage;
   FTreeTopNode := '{Dir View}';
end;

destructor TCAKTreeView2.Destroy;
begin
  Images := nil;
  FreeAndNil(MyImage);
  inherited Destroy;
end;

procedure TCakTreeview2.SetCakdir(aCakdir : TCakdir2);
begin
   Cakdir := aCakdir;
   if assigned(Cakdir) then
      Cakdir._SetTreeview(Self);
end;

procedure Register;
begin
  RegisterComponents('CAKE', [TCAKTreeView2]);
end;

end.