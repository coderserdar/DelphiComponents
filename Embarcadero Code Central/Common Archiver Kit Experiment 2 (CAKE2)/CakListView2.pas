unit CakListView2;

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

// ver 0.1.2.0
// lastupdate 10.17.2003
// ver 0.1.2.0 - Update OwnerData and allow column customization.
// ver 0.1.2.0 - Save and Loading columns data.
// ver 0.1.2.0 - Sort when click on columns.
// ver 0.1.2.0 - Column translations (event)

interface
{$DEFINE DRAGnDROP}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CakDir2, CakDefs2, CakUtils2
  {$IFDEF DRAGnDROP},DragDropFile, DragDrop{$ENDIF};

const MaxColType = 9;
      listwidth : array[0..MaxColType-1] of integer = (150,100,50,120,50,40,70,200,200);
      listname  : array[0..MaxColType-1] of string =
                  ('File name','Type','Size','Time',
                  'Comp','%','CRC',
                  'Defpath','FileArchive');
      deffilelist = ' 0_ 150_0| 1_ 100_0| 2_  50_0| 3_ 120_0| 4_  50_0| 5_  40_0| 6_  70_0| 7_ 200_0| 8_ 200_0|';
type

  ColRec = record
           ColType  : -1..MaxColType-1;     //-1 = None
           Width    : integer;
           Autosize : boolean;
           end;

  TCakListView2 = class(TListView)
  private
    { Private declarations }
    FOnData : TLVOwnerDataEvent;
    FOnReqColCap : TCReqColumnCaptionEvent;
    updSel : boolean;
    UserCol      : Array[0..MaxColType-1] of ColRec;
    DragPoint : TPoint;
    AlreadyDragging : boolean;
    {$IFDEF DRAGnDROP} DropFileSource : TDropFileSource; {$ENDIF}

    procedure LVData(Sender: TObject; Item: TListItem);
    procedure LVColumnClick(Sender: TObject;Column: TListColumn);
  protected
    { Protected declarations }
  public
    CakDir : TCakDir2;
    Passive : Boolean;
    procedure ReloadCAK;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure SetCakdir(aCakdir : TCakdir2);

    procedure DefaultCol;
    procedure UpdateCol;
    procedure ResetCol;
    procedure UpdateColWidth;
    procedure AddCol(ColType, DefWidth : integer; Autosize : boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    {$IFDEF DRAGnDROP}
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure DropFileSourceDrop(Sender: TObject; DragType: TDragType; var ContinueDrop: Boolean);
    procedure Copy2Clipboard;
    {$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function  SaveColSetting : string;
    procedure LoadColSetting(Value : string);
    procedure DblClick; override;
    { Public declarations }
  published
     property CakDir2 : TCakDir2 read CakDir write SetCakDir;
     property OnLVData : TLVOwnerDataEvent read FOnData write FOnData;
     property OnRequestColCapt : TCReqColumnCaptionEvent read FOnReqColCap write FOnReqColCap;
     property PassiveMode : boolean read Passive write Passive;
     property UpdateSelection : boolean read updSel write updSel;
    { Published declarations }
  end;

procedure Register;

implementation

procedure TCakListView2.ReloadCAK;
begin
        if not assigned(CakDir) then exit;
//        UpdateCol;
        OwnerData   := true;
        SmallImages := CakDir.ImageS;
        LargeImages := Cakdir.ImageL;
        Items.Clear;
        Items.count := CakDir.Total_Contents;
        UpdateColWidth;
end;

constructor TCakListView2.Create( AOwner: TComponent );
begin
     inherited Create( AOwner );
     DefaultCol;
     {$IFDEF DRAGnDROP}
     DropFileSource := TDropFileSource.Create(Self);
     DropFileSource.OnDrop := DropFileSourceDrop;
     AlreadyDragging := false;
     {$ENDIF}
     Self.OnData := LVData;
     Self.OnColumnClick := LVColumnClick;
     MultiSelect := true;
     ReadOnly := True;
     Passive := false;
     updSel := true;
end;

destructor TCakListView2.Destroy;
begin
     {$IFDEF DRAGnDROP}DropFileSource.Free;{$ENDIF}
     inherited Destroy;
end;

procedure TCakListview2.LVData(Sender: TObject; Item: TListItem);
var i : integer;
function ReturnItemData(ColNum : integer) : string;
begin
        with Cakdir.Archive_Contents[Item.Index] do
        Case UserCol[ColNum].ColType of
        -1: Result := '';
        0 : Result := _FileName;
        1 : if _Encrypted then Result := '*' + _FileType
                          else Result := _FileType;
        2 : Result := FormatSize(_FileSize);
        3 : Result := Datetimetostr(_Filetime);
        4 : Result := FormatSize(_FilePackedSize);
        5 : Result := inttostr(_FileRatio) + '%';
        6 : Result := _FileCRC;
        7 : Result := _FileDefpath;
        8 : Result := _FileArchive;
        else Result := '';
        end;
end;

begin
        If Item.Index >= CakDir.Total_Contents then exit;
        if UserCol[0].ColType <> -1 then
        begin
          Item.Caption := ReturnItemData(0);
          Item.ImageIndex := Cakdir.Archive_Contents[Item.Index]._FileIcon;
        end;
        for i := 1 to MaxColType-1 do
        if UserCol[i].ColType <> -1 then
          Item.SubItems.Add(ReturnItemData(i));
        if Assigned(FONData) then
                FOnData(Sender,Item);
end;

procedure TCakListview2.SetCakdir(aCakdir : TCakdir2);
begin
        Cakdir := aCakdir;
        if assigned(Cakdir) then
        Cakdir._SetListview(Self);
end;

procedure TCakListview2.KeyDown(var Key: Word; Shift: TShiftState);
var
  Item: TListItem;
begin
        Cakdir.UnSelectALL;
        Item := Self.Selected;
        while Item <> nil do
        begin
          Cakdir.Archive_Contents[Item.Index]._Selected := True;
          Item := Self.GetNextItem(Item, sdAll, [isSelected]);
       end;

       inherited KeyDown(Key,Shift);
end;

procedure TCakListview2.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: TListItem;
begin
        Cakdir.UnSelectALL;
        Item := Self.Selected;
        if UpdSel then
        while Item <> nil do
        begin
          Cakdir.Archive_Contents[Item.Index]._Selected := True;
          Item := Self.GetNextItem(Item, sdAll, [isSelected]);
       end;
       DragPoint := Point(X,Y);
       inherited Mousedown(Button,Shift,X,Y);
end;

{$IFDEF DRAGnDROP}
procedure TCakListview2.MouseMove(Shift: TShiftState; X,Y: Integer);
var b : boolean;
    temppath : string;
    i : integer;
begin
  if not AlreadyDragging  then
  begin
  Shift := (Shift * [ssLeft,ssRight]);

  b := false;
  if  (Shift = []) or
      ((abs(DragPoint.X - X) <10) and
      (abs(DragPoint.Y - Y) <10)) then
    b := true;

  if not b and (CakDir. Get_Selected_Count > 0) then
  begin
  DropFileSource.Files.Clear;
  TempPath := grabtemppath;


  for i := 0 to CakDir.Total_Contents -1 do
        with CakDir.Archive_Contents[i] do
        if _Selected then
        DropFileSource.Files.Add(grabtemppath + _FileDefPath + _Filename);

  AlreadyDragging := true;
  DropFileSource.execute;
  AlreadyDragging := false;
  end;
  end;

end;

procedure TCakListview2.Copy2Clipboard;
var i : integer;
begin
   If CakDir.Get_Selected_Count > 0 then
        begin
        for i := 0 to CakDir.Total_Contents -1 do
        with Cakdir.Archive_Contents[i] do
        if _Selected then
        DropFileSource.Files.Add(grabtemppath + _FileDefPath + _Filename);
        Cakdir.Extractoptions.extr_to := GrabTempPath;
        Cakdir.Extractoptions.extr_DirNames := True;
        Cakdir.Extractoptions.extr_OverWrite := True;
        Cakdir.extract;
        if DropFileSource.Files.Count > 0 then
          DropFileSource.CopyToClipboard;
        end;
end;
{$ENDIF}

procedure TCakListview2.UpdateCol;
var aListcolumn : TListColumn;
    i : integer;
    Capt : string;
function IndexedCol : integer;
begin
    Result := -1;
    if Assigned(CakDir) then
    Case Cakdir.SortIndex of
    _FName    : Result := 0;
    _FType    : Result := 1;
    _FSize    : Result := 2;
    _FTime    : Result := 3;
    _FPSize   : Result := 4;
    _FRatio   : Result := 5;
    _FCRC     : Result := 6;
    _FDefPath : Result := 7;
    _FArchive : Result := 8;
    else Result := -1;
    end;
end;
begin
     Columns.BeginUpdate;
     Columns.Clear;
     for i := 0 to MaxColType-1 do
     if UserCol[i].ColType <> -1 then
     begin
        aListcolumn          := Columns.Add;
        aListcolumn.Width    := UserCol[i].Width;
        Capt := Listname[UserCol[i].Coltype];
       if Assigned(FOnReqColCap) then
          FOnReqColCap(Self,UserCol[i].Coltype,Capt);
        if UserCol[i].ColType = IndexedCol then
        if Cakdir.SortAccend then
          aListcolumn.Caption  := Capt + ' [a]' else
          aListcolumn.Caption  := Capt + ' [z]' else
        aListcolumn.Caption  := Capt;
        aListColumn.AutoSize := UserCol[i].Autosize;
     end;
     Columns.EndUpdate;
end;

procedure TCakListview2.DefaultCol;
var i : integer;
begin
     ResetCol;
     for i := 0 to MaxColType-1 do
     begin
        UserCol[i].ColType   := i;
        UserCol[i].Width     := ListWidth[i];
        UserCol[i].Autosize  := False;
     end;
end;

procedure TCakListview2.ResetCol;
var i : integer;
begin
     Columns.Clear;
     for i := 0 to MaxColType-1 do
        UserCol[i].ColType   := -1;
end;

procedure TCakListview2.AddCol(ColType, DefWidth : integer; Autosize : boolean);
var i : integer;
begin
     i := 0;
     while (UserCol[i].ColType <> -1) and (i <= MaxColType-1) do
        Inc(i);
     if (UserCol[i].ColType = -1) then
        begin
        UserCol[i].ColType := ColType;
        UserCol[i].Width := DefWidth;
        UserCol[i].Autosize := Autosize;
        end;
end;

procedure TCakListview2.UpdateColWidth;
var i : integer;
begin
    for i := 0 to MaxColType-1 do
        if UserCol[i].ColType <> -1 then
        if Columns.Count > i then
        UserCol[i].Width := Columns.Items[i].Width;
end;


function TCakListView2.SaveColSetting : string;
// Sample ColSetting;
// "99_0100_1_" <-AutoSize 1 = true
//  /\  ^Width
//  |_ColType
var i, AutoSe : integer;
    aSetting : string[10];
begin
    UpdateColWidth;
    for i := 0 to MaxColType-1 do
      begin
       AutoSe := 0;
       if UserCol[i].Autosize then AutoSe := 1;
       aSetting := Format('%2d_%4d_%1d|',[UserCol[i].ColType,
                                         UserCol[i].Width, autose]);
       Result := Result + aSetting;
      end;
    Result := Replace(Result,' ','_');
end;

procedure TCakListView2.LoadColSetting(Value : string);
var i : integer;
    aSetting : string[10];
    Val : string;
procedure LoadSetting;
var ColType, Width, AutoSe : integer;
begin
    if aSetting[10] = '|' then
      begin
         ColType := Strtointdef(Copy(aSetting,1,2),1);
         Width   := Strtointdef(Copy(aSetting,4,4),100);
         AutoSe  := Strtointdef(Copy(aSetting,9,1),0);
         if ColType <> -1 then
         AddCol(ColType,Width,(AutoSe=1));
      end;
end;
begin
    ResetCol;
    Val := Replace(Value,'_',' ');
    for i := 0 to MaxColType-1 do
    if Length(Val) > (i*10) then
      begin
        aSetting[10] := '-';
        aSetting := Copy(Val,(i*10)+1,10);
        LoadSetting;
      end;
    UpdateCol;
    if Columns.Count = 0 then DefaultCol;
end;

procedure TCakListView2.LVColumnClick(Sender: TObject;Column: TListColumn);
var sortType : SortByType;
begin
    Case Usercol[Column.Index].ColType of
    0 : SortType := _FName;
    1 : SortType := _FType;
    2 : SortType := _FSize;
    3 : SortType := _FTime;
    4 : SortType := _FPSize;
    5 : SortType := _FRatio;
    6 : SortType := _FCRC;
    7 : SortType := _FDefPath;
    8 : SortType := _FArchive;
    else SortType := _FName;
    end;
    if not Assigned(Cakdir) then exit;
    Cakdir.SortAuto := true;
    if Cakdir.SortIndex = SortType then
        Cakdir.SortAccend := not Cakdir.SortAccend else
        Cakdir.SortIndex := SortType;
    UpdateCol;
    Cakdir.Sort(Cakdir.SortAccend, Cakdir.SortIndex);
    ReloadCak;
end;

procedure TCakListView2.DblClick;
var k : string;
begin
    inherited DblClick;
    if Assigned(OnDblClick) then exit;
    if Cakdir.SelectedCount > 0 then
        k := Cakdir.Archive_Contents[Cakdir.SelectedIndex]._FileName else
        exit;

    Cakdir.Extractoptions.Extr_to := NewTempPath;
    Cakdir.Extractoptions.Extr_DirNames := False;
    Cakdir.Extract;
    Run(Appendslash(Cakdir.Extractoptions.Extr_to) +k,'');
end;
{$IFDEF DRAGnDROP}
procedure TCakListView2.DropFileSourceDrop(Sender: TObject;
  DragType: TDragType; var ContinueDrop: Boolean);
begin
        with Cakdir do
        begin
        Extractoptions.extr_to := GrabTempPath;
        Extractoptions.extr_DirNames := True;
        Extractoptions.extr_OverWrite := True;
        extract;
        end;
end;
{$ENDIF}
procedure Register;
begin
  RegisterComponents('CAKE', [TCakListView2]);
end;

end.
