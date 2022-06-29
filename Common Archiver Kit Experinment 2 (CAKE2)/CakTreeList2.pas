unit CakTreeList2;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls,
  Graphics, CakDir2, CakUtils2, CakDefs2;

type
  TCakTreeList2 = class(TListview)
  private
    { Private declarations }
   // FOnMouseDown : TMouseEvent;
    FOnData : TLVOwnerDataEvent;
    FOnReqColCap : TCReqColumnCaptionEvent;
    DirListStartAt : Integer;
   // procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure TLData(Sender: TObject; Item: TListItem);
    procedure SetCakdir(aCakdir : TCakdir2);
   // Property OnMouseDown;
    //Property OnData;
  protected
    { Protected declarations }
  public
    { Public declarations }
    TransUpText : string;
    Passive : Boolean;
    CakDir : TCakDir2;
    BaseDir : string;
    procedure UpdateCol;
    procedure ReloadCAK;
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure DblClick; override;

  published
    { Published declarations }
    property PassiveMode : boolean read Passive write Passive;
    property CakDir2 : TCakDir2 read CakDir write SetCakDir;
    property OnLVData : TLVOwnerDataEvent read FOnData write FOnData;
    property TranslateUp : string read TransUpText write TransUpText;
    property OnRequestColCapt2 : TCReqColumnCaptionEvent read FOnReqColCap write FOnReqColCap;
  end;

procedure Register;

implementation

constructor TCakTreeList2.Create( AOwner: TComponent );
begin
  inherited Create(AOwner);
  Self.OnData := TLData;
  TransUpText := '<up>';
  ReadOnly := true;
  Passive := false;
end;

procedure TCakTreeList2.TLData(Sender: TObject; Item: TListItem);
var Dirsize : Longint;
begin
   if Item.Index-DirListStartAt < Cakdir.SubDirectoryList.count then
   if (Item.Index = 0) and (DirListStartAt = 1) then
      Item.Caption    := TransUpText else
      begin
      Item.Caption    := Cakdir.SubDirectoryList.Strings[Item.Index-DirListStartAt];
      DirSize := Longint(Cakdir.SubDirectoryList.Objects[Item.Index-DirListStartAt]);
      Item.SubItems.Add(SizeinK(Dirsize));
      end;
   Item.ImageIndex := Cakdir.ReturnIconType('*DIR*');
end;

procedure TCakTreeList2.ReloadCAK;
begin
  UpdateCol;
  OwnerData := true;
  SmallImages := Cakdir.ImageS;
  LargeImages := Cakdir.ImageL;
  Items.Count := 0;
  Items.Clear;
  If (Cakdir.BaseDir = '') or (Cakdir.BaseDir = '\') then
    DirListStartAt := 0 else
    DirListStartAt := 1;
  Items.count := CakDir.SubDirectoryList.Count+DirListStartAt;
end;

destructor TCakTreeList2.Destroy;
begin
  inherited Destroy;
end;

procedure TCakTreeList2.SetCakdir(aCakdir : TCakdir2);
begin
   Cakdir := aCakdir;
   if assigned(Cakdir) then
   Cakdir._SetTreeList(Self);
end;

procedure TCakTreeList2.DblClick;
begin
  if Assigned(Selected) then
  begin
  with Cakdir do
  if (Selected.Index = 0) and (DirListStartAt = 1) then
  List(Appendslash(Extractfilepath(Removeslash(BaseDir)))+'*',False) else
  List(BaseDir + SubDirectoryList.strings[Selected.Index-DirListStartAt]+'\*',False);
  end;
end;

procedure TCakTreeList2.UpdateCol;
var aListcolumn : TListColumn;
    i : integer;
    Capt : string;
begin
     Columns.BeginUpdate;
     Items.Clear;
     Columns.Clear;
     aListcolumn          := Columns.Add;
     aListcolumn.Width    := Width-120;
     Capt := 'Directory';
     if Assigned(FOnReqColCap) then
        FOnReqColCap(Self,0,Capt);
     aListcolumn.Caption  := capt;

     aListcolumn          := Columns.Add;
     aListcolumn.Width    := 100;
     Capt := 'Size';
     if Assigned(FOnReqColCap) then
        FOnReqColCap(Self,1,Capt);
     aListcolumn.Caption  := capt;
     Columns.EndUpdate;
end;

procedure Register;
begin
  RegisterComponents('CAKE', [TCakTreeList2]);
end;

end.
