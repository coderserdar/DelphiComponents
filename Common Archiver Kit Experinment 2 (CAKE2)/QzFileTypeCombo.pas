unit QzFileTypeCombo;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ShellApi, CakComboBoxEx, ExtCtrls, ImgList;

type
  TQzFileTypeCombo = class(TCakComboBoxEx)
  private
    { Private declarations }
    ListView1: TListView;
    ImageS: TImageList;
    CacheExt, CacheType : Tstrings;
    FExts : TStringList;

    aPanel : TPanel;
    procedure ListView1Click(Sender: TObject);
    function ReturnIconType(FileName: string): Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    ftype : string;
    fext  : string;
    Defext, DefType : string;
    findex: integer;
    TreeViewHeight : integer;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure FormDropDown(Sender : TObject);
    procedure Paint; override;

    procedure AddNewType(ext : string); overload;
    procedure AddNewType; overload;
    procedure ClearList;
  published
    { Published declarations }
    property FileExtensions : TStringList read fExts write fExts;
  end;

  procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('QZIP', [TQzFileTypeCombo]);
end;

constructor TQzFileTypeCombo.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  OnDroppedDown := FormDropDown;

  ImageS := TImageList.Create(DropDownForm);

  ListView1 := TListView.Create(DropDownForm);
  ListView1.Parent := DropDownForm;
  ListView1.Align := alClient;
  ListView1.OnClick := ListView1Click;
  Listview1.ViewStyle := vsReport;
  ListView1.SmallImages := ImageS;
  Listview1.ShowColumnHeaders := False;
  ListView1.Columns.Add;

  FileExtensions:= TStringList.Create;
  CacheType:= TStringList.Create;
  CacheExt:= TStringList.Create;
  DefExt := '*.*';
  DefType := 'All files';
//  ftype := DefType;
//  fExt := DefExt;
end;

destructor TQzFileTypeCombo.Destroy;
begin
  ListView1.Free;
  ImageS.Free;
  CacheExt.Free;
  CacheType.Free;
  FExts.Free;
  inherited Destroy;
end;

procedure TQzFileTypeCombo.FormDropDown(Sender : TObject);
var i : integer;
begin
  FileExtensions.Sort;
  ClearList;
  AddNewType;
  ListView1.Columns[0].Width := Self.Width - 12;
  for i := 0 to fexts.count -1 do
      AddNewType(fexts[i]);
end;

procedure TQzFileTypeCombo.paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,Width,Height));
  if Assigned(DropDownForm) then
  if ftype <> '' then
  begin
      if findex >= 0 then
      ImageS.Draw(Canvas,2,0,findex);
      Canvas.TextOut(20,2,Format('%s (%s)',[ftype,fext]))
  end else
  begin
      Canvas.TextOut(20,2,Format('%s (%s)',[deftype,defext]));
      Canvas.Draw(2,0,IconBitmap);
  end;
end;

function TQzFileTypeCombo.ReturnIconType(FileName: string): Integer;
var
  loc:  Integer;
  Ext:  String;
  shinfo: TSHFileInfo;
  Icon: TIcon;
begin
  Icon := TIcon.Create();
  Ext := ExtractFileExt(FileName);
  loc := CacheExt.IndexOf(Ext);
  if (loc = -1) then
   begin
    SHGetFileInfo(PChar(Ext), 0, shInfo, SizeOf(shInfo), // GC -Oct 14 2001
    (SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES) or
    (SHGFI_ICON or SHGFI_TYPENAME));
    Icon.Handle := shinfo.hIcon;
    loc := ImageS.AddIcon(Icon);
    //loc := ImageL.AddIcon(Icon);
    CacheType.Add(Shinfo.szTypeName);
    CacheExt.Add(Ext);
   end;
  Result := loc;
  Icon.Free;
end;

procedure TQzFileTypeCombo.AddNewType(ext : string);
var ListItem : TListItem;
begin
  if CacheExt.IndexOf(ext) <> -1 then exit;
  ListItem := Listview1.Items.Add;
  Listitem.ImageIndex := ReturnIconType(ext);
  ListItem.Caption := Format('%s (%s)',[CacheType.Strings[Listitem.ImageIndex],CacheExt.Strings[ListItem.ImageIndex]]);
end;

procedure TQzFileTypeCombo.AddNewType;
var ListItem : TListItem;
begin
  ListItem := Listview1.Items.Add;
  if IconBitmap.Width = ImageS.Width then
  if IconBitmap.Height = ImageS.Height then
  begin
  Listitem.ImageIndex := ImageS.Add(IconBitmap,nil);
  CacheExt.Add(DefExt);
  CacheType.Add(DefType);
  end;

  Listitem.ImageIndex := -1;
  ListItem.Caption := Format('%s (%s)',[DefType, DefExt]);
end;

procedure TQzFileTypeCombo.ListView1Click(Sender: TObject);
begin
  if Listview1.Selected <> nil then
  if Listview1.Selected.Index <> 0 then
  begin
  fext := CacheExt.Strings[Listview1.Selected.ImageIndex];
  ftype := CacheType.Strings[Listview1.Selected.ImageIndex];
  findex := Listview1.Selected.ImageIndex;
  DropDownForm.Close;
  if Assigned(OnChanged) then
    OnChanged(Sender);
  end else
  begin
  fext := '';
  ftype := '';
  findex := -1;
  DropDownForm.Close;
  if Assigned(OnChanged) then
    OnChanged(Sender);
  end;
  Invalidate;

end;

procedure TQzFileTypeCombo.ClearList;
begin
  ListView1.Items.Clear;
  ImageS.Clear;
  CacheExt.Clear;
  CacheType.Clear;
end;

end.
