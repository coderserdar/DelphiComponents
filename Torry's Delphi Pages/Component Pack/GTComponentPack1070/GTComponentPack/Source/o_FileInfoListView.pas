{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtFileInfoListView                             }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_FileInfoListView;

interface
uses
   Classes
  ,Controls 
  ,ComCtrls
  ,o_FileInfo
  ;

type
  TgtFileInfoListView = class(TCustomListView)
  private
    FFileName: string;
    procedure SetFileName(const Value: string);
  {Private Declarations}
  protected
  {Protected Declarations}
    FFileInfo : TgtFileInfo;
    procedure FillListView;virtual;
    procedure SetParent(AParent: TWinControl);override;
  public
  {Public Declarations}
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  published
  {Published Declarations}
    property FileName : string read FFileName write SetFileName;
    property Items;
    property Columns;
    property Align;
    property Visible;
    property ReadOnly;
    property RowSelect;
    property PopUpMenu;
    property Color;
    property CheckBoxes;
  end;

implementation
uses
  SysUtils
  ;

{ TgtFileInfoListView }
{------------------------------------------------------------------------------}
constructor TgtFileInfoListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ViewStyle := vsReport;
  FFileInfo := TgtFileInfo.Create(Self);
  Width     := 254;
  Height    := 166;
end;
{------------------------------------------------------------------------------}
destructor TgtFileInfoListView.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtFileInfoListView.FillListView;
var
  LstItem : TListItem;
  LstCol  : TListColumn;
begin
  Items.Clear;
  Columns.Clear;
  LstCol         := Columns.Add;
  LstCol.Caption := 'Key';
  LstCol.Width   := Width div 2;

  LstCol         := Columns.Add;
  LstCol.Caption := 'Value';
  LstCol.Width   := Width div 2;

  LstItem := Self.Items.Add;
  LstItem.Caption := 'CompanyName';
  LstItem.SubItems.Add(FFileInfo.CompanyName);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'File Description';
  LstItem.SubItems.Add(FFileInfo.FileDescription);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'FileName';
  LstItem.SubItems.Add(FFileInfo.FileName);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'File Version';
  LstItem.SubItems.Add(FFileInfo.FileVersion);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'Internal Name';
  LstItem.SubItems.Add(FFileInfo.InternalName);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'Legal Copyright';
  LstItem.SubItems.Add(FFileInfo.LegalCopyright);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'Legal TradeMarks';
  LstItem.SubItems.Add(FFileInfo.LegalTrademarks);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'Original Filename';
  LstItem.SubItems.Add(FFileInfo.OriginalFilename);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'Product Name';
  LstItem.SubItems.Add(FFileInfo.ProductName);
  LstItem := Self.Items.Add;
  LstItem.Caption := 'Product Version';
  LstItem.SubItems.Add(FFileInfo.ProductVersion);

end;
{------------------------------------------------------------------------------}




//Getters - Setters\\
{------------------------------------------------------------------------------}
procedure TgtFileInfoListView.SetFileName(const Value: string);
const
  ERR_FILE_DOES_NOT_EXIST = '%s file does not exist!';
begin
  FFileName := Value;
  if (Length(Trim(FFileName)) > 0) and (not FileExists(FFileName)) then
    raise Exception.CreateFmt(ERR_FILE_DOES_NOT_EXIST,[FFileName])
  else
    if Assigned(FFileInfo) then
    begin
      FFileInfo.FileName := FFileName;
      FillListView;
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtFileInfoListView.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(Self.Parent) then
      FillListView;
end;
{------------------------------------------------------------------------------}

end.
