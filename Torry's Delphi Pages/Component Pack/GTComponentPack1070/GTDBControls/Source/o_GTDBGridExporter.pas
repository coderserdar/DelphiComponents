{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       TgtDBGridExporter                               }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTDBGridExporter;

interface
uses
   Classes
  ,DBGrids
  ,Menus
  ,ComCtrls
  ,ComObj
  ;
type
{------------------------------------------------------------------------------}
  TgtExportType = (
                     etNone
                    ,etCSV
                    ,etDelimited
                    ,etExcel
                   );
{------------------------------------------------------------------------------}
  TgtDBGridExporterMenuItemsCaption = class(TPersistent)
  private
    FExportToCSV: string;
    FExportedDelimited: string;
    FExportRootMenu: string;
    FExportToExcel: string;
    { Private declarations }
  public
    { Public declarations }
    procedure  Assign(Source : TPersistent);override;
    constructor Create;
    destructor  Destroy;override;
  published
    { Published declarations}
    property ExportRootMenu  : string read FExportRootMenu      write FExportRootMenu;
    property ExportToCSV     : string read FExportToCSV         write FExportToCSV;
    property ExportDelimited : string read FExportedDelimited   write FExportedDelimited;
    property ExportToExcel   : string read FExportToExcel       write FExportToExcel;
 end;
{------------------------------------------------------------------------------}
  TgtDBGridExporter = class(TComponent)
  private
    FDelimiterChar   : Char;
    FFileName        : string;
    FDBGrid          : TDBGrid;
    FExportType      : TgtExportType;
    FMenuItemCaptions: TgtDBGridExporterMenuItemsCaption;
    FProgressBar: TProgressBar;
    procedure SetDBGrid(const Value: TDBGrid);
    procedure SetProgressBar(const Value: TProgressBar);
    { Private declarations }
  protected
    { Protected declarations }
    procedure Notification   (AComponent  : TComponent ; Operation : TOperation);override;
    procedure InternalExport (AExportType : TgtExportType);
    procedure ExportToTextFile;
    procedure ExportToExcel;
    procedure RemoveMenuItems;
    procedure AddMenuItems;
    procedure InternalOnMenuItemClick(Sender : TObject);
    function  GetListSeparator:Char;
    function  CreateExcelApp:Boolean;
    function  IsExcelRunning:Boolean;
    function  CloseExcelApp :Boolean;
    procedure ShowExcel;    
  protected
    FGridPopUpMenu : TPopUpMenu;
    FMenuItemsID   : TStringList;
    FExportStrings : TStringList;
    FExcelApp      : OleVariant;
    FWorkBooks     : OleVariant;
    FActiveSheet   : OleVariant;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
    procedure   ExportGrid;
  published
    { Published declarations}
    property DBGrid           : TDBGrid                             read FDBGrid            write SetDBGrid;
    property ExportType       : TgtExportType                       read FExportType        write FExportType;
    property DelimiterChar    : Char                                read FDelimiterChar     write FDelimiterChar;
    property FileName         : string                              read FFileName          write FFileName;
    property MenuItemCaptions : TgtDBGridExporterMenuItemsCaption   read FMenuItemCaptions  write FMenuItemCaptions;
    property ProgressBar      : TProgressBar                        read FProgressBar       write SetProgressBar;
  end;
{------------------------------------------------------------------------------}


implementation

uses
   SysUtils
  ,DB
  ,Forms
  ,ActiveX
  ,Variants
  ;

const
  EXPORT_TO_CSV    = 100;
  EXPORT_DELIMITED = 101;
  EXPORT_TO_EXCEL  = 102;

  ExcelApplication = 'Excel.Application';

  EXCEL_COLS_ARRAY : array [0..25] of char =('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z');

{ TgtDBGridExporter }
{------------------------------------------------------------------------------}
constructor TgtDBGridExporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMenuItemsID      := TStringList.Create;
  FMenuItemCaptions := TgtDBGridExporterMenuItemsCaption.Create;
end;
{------------------------------------------------------------------------------}
destructor TgtDBGridExporter.Destroy;
begin
  FMenuItemCaptions.Free;
  FMenuItemsID.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.Notification(AComponent: TComponent;Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FDBGrid then
      DBGrid := nil;
    if AComponent = FProgressBar then
      ProgressBar := nil;
  end;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.ExportToTextFile;
var
  TempDelChar : Char;
  TempDs      : TDataSet;
  TempLine    : string;
  i           : Integer;
  Fld         : TField;
begin
  TempDelChar := GetListSeparator;

  if FExportType = etCSV       then
    TempDelChar := GetListSeparator;
  if FExportType = etDelimited then
   TempDelChar := FDelimiterChar;

  if Assigned(FDBGrid.DataSource) then
    if Assigned(FDBGrid.DataSource.DataSet) then
    begin
      TempDs         := FDBGrid.DataSource.DataSet;
      FExportStrings := TStringList.Create;
      try
        FDBGrid.Enabled := False;
        TempDs.DisableControls;
        if Assigned(FProgressBar) then
        begin
          FProgressBar.Max      := TempDs.RecordCount;
          FProgressBar.Position := 0;
        end;
        TempDs.First;
        while not TempDs.Eof do
        begin
          for i:= 0 to Pred(FDBGrid.Columns.Count) do
          begin
            Fld := Tempds.FindField(FDBGrid.Columns[i].FieldName);
            if Assigned(Fld) then
            begin
              if (not Fld.IsNull) or (Length(Fld.AsString) > 0) then
              begin
                TempLine := TempLine + Fld.AsString;
                TempLine := TempLine + TempDelChar;
              end;
            end;
          end;
          Delete(TempLine,Length(TempLine),1);
          FExportStrings.Add(TempLine);
          TempLine := '';
          TempDs.Next;
          if Assigned(FProgressBar) then
            FProgressBar.Position := FProgressBar.Position + 1;
          Application.ProcessMessages;
        end;
        FExportStrings.SaveToFile(FileName);
      finally
        FExportStrings.Free;
        TempDs.EnableControls;
        FDBGrid.Enabled := True;
      end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.ExportToExcel;
var
  GridDataSet : TDataSet;
  i           : Integer;
  HCell       : string;
  ExcelRow    : Integer;
  OpenResult  : OleVariant;
begin
 if Assigned(FDBGrid) then
  if Assigned(FDBGrid.DataSource) then
    if Assigned(FDBGrid.DataSource.DataSet) then
    begin
      GridDataSet := FDBGrid.DataSource.DataSet;
      if Assigned(FProgressBar) then
      begin
        FProgressBar.Max      := GridDataSet.RecordCount;
        FProgressBar.Position := 0;
      end;
      if FileExists(FileName) then
      begin
        if CreateExcelApp then
        begin
          FWorkBooks := FExcelApp.WorkBooks;
          try
            OpenResult := FWorkBooks.Open(FileName);
          except
            OpenResult := null;
          end;
        end;
      end
      else
      begin
        if CreateExcelApp then
        begin
          FWorkBooks := FExcelApp.WorkBooks.Add;
          if Length(FileName) > 0  then
            FExcelApp.Save(FileName);
          OpenResult := UnAssigned;
        end;
      end;
      if not VarIsNull(OpenResult) then
      begin
        FExcelApp.Visible := False;
        FActiveSheet      := FExcelApp.Sheets[1];
        try
          FDBGrid.Enabled := False;
          GridDataSet.DisableControls;
          ExcelRow :=1;
          for i:=0 to Pred(FDBGrid.Columns.Count) do
          begin
            if FDBGrid.Columns[i].Visible  then
            begin
              HCell := EXCEL_COLS_ARRAY[i] + IntToStr(ExcelRow);
              FActiveSheet.Range[HCell,HCell].Value := FDBGrid.Columns[i].Title.Caption;
            end;
          end;
          ExcelRow :=2;
          GridDataSet.First;
          while not GridDataSet.Eof do
          begin
            for i:=0 to Pred(FDBGrid.Columns.Count) do
            begin
              if FDBGrid.Columns[i].Visible  then
              begin
                if Assigned(FDBGrid.Columns[i].Field) then
                begin
                  HCell := EXCEL_COLS_ARRAY[i] + IntToStr(ExcelRow);
                  FActiveSheet.Range[HCell,HCell].Value := FDBGrid.Columns[i].Field.AsString;
                end;
              end;
            end;
            inc(ExcelRow);
            GridDataSet.Next;
            if Assigned(FProgressBar) then
              FProgressBar.Position := FProgressBar.Position + 1;
            Application.ProcessMessages;
          end;
        finally
          FDBGrid.Enabled := True;
          GridDataSet.EnableControls;
        end;
        if Assigned(FProgressBar) then
          FProgressBar.Position := 0;
        ShowExcel;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.InternalExport(AExportType: TgtExportType);
begin
  case AExportType of
    etNone       :;
    etCSV
    ,etDelimited : ExportToTextFile;
    etExcel      : ExportToExcel;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.ExportGrid;
begin
  InternalExport(FExportType);
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.AddMenuItems;
var
  FMenuItem : TMenuItem;
  FSubItem  : TMenuItem;
begin
  FMenuItemsID.Clear;
  if Assigned(DBGrid.PopupMenu) then
  begin
    FMenuItem         := TMenuItem.Create(DBGrid.PopupMenu);
    FMenuItem.Caption := '-';
    DBGrid.PopupMenu.Items.Add(FMenuItem);
    FMenuItemsID.Add(IntToStr(FMenuItem.Command));

    FMenuItem         := TMenuItem.Create(DBGrid.PopupMenu);
    FMenuItem.Caption := FMenuItemCaptions.ExportRootMenu;
    DBGrid.PopupMenu.Items.Add(FMenuItem);
    FMenuItemsID.Add(IntToStr(FMenuItem.Command));

    FSubItem         := TMenuItem.Create(FMenuItem);
    FSubItem.Caption := FMenuItemCaptions.ExportToCSV;
    FMenuItem.Add(FSubItem);
    FMenuItemsID.Add(IntToStr(FSubItem.Command));
    FSubItem.Tag     := EXPORT_TO_CSV;
    FSubItem.OnClick := InternalOnMenuItemClick;

    FSubItem         := TMenuItem.Create(FMenuItem);
    FSubItem.Caption := FMenuItemCaptions.ExportDelimited;
    FMenuItem.Add(FSubItem);
    FMenuItemsID.Add(IntToStr(FSubItem.Command));
    FSubItem.Tag     := EXPORT_DELIMITED;
    FSubItem.OnClick := InternalOnMenuItemClick;

    FSubItem         := TMenuItem.Create(FMenuItem);
    FSubItem.Caption := FMenuItemCaptions.ExportToExcel;
    FMenuItem.Add(FSubItem);
    FMenuItemsID.Add(IntToStr(FSubItem.Command));
    FSubItem.Tag     := EXPORT_TO_EXCEL;
    FSubItem.OnClick := InternalOnMenuItemClick;
  end
  else
  begin
    FGridPopUpMenu    := TPopUpMenu.Create(Self);
    DBGrid.PopupMenu  := FGridPopUpMenu;

    FMenuItem         := TMenuItem.Create(FGridPopUpMenu);
    FMenuItem.Caption := FMenuItemCaptions.ExportRootMenu;
    DBGrid.PopupMenu.Items.Add(FMenuItem);
    FMenuItemsID.Add(IntToStr(FMenuItem.Command));

    FSubItem         := TMenuItem.Create(FMenuItem);
    FSubItem.Caption := FMenuItemCaptions.ExportToCSV;
    FMenuItem.Add(FSubItem);
    FMenuItemsID.Add(IntToStr(FSubItem.Command));
    FSubItem.Tag     := EXPORT_TO_CSV;
    FSubItem.OnClick := InternalOnMenuItemClick;

    FSubItem         := TMenuItem.Create(FMenuItem);
    FSubItem.Caption := FMenuItemCaptions.ExportDelimited;
    FMenuItem.Add(FSubItem);
    FMenuItemsID.Add(IntToStr(FSubItem.Command));
    FSubItem.Tag     := EXPORT_DELIMITED;
    FSubItem.OnClick := InternalOnMenuItemClick;

    FSubItem         := TMenuItem.Create(FMenuItem);
    FSubItem.Caption := FMenuItemCaptions.ExportToExcel;
    FMenuItem.Add(FSubItem);
    FMenuItemsID.Add(IntToStr(FSubItem.Command));
    FSubItem.Tag     := EXPORT_TO_EXCEL;
    FSubItem.OnClick := InternalOnMenuItemClick;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.RemoveMenuItems;
var
  i   : Integer;
  Itm : TMenuItem;
begin
  if Assigned(DBGrid.PopupMenu) then
  begin
    for i:=Pred(DBGrid.PopupMenu.Items.Count) downto 0 do
    begin
      if FMenuItemsID.IndexOf(IntToStr(DBGrid.PopupMenu.Items[i].Command)) >=0 then
      begin
        Itm := DBGrid.PopupMenu.Items[i];
        FreeAndNil(Itm);
      end;
    end;
  end
  else
  begin
    if Assigned(FGridPopUpMenu) then
      FreeAndNil(FGridPopUpMenu);
  end;
end;
{------------------------------------------------------------------------------}
function TgtDBGridExporter.GetListSeparator: Char;
begin
  Result := SysUtils.ListSeparator;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.InternalOnMenuItemClick(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    EXPORT_TO_CSV    : FExportType := etCSV;
    EXPORT_DELIMITED : FExportType := etDelimited;
    EXPORT_TO_EXCEL  : FExportType := etExcel;
  end;
  Self.ExportGrid;
end;
{------------------------------------------------------------------------------}
function TgtDBGridExporter.CreateExcelApp: Boolean;
begin
  try
    if IsExcelRunning then
      FExcelApp := GetActiveOleObject(ExcelApplication)
    else
      FExcelApp := CreateOleObject(ExcelApplication);
    Result   := True;
    if Result then
      FExcelApp.Visible := True;
  except
    on E:Exception do
      begin
        raise Exception.Create(E.Message);
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtDBGridExporter.IsExcelRunning: Boolean;
var
  ClassID: TCLSID;
  Unknown: IUnknown;
begin
  try
    ClassID := ProgIDToClassID(ExcelApplication);
    Result  := GetActiveObject(ClassID, nil, Unknown) = S_OK;
  except
    on E:Exception do
      begin
        raise Exception.Create(E.Message);
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TgtDBGridExporter.CloseExcelApp: Boolean;
begin
  Result := False;
  if not VarIsEmpty(FExcelApp) then
    begin
      try
        if IsExcelRunning then
          begin
            try
              FExcelApp.Quit;
              VarClear(FExcelApp);
            except
              raise;
            end;
          end
        else
          begin
            VarClear(FExcelApp);
          end;
          Result := True;
      except
        on E:Exception do
          begin
            raise Exception.Create(E.Message);
          end;
      end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.ShowExcel;
begin
  if not VarIsEmpty(FExcelApp) then
    if IsExcelRunning then
      FExcelApp.Visible := True;
end;
{------------------------------------------------------------------------------}







{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.SetDBGrid(const Value: TDBGrid);
begin
  if Assigned(FDBGrid) then
  begin
    FDBGrid.RemoveFreeNotification(Self);
    RemoveMenuItems;
  end;

  FDBGrid := Value;

  if Assigned(FDBGrid) then
  begin
    FDBGrid.FreeNotification(Self);
    AddMenuItems;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporter.SetProgressBar(const Value: TProgressBar);
begin
  if Assigned(FProgressBar) then
    FProgressBar.RemoveFreeNotification(Self);

  FProgressBar := Value;

  if Assigned(FProgressBar) then
    FProgressBar.FreeNotification(Self);
end;
{------------------------------------------------------------------------------}



{ TgtDBGridExporterMenuItemsCaption }
{------------------------------------------------------------------------------}
constructor TgtDBGridExporterMenuItemsCaption.Create;
begin
  FExportRootMenu    := 'Export';
  FExportToCSV       := 'Export to CSV';
  FExportedDelimited := 'Export Delimited';
  FExportToExcel     := 'Export to Excel';
end;
{------------------------------------------------------------------------------}
procedure TgtDBGridExporterMenuItemsCaption.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if Source is TgtDBGridExporterMenuItemsCaption then
    begin
      Self.ExportRootMenu  := TgtDBGridExporterMenuItemsCaption(Source).ExportRootMenu;
      Self.ExportToCSV     := TgtDBGridExporterMenuItemsCaption(Source).ExportToCSV;
      Self.ExportDelimited := TgtDBGridExporterMenuItemsCaption(Source).ExportDelimited;
      Self.ExportToExcel   := TgtDBGridExporterMenuItemsCaption(Source).ExportToExcel;
    end
    else
      inherited Assign(Source);
  end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
destructor TgtDBGridExporterMenuItemsCaption.Destroy;
begin
  inherited;
end;
{------------------------------------------------------------------------------}













end.
 