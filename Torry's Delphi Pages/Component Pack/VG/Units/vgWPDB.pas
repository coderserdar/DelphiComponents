{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Report components for MS Word: TDBDataSet     }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgWPDB;

interface
uses vgWP, DB{$IFNDEF _D3_}, DBTables{$ENDIF};

type
{ TDBWordBookmarkDataSource }
  TDBWordBookmarkDataSource = class(TWordBookmarkDataSource)
  private
    FDataSet: TDataSet;
  protected
    function GetBOF: Boolean; override;
    function GetEOF: Boolean; override;
  public
    constructor CreateNew(ABookmark: TWordBookmark; ADataSet: TDataSet); virtual;
    destructor Destroy; override;
    procedure Close; override;
    procedure First; override;
    procedure Next; override;
    procedure Open; override;
    property DataSet: TDataSet read FDataSet;
  end;

  TCreateDataSetEvent = procedure(Sender: TObject; Group: TWordBookmark;
    ASQL: String; var DataSet: TDataSet) of Object;

  TFormatFieldEvent = procedure(Sender: TObject; ABookmark: TWordBookmark;
    Field: TField; var Text: String) of object;

  TWordBookmarkDataSetEvent = procedure(Sender: TObject; Group: TWordBookmark;
    DataSet: TDataSet) of object;

  TDataSetClass = class of TDataSet;

{ TvgDBWordPrint }
  TvgDBWordPrint = class(TvgWordPrint)
  private
    FDataSetClass: TDataSetClass;
    FOnCloseDataSet, FOnOpenDataSet: TWordBookmarkDataSetEvent;
    FOnCreateDataSet: TCreateDataSetEvent;
    FOnDataSetChanged: TWordBookmarkDataSetEvent;
    FOnDestroyDataSet: TWordBookmarkDataSetEvent;
    FOnFormatField: TFormatFieldEvent;
  protected
    function CreateDataSet(Group: TWordBookmark): TDataSet; virtual;
    procedure CreateDataSource(Group: TWordBookmark); override;
    procedure CloseDataSet(Group: TWordBookmark); virtual;
    procedure DataChanged(Group: TWordBookmark); override;
    procedure DestroyDataSet(Group: TWordBookmark); virtual;
    function FieldText(ABookmark: TWordBookmark; Text: String): String; override;
    procedure FormatField(ABookmark: TWordBookmark; Field: TField; var Text: String); virtual;
    procedure OpenDataSet(Group: TWordBookmark); virtual;
  public
    function DefaultCopyToClipboard(ABookmark: TWordBookmark): Boolean; override;
    procedure DefaultCreateDataSet(Group: TWordBookmark; var DataSet: TDataSet); virtual;
    procedure DefaultDestroyDataSet(Group: TWordBookmark); virtual;
    procedure DefaultFormatField(Bookmark: TWordBookmark; Field: TField; var Text: String); virtual;
    procedure DefaultUpdateParams(Group: TWordBookmark; DataSet: TDataSet); virtual;
    property DataSetClass: TDataSetClass read FDataSetClass write FDataSetClass;
  published
    property OnCloseDataSet: TWordBookmarkDataSetEvent read FOnCloseDataSet write FOnCloseDataSet;
    property OnCreateDataSet: TCreateDataSetEvent read FOnCreateDataSet write FOnCreateDataSet;
    property OnDataSetChanged: TWordBookmarkDataSetEvent read FOnDataSetChanged write FOnDataSetChanged;
    property OnDestroyDataSet: TWordBookmarkDataSetEvent read FOnDestroyDataSet write FOnDestroyDataSet;
    property OnFormatField: TFormatFieldEvent read FOnFormatField write FOnFormatField;
    property OnOpenDataSet: TWordBookmarkDataSetEvent read FOnOpenDataSet write FOnOpenDataSet;
  end;

  TWordPrintDataSetClass = class of TDataSet;

  EFieldNotFound       = class(EPrintingError);
  EDataSetClassEmpty   = class(EPrintingError);

function FindMasterField(ParamName: String; Group: TWordBookmark): TField;

implementation
uses SysUtils, vgUtils, vgWPRes, Clipbrd, vgClpbrd, Graphics;

function FindMasterField(ParamName: String; Group: TWordBookmark): TField;
var
  I: Integer;
  DataSet: TDataSet;
  Field: TField;
begin
  Result := nil;
  while not Assigned(Result) and (Group.ParentGroup <> nil) do
  begin
    Group := Group.ParentGroup;
    if (Group.DataSource is TDBWordBookmarkDataSource) then
    begin
      DataSet := TDBWordBookmarkDataSource(Group.DataSource).DataSet;
      for I := 0 to DataSet.FieldCount - 1 do
      begin
        Field := DataSet.Fields[I];
        if (AnsiCompareText(Format('%s_%s', [Group.Name, Field.FieldName]), ParamName) = 0) then
        begin
          Result := Field;
          Break;
        end;
      end;
    end;
  end;
end;

{ TDBWordBookmarkDataSource }
constructor TDBWordBookmarkDataSource.CreateNew(ABookmark: TWordBookmark; ADataSet: TDataSet);
begin
  inherited Create(ABookmark);
  FDataSet := ADataSet;
end;

destructor TDBWordBookmarkDataSource.Destroy;
begin
  inherited;
  TvgDBWordPrint(Bookmark.WordPrint).DestroyDataSet(Bookmark);
end;

procedure TDBWordBookmarkDataSource.Close;
begin
  TvgDBWordPrint(Bookmark.WordPrint).CloseDataSet(Bookmark);
  inherited;
end;

procedure TDBWordBookmarkDataSource.First;
begin
  FDataSet.First;
  inherited;
end;

function TDBWordBookmarkDataSource.GetBOF: Boolean;
begin
  Result := FDataSet.BOF;
end;

function TDBWordBookmarkDataSource.GetEOF: Boolean;
begin
  Result := FDataSet.EOF;
end;

procedure TDBWordBookmarkDataSource.Next;
begin
  FDataSet.Next;
  inherited;
end;

procedure TDBWordBookmarkDataSource.Open;
begin
  TvgDBWordPrint(Bookmark.WordPrint).OpenDataSet(Bookmark);
  inherited;
end;

{ TvgDBWordPrint }
function TvgDBWordPrint.DefaultCopyToClipboard(ABookmark: TWordBookmark): Boolean;
  function FindField(DataSet: TDataSet): TField;
  var
    I: Integer;
  begin
    Result := DataSet.FindField(ABookmark.FieldName);
    if not Assigned(Result) and (Pos(MacroChar, ABookmark.FieldName) = 1) then
    try
      I := StrToInt(Copy(ABookmark.FieldName, 2, 255));
      Result := DataSet.Fields[I];
    except end;
  end;
var
  Field: TField;
  DataSource: TWordBookmarkDataSource;
  Text: String;
  Picture: TPicture;
begin
  DataSource := ABookmark.Group.DataSource;
  if (DataSource is TDBWordBookmarkDataSource) then
  with TDBWordBookmarkDataSource(DataSource) do
  begin
    Field := FindField(DataSet);
    if (Field is TMemoField) then
    begin
      Text := Field.AsString;
      ClipboardCopy(Text);
      Result := (Text <> '');
      Exit;
    end else if (Field is TGraphicField) then
    begin
      Picture := TPicture.Create;
      try
        Picture.Assign(Field);
        Result := (Picture.Graphic <> nil);
        if Result then Clipboard.Assign(Picture);
        Exit;
      finally
        Picture.Free;
      end;
    end;
  end;
  Result := inherited DefaultCopyToClipboard(ABookmark);
end;

procedure TvgDBWordPrint.CreateDataSource(Group: TWordBookmark);
var
  I: Integer;
  Bookmark: TWordBookmark;
begin
  Bookmark := nil;
  for I := 0 to Group.Count - 1 do
  begin
    if Group[I].Kind = bkSQL then
    begin
      Bookmark := Group[I];
      Break;
    end;
  end;
  if Assigned(Bookmark) then
    TDBWordBookmarkDataSource.CreateNew(Group, CreateDataSet(Group))
  else
    inherited CreateDataSource(Group);
end;

function TvgDBWordPrint.CreateDataSet(Group: TWordBookmark): TDataSet;
begin
  if Assigned(FOnCreateDataSet) then
    FOnCreateDataSet(Self, Group, Group.BookmarkSQL.SQL, Result)
  else
    DefaultCreateDataSet(Group, Result);
end;

procedure TvgDBWordPrint.DataChanged(Group: TWordBookmark);
var
  DataSource: TWordBookmarkDataSource;
begin
  DataSource := Group.DataSource;
  if (DataSource is TDBWordBookmarkDataSource) and Assigned(FOnDataSetChanged) then
    FOnDataSetChanged(Self, Group, TDBWordBookmarkDataSource(DataSource).DataSet);
  inherited;
end;

procedure TvgDBWordPrint.DefaultCreateDataSet(Group: TWordBookmark; var DataSet: TDataSet);
begin
  if not Assigned(FDataSetClass) then
    raise EDataSetClassEmpty.Create(LoadStr(SDataSetClassEmpty));
  DataSet := FDataSetClass.Create(Self);
end;

procedure TvgDBWordPrint.DefaultDestroyDataSet(Group: TWordBookmark);
begin
  TDBWordBookmarkDataSource(Group.DataSource).DataSet.Free;
end;

procedure TvgDBWordPrint.DefaultFormatField(Bookmark: TWordBookmark; Field: TField; var Text: String);
var
  SaveFormat: String;
  NumericField: TNumericField;
  DateTimeField: TDateTimeField;
begin
  if (Field is TNumericField) then
  begin
    NumericField := TNumericField(Field);
    if (Bookmark.Mask <> '') then
    begin
      SaveFormat := NumericField.DisplayFormat;
      try
        TNumericField(Field).DisplayFormat := Bookmark.Mask;
        Text := Field.Text;
      finally
        TNumericField(Field).DisplayFormat := SaveFormat;
      end;
    end;
  end else if (Field is TDateTimeField) then
  begin
    DateTimeField := TDateTimeField(Field);
    if (Bookmark.Mask <> '') then
    begin
      SaveFormat := DateTimeField.DisplayFormat;
      try
        TDateTimeField(Field).DisplayFormat := Bookmark.Mask;
        Text := Field.Text;
      finally
        TDateTimeField(Field).DisplayFormat := SaveFormat;
      end;
    end;
  end;
end;

procedure TvgDBWordPrint.DefaultUpdateParams(Group: TWordBookmark; DataSet: TDataSet);
begin
end;

procedure TvgDBWordPrint.CloseDataSet(Group: TWordBookmark);
begin
  if Assigned(FOnCloseDataSet) then
    FOnCloseDataSet(Self, Group, TDBWordBookmarkDataSource(Group.DataSource).DataSet)
  else
    TDBWordBookmarkDataSource(Group.DataSource).DataSet.Close;
end;

procedure TvgDBWordPrint.DestroyDataSet(Group: TWordBookmark);
begin
  if Assigned(FOnDestroyDataSet) then
    FOnDestroyDataSet(Self, Group, TDBWordBookmarkDataSource(Group.DataSource).DataSet)
  else if (TDBWordBookmarkDataSource(Group.DataSource).DataSet.Owner = Self) then
    DefaultDestroyDataSet(Group);
end;

function TvgDBWordPrint.FieldText(ABookmark: TWordBookmark; Text: String): String;
var
  Field: TField;
  DataSource: TWordBookmarkDataSource;
begin
  Result := 'N/A';
  DataSource := ABookmark.Group.DataSource;
  if (DataSource is TDBWordBookmarkDataSource) then
  with TDBWordBookmarkDataSource(DataSource) do
  begin
    Field := DataSet.FindField(ABookmark.FieldName);
    if Assigned(Field) then
      Result := Field.Text
    else if not Assigned(OnFieldText) then
      raise EFieldNotFound.Create(Format(LoadStr(SFieldNotFound), [ABookmark.FieldName, ABookmark.Name]));
    FormatField(ABookmark, Field, Result);
  end;
  Result := inherited FieldText(ABookmark, Result);
end;

procedure TvgDBWordPrint.FormatField(ABookmark: TWordBookmark; Field: TField; var Text: String);
begin
  if Assigned(FOnFormatField) then
    FOnFormatField(Self, ABookmark, Field, Text)
  else if Assigned(Field) then
    DefaultFormatField(ABookmark, Field, Text);
end;

procedure TvgDBWordPrint.OpenDataSet(Group: TWordBookmark);
var
  DataSource: TWordBookmarkDataSource;
begin
  DataSource := Group.DataSource;
  if (DataSource is TDBWordBookmarkDataSource) then
  begin
    if Assigned(FOnOpenDataSet) then
      FOnOpenDataSet(Self, Group, TDBWordBookmarkDataSource(Group.DataSource).DataSet)
    else begin
      DefaultUpdateParams(Group, TDBWordBookmarkDataSource(DataSource).DataSet);
      TDBWordBookmarkDataSource(Group.DataSource).DataSet.Open;
    end;
  end;
end;

end.
