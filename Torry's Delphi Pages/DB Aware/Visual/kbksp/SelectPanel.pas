unit SelectPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ExtCtrls,
  DBCtrls, DBClient, DB, DBConsts;


type

  TawcCondition = (awcNone = 0, awcWhere = 1, awcAnd = 2, awcOr = 3);

  TSelectPanel = class;

  TSPListSourceLink = class(TDataLink)
  private
    FSelectPanel: TSelectPanel;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;
  public
    constructor Create;
  end;


  TSelectPanel = class(TGroupBox)
  private
    { Private declarations }
    FLookupSource: TDataSource;
    FListLink: TSPListSourceLink;
    FKeyFieldName: string;
    FListFieldName: string;
    FNoteFieldName: string;
    FListFieldIndex: Integer;
    FKeyField: TField;
    FListField: TField;
    FNoteField: TField;
    FListActive: Boolean;
    FListFields: TList;
    FSizeText: Integer;

    FIndent: Integer;
    function GetListSource: TDataSource;
    procedure SetListSource(Value: TDataSource);
    procedure SetKeyFieldName(const Value: string);
    procedure SetListFieldName(const Value: string);
    procedure SetNoteFieldName(const Value: string);
  protected
    { Protected declarations }
    FTopPanel, FLeftPanel, FRightPanel, FBottomPanel: TPanel;
    FList: TDBLookupListBox;
    FSelectButton: TButton;
    FAllCheck, FExceptCheck: TCheckBox;
{    OldT: Cardinal;
    FindString: String;}
    FSQL: TStringList;
    procedure SetIndent(const AIndent: Integer);
    procedure FAllCheckClick(Sender: TObject);
    procedure SelectButtonClick(Sender: TObject);
    procedure ListLinkDataChanged; virtual;
    procedure UpdateListFields; virtual;
    procedure SetSizeText(const ASizeText: Integer);
  public
    { Public declarations }
    OldKeyValue: Variant;
    DS: TClientDataset;
    DataSource: TDataSource;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property KeyF: TField read FKeyField;
    property ListF: TField read FListField;
    property NoteF: TField read FNoteField;
    function GetWhereClause(const AFieldName: String): TStringList;
    procedure AddWhereClause(const awcItem: TawcCondition; const AFieldName: String; ASQL: TStrings; const ASQLCount: Integer = -1);
  published
    { Published declarations }
    property Indent: Integer read FIndent write SetIndent;
    property KeyField: string read FKeyFieldName write SetKeyFieldName;
    property ListField: string read FListFieldName write SetListFieldName;
    property NoteField: string read FNoteFieldName write SetNoteFieldName;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property SizeText: Integer read FSizeText write SetSizeText;
  end;

procedure TruncateSQL(ASQL: TStrings; const ASQLCount: Integer);

implementation

uses MakeSelectD, Variants, SelectPanelConsts;

const
  Conds: array[TawcCondition] of String[6] = ('','WHERE ','AND ','OR ');

constructor TSelectPanel.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FSizeText:=60;
  FSQL:=nil;
  OldKeyValue:=Null;
  FListFields := TList.Create;
  FTopPanel:=TPanel.Create(Self);
  FTopPanel.Caption:='';
  FTopPanel.Parent:=Self;
  FTopPanel.Align:=alTop;
  FTopPanel.BevelOuter:=bvNone;
  FTopPanel.Height:=34;
  FLeftPanel:=TPanel.Create(Self);
  FLeftPanel.Caption:='';
  FLeftPanel.Parent:=Self;
  FLeftPanel.Align:=alLeft;
  FLeftPanel.BevelOuter:=bvNone;
  FRightPanel:=TPanel.Create(Self);
  FRightPanel.Caption:='';
  FRightPanel.Parent:=Self;
  FRightPanel.Align:=alRight;
  FRightPanel.BevelOuter:=bvNone;
  FBottomPanel:=TPanel.Create(Self);
  FBottomPanel.Caption:='';
  FBottomPanel.Parent:=Self;
  FBottomPanel.Align:=alBottom;
  FBottomPanel.BevelOuter:=bvNone;
  FList:=TDBLookupListBox.Create(Self);
  FList.Parent:=Self;
  FList.Align:=alClient;
  FSelectButton:=TButton.Create(Self);
  FSelectButton.Parent:=FTopPanel;
  FSelectButton.Top:=4;
  FSelectButton.Width:=50;
  FSelectButton.OnClick:=SelectButtonClick;
  FSelectButton.Caption:=SSelectButtonCaption;
  FAllCheck:=TCheckBox.Create(Self);
  FAllCheck.Parent:=FTopPanel;
  FAllCheck.Caption:=SAllCheckCaption;
  FAllCheck.Top:=0;
  FAllCheck.Width:=150;
  FAllCheck.OnClick:=FAllCheckClick;
  FExceptCheck:=TCheckBox.Create(Self);
  FExceptCheck.Parent:=FTopPanel;
  FExceptCheck.Checked:=false;
  FExceptCheck.Caption:=SExceptCheckCaption;
  FExceptCheck.Top:=16;
  FExceptCheck.Width:=150;
  FAllCheck.Checked:=true;
  FAllCheckClick(nil);
  InDent:=8;
  DS:=TClientDataset.Create(Self);
  with DS do begin
    with FieldDefs do begin
      Clear;
      with AddFieldDef do begin
        Name := 'ID';
        DataType := ftInteger;
        Required := True;
      end;
      with AddFieldDef do begin
        Name := 'TEXT';
        DataType := ftString;
        Size := SizeText;
      end;
    end;
    with IndexDefs do begin
      Clear;
      with AddIndexDef do begin
        Name := 'IDIndex';
        Fields := 'ID';
        Options := [ixPrimary];
      end;
      with AddIndexDef do begin
        Name := 'TEXTIndex';
        Fields := 'TEXT';
        Options := [ixCaseInsensitive];
      end;
    end;
    IndexName:=IndexDefs.Items[1].Name;
    CreateDataSet;
    Fields[0].Visible:=false;
  end;
  DataSource:=TDataSource.Create(Self);
  DataSource.DataSet:=DS;
  FListLink := TSPListSourceLink.Create;
  FListLink.FSelectPanel := Self;
  FList.ListSource:=DataSource;
  FList.KeyField:=DS.Fields[0].FieldName;
  FList.ListField:=DS.Fields[1].FieldName;
{  DS.InsertRecord([1,'‚‚']);
  DS.InsertRecord([2,'‚‡‡‡‡']);
  DS.InsertRecord([3,'··']);}
  Flist.Invalidate;
  FLookupSource := TDataSource.Create(Self);
  FAllCheck.Enabled:=not DS.IsEmpty;
end;

destructor TSelectPanel.Destroy;
begin
  FSQL.Free;
  FSQL:=nil;
  FListFields.Free;
  FListFields := nil;
  if FListLink <> nil then FListLink.FSelectPanel := nil;
  FListLink.Free;
  FListLink := nil;
  inherited Destroy;
end;

procedure TSelectPanel.SetIndent(const AIndent: Integer);
begin
  FIndent:=AIndent;
  if FIndent<0 then FIndent:=0;
  if FIndent>Width div 4 then FIndent:=Width div 4;
  if FIndent>Height div 4 then FIndent:=Height div 4;
  FBottomPanel.Height:=FIndent;
  FLeftPanel.Width:=FIndent;
  FRightPanel.Width:=FIndent;
  FSelectButton.Left:=FIndent;
  FAllCheck.Left:=FSelectButton.Left+FSelectButton.Width+8;
  FExceptCheck.Left:=FAllCheck.Left;
end;

procedure TSelectPanel.FAllCheckClick(Sender: TObject);
begin
  if FAllCheck.Checked then FExceptCheck.Checked:=False;
  FExceptCheck.Enabled:=not FAllCheck.Checked;
  FList.Enabled:=not FAllCheck.Checked;
end;

procedure TSelectPanel.SelectButtonClick(Sender: TObject);
begin
  if MakeSelectExecute(Self) then begin
    FAllCheck.Checked:=DS.IsEmpty;
    FAllCheckClick(nil);
    FAllCheck.Enabled:=not DS.IsEmpty;
  end;
end;

procedure TSelectPanel.SetKeyFieldName(const Value: string);
begin
  if FKeyFieldName <> Value then begin
    FKeyFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TSelectPanel.SetListFieldName(const Value: string);
begin
  if FListFieldName <> Value then begin
    FListFieldName := Value;
    UpdateListFields;
  end;
end;

procedure TSelectPanel.SetNoteFieldName(const Value: string);
begin
  if FNoteFieldName <> Value then begin
    FNoteFieldName := Value;
    UpdateListFields;
  end;
end;

constructor TSpListSourceLink.Create;
begin
  inherited Create;
  VisualControl := True;
end;

procedure TSpListSourceLink.ActiveChanged;
begin
  if FSelectPanel <> nil then FSelectPanel.UpdateListFields;
end;

procedure TSPListSourceLink.DataSetChanged;
begin
  if FSelectPanel <> nil then FSelectPanel.ListLinkDataChanged;
end;

procedure TSPListSourceLink.LayoutChanged;
begin
  if FSelectPanel <> nil then FSelectPanel.UpdateListFields;
end;

procedure TSelectPanel.UpdateListFields;
var
  DataSet: TDataSet;
  W: String;
begin
  FListActive := False;
  FKeyField := nil;
  FListField := nil;
  FNoteField := nil;
  if FListLink.Active and (FKeyFieldName <> '') then
  begin
    DataSet := FListLink.DataSet;
    FKeyField := GetFieldProperty(DataSet, Self, FKeyFieldName);
    if not (FKeyField is TIntegerField) then DatabaseErrorFmt(SFieldTypeMismatch, [FKeyFieldName, FieldTypeNames[ftInteger], FieldTypeNames[FKeyField.DataType], Self]);
    if FListFieldName='' then FListFieldName:=FKeyFieldName;
    try
      DataSet.GetFieldList(FListFields, FListFieldName);
    except
      W:=FListFieldName;
      FListFieldName:=FKeyFieldName;
      DataSet.GetFieldList(FListFields, FListFieldName);
      DatabaseErrorFmt(SFieldNotFound, [W, Self.Name]);
    end;
    try
      if FNoteFieldName<>''
      then FNoteField := GetFieldProperty(DataSet, Self, FNoteFieldName)
      else FNoteField:=nil;
    except
      W:=FNoteFieldName;
      FNoteFieldName:='';
      FNoteField:=nil;
      DatabaseErrorFmt(SFieldNotFound, [W, Self.Name]);
    end;
    if FListFields.Count = 0 then FListFields.Add(FKeyField);
    if (FListFieldIndex >= 0) and (FListFieldIndex < FListFields.Count) then
      FListField := FListFields[FListFieldIndex] else
      FListField := FListFields[0];
    FListActive := True;
  end;
end;

procedure TSelectPanel.ListLinkDataChanged;
begin
end;

function TSelectPanel.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TSelectPanel.SetListSource(Value: TDataSource);
begin
  FListLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;

function TSelectPanel.GetWhereClause(const AFieldName: String): TStringList;
const
  EN: array[Boolean] of String[2] = ('=','<>');
  BOR: array[Boolean] of String[3] = (' ','OR ');

var
  WS: String;
  B1, E1: Integer;

procedure Add1;
begin
  if E1-B1<>0 then begin
    FSQL.Add(BOR[FSQL.Count>0]+'('+AFieldName+'>='+IntToStr(B1)+' AND '+AFieldName+'<='+IntToStr(E1)+')')
  end
  else begin
    if WS<>'' then  WS:=WS+',';
    WS:=WS+IntToStr(B1);
  end;
end;

begin
  if FSQL=nil then FSQL:=TStringList.Create;
  FSQL.Clear;
  result:=FSQL;
  if FAllCheck.Checked or DS.IsEmpty then exit;
  try
    DS.IndexName:=DS.IndexDefs.Items[0].Name;
    DS.First;
    WS:='';
    if DS.RecordCount=1 then begin
      WS:=' '+WS+AFieldName+EN[FExceptCheck.Checked]+DS.Fields[0].AsString;
      FSQL.Add(WS);
    end
    else begin
      B1:=DS.Fields[0].AsInteger;
      E1:=B1;
      WS:='';
      repeat
        DS.Next;
        if DS.Fields[0].AsInteger-E1=1 then begin
          E1:=DS.Fields[0].AsInteger;
        end
        else begin
          Add1;
          B1:=DS.Fields[0].AsInteger;
          E1:=B1;
        end;
      until DS.Eof;
      if WS<>'' then begin
        if Pos(',', WS)>0
        then FSQL.Add(BOR[FSQL.Count>0]+AFieldName+' IN ('+WS+')')
        else FSQL.Add(BOR[FSQL.Count>0]+AFieldName+'='+WS);
      end;
      if FExceptCheck.Checked then begin
        FSQL[0]:='NOT ('+FSQL[0];
        FSQL[FSQL.Count-1]:=FSQL[FSQL.Count-1]+')';
      end;
    end;
  finally
    DS.IndexName:=DS.IndexDefs.Items[1].Name;
  end;
end;

procedure TSelectPanel.SetSizeText(const ASizeText: Integer);
var
  TM: TTextmetric;
begin
  if FSizetext=ASizeText then exit;
  if FSizetext>255 then exit;
  if ASizeText>0 then begin
    FSizetext:=ASizeText;
    DS.Active:=false;
    DS.FieldDefs.Items[1].Size:=FSizetext;
    DS.CreateDataSet;
  end
  else begin
     try
       FSizeText:=ListSource.DataSet.FieldByName(ListField).Size;
       DS.Active:=false;
       DS.FieldDefs.Items[1].Size:=FSizetext;
       GetTextMetrics(Canvas.Handle, TM);
       Width:=(FSizetext+1)*TM.tmaveCharWidth+GetSystemMetrics(SM_CYVSCROLL)+2*Indent+4;
       DS.CreateDataSet;
     except
        FSizeText:=1;
        DS.Active:=false;
        DS.FieldDefs.Items[1].Size:=FSizetext;
        DS.CreateDataSet;
     end;
  end;
end;

procedure TSelectPanel.AddWhereClause(const awcItem: TawcCondition; const AFieldName: String; ASQL: TStrings; const ASQLCount: Integer = -1);
var
  I, P: Integer;
  fawcItem: TawcCondition;

begin
  GetWhereClause(AFieldName);
{  if ASQLCount>=0 then begin
    while ASQL.Count>ASQLCount do  ASQL.Delete(ASQL.Count-1);
  end;}
  TruncateSQL(ASQL, ASQLCount);
  if FSQL.Count=0 then exit;
  fawcItem:=awcItem;
  if (fawcItem=awcAnd) or (fawcItem=awcOr) then begin
    P:=0;
    for i:= 0 to ASQL.Count-1 do begin
      P:=Pos(Conds[awcWhere], ASQL[i]);
      if P=1 then break;
    end;
    if P<>1 then fawcItem:=awcWhere;
  end;
  ASQL.Add(Conds[fawcItem]+'('+FSQL[0]);
  for i:= 1 to FSQL.Count-1 do ASQL.Add(FSQL[i]);
  ASQL[ASQL.Count-1]:=ASQL[ASQL.Count-1]+')';
end;

procedure TruncateSQL(ASQL: TStrings; const ASQLCount: Integer);
begin
  if ASQLCount>=0 then begin
    while ASQL.Count>ASQLCount do  ASQL.Delete(ASQL.Count-1);
  end;
end;

end.
