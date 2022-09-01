unit UnitMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, StdCtrls, DBCtrls, Mask, Grids, DBGrids, SnapVirtualDataset,
  SnapBaseDataset;

type
  POrderObject = ^TOrderObject;
  TOrderObject = record
    OrderDescription: string;
    OrderDate: TDateTime;
  end;

  PDataObject = ^TDataObject;
  TDataObject = record
    Name: string;
    Age: Integer;
    Birthday: TDateTime;
    Memo: string;
    Orders: TList;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    edRecNo: TEdit;
    btnRecNoGo: TButton;
    DBGrid1: TDBGrid;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBMemo1: TDBMemo;
    DBGrid3: TDBGrid;
    Memo1: TMemo;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    Button2: TButton;
    vdsNamesDataset: TSnapVirtualDataset;
    vdsOrdersDataset: TSnapVirtualDataset;
    vdsNamesDatasetName: TStringField;
    vdsNamesDatasetAge: TIntegerField;
    vdsNamesDatasetBirthday: TDateTimeField;
    vdsNamesDatasetmemo: TMemoField;
    vdsOrdersDatasetDescription: TStringField;
    vdsOrdersDatasetOrderDate: TDateField;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnRecNoGoClick(Sender: TObject);
    procedure vdsNamesDatasetGetDataCount(Sender: TSnapCustomDataset;
      var Count: Integer);
    procedure vdsNamesDatasetGetDataValue(Sender: TSnapCustomDataset;
      Field: TField; Index: Integer; var Value: Variant);
    procedure vdsOrdersDatasetGetDataValue(Sender: TSnapCustomDataset;
      Field: TField; Index: Integer; var Value: Variant);
    procedure vdsOrdersDatasetGetDataCount(Sender: TSnapCustomDataset;
      var Count: Integer);
    procedure vdsNamesDatasetAfterScroll(DataSet: TDataSet);
    procedure vdsNamesDatasetGetDataBlobValue(Sender: TSnapCustomDataset;
      Field: TBlobField; Index: Integer; Stream: TStream);
    procedure vdsNamesDatasetPostData(Sender: TSnapCustomDataset;
      Index: Integer);
    procedure vdsNamesDatasetDeleteData(Sender: TSnapCustomDataset;
      Index: Integer);
    procedure edRecNoKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure vdsOrdersDatasetPostData(Sender: TSnapCustomDataset;
      Index: Integer);
    procedure vdsOrdersDatasetDeleteData(Sender: TSnapCustomDataset;
      Index: Integer);
    procedure Button2Click(Sender: TObject);
    procedure vdsOrdersDatasetBeforeInsert(DataSet: TDataSet);
  private
    FData: TList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  P: PDataObject;
  i: Integer;

begin
  FData := TList.Create;
  //
  // Create some sample data
  //
  for i:=0 to 19 do
  begin
    new(P);
    P^.Name := 'Name ' + IntToStr(i+1);
    P^.Age := i;
    P^.Birthday := EncodeDate(2002, 1, 1) + i;
    P^.Memo := 'Memo data for ' + P^.Name;
    P^.Orders := TList.Create;
    FData.Add(P);
  end;
end;


procedure FreeOrderObject(Value: TOrderObject);
begin
  Finalize(Value);
end;

procedure FreeDataObject(Value: TDataObject);
var
  i: Integer;

begin
  with Value.Orders do
  begin
    for i:=0 to Count-1 do
      FreeOrderObject(POrderObject(Items[i])^);
  end;
  Value.Orders.Destroy;
  Finalize(Value);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to FData.Count-1 do
  begin
    FreeDataObject(PDataObject(FData[i])^);
    Dispose(FData[i]);
  end;

  FData.Destroy;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  vdsNamesDataset.Open;
  vdsOrdersDataset.Open;
end;

procedure TForm1.btnRecNoGoClick(Sender: TObject);
begin
  if EdRecNo.Text<>'' then
    vdsNamesDataset.RecNo := StrToInt(EdRecNo.Text);
end;

procedure TForm1.vdsNamesDatasetGetDataCount(Sender: TSnapCustomDataset;
  var Count: Integer);
  //
  // Event handler for OnGetRecordCount event.
  //
  // Count must be set to the number of records the virtual dataset contains.
  //

begin
  Count := FData.Count;
end;

procedure TForm1.vdsNamesDatasetGetDataValue(Sender: TSnapCustomDataset;
  Field: TField; Index: Integer; var Value: Variant);

  //
  // Event handler for OnGetFieldValue event.
  //
  // This event returns the field data to the virtual dataset.
  //
  //  Sender: TCustomEzArrayDataset firing this event
  //  Field:  Field object for which the value should be returned
  //  Index:  Absolute index of the record for which data should be returned (0 based)
  //  Value: Variable to store result in.

var
  PData: PDataObject;

begin
  if (Index>=0) and (Index<FData.Count) then
  begin
    PData := FData[Index];
  {
    Alternative implementation

    case Field.FieldNo of
      1: Value := PData^.Name;
      2: Value := PData^.Age;
      3: Value := PData^.Birthday;
      4: Value := PData^.Memo;
    end;
  }
    if SameText(Field.FieldName, 'Name') then
      Value := PData^.Name
    else if SameText(Field.FieldName, 'Age') then
      Value := PData^.Age
    else if SameText(Field.FieldName, 'Birthday') then
      Value := PData^.Birthday
    else if SameText(Field.FieldName, 'Memo') then
      Value := PData^.Memo;
  end;
end;

procedure TForm1.vdsOrdersDatasetGetDataCount(Sender: TSnapCustomDataset;
  var Count: Integer);
begin
  Count := 0;
  if vdsNamesDataset.Index>-1 then
    Count := PDataObject(FData[vdsNamesDataset.Index])^.Orders.Count;
end;

procedure TForm1.vdsNamesDatasetAfterScroll(DataSet: TDataSet);
begin
  edRecNo.Text := IntToStr(vdsNamesDataset.recno);
  Label2.Caption := format('RecNo: %d, Index: %d', [vdsNamesDataset.RecNo, vdsNamesDataset.Index]);
end;

procedure TForm1.vdsNamesDatasetGetDataBlobValue(
  Sender: TSnapCustomDataset; Field: TBlobField; Index: Integer;
  Stream: TStream);
var
  PData: PDataObject;
  sStream: TStringStream;
begin
  if (Index>=0) and (Index<FData.Count) then
  begin
    PData := FData[Index];

    if SameText(Field.FieldName, 'Memo') then
    begin
      sStream := TStringStream.Create(PData^.Memo);
      try
        Stream.CopyFrom(sStream, 0);
      finally
        sStream.Free;
      end;
    end;
  end;
end;

procedure TForm1.vdsNamesDatasetPostData(Sender: TSnapCustomDataset;
  Index: Integer);
var
  PData: PDataObject;

begin
  if vdsNamesDataset.State = dsInsert then
  begin
    New(PData);
    PData^.Orders := TList.Create;
    if Index = -1 then
      FData.Add(PData)
    else
      FData.Insert(Index, PData);
  end
  else
    PData := PDataObject(FData[Index]);

  PData^.Name := vdsNamesDataset.FieldByName('Name').AsString;
  PData^.Age := vdsNamesDataset.FieldByName('Age').AsInteger;
  PData^.Birthday := vdsNamesDataset.FieldByName('Birthday').AsDatetime;
  PData^.Memo := vdsNamesDataset.FieldByName('Memo').AsString;
end;

procedure TForm1.vdsNamesDatasetDeleteData(Sender: TSnapCustomDataset;
  Index: Integer);

  //
  // Event handler for OnDeleteRecord event.
  //
  // This event is fired when a record is deleted from the dataset.
  // Here you should remove your data object from the list.
  //
  //  Sender: TCustomEzArrayDataset firing this event
  //  Index:  Absolute index of the record being deleted.

begin
  FreeDataObject(PDataObject(FData[Index])^);
  Dispose(FData[Index]);
  FData.Delete(Index);
end;

procedure TForm1.edRecNoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btnRecNoGo.Click;
end;

procedure TForm1.vdsOrdersDatasetGetDataValue(Sender: TSnapCustomDataset;
  Field: TField; Index: Integer; var Value: Variant);
var
  PData: PDataObject;
  PDataOrder: POrderObject;

begin
  PData := FData[vdsNamesDataset.Index];
  if (Index>=0) and (Index<PData.Orders.Count) then
  begin
    PDataOrder := PData.Orders[Index];

    case Field.FieldNo of
      1: Value := PDataOrder^.OrderDescription;
      2: Value := PDataOrder^.OrderDate;
    end;
  end;
end;

procedure TForm1.vdsOrdersDatasetPostData(Sender: TSnapCustomDataset;
  Index: Integer);
var
  PData: POrderObject;

begin
  if vdsOrdersDataset.State = dsInsert then
  begin
    New(PData);
    if Index = -1 then
      PDataObject(FData[vdsNamesDataset.Index])^.Orders.Add(PData)
    else
      PDataObject(FData[vdsNamesDataset.Index])^.Orders.Insert(Index, PData);
  end
  else
    PData := PDataObject(FData[vdsNamesDataset.Index])^.Orders[Index];

  PData^.OrderDescription := vdsOrdersDataset.FieldByName('Description').AsString;
  PData^.OrderDate := vdsOrdersDataset.FieldByName('OrderDate').AsDateTime;
end;

procedure TForm1.vdsOrdersDatasetDeleteData(Sender: TSnapCustomDataset;
  Index: Integer);
var
  PData: POrderObject;

begin
  PData := PDataObject(FData[vdsNamesDataset.Index])^.Orders[Index];
  Finalize(PData^);
  Dispose(PData);
  PDataObject(FData[vdsNamesDataset.Index])^.Orders.Delete(Index);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.vdsOrdersDatasetBeforeInsert(DataSet: TDataSet);
begin
  if vdsNamesDataset.State = dsInsert then
    vdsNamesDataset.Post;
end;

end.
