{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnPagedGrid; 
{* |<PRE>
================================================================================
* ������ƣ����������
* ��Ԫ���ƣ�ʵ�ֿɷ�ҳ������������ʾ��Ԫ
* ��Ԫ���ߣ�rarnu(rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��Windows2003 Server + Delphi2007 up2
* ���ݲ��ԣ�Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.14 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_DB}

uses
  SysUtils, Classes, Controls, Grids, DB;

type
  {Goto Ways}
  TGotoWay = (gwFirst, gwPrior, gwNext, gwLast, gwNone); 
  {Event Declare}

  TOnPageChange = procedure(Sender: TObject; Page: Integer) of object; 
  {Class Inherit}

  TCnPagedGrid = class(TStringGrid)
  private
    {DataSource}
    FDataSource: TDataSource; 
    {Table's Page Count}
    FPageCount: Integer; 
    {Current Page}
    FPageNo: Integer; 
    {Display Fields' List}
    FFieldList: TStringList; 
    {Max Rows in a Page}
    FMaxRows: Integer; 
    {The Page Change Event}
    FOnPageChange: TOnPageChange; 
    {The Cell (0,0)}
    FCellZero: string; 
    {Goto Method}
    FMethod: TGotoWay; 
    {Set the Number of Current Page}
    procedure SetPageNo(const Value: Integer); 
    {Assign Field List}
    procedure SetFieldList(const Value: TStringList); 
    {Return the Page Count}
    function GetPageCount: Integer;
  protected
    {Create Table Title(Head)}
    procedure GetTitle; 
    {Clear Grid}
    procedure ClearGrid;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {Show Data First Time, and Set PageNo to 1}
    procedure ShowData; 
    {Get the Field list, must use this method}
    procedure GetFieldList;
    procedure AddField(const AFieldName, ADisplayName: string);
    procedure DeleteField(const AFieldName: string);
    procedure SetFieldName(const AFieldName, ADisplayName: string);
    procedure NavigatePage(GotoWay: TGotoWay);
    procedure Last;
    procedure First;
    procedure Prior;
    procedure Next;
  published
    {Goto Method}
    property GotoMethod: TGotoWay read FMethod write FMethod nodefault; 
    {Cell(0,0)}
    property CellZero: string read FCellZero write FCellZero; 
    {Event}
    property OnPageChange: TOnPageChange read FOnPageChange write FOnPageChange; 
    {Property}
    property MaxRows: Integer read FMaxRows write FMaxRows default 10;
    property FieldList: TStringList read FFieldList write SetFieldList;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property PageNo: Integer read FPageNo write SetPageNo default 1;
    property PageCount: Integer read GetPageCount write FPageCount;
  end;

{$ENDIF}

implementation

{$IFDEF SUPPORT_DB}

{ TCnPagedGrid }

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.AddField
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: const AFieldName, ADisplayName: string
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.AddField(const AFieldName, ADisplayName: string);
begin
  {The Style of List is "FieldName=DisplayName"}
  Self.FFieldList.Add(AFieldName + '=' + ADisplayName);
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.Create
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: AOwner: TComponent
  Result:    None
-------------------------------------------------------------------------------}
constructor TCnPagedGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFieldList := TStringList.Create;
  FFieldList.Clear; 
  {Set MaxRows' default value to 10}
  Self.FMaxRows := 10;
  FPageNo := 1;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.GetTitle
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.GetTitle;
var
  i: Integer;
  ColName: string;
  DispName: string;
  Xpo: Integer;
  ds: TDataSet;
begin
  if Self.FDataSource = nil then
    Exit;
  ds := Self.FDataSource.DataSet; 
  {Set Col Count to Fields' Count}
  Self.ColCount := Self.FFieldList.Count + 1;
  for i := 0 to Self.FFieldList.Count - 1 do
  begin
    {Get Equal Signal's Position}
    XPO := Pos('=', Self.FFieldList.Strings[i]); 
    {Get Value Name}
    ColName := Copy(Self.FFieldList.Strings[i], 1, xpo - 1);
    DispName := Self.FFieldList.Values[ColName]; 
    {Set Title's Width}
    Self.ColWidths[i + Self.FixedCols] := ds.FieldByName(ColName).DisplayWidth * Self.Canvas.TextWidth('0'); 
    {Set Title's Caption}
    Self.Cells[i + Self.FixedCols, 0] := DispName;
  end;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.DeleteField
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: const AFieldName: string
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.DeleteField(const AFieldName: string);
var
  i, Cnt: Integer;
  ColName: string;
  XPO: Integer;
begin
  Cnt := Self.FFieldList.Count;
  for i := 0 to Cnt - 1 do
  begin
    XPO := Pos('=', Self.FFieldList.Strings[i]);
    ColName := Copy(Self.FFieldList.Strings[i], 1, XPO - 1); 
    {If Gotten Value Name Equals to Field Name}
    if ColName = AFieldName then
    begin
      {Delete Current Item}
      Self.FFieldList.Delete(i);
      Break;
    end;
  end;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.Destroy
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
destructor TCnPagedGrid.Destroy;
begin
  FFieldList.Free;
  FDataSource := nil;
  inherited Destroy;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.GetFieldList
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.GetFieldList;
var
  i, FldCnt: Integer;
  FldName: string;
begin
  if Self.FDataSource = nil then
    Exit; 
  {Clear the Field List}
  Self.FFieldList.Clear;
  FldCnt := Self.FDataSource.DataSet.FieldCount;
  for i := 0 to FldCnt - 1 do
  begin
    {Get Field Name and Copy it}
    FldName := Self.FDataSource.DataSet.Fields.Fields[i].DisplayName;
    Self.FFieldList.Add(FldName + '=' + FldName);
  end;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.SetFieldList
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: const Value: TStringList
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.SetFieldList(const Value: TStringList);
begin
  {Assign Values, an Object must use "Assign" but not "="}
  FFieldList.Assign(Value);
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.SetFieldName
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: const AFieldName, ADisplayName: string
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.SetFieldName(const AFieldName, ADisplayName: string);
begin
  Self.FFieldList.Values[AFieldName] := ADisplayName;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.SetPageNo
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: const Value: Integer
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.SetPageNo(const Value: Integer);
var
  RecNum: Integer;
  Rows: Integer;
  i: Integer;
  ColName: string;
  Xpo: Integer;
  ds: TDataSet;
begin
  if Self.FDataSource = nil then
    Exit;
  ds := Self.FDataSource.DataSet; 
  {Check DataSet is Actived ot not}
  if not ds.Active then
    Exit; 
  //if (Self.FMethod=gwfirst)or(Self.FMethod=gwlast)then Exit;
  if FPageNo < 1 then
  begin
    FPageNo := 1;
    Exit;
  end;
  if FPageNo > PageCount then
  begin
    FPageNo := PageCount;
    Exit;
  end;
  FPageNo := Value; 
  {Get Page Count}
//  if ds.RecordCount=0 then
//    FPageCount:=1
//  else
//    FPageCount:=((ds.RecordCount-1) div FMaxRows)+1;
  {Count Current Record Number}
  if FPageNo < 1 then
  begin
    FPageNo := 1;
    Exit;
  end;
  if FPageNo > PageCount then
  begin
    FPageNo := PageCount;
    Exit;
  end;
  RecNum := (FPageNo - 1) * FMaxRows + 1;
  if RecNum > ds.RecordCount then
    Exit; 
  {Set Table's Current Record Number}
  if ds.RecordCount = 0 then
    ds.Last
  else
    ds.RecNo := RecNum;
  Rows := 0; 
  {Set Rows Forward}
  Self.RowCount := Self.FMaxRows + 1;
  Self.ClearGrid;
  while not ds.Eof do
  begin
    {Write Record Number to Grid Cell}
    if FixedCols <> 0 then
      Self.Cells[0, Rows + 1] := IntToStr(RecNum);
    for i := 0 to Self.FFieldList.Count - 1 do
    begin
      XPO := Pos('=', Self.FFieldList.Strings[i]);
      ColName := Copy(Self.FFieldList.Strings[i], 1, xpo - 1); 
      {Get Data from Table and Write It to Grid}
      Self.Cells[i + Self.FixedCols, Rows + 1] := ds.FieldByName(ColName).AsString;
    end;
    Inc(Rows);
    Inc(RecNum);
    if Rows = FMaxRows then
      break; 
    {dataSet's Pointer Move to Next Record}
    ds.Next;
  end; 
  {Set Rows}
  if Rows = 0 then
    Self.RowCount := 2
  else
    Self.RowCount := Rows + 1; 
  {The Page Change Event Binding}
  if Assigned(OnPageChange) then
    OnPageChange(Self, FPageNo);
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.ShowData
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.ShowData;
begin
  if Self.FDataSource = nil then
    Exit;
  Self.ClearGrid; 
  {Call GetTitle}
  Self.GetTitle;
  PageNo := 1;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.GetPageCount
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: None
  Result:    Integer
-------------------------------------------------------------------------------}
function TCnPagedGrid.GetPageCount: Integer;
var
  RowCnt: Integer;
begin
  Result := 0;
  if Self.FDataSource = nil then
    Exit; 
  {Get Table's Record Count}
  RowCnt := Self.FDataSource.DataSet.RecordCount;
  if RowCnt = 0 then
    FPageCount := 1
  else
    FPageCount := ((RowCnt - 1) div FMaxRows) + 1;
  Result := FPageCount;
end; 

{-------------------------------------------------------------------------------
  Procedure: TCnPagedGrid.ClearGrid
  Author:    Rarnu
  DateTime:  2006.09.20
  Arguments: None
  Result:    None
-------------------------------------------------------------------------------}
procedure TCnPagedGrid.ClearGrid;
var
  i, j: Integer;
begin
  for i := 0 to Self.ColCount - 1 do
    for j := 1 to Self.RowCount - 1 do
      Self.Cells[i, j] := '';
end;

procedure TCnPagedGrid.NavigatePage(GotoWay: TGotoWay);
begin
  case GotoWay of
    gwFirst:
      Self.First;
    gwLast:
      Self.Last;
    gwPrior:
      Self.Prior;
    gwNext:
      Self.Next;
  end;
  Self.FMethod := GotoWay;
end;

procedure TCnPagedGrid.First;
begin
  PageNo := 1;
end;

procedure TCnPagedGrid.Last;
var
  fNO: Integer;
begin
  fNo := PageNo;
  while fNO < PageCount do
  begin
    Self.Next;
    Inc(fNO);
  end;
end;

procedure TCnPagedGrid.Next;
begin
  PageNo := PageNo + 1;
end;

procedure TCnPagedGrid.Prior;
begin
  PageNo := PageNo - 1;
end;

{$ENDIF}
end.
