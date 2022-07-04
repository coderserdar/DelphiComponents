
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Report DB dataset            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_DBSet;

interface

{$I FR.inc}

uses
  SysUtils, Windows, Messages, Classes, FR_DBRel, FR_DSet
{$IFDEF IBO}
 , IB_Components
{$ELSE}
 , DB
{$ENDIF};

type
  TfrDBDataSet = class(TfrDataset)
  private
{$IFDEF IBO}
    FDataSet: TIB_DataSet;
    FDataSource: TIB_DataSource;
{$ELSE}
    FDataSet: TDataSet;
    FDataSource: TDataSource;
{$ENDIF}
    FOpenDataSource, FCloseDataSource: Boolean;
    FOnOpen, FOnClose: TNotifyEvent;
    FBookmark: TfrBookmark;
    FEof: Boolean;
{$IFDEF IBO}
    procedure SetDataSet(Value: TIB_DataSet);
    procedure SetDataSource(Value: TIB_DataSource);
{$ELSE}
    procedure SetDataSet(Value: TDataSet);
    procedure SetDataSource(Value: TDataSource);
{$ENDIF}
  protected
    procedure Notification(AComponent: TComponent;
                           Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init; override;
    procedure Exit; override;
    procedure First; override;
    procedure Next; override;
    procedure Open;
    procedure Close;
    function Eof: Boolean; override;
    function GetDataSet: TfrTDataSet;
  published
    property CloseDataSource: Boolean read FCloseDataSource write FCloseDataSource default False;
{$IFDEF IBO}
    property DataSet: TIB_DataSet read FDataSet write SetDataSet;
    property DataSource: TIB_DataSource read FDataSource write SetDataSource;
{$ELSE}
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property DataSource: TDataSource read FDataSource write SetDataSource;
{$ENDIF}
    property OpenDataSource: Boolean read FOpenDataSource write FOpenDataSource default True;
    property RangeBegin;
    property RangeEnd;
    property RangeEndCount;
    property OnCheckEOF;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnFirst;
    property OnNext;
    property OnOpen: TNotifyEvent read FOnOpen write FOnOpen;
  end;


implementation

uses FR_Class;

type
  EDSError = class(Exception);

constructor TfrDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOpenDataSource := True;
end;

procedure TfrDBDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FDataSource then
      FDataSource := nil
    else if AComponent = FDataSet then
      FDataSet := nil
end;

{$IFDEF IBO}
procedure TfrDBDataSet.SetDataSet(Value: TIB_DataSet);
{$ELSE}
procedure TfrDBDataSet.SetDataSet(Value: TDataSet);
{$ENDIF}
begin
  FDataSet := Value;
  FDataSource := nil;
end;

{$IFDEF IBO}
procedure TfrDBDataSet.SetDataSource(Value: TIB_DataSource);
{$ELSE}
procedure TfrDBDataSet.SetDataSource(Value: TDataSource);
{$ENDIF}
begin
  FDataSource := Value;
  if Value <> nil then
    FDataSet := nil;
end;

function TfrDBDataSet.GetDataSet: TfrTDataSet;
begin
  if (FDataSource <> nil) and (FDataSource.DataSet <> nil) then
    Result := TfrTDataSet(FDataSource.DataSet)
  else if FDataSet <> nil then
    Result := TfrTDataSet(FDataSet)
  else
  begin
    raise EDSError.Create('Unable to open dataset ' + Name);
    Result := nil;
  end;
end;

procedure TfrDBDataSet.Init;
begin
  Open;
  FBookmark := frGetBookmark(TfrTDataSet(GetDataSet));
  FEof := False;
end;

procedure TfrDBDataSet.Exit;
begin
  if FBookMark <> frEmptyBookmark then
  begin
    if (FRangeBegin = rbCurrent) or (FRangeEnd = reCurrent) then
      frGotoBookmark(TfrTDataSet(GetDataSet), FBookmark);
    frFreeBookmark(TfrTDataSet(GetDataSet), FBookmark);
  end;
  FBookMark := frEmptyBookmark;
  Close;
end;

procedure TfrDBDataSet.First;
begin
  if FRangeBegin = rbFirst then
    GetDataSet.First
  else if FRangeBegin = rbCurrent then
    frGotoBookmark(GetDataSet, FBookmark);
  FEof := False;
  inherited First;
end;

procedure TfrDBDataSet.Next;
var
  b: TfrBookmark;
begin
  FEof := False;
  if FRangeEnd = reCurrent then
  begin
    b := frGetBookmark(GetDataSet);
    if frIsBookmarksEqual(GetDataSet, b, FBookmark) then
      FEof := True;
    frFreeBookmark(GetDataSet, b);
    System.Exit;
  end;
  GetDataSet.Next;
  inherited Next;
end;

function TfrDBDataSet.Eof: Boolean;
begin
  Result := inherited Eof or GetDataSet.Eof or FEof;
end;

procedure TfrDBDataSet.Open;
begin
  if FOpenDataSource then GetDataSet.Open;
  if Assigned(FOnOpen) then FOnOpen(Self);
end;

procedure TfrDBDataSet.Close;
begin
  if Assigned(FOnClose) then FOnClose(Self);
  if FCloseDataSource then GetDataSet.Close;
end;



end.
