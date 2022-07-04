unit KADaoDummyDataset;
//******************************************************************************
//                    Delphi Dao Project Version 2.40
//                 Copyright (c) 2000 by Kiril Antonov
//******************************************************************************
{$I KADaoCommonDirectives.pas}
interface
uses
 Windows, SysUtils, Classes, Db, Forms
 {$IFDEF D6UP}, Variants{$ENDIF};

Type
TDummyDataset = class(TDataSet)
private
protected
        Procedure       InternalOpen; override;
        Procedure       InternalClose; override;
        Function        IsCursorOpen: Boolean; override;
        Function        GetCanModify: Boolean; override;
        Function        GetRecordSize: Word;override;
        Function        AllocRecordBuffer: PChar; override;
        Procedure       FreeRecordBuffer(var Buffer: PChar); override;
        Procedure       InternalFirst;override;
        Procedure       InternalLast;override;
        Procedure       InternalInitFieldDefs; override;
        Procedure       InternalInitRecord(Buffer: PChar); override;
        Procedure       SetFieldData(Field: TField; Buffer: Pointer);override;
        Procedure       InternalEdit; override;
        Procedure       InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
        Procedure       InternalPost; override;
        Procedure       InternalDelete; override;
        Function        GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
        Procedure       InternalSetToRecord(Buffer: PChar); override;
        Procedure       InternalRefresh; override;
        Procedure       InternalGotoBookmark(Bookmark: Pointer); override;
        Function        GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
        Procedure       SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
        Function        GetBookmarkStr: TBookmarkStr; override;
        Procedure       SetBookmarkStr(const Value: TBookmarkStr); override;
        Procedure       GetBookmarkData(Buffer: PChar; Data: Pointer); override;
        Procedure       SetBookmarkData(Buffer: PChar; Data: Pointer); override;
        Procedure       InternalHandleException; override;
        Procedure       DataEvent(Event: TDataEvent; Info: Longint); override;
        Function        GetRecordCount  : Integer; override;
        Function        GetRecNo        : Integer; override;
  public
        Constructor                        Create(AOwner: TComponent); override;
        Destructor                         Destroy; override;
        Function                           GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
        Function                           CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
        Function                           Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
        Function                           Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
        Function                           CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
  published
end;



implementation
//****************************************************************************
constructor TDummyDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TDummyDataset.Destroy;
begin
  inherited Destroy;
end;

Procedure TDummyDataset.InternalOpen;
Begin
//******************************************************************************
end;

Procedure TDummyDataset.InternalClose;
begin
//******************************************************************************
end;

Procedure TDummyDataset.InternalFirst;
begin
//******************************************************************************
end;

Procedure TDummyDataset.InternalLast;
begin
//******************************************************************************
end;

Procedure TDummyDataset.InternalSetToRecord(Buffer: PChar);
begin
//******************************************************************************
end;

Procedure TDummyDataset.InternalRefresh;
begin
//******************************************************************************
end;

procedure TDummyDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
//******************************************************************************
end;

Function TDummyDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
Result:=bfCurrent;
//******************************************************************************
end;

procedure TDummyDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
//******************************************************************************
end;

Function TDummyDataset.GetBookmarkStr: TBookmarkStr;
Begin
  Result := '';
  //****************************************************************************
End;

Procedure TDummyDataset.SetBookmarkStr(const Value: TBookmarkStr);
Begin
//******************************************************************************
End;

procedure TDummyDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin                                                                                                    
//******************************************************************************
end;

procedure TDummyDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
//******************************************************************************
end;

Function TDummyDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
Begin
  Result := 0;
  //****************************************************************************
End;

Function TDummyDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
Begin
  Result:=Nil;
End;


Procedure TDummyDataset.InternalInitFieldDefs;
begin
//******************************************************************************
end;


Procedure TDummyDataset.InternalHandleException;
begin
  Application.HandleException(Self);
  //****************************************************************************
end;


procedure TDummyDataset.DataEvent(Event: TDataEvent; Info: Longint);
begin
//******************************************************************************
end;


Function TDummyDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
 Result :=False;
//******************************************************************************
end;

Procedure TDummyDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
//******************************************************************************
end;

procedure TDummyDataset.InternalEdit;
begin
   inherited InternalEdit;
   //***************************************************************************
end;

procedure TDummyDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
//******************************************************************************
end;

procedure TDummyDataset.InternalPost;
Begin
//******************************************************************************
End;


Procedure TDummyDataset.InternalDelete;
Begin
//******************************************************************************
End;

Function TDummyDataset.IsCursorOpen: Boolean;
begin
 Result:=False;
//******************************************************************************
end;

Function TDummyDataset.GetCanModify: Boolean;
begin
 Result:=False;
//******************************************************************************
end;

Function TDummyDataset.GetRecordSize: Word;
begin
 Result:=0;
//******************************************************************************
end;

Function TDummyDataset.AllocRecordBuffer: PChar;
begin
 Result :=Nil;
//******************************************************************************
end;

procedure TDummyDataset.InternalInitRecord(Buffer: PChar);
begin
//******************************************************************************
end;

Procedure TDummyDataset.FreeRecordBuffer(var Buffer: PChar);
begin
//******************************************************************************
end;

Function TDummyDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
 Result := grOK;
 //*****************************************************************************
end;

Function TDummyDataset.GetRecordCount: Integer;
Begin
 Result:=-1;
//******************************************************************************
End;

Function  TDummyDataset.GetRecNo: Integer;
Begin
  Result := -1;
 //*****************************************************************************
End;


Function  TDummyDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
Begin
 Result:=False;
//******************************************************************************
End;

Function  TDummyDataset.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
Begin
 Result:= NULL;
//******************************************************************************
End;
end.

