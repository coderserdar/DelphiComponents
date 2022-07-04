{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLEvents.pas
   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}

{$I fbl.inc}

{
@abstract(Managing database events)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FDBEvents.pas synchronous event  notification
}

unit FBLEvents;

interface

uses
  Classes, SysUtils,
  SyncObjs, FBLDatabase, ibase_h, FBLExcept;

type
  {Occurs when firebird events manager notifies an event}
  TOnPostEvent = procedure(Sender: TObject; EventName: string; EventCount: integer) of
  object;
  {@exclude}
  TFBLThreadEvent = class;

  {@abstract(encapsulates the properties and methods for managing database events)}
  TFBLEvent = class(TComponent, IFBLDbEvent)
  private
    FDatabase: TFBLDatabase;
    FEventList: TStrings;
    FThreadEvent: TFBLThreadEvent;
    FOnPostEvent: TOnPostEvent;
    FThreadActive: boolean;
    procedure SetEventList(Value: TStrings);
    procedure SetDatabase(Value: TFBLDatabase);
    procedure CheckDatabase;
    procedure CheckEventList;
    procedure PostEvent(Sender: TObject; EventName: string; EventCount: integer);
    procedure OnChangeEventList(Sender: TObject);
  public
    {Create an instance  of a TFBLEvent}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;
    {$IFDEF FPC_INFD}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
    {$ENDIF}
    {@exclude}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    {@exclude}
    procedure DoOnDatabaseDisconnect;
    {@exclude}
    procedure DoOnDestroy;
    {start to listen events notification}
    procedure Start;
    {stop to listen events notification}
    procedure Stop;
    {True if notification is listening for events}
    property Active: boolean read FThreadActive;
  published
    {List of register events}
    property EventList: TStrings read FEventList write SetEventList;
    {TFBLDatabase object where is attached current TFBLEvent instance }
    property Database: TFBLDatabase read FDatabase write SetDatabase;
    {Occurs when firebird events manager notifies an event}
    property OnPostEvent: TOnPostEvent read FOnPostEvent write FOnPostEvent;
  end;


  //Thread Class
  {@exclude}
  TFBLThreadEvent = class(TThread)
  private
    FPrepared: boolean;         //True if FEventBuffer and FResultBuffer is Allocated
    FFirstTime: boolean;        //True if First event of isc_que_events
    FEventBuffer: PChar;        //Buffer allocated by isc_event_block
    FResultBuffer: PChar;       //Buffer allocated by isc_event_block
    FEventList: TStrings;       //List of events
    FBlockLength,               //Length of FEventBuffer and  FResultBuffer set by isc_event_block
    FEventID: ISC_LONG;         //Handle of current event  set by isc_que_events
    FNumberOfStock: integer;    // number of events
    FDBHandle: PISC_DB_HANDLE;  //Handle of database
    FEventPosted: boolean;
    //True if Event is posted set by isc_que_events and AST Function
    FOnPostEvent: TOnPostEvent;
    FCS: TCriticalSection;
    procedure SetEventList(Value: TStrings);
    procedure ReadEvent;
    procedure AllocBlock;
    procedure FreeBlock;
    procedure ProcessEvents;
    procedure QueEvents;
    procedure CancelEvent;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateBuffer(ALength: UShort; AUpdate: PChar);
    procedure PostEvent;
    procedure StartThread;
    property DBHandle: PISC_DB_HANDLE read FDBHandle write FDBHandle;
    property Prepared: boolean read FPrepared write FPrepared;
    property EventList: TStrings read FEventList write SetEventList;
    property OnPostEvent: TOnPostEvent read FOnPostEvent write FOnPostEvent;
  end;

  {$IFNDEF FPC}
  TFBL_event_block = function(EventBuffer, ResultBuffer: PPChar; IDCount: UShort;
    Event1, Event2, Event3, Event4, Event5, Event6, Event7, Event8, Event9,
    Event10, Event11, Event12, Event13, Event14, Event15: PChar): ISC_LONG; 
  cdecl;
  {$ENDIF}
const
  {@exclude}
  MAX_EVENTS = 15;

implementation

uses
  FBLConst;

constructor TFBLEvent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FThreadActive := False;
  FEventList := TStringList.Create;
  TStringList(FEventList).OnChange := OnChangeEventList;
end;

//------------------------------------------------------------------------------

destructor TFBLEvent.Destroy;
begin
  TStringList(FEventList).OnChange := nil;
  FEventList.Free;
  if Assigned(FDatabase) then FDatabase.RemoveAttachObj(self);
  inherited Destroy;
end;

//------------------------------------------------------------------------------
{$IFDEF FPC_INFD}
function TFBLEvent.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then Result := 0 
  else 
    Result := E_NOINTERFACE;
end;

function TFBLEvent._AddRef: integer;
begin
  Result := -1;
end;

function TFBLEvent._Release: integer;   
begin
  Result := -1;
end;
{$ENDIF}
//------------------------------------------------------------------------------

procedure TFBLEvent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FDatabase then FDatabase := nil;
  end;
  inherited Notification(AComponent, Operation);
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.PostEvent(Sender: TObject; EventName: string; EventCount: integer);
begin
  if Assigned(FOnPostEvent) then
    FOnPostEvent(Self, EventName, EventCount);
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.DoOnDatabaseDisconnect;
begin
  if FThreadActive then
    Stop;
end;

//------------------------------------------------------------------------------

procedure TFBLevent.DoOnDestroy;
begin
  if FDatabase <> nil then FDatabase := nil;
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.SetEventList(Value: TStrings);
begin
  FEventList.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.OnChangeEventList(Sender: TObject);
begin
  if FThreadActive then
  begin
    Stop;
    Start;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.SetDatabase(Value: TFBLDatabase);
begin
  if Assigned(Value) and (Value <> FDatabase) then
    Value.AddAttachObj(self);
  FDatabase := Value;
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.CheckDatabase;
begin
  if not Assigned(FDatabase) then
    FBLError(E_TR_DB_NOT_ASSIGNED);
  if not FDatabase.Connected then
    FBLError(E_DB_NOACTIVE_CON);
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.CheckEventList;
var
  nItem: integer;
begin
  nItem := FEventList.Count;
  if nItem = 0 then
    FBLError(E_EV_LIST_EMPTY);
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.Start;
begin
  CheckDatabase;
  CheckEventList;
  if (FThreadEvent = nil) or (FThreadActive = False) then
  begin
    FThreadEvent := TFBLThreadEvent.Create;
    FThreadEvent.DBHandle := FDatabase.DBHandle;
    FThreadEvent.EventList := EventList;
    FThreadEvent.OnPostEvent := PostEvent;
    FThreadEvent.StartThread;
    FThreadActive := True
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLEvent.Stop;
begin
  if FThreadActive then
  begin
    FThreadEvent.Terminate;
    FThreadActive := False;
  end;
end;

//------------------------------------------------------------------------------
//   CALLBACK AST Function
//------------------------------------------------------------------------------

procedure ASTFunction(APointer: Pointer; ALength: UShort; AUpdate: PChar); cdecl;
begin
  if (Assigned(APointer) and Assigned(AUpdate)) then
  begin
    TFBLThreadEvent(APointer).UpdateBuffer(ALength, AUpdate);
    TFBLThreadEvent(APointer).PostEvent;
  end;
end;

//-------------------------------------------------------------------------------
// TFBLThreadEvent
//-------------------------------------------------------------------------------

constructor TFBLThreadEvent.Create;
begin
  inherited Create(True);
  {$IFDEF UNIX}
  //Priority := 0;
  {$ELSE}
  Priority := tpIdle;
  {$ENDIF}
  FreeOnTerminate := True;
  FCS := TCriticalSection.Create;
  FPrepared := False;
  FEventPosted := False;
  FNumberOfStock := 0;
  FFirstTime := True;
  FEventList := TStringList.Create;
  FEventBuffer := nil;
  FResultBuffer := nil;
end;

//------------------------------------------------------------------------------

destructor TFBLThreadEvent.Destroy;
begin
  FEventList.Free;
  FreeBlock;
  FCS.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.SetEventList(Value: TStrings);
begin
  FEventList.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.UpdateBuffer(ALength: UShort; AUpdate: PChar);
begin
  Move(AUpdate[0], FResultBuffer[0], ALength);
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.PostEvent;
begin
  FEventPosted := True;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.ReadEvent;
var
  i: integer;
  Status_vector: ISC_STATUS_VECTOR;
begin
  FCS.Enter;
  try
    isc_event_counts(@Status_vector, Short(FBlockLength), FEventBuffer, FResultBuffer);
    if Assigned(FOnPostEvent) and (not FFirstTime) then
    begin
      for i := 0 to FNumberOfStock - 1 do
      begin
        if Status_vector[i] <> 0 then
          FOnPostEvent(Self, FEventList.Strings[i], Status_vector[i]);
      end;
    end;
    FFirstTime := False;
  finally
    FCS.Leave;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.ProcessEvents;
begin
  if FEventPosted then
  begin
    ReadEvent;
    FEventPosted := False;
  end;
  QueEvents;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.StartThread;
begin
  if not FPrepared then
    AllocBlock;
  if Suspended then
    Resume;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.QueEvents;
var
  Status_vector: ISC_STATUS_VECTOR;
begin
  FCS.Enter;
  try
    isc_que_events(@Status_vector, FDBHandle, @FEventId, Short(FBlockLength),
      FEventBuffer,
      TISC_CALLBACK(@ASTFunction), PVoid(Self));
    if (status_vector[0] = 1) and (status_vector[1] > 0) then
      FBLShowError(@Status_vector);
    CancelEvent;
  finally
    FCS.Leave;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.AllocBlock;
  function EPB(AIdx: integer): PChar;
  begin
    if (AIdx < FNumberOfStock) and (FNumberOfStock > 0) then
      Result := PChar(FEventList.Strings[AIdx])
    else
      Result := nil;
  end;
begin
  FCS.Enter;
  try
    if not FPrepared then
    begin
      FBlockLength := 0;
      FNumberOfStock := FEventList.Count;
      if FNumberOfStock > MAX_EVENTS then
        FNumberOfStock := MAX_EVENTS;
      {$IFDEF FPC}
      FBlockLength := isc_event_block(@FEventBuffer, @FResultBuffer,
        UShort(FNumberOfStock), EPB(0), EPB(1), EPB(2), EPB(3), EPB(4),
        EPB(5), EPB(6), EPB(7),
        EPB(8), EPB(9), EPB(10), EPB(11), EPB(12), EPB(13), EPB(14));
      {$ELSE}
      FBlockLength := TFBL_event_block(isc_event_block)(@FEventBuffer, @FResultBuffer,
        UShort(FNumberOfStock), EPB(0), EPB(1), EPB(2), EPB(3), EPB(4),
        EPB(5), EPB(6), EPB(7),
        EPB(8), EPB(9), EPB(10), EPB(11), EPB(12), EPB(13), EPB(14));
      {$ENDIF}
      FPrepared := True;
    end;
  finally
    FCS.Leave;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.FreeBlock;
begin
  FCS.Enter;
  try
    isc_free(FEventBuffer);
    FEventBuffer := nil;
    isc_free(FResultBuffer);
    FResultBuffer := nil;
  finally
    FCS.Leave;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.CancelEvent;
var
  Status_vector: ISC_STATUS_VECTOR;
begin
  FCS.Enter;
  try
    isc_cancel_events(@Status_vector, DbHandle, @FEventId);
    if (status_vector[0] = 1) and (status_vector[1] > 0) then
      FBLShowError(@Status_vector);
  finally
    FCS.Leave;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLThreadEvent.Execute;
begin
  try
    while not Terminated do
    begin
      Synchronize(ProcessEvents);
      {$IFNDEF D6M}
      Sleep(1);
      {$ENDIF} 
    end;
  except
    on E: Exception do raise Exception.Create('Fatal error in "TFBLThreadEvent"');
  end;
end;

//------------------------------------------------------------------------------

end.
