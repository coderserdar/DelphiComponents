{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: Interruptable.pas,v 1.7 2003/02/26 08:03:09 laa Exp $}

unit Interruptable;

interface

uses
  SyncObjs, DataTypes;

type
  TInterruptable = class;

  TOperationEvent = procedure(Sender : TInterruptable; Operation : TOperationType) of object;

  TInterruptable = class
  private
    FInterruptInterval : Integer;
    FOnStart : TOperationEvent;
    FOnInterrupt : TOperationEvent;
    FOnFinished : TOperationEvent;
//    FRowsAffected : Integer;
    FRowsAffected : array[TOperationType] of Integer;
    FSourceToCount : array[TOperationType] of TObject;
    FInterrupted : Boolean;
    FInterruptEvent : TSimpleEvent;

    function GetInterruptInterval : Integer;
    function GetOnStart : TOperationEvent;
    function GetOnInterrupt : TOperationEvent;
    function GetOnFinished : TOperationEvent;
    function GetRowsAffected(AType:TOperationType) : Integer;
    procedure SetRowsAffected(AType:TOperationType; RA : Integer);
    function GetInterrupted : Boolean;
    procedure SetInterrupted(I : Boolean);
    function GetSourceToCount(AType: TOperationType): TObject;
    procedure SetSourceToCount(AType: TOperationType;
      const Value: TObject);
    function IsSourceToCount(Operation:TOperationType; Sender:TObject) : boolean;
  public
    property InterruptInterval : Integer read GetInterruptInterval write FInterruptInterval;

    property OnStart : TOperationEvent read GetOnStart write FOnStart;
    property OnInterrupt : TOperationEvent read GetOnInterrupt write FOnInterrupt;
    property OnFinished : TOperationEvent read GetOnFinished write FOnFinished;

    procedure InitParams(Operation : TOperationType; Sender:TObject);
    function CheckInterrupt(OperationType : TOperationType) : Boolean;
    procedure AddRow(Operation : TOperationType; Sender:TObject);
    procedure Finished(Operation : TOperationType; Sender:TObject);

    // Sleep function that returns if Interrupted is set
    procedure Sleep(Timeout : Cardinal);
    property RowsAffected[AType:TOperationType] : Integer read GetRowsAffected write SetRowsAffected;
    // If you only want counting on one TRowSource/TReadConnection assign it here
    property SourceToCount[AType:TOperationType] : TObject read GetSourceToCount write SetSourceToCount;
    property Interrupted : Boolean read GetInterrupted write SetInterrupted;

    constructor Create;
    destructor Destroy; override;
  end;

  function IsInterrupted(AInterruptable : TInterruptable) : Boolean;

implementation

uses
  Sysutils;
  
function IsInterrupted(AInterruptable : TInterruptable) : Boolean;
begin
  Result := (AInterruptable<>nil) and AInterruptable.Interrupted;
end;

{ TInterruptable }

constructor TInterruptable.Create;
var
  aType : TOperationType;
begin
  FInterruptEvent := nil;

  inherited Create;

  FInterruptInterval := 10;
  FOnInterrupt := nil;
  Interrupted := False;
  for aType := Low(TOperationType) to High(TOperationType) do
  begin
    fRowsAffected[AType] := 0;
    FSourceToCount[AType] := nil;
  end;
end;

destructor TInterruptable.Destroy;
begin
  if Assigned(FInterruptEvent) then
    FInterruptEvent.SetEvent;
  inherited Destroy;

  FInterruptEvent.Free;
end;

function TInterruptable.CheckInterrupt(OperationType : TOperationType) : Boolean;
  function InterruptIntervalPassed : Boolean;
  begin
    // LAA: detta betyder att interrupt-intervallet är väldans mycket beroende
    // på hur man definierat att RowsAffected skall räknas. Det skulle vara
    // rättare att ha en skild counter för detta -- men lite overkill.
    Result := (RowsAffected[OperationType] mod InterruptInterval = 0)
  end;

begin
  if (Self <> nil) and
     (InterruptInterval > 0) and
     Assigned(Self.OnInterrupt) and
     InterruptIntervalPassed then
    Self.OnInterrupt( Self, OperationType );

  Result := Interrupted;
end;

function TInterruptable.IsSourceToCount(Operation:TOperationType; Sender:TObject) : boolean;
begin
  // The default is to count all AddRows of each type; however if one source
  // is declared to be SourceToCount disregard all other
  Result := (FSourceToCount[Operation] = nil) or (FSourceToCount[Operation] = Sender);
end;

procedure TInterruptable.AddRow(Operation : TOperationType; Sender:TObject);
begin
  if Self<>nil then
  begin
    if IsSourceToCount(Operation,Sender) then
      RowsAffected[Operation] := RowsAffected[Operation] + 1;

    Self.CheckInterrupt(Operation);
  end;
end;

procedure TInterruptable.InitParams(Operation : TOperationType; Sender:TObject);
begin
  if (Self <> nil) and IsSourceToCount(Operation,Sender) then
  begin
    RowsAffected[Operation] := 0;

    if Assigned(Self.OnStart) then
      Self.OnStart(Self, Operation);
  end;
end;

procedure TInterruptable.Finished(Operation : TOperationType; Sender:TObject);
begin
  if (Self <> nil) and IsSourceToCount(Operation,Sender) and Assigned(Self.OnFinished) then
    Self.OnFinished(Self, Operation);
end;

function TInterruptable.GetInterruptInterval : Integer;
begin
  if Self = nil then
    Result := 0
  else
    Result := FInterruptInterval;
end;

function TInterruptable.GetRowsAffected(AType:TOperationType) : Integer;
begin
  if Self = nil then
    Result := 0
  else
    Result := FRowsAffected[AType];
end;

procedure TInterruptable.SetRowsAffected(AType:TOperationType; RA : Integer);
begin
  if Self <> nil then
    FRowsAffected[AType] := RA;
end;

function TInterruptable.GetInterrupted : Boolean;
begin
  if Self = nil then
    Result := False
  else
    Result := FInterrupted;
end;

procedure TInterruptable.SetInterrupted(I : Boolean);
begin
  if Self <> nil then
  begin
    FInterrupted := I;
    if FInterruptEvent <> nil then
    begin
      if I then
        FInterruptEvent.SetEvent
      else
        FInterruptEvent.ResetEvent;
    end;
  end;
end;

function TInterruptable.GetOnStart : TOperationEvent;
begin
  if Self = nil then
    Result := nil
  else
    Result := FOnStart;
end;

function TInterruptable.GetOnInterrupt : TOperationEvent;
begin
  if Self = nil then
    Result := nil
  else
    Result := FOnInterrupt;
end;

function TInterruptable.GetOnFinished : TOperationEvent;
begin
  if Self = nil then
    Result := nil
  else
    Result := FOnFinished;
end;

function TInterruptable.GetSourceToCount(AType: TOperationType): TObject;
begin
  if Self = nil then
    Result := nil
  else
    Result := fSourceToCount[AType];
end;

procedure TInterruptable.SetSourceToCount(AType: TOperationType; const Value: TObject);
begin
  if Self <> nil then
    fSourceToCount[AType] := Value;
end;

procedure TInterruptable.Sleep(Timeout: Cardinal);
begin
  if not Interrupted then
  begin
    if FInterruptEvent = nil then
      FInterruptEvent := TSimpleEvent.Create;
    if Interrupted then
      FInterruptEvent.SetEvent
    else
      FInterruptEvent.WaitFor(Timeout);
  end;
end;

end.

