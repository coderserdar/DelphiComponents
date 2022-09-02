(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I BTDefine.Inc}
{$F-,V-,B-,S-,I-,R-}
{$IFDEF CanSetOvrflowCheck}
  {$Q-,P-}
{$ENDIF}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

unit OopSema;

interface

uses
  NWBase,
  NWSema;

type
  PSemaphore = ^TSemaphore;
  TSemaphore = object
    {-An object that simply encapsulates the semaphore calls}
    ssName   : ^string;
    ssHandle : nwLong;
    ssServer : TnwServer;
    ssError  : word;

    constructor Init(Server : TnwServer;
                     Name   : TnwSemaName;
                     InitialValue : nwInt);
      {-Open a semaphore}
    destructor Done; virtual;
      {-Close a semaphore}
    function GetLastError : word;
      {-Return the last error}
    function GetName : string;
      {-Return the name of the semaphore}
    function GetOpenCount : word;
      {-Return the number of processes that have the semaphore open}
    function GetValue : nwInt;
      {-Return the current value of the semaphore}
    procedure IncValue;
      {-Increment the value of the semaphore}
    procedure DecValue(TimeOut : word);
      {-Decrement the value of the semaphore}
  end;


type
  SemaphoreRec = record
    Sema : PSemaphore;
    Valu : nwInt;
    Cnt  : word;
  end;

const
  MaxSemaphores = 65520 div sizeof(SemaphoreRec);

type
  SemaphoreList = Array [1..MaxSemaphores] of SemaphoreRec;

  {This is a higher level semaphore object used to provide synchronization
   between workstations. See TurboPower's FBDEMO for an example usage of this
   object type.}
  TFilerSemaphore = object
    fsNrOfKeys   : integer;
    fsSemaphores : ^SemaphoreList;
    constructor Init(Server : TnwServer; Name : String; NrOfKeys : integer);
      {-create the semaphores}
    destructor Done; Virtual;
      {-destroy the semaphores}
    procedure IndicateDirty(KeyNr : integer);
      {-Indicate to others that a file has been modified}
    function IsDirty(KeyNr : integer) : boolean;
      {-see if the file has been modified}
    function NumberOpen(KeyNr : integer) : word;
      {-Return the number of stations currently using the semaphore}
  end;

implementation

constructor TSemaphore.Init(Server : TnwServer;
                            Name   : TnwSemaName;
                            InitialValue : nwInt);
  var
    Result : word;
    OpenCount : word;

  begin
    Result := nwOpenSema(Server, Name, InitialValue,
                            OpenCount, ssHandle);
    if (Result <> 0) then
      Fail;
    ssServer := Server;

    if not nwGetMem(ssName, succ(length(Name))) then
      begin
        Done;
        Fail;
      end;
    ssName^ := Name;
    ssError := 0;
  end;

destructor TSemaphore.Done;
  begin
    if (ssName <> nil) then
      FreeMem(ssName, succ(length(ssName^)));
    ssError := nwCloseSema(ssServer, ssHandle);
  end;

function TSemaphore.GetLastError : word;
  begin
    GetLastError := ssError;
    ssError := 0;
  end;

function TSemaphore.GetName : string;
  begin
    if (ssName <> nil) then
         GetName := ssName^
    else GetName := '';
  end;

function TSemaphore.GetOpenCount : word;
  var
    Value : nwInt;
    OpenCount : word;
  begin
    GetOpenCount := 0;
    if (ssError = 0) then
      begin
        ssError := nwExamineSema(ssServer, ssHandle, Value, OpenCount);
        if (ssError = 0) then
          GetOpenCount := OpenCount;
      end;
  end;

function TSemaphore.GetValue : nwInt;
  var
    Value : nwInt;
    OpenCount : word;
  begin
    GetValue := 0;
    if (ssError = 0) then
      begin
        ssError := nwExamineSema(ssServer, ssHandle, Value, OpenCount);
        if (ssError = 0) then
          GetValue := Value;
      end;
  end;

procedure TSemaphore.IncValue;
  begin
    if (ssError = 0) then
      ssError := nwIncSema(ssServer, ssHandle);
  end;

procedure TSemaphore.DecValue(TimeOut : word);
  begin
    if (ssError = 0) then
      ssError := nwDecSema(ssServer, ssHandle, TimeOut);
  end;




function IntToStr(I : Integer) : String;
  var
    S : string[11];
  begin
    Str(I, S);
    IntToStr := S;
  end;

constructor TFilerSemaphore.Init(Server : TnwServer;
                                 Name   : string;
                                 NrOfKeys : integer);
  var
    i    : integer;
    Size : word;
  begin
    if (NrOfKeys < 1) or (NrOfKeys > MaxSemaphores) then
      Fail;

    Size := NrOfKeys * SizeOf(SemaphoreRec);
    if not nwGetMem(fsSemaphores, Size) then
      Fail;
    FillChar(fsSemaphores^, Size, 0);
    fsNrOfKeys := NrOfKeys;

    for i := 1 to NrOfKeys do
      with fsSemaphores^[I] do
        begin
          if not nwGetMem(Sema, sizeof(TSemaphore)) then
            begin
              Done;
              Fail;
            end;
          Sema^.Init(Server, 'fs__'+Name+IntToStr(i), 0);
          with Sema^ do
            begin
              Valu := GetValue;
              Cnt := GetOpenCount;
              if (GetLastError <> 0) then
                begin
                  Done;
                  Fail;
                end;
            end;
        end;
  end;

destructor TFilerSemaphore.Done;
  var
    i : integer;
  begin
    if (fsSemaphores <> nil) then
      begin
        for i := 1 to fsNrOfKeys do
          with fsSemaphores^[i] do
            if (Sema <> nil) then
              Dispose(Sema, Done);
        FreeMem(fsSemaphores, fsNrOfKeys * sizeof(SemaphoreRec));
      end;
  end;

procedure TFilerSemaphore.IndicateDirty(KeyNr : Integer);
  var
    i : integer;
  begin
    if (1 <= KeyNr) and (KeyNr <= fsNrOfKeys) then
      with fsSemaphores^[KeyNr], Sema^ do
        begin
          IncValue;
          if (GetLastError = nwsErrOverflow) then
            begin
              for i := 127 downto 1 do
                DecValue(0);
              if (GetLastError = nwsErrTimeout) then
                {nothing};
            end;
          Valu := GetValue;
          Cnt := GetOpenCount;
          if (GetLastError <> 0) then
            {nothing};
        end;
  end;

function TFilerSemaphore.IsDirty(KeyNr : Integer) : Boolean;
  var
    CurValue : nwInt;
  begin
    IsDirty := False;
    if (1 <= KeyNr) and (KeyNr <= fsNrOfKeys) then
      with fsSemaphores^[KeyNr], Sema^ do
        begin
          CurValue := GetValue;
          if (GetLastError = 0) and
             (CurValue <> Valu) then
            begin
              IsDirty := true;
              Valu := CurValue;
            end;
        end;
  end;

function TFilerSemaphore.NumberOpen(KeyNr : Integer) : word;
  begin
    NumberOpen := 0;
    if (1 <= KeyNr) and (KeyNr <= fsNrOfKeys) then
      with fsSemaphores^[KeyNr], Sema^ do
        begin
          Cnt := GetOpenCount;                                  {!!.52}
          if GetLastError = 0 then
            NumberOpen := Cnt;
        end;
  end;

end.
