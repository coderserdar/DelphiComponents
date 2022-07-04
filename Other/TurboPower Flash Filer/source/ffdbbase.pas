{*********************************************************}
{* FlashFiler: Support classes for FFDB                  *}
{*********************************************************}

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
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit ffdbbase;

interface

uses
  classes,
  db,
  ffclbase,                                                            {!!.06}
  ffsrbde,
  ffllbase,
  ffsrmgr;

{$I ffdscnst.inc}

var
  ffStrResDataSet : TffStringResource;

type
  EffDatabaseError = class(EDatabaseError)
  protected {private}
    deErrorCode : TffResult;
  protected
    function deGetErrorString : string;
  public
    constructor Create(const aMsg : string);
    constructor CreateViaCode(aErrorCode : TffResult; aDummy : Boolean);
    constructor CreateViaCodeFmt(const aErrorCode : TffResult;         {!!.06}
                                 const args : array of const;          {!!.06}
                                 const aDummy : Boolean);              {!!.06}
    constructor CreateWithObj(aObj : TComponent;
                        const aErrorCode : TffResult;
                        const aMsg : string);
    constructor CreateWithObjFmt(aObj : TComponent; const aErrorCode : TffResult;
                                 const args : array of const);         {!!.11}
    property ErrorCode : TffResult read deErrorCode;
    property ErrorString : string read deGetErrorString;
  end;

type
  TffDBListItem = class;
  TffDBList = class;

  TffDBListItem = class(TffComponent)
    protected {private}
      dbliActive       : Boolean;
      dbliDBName       : string;
      dbliDBOwner      : TffDBListItem;
      dbliDBOwnerName  : string;
      dbliFailedActive : Boolean;
      dbliFixing       : Boolean;
      dbliLoading      : Boolean;
      dbliMakeActive   : Boolean;
      dbliOwnedDBItems : TffDBList;
      dbliReqPropName  : string;
      dbliTemporary    : Boolean;                                      {!!.01}
        { The actual name of the required property corresponding to DBName. }
    protected
      dbliLoadPriority : Integer; {*not* private, descendants set it}
      dbliNeedsNoOwner : Boolean; {*not* private, descendants set it}

      function dbliGetDBOwner : TffDBListItem;
      function dbliGetDBOwnerName : string;
      function dbliGetOwned : Boolean;
      procedure dbliSetActive(const aValue : Boolean);
      procedure dbliSetDBName(const aName : string);
      procedure dbliSetDBOwner(const aDBOwner : TffDBListItem);
      procedure dbliSetDBOwnerName(const aName : string);

      procedure dbliClosePrim; virtual;
      function dbliCreateOwnedList : TffDBList; virtual;
      procedure dbliDBItemAdded(aItem : TffDBListItem); virtual;
      procedure dbliDBItemDeleted(aItem : TffDBListItem); virtual;
      procedure dbliNotifyDBOwnerChanged; virtual;
      procedure dbliDBOwnerChanged; virtual;
      function dbliFindDBOwner(const aName : string) : TffDBListItem; virtual;
      procedure dbliFreeTemporaryDependents;                           {!!.01}
      procedure dbliLoaded; virtual;
      procedure dbliMustBeClosedError; virtual;
      procedure dbliMustBeOpenError; virtual;
      procedure dbliOpenPrim; virtual;
      function dbliResolveDBOwner(const aName : string) : TffDBListItem;
      procedure dbliSwitchOwnerTo(const aDBOwner : TffDBListItem);

      property Active : Boolean
         read dbliActive
         write dbliSetActive
         default False;
      property Connected : Boolean
         read dbliActive
         write dbliSetActive
         default False;
      property DBName : string
         read dbliDBName
         write dbliSetDBName;
      property DBOwner : TffDBListItem
         read dbliGetDBOwner
         write dbliSetDBOwner;
      property DBOwnerName : string
         read dbliGetDBOwnerName
         write dbliSetDBOwnerName;
      property FixingFromStream : Boolean
         read dbliFixing
         write dbliFixing;
      property LoadPriority : Integer
         read dbliLoadPriority;
      property LoadingFromStream : Boolean
         read dbliLoading
         write dbliLoading;
      property NeedsNoOwner : Boolean
         read dbliNeedsNoOwner;
      property OwnedDBItems : TffDBList
         read dbliOwnedDBItems;
      property Temporary : Boolean                                     {!!.01}
         read dbliTemporary write dbliTemporary;                       {!!.01}
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      procedure Loaded; override;

      procedure Open;
      procedure CheckActive;
      procedure CheckInactive(const aCanClose : Boolean);
      procedure Close;
      procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                 const AData : TffWord32); override;
      procedure ForceClosed;

      property IsOwned : Boolean
         read dbliGetOwned;
      property LoadActiveFailed : Boolean
         read dbliFailedActive;
  end;

    { All list management was moved to TffComponent after documentation was
      released to the printers. This class does not store items anymore,
      instead it's methods reference the dependent list in TffComponent. This
      required the addition of a owner field. Owner references the item
      controlling a collection of other items. For instance, if the list
      belonged to a TffBaseClient, then this class would control TffSession
      components.}
  TffDBList = class(TffObject)
    protected {private}
      dblOwner : TffDBListItem; {controller of this list}
    protected
      function dblGetCount : Integer;
      function dblGetItem(aInx : Integer) : TffDBListItem;
      procedure dblFreeItem(aItem : TffDBListItem); virtual;
      procedure dblFreeUnownedItems;
    public
      constructor Create(aOwner : TffDBListItem);
      destructor Destroy; override;

      function FindItem(const aName : string; var aItem : TffDBListItem) : Boolean;
      procedure GetItem(const aName : string; var aItem : TffDBListItem);
      procedure GetItemNames(aList : TStrings);
      function IndexOfItem(aItem : TffDBListItem) : Integer;

      property Count : Integer
         read dblGetCount;
      property Items[aInx : Integer] : TffDBListItem
         read dblGetItem; default;
    end;

  TffDBStandaloneList = class
    protected {private}
      dblList : TffThreadList;
    protected
      function dblGetCount : integer;
      function dblGetItem(aInx : integer) : TffDBListItem;

      procedure dblCloseAllItems;
      procedure dblFreeItem(aItem : TffDBListItem); virtual;
      procedure dblFreeUnownedItems;
    public
      constructor Create;
      destructor Destroy; override;

      procedure AddItem(aItem : TffDBListItem);
      procedure DeleteItem(aItem : TffDBListItem);
      function FindItem(const aName : string; var aItem : TffDBListItem) : boolean;
      procedure GetItem(const aName : string; var aItem : TffDBListItem);
      procedure GetItemNames(aList : TStrings);
      function IndexOfItem(aItem : TffDBListItem) : integer;

      procedure BeginRead;                                             {!!.02}
      procedure BeginWrite;                                            {!!.02}
      procedure EndRead;                                               {!!.02}
      procedure EndWrite;                                              {!!.02}

      property Count : integer read dblGetCount;
      property Items[aInx : integer] : TffDBListItem read dblGetItem; default;
    end;


{---Helper routines---}
procedure Check(const aStatus : TffResult);
procedure RaiseFFErrorCode(const aErrorCode : TffResult);
procedure RaiseFFErrorMsg(const aMsg : string);
procedure RaiseFFErrorObj(aObj : TComponent; const aErrorCode : TffResult);
procedure RaiseFFErrorObjFmt(aObj : TComponent; const aErrorCode : TffResult;
                             args: array of const);
function IsPath(const Value : string) : Boolean;

{---Internal helper routines---}
procedure AddToFixupList(aItem : TffDBListItem);
procedure ApplyFixupList;

implementation

{$R ffdscnst.res}

uses
  dialogs,
  sysutils,
  forms,
  ffconst,
  ffllexcp,
  ffnetmsg;                                                            {!!.06}

{===Fixup list helper code===========================================}
{Notes: this fixup list is to ensure that components that depend on
        others being fully loaded from the DFM file first, are
        completely initialized only after the components they depend
        on are initialized.
        The properties whose values are deferred at load time are the
        DBOwner name and the Active properties. For example, a
        database component which has a session name for a session
        component that hasn't been completely loaded yet cannot itself
        be loaded properly.
        The fixup list ensures that components with a high load
        priority (1) are fully loaded before those with a lower load
        priority (4).}
var
  DBItemFixupList : TList;

{--------}
procedure CreateFixupList;
begin
  DBItemFixupList := TList.Create;
end;
{--------}
procedure DestroyFixupList;
begin
  if (DBItemFixupList <> nil) then begin
    DBItemFixupList.Destroy;
    DBItemFixupList := nil;
  end;
end;
{--------}
procedure AddToFixupList(aItem : TffDBListItem);
begin
  if (DBItemFixupList = nil) then
    CreateFixupList;
  if (DBItemFixupList.IndexOf(aItem) = -1) then
    DBItemFixupList.Add(aItem);
end;
{--------}
procedure ApplyFixupList;
var
  LoadPty : Integer;
  Inx     : Integer;
  Item    : TffDBListItem;
begin
  if (DBItemFixupList <> nil) then begin
    for LoadPty := 1 to 4 do begin
      for Inx := pred(DBItemFixupList.Count) downto 0 do begin
        Item := TffDBListItem(DBItemFixupList[Inx]);
        if (Item.LoadPriority = LoadPty) then begin
          Item.LoadingFromStream := false;
          Item.FixingFromStream := true;
          Item.dbliLoaded;
          Item.FixingFromStream := false;
          DBItemFixupList.Delete(Inx);
        end;
      end;
    end;
    if (DBItemFixupList.Count = 0) then
      DestroyFixupList;
  end;
end;
{====================================================================}


{===Interfaced helper routines=======================================}
procedure Check(const aStatus : TffResult);
begin
  if aStatus <> 0 then
    RaiseFFErrorCode(aStatus);
end;
{--------}
procedure RaiseFFErrorCode(const aErrorCode : TffResult);
begin
  raise EffDatabaseError.CreateViaCode(aErrorCode, False);
end;
{--------}
procedure RaiseFFErrorMsg(const aMsg : string);
begin
  raise EffDatabaseError.Create(aMsg);
end;
{--------}
procedure RaiseFFErrorObj(aObj : TComponent; const aErrorCode : TffResult);
begin
  raise EffDatabaseError.CreateWithObj(aObj, aErrorCode,
                                       ffStrResDataSet[aErrorCode]);
end;
{--------}
procedure RaiseFFErrorObjFmt(aObj : TComponent; const aErrorCode : TffResult;
                             args: array of const);
begin
  raise EffDatabaseError.CreateWithObjFmt(aObj, aErrorCode, args);
end;
{--------}
function IsPath(const Value : string) : Boolean;
begin
  Result := (Pos(':', Value) <> 0 ) or
            (Pos('\', Value) <> 0 ) or                                 {!!.05}
            (Value = '.') or                                           {!!.05}
            (Value = '..');                                            {!!.05}
end;
{====================================================================}


{===EffDatabaseError=================================================}
constructor EffDatabaseError.Create(const aMsg : string);
begin
  deErrorCode := 0;
  inherited CreateFmt(ffStrResDataSet[ffdse_NoErrorCode], [aMsg]);
end;
{--------}
constructor EffDatabaseError.CreateViaCode(aErrorCode : TffResult; aDummy : Boolean);
var
  Msg : string;
begin
  deErrorCode := aErrorCode;
  Msg := deGetErrorString;
  inherited CreateFmt(ffStrResDataSet[ffdse_HasErrorCode], [Msg, aErrorCode, aErrorCode]);
end;
{Begin !!.06}
{--------}
constructor EffDatabaseError.CreateViaCodeFmt(const aErrorCode : TffResult;
                                              const args : array of const;
                                              const aDummy : boolean);
var
  Msg : string;
begin
  deErrorCode := aErrorCode;
  Msg := deGetErrorString;
  inherited Create(Format(Msg, args));
end;
{End !!.06}
{--------}
constructor EffDatabaseError.CreateWithObj(aObj : TComponent;
                                     const aErrorCode : TffResult;
                                     const aMsg : string);
var
  ObjName : string;
begin
  deErrorCode := aErrorCode;
  if (aObj = nil) then
    ObjName := ffStrResDataSet[ffdse_NilPointer]
  else begin
    ObjName := aObj.Name;
    if (ObjName = '') then
      ObjName := Format(ffStrResDataSet[ffdse_UnnamedInst], [aObj.ClassName]);
  end;
  inherited CreateFmt(ffStrResDataSet[ffdse_InstNoCode], [ObjName, aMsg]);
end;
{--------}
constructor EffDatabaseError.CreateWithObjFmt(aObj : TComponent;
                                        const aErrorCode : TffResult;
                                        const args : array of const); {!!.11}
var
  Msg : string;
  ObjName : string;
begin
  deErrorCode := aErrorCode;
  Msg := format(deGetErrorString, args);

  if (aObj = nil) then
    ObjName := ffStrResDataSet[ffdse_NilPointer]
  else begin
    ObjName := aObj.Name;
    if (ObjName = '') then
      ObjName := Format(ffStrResDataSet[ffdse_UnnamedInst], [aObj.ClassName]);
  end;

  inherited CreateFmt(ffStrResDataSet[ffdse_InstCode],
                      [ObjName, Msg, aErrorCode, aErrorCode]);
end;
{--------}
function EffDatabaseError.deGetErrorString : string;
var
  PC : array [0..127] of char;
begin
  if (deErrorCode >= ffDSCNSTLow) and (deErrorCode <= ffDSCNSTHigh) then
    ffStrResDataSet.GetASCIIZ(deErrorCode, PC, sizeOf(DBIMSG))
  else if (deErrorCode >= ffLLCNSTLow) and (deErrorCode <= ffLLCNSTHigh) then
    ffStrResGeneral.GetASCIIZ(deErrorCode, PC, sizeOf(DBIMSG))
  else if (deErrorCode >= ffCLCNSTLow) and (deErrorCode <= ffCLCNSTHigh) then {!!.06}
    ffStrResClient.GetASCIIZ(deErrorCode, PC, SizeOf(DBIMSG))                 {!!.06}
  else
    GetErrorStringPrim(deErrorCode, PC);
  Result := StrPas(PC);
end;
{====================================================================}


{===TffDBList========================================================}
constructor TffDBList.Create(aOwner : TffDBListItem);
begin
  dblOwner := aOwner;
end;
{--------}
destructor TffDBList.Destroy;
begin
  dblOwner.FFNotifyDependents(ffn_Destroy);

  dblOwner := nil;

  inherited Destroy;
end;
{--------}
procedure TffDBList.dblFreeItem(aItem : TffDBListItem);
begin
  aItem.Free;
end;
{--------}
procedure TffDBList.dblFreeUnownedItems;
var
  Idx    : Integer;
begin
  if Assigned(dblOwner.fcDependentList) then
{Begin !!.11}
    with dblOwner do begin
      fcLock.Lock;
      try
        for Idx := Pred(fcDependentList.Count) downto 0 do
          if TObject(fcDependentList[Idx]) is TffDBListItem then
            with TffDBListItem(fcDependentList[Idx]) do
              if IsOwned then
                DBOwnerName := ''
              else
                dblFreeItem(TffDBListItem(fcDependentList[Idx]));
      finally
        fcLock.Unlock;
      end;
    end;  { with }
{End !!.11}
end;
{--------}
function TffDBList.dblGetCount : Integer;
begin
  with dblOwner do
{Begin !!.11}
    if Assigned(fcDependentList) then begin
      fcLock.Lock;
      try
        Result := fcDependentList.Count;
      finally
        fcLock.Unlock;
      end;
    end
{End !!.11}
    else
      Result := 0;
end;
{--------}
function TffDBList.dblGetItem(aInx : Integer): TffDBListItem;
begin
  Assert(aInx > -1);
  Assert(aInx < Count, Format('%d not < %d', [aInx, Count]));
  with dblOwner do
{Begin !!.11}
    if Assigned(fcDependentList) then begin
      fcLock.Lock;
      try
        Result := TffDBListItem(fcDependentList.Items[aInx].Key^);
      finally
        fcLock.Unlock;
      end;
    end
{End !!.11}
    else
      Result := nil;
end;
{--------}
function TffDBList.FindItem(const aName: string; var aItem: TffDBListItem): Boolean;
var
  Inx    : Integer;
  DBItem : TffDBListItem;
begin
  aItem := nil;
  Result := False;
  if aName <> '' then
    with dblOwner do
{Begin !!.11}
      if Assigned(fcDependentList) then begin
        fcLock.Lock;
        try
          with fcDependentList do
            for Inx := Pred(Count) downto 0 do begin
              DBItem := TffDBListItem(Items[Inx].Key^);
              if (FFAnsiCompareText(DBItem.DBName, aName) = 0) then begin {!!.07}
                aItem := DBItem;
                Result := true;
                Exit;
              end;
            end;
        finally
          fcLock.Unlock;
        end;
      end
{End !!.11}
      else
        Result := False;
end;
{--------}
procedure TffDBList.GetItem(const aName: string; var aItem: TffDBListItem);
begin
  if aName = '' then
    aItem := nil
  else
    if not FindItem(aName, aItem) then
      RaiseFFErrorMsg(ffStrResDataSet[ffdse_MissingItem]);
end;
{--------}
procedure TffDBList.GetItemNames(aList : TStrings);
var
  Inx  : Integer;
  Item : TffDBListItem;
begin
  Assert(Assigned(aList));
  with dblOwner do
{Begin !!.11}
    if Assigned(fcDependentList) then begin
      fcLock.Lock;
      try
        with fcDependentList do begin
          aList.BeginUpdate;
          try
            for Inx := Pred(Count) downto 0 do begin
              Item := TffDBListItem(Items[Inx].Key^);
              if (Item.DBName <> '') then
                aList.Add(Item.DBName);
            end;
          finally
            aList.EndUpdate;
          end;
        end;
      finally
        fcLock.Unlock;
      end;
    end;
{End !!.11}
end;
{--------}
function TffDBList.IndexOfItem(aItem : TffDBListItem) : Integer;
begin
  with dblOwner do
{Begin !!.11}
    if Assigned(fcDependentList) then begin
      fcLock.Lock;
      try
        Result := IndexofItem(@aItem);
      finally
        fcLock.Unlock;
      end;
    end
{End !!.11}
    else
      Result := -1;
end;
{====================================================================}


{===TffDBListItem====================================================}
constructor TffDBListItem.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  dbliOwnedDBItems := dbliCreateOwnedList;
end;
{--------}
destructor TffDBListItem.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);

  dbliSwitchOwnerTo(nil);

  dbliOwnedDBItems.Free;
  dbliOwnedDBItems := nil;

  inherited Destroy;
end;
{--------}
procedure TffDBListItem.CheckActive;
begin
  if not Active then
    dbliMustBeOpenError;
end;
{--------}
procedure TffDBListItem.CheckInactive(const aCanClose : Boolean);

begin
  if Active then
    if aCanClose then
      Close
    else
      dbliMustBeClosedError;
end;
{--------}
procedure TffDBListItem.Close;
begin
  Active := False;
end;
{--------}
procedure TffDBListItem.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                         const AData : TffWord32);
begin
  if (dbliDBOwner = AFrom) then
    case AOp of
      ffn_Destroy,
      ffn_Remove :
        begin
          Close;
          dbliDBOwner := nil;
        end;
      ffn_Deactivate :
        begin
          Close;
        end;
      ffn_OwnerChanged :
        begin
          dbliDBOwnerChanged;
          DBOwnerName := TffDBListItem(AFrom).dbliDBName;
        end;
    end;
end;
{--------}
procedure TffDBListItem.dbliClosePrim;
begin
  FFNotifyDependents(ffn_Deactivate);
end;
{--------}
function TffDBListItem.dbliCreateOwnedList : TffDBList;
begin
  Result := TffDBList.Create(Self);
end;
{--------}
procedure TffDBListItem.dbliDBItemAdded(aItem : TffDBListItem);
begin
  {do nothing}
end;
{--------}
procedure TffDBListItem.dbliDBItemDeleted(aItem : TffDBListItem);
begin
  {do nothing}
end;
{--------}
procedure TffDBListItem.dbliNotifyDBOwnerChanged;
begin
  FFNotifyDependents(ffn_OwnerChanged);
end;
{--------}
procedure TffDBListItem.dbliDBOwnerChanged;
begin
  { do nothing }
end;
{--------}
function TffDBListItem.dbliFindDBOwner(const aName : string) : TffDBListItem;
begin
  {at this level we have no hope of identifying a DB owner}
  Result := nil;
end;
{Begin !!.01}
{--------}
procedure TffDBListItem.dbliFreeTemporaryDependents;
var
  aComp : TffDBListItem;
  aList : TffPointerList;
  Idx,Idx2 : Integer;                                                  {!!.02}
begin
  { Note: Removal of items from dependency list must be separated from
    deactivation of those items otherwise we get a list deadlock. }
  if Assigned(fcDependentList) then begin

    aList := nil;

    { Stage 1: Look for temporary items. }
{Begin !!.11}
    fcLock.Lock;
    try
      for Idx := Pred(fcDependentList.Count) downto 0 do begin
        aComp := TffDBListItem(TffIntListItem(fcDependentList[Idx]).KeyAsInt);
        if aComp.Temporary then begin
          if aList = nil then
            aList := TffPointerList.Create;
          aList.Append(pointer(Idx));
        end;
      end;  { for }
    finally
      fcLock.Unlock;
    end;
{End !!.11}

    { Stage 2: Tell the temporary items to close. Must do this without locking
      the dependency list otherwise we get a deadlock. }
    if aList <> nil then begin
      for Idx := 0 to pred(aList.Count) do begin
        Idx2 := Longint(aList[Idx]);                                   {!!.02}
        aComp := TffDBListItem(TffIntListItem(fcDependentList[Idx2]).KeyAsInt);  {!!.02}
        aComp.Active := False;
      end;

      { Stage 3: Remove the temporary items from the dependency list. }
{Begin !!.11}
      fcLock.Lock;
      try
        for Idx := 0 to pred(aList.Count) do begin
          Idx2 := Longint(aList[Idx]);                               {!!.02}
          aComp := TffDBListItem(TffIntListItem(fcDependentList[Idx2]).KeyAsInt);  {!!.02}
          fcDependentList.DeleteAt(Idx2);                            {!!.02}
          aComp.Free;
        end;
      finally
        fcLock.Unlock;
      end;
{End !!.11}
      aList.Free;
    end; { if aList <> nil }
  end;

end;
{End !!.01}
{--------}
function TffDBListItem.dbliGetDBOwner : TffDBListItem;
begin
  if (dbliDBOwner = nil) then
    DBOwner := dbliFindDBOwner(dbliDBOwnerName);
  Result := dbliDBOwner;
end;
{--------}
function TffDBListItem.dbliGetDBOwnerName : string;
begin
  if (dbliDBOwner <> nil) then begin
    dbliDBOwnerName := dbliDBOwner.DBName;
    Result := dbliDBOwnerName;
  end else begin
    DBOwner := dbliFindDBOwner(dbliDBOwnerName);
    if (dbliDBOwner = nil) then
      Result := dbliDBOwnerName
    else {DB owner exists} begin
      dbliDBOwnerName := dbliDBOwner.DBName;
      Result := dbliDBOwnerName;
    end;
  end;
end;
{--------}
function TffDBListItem.dbliGetOwned : Boolean;
begin
  Result := Assigned(Owner);
end;
{--------}
procedure TffDBListItem.dbliLoaded;
begin
  try
    if dbliMakeActive then begin
      {if we need a DB owner, resolve our DB owner name to an object}
      if not NeedsNoOwner then
        DBOwner := dbliResolveDBOwner(dbliDBOwnerName);
      {if we don't need a DB owner or our DB owner has managed to
       become active, make ourselves active}
      if NeedsNoOwner or not (DBOwner.LoadActiveFailed) then begin
        dbliFailedActive := true;
        Active := true;
        dbliMakeActive := false;
        dbliFailedActive := false;
      end;
    end else
      if (dbliDBOwnerName <> '') then
        dbliGetDBOwner;
  except
    if (csDesigning in ComponentState) then
      Application.HandleException(Self)
    else
      raise;
  end;{try..except}
end;
{--------}
procedure TffDBListItem.dbliMustBeClosedError;
begin
  RaiseFFErrorObj(Self, ffdse_MustBeClosed);
end;
{--------}
procedure TffDBListItem.dbliMustBeOpenError;
begin
  RaiseFFErrorObj(Self, ffdse_MustBeOpen);
end;
{--------}
procedure TffDBListItem.dbliOpenPrim;
begin
  {do nothing at this level}
end;
{--------}
function TffDBListItem.dbliResolveDBOwner(const aName : string) : TffDBListItem;
begin
  Result := dbliFindDBOwner(aName);
  if (Result = nil) then
    if not NeedsNoOwner then
      RaiseFFErrorObjFmt(Self, ffdse_MissingOwner, [Self.ClassName, Self.DBName]);
end;
{--------}
procedure TffDBListItem.dbliSetActive(const aValue: Boolean);
begin
  if aValue <> dbliActive then
    if (csReading in ComponentState) or LoadingFromStream then begin
      if aValue then
        dbliMakeActive := true;
      AddToFixupList(Self);
    end else begin
      {if we're making ourselves active...}
      if aValue then begin
        {if we haven't actually become active yet...}
        if not dbliActive then begin
          {we need a name}
          if (DBName = '') then
            RaiseFFErrorObjFmt(Self, ffdse_NeedsName, [dbliReqPropName]);
          {if we need a DB owner...}
          if not NeedsNoOwner then begin
            {make sure we have a DB owner name}
            if (DBOwnerName = '') then
              RaiseFFErrorObj(Self, ffdse_NeedsOwnerName);
            {make sure we have a DB owner object}
            if (dbliDBOwner = nil) then
              DBOwner := dbliResolveDBOwner(dbliDBOwnerName);
            {make sure our DB owner is open}
            if not DBOwner.Active then
              DBOwner.Active := true;
          end;
          {now we open ourselves}
          dbliOpenPrim;
        end;
        dbliActive := True;
      end else {closing} begin
        dbliClosePrim;
        dbliActive := False;
      end;
    end;
end;
{--------}
procedure TffDBListItem.dbliSetDBName(const aName: string);
begin
  CheckInactive(True);
  dbliDBName := aName;
end;
{--------}
procedure TffDBListItem.dbliSetDBOwner(const aDBOwner : TffDBListItem);
begin
  if (aDBOwner = nil) and (dbliDBOwner = nil) then
    Exit;
  CheckInactive(True);
  dbliSwitchOwnerTo(aDBOwner);
  dbliNotifyDBOwnerChanged;
end;
{--------}
procedure TffDBListItem.dbliSetDBOwnerName(const aName: string);
begin
  if (csReading in ComponentState) or LoadingFromStream then begin
    dbliDBOwnerName := aName;
    AddToFixupList(Self);
  end else
    if (FFAnsiCompareText(dbliDBOwnerName, aName) <> 0) then begin    {!!.07}
      CheckInactive(true);
      {set our DB owner to nil}
      dbliSwitchOwnerTo(nil);
      {save our new DB owner name}
      dbliDBOwnerName := aName;
      dbliNotifyDBOwnerChanged;
    end;
end;
{--------}
procedure TffDBListItem.dbliSwitchOwnerTo(const aDBOwner : TffDBListItem);
begin
  if (dbliDBOwner <> nil) then begin
    dbliDBOwner.FFRemoveDependent(Self);
  end;
  dbliDBOwner := aDBOwner;
  if (dbliDBOwner = nil) then
    dbliDBOwnerName := ''
  else begin
    dbliDBOwner.FFAddDependent(Self);
    dbliDBOwnerName := dbliDBOwner.DBName;
  end;
end;
{--------}
procedure TffDBListItem.ForceClosed;
begin
  Close;
end;
{--------}
procedure TffDBListItem.Loaded;
begin
  inherited Loaded;
  ApplyFixupList;
  LoadingFromStream := False;
end;
{--------}
procedure TffDBListItem.Open;
begin
  Active := True;
end;
{====================================================================}


{===TffDBStandaloneList========================================================}
constructor TffDBStandaloneList.Create;
begin
  inherited Create;
  dblList := TffThreadList.Create;
end;
{--------}
destructor TffDBStandaloneList.Destroy;
begin
  if Assigned(dblList) then
    with dblList.BeginWrite do
      try
        dblCloseAllItems;
      finally
        EndWrite;
      end;

  dblList.Free;
  dblList := nil;

  inherited Destroy;
end;
{--------}
procedure TffDBStandaloneList.AddItem(aItem: TffDBListItem);
begin
  Assert(Assigned(dblList));
  with dblList.BeginWrite do
    try
      Insert(TffIntListItem.Create(Longint(aItem)));
    finally
      EndWrite;
    end;
end;
{--------}
procedure TffDBStandaloneList.dblCloseAllItems;
var
  Inx  : integer;
  Item : TffDBListItem;
begin
  for Inx := pred(dblList.Count) downto 0 do begin
    Item := Items[Inx];
    {note: item opens are reference counted, so we need to force the
           item closed}
    Item.Close;
  end;
end;
{--------}
procedure TffDBStandaloneList.dblFreeItem(aItem : TffDBListItem);
begin
  aItem.Free;
end;
{--------}
procedure TffDBStandaloneList.dblFreeUnownedItems;
var
  Inx    : integer;
  DBItem : TffDBListItem;
begin
  for Inx := pred(dblList.Count) downto 0 do begin
    DBItem := Items[Inx];
    if DBItem.IsOwned then
      DBItem.DBOwnerName := ''
    else
      dblFreeItem(DBItem);
  end;
end;
{--------}
function TffDBStandaloneList.dblGetCount: integer;
begin
  with dblList.BeginRead do
    try
      Result := Count;
    finally
      EndRead;
    end;
end;
{--------}
function TffDBStandaloneList.dblGetItem(aInx: integer): TffDBListItem;
begin
  with dblList.BeginRead do
    try
      Result := TffDBListItem(dblList[aInx].Key^);
    finally
      EndRead;
    end;
end;
{--------}
procedure TffDBStandaloneList.DeleteItem(aItem: TffDBListItem);
var
  Inx : integer;
begin
  with dblList.BeginWrite do
    try
      Inx := dblList.Index(Longint(aItem));
      if (Inx <> -1) then
        dblList.Delete(Longint(aItem));
    finally
      EndWrite;
    end;
end;
{--------}
function TffDBStandaloneList.FindItem(const aName: string; var aItem: TffDBListItem): boolean;
var
  Inx    : integer;
  DBItem : TffDBListItem;
begin
  with dblList.BeginRead do
    try
      for Inx := Pred(Count) downto 0 do begin
        DBItem := TffDBListItem(Items[Inx].Key^);
        if (FFAnsiCompareText(DBItem.DBName, aName) = 0) then begin   {!!.07}
          aItem := DBItem;
          Result := true;
          Exit;
        end;
      end;
      aItem := nil;
      Result := false;
    finally
      EndRead;
    end;
end;
{--------}
procedure TffDBStandaloneList.GetItem(const aName: string; var aItem: TffDBListItem);
begin
  with dblList.BeginRead do
    try
      if not FindItem(aName, aItem) then
        RaiseFFErrorMsg(ffStrResDataSet[ffdse_MissingItem]);
    finally
      EndRead;
    end;
end;
{--------}
procedure TffDBStandaloneList.GetItemNames(aList: TStrings);
var
  Inx : integer;
  Item: TffDBListItem;
begin
  with dblList.BeginRead do
    try
      aList.BeginUpdate;
      try
        for Inx := pred(dblList.Count) downto 0 do begin
          Item := TffDBListItem(Items[Inx].Key^);
          if (Item.DBName <> '') then
            aList.Add(Item.DBName);
        end;
      finally
        aList.EndUpdate;
      end;{try..finally}
    finally
      EndRead;
    end;
end;
{--------}
function TffDBStandaloneList.IndexOfItem(aItem : TffDBListItem) : integer;
begin
  with dblList.BeginRead do
    try
      Result := IndexOfItem(@aItem)
    finally
      EndRead;
    end;
end;
{Begin !!.02}
{--------}
procedure TffDBStandaloneList.BeginRead;
begin
  dblList.BeginRead;
end;
{--------}
procedure TffDBStandaloneList.BeginWrite;
begin
  dblList.BeginWrite;
end;
{--------}
procedure TffDBStandaloneList.EndRead;
begin
  dblList.EndRead;
end;
{--------}
procedure TffDBStandaloneList.EndWrite;
begin
  dblList.EndWrite;
end;
{End !!.02}
{====================================================================}

procedure FinalizeUnit;
begin
  ffStrResDataSet.Free;
end;

procedure InitializeUnit;
begin
  ffStrResDataSet := nil;
  ffStrResDataSet := TffStringResource.Create(hInstance, 'FF_DATASET_ERROR_STRINGS');
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.
