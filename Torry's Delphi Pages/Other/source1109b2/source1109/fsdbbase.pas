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

{$I fsdefine.inc}

Unit fsdbbase;

Interface

Uses
  Classes,
  db,
  fsclbase, {!!.06}
  fssrbde,
  fsllbase,
  fssrmgr;

{$I fsdscnst.inc}

Var
  fsStrResDataSet: TfsStringResource;

Type
  EfsDatabaseError = Class(EDatabaseError)
  Protected {private}
    deErrorCode: TffResult;
  Protected
    Function deGetErrorString: String;
  Public
    Constructor Create(Const aMsg: String);
    Constructor CreateViaCode(aErrorCode: TffResult; aDummy: Boolean);
    Constructor CreateViaCodeFmt(Const aErrorCode: TffResult; {!!.06}
      Const args: Array Of Const; {!!.06}
      Const aDummy: Boolean); {!!.06}
    Constructor CreateWithObj(aObj: TComponent;
      Const aErrorCode: TffResult;
      Const aMsg: String);
    Constructor CreateWithObjFmt(aObj: TComponent; Const aErrorCode: TffResult;
      Const args: Array Of Const); {!!.11}
    Property ErrorCode: TffResult Read deErrorCode;
    Property ErrorString: String Read deGetErrorString;
  End;

Type
  TfsDBListItem = Class;
  TfsDBList = Class;

  TfsDBListItem = Class(TFSSpecComp)
  Protected {private}
    dbliActive: Boolean;
    dbliDBName: String;
    dbliDBOwner: TfsDBListItem;
    dbliDBOwnerName: String;
    dbliFailedActive: Boolean;
    dbliFixing: Boolean;
    dbliLoading: Boolean;
    dbliMakeActive: Boolean;
    dbliOwnedDBItems: TfsDBList;
    dbliReqPropName: String;
    dbliTemporary: Boolean; {!!.01}
    { The actual name of the required property corresponding to DBName. }
  Protected
    dbliLoadPriority: Integer; {*not* private, descendants set it}
    dbliNeedsNoOwner: Boolean; {*not* private, descendants set it}

    Function dbliGetDBOwner: TfsDBListItem;
    Function dbliGetDBOwnerName: String;
    Function dbliGetOwned: Boolean;
    Procedure dbliSetActive(Const aValue: Boolean);
    Procedure dbliSetDBName(Const aName: String);
    Procedure dbliSetDBOwner(Const aDBOwner: TfsDBListItem);
    Procedure dbliSetDBOwnerName(Const aName: String);

    Procedure dbliClosePrim; Virtual;
    Function dbliCreateOwnedList: TfsDBList; Virtual;
    Procedure dbliDBItemAdded(aItem: TfsDBListItem); Virtual;
    Procedure dbliDBItemDeleted(aItem: TfsDBListItem); Virtual;
    Procedure dbliNotifyDBOwnerChanged; Virtual;
    Procedure dbliDBOwnerChanged; Virtual;
    Function dbliFindDBOwner(Const aName: String): TfsDBListItem; Virtual;
    Procedure dbliFreeTemporaryDependents; {!!.01}
    Procedure dbliLoaded; Virtual;
    Procedure dbliMustBeClosedError; Virtual;
    Procedure dbliMustBeOpenError; Virtual;
    Procedure dbliOpenPrim; Virtual;
    Function dbliResolveDBOwner(Const aName: String): TfsDBListItem;
    Procedure dbliSwitchOwnerTo(Const aDBOwner: TfsDBListItem);

    Property Active: Boolean
      Read dbliActive
      Write dbliSetActive
      Default False;
    Property Connected: Boolean
      Read dbliActive
      Write dbliSetActive
      Default False;
    Property DBName: String
      Read dbliDBName
      Write dbliSetDBName;
    Property DBOwner: TfsDBListItem
      Read dbliGetDBOwner
      Write dbliSetDBOwner;
    Property DBOwnerName: String
      Read dbliGetDBOwnerName
      Write dbliSetDBOwnerName;
    Property FixingFromStream: Boolean
      Read dbliFixing
      Write dbliFixing;
    Property LoadPriority: Integer
      Read dbliLoadPriority;
    Property LoadingFromStream: Boolean
      Read dbliLoading
      Write dbliLoading;
    Property NeedsNoOwner: Boolean
      Read dbliNeedsNoOwner;
    Property OwnedDBItems: TfsDBList
      Read dbliOwnedDBItems;
    Property Temporary: Boolean {!!.01}
    Read dbliTemporary Write dbliTemporary; {!!.01}
  Public
    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Loaded; Override;

    Procedure Open;
    Procedure CheckActive;
    Procedure CheckInactive(Const aCanClose: Boolean);
    Procedure Close;
    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
      Const AData: TffWord32); Override;
    Procedure ForceClosed;

    Property IsOwned: Boolean
      Read dbliGetOwned;
    Property LoadActiveFailed: Boolean
      Read dbliFailedActive;
  End;

  { All list management was moved to TFSSpecComp after documentation was
    released to the printers. This class does not store items anymore,
    instead it's methods reference the dependent list in TFSSpecComp. This
    required the addition of a owner field. Owner references the item
    controlling a collection of other items. For instance, if the list
    belonged to a TffBaseClient, then this class would control TffSession
    components.}
  TfsDBList = Class(TFSSpecObject)
  Protected {private}
    dblOwner: TfsDBListItem; {controller of this list}
  Protected
    Function dblGetCount: Integer;
    Function dblGetItem(aInx: Integer): TfsDBListItem;
    Procedure dblFreeItem(aItem: TfsDBListItem); Virtual;
    Procedure dblFreeUnownedItems;
  Public
    Constructor Create(aOwner: TfsDBListItem);
    Destructor Destroy; Override;

    Function FindItem(Const aName: String; Var aItem: TfsDBListItem): Boolean;
    Procedure GetItem(Const aName: String; Var aItem: TfsDBListItem);
    Procedure GetItemNames(aList: TStrings);
    Function IndexOfItem(aItem: TfsDBListItem): Integer;

    Property Count: Integer
      Read dblGetCount;
    Property Items[aInx: Integer]: TfsDBListItem
    Read dblGetItem; Default;
  End;

  TfsDBStandaloneList = Class
  Protected {private}
    dblList: TFSSpecThreadList;
  Protected
    Function dblGetCount: Integer;
    Function dblGetItem(aInx: Integer): TfsDBListItem;

    Procedure dblCloseAllItems;
    Procedure dblFreeItem(aItem: TfsDBListItem); Virtual;
    Procedure dblFreeUnownedItems;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure AddItem(aItem: TfsDBListItem);
    Procedure DeleteItem(aItem: TfsDBListItem);
    Function FindItem(Const aName: String; Var aItem: TfsDBListItem): boolean;
    Procedure GetItem(Const aName: String; Var aItem: TfsDBListItem);
    Procedure GetItemNames(aList: TStrings);
    Function IndexOfItem(aItem: TfsDBListItem): Integer;

    Procedure BeginRead; {!!.02}
    Procedure BeginWrite; {!!.02}
    Procedure EndRead; {!!.02}
    Procedure EndWrite; {!!.02}

    Property Count: Integer Read dblGetCount;
    Property Items[aInx: Integer]: TfsDBListItem Read dblGetItem; Default;
  End;

  {---Helper routines---}
Procedure Check(Const aStatus: TffResult);
Procedure RaiseFSErrorCode(Const aErrorCode: TffResult);
Procedure RaiseFSErrorMsg(Const aMsg: String);
Procedure RaiseFSErrorObj(aObj: TComponent; Const aErrorCode: TffResult);
Procedure RaiseFSErrorObjFmt(aObj: TComponent; Const aErrorCode: TffResult;
  args: Array Of Const);
Function fsIsPath(Const Value: String): Boolean;

{---Internal helper routines---}
Procedure fsAddToFixupList(aItem: TfsDBListItem);
Procedure fsApplyFixupList;

Implementation

{$R fsdscnst.res}

Uses
  Dialogs,
  SysUtils,
  Forms,
  fsconst,
  fsllexcp,
  fsnetmsg; {!!.06}

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
Var
  DBItemFixupList: TList;

  {--------}

Procedure CreateFixupList;
Begin
  DBItemFixupList := TList.Create;
End;
{--------}

Procedure DestroyFixupList;
Begin
  If (DBItemFixupList <> Nil) Then
    Begin
      DBItemFixupList.Destroy;
      DBItemFixupList := Nil;
    End;
End;
{--------}

Procedure fsAddToFixupList(aItem: TfsDBListItem);
Begin
  If (DBItemFixupList = Nil) Then
    CreateFixupList;
  If (DBItemFixupList.IndexOf(aItem) = -1) Then
    DBItemFixupList.Add(aItem);
End;
{--------}

Procedure fsApplyFixupList;
Var
  LoadPty: Integer;
  Inx: Integer;
  Item: TfsDBListItem;
Begin
  If (DBItemFixupList <> Nil) Then
    Begin
      For LoadPty := 1 To 4 Do
        Begin
          For Inx := pred(DBItemFixupList.Count) Downto 0 Do
            Begin
              Item := TfsDBListItem(DBItemFixupList[Inx]);
              If (Item.LoadPriority = LoadPty) Then
                Begin
                  Item.LoadingFromStream := False;
                  Item.FixingFromStream := True;
                  Item.dbliLoaded;
                  Item.FixingFromStream := False;
                  DBItemFixupList.Delete(Inx);
                End;
            End;
        End;
      If (DBItemFixupList.Count = 0) Then
        DestroyFixupList;
    End;
End;
{====================================================================}

{===Interfaced helper routines=======================================}

Procedure Check(Const aStatus: TffResult);
Begin
  If aStatus <> 0 Then
    RaiseFSErrorCode(aStatus);
End;
{--------}

Procedure RaiseFSErrorCode(Const aErrorCode: TffResult);
Begin
  Raise EfsDatabaseError.CreateViaCode(aErrorCode, False);
End;
{--------}

Procedure RaiseFSErrorMsg(Const aMsg: String);
Begin
  Raise EfsDatabaseError.Create(aMsg);
End;
{--------}

Procedure RaiseFSErrorObj(aObj: TComponent; Const aErrorCode: TffResult);
Begin
  Raise EfsDatabaseError.CreateWithObj(aObj, aErrorCode,
    fsStrResDataSet[aErrorCode]);
End;
{--------}

Procedure RaiseFSErrorObjFmt(aObj: TComponent; Const aErrorCode: TffResult;
  args: Array Of Const);
Begin
  Raise EfsDatabaseError.CreateWithObjFmt(aObj, aErrorCode, args);
End;
{--------}

Function fsIsPath(Const Value: String): Boolean;
Begin
  Result := (Pos(':', Value) <> 0) Or
    (Pos('\', Value) <> 0) Or {!!.05}
  (Value = '.') Or {!!.05}
  (Value = '..'); {!!.05}
End;
{====================================================================}

{===EfsDatabaseError=================================================}

Constructor EfsDatabaseError.Create(Const aMsg: String);
Begin
  deErrorCode := 0;
  Inherited CreateFmt(fsStrResDataSet[fsdse_NoErrorCode], [aMsg]);
End;

{--------}

Constructor EfsDatabaseError.CreateViaCode(aErrorCode: TffResult; aDummy: Boolean);
Var
  Msg: String;
Begin
  If (aErrorCode >= 50000) And (aErrorCode <= 60000) Then
    Begin
      Case aErrorcode Of
        50000:
          Begin
            Msg := 'Cannot delete column being used in an Integrity Constraint!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50001:
          Begin
            Msg := 'Cannot update column being used in an Integrity Constraint!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50002:
          Begin
            Msg := 'Error execute script!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50003:
          Begin
            Msg := 'Error execute script: Triger BeforeInsert!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50004:
          Begin
            Msg := 'Error execute script: Triger AfterInsert!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50005:
          Begin
            Msg := 'Error execute script: Triger BeforeUpdate!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50006:
          Begin
            Msg := 'Error execute script: Triger AfterUpdate!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50007:
          Begin
            Msg := 'Error execute script: Triger BeforeDelete!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50008:
          Begin
            Msg := 'Error execute script: Triger AfterDelete!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50009:
          Begin
            Msg := 'Error execute script: Execute procedure!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50010:
          Begin
            Msg := 'Error: Bad value!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50011:
          Begin
            Msg := 'Error: User intervention!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50012:
          Begin
            Msg := 'Error delete cursor: Script execute!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50013:
          Begin
            Msg := 'Error delete : System intervention!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50014:
          Begin
            Msg := 'Error update : System intervention!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50015:
          Begin
            Msg := 'Error insert : System intervention!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50016:
          Begin
            Msg := 'Error Create : Declare input variable!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
        50017:
          Begin
            Msg := 'Error Create : Declare output variable!';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
         50100:
          Begin
            Msg := 'FSSQL: Invalid password given.';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
          50101:
          Begin
            Msg := 'FSSQL: Invalid ID database.';
            Inherited CreateFmt(Msg, [Msg, aErrorCode, aErrorCode]);
          End;
      End;
    End
  Else
    Begin
      deErrorCode := aErrorCode;
      Msg := deGetErrorString;
      Inherited CreateFmt(fsStrResDataSet[fsdse_HasErrorCode], [Msg, aErrorCode, aErrorCode]);
    End;
End;
{Begin !!.06}
{--------}

Constructor EfsDatabaseError.CreateViaCodeFmt(Const aErrorCode: TffResult;
  Const args: Array Of Const;
  Const aDummy: boolean);
Var
  Msg: String;
Begin
  deErrorCode := aErrorCode;
  Msg := deGetErrorString;
  Inherited Create(Format(Msg, args));
End;
{End !!.06}
{--------}

Constructor EfsDatabaseError.CreateWithObj(aObj: TComponent;
  Const aErrorCode: TffResult;
  Const aMsg: String);
Var
  ObjName: String;
Begin
  deErrorCode := aErrorCode;
  If (aObj = Nil) Then
    ObjName := fsStrResDataSet[fsdse_NilPointer]
  Else
    Begin
      ObjName := aObj.Name;
      If (ObjName = '') Then
        ObjName := Format(fsStrResDataSet[fsdse_UnnamedInst], [aObj.ClassName]);
    End;
  Inherited CreateFmt(fsStrResDataSet[fsdse_InstNoCode], [ObjName, aMsg]);
End;
{--------}

Constructor EfsDatabaseError.CreateWithObjFmt(aObj: TComponent;
  Const aErrorCode: TffResult;
  Const args: Array Of Const); {!!.11}
Var
  Msg: String;
  ObjName: String;
Begin
  deErrorCode := aErrorCode;
  Msg := format(deGetErrorString, args);

  If (aObj = Nil) Then
    ObjName := fsStrResDataSet[fsdse_NilPointer]
  Else
    Begin
      ObjName := aObj.Name;
      If (ObjName = '') Then
        ObjName := Format(fsStrResDataSet[fsdse_UnnamedInst], [aObj.ClassName]);
    End;

  Inherited CreateFmt(fsStrResDataSet[fsdse_InstCode],
    [ObjName, Msg, aErrorCode, aErrorCode]);
End;
{--------}

Function EfsDatabaseError.deGetErrorString: String;
Var
  PC: Array[0..127] Of char;
Begin
  If (deErrorCode >= fsDSCNSTLow) And (deErrorCode <= fsDSCNSTHigh) Then
    fsStrResDataSet.GetASCIIZ(deErrorCode, PC, sizeOf(DBIMSG))
  Else If (deErrorCode >= fsLLCNSTLow) And (deErrorCode <= fsLLCNSTHigh) Then
    fsStrResGeneral.GetASCIIZ(deErrorCode, PC, sizeOf(DBIMSG))
  Else If (deErrorCode >= fsCLCNSTLow) And (deErrorCode <= fsCLCNSTHigh) Then {!!.06}
    fsStrResClient.GetASCIIZ(deErrorCode, PC, SizeOf(DBIMSG)) {!!.06}
  Else
    GetErrorStringPrim(deErrorCode, PC);
  Result := StrPas(PC);
End;
{====================================================================}

{===TfsDBList========================================================}

Constructor TfsDBList.Create(aOwner: TfsDBListItem);
Begin
  dblOwner := aOwner;
End;
{--------}

Destructor TfsDBList.Destroy;
Begin
  dblOwner.FFNotifyDependents(ffn_Destroy);

  dblOwner := Nil;

  Inherited Destroy;
End;
{--------}

Procedure TfsDBList.dblFreeItem(aItem: TfsDBListItem);
Begin
  aItem.Free;
End;
{--------}

Procedure TfsDBList.dblFreeUnownedItems;
Var
  Idx: Integer;
Begin
  If Assigned(dblOwner.fcDependentList) Then
    {Begin !!.11}
    With dblOwner Do
      Begin
        fcLock.Lock;
        Try
          For Idx := Pred(fcDependentList.Count) Downto 0 Do
            If TObject(fcDependentList[Idx]) Is TfsDBListItem Then
              With TfsDBListItem(fcDependentList[Idx]) Do
                If IsOwned Then
                  DBOwnerName := ''
                Else
                  dblFreeItem(TfsDBListItem(fcDependentList[Idx]));
        Finally
          fcLock.Unlock;
        End;
      End; { with }
  {End !!.11}
End;
{--------}

Function TfsDBList.dblGetCount: Integer;
Begin
  With dblOwner Do
    {Begin !!.11}
    If Assigned(fcDependentList) Then
      Begin
        fcLock.Lock;
        Try
          Result := fcDependentList.Count;
        Finally
          fcLock.Unlock;
        End;
      End
        {End !!.11}
    Else
      Result := 0;
End;
{--------}

Function TfsDBList.dblGetItem(aInx: Integer): TfsDBListItem;
Begin
  Assert(aInx > -1);
  Assert(aInx < Count, Format('%d not < %d', [aInx, Count]));
  With dblOwner Do
    {Begin !!.11}
    If Assigned(fcDependentList) Then
      Begin
        fcLock.Lock;
        Try
          Result := TfsDBListItem(fcDependentList.Items[aInx].Key^);
        Finally
          fcLock.Unlock;
        End;
      End
        {End !!.11}
    Else
      Result := Nil;
End;
{--------}

Function TfsDBList.FindItem(Const aName: String; Var aItem: TfsDBListItem): Boolean;
Var
  Inx: Integer;
  DBItem: TfsDBListItem;
Begin
  aItem := Nil;
  Result := False;
  If aName <> '' Then
    With dblOwner Do
      {Begin !!.11}
      If Assigned(fcDependentList) Then
        Begin
          fcLock.Lock;
          Try
            With fcDependentList Do
              For Inx := Pred(Count) Downto 0 Do
                Begin
                  DBItem := TfsDBListItem(Items[Inx].Key^);
                  If (FFAnsiCompareText(DBItem.DBName, aName) = 0) Then
                    Begin {!!.07}
                      aItem := DBItem;
                      Result := True;
                      Exit;
                    End;
                End;
          Finally
            fcLock.Unlock;
          End;
        End
          {End !!.11}
      Else
        Result := False;
End;
{--------}

Procedure TfsDBList.GetItem(Const aName: String; Var aItem: TfsDBListItem);
Begin
  If aName = '' Then
    aItem := Nil
  Else If Not FindItem(aName, aItem) Then
    RaiseFSErrorMsg(fsStrResDataSet[fsdse_MissingItem]);
End;
{--------}

Procedure TfsDBList.GetItemNames(aList: TStrings);
Var
  Inx: Integer;
  Item: TfsDBListItem;
Begin
  Assert(Assigned(aList));
  With dblOwner Do
    {Begin !!.11}
    If Assigned(fcDependentList) Then
      Begin
        fcLock.Lock;
        Try
          With fcDependentList Do
            Begin
              aList.BeginUpdate;
              Try
                For Inx := Pred(Count) Downto 0 Do
                  Begin
                    Item := TfsDBListItem(Items[Inx].Key^);
                    If (Item.DBName <> '') Then
                      aList.Add(Item.DBName);
                  End;
              Finally
                aList.EndUpdate;
              End;
            End;
        Finally
          fcLock.Unlock;
        End;
      End;
  {End !!.11}
End;
{--------}

Function TfsDBList.IndexOfItem(aItem: TfsDBListItem): Integer;
Begin
  With dblOwner Do
    {Begin !!.11}
    If Assigned(fcDependentList) Then
      Begin
        fcLock.Lock;
        Try
          Result := IndexofItem(@aItem);
        Finally
          fcLock.Unlock;
        End;
      End
        {End !!.11}
    Else
      Result := -1;
End;
{====================================================================}

{===TfsDBListItem====================================================}

Constructor TfsDBListItem.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  dbliOwnedDBItems := dbliCreateOwnedList;
End;
{--------}

Destructor TfsDBListItem.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy);

  dbliSwitchOwnerTo(Nil);

  dbliOwnedDBItems.Free;
  dbliOwnedDBItems := Nil;

  Inherited Destroy;
End;
{--------}

Procedure TfsDBListItem.CheckActive;
Begin
  If Not Active Then
    dbliMustBeOpenError;
End;
{--------}

Procedure TfsDBListItem.CheckInactive(Const aCanClose: Boolean);

Begin
  If Active Then
    If aCanClose Then
      Close
    Else
      dbliMustBeClosedError;
End;
{--------}

Procedure TfsDBListItem.Close;
Begin
  Active := False;
End;
{--------}

Procedure TfsDBListItem.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const AData: TffWord32);
Begin
  If (dbliDBOwner = AFrom) Then
    Case AOp Of
      ffn_Destroy,
        ffn_Remove:
        Begin
          Close;
          dbliDBOwner := Nil;
        End;
      ffn_Deactivate:
        Begin
          Close;
        End;
      ffn_OwnerChanged:
        Begin
          dbliDBOwnerChanged;
          DBOwnerName := TfsDBListItem(AFrom).dbliDBName;
        End;
    End;
End;
{--------}

Procedure TfsDBListItem.dbliClosePrim;
Begin
  FFNotifyDependents(ffn_Deactivate);
End;
{--------}

Function TfsDBListItem.dbliCreateOwnedList: TfsDBList;
Begin
  Result := TfsDBList.Create(Self);
End;
{--------}

Procedure TfsDBListItem.dbliDBItemAdded(aItem: TfsDBListItem);
Begin
  {do nothing}
End;
{--------}

Procedure TfsDBListItem.dbliDBItemDeleted(aItem: TfsDBListItem);
Begin
  {do nothing}
End;
{--------}

Procedure TfsDBListItem.dbliNotifyDBOwnerChanged;
Begin
  FFNotifyDependents(ffn_OwnerChanged);
End;
{--------}

Procedure TfsDBListItem.dbliDBOwnerChanged;
Begin
  { do nothing }
End;
{--------}

Function TfsDBListItem.dbliFindDBOwner(Const aName: String): TfsDBListItem;
Begin
  {at this level we have no hope of identifying a DB owner}
  Result := Nil;
End;
{Begin !!.01}
{--------}

Procedure TfsDBListItem.dbliFreeTemporaryDependents;
Var
  aComp: TfsDBListItem;
  aList: TfsPointerList;
  Idx, Idx2: Integer; {!!.02}
Begin
  { Note: Removal of items from dependency list must be separated from
    deactivation of those items otherwise we get a list deadlock. }
  If Assigned(fcDependentList) Then
    Begin

      aList := Nil;

      { Stage 1: Look for temporary items. }
  {Begin !!.11}
      fcLock.Lock;
      Try
        For Idx := Pred(fcDependentList.Count) Downto 0 Do
          Begin
            aComp := TfsDBListItem(TfsIntListItem(fcDependentList[Idx]).KeyAsInt);
            If aComp.Temporary Then
              Begin
                If aList = Nil Then
                  aList := TfsPointerList.Create;
                aList.Append(pointer(Idx));
              End;
          End; { for }
      Finally
        fcLock.Unlock;
      End;
      {End !!.11}

          { Stage 2: Tell the temporary items to close. Must do this without locking
            the dependency list otherwise we get a deadlock. }
      If aList <> Nil Then
        Begin
          For Idx := 0 To pred(aList.Count) Do
            Begin
              Idx2 := Longint(aList[Idx]); {!!.02}
              aComp := TfsDBListItem(TfsIntListItem(fcDependentList[Idx2]).KeyAsInt); {!!.02}
              aComp.Active := False;
            End;

          { Stage 3: Remove the temporary items from the dependency list. }
    {Begin !!.11}
          fcLock.Lock;
          Try
            For Idx := 0 To pred(aList.Count) Do
              Begin
                Idx2 := Longint(aList[Idx]); {!!.02}
                aComp := TfsDBListItem(TfsIntListItem(fcDependentList[Idx2]).KeyAsInt); {!!.02}
                fcDependentList.DeleteAt(Idx2); {!!.02}
                aComp.Free;
              End;
          Finally
            fcLock.Unlock;
          End;
          {End !!.11}
          aList.Free;
        End; { if aList <> nil }
    End;

End;
{End !!.01}
{--------}

Function TfsDBListItem.dbliGetDBOwner: TfsDBListItem;
Begin
  If (dbliDBOwner = Nil) Then
    DBOwner := dbliFindDBOwner(dbliDBOwnerName);
  Result := dbliDBOwner;
End;
{--------}

Function TfsDBListItem.dbliGetDBOwnerName: String;
Begin
  If (dbliDBOwner <> Nil) Then
    Begin
      dbliDBOwnerName := dbliDBOwner.DBName;
      Result := dbliDBOwnerName;
    End
  Else
    Begin
      DBOwner := dbliFindDBOwner(dbliDBOwnerName);
      If (dbliDBOwner = Nil) Then
        Result := dbliDBOwnerName
      Else {DB owner exists}
        Begin
          dbliDBOwnerName := dbliDBOwner.DBName;
          Result := dbliDBOwnerName;
        End;
    End;
End;
{--------}

Function TfsDBListItem.dbliGetOwned: Boolean;
Begin
  Result := Assigned(Owner);
End;
{--------}

Procedure TfsDBListItem.dbliLoaded;
Begin
  Try
    If dbliMakeActive Then
      Begin
        {if we need a DB owner, resolve our DB owner name to an object}
        If Not NeedsNoOwner Then
          DBOwner := dbliResolveDBOwner(dbliDBOwnerName);
        {if we don't need a DB owner or our DB owner has managed to
         become active, make ourselves active}
        If NeedsNoOwner Or Not (DBOwner.LoadActiveFailed) Then
          Begin
            dbliFailedActive := True;
            Active := True;
            dbliMakeActive := False;
            dbliFailedActive := False;
          End;
      End
    Else If (dbliDBOwnerName <> '') Then
      dbliGetDBOwner;
  Except
    If (csDesigning In ComponentState) Then
      Application.HandleException(Self)
    Else
      Raise;
  End; {try..except}
End;
{--------}

Procedure TfsDBListItem.dbliMustBeClosedError;
Begin
  RaiseFSErrorObj(Self, fsdse_MustBeClosed);
End;
{--------}

Procedure TfsDBListItem.dbliMustBeOpenError;
Begin
  RaiseFSErrorObj(Self, fsdse_MustBeOpen);
End;
{--------}

Procedure TfsDBListItem.dbliOpenPrim;
Begin
  {do nothing at this level}
End;
{--------}

Function TfsDBListItem.dbliResolveDBOwner(Const aName: String): TfsDBListItem;
Begin
  Result := dbliFindDBOwner(aName);
  If (Result = Nil) Then
    If Not NeedsNoOwner Then
      RaiseFSErrorObjFmt(Self, fsdse_MissingOwner, [Self.ClassName, Self.DBName]);
End;
{--------}

Procedure TfsDBListItem.dbliSetActive(Const aValue: Boolean);
Begin
  If aValue <> dbliActive Then
    If (csReading In ComponentState) Or LoadingFromStream Then
      Begin
        If aValue Then
          dbliMakeActive := True;
        fsAddToFixupList(Self);
      End
    Else
      Begin
        {if we're making ourselves active...}
        If aValue Then
          Begin
            {if we haven't actually become active yet...}
            If Not dbliActive Then
              Begin
                {we need a name}
                If (DBName = '') Then
                  RaiseFSErrorObjFmt(Self, fsdse_NeedsName, [dbliReqPropName]);
                {if we need a DB owner...}
                If Not NeedsNoOwner Then
                  Begin
                    {make sure we have a DB owner name}
                    If (DBOwnerName = '') Then
                      RaiseFSErrorObj(Self, fsdse_NeedsOwnerName);
                    {make sure we have a DB owner object}
                    If (dbliDBOwner = Nil) Then
                      DBOwner := dbliResolveDBOwner(dbliDBOwnerName);
                    {make sure our DB owner is open}
                    If Not DBOwner.Active Then
                      DBOwner.Active := True;
                  End;
                {now we open ourselves}
                dbliOpenPrim;
              End;
            dbliActive := True;
          End
        Else {closing}
          Begin
            dbliClosePrim;
            dbliActive := False;
          End;
      End;
End;
{--------}

Procedure TfsDBListItem.dbliSetDBName(Const aName: String);
Begin
  CheckInactive(True);
  dbliDBName := aName;
End;
{--------}

Procedure TfsDBListItem.dbliSetDBOwner(Const aDBOwner: TfsDBListItem);
Begin
  If (aDBOwner = Nil) And (dbliDBOwner = Nil) Then
    Exit;
  CheckInactive(True);
  dbliSwitchOwnerTo(aDBOwner);
  dbliNotifyDBOwnerChanged;
End;
{--------}

Procedure TfsDBListItem.dbliSetDBOwnerName(Const aName: String);
Begin
  If (csReading In ComponentState) Or LoadingFromStream Then
    Begin
      dbliDBOwnerName := aName;
      fsAddToFixupList(Self);
    End
  Else If (FFAnsiCompareText(dbliDBOwnerName, aName) <> 0) Then
    Begin {!!.07}
      CheckInactive(True);
      {set our DB owner to nil}
      dbliSwitchOwnerTo(Nil);
      {save our new DB owner name}
      dbliDBOwnerName := aName;
      dbliNotifyDBOwnerChanged;
    End;
End;
{--------}

Procedure TfsDBListItem.dbliSwitchOwnerTo(Const aDBOwner: TfsDBListItem);
Begin
  If (dbliDBOwner <> Nil) Then
    Begin
      dbliDBOwner.FFRemoveDependent(Self);
    End;
  dbliDBOwner := aDBOwner;
  If (dbliDBOwner = Nil) Then
    dbliDBOwnerName := ''
  Else
    Begin
      dbliDBOwner.FFAddDependent(Self);
      dbliDBOwnerName := dbliDBOwner.DBName;
    End;
End;
{--------}

Procedure TfsDBListItem.ForceClosed;
Begin
  Close;
End;
{--------}

Procedure TfsDBListItem.Loaded;
Begin
  Inherited Loaded;
  fsApplyFixupList;
  LoadingFromStream := False;
End;
{--------}

Procedure TfsDBListItem.Open;
Begin
  Active := True;
End;
{====================================================================}

{===TfsDBStandaloneList========================================================}

Constructor TfsDBStandaloneList.Create;
Begin
  Inherited Create;
  dblList := TFSSpecThreadList.Create;
End;
{--------}

Destructor TfsDBStandaloneList.Destroy;
Begin
  If Assigned(dblList) Then
    With dblList.BeginWrite Do
      Try
        dblCloseAllItems;
      Finally
        EndWrite;
      End;

  dblList.Free;
  dblList := Nil;

  Inherited Destroy;
End;
{--------}

Procedure TfsDBStandaloneList.AddItem(aItem: TfsDBListItem);
Begin
  Assert(Assigned(dblList));
  With dblList.BeginWrite Do
    Try
      Insert(TfsIntListItem.Create(Longint(aItem)));
    Finally
      EndWrite;
    End;
End;
{--------}

Procedure TfsDBStandaloneList.dblCloseAllItems;
Var
  Inx: Integer;
  Item: TfsDBListItem;
Begin
  For Inx := pred(dblList.Count) Downto 0 Do
    Begin
      Item := Items[Inx];
      {note: item opens are reference counted, so we need to force the
             item closed}
      Item.Close;
    End;
End;
{--------}

Procedure TfsDBStandaloneList.dblFreeItem(aItem: TfsDBListItem);
Begin
  aItem.Free;
End;
{--------}

Procedure TfsDBStandaloneList.dblFreeUnownedItems;
Var
  Inx: Integer;
  DBItem: TfsDBListItem;
Begin
  For Inx := pred(dblList.Count) Downto 0 Do
    Begin
      DBItem := Items[Inx];
      If DBItem.IsOwned Then
        DBItem.DBOwnerName := ''
      Else
        dblFreeItem(DBItem);
    End;
End;
{--------}

Function TfsDBStandaloneList.dblGetCount: Integer;
Begin
  With dblList.BeginRead Do
    Try
      Result := Count;
    Finally
      EndRead;
    End;
End;
{--------}

Function TfsDBStandaloneList.dblGetItem(aInx: Integer): TfsDBListItem;
Begin
  With dblList.BeginRead Do
    Try
      Result := TfsDBListItem(dblList[aInx].Key^);
    Finally
      EndRead;
    End;
End;
{--------}

Procedure TfsDBStandaloneList.DeleteItem(aItem: TfsDBListItem);
Var
  Inx: Integer;
Begin
  With dblList.BeginWrite Do
    Try
      Inx := dblList.Index(Longint(aItem));
      If (Inx <> -1) Then
        dblList.Delete(Longint(aItem));
    Finally
      EndWrite;
    End;
End;
{--------}

Function TfsDBStandaloneList.FindItem(Const aName: String; Var aItem: TfsDBListItem): boolean;
Var
  Inx: Integer;
  DBItem: TfsDBListItem;
Begin
  With dblList.BeginRead Do
    Try
      For Inx := Pred(Count) Downto 0 Do
        Begin
          DBItem := TfsDBListItem(Items[Inx].Key^);
          If (FFAnsiCompareText(DBItem.DBName, aName) = 0) Then
            Begin {!!.07}
              aItem := DBItem;
              Result := True;
              Exit;
            End;
        End;
      aItem := Nil;
      Result := False;
    Finally
      EndRead;
    End;
End;
{--------}

Procedure TfsDBStandaloneList.GetItem(Const aName: String; Var aItem: TfsDBListItem);
Begin
  With dblList.BeginRead Do
    Try
      If Not FindItem(aName, aItem) Then
        RaiseFSErrorMsg(fsStrResDataSet[fsdse_MissingItem]);
    Finally
      EndRead;
    End;
End;
{--------}

Procedure TfsDBStandaloneList.GetItemNames(aList: TStrings);
Var
  Inx: Integer;
  Item: TfsDBListItem;
Begin
  With dblList.BeginRead Do
    Try
      aList.BeginUpdate;
      Try
        For Inx := pred(dblList.Count) Downto 0 Do
          Begin
            Item := TfsDBListItem(Items[Inx].Key^);
            If (Item.DBName <> '') Then
              aList.Add(Item.DBName);
          End;
      Finally
        aList.EndUpdate;
      End; {try..finally}
    Finally
      EndRead;
    End;
End;
{--------}

Function TfsDBStandaloneList.IndexOfItem(aItem: TfsDBListItem): Integer;
Begin
  With dblList.BeginRead Do
    Try
      Result := IndexOfItem(@aItem)
    Finally
      EndRead;
    End;
End;
{Begin !!.02}
{--------}

Procedure TfsDBStandaloneList.BeginRead;
Begin
  dblList.BeginRead;
End;
{--------}

Procedure TfsDBStandaloneList.BeginWrite;
Begin
  dblList.BeginWrite;
End;
{--------}

Procedure TfsDBStandaloneList.EndRead;
Begin
  dblList.EndRead;
End;
{--------}

Procedure TfsDBStandaloneList.EndWrite;
Begin
  dblList.EndWrite;
End;
{End !!.02}
{====================================================================}

Procedure FinalizeUnit;
Begin
  fsStrResDataSet.Free;
End;

Procedure InitializeUnit;
Begin
  fsStrResDataSet := Nil;
  fsStrResDataSet := TfsStringResource.Create(hInstance, 'FS_DATASET_ERROR_STRINGS');
End;

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

