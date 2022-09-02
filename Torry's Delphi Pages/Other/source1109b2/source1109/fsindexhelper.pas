{*********************************************************}
{* FlashFiler: Index helper objects for composite indices*}
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
 * Thorsten Engler.
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 * Used with permisson.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I fsdefine.inc}

Unit fsindexhelper;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fssrmgr,
  fsllexcp,
  fssrintf,
  fssrbase;

Type
  TfsSrIndexHelper = Class(TfsUCStrListItem)
  Protected {private}
    ihFieldTypes: TfsFieldTypes;
  Public
    Class Procedure Register(Const aName: TffShStr;
      aFieldTypes: TfsFieldTypes;
      Const aParams: Array Of Const);
    {-creates an instance of this object and adds it to the internal list}
    Class Procedure Unregister;
    {-removes all IndexHelpers of this ClassType from the internal list}
    Class Function FindHelper(Const aName: TffShStr;
      aFieldType: TfsFieldType)
      : TfsSrIndexHelper;
    {-searches the internal list for a helper with the specified name
      and checks if the fieldtype is supported by that helper}

    Procedure Initialize(Const aParams: Array Of Const); Virtual;
    {-called after the object is created by Register}
    Procedure BuildKey(Const aFieldBuffer;
      Var aKeyBuffer;
      aFieldDesc: PffFieldDescriptor;
      aLenToUse: Integer); Virtual;
    {-builds the key for a specific field
      aLenToUse > 0 means a partial string field is required}
    Function CompareKey(Const Key1,
      Key2;
      aFieldDesc: PffFieldDescriptor;
      aLenToUse: Integer;
      aNoCase: Boolean;
      PartLen: integer = 0)
      : Integer; Virtual;
    {-compares the keys for a specific field
      aLenToUse > 0 means a partial string field is required}

    Property FieldTypes: TfsFieldTypes
      {-field types supported by this index helper}
    Read ihFieldTypes;
  End;

  TffSrNumbersOnlyIH = Class(TfsSrIndexHelper)
  Public
    Procedure BuildKey(Const aFieldBuffer;
      Var aKeyBuffer;
      aFieldDesc: PffFieldDescriptor;
      aLenToUse: Integer); Override;
  End;

  { Use the following to pass around arrays of index helpers. }
  PffIndexHelperArray = ^TffIndexHelperArray;
  TffIndexHelperArray = Array[0..fscl_MaxIndexFlds] Of TfsSrIndexHelper;

  { Pre-defined helper names }
Const
  ffc_ihlpNumbersOnly = 'NumbersOnly';

Implementation

Uses
  TypInfo,
  fstablehelper;

Var
  _HelperList: TFSSpecThreadList;

  {===TfsSrIndexHelper=================================================}

Class Procedure TfsSrIndexHelper.Register(Const aName: TffShStr;
  aFieldTypes: TfsFieldTypes;
  Const aParams: Array Of Const);
Var
  Helper: TfsSrIndexHelper;
Begin
  _HelperList.BeginWrite;
  Try
    Helper := Create(aName);
    If Not _HelperList.Insert(Helper) Then
      Begin
        Helper.Free;
        FSRaiseException(EfsServerException, fsStrResGeneral,
          fserrIxHlprRegistered, [aName]);
      End
    Else
      Try
        Helper.ihFieldTypes := aFieldTypes;
        Helper.Initialize(aParams);
      Except
        Helper.Free;
        Raise;
      End;
  Finally
    _HelperList.EndWrite;
  End;
End;
{--------}

Class Procedure TfsSrIndexHelper.Unregister;
Var
  i: Integer;
Begin
  If Not Assigned(_HelperList) Then
    Exit;
  _HelperList.BeginWrite;
  Try
    For i := Pred(_HelperList.Count) Downto 0 Do
      With _HelperList.Items[i] Do
        If (ClassType = Self) Or ClassType.InheritsFrom(Self) Then
          Free;
  Finally
    _HelperList.EndWrite;
  End;
End;
{--------}

Class Function TfsSrIndexHelper.FindHelper(Const aName: TffShStr;
  aFieldType: TfsFieldType)
  : TfsSrIndexHelper;
Var
  i: Integer;
Begin
  _HelperList.BeginRead;
  Try
    i := _HelperList.Index(aName);
    If i < 0 Then
      FSRaiseException(EfsServerException, fsStrResGeneral,
        fserrIxHlprNotReg, [aName]);
    Result := TfsSrIndexHelper(_HelperList.Items[i]);
    If Not (aFieldType In Result.ihFieldTypes) Then
      FSRaiseException(EfsServerException, fsStrResGeneral,
        fserrIxHlprNotSupp,
        [aName, GetEnumName(TypeInfo(TfsFieldType), ord(aFieldType))]);
  Finally
    _HelperList.EndRead;
  End;
End;
{--------}

Procedure TfsSrIndexHelper.Initialize(Const aParams: Array Of Const);
Begin
  { May be overriden by descendant classes for custom initialization. }
End;
{--------}

Procedure TfsSrIndexHelper.BuildKey(Const aFieldBuffer;
  Var aKeyBuffer;
  aFieldDesc: PffFieldDescriptor;
  aLenToUse: Integer);
Begin
  If aLenToUse < 0 Then
    Move(aFieldBuffer, aKeyBuffer, aFieldDesc^.fdLength)
  Else
    With aFieldDesc^ Do
      Begin
        {If (fdType = fstShortString) Then
          Begin
            Move(aFieldBuffer, aKeyBuffer, aLenToUse + 1);
            Byte(aKeyBuffer) := aLenToUse;
          End
        Else }
          Move(aFieldBuffer, aKeyBuffer, aLenToUse);
      End;
End;
{--------}

Function TfsSrIndexHelper.CompareKey(Const Key1,
  Key2;
  aFieldDesc: PffFieldDescriptor;
  aLenToUse: Integer;
  aNoCase: Boolean;
  PartLen: integer = 0)
  : Integer;
Begin
  With aFieldDesc^ Do
    If aLenToUse < 0 Then
      Result := FFKeyCompareField(Key1, Key2, fdType, fdLength, aNoCase, PartLen)
    Else
      Result := FFKeyCompareField(Key1, Key2, fdType, aLenToUse, aNoCase, PartLen);
End;
{====================================================================}

{===TffSrNumbersOnlyIH================================================}

Procedure TffSrNumbersOnlyIH.BuildKey(Const aFieldBuffer;
  Var aKeyBuffer;
  aFieldDesc: PffFieldDescriptor;
  aLenToUse: Integer);
Var
  Source: TffShStr Absolute aFieldBuffer;
  Target: TffShStr Absolute aKeyBuffer;
  i: Integer;
Begin
  If aLenToUse < 0 Then
    aLenToUse := aFieldDesc^.fdUnits;
  Target := '';
  For i := 1 To Length(Source) Do
    //#254 is allowed for setting "123*" type of ranges...
    If Source[i] In ['0'..'9', #254] Then
      Begin
        Target := Target + Source[i];
        If Length(Target) >= aLenToUse Then
          Exit;
      End;
End;
{====================================================================}

Initialization
  _HelperList := TFSSpecThreadList.Create;
  TfsSrIndexHelper.Register
    ('',
    [fstBoolean..fstDateTime, fstBcd..fstWideString {fstUnicode}],
    {$IFDEF DCC4OrLater}
    []);
  {$ELSE}
    ['']);
  {$ENDIF}

  TffSrNumbersOnlyIH.Register(ffc_ihlpNumbersOnly,
    [fstShortString],
    {$IFDEF DCC4OrLater}
    []);
  {$ELSE}
    ['']);
  {$ENDIF}

Finalization
  TffSrNumbersOnlyIH.Unregister;
  TfsSrIndexHelper.Unregister;
  _HelperList.Free;
  _HelperList := Nil;
End.

