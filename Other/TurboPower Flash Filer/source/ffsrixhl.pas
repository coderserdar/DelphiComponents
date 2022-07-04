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

{$I ffdefine.inc}

unit ffsrixhl;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  ffsrmgr,
  ffllexcp,
  ffsrintf,
  ffsrbase;

type
  TffSrIndexHelper = class(TffUCStrListItem)
  protected {private}
    ihFieldTypes: TffFieldTypes;
  public
    class procedure Register(const aName      : TffShStr;
                                   aFieldTypes: TffFieldTypes;
                             const aParams    : array of const);
      {-creates an instance of this object and adds it to the internal list}
    class procedure Unregister;
      {-removes all IndexHelpers of this ClassType from the internal list}
    class function FindHelper(const aName     : TffShStr;
                                    aFieldType: TffFieldType)
                                              : TffSrIndexHelper;
      {-searches the internal list for a helper with the specified name
        and checks if the fieldtype is supported by that helper}

    procedure Initialize(const aParams: array of const); virtual;
      {-called after the object is created by Register}
    procedure BuildKey(const aFieldBuffer;
                         var aKeyBuffer;
                             aFieldDesc: PffFieldDescriptor;
                             aLenToUse: Integer); virtual;
      {-builds the key for a specific field
        aLenToUse > 0 means a partial string field is required}
    function CompareKey(const Key1,
                              Key2;
                              aFieldDesc: PffFieldDescriptor;
                              aLenToUse : Integer;
                              aNoCase   : Boolean)
                                        : Integer; virtual;
      {-compares the keys for a specific field
        aLenToUse > 0 means a partial string field is required}

    property FieldTypes : TffFieldTypes
      {-field types supported by this index helper}
       read ihFieldTypes;
  end;

  TffSrNumbersOnlyIH = class(TffSrIndexHelper)
  public
    procedure BuildKey(const aFieldBuffer;
                         var aKeyBuffer;
                             aFieldDesc: PffFieldDescriptor;
                             aLenToUse: Integer); override;
  end;

  { Use the following to pass around arrays of index helpers. }
  PffIndexHelperArray = ^TffIndexHelperArray;
  TffIndexHelperArray = array[0..ffcl_MaxIndexFlds] of TffSrIndexHelper;

{ Pre-defined helper names }
const
  ffc_ihlpNumbersOnly = 'NumbersOnly';

implementation

uses
  TypInfo,
  fftbbase;

var
  _HelperList : TffThreadList;

{===TffSrIndexHelper=================================================}
class procedure TffSrIndexHelper.Register(const aName      : TffShStr;
                                                aFieldTypes: TffFieldTypes;
                                          const aParams    : array of const);
var
  Helper: TffSrIndexHelper;
begin
  _HelperList.BeginWrite;
  try
    Helper := Create(aName);
    if not _HelperList.Insert(Helper) then begin
      Helper.Free;
      FFRaiseException(EffServerException, ffStrResGeneral,
                       fferrIxHlprRegistered, [aName]);
    end else try
      Helper.ihFieldTypes := aFieldTypes;
      Helper.Initialize(aParams);
    except
      Helper.Free;
      raise;
    end;
  finally
    _HelperList.EndWrite;
  end;
end;
{--------}
class procedure TffSrIndexHelper.Unregister;
var
  i                           : Integer;
begin
  if not Assigned(_HelperList) then
    Exit;
  _HelperList.BeginWrite;
  try
    for i := Pred(_HelperList.Count) downto 0 do
      with _HelperList.Items[i] do
        if (ClassType = Self) or ClassType.InheritsFrom(Self) then
          Free;
  finally
    _HelperList.EndWrite;
  end;
end;
{--------}
class function TffSrIndexHelper.FindHelper(const aName     : TffShStr;
                                                 aFieldType: TffFieldType)
                                                           : TffSrIndexHelper;
var
  i: Integer;
begin
  _HelperList.BeginRead;
  try
    i := _HelperList.Index(aName);
    if i < 0 then
      FFRaiseException(EffServerException, ffStrResGeneral,
                       fferrIxHlprNotReg, [aName]);
    Result := TffSrIndexHelper(_HelperList.Items[i]);
    if not (aFieldType in Result.ihFieldTypes) then
      FFRaiseException(EffServerException, ffStrResGeneral,
                       fferrIxHlprNotSupp,
                       [aName, GetEnumName(TypeInfo(TffFieldType), ord(aFieldType))]);
  finally
    _HelperList.EndRead;
  end;
end;
{--------}
procedure TffSrIndexHelper.Initialize(const aParams: array of const);
begin
  { May be overriden by descendant classes for custom initialization. }
end;
{--------}
procedure TffSrIndexHelper.BuildKey(const aFieldBuffer;
                                      var aKeyBuffer;
                                          aFieldDesc: PffFieldDescriptor;
                                          aLenToUse: Integer);
begin
  if aLenToUse<0 then
    Move(aFieldBuffer, aKeyBuffer, aFieldDesc^.fdLength)
  else with aFieldDesc^ do begin
    if (fdType = fftShortString) or
       (fdType = fftShortAnsiStr) then begin
      Move(aFieldBuffer, aKeyBuffer, aLenToUse+1);
      Byte(aKeyBuffer) := aLenToUse;
    end
    else
      Move(aFieldBuffer, aKeyBuffer, aLenToUse);
  end;
end;
{--------}
function TffSrIndexHelper.CompareKey(const Key1,
                                           Key2;
                                           aFieldDesc: PffFieldDescriptor;
                                           aLenToUse : Integer;
                                           aNoCase   : Boolean)
                                                     : Integer;
begin
  with aFieldDesc^ do
    if aLenToUse < 0 then
      Result := FFKeyCompareField(Key1, Key2, fdType, fdLength, aNoCase)
    else
      Result := FFKeyCompareField(Key1, Key2, fdType, aLenToUse, aNoCase);
end;
{====================================================================}

{===TffSrNumbersOnlyIH================================================}
procedure TffSrNumbersOnlyIH.BuildKey(const aFieldBuffer;
                                        var aKeyBuffer;
                                            aFieldDesc: PffFieldDescriptor;
                                            aLenToUse: Integer);
var
  Source : TffShStr absolute aFieldBuffer;
  Target : TffShStr absolute aKeyBuffer;
  i      : Integer;
begin
  if aLenToUse < 0 then
    aLenToUse := aFieldDesc^.fdUnits;
  Target := '';
  for i:= 1 to Length(Source) do
    //#254 is allowed for setting "123*" type of ranges...
    if Source[i] in ['0'..'9', #254] then begin
      Target := Target + Source[i];
      if Length(Target) >= aLenToUse then
        Exit;
    end;
end;
{====================================================================}

initialization
  _HelperList := TffThreadList.Create;
  TffSrIndexHelper.Register
                     ('',
                      [fftBoolean..fftDateTime, fftByteArray..fftWideString],
                      {$IFDEF DCC4OrLater}
                      []);
                      {$ELSE}
                      ['']);
                      {$ENDIF}


  TffSrNumbersOnlyIH.Register(ffc_ihlpNumbersOnly,
                              [fftShortString, fftShortAnsiStr],
                              {$IFDEF DCC4OrLater}
                              []);
                              {$ELSE}
                              ['']);
                              {$ENDIF}

finalization
  TffSrNumbersOnlyIH.Unregister;
  TffSrIndexHelper.Unregister;
  _HelperList.Free;
  _HelperList:=nil;
end.
