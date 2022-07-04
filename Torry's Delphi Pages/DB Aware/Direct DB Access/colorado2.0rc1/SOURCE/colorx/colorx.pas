{The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in
compliance with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
License for the specific language governing rights and limitations
under the License.

The Original Code is colorADO Database Components.

The Initial Developer of the Original Code is Maciej Kujalowicz.
Portions created by Maciej Kujalowicz are Copyright (C) 2000-2003
Maciej Kujalowicz. All Rights Reserved.}

unit colorx;

interface

{$I ..\COLORADO\CDEFINES.INC}

uses Forms, Windows, SysUtils, Classes, consts, db, dbconsts, ActiveX, ComConst,
{$IFDEF VCL60}
     Variants, RTLConsts,
{$ENDIF}
     Comobj, cadodb, cadoxdb, colorado;

type

  TSchemaObjects = set of (soTables, soViews, soProcedures);

  TCollectionDataSet = class;

  TDBCatalog = class(TComponent)
  private
    { Private declarations }
    FADOXCatalog: IADOXCatalog;
    FActive: Boolean;
    FConnection: TConnection;
    FDataSource: TDataSource;
    FDataSet: TCollectionDataSet;
    procedure SetActive(Value: Boolean);
    procedure SetConnection(Connection: TConnection);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Close;
    procedure CreateNew(const ConnectionString: WideString);
    procedure Open;
  published
    property Active : Boolean read FActive write SetActive default FALSE;
    property Connection: TConnection read FConnection write SetConnection;
    property CatalogObject: IADOXCatalog read FADOXCatalog;
  end;

  TCollectionDataSet = class(TCDataSet)
  private
    { Private declarations }
    FCurrentItem: Integer;
    FCursorOpen: Boolean;
    FItemCount: Integer;
    procedure GetFields(Fields: string; List: TStrings);
  protected
    { Protected declarations }
    procedure AppendItem; virtual; abstract;
    function GetRecNo: Longint; override;
    procedure SetRecNo(Value: Longint); override;
    function GetCurrentItem(Buffer: PChar): Integer;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function GetRecordCount: Integer; override;
    procedure GetRecordData(Buffer: PChar); virtual; abstract;
    function GetCanModify: Boolean; override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalRefresh; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalPost; override;
    function IsCursorOpen: Boolean; override;
    function GetItemCount: Integer; virtual;
    procedure InitItemFields; virtual;
    procedure DeleteItem(Index: Integer); virtual;
  public
    { Public declarations }
    constructor Create(Aowner: TComponent); override;
    function IsSequenced: Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; overload; override;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; Origin: TLocateOrigin): Boolean; reintroduce; overload;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
  published
    { Published declarations }
  end;

  TCollectionDataLink = class;

  TADOXDataSet = class(TCollectionDataSet)
  private
    FDataLink: TCollectionDataLink;
    FOperationAllowed: Boolean;
    FEmpty: Boolean;
    FADOXObject: Variant;
    procedure SetDS(Value: TDataSource);
    function GetDS: TDataSource;
    procedure RefreshDetail;
  protected
    FDataSource: TDataSource;
    procedure DoAfterMasterChange; virtual;
    function IsParentValid(Parent: TComponent): Boolean; virtual;
    function GetADOXObject: Variant;
    function GetDispADOXObject: IDispatch;
    function GetCanModify: Boolean; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; virtual;
    function GetRecordCount: Integer; override;
    procedure GetRecordData(Buffer: PChar); override;
    property MasterSource: TDataSource read GetDS write SetDS;
    procedure InternalInsert; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalPost; override;
    procedure RemoveMasterSource; virtual; abstract;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); virtual;
    property Active;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ADOXObject: Variant read GetADOXObject;
  end;

  TCollectionDataLink = class(TDetailDataLink)
  private
    FDataSet: TADOXDataSet;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    function GetDetailDataSet: TDataSet; override;
    procedure CheckBrowseMode; override;
  public
    constructor Create(ADataSet: TADOXDataSet);
  end;

  TADOXColumnedDataSet = class(TADOXDataSet);

  TDBTables = class(TADOXColumnedDataSet)
  private
    function GetCatalog: TDBCatalog;
    procedure SetCatalog(const Value: TDBCatalog);
    function GetTableObject: IADOXTable;
  protected
    procedure AppendItem; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure InitItemFields; override;
    procedure DeleteItem(Index: Integer); override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property TableObject: IADOXTable read GetTableObject;
  published
    property Catalog: TDBCatalog read GetCatalog write SetCatalog;
  end;

  TADOXCommandDataSet = class(TADOXDataSet)
  private
    FADOCommand: IADOCommand;
  protected
    procedure DataEvent(Event: TDataEvent; Info: LongInt); override;
    procedure DoAfterOpen; override;
    procedure InternalInsert; override;
  end;

  TDBViews = class(TADOXCommandDataSet)
  private
    function GetCatalog: TDBCatalog;
    procedure SetCatalog(const Value: TDBCatalog);
    function GetViewObject: IADOXView;
  protected
    procedure AppendItem; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure InitItemFields; override;
    procedure DeleteItem(Index: Integer); override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ViewObject: IADOXView read GetViewObject;
  published
    property Catalog: TDBCatalog read GetCatalog write SetCatalog;
  end;


  TDBProcedures = class(TADOXCommandDataSet)
  private
    function GetCatalog: TDBCatalog;
    procedure SetCatalog(const Value: TDBCatalog);
    function GetProcedureObject: IADOXProcedure;
  protected
    procedure AppendItem; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure InitItemFields; override;
    procedure DeleteItem(Index: Integer); override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ProcedureObject: IADOXProcedure read GetProcedureObject;
  published
    property Catalog: TDBCatalog read GetCatalog write SetCatalog;
  end;

  TDBParameters = class(TADOXDataSet)
  private
    FADOCommand: IADOCommand;
    function GetParent: TADOXCommandDataSet;
    procedure SetParent(const Value: TADOXCommandDataSet);
    function GetParameterObject: IADOParameter;
  protected
    procedure AppendItem; override;
    procedure DeleteItem(Index: Integer); override;
    procedure DoAfterMasterChange; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeEdit; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure InitItemFields; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
    function ValidateFieldData(Field: TField;
      var Value: Variant): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ParameterObject: IADOParameter read GetParameterObject;
  published
    property Parent: TADOXCommandDataSet read GetParent write SetParent;
  end;

  TDBIndexes = class(TADOXColumnedDataSet)
  private
    function GetTables: TDBTables;
    procedure SetTables(const Value: TDBTables);
    function GetIndexObject: IADOXIndex;
  protected
    procedure AppendItem; override;
    procedure DeleteItem(Index: Integer); override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    procedure InitItemFields; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function ValidateFieldData(Field: TField;
      var Value: Variant): Boolean; override;
    function GetItemCount: Integer; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property IndexObject: IADOXIndex read GetIndexObject;
  published
    property Tables: TDBTables read GetTables write SetTables;
  end;

  TDBKeys = class(TADOXColumnedDataSet)
  private
    function GetTables: TDBTables;
    procedure SetTables(const Value: TDBTables);
    function GetKeyObject: IADOXKey;
  protected
    procedure AppendItem; override;
    procedure DeleteItem(Index: Integer); override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    procedure InitItemFields; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function ValidateFieldData(Field: TField;
      var Value: Variant): Boolean; override;
    function GetItemCount: Integer; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property KeyObject: IADOXKey read GetKeyObject;
  published
    property Tables: TDBTables read GetTables write SetTables;
  end;

  TDBColumns = class(TADOXDataSet)
  private
    function GetParent: TADOXColumnedDataSet;
    procedure SetParent(const Value: TADOXColumnedDataSet);
    function GetColumnObject: IADOXColumn;
  protected
    procedure AppendItem; override;
    procedure DeleteItem(Index: Integer); override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    procedure InitItemFields; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function ValidateFieldData(Field: TField;
      var Value: Variant): Boolean; override;
    function GetItemCount: Integer; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ColumnObject: IADOXColumn read GetColumnObject;
  published
    property Parent: TADOXColumnedDataSet read GetParent write SetParent;
  end;

  TDBProperties = class(TADOXDataSet)
  private
    procedure SetParent(const Value: TADOXDataSet);
    function GetParent: TADOXDataSet;
    function GetPropertyObject: IADOProperty;
  protected
    procedure DeleteItem(Index: Integer); override;
    procedure DoBeforeInsert; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure InitItemFields; override;
    procedure InternalEdit; override;
    procedure InternalRefresh; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    property PropertyObject: IADOProperty read GetPropertyObject;
  published
    property Parent: TADOXDataSet read GetParent write SetParent;
  end;

  TADOXSecurityCollection = class(TADOXDataSet);

  TDBGroups = class(TADOXSecurityCollection)
  private
    function GetCatalogOrUsers: TComponent;
    procedure SetCatalogOrUsers(const Value: TComponent);
    function GetGroupObject: IADOXGroup;
  protected
    procedure AppendItem; override;
    procedure DeleteItem(Index: Integer); override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    procedure InitItemFields; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property GroupObject: IADOXGroup read GetGroupObject;
  published
    property CatalogOrUsers: TComponent read GetCatalogOrUsers write SetCatalogOrUsers;
  end;

  TDBUsers = class(TADOXSecurityCollection)
  private
    function GetCatalogOrGroups: TComponent;
    procedure SetCatalogOrGroups(const Value: TComponent);
    function GetUserObject: IADOXUser;
  protected
    procedure AppendItem; override;
    procedure DeleteItem(Index: Integer); override;
    procedure InitItemFields; override;
    procedure InternalInsert; override;
    procedure InternalRefresh; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    procedure SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
      Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangePassword(const OldPassword: WideString;
      const NewPassword: WideString);
    property UserObject: IADOXUser read GetUserObject;
  published
    property CatalogOrGroups: TComponent read GetCatalogOrGroups write SetCatalogOrGroups;
  end;

  TDBPermissions = class(TADOXDataSet)
  private
    FCatalog: TDBCatalog;
    FSchemaObjects: TSchemaObjects;
    function GetUsersOrGroups: TADOXSecurityCollection;
    procedure SetUsersOrGroups(const Value: TADOXSecurityCollection);
  protected
    procedure DeleteItem(Index: Integer); override;
    procedure InitItemFields; override;
    procedure InternalInsert; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    function GetFieldValue(ADOXObject: IDispatch; Item: Integer;
      FieldNo: Integer): Variant; override;
    function GetItemCount: Integer; override;
    function ValidateFieldData(Field: TField;
      var Value: Variant): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property UsersOrGroups: TADOXSecurityCollection read GetUsersOrGroups write SetUsersOrGroups;
    property SchemaObjects: TSchemaObjects read FSchemaObjects
             write FSchemaObjects;
  end;


implementation
uses cconsts;


{:-- TCollectionDataSet }

function TCollectionDataSet.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
begin
  Result := -1;
end;

constructor TCollectionDataSet.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FCursorOpen := FALSE;
  FCurrentItem := -1;
end;

procedure TCollectionDataSet.DeleteItem(Index: Integer);
begin

end;

function TCollectionDataSet.GetCanModify: Boolean;
begin
  Result := TRUE;
end;

function TCollectionDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var RecBuffer     : PChar;
begin
  if State = dsCalcFields
     then RecBuffer := CalcBuffer
     else RecBuffer := ActiveBuffer;
  Result := InternalGetFieldData(Field, RecBuffer, Buffer, FALSE);
end;

function TCollectionDataSet.GetItemCount: Integer;
begin
  Result := 0;
end;

function TCollectionDataSet.GetRecNo: Longint;
begin
  Result := -1;
  if ActiveBuffer <> nil then Result := Integer(ActiveBuffer^);
end;

function TCollectionDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  InternalInitRecord(Buffer);
  Result := grOK;
  case GetMode of
    gmPrior: if FCurrentItem <= 0
                then Result := grBOF
                else dec(FCurrentItem);
    gmNext : if FCurrentItem >= GetRecordCount - 1
                then Result := grEOF
                else inc(FCurrentItem);
    gmCurrent: begin
                 if FCurrentItem < 0 then Result := grBOF;
                 if FCurrentItem > GetRecordCount - 1 then Result := grEOF;
               end;
  end;
  if FCurrentItem > GetRecordCount then FCurrentItem := GetRecordCount - 1;
  if Result = grOK then
     begin
       with PRecInfo(Buffer + RecInfoOfs)^ do
            begin
              BookmarkFlag := bfCurrent;
              Bookmark := FCurrentItem;
            end;
       GetRecordData(Buffer);
       GetCalcFields(Buffer);
     end;
end;

function TCollectionDataSet.GetRecordCount: Integer;
begin
  Result := GetItemCount;
end;

procedure TCollectionDataSet.InitItemFields;
begin

end;

procedure TCollectionDataSet.InternalAddRecord(Buffer: Pointer;
  Append: Boolean);
begin
  InternalPost;
end;

procedure TCollectionDataSet.InternalClose;
begin
  BindFields(FALSE);
  if DefaultFields then DestroyFields;
  FCursorOpen := FALSE;
end;

procedure TCollectionDataSet.InternalDelete;
begin
  try
    DeleteItem(PRecInfo(ActiveBuffer + RecInfoOfs)^.Bookmark);
  finally
    FItemCount := GetItemCount;
  end;
end;

procedure TCollectionDataSet.InternalFirst;
begin
  FCurrentItem := -1;
end;

procedure TCollectionDataSet.InternalGotoBookmark(Bookmark: Pointer);
var FItemNo: Integer;
begin
  FItemNo := Variant(Bookmark^);
  if (FItemNo > GetItemCount - 1)
  or (FItemNo < 0)
  then DatabaseError(SRecordNotFound, Self)
  else FCurrentItem := FItemNo;
end;

procedure TCollectionDataSet.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  InitItemFields;
end;

procedure TCollectionDataSet.InternalLast;
begin
  FCurrentItem := GetRecordCount;
end;

procedure TCollectionDataSet.InternalOpen;
begin
  FCursorOpen := TRUE;
  FieldDefs.Updated := False;
  FieldDefs.Update;
  if DefaultFields then CreateFields;
  BindFields(True);
  FCurrentItem := -1;
  inherited;
  FItemCount := GetItemCount;
end;

procedure TCollectionDataSet.InternalPost;
begin
  if State = dsInsert then
     try
       AppendItem;
     finally
       FItemCount := GetItemCount;
     end;
end;

procedure TCollectionDataSet.InternalRefresh;
begin
  FItemCount := GetItemCount;
end;

function TCollectionDataSet.IsCursorOpen: Boolean;
begin
  Result := FCursorOpen;
end;

function TCollectionDataSet.IsSequenced: Boolean;
begin
  Result := FALSE;
end;

function TCollectionDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := Locate(KeyFields, KeyValues, Options, loFromBeginningForward);
end;

function TCollectionDataSet.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var List             : TStringList;
    i                : Integer;
begin
  Result := Null;
  List := TStringList.Create;
  try
    Result := Locate(KeyFields, KeyValues, [loCaseInsensitive], loFromBeginningForward);
    if Result then
       begin
         GetFields(ResultFields, List);
         if List.Count > 1 then
            begin
              Result := VarArrayCreate([0, List.Count - 1], varVariant);
              for i := 0 to List.Count - 1 do
                  Result := FieldByName(List [i]).AsVariant;
            end else
                  if List.Count = 1
                     then Result := FieldByName(List [0]).AsVariant
                     else Result := Null;
       end else Result := Null;
  finally
    List.Free;
  end;
end;

procedure TCollectionDataSet.SetRecNo(Value: Integer);
begin

end;

function TCollectionDataSet.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions;
  Origin: TLocateOrigin): Boolean;
var FPrevItem: Integer;
    FKeyValues: Variant;
    FFieldList: TStringList;
    ArrayLB, i: Integer;
    FValue1, FValue2: string;
    GetMode: TGetMode;
    CompareOK: Boolean;
    GetResult: TGetResult;
begin
  FFieldList := TStringList.Create;
  GetFields(KeyFields, FFieldList);
  if VarIsArray(KeyValues)
     then FKeyValues := KeyValues
     else
       begin
         FKeyValues := VarArrayCreate([1, 1], varVariant);
         FKeyValues [1] := KeyValues;
       end;
  if (VarArrayHighBound(FKeyValues, 1) - VarArrayLowBound(FKeyValues, 1) + 1
     <> FFieldList.Count) or (FFieldList.Count = 0) then
     begin
       FFieldList.Free;
       FKeyValues := Null;
       DatabaseError('Invalid parameter', nil);
     end;
  FPrevItem := FCurrentItem;
  Result := FALSE;
  CheckBrowseMode;
  DoBeforeScroll;
  GetMode := gmNext;
  if Origin = loFromBeginningForward then
     begin
       FCurrentItem := 0;
       GetMode := gmNext;
     end;
  if Origin = loFromCurrentForward then
     begin
       Inc(FCurrentItem);
       GetMode := gmNext;
     end;
  if Origin = loFromCurrentBackward then
     begin
       Dec(FCurrentItem);
       GetMode := gmPrior;
     end;
  if Origin = loFromEndBackward then
     begin
       FCurrentItem := GetRecordCount - 1;
       GetMode := gmPrior;
     end;
  if GetRecord(ActiveBuffer, gmCurrent, FALSE) <> grOK then
     begin
       FFieldList.Free;
       FKeyValues := Null;
       FCurrentItem := FPrevItem;
       Exit;
     end;
  ArrayLB := VarArrayLowBound(FKeyValues, 1);
  try
    repeat
      CompareOK := TRUE;
      for i := 0 to FFieldList.Count - 1 do
          begin
            if loCaseInsensitive in Options then
               begin
                 FValue1 := AnsiUpperCase(FieldByName(FFieldList [i]).AsString);
                 FValue2 := AnsiUpperCase(VarToStr(FKeyValues [i + ArrayLB]));
               end
                 else
                   begin
                     FValue1 := FieldByName(FFieldList [i]).AsString;
                     FValue2 := VarToStr(FKeyValues [i + ArrayLB]);
                   end;
            if loPartialKey in Options then
               begin
                 if AnsiStrPos(PChar(FValue1), PChar(FValue2)) = nil then
                    CompareOK := FALSE;
               end
                 else
                   begin
                     if AnsiCompareStr(FValue1, FValue2) <> 0 then
                        CompareOK := FALSE;
                   end;
          end;
      GetResult := grOK;
      if not CompareOK then
         GetResult := GetRecord(ActiveBuffer, GetMode, FALSE);
    until CompareOK or (GetResult <> grOK);
    if CompareOK
       then
         begin
           Result := TRUE;
           CursorPosChanged;
           Resync([rmExact, rmCenter]);
           DoAfterScroll;
         end
       else FCurrentItem := FPrevItem;
    FFieldList.Free;
    FKeyValues := Null;
  except
    FCurrentItem := FPrevItem;
    FFieldList.Free;
    FKeyValues := Null;
    raise;
  end;
  if (FCurrentItem < 0)
  or (FCurrentItem > GetItemCount - 1)
  then
    begin
      Result := FALSE;
      CursorPosChanged;
    end;
end;

procedure TCollectionDataSet.GetFields(Fields: string; List: TStrings);
var p: Integer;
begin
  List.Clear;
  p := Pos(';', Fields);
  while p > 0 do
        begin
          if p > 1 then List.Add(Copy(Fields, 1, p-1));
          System.Delete(Fields, 1, p);
          p := Pos(';', Fields);
        end;
  if Length(Fields) > 0 then List.Add(Fields);
end;

function TCollectionDataSet.GetCurrentItem(Buffer: PChar): Integer;
begin
  Result := Integer(PRecInfo(Buffer + RecInfoOfs)^.Bookmark);
end;

{:-- TADOXDataSet }

constructor TADOXDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOperationAllowed := FALSE;
  FEmpty := FALSE;
  FDataLink := TCollectionDataLink.Create(Self);
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := Self;
end;

destructor TADOXDataSet.Destroy;
begin
  FDataSource.Free;
  FOperationAllowed := TRUE;
  FDataLink.Free;
  FOperationAllowed := TRUE;
  inherited Destroy;
end;

procedure TADOXDataSet.DoAfterMasterChange;
begin
  
end;

function TADOXDataSet.GetADOXObject: Variant;
begin
  CheckActive;
  if State = dsInsert
     then Result := FADOXObject
     else if ActiveBuffer = nil
             then Result := Null
             else
               if EOF and BOF
                  then DatabaseError(SRecordNotFound, Self)
                  else Result := GetFieldValue(nil,
                                   GetCurrentItem(ActiveBuffer), 0);
{ if VarType(Result) <> varDispatch then
     raise Exception.Create(SVarNotObject);}
end;

function TADOXDataSet.GetCanModify: Boolean;
begin
  if FEmpty
     then Result := FALSE
     else Result := inherited GetCanModify;
end;

function TADOXDataSet.GetDispADOXObject: IDispatch;
var FADOXObject: Variant;
begin
  FADOXObject := GetADOXObject;
  if VarType(FADOXObject) <> varDispatch then
     raise Exception.Create(SVarNotObject);
  Result := IDispatch(FADOXObject);
  FADOXObject := Null;
end;

function TADOXDataSet.GetDS: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TADOXDataSet.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin

end;

function TADOXDataSet.GetRecordCount: Integer;
begin
  if FEmpty
     then Result := 0
     else Result := inherited GetRecordCount;
end;

procedure TADOXDataSet.GetRecordData(Buffer: PChar);
var i, n: Integer;
begin
  FADOXObject := GetFieldValue(nil, FCurrentItem, 0);
  for i := 0 to Fields.Count - 1 do
      if Fields [i].FieldKind = fkData then
         begin
           n := Fields [i].FieldNo;
           if VarIsNull(FADOXObject)
              then TRecData(pointer(Buffer)^)[i].Data := Null
              else TRecData(pointer(Buffer)^)[i].Data :=
                   GetFieldValue(FADOXObject, FCurrentItem, n);
         end;
end;

procedure TADOXDataSet.InternalClose;
begin
  if not FOperationAllowed and not (csDesigning in ComponentState)
     and not (csDestroying in ComponentState)
     then DatabaseError('Operation not allowed');
  inherited InternalClose;
end;

procedure TADOXDataSet.InternalInsert;
var i, n: Integer;
begin
  inherited;
  for i := 0 to Fields.Count - 1 do
      if Fields [i].FieldKind = fkData then
         begin
           n := Fields [i].FieldNo;
           if VarIsNull(FADOXObject)
              then TRecData(pointer(ActiveBuffer)^)[i].Data := Null
              else TRecData(pointer(ActiveBuffer)^)[i].Data :=
                   GetFieldValue(FADOXObject, -1, n);
         end;
end;

procedure TADOXDataSet.InternalOpen;
begin
  if  (FDataLink.DataSource <> nil)
  and (FDataLink.DataSource.DataSet <> nil)
  and (FDataLink.DataSet.Active)
  and ( (FDataLink.DataSource.Owner is TDBCatalog)
       or not (FDataLink.DataSet.EOF and FDataLink.DataSet.BOF)
       or (FDataLink.DataSet.State = dsInsert)
      )
  and IsParentValid(FDataLink.DataSource.Owner)
  then inherited InternalOpen
  else DatabaseError('Operation not allowed');
end;

procedure TADOXDataSet.InternalPost;
var i, n: Integer;
    ADOXObject: Variant;
begin
  ADOXObject := GetADOXObject;
  for i := 0 to Fields.Count - 1 do
    if Fields [i].FieldKind = fkData then
      begin
      n := Fields [i].FieldNo;
      if TRecData(pointer(ActiveBuffer)^)[i].Modified then
         if VarIsNull(ADOXObject)
            then SetFieldValue(nil, n, OleVariant(TRecData(pointer(ActiveBuffer)^)[i].Data))
            else SetFieldValue(ADOXObject, n, OleVariant(TRecData(pointer(ActiveBuffer)^)[i].Data));
      end;
  ADOXObject := Null;
  inherited InternalPost;
end;

function TADOXDataSet.IsParentValid(Parent: TComponent): Boolean;
begin
  Result := TRUE;
end;

procedure TADOXDataSet.RefreshDetail;
begin
  try
    FOperationAllowed := TRUE;
    if  (FDataLink.DataSource <> nil)
    and (FDataLink.DataSource.DataSet <> nil)
    and (FDataLink.DataSet.Active)
    and ( (FDataLink.DataSource.Owner is TDBCatalog)
          or not (FDataLink.DataSet.EOF and FDataLink.DataSet.BOF)
          or (FDataLink.DataSet.State = dsInsert)
        )
    and IsParentValid(FDataLink.DataSource.Owner)
    then begin
           if Active and (not FEmpty)
              then
                begin
                  DoAfterMasterChange;
                  FItemCount := GetItemCount;
                  First;
                end
              else
                begin
                  DoAfterMasterChange;
                  Close;
                  FEmpty := FALSE;
                  Open;
                end;
         end else
               begin
                 if  (FDataLink.DataSource <> nil)
                 and (FDataLink.DataSource.DataSet <> nil)
                 then begin
                        Close;
//                        FEmpty := TRUE;
  //                      if not Active then Open;
                      end
                 else begin
                        Close;
                      end;
               end;
  finally
    FOperationAllowed := FALSE;
  end;
end;

procedure TADOXDataSet.SetDS(Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, Self);
  FDataLink.DataSource := Value;
end;

procedure TADOXDataSet.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin

end;

{:-- TCollectionDataLink }

procedure TCollectionDataLink.ActiveChanged;
begin
  FDataSet.RefreshDetail;
end;

procedure TCollectionDataLink.CheckBrowseMode;
begin
  if FDataSet.Active then FDataSet.CheckBrowseMode;
end;

constructor TCollectionDataLink.Create(ADataSet: TADOXDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

function TCollectionDataLink.GetDetailDataSet: TDataSet;
begin
  Result := FDataSet;
end;

procedure TCollectionDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) then FDataSet.RefreshDetail;
end;

{:-- TDBCatalog }

procedure TDBCatalog.Close;
begin
  SetActive(FALSE);
end;

constructor TDBCatalog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FADOXCatalog := coCatalog.Create;
  FActive := FALSE;
  FConnection := nil;
  {$WARNINGS OFF}
  FDataSet := TCollectionDataSet.Create(Self);
  {$WARNINGS ON}
  FDataSource := TDataSource.Create(Self);
  FDataSource.DataSet := FDataSet;
end;

procedure TDBCatalog.CreateNew(const ConnectionString: WideString);
begin
  FDataSet.Close;
  FActive := FALSE;
  FADOXCatalog.Create(ConnectionString);
  FActive := TRUE;
  FDataSet.Open
end;

destructor TDBCatalog.Destroy;
begin
  FDataSource.DataSet := nil;
  FDataSet.Free;
  FDataSource.Free;
  FADOXCatalog := nil;
  inherited Destroy;
end;

procedure TDBCatalog.Loaded;
begin
  inherited;
  if FActive then
     try
       SetActive(TRUE);
     except
       if csDesigning in ComponentState
          then Application.HandleException(Self)
          else raise;
     end;
end;

procedure TDBCatalog.Open;
begin
  SetActive(TRUE);
end;

procedure TDBCatalog.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
     begin
       FActive := Value;
       Exit;
     end;
  if Value then
     begin
       if FConnection = nil
          then DatabaseError(SNoConnection, Self)
          else if not FConnection.Active then FConnection.Open;
       FADOXCatalog._Set_ActiveConnection(FConnection.ConnectionObject)
     end
       else FADOXCatalog.Set_ActiveConnection(nil);
  FActive := Value;
  if Value
     then FDataSet.Open
     else FDataSet.Close;
end;

procedure TDBCatalog.SetConnection(Connection: TConnection);
begin
  if not (csLoading in ComponentState) then SetActive(FALSE);
  if FConnection <> nil then FConnection.RemoveReference(Self);
  if Connection <> nil then Connection.AddReference(Self);
  FConnection := Connection;
end;

{:-- TDBTables }

constructor TDBTables.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBTables.AppendItem;
begin
  GetCatalog.FADOXCatalog.Tables.Append(FADOXObject);
end;

procedure TDBTables.DeleteItem(Index: Integer);
begin
  GetCatalog.FADOXCatalog.Tables.Delete(Index);
end;

function TDBTables.GetCatalog: TDBCatalog;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TDBCatalog(MasterSource.Owner);
end;

function TDBTables.GetItemCount: Integer;
var FTables: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetCatalog.FADOXCatalog, 'Tables', FTables);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FTables, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FTables := NULL;
    Count := NULL;
  end;
end;

function TDBTables.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  case FieldNo of
    0 : Result := GetCatalog.FADOXCatalog.Tables[Item];
    1 : Result := IADOXTable(ADOXObject).Name;
    2 : Result := IADOXTable(ADOXObject).Type_;
    3 : Result := IADOXTable(ADOXObject).DateCreated;
    4 : Result := IADOXTable(ADOXObject).DateModified;
  end;
end;

procedure TDBTables.InitItemFields;
var FFieldDef: TFieldDef;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 0);
  FFieldDef := TFieldDef.Create(FieldDefs, 'TYPE', ftString, 20, TRUE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
  FFieldDef := TFieldDef.Create(FieldDefs, 'DATE_CREATED', ftDateTime, 0, FALSE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
  FFieldDef := TFieldDef.Create(FieldDefs, 'DATE_MODIFIED', ftDateTime, 0, FALSE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
end;

procedure TDBTables.InternalInsert;
begin
  FADOXObject := coTable.Create;
  IADOXTable(IDispatch(FADOXObject)).ParentCatalog := GetCatalog.FADOXCatalog;
  inherited;
end;

procedure TDBTables.InternalRefresh;
begin
  GetCatalog.FADOXCatalog.Tables.Refresh;
  inherited;
end;

procedure TDBTables.SetCatalog(const Value: TDBCatalog);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

procedure TDBTables.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin
  case FieldNo of
    1: IADOXTable(ADOXObject).Name := Value;
  end;
end;

function TDBTables.GetTableObject: IADOXTable;
begin
  Result := IADOXTable(GetDispADOXObject);
end;

{:-- TADOXCommandDataSet }

procedure TADOXCommandDataSet.DataEvent(Event: TDataEvent; Info: Integer);
var FCommand: Variant;
    FParameter: IADOParameter;
    n: Integer;
begin
  if (   (Event = deDataSetChange)
      or (Event = deDataSetScroll)
      or (Event = deRecordChange)
      )
  and (State <> dsInactive)
  and (State <> dsInsert)
  and (State <> dsEdit)
  and (State <> dsBlockRead)
  and not (EOF and BOF)
  and (not VarIsNull(ADOXObject))
  then
      begin
        DispGetPropValue(ADOXObject, 'Command', FCommand);
        if not VarIsNull(FCommand) then
           begin
             FADOCommand := coCommand.Create;
             DispInvokeMethod(FCommand.Parameters, 'Refresh');
             for n := 0 to FCommand.Parameters.Count - 1 do
                 with IADOParameter(IDispatch(FCommand.Parameters [n])) do
                      begin
                        FParameter := coParameter.Create;
                        FParameter.Set_Name(Name);
                        FParameter.Set_Type_(Type_);
                        FParameter.Set_Direction(Direction);
                        FParameter.Set_Precision(Precision);
                        FParameter.Set_NumericScale(NumericScale);
                        FParameter.Set_Size(Size);
                        FParameter.Set_Attributes(Attributes);
                        FADOCommand.Parameters.Append(FParameter);
                        FParameter := nil;
                      end;
           end else FADOCommand := nil;
      end;
  FCommand := Null;
  inherited DataEvent(Event, Info);
end;

procedure TADOXCommandDataSet.DoAfterOpen;
begin
  First;
  inherited;
end;

procedure TADOXCommandDataSet.InternalInsert;
begin
  FADOCommand := nil;
  FADOCommand := coCommand.Create;
  inherited;
end;

{:-- TDBViews }

constructor TDBViews.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBViews.AppendItem;
var ViewName, ViewCommand: Variant;
    Field: TField;
begin
  ViewName := '';
  ViewCommand := '';
  Field := Fields.FindField('NAME');
  if (Field <> nil)
  and (Field.FieldKind = fkData)
  then ViewName := TRecData(pointer(ActiveBuffer)^)[Field.Index].Data;
  Field := Fields.FindField('COMMAND');
  if (Field <> nil)
  and (Field.FieldKind = fkData)
  then ViewCommand := TRecData(pointer(ActiveBuffer)^)[Field.Index].Data;
  try
    FADOCommand.CommandText := ViewCommand;
    GetCatalog.FADOXCatalog.Views.Append(ViewName, FADOCommand);
  finally
  end;
end;

procedure TDBViews.DeleteItem(Index: Integer);
begin
  GetCatalog.FADOXCatalog.Views.Delete(Index);
end;

function TDBViews.GetCatalog: TDBCatalog;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TDBCatalog(MasterSource.Owner);
end;

function TDBViews.GetItemCount: Integer;
var FViews: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetCatalog.FADOXCatalog, 'Views', FViews);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FViews, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FViews := NULL;
    Count := NULL;
  end;
end;

function TDBViews.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
var FCommand: Variant;
begin
  if (FieldNo > 0) and (ADOXObject = nil) then Exit;
  case FieldNo of
    0 : Result := GetCatalog.FADOXCatalog.Views[Item];
    1 : Result := IADOXView(ADOXObject).Name;
    2 : begin
          DispGetPropValue(ADOXObject, 'Command', FCommand);
          if not VarIsNull(FCommand)
             then Result := FCommand.CommandText
             else Result := Null;
          FCommand := Null;
        end;
    3 : Result := IADOXView(ADOXObject).DateCreated;
    4 : Result := IADOXView(ADOXObject).DateModified;
  end;
end;

procedure TDBViews.InitItemFields;
var FFieldDef: TFieldDef;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 0);
  TFieldDef.Create(FieldDefs, 'COMMAND', ftMemo, 0, TRUE, 0);
  FFieldDef := TFieldDef.Create(FieldDefs, 'DATE_CREATED', ftDateTime, 0, FALSE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
  FFieldDef := TFieldDef.Create(FieldDefs, 'DATE_MODIFIED', ftDateTime, 0, FALSE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
end;

procedure TDBViews.InternalInsert;
begin
  FADOXObject := Null;
  inherited;
end;

procedure TDBViews.InternalRefresh;
begin
  GetCatalog.FADOXCatalog.Views.Refresh;
  inherited;
end;

procedure TDBViews.SetCatalog(const Value: TDBCatalog);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

procedure TDBViews.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin
  if State = dsInsert then Exit;
  if FieldNo = 2 then
     begin
       FADOCommand.CommandText := Value;
       IADOXProcedure(ADOXObject).Set_Command(FADOCommand);
     end;
end;

function TDBViews.GetViewObject: IADOXView;
begin
  Result := IADOXView(GetDispADOXObject);
end;

{:-- TDBProcedures }

constructor TDBProcedures.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBProcedures.AppendItem;
var ProcName, ProcCommand: Variant;
    Field: TField;
begin
  try
    ProcName := '';
    ProcCommand := '';
    Field := Fields.FindField('NAME');
    if (Field <> nil)
    and (Field.FieldKind = fkData)
    then ProcName := TRecData(pointer(ActiveBuffer)^)[Field.Index].Data;
    Field := Fields.FindField('COMMAND');
    if (Field <> nil)
    and (Field.FieldKind = fkData)
    then ProcCommand := TRecData(pointer(ActiveBuffer)^)[Field.Index].Data;
    FADOCommand.CommandText := ProcCommand;
    GetCatalog.FADOXCatalog.Procedures.Append(ProcName, FADOCommand);
  finally
  end;
end;

procedure TDBProcedures.DeleteItem(Index: Integer);
begin
  GetCatalog.FADOXCatalog.Procedures.Delete(Index);
end;

function TDBProcedures.GetCatalog: TDBCatalog;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TDBCatalog(MasterSource.Owner);
end;

function TDBProcedures.GetItemCount: Integer;
var FProcedures: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetCatalog.FADOXCatalog, 'Procedures', FProcedures);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FProcedures, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FProcedures := NULL;
    Count := NULL;
  end;
end;

function TDBProcedures.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
var FCommand: Variant;
begin
  if (FieldNo > 0) and (ADOXObject = nil) then Exit;
  case FieldNo of
    0 : Result := GetCatalog.FADOXCatalog.Procedures[Item];
    1 : Result := IADOXProcedure(ADOXObject).Name;
    2 : begin
          DispGetPropValue(ADOXObject, 'Command', FCommand);
          if not VarIsNull(FCommand)
             then Result := FCommand.CommandText
             else Result := Null;
          FCommand := Null;
        end;
    3 : Result := IADOXProcedure(ADOXObject).DateCreated;
    4 : Result := IADOXProcedure(ADOXObject).DateModified;
  end;
end;

procedure TDBProcedures.InitItemFields;
var FFieldDef: TFieldDef;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 0);
  TFieldDef.Create(FieldDefs, 'COMMAND', ftMemo, 0, TRUE, 0);
  FFieldDef := TFieldDef.Create(FieldDefs, 'DATE_CREATED', ftDateTime, 0, FALSE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
  FFieldDef := TFieldDef.Create(FieldDefs, 'DATE_MODIFIED', ftDateTime, 0, FALSE, 0);
  FFieldDef.Attributes := FFieldDef.Attributes + [faReadOnly];
end;

procedure TDBProcedures.InternalInsert;
begin
  FADOXObject := Null;
  inherited;
end;

procedure TDBProcedures.InternalRefresh;
begin
  GetCatalog.FADOXCatalog.Procedures.Refresh;
  inherited;
end;

procedure TDBProcedures.SetCatalog(const Value: TDBCatalog);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

procedure TDBProcedures.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin
  if State = dsInsert then Exit;
  if FieldNo = 2 then
     begin
       FADOCommand.CommandText := Value;
       IADOXProcedure(ADOXObject).Set_Command(FADOCommand);
     end;
end;

function TDBProcedures.GetProcedureObject: IADOXProcedure;
begin
  Result := IADOXProcedure(GetDispADOXObject);
end;

{:-- TDBParameters }

procedure TDBParameters.AppendItem;
begin
  if FADOCommand = nil then
     DatabaseError(SMasterSourceInterfaceIsNull, Self);
  if (GetParent.State <> dsEdit) and (GetParent.State <> dsInsert) then
     DatabaseError(SMasterNotEditing, Self);
  FADOCommand.Parameters.Append(FADOXObject);
end;

constructor TDBParameters.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBParameters.DeleteItem(Index: Integer);
begin
  if FADOCommand = nil then
     DatabaseError(SMasterSourceInterfaceIsNull, Self);
  if (GetParent.State <> dsEdit) and (GetParent.State <> dsInsert) then
     DatabaseError(SMasterNotEditing, Self);
  FADOCommand.Parameters.Delete(Index);
end;

procedure TDBParameters.DoAfterMasterChange;
begin
  FADOCommand := GetParent.FADOCommand;
end;

procedure TDBParameters.DoBeforeEdit;
begin
  if FADOCommand = nil then
     DatabaseError(SMasterSourceInterfaceIsNull, Self);
  if (GetParent.State <> dsEdit) and (GetParent.State <> dsInsert) then
     DatabaseError(SMasterNotEditing, Self);
  inherited;
end;

procedure TDBParameters.DoBeforeInsert;
begin
  if FADOCommand = nil then
     DatabaseError(SMasterSourceInterfaceIsNull, Self);
  if (GetParent.State <> dsEdit) and (GetParent.State <> dsInsert) then
     DatabaseError(SMasterNotEditing, Self);
  inherited;
end;

function TDBParameters.GetFieldValue(ADOXObject: IDispatch; Item,
  FieldNo: Integer): Variant;
begin
  case FieldNo of
    0 : Result := FADOCommand.Parameters[Item];
    1 : Result := IADOParameter(ADOXObject).Name;
    2 : case IADOParameter(ADOXObject).Type_ of
          adEmpty: Result := 'Empty';
          adTinyInt: Result := 'TinyInt';
          adSmallInt: Result := 'SmallInt';
          adInteger: Result := 'Integer';
          adBigInt: Result := 'BigInt';
          adUnsignedTinyInt: Result := 'UnsignedTinyInt';
          adUnsignedSmallInt: Result := 'UnsignedSmallInt';
          adUnsignedInt: Result := 'UnsignedInt';
          adUnsignedBigInt: Result := 'UnsignedBigInt';
          adSingle: Result := 'Single';
          adDouble: Result := 'Double';
          adCurrency: Result := 'Currency';
          adDecimal: Result := 'Decimal';
          adNumeric: Result := 'Numeric';
          adBoolean: Result := 'Boolean';
          adError: Result := 'Error';
          adUserDefined: Result := 'UserDefined';
          adVariant: Result := 'Variant';
          adIDispatch: Result := 'IDispatch';
          adIUnknown: Result := 'IUnknown';
          adGUID: Result := 'GUID';
          adDate: Result := 'Date';
          adDBDate: Result := 'DBDate';
          adDBTime: Result := 'DBTime';
          adDBTimeStamp: Result := 'DBTimeStamp';
          adBSTR: Result := 'BSTR';
          adChar: Result := 'Char';
          adVarChar: Result := 'VarChar';
          adLongVarChar: Result := 'LongVarChar';
          adWChar: Result := 'WideChar';
          adVarWChar: Result := 'VarWideChar';
          adLongVarWChar: Result := 'LongVarWideChar';
          adBinary: Result := 'Binary';
          adVarBinary: Result := 'VarBinary';
          adLongVarBinary: Result := 'LongVarBinary';
          adChapter: Result := 'Chapter';
          adFileTime: Result := 'FileTime';
          adPropVariant: Result := 'PropVariant';
          adVarNumeric: Result := 'VarNumeric';
          else Result := 'Unknown';
        end;
    3 : case IADOParameter(ADOXObject).Direction of
          adParamInput: Result := 'In';
          adParamOutput: Result := 'Out';
          adParamInputOutput: Result := 'InOut';
          adParamReturnValue: Result := 'Result';
          adParamUnknown: Result := 'Unknown';
        end;
    4 : Result := IADOParameter(ADOXObject).Size;
    5 : if IADOParameter(ADOXObject).Attributes and adParamSigned = adParamSigned
           then Result := TRUE
           else Result := FALSE;
    6 : if IADOParameter(ADOXObject).Attributes and adParamNullable = adParamNullable
           then Result := TRUE
           else Result := FALSE;
    7 : if IADOParameter(ADOXObject).Attributes and adParamLong = adParamLong
           then Result := TRUE
           else Result := FALSE;
    8 : Result := IADOParameter(ADOXObject).Precision;
    9 : Result := IADOParameter(ADOXObject).NumericScale;
  end;
end;

function TDBParameters.GetItemCount: Integer;
var Count: Variant;
    hr: HRESULT;
begin
  if FADOCommand = nil then
     begin
       Result := 0;
       Exit;
     end;
  hr := DispGetPropValue(FADOCommand.Parameters, 'Count', Count);
  if hr <> S_OK
     then Result := 0
     else Result := Count;
end;

function TDBParameters.GetParameterObject: IADOParameter;
begin
  if FADOCommand = nil then
     DatabaseError(SMasterSourceInterfaceIsNull, Self);
  Result := IADOParameter(GetDispADOXObject);
end;

function TDBParameters.GetParent: TADOXCommandDataSet;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TADOXCommandDataSet(MasterSource.Owner);
end;

procedure TDBParameters.InitItemFields;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'TYPE', ftString, 20, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'DIRECTION', ftString, 10, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'SIZE', ftInteger, 0, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'SIGNED', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'NULLABLE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'LONG', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'PRECISION', ftInteger, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'NUMERIC_SCALE', ftSmallInt, 0, FALSE, 1);
end;

procedure TDBParameters.InternalInsert;
begin
  FADOXObject := coParameter.Create;
  inherited;
end;

procedure TDBParameters.InternalRefresh;
begin
  DoAfterMasterChange;
  inherited;
end;

procedure TDBParameters.SetFieldValue(ADOXObject: IDispatch;
  FieldNo: Integer; Value: Variant);
var ParamType: string;
begin
  case FieldNo of
    1 : IADOParameter(ADOXObject).Name := Value;
    2 : begin
          ParamType := Trim(LowerCase(Value));
          if ParamType = 'empty' then
             IADOParameter(ADOXObject).Type_ := adEmpty;
          if ParamType = 'tinyint' then
             IADOParameter(ADOXObject).Type_ := adTinyInt;
          if ParamType = 'smallint' then
             IADOParameter(ADOXObject).Type_ := adSmallInt;
          if ParamType = 'integer' then
             IADOParameter(ADOXObject).Type_ := adInteger;
          if ParamType = 'bigint' then
             IADOParameter(ADOXObject).Type_ := adBigInt;
          if ParamType = 'unsignedtinyint' then
             IADOParameter(ADOXObject).Type_ := adUnsignedTinyInt;
          if ParamType = 'unsignedsmallint' then
             IADOParameter(ADOXObject).Type_ := adUnsignedSmallInt;
          if ParamType = 'unsignedint' then
             IADOParameter(ADOXObject).Type_ := adUnsignedInt;
          if ParamType = 'unsignedbigint' then
             IADOParameter(ADOXObject).Type_ := adUnsignedBigInt;
          if ParamType = 'single' then
             IADOParameter(ADOXObject).Type_ := adSingle;
          if ParamType = 'double' then
             IADOParameter(ADOXObject).Type_ := adDouble;
          if ParamType = 'currency' then
             IADOParameter(ADOXObject).Type_ := adCurrency;
          if ParamType = 'decimal' then
             IADOParameter(ADOXObject).Type_ := adDecimal;
          if ParamType = 'numeric' then
             IADOParameter(ADOXObject).Type_ := adNumeric;
          if ParamType = 'boolean' then
             IADOParameter(ADOXObject).Type_ := adBoolean;
          if ParamType = 'error' then
             IADOParameter(ADOXObject).Type_ := adError;
          if ParamType = 'userdefined' then
             IADOParameter(ADOXObject).Type_ := adUserDefined;
          if ParamType = 'variant' then
             IADOParameter(ADOXObject).Type_ := adVariant;
          if ParamType = 'idispatch' then
             IADOParameter(ADOXObject).Type_ := adIDispatch;
          if ParamType = 'iunknown' then
             IADOParameter(ADOXObject).Type_ := adIUnknown;
          if ParamType = 'guid' then IADOParameter(ADOXObject).Type_ := adGUID;
          if ParamType = 'date' then IADOParameter(ADOXObject).Type_ := adDate;
          if ParamType = 'dbdate' then
             IADOParameter(ADOXObject).Type_ := adDBDate;
          if ParamType = 'dbtime' then
             IADOParameter(ADOXObject).Type_ := adDBTime;
          if ParamType = 'dbtimestamp' then
             IADOParameter(ADOXObject).Type_ := adDBTimeStamp;
          if ParamType = 'bstr' then IADOParameter(ADOXObject).Type_ := adBSTR;
          if ParamType = 'char' then IADOParameter(ADOXObject).Type_ := adChar;
          if ParamType = 'varchar' then
             IADOParameter(ADOXObject).Type_ := adVarChar;
          if ParamType = 'longvarchar' then
             IADOParameter(ADOXObject).Type_ := adLongVarChar;
          if ParamType = 'widechar' then
             IADOParameter(ADOXObject).Type_ := adWChar;
          if ParamType = 'varwidechar' then
             IADOParameter(ADOXObject).Type_ := adVarWChar;
          if ParamType = 'longvarwidechar' then
             IADOParameter(ADOXObject).Type_ := adLongVarWChar;
          if ParamType = 'binary' then
             IADOParameter(ADOXObject).Type_ := adBinary;
          if ParamType = 'varbinary' then
             IADOParameter(ADOXObject).Type_ := adVarBinary;
          if ParamType = 'longvarbinary' then
             IADOParameter(ADOXObject).Type_ := adLongVarBinary;
          if ParamType = 'chapter' then
             IADOParameter(ADOXObject).Type_ := adChapter;
          if ParamType = 'filetime' then
             IADOParameter(ADOXObject).Type_ := adFileTime;
          if ParamType = 'propvariant' then
             IADOParameter(ADOXObject).Type_ := adPropVariant;
          if ParamType = 'varnumeric' then
             IADOParameter(ADOXObject).Type_ := adVarNumeric;
        end;
    3 : begin
          ParamType := Trim(LowerCase(Value));
          if ParamType = 'in' then
             IADOParameter(ADOXObject).Direction := adParamInput;
          if ParamType = 'out' then
             IADOParameter(ADOXObject).Direction := adParamOutput;
          if ParamType = 'inout' then
             IADOParameter(ADOXObject).Direction := adParamInputOutput;
          if ParamType = 'result' then
             IADOParameter(ADOXObject).Direction := adParamReturnValue;
          if ParamType = 'unknown' then
             IADOParameter(ADOXObject).Direction := adParamUnknown;
        end;
    4 : IADOParameter(ADOXObject).Size := Value;
    5 : if WordBool(Value)
           then IADOParameter(ADOXObject).Attributes :=
                IADOParameter(ADOXObject).Attributes or adParamSigned
           else IADOParameter(ADOXObject).Attributes :=
                IADOParameter(ADOXObject).Attributes and not adParamSigned;
    6 : if WordBool(Value)
           then IADOParameter(ADOXObject).Attributes :=
                IADOParameter(ADOXObject).Attributes or adParamNullable
           else IADOParameter(ADOXObject).Attributes :=
                IADOParameter(ADOXObject).Attributes and not adParamNullable;
    7 : if WordBool(Value)
           then IADOParameter(ADOXObject).Attributes :=
                IADOParameter(ADOXObject).Attributes or adParamLong
           else IADOParameter(ADOXObject).Attributes :=
                IADOParameter(ADOXObject).Attributes and not adParamLong;
    8 : IADOParameter(ADOXObject).Precision := Value;
    9 : IADOParameter(ADOXObject).NumericScale := Value;
  end;
end;

procedure TDBParameters.SetParent(const Value: TADOXCommandDataSet);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

function TDBParameters.ValidateFieldData(Field: TField;
  var Value: Variant): Boolean;
var vStr: string;
begin
  Result := TRUE;
  if (Field.FieldNo = 2) then
     begin
       Result := FALSE;
       vStr := LowerCase(Trim(VarToStr(Value)));
       if vStr = 'empty' then Result := TRUE;
       if vStr = 'tinyint' then Result := TRUE;
       if vStr = 'smallint' then Result := TRUE;
       if vStr = 'integer' then Result := TRUE;
       if vStr = 'bigint' then Result := TRUE;
       if vStr = 'unsignedtinyint' then Result := TRUE;
       if vStr = 'unsignedsmallint' then Result := TRUE;
       if vStr = 'unsignedint' then Result := TRUE;
       if vStr = 'unsignedbigint' then Result := TRUE;
       if vStr = 'single' then Result := TRUE;
       if vStr = 'double' then Result := TRUE;
       if vStr = 'currency' then Result := TRUE;
       if vStr = 'decimal' then Result := TRUE;
       if vStr = 'numeric' then Result := TRUE;
       if vStr = 'boolean' then Result := TRUE;
       if vStr = 'error' then Result := TRUE;
       if vStr = 'userdefined' then Result := TRUE;
       if vStr = 'variant' then Result := TRUE;
       if vStr = 'idispatch' then Result := TRUE;
       if vStr = 'iunknown' then Result := TRUE;
       if vStr = 'guid' then Result := TRUE;
       if vStr = 'date' then Result := TRUE;
       if vStr = 'dbdate' then Result := TRUE;
       if vStr = 'dbtime' then Result := TRUE;
       if vStr = 'dbtimestamp' then Result := TRUE;
       if vStr = 'bstr' then Result := TRUE;
       if vStr = 'char' then Result := TRUE;
       if vStr = 'varchar' then Result := TRUE;
       if vStr = 'longvarchar' then Result := TRUE;
       if vStr = 'widechar' then Result := TRUE;
       if vStr = 'varwidechar' then Result := TRUE;
       if vStr = 'longvarwidechar' then Result := TRUE;
       if vStr = 'binary' then Result := TRUE;
       if vStr = 'varbinary' then Result := TRUE;
       if vStr = 'longvarbinary' then Result := TRUE;
       if vStr = 'chapter' then Result := TRUE;
       if vStr = 'filetime' then Result := TRUE;
       if vStr = 'propvariant' then Result := TRUE;
       if vStr = 'varnumeric' then Result := TRUE;
     end;
  if (Field.FieldNo = 3) then
     begin
       vStr := LowerCase(Trim(VarToStr(Value)));
       if  (vStr <> 'in')
       and (vStr <> 'out')
       and (vStr <> 'inout')
       and (vStr <> 'result')
       and (vStr <> 'unknown')
       then Result := FALSE;
     end;
end;

{:-- TDBIndexes }

constructor TDBIndexes.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBIndexes.AppendItem;
begin
  GetTables.GetADOXObject.Indexes.Append(FADOXObject);
end;

procedure TDBIndexes.DeleteItem(Index: Integer);
begin
  GetTables.GetADOXObject.Indexes.Delete(Index);
end;

function TDBIndexes.GetItemCount: Integer;
var FIndexes: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetTables.GetADOXObject, 'Indexes', FIndexes);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FIndexes, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FIndexes := NULL;
    Count := NULL;
  end;
end;

function TDBIndexes.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  case FieldNo of
    0: Result := GetTables.GetADOXObject.Indexes[Item];
    1: Result := IADOXIndex(ADOXObject).Name;
    2: Result := IADOXIndex(ADOXObject).Unique;
    3: Result := IADOXIndex(ADOXObject).PrimaryKey;
    4: case IADOXIndex(ADOXObject).IndexNulls of
         adIndexNullsAllow: Result := 'Allow';
         adIndexNullsDisallow: Result := 'Disallow';
         adIndexNullsIgnore: Result := 'Ignore';
         adIndexNullsIgnoreAny: Result := 'IgnoreAny';
       end;
    5: Result := IADOXIndex(ADOXObject).Clustered;
  end;
end;

function TDBIndexes.GetTables: TDBTables;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TDBTables(MasterSource.Owner);
end;

procedure TDBIndexes.InitItemFields;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 30, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'UNIQUE', ftBoolean, 0, FALSE, 2);
  TFieldDef.Create(FieldDefs, 'PRIMARY_KEY', ftBoolean, 0, FALSE, 3);
  TFieldDef.Create(FieldDefs, 'INDEX_NULLS', ftString, 10, FALSE, 4);
  TFieldDef.Create(FieldDefs, 'CLUSTERED', ftBoolean, 0, FALSE, 5);
end;

procedure TDBIndexes.InternalInsert;
begin
  FADOXObject := coIndex.Create;
  inherited;
end;

procedure TDBIndexes.InternalRefresh;
begin
  GetTables.GetADOXObject.Indexes.Refresh;
  inherited;
end;

procedure TDBIndexes.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
var IndexNulls: string;
begin
  case FieldNo of
    1 : IADOXIndex(ADOXObject).Name := Value;
    2 : IADOXIndex(ADOXObject).Unique := Value;
    3 : IADOXIndex(ADOXObject).PrimaryKey := Value;
    4 : begin
          IndexNulls := Trim(LowerCase(Value));
          if IndexNulls = 'allow' then IADOXIndex(ADOXObject).IndexNulls := adIndexNullsAllow;
          if IndexNulls = 'disallow' then IADOXIndex(ADOXObject).IndexNulls := adIndexNullsDisallow;
          if IndexNulls = 'ignore' then IADOXIndex(ADOXObject).IndexNulls := adIndexNullsIgnore;
          if IndexNulls = 'ignoreany' then IADOXIndex(ADOXObject).IndexNulls := adIndexNullsIgnoreAny;
        end;
    5 : IADOXIndex(ADOXObject).Clustered := Value;
  end;
end;

procedure TDBIndexes.SetTables(const Value: TDBTables);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

function TDBIndexes.ValidateFieldData(Field: TField;
  var Value: Variant): Boolean;
var vStr: string;
begin
  Result := TRUE;
  if Field.FieldNo = 4 then
     if VarIsNull(Value)
        then Result := FALSE
          else
            begin
              vStr := LowerCase(Copy(VarToStr(Value), 1, 1));
              if vStr = ''
                 then Result := FALSE
                 else
                   case vStr [1] of
                     'a': Value := 'Allow';
                     'd': Value := 'Disallow';
                     'i': if Pos('a', LowerCase(VarToStr(Value))) = 0
                             then Value := 'Ignore'
                             else Value := 'IgnoreAny';
                     else Result := FALSE;
                   end;
            end;
end;

function TDBIndexes.GetIndexObject: IADOXIndex;
begin
  Result := IADOXIndex(GetDispADOXObject);
end;

{:-- TDBKeys }

constructor TDBKeys.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBKeys.AppendItem;
begin
  GetTables.GetADOXObject.Keys.Append(FADOXObject);
end;

procedure TDBKeys.DeleteItem(Index: Integer);
begin
  GetTables.GetADOXObject.Keys.Delete(Index);
end;

function TDBKeys.GetItemCount: Integer;
var FKeys: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetTables.GetADOXObject, 'Keys', FKeys);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FKeys, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FKeys := NULL;
    Count := NULL;
  end;
end;

function TDBKeys.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  case FieldNo of
    0 : Result := GetTables.GetADOXObject.Keys[Item];
    1 : Result := IADOXKey(ADOXObject).Name;
    2 : case IADOXKey(ADOXObject).Type_ of
          adKeyPrimary: Result := 'Primary';
          adKeyForeign: Result := 'Foreign';
          adKeyUnique: Result := 'Unique';
        else Result := 'Unknown';
        end;
    3 : Result := IADOXKey(ADOXObject).RelatedTable;
    4 : case IADOXKey(ADOXObject).UpdateRule of
          adRINone: Result := 'None';
          adRICascade: Result := 'Cascade';
          adRISetNull: Result := 'SetNull';
          adRISetDefault: Result := 'SetDefault';
        end;
    5 : case IADOXKey(ADOXObject).DeleteRule of
          adRINone: Result := 'None';
          adRICascade: Result := 'Cascade';
          adRISetNull: Result := 'SetNull';
          adRISetDefault: Result := 'SetDefault';
        end;
    end;
end;

function TDBKeys.GetTables: TDBTables;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TDBTables(MasterSource.Owner);
end;

procedure TDBKeys.InitItemFields;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'TYPE', ftString, 10, TRUE, 1);
  TFieldDef.Create(FieldDefs, 'RELATED_TABLE', ftString, 30, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'UPDATE_RULE', ftString, 15, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'DELETE_RULE', ftString, 15, FALSE, 1);
end;

procedure TDBKeys.InternalInsert;
begin
  FADOXObject := coKey.Create;
  inherited;
end;

procedure TDBKeys.InternalRefresh;
begin
  GetTables.GetADOXObject.Keys.Refresh;
  inherited;
end;

procedure TDBKeys.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
var FStrProp: string;
begin
  case FieldNo of
    1 : IADOXKey(ADOXObject).Name := Value;
    2 : begin
          FStrProp := Trim(LowerCase(Value));
          if Copy(FStrProp, 1, 1) = 'p' then IADOXKey(ADOXObject).Type_ := adKeyPrimary;
          if Copy(FStrProp, 1, 1) = 'f' then IADOXKey(ADOXObject).Type_ := adKeyForeign;
          if Copy(FStrProp, 1, 1) = 'u' then IADOXKey(ADOXObject).Type_ := adKeyUnique;
        end;
    3 : IADOXKey(ADOXObject).RelatedTable := Value;
    4 : begin
          FStrProp := Trim(LowerCase(Value));
          if Copy(FStrProp, 1, 1) = 'n' then IADOXKey(ADOXObject).UpdateRule := adRINone;
          if Copy(FStrProp, 1, 1) = 'c' then IADOXKey(ADOXObject).UpdateRule := adRICascade;
          if Copy(FStrProp, 1, 4) = 'setn' then IADOXKey(ADOXObject).UpdateRule := adRISetNull;
          if Copy(FStrProp, 1, 4) = 'setd' then IADOXKey(ADOXObject).UpdateRule := adRISetDefault;
        end;
    5 : begin
          FStrProp := Trim(LowerCase(Value));
          if Copy(FStrProp, 1, 1) = 'n' then IADOXKey(ADOXObject).DeleteRule := adRINone;
          if Copy(FStrProp, 1, 1) = 'c' then IADOXKey(ADOXObject).DeleteRule := adRICascade;
          if Copy(FStrProp, 1, 4) = 'setn' then IADOXKey(ADOXObject).DeleteRule := adRISetNull;
          if Copy(FStrProp, 1, 4) = 'setd' then IADOXKey(ADOXObject).DeleteRule := adRISetDefault;
        end;
  end;
end;

procedure TDBKeys.SetTables(const Value: TDBTables);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

function TDBKeys.ValidateFieldData(Field: TField;
  var Value: Variant): Boolean;
var vStr: string;
begin
  Result := TRUE;
  if Field.FieldNo = 2 then
     if VarIsNull(Value)
        then Result := FALSE
          else
            begin
              vStr := LowerCase(Copy(VarToStr(Value), 1, 1));
              if vStr = ''
                 then Result := FALSE
                 else
                   case vStr [1] of
                     'p': Value := 'Primary';
                     'f': Value := 'Foreign';
                     'u': Value := 'Unique';
                     else Result := FALSE;
                   end;
            end;
  if Result and (Field.FieldNo >= 4) and (Field.FieldNo <= 5) then
     if VarIsNull(Value)
        then Result := FALSE
          else
            begin
              vStr := LowerCase(Copy(VarToStr(Value), 1, 1));
              if vStr = ''
                 then Result := FALSE
                 else
                   case vStr [1] of
                     'n': Value := 'None';
                     'c': Value := 'Cascade';
                     's': if Pos('d', LowerCase(VarToStr(Value))) = 0
                             then Value := 'SetNull'
                             else Value := 'SetDefault';
                     else Result := FALSE;
                   end;
            end;
end;

function TDBKeys.GetKeyObject: IADOXKey;
begin
  Result := IADOXKey(GetDispADOXObject);
end;

{:-- TDBColumns }

constructor TDBColumns.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBColumns.AppendItem;
begin
  GetParent.GetADOXObject.Columns.Append(FADOXObject);
end;

procedure TDBColumns.DeleteItem(Index: Integer);
begin
  GetParent.GetADOXObject.Columns.Delete(Index);
end;

function TDBColumns.GetParent: TADOXColumnedDataSet;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TADOXColumnedDataSet(MasterSource.Owner);
end;

function TDBColumns.GetItemCount: Integer;
var FColumns: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetParent.GetADOXObject, 'Columns', FColumns);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FColumns, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FColumns := NULL;
    Count := NULL;
  end;
end;

function TDBColumns.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  case FieldNo of
    0 : Result := GetParent.GetADOXObject.Columns[Item];
    1 : Result := IADOXColumn(ADOXObject).Name;
  end;
  if GetParent is TDBTables then
     case FieldNo of
       2 : case IADOXColumn(ADOXObject).Type_ of
             adEmpty: Result := 'Empty';
             adTinyInt: Result := 'TinyInt';
             adSmallInt: Result := 'SmallInt';
             adInteger: Result := 'Integer';
             adBigInt: Result := 'BigInt';
             adUnsignedTinyInt: Result := 'UnsignedTinyInt';
             adUnsignedSmallInt: Result := 'UnsignedSmallInt';
             adUnsignedInt: Result := 'UnsignedInt';
             adUnsignedBigInt: Result := 'UnsignedBigInt';
             adSingle: Result := 'Single';
             adDouble: Result := 'Double';
             adCurrency: Result := 'Currency';
             adDecimal: Result := 'Decimal';
             adNumeric: Result := 'Numeric';
             adBoolean: Result := 'Boolean';
             adError: Result := 'Error';
             adUserDefined: Result := 'UserDefined';
             adVariant: Result := 'Variant';
             adIDispatch: Result := 'IDispatch';
             adIUnknown: Result := 'IUnknown';
             adGUID: Result := 'GUID';
             adDate: Result := 'Date';
             adDBDate: Result := 'DBDate';
             adDBTime: Result := 'DBTime';
             adDBTimeStamp: Result := 'DBTimeStamp';
             adBSTR: Result := 'BSTR';
             adChar: Result := 'Char';
             adVarChar: Result := 'VarChar';
             adLongVarChar: Result := 'LongVarChar';
             adWChar: Result := 'WideChar';
             adVarWChar: Result := 'VarWideChar';
             adLongVarWChar: Result := 'LongVarWideChar';
             adBinary: Result := 'Binary';
             adVarBinary: Result := 'VarBinary';
             adLongVarBinary: Result := 'LongVarBinary';
             adChapter: Result := 'Chapter';
             adFileTime: Result := 'FileTime';
             adPropVariant: Result := 'PropVariant';
             adVarNumeric: Result := 'VarNumeric';
             else Result := 'Unknown';
           end;
       3 : if IADOXColumn(ADOXObject).Attributes and adColFixed = adColFixed
              then Result := TRUE
              else Result := FALSE;
       4 : if IADOXColumn(ADOXObject).Attributes and adColNullable = adColNullable
              then Result := TRUE
              else Result := FALSE;
       5 : Result := IADOXColumn(ADOXObject).DefinedSize;
       6 : Result := IADOXColumn(ADOXObject).NumericScale;
       7 : Result := IADOXColumn(ADOXObject).Precision;
     end;
  if GetParent is TDBIndexes then
     case FieldNo of
       2 : case IADOXColumn(ADOXObject).SortOrder of
             adSortAscending: Result := 'Ascending';
             adSortDescending: Result := 'Descending';
             else Result := '';
           end;
     end;
  if GetParent is TDBKeys then
     case FieldNo of
       2 : Result := IADOXColumn(ADOXObject).RelatedColumn;
     end;
end;

procedure TDBColumns.InitItemFields;
var FColumnedStructure: TADOXColumnedDataSet;
begin
  FColumnedStructure := GetParent;
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 1);
  if FColumnedStructure is TDBTables then
     begin
       TFieldDef.Create(FieldDefs, 'TYPE', ftString, 20, TRUE, 1);
       TFieldDef.Create(FieldDefs, 'FIXED_LENGTH', ftBoolean, 0, FALSE, 1);
       TFieldDef.Create(FieldDefs, 'NULLABLE', ftBoolean, 0, FALSE, 1);
       TFieldDef.Create(FieldDefs, 'SIZE', ftInteger, 0, FALSE, 1);
       TFieldDef.Create(FieldDefs, 'NUMERIC_SCALE', ftSmallInt, 0, FALSE, 1);
       TFieldDef.Create(FieldDefs, 'PRECISION', ftInteger, 0, FALSE, 1);
     end;
  if FColumnedStructure is TDBIndexes
     then TFieldDef.Create(FieldDefs, 'SORT_ORDER', ftString, 10, FALSE, 1);
  if FColumnedStructure is TDBKeys
     then TFieldDef.Create(FieldDefs, 'RELATED_COLUMN', ftString, 50, FALSE, 1);
end;

procedure TDBColumns.InternalInsert;
var FColumnedStructure: TADOXColumnedDataSet;
    FMaster: TComponent;
    FMasterExists: Boolean;
    FDBCatalog: TDBCatalog;
begin
  FColumnedStructure := GetParent;
  FADOXObject := coColumn.Create;
  FMasterExists := TRUE;
  FMaster := FColumnedStructure;
  FDBCatalog := nil;
  while FMasterExists do
        begin
          FMasterExists := FALSE;
          if FMaster is TDBCatalog
             then FDBCatalog := TDBCatalog(FMaster);
          if FMaster is TDBTables then
             begin
               FMaster := TDBTables(FMaster).Catalog;
               FMasterExists := TRUE;
             end;
          if FMaster is TDBIndexes then
             begin
               FMaster := TDBIndexes(FMaster).Tables;
               FMasterExists := TRUE;
             end;
          if FMaster is TDBKeys then
             begin
               FMaster := TDBKeys(FMaster).Tables;
               FMasterExists := TRUE;
             end;
        end;
  FADOXObject.ParentCatalog := FDBCatalog.FADOXCatalog;
  inherited;
end;

procedure TDBColumns.InternalRefresh;
begin
  GetParent.GetADOXObject.Columns.Refresh;
  inherited;
end;

procedure TDBColumns.SetParent(const Value: TADOXColumnedDataSet);
begin
  if Value = nil
     then MasterSource := nil
     else MasterSource := Value.FDataSource;
end;

procedure TDBColumns.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
var ColumnType: string;
    SortOrder: string;
    FColumnedStructure: TADOXColumnedDataSet;
begin
  FColumnedStructure := GetParent;
  if FColumnedStructure is TDBTables then
     case FieldNo of
       1 : IADOXColumn(ADOXObject).Name := Value;
       2 : begin
             ColumnType := Trim(LowerCase(Value));
             if ColumnType = 'empty' then IADOXColumn(ADOXObject).Type_ := adEmpty;
             if ColumnType = 'tinyint' then IADOXColumn(ADOXObject).Type_ := adTinyInt;
             if ColumnType = 'smallint' then IADOXColumn(ADOXObject).Type_ := adSmallInt;
             if ColumnType = 'integer' then IADOXColumn(ADOXObject).Type_ := adInteger;
             if ColumnType = 'bigint' then IADOXColumn(ADOXObject).Type_ := adBigInt;
             if ColumnType = 'unsignedtinyint' then IADOXColumn(ADOXObject).Type_ := adUnsignedTinyInt;
             if ColumnType = 'unsignedsmallint' then IADOXColumn(ADOXObject).Type_ := adUnsignedSmallInt;
             if ColumnType = 'unsignedint' then IADOXColumn(ADOXObject).Type_ := adUnsignedInt;
             if ColumnType = 'unsignedbigint' then IADOXColumn(ADOXObject).Type_ := adUnsignedBigInt;
             if ColumnType = 'single' then IADOXColumn(ADOXObject).Type_ := adSingle;
             if ColumnType = 'double' then IADOXColumn(ADOXObject).Type_ := adDouble;
             if ColumnType = 'currency' then IADOXColumn(ADOXObject).Type_ := adCurrency;
             if ColumnType = 'decimal' then IADOXColumn(ADOXObject).Type_ := adDecimal;
             if ColumnType = 'numeric' then IADOXColumn(ADOXObject).Type_ := adNumeric;
             if ColumnType = 'boolean' then IADOXColumn(ADOXObject).Type_ := adBoolean;
             if ColumnType = 'error' then IADOXColumn(ADOXObject).Type_ := adError;
             if ColumnType = 'userdefined' then IADOXColumn(ADOXObject).Type_ := adUserDefined;
             if ColumnType = 'variant' then IADOXColumn(ADOXObject).Type_ := adVariant;
             if ColumnType = 'idispatch' then IADOXColumn(ADOXObject).Type_ := adIDispatch;
             if ColumnType = 'iunknown' then IADOXColumn(ADOXObject).Type_ := adIUnknown;
             if ColumnType = 'guid' then IADOXColumn(ADOXObject).Type_ := adGUID;
             if ColumnType = 'date' then IADOXColumn(ADOXObject).Type_ := adDate;
             if ColumnType = 'dbdate' then IADOXColumn(ADOXObject).Type_ := adDBDate;
             if ColumnType = 'dbtime' then IADOXColumn(ADOXObject).Type_ := adDBTime;
             if ColumnType = 'dbtimestamp' then IADOXColumn(ADOXObject).Type_ := adDBTimeStamp;
             if ColumnType = 'bstr' then IADOXColumn(ADOXObject).Type_ := adBSTR;
             if ColumnType = 'char' then IADOXColumn(ADOXObject).Type_ := adChar;
             if ColumnType = 'varchar' then IADOXColumn(ADOXObject).Type_ := adVarChar;
             if ColumnType = 'longvarchar' then IADOXColumn(ADOXObject).Type_ := adLongVarChar;
             if ColumnType = 'widechar' then IADOXColumn(ADOXObject).Type_ := adWChar;
             if ColumnType = 'varwidechar' then IADOXColumn(ADOXObject).Type_ := adVarWChar;
             if ColumnType = 'longvarwidechar' then IADOXColumn(ADOXObject).Type_ := adLongVarWChar;
             if ColumnType = 'binary' then IADOXColumn(ADOXObject).Type_ := adBinary;
             if ColumnType = 'varbinary' then IADOXColumn(ADOXObject).Type_ := adVarBinary;
             if ColumnType = 'longvarbinary' then IADOXColumn(ADOXObject).Type_ := adLongVarBinary;
             if ColumnType = 'chapter' then IADOXColumn(ADOXObject).Type_ := adChapter;
             if ColumnType = 'filetime' then IADOXColumn(ADOXObject).Type_ := adFileTime;
             if ColumnType = 'propvariant' then IADOXColumn(ADOXObject).Type_ := adPropVariant;
             if ColumnType = 'varnumeric' then IADOXColumn(ADOXObject).Type_ := adVarNumeric;
           end;
       3 : if Value = TRUE
              then IADOXColumn(ADOXObject).Attributes := IADOXColumn(ADOXObject).Attributes or adColFixed
              else IADOXColumn(ADOXObject).Attributes := IADOXColumn(ADOXObject).Attributes and not adColFixed;
       4 : if Value = TRUE
              then IADOXColumn(ADOXObject).Attributes := IADOXColumn(ADOXObject).Attributes or adColNullable
              else IADOXColumn(ADOXObject).Attributes := IADOXColumn(ADOXObject).Attributes and not adColNullable;
       5 : IADOXColumn(ADOXObject).DefinedSize := Value;
       6 : IADOXColumn(ADOXObject).NumericScale := Value;
       7 : IADOXColumn(ADOXObject).Precision := Value;
     end;
  if FColumnedStructure is TDBIndexes then
     case FieldNo of
       1 : IADOXColumn(ADOXObject).Name := Value;
       2 : begin
             SortOrder := Trim(LowerCase(Value));
             if SortOrder = 'ascending' then
                IADOXColumn(ADOXObject).SortOrder := adSortAscending;
             if SortOrder = 'descending' then
                IADOXColumn(ADOXObject).SortOrder := adSortDescending;
           end;
     end;
  if FColumnedStructure is TDBKeys then
     case FieldNo of
       1 : IADOXColumn(ADOXObject).Name := Value;
       2 : IADOXColumn(ADOXObject).RelatedColumn := Value;
     end;
end;

function TDBColumns.ValidateFieldData(Field: TField;
  var Value: Variant): Boolean;
var vStr: string;
    FColumnedStructure: TADOXColumnedDataSet;
begin
  Result := TRUE;
  FColumnedStructure := GetParent;
  if (FColumnedStructure is TDBIndexes) and (Field.FieldNo = 2) then
     if VarIsNull(Value)
        then Result := FALSE
        else
          begin
            vStr := LowerCase(Copy(Trim(VarToStr(Value)), 1, 1));
            if vStr = 'a'
               then Value := 'Ascending'
               else if vStr = 'd'
                       then Value := 'Descending'
                       else Result := FALSE;
          end;
  if (FColumnedStructure is TDBTables) and (Field.FieldNo = 2) then
     begin
       Result := FALSE;
       vStr := LowerCase(Trim(VarToStr(Value)));
       if vStr = 'empty' then Result := TRUE;
       if vStr = 'tinyint' then Result := TRUE;
       if vStr = 'smallint' then Result := TRUE;
       if vStr = 'integer' then Result := TRUE;
       if vStr = 'bigint' then Result := TRUE;
       if vStr = 'unsignedtinyint' then Result := TRUE;
       if vStr = 'unsignedsmallint' then Result := TRUE;
       if vStr = 'unsignedint' then Result := TRUE;
       if vStr = 'unsignedbigint' then Result := TRUE;
       if vStr = 'single' then Result := TRUE;
       if vStr = 'double' then Result := TRUE;
       if vStr = 'currency' then Result := TRUE;
       if vStr = 'decimal' then Result := TRUE;
       if vStr = 'numeric' then Result := TRUE;
       if vStr = 'boolean' then Result := TRUE;
       if vStr = 'error' then Result := TRUE;
       if vStr = 'userdefined' then Result := TRUE;
       if vStr = 'variant' then Result := TRUE;
       if vStr = 'idispatch' then Result := TRUE;
       if vStr = 'iunknown' then Result := TRUE;
       if vStr = 'guid' then Result := TRUE;
       if vStr = 'date' then Result := TRUE;
       if vStr = 'dbdate' then Result := TRUE;
       if vStr = 'dbtime' then Result := TRUE;
       if vStr = 'dbtimestamp' then Result := TRUE;
       if vStr = 'bstr' then Result := TRUE;
       if vStr = 'char' then Result := TRUE;
       if vStr = 'varchar' then Result := TRUE;
       if vStr = 'longvarchar' then Result := TRUE;
       if vStr = 'widechar' then Result := TRUE;
       if vStr = 'varwidechar' then Result := TRUE;
       if vStr = 'longvarwidechar' then Result := TRUE;
       if vStr = 'binary' then Result := TRUE;
       if vStr = 'varbinary' then Result := TRUE;
       if vStr = 'longvarbinary' then Result := TRUE;
       if vStr = 'chapter' then Result := TRUE;
       if vStr = 'filetime' then Result := TRUE;
       if vStr = 'propvariant' then Result := TRUE;
       if vStr = 'varnumeric' then Result := TRUE;
     end;
end;

function TDBColumns.GetColumnObject: IADOXColumn;
begin
  Result := IADOXColumn(GetDispADOXObject);
end;

{:-- TDBGroups }

constructor TDBGroups.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBGroups.AppendItem;
var FCatalogOrUsers: TComponent;
begin
  FCatalogOrUsers := GetCatalogOrUsers;
  if FCatalogOrUsers is TDBCatalog
     then TDBCatalog(FCatalogOrUsers).FADOXCatalog.Groups.Append(FADOXObject)
     else TADOXDataSet(FCatalogOrUsers).GetADOXObject.Groups.Append(FADOXObject)
end;

procedure TDBGroups.DeleteItem(Index: Integer);
var FCatalogOrUsers: TComponent;
begin
  FCatalogOrUsers := GetCatalogOrUsers;
  if FCatalogOrUsers is TDBCatalog
     then TDBCatalog(FCatalogOrUsers).FADOXCatalog.Groups.Delete(Index)
     else TADOXDataSet(FCatalogOrUsers).GetADOXObject.Groups.Delete(Index);
end;

function TDBGroups.GetCatalogOrUsers: TComponent;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TComponent(MasterSource.Owner);
end;

function TDBGroups.GetItemCount: Integer;
var FCatalogOrUsers: TComponent;
    FGroups: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  FCatalogOrUsers := GetCatalogOrUsers;
  if FCatalogOrUsers is TDBCatalog
     then hr := DispGetPropValue(TDBCatalog(FCatalogOrUsers).FADOXCatalog,
                  'Groups', FGroups)
     else hr := DispGetPropValue(TADOXDataSet(FCatalogOrUsers).GetADOXObject,
                  'Groups', FGroups);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FGroups, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FGroups := NULL;
    Count := NULL;
  end;
end;

function TDBGroups.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  if GetCatalogOrUsers is TDBCatalog then
     case FieldNo of
       0 : Result := TDBCatalog(GetCatalogOrUsers).FADOXCatalog.Groups[Item];
       1 : Result := IADOXGroup(ADOXObject).Name;
     end
       else
         case FieldNo of
           0 : Result := TADOXDataSet(GetCatalogOrUsers).GetADOXObject.Groups[Item];
           1 : Result := IADOXGroup(ADOXObject).Name;
         end;
end;

function TDBGroups.GetGroupObject: IADOXGroup;
begin
  Result := IADOXGroup(GetDispADOXObject);
end;

procedure TDBGroups.InitItemFields;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 1);
end;

procedure TDBGroups.InternalInsert;
{$IFDEF ADOX26}
var FMaster: TComponent;
    FMasterExists: Boolean;
    FDBCatalog: TDBCatalog;
{$ENDIF}
begin
  FADOXObject := coGroup.Create;
  {$IFDEF ADOX26}
  FMasterExists := TRUE;
  FMaster := GetCatalogOrUsers;
  FDBCatalog := nil;
  while FMasterExists do
        begin
          FMasterExists := FALSE;
          if FMaster is TDBCatalog
             then FDBCatalog := TDBCatalog(FMaster);
          if FMaster is TDBUsers then
             begin
               FMaster := TDBUsers(FMaster).CatalogOrGroups;
               FMasterExists := TRUE;
             end;
          if FMaster is TDBGroups then
             begin
               FMaster := TDBGroups(FMaster).CatalogOrUsers;
               FMasterExists := TRUE;
             end;
        end;
  FADOXObject.ParentCatalog := FDBCatalog.FADOXCatalog;
  {$ENDIF}
  inherited;
end;

procedure TDBGroups.InternalRefresh;
var FCatalogOrUsers: TComponent;
begin
  FCatalogOrUsers := GetCatalogOrUsers;
  if FCatalogOrUsers is TDBCatalog
     then TDBCatalog(FCatalogOrUsers).FADOXCatalog.Groups.Refresh
     else TADOXDataSet(FCatalogOrUsers).GetADOXObject.Groups.Refresh;
  inherited;     
end;

procedure TDBGroups.SetCatalogOrUsers(const Value: TComponent);
begin
  if Value = nil
     then MasterSource := nil
     else if Value is TDBCatalog
             then MasterSource := TDBCatalog(Value).FDataSource
             else if not (Value is TDBUsers)
                     then raise Exception.Create(SInvalidPropertyValue)
                     else if TDBUsers(Value).MasterSource = FDataSource
                             then DatabaseError(SCircularDataLink, Self)
                             else MasterSource := TDBUsers(Value).FDataSource;
end;

procedure TDBGroups.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin
  if FieldNo = 1 then IADOXGroup(ADOXObject).Name := Value;
end;

{:-- TDBUsers }

constructor TDBUsers.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TDBUsers.AppendItem;
var FCatalogOrGroups: TComponent;
begin
  FCatalogOrGroups := GetCatalogOrGroups;
  if FCatalogOrGroups is TDBCatalog
     then Variant(TDBCatalog(FCatalogOrGroups).FADOXCatalog).Users.Append(FADOXObject)
     else TADOXDataSet(FCatalogOrGroups).GetADOXObject.Users.Append(FADOXObject);
end;

procedure TDBUsers.DeleteItem(Index: Integer);
var FCatalogOrGroups: TComponent;
begin
  FCatalogOrGroups := GetCatalogOrGroups;
  if FCatalogOrGroups is TDBCatalog
     then TDBCatalog(FCatalogOrGroups).FADOXCatalog.Users.Delete(Index)
     else TADOXDataSet(FCatalogOrGroups).GetADOXObject.Users.Delete(Index);
end;

procedure TDBUsers.ChangePassword(const OldPassword: WideString;
  const NewPassword: WideString);
begin
  GetUserObject.ChangePassword(OldPassword, NewPassword);
end;

function TDBUsers.GetCatalogOrGroups: TComponent;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TComponent(MasterSource.Owner);
end;

function TDBUsers.GetItemCount: Integer;
var FCatalogOrGroups: TComponent;
    FUsers: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  FCatalogOrGroups := GetCatalogOrGroups;
  if FCatalogOrGroups is TDBCatalog
     then hr := DispGetPropValue(TDBCatalog(FCatalogOrGroups).FADOXCatalog,
                  'Users', FUsers)
     else hr := DispGetPropValue(TADOXDataSet(FCatalogOrGroups).GetADOXObject,
                  'Users', FUsers);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FUsers, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FUsers := NULL;
    Count := NULL;
  end;
end;

function TDBUsers.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  if GetCatalogOrGroups is TDBCatalog then
     case FieldNo of
       0 : Result := TDBCatalog(GetCatalogOrGroups).FADOXCatalog.Users[Item];
       1 : Result := IADOXUser(ADOXObject).Name;
     end
       else
         case FieldNo of
           0 : Result := TADOXDataSet(GetCatalogOrGroups).GetADOXObject.Users[Item];
           1 : Result := IADOXUser(ADOXObject).Name;
         end;
end;

function TDBUsers.GetUserObject: IADOXUser;
begin
  Result := IADOXUser(GetDispADOXObject);
end;

procedure TDBUsers.InitItemFields;
begin
  TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, TRUE, 1);
end;

procedure TDBUsers.InternalInsert;
{$IFDEF ADOX26}
var FMaster: TComponent;
    FMasterExists: Boolean;
    FDBCatalog: TDBCatalog;
{$ENDIF}
begin
  FADOXObject := coUser.Create;
  {$IFDEF ADOX26}
  FMasterExists := TRUE;
  FMaster := GetCatalogOrGroups;
  FDBCatalog := nil;
  while FMasterExists do
        begin
          FMasterExists := FALSE;
          if FMaster is TDBCatalog
             then FDBCatalog := TDBCatalog(FMaster);
          if FMaster is TDBUsers then
             begin
               FMaster := TDBUsers(FMaster).CatalogOrGroups;
               FMasterExists := TRUE;
             end;
          if FMaster is TDBGroups then
             begin
               FMaster := TDBGroups(FMaster).CatalogOrUsers;
               FMasterExists := TRUE;
             end;
        end;
  FADOXObject.ParentCatalog := FDBCatalog.FADOXCatalog;
  {$ENDIF}
  inherited;
end;

procedure TDBUsers.InternalRefresh;
var FCatalogOrGroups: TComponent;
begin
  FCatalogOrGroups := GetCatalogOrGroups;
  if FCatalogOrGroups is TDBCatalog
     then TDBCatalog(FCatalogOrGroups).FADOXCatalog.Users.Refresh
     else TADOXDataSet(FCatalogOrGroups).GetADOXObject.Users.Refresh;
  inherited;     
end;

procedure TDBUsers.SetCatalogOrGroups(const Value: TComponent);
begin
  if Value = nil
     then MasterSource := nil
     else if Value is TDBCatalog
             then MasterSource := TDBCatalog(Value).FDataSource
             else if not (Value is TDBGroups)
                     then raise Exception.Create(SInvalidPropertyValue)
                          else if TDBGroups(Value).MasterSource = FDataSource
                                  then DatabaseError(SCircularDataLink, Self)
                                  else MasterSource := TDBGroups(Value).FDataSource;
end;

procedure TDBUsers.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin
  if FieldNo = 1 then IADOXUser(ADOXObject).Name := Value;
end;

{:-- TDBPermissions }

constructor TDBPermissions.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCatalog := nil;
  FSchemaObjects := [soTables, soViews, soProcedures];
end;

procedure TDBPermissions.DeleteItem(Index: Integer);
begin
end;

function TDBPermissions.GetItemCount: Integer;
var FCollection: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  Result := 0;
  if FCatalog = nil then Exit;
  if GetUsersOrGroups.State = dsInsert then Exit;
  try
    if soTables in FSchemaObjects then
       begin
         hr := DispGetPropValue(FCatalog.FADOXCatalog, 'Tables', FCollection);
         if hr = S_OK then
            begin
              hr := DispGetPropValue(FCollection, 'Count', Count);
              if hr = S_OK then Inc(Result, Integer(Count));
            end;
       end;
    if soViews in FSchemaObjects then
       begin
         hr := DispGetPropValue(FCatalog.FADOXCatalog, 'Views', FCollection);
         if hr = S_OK then
            begin
              hr := DispGetPropValue(FCollection, 'Count', Count);
              if hr = S_OK then Inc(Result, Integer(Count));
            end;
       end;
    if soProcedures in FSchemaObjects then
       begin
         hr := DispGetPropValue(FCatalog.FADOXCatalog, 'Procedures', FCollection);
         if hr = S_OK then
            begin
              hr := DispGetPropValue(FCollection, 'Count', Count);
              if hr = S_OK then Inc(Result, Integer(Count));
            end;
       end;
  finally
    FCollection := NULL;
    Count := NULL;
  end;
end;

function TDBPermissions.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
var FSchemaType: Integer;
    FIndexInCollection: Integer;
    i, j: Integer;
    FPermissions: DWORD;
    FObjName: WideString;
    FObjType: WideString;
    FUsersOrGroups: TADOXSecurityCollection;
    FCollection: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  FUsersOrGroups := GetUsersOrGroups;
  if FCatalog = nil then Exit;
  FSchemaType := adPermObjTable;
  FIndexInCollection := Item;
  j := 0;
  if (soTables in FSchemaObjects) then
     begin
       i := 0;
       hr := DispGetPropValue(FCatalog.FADOXCatalog, 'Tables', FCollection);
       if hr = S_OK then
          begin
            hr := DispGetPropValue(FCollection, 'Count', Count);
            if hr = S_OK then i := Integer(Count);
          end;
       if FIndexInCollection > i - 1 then
          begin
            FSchemaType := adPermObjView;
            Dec(FIndexInCollection, i);
          end else j := i;
     end;
  if (soViews in FSchemaObjects) then
     begin
       i := 0;
       hr := DispGetPropValue(FCatalog.FADOXCatalog, 'Views', FCollection);
       if hr = S_OK then
          begin
            hr := DispGetPropValue(FCollection, 'Count', Count);
            if hr = S_OK then i := Integer(Count);
          end;
       if FIndexInCollection > j + i - 1 then
          begin
            FSchemaType := adPermObjProcedure;
            Dec(FIndexInCollection, i);
          end;
     end;
  FPermissions := 0;
  case FSchemaType of
    adPermObjTable:
          begin
            FObjName := FCatalog.FADOXCatalog.Tables[FIndexInCollection].Name;
            FPermissions := FUsersOrGroups.GetADOXObject.GetPermissions
                               (FObjName, adPermObjTable);
            FObjType := 'TABLE';
          end;
    adPermObjView:
          begin
            FObjName := FCatalog.FADOXCatalog.Views[FIndexInCollection].Name;
            FPermissions := FUsersOrGroups.GetADOXObject.GetPermissions
                               (FObjName, adPermObjView);
            FObjType := 'VIEW';
          end;
    adPermObjProcedure:
          begin
            FObjName := FCatalog.FADOXCatalog.Procedures[FIndexInCollection].Name;
            FPermissions := FUsersOrGroups.GetADOXObject.GetPermissions
                               (FObjName, adPermObjProcedure);
            FObjType := 'PROCEDURE';
          end;
  end;
  case FieldNo of
    1 : Result := FObjName;
    2 : Result := FObjType;
    {adRightNone}
    3 : if FPermissions = adRightNone
           then Result := TRUE
           else Result := FALSE;
    {adRightFull}
    4 : if FPermissions and adRightFull = adRightFull
           then Result := TRUE
           else Result := FALSE;
    {adRightRead}
    5 : if FPermissions and adRightRead = adRightRead
           then Result := TRUE
           else Result := FALSE;
    {adRightInsert}
    6 : if FPermissions and adRightInsert = adRightInsert
           then Result := TRUE
           else Result := FALSE;
    {adRightUpdate}
    7 : if FPermissions and adRightUpdate = adRightUpdate
           then Result := TRUE
           else Result := FALSE;
    {adRightDelete}
    8 : if FPermissions and adRightDelete = adRightDelete
           then Result := TRUE
           else Result := FALSE;
    {adRightExecute}
    9 : if FPermissions and adRightExecute = adRightExecute
           then Result := TRUE
           else Result := FALSE;
    {adRightCreate}
    10 : if FPermissions and adRightCreate = adRightCreate
            then Result := TRUE
            else Result := FALSE;
    {adRightDrop}
    11 : if FPermissions and adRightDrop = adRightDrop
            then Result := TRUE
            else Result := FALSE;
    {adRightReadDesign}
    12 : if FPermissions and adRightReadDesign = adRightReadDesign
            then Result := TRUE
            else Result := FALSE;
    {adRightWriteDesign}
    13 : if FPermissions and adRightWriteDesign = adRightWriteDesign
            then Result := TRUE
            else Result := FALSE;
    {adRightReadPermissions}
    14 : if FPermissions and adRightReadPermissions = adRightReadPermissions
            then Result := TRUE
            else Result := FALSE;
    {adRightWritePermissions}
    15 : if FPermissions and adRightWritePermissions = adRightWritePermissions
            then Result := TRUE
            else Result := FALSE;
    {adRightWriteOwner}
    16 : if FPermissions and adRightWriteOwner = adRightWriteOwner
            then Result := TRUE
            else Result := FALSE;
    {adRightExclusive}
    17 : if FPermissions and adRightExclusive = adRightExclusive
            then Result := TRUE
            else Result := FALSE;
    {adRightWithGrant}
    18 : if FPermissions and adRightWithGrant = adRightWithGrant
            then Result := TRUE
            else Result := FALSE;
    {adRightReference}
    19 : if FPermissions and adRightReference = adRightReference
            then Result := TRUE
            else Result := FALSE;
    {adRightMaximumAllowed}
    20 : if FPermissions and adRightMaximumAllowed = adRightMaximumAllowed
            then Result := TRUE
            else Result := FALSE;
  end;
end;

function TDBPermissions.GetUsersOrGroups: TADOXSecurityCollection;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TADOXSecurityCollection(MasterSource.Owner);
end;

procedure TDBPermissions.InitItemFields;
var FieldDef: TFieldDef;
begin
  FieldDef := TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  FieldDef := TFieldDef.Create(FieldDefs, 'TYPE', ftString, 15, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  TFieldDef.Create(FieldDefs, 'NONE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'FULL', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'READ', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'INSERT', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'UPDATE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'DELETE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'EXECUTE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'CREATE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'DROP', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'READ_DESIGN', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'WRITE_DESIGN', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'READ_PERMISSIONS', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'WRITE_PERMISSIONS', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'WRITE_OWNER', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'EXCLUSIVE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'GRANT', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'REFERENCE', ftBoolean, 0, FALSE, 1);
  TFieldDef.Create(FieldDefs, 'MAXIMUM_ALLOWED', ftBoolean, 0, FALSE, 1);
end;

procedure TDBPermissions.InternalInsert;
begin
end;

procedure TDBPermissions.InternalPost;
var i, n: Integer;
    ObjName: WideString;
    ObjType: DWORD;
    FUsersOrGroups: TADOXSecurityCollection;
    RevokedRights: DWORD;
    GrantedRights: DWORD;
    RevokeAll: Boolean;
begin
  if State = dsInsert then Exit;
  ObjName := '';
  ObjType := 0;
  RevokeAll := FALSE;
  RevokedRights := 0;
  GrantedRights := 0;
  FUsersOrGroups := GetUsersOrGroups;
  if FCatalog = nil then Exit;
  for i := 0 to Fields.Count - 1 do
    if Fields [i].FieldKind = fkData then
      begin
        n := Fields [i].FieldNo;
        case n of
          1 : ObjName := TRecData(pointer(ActiveBuffer)^)[i].Data;
          2 : if LowerCase(Trim(TRecData(pointer(ActiveBuffer)^)[i].Data)) = 'table'
               then ObjType := adPermObjTable
               else if LowerCase(Trim(TRecData(pointer(ActiveBuffer)^)[i].Data)) = 'view'
                       then ObjType := adPermObjView
                       else ObjType := adPermObjProcedure;
          {adRightNone}
          3 : if TRecData(pointer(ActiveBuffer)^)[i].Modified
              and (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
              then RevokeAll := TRUE;
          {adRightFull}
          4 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
              if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                  then GrantedRights := GrantedRights or adRightFull
                  else RevokedRights := RevokedRights or adRightFull;
          {adRightRead}
          5 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
              if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                  then GrantedRights := GrantedRights or adRightRead
                  else RevokedRights := RevokedRights or adRightRead;
          {adRightInsert}
          6 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
              if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                  then GrantedRights := GrantedRights or adRightInsert
                  else RevokedRights := RevokedRights or adRightInsert;
          {adRightUpdate}
          7 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
              if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                  then GrantedRights := GrantedRights or adRightUpdate
                  else RevokedRights := RevokedRights or adRightUpdate;
          {adRightDelete}
          8 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
              if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                  then GrantedRights := GrantedRights or adRightDelete
                  else RevokedRights := RevokedRights or adRightDelete;
          {adRightExecute}
          9 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
              if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                  then GrantedRights := GrantedRights or adRightExecute
                  else RevokedRights := RevokedRights or adRightExecute;
          {adRightCreate}
          10 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightCreate
                   else RevokedRights := RevokedRights or adRightCreate;
          {adRightDrop}
          11 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightDrop
                   else RevokedRights := RevokedRights or adRightDrop;
          {adRightReadDesign}
          12 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightReadDesign
                   else RevokedRights := RevokedRights or adRightReadDesign;
          {adRightWriteDesign}
          13 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightWriteDesign
                   else RevokedRights := RevokedRights or adRightWriteDesign;
          {adRightReadPermissions}
          14 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightReadPermissions
                   else RevokedRights := RevokedRights or adRightReadPermissions;
          {adRightWritePermissions}
          15 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightWritePermissions
                   else RevokedRights := RevokedRights or adRightWritePermissions;
          {adRightWriteOwner}
          16 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightWriteOwner
                   else RevokedRights := RevokedRights or adRightWriteOwner;
          {adRightExclusive}
          17 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightExclusive
                   else RevokedRights := RevokedRights or adRightExclusive;
          {adRightWithGrant}
          18 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightWithGrant
                   else RevokedRights := RevokedRights or adRightWithGrant;
          {adRightReference}
          19 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightReference
                   else RevokedRights := RevokedRights or adRightReference;
          {adRightMaximumAllowed}
          20 : if TRecData(pointer(ActiveBuffer)^)[i].Modified then
               if (WordBool(TRecData(pointer(ActiveBuffer)^)[i].Data) = TRUE)
                   then GrantedRights := GrantedRights or adRightMaximumAllowed
                   else RevokedRights := RevokedRights or adRightMaximumAllowed;
        end;  
      end;
  if RevokeAll then
     FUsersOrGroups.GetADOXObject.SetPermissions(ObjName, ObjType,
                                      adAccessRevoke, 0);
  if (RevokedRights = adRightFull) and (GrantedRights <> 0) then
     FUsersOrGroups.GetADOXObject.SetPermissions(ObjName, ObjType,
                                      adAccessDeny, RevokedRights);
  if GrantedRights <> 0 then
     FUsersOrGroups.GetADOXObject.SetPermissions(ObjName, ObjType,
                                      adAccessGrant, GrantedRights);
  if (RevokedRights = adRightFull) and (GrantedRights <> 0) then Exit;
  if (GrantedRights = adRightFull) then Exit;
  if RevokedRights <> 0 then
     FUsersOrGroups.GetADOXObject.SetPermissions(ObjName, ObjType,
                                      adAccessDeny, RevokedRights);
end;

procedure TDBPermissions.InternalRefresh;
begin
  if FCatalog = nil then Exit;
  if soTables in FSchemaObjects then FCatalog.FADOXCatalog.Tables.Refresh;
  if soViews in FSchemaObjects then FCatalog.FADOXCatalog.Views.Refresh;
  if soProcedures in FSchemaObjects then FCatalog.FADOXCatalog.Procedures.Refresh;
  inherited;
end;

procedure TDBPermissions.SetUsersOrGroups(const Value: TADOXSecurityCollection);
var FMasterExists: Boolean;
    FMaster: TComponent;
begin
  if Value <> nil then
     begin
       FMasterExists := TRUE;
       FMaster := Value;
       while FMasterExists do
             begin
               FMasterExists := FALSE;
               if FMaster is TDBCatalog
                  then FCatalog := TDBCatalog(FMaster);
               if FMaster is TDBUsers then
                  begin
                    FMaster := TDBUsers(FMaster).CatalogOrGroups;
                    if FMaster <> nil then FMasterExists := TRUE;
                  end;
               if FMaster is TDBGroups then
                  begin
                    FMaster := TDBGroups(FMaster).CatalogOrUsers;
                    if FMaster <> nil then FMasterExists := TRUE;
                  end;
             end;
     end;
  if Value = nil
     then
         begin
           MasterSource := nil;
           FCatalog := nil;
         end
     else MasterSource := TADOXSecurityCollection(Value).FDataSource;
end;

function TDBPermissions.ValidateFieldData(Field: TField;
  var Value: Variant): Boolean;
var n: Integer;
begin
  Result := TRUE;
  if (Field.FieldNo > 2) and (WordBool(Value) = TRUE) then
     case Field.FieldNo of
       3: for n := 0 to Fields.Count - 1 do
              if Fields [n].FieldNo > 3 then
                 Fields [n].AsBoolean := FALSE;
       4: for n := 0 to Fields.Count - 1 do
              if (Fields [n].FieldNo = 3) or (Fields [n].FieldNo > 4) then
                 Fields [n].AsBoolean := FALSE;
       5..20: for n := 0 to Fields.Count - 1 do
                  if (Fields [n].FieldNo = 3) or (Fields [n].FieldNo = 4) then
                     Fields [n].AsBoolean := FALSE;
     end;
end;

{:-- TDBProperties }

procedure TDBProperties.DeleteItem(Index: Integer);
begin

end;

procedure TDBProperties.DoBeforeInsert;
begin
  DatabaseError(SCannotAddNewProperties, Self);
end;

function TDBProperties.GetItemCount: Integer;
var FProperties: Variant;
    hr: HRESULT;
    Count: Variant;
begin
  hr := DispGetPropValue(GetParent.GetADOXObject, 'Properties', FProperties);
  try
    if hr <> S_OK
       then Result := 0
       else
         begin
           hr := DispGetPropValue(FProperties, 'Count', Count);
           if hr <> S_OK
              then Result := 0
              else Result := Count;
         end;
  finally
    FProperties := NULL;
    Count := NULL;
  end;
end;

function TDBProperties.GetFieldValue(ADOXObject: IDispatch; Item: Integer;
  FieldNo: Integer): Variant;
begin
  case FieldNo of
    0 : Result := GetParent.GetADOXObject.Properties [Item];
    1 : Result := IADOProperty(ADOXObject).Get_Name;
    2 : case IADOProperty(ADOXObject).Get_Type_ of
          adEmpty: Result := 'Empty';
          adTinyInt: Result := 'TinyInt';
          adSmallInt: Result := 'SmallInt';
          adInteger: Result := 'Integer';
          adBigInt: Result := 'BigInt';
          adUnsignedTinyInt: Result := 'UnsignedTinyInt';
          adUnsignedSmallInt: Result := 'UnsignedSmallInt';
          adUnsignedInt: Result := 'UnsignedInt';
          adUnsignedBigInt: Result := 'UnsignedBigInt';
          adSingle: Result := 'Single';
          adDouble: Result := 'Double';
          adCurrency: Result := 'Currency';
          adDecimal: Result := 'Decimal';
          adNumeric: Result := 'Numeric';
          adBoolean: Result := 'Boolean';
          adError: Result := 'Error';
          adUserDefined: Result := 'UserDefined';
          adVariant: Result := 'Variant';
          adIDispatch: Result := 'IDispatch';
          adIUnknown: Result := 'IUnknown';
          adGUID: Result := 'GUID';
          adDate: Result := 'Date';
          adDBDate: Result := 'DBDate';
          adDBTime: Result := 'DBTime';
          adDBTimeStamp: Result := 'DBTimeStamp';
          adBSTR: Result := 'BSTR';
          adChar: Result := 'Char';
          adVarChar: Result := 'VarChar';
          adLongVarChar: Result := 'LongVarChar';
          adWChar: Result := 'WideChar';
          adVarWChar: Result := 'VarWideChar';
          adLongVarWChar: Result := 'LongVarWideChar';
          adBinary: Result := 'Binary';
          adVarBinary: Result := 'VarBinary';
          adLongVarBinary: Result := 'LongVarBinary';
          adChapter: Result := 'Chapter';
          adFileTime: Result := 'FileTime';
          adPropVariant: Result := 'PropVariant';
          adVarNumeric: Result := 'VarNumeric';
          else Result := 'Unknown';
        end;
    4 : if (IADOProperty(ADOXObject).Get_Attributes and adPropRequired = adPropRequired)
           then Result := TRUE
           else Result := FALSE;
    5 : if (IADOProperty(ADOXObject).Get_Attributes and adPropWrite <> adPropWrite)
           then Result := TRUE
           else Result := FALSE;
    6 : if (IADOProperty(ADOXObject).Get_Attributes and adPropRead <> adPropRead)
           then Result := TRUE
           else Result := FALSE;
    3 : DispGetPropValue(ADOXObject, 'Value', Result);
  end;
end;

function TDBProperties.GetParent: TADOXDataSet;
begin
  if MasterSource = nil
     then Result := nil
     else Result := TADOXDataSet(MasterSource.Owner);
end;

function TDBProperties.GetPropertyObject: IADOProperty;
begin
  Result := IADOProperty(GetDispADOXObject);
end;

procedure TDBProperties.InitItemFields;
var FieldDef: TFieldDef;
begin
  FieldDef := TFieldDef.Create(FieldDefs, 'NAME', ftString, 50, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  FieldDef := TFieldDef.Create(FieldDefs, 'TYPE', ftString, 20, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  TFieldDef.Create(FieldDefs, 'VALUE', TFieldType(ftVariant), 0, FALSE, 1);
  FieldDef := TFieldDef.Create(FieldDefs, 'REQUIRED', ftBoolean, 0, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  FieldDef := TFieldDef.Create(FieldDefs, 'READONLY', ftBoolean, 0, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
  FieldDef := TFieldDef.Create(FieldDefs, 'WRITEONLY', ftBoolean, 0, FALSE, 1);
  FieldDef.Attributes := FieldDef.Attributes + [faReadOnly];
end;

procedure TDBProperties.InternalEdit;
begin
  if WordBool(GetFieldValue(GetFieldValue(nil, GetCurrentItem(ActiveBuffer), 0),
       GetCurrentItem(ActiveBuffer), 5))
     then DatabaseError(SReadOnlyProperty, Self);
end;

procedure TDBProperties.InternalRefresh;
begin
  GetParent.GetADOXObject.Properties.Refresh;
  inherited;
end;

procedure TDBProperties.SetFieldValue(ADOXObject: IDispatch; FieldNo: Integer;
  Value: Variant);
begin
  if FieldNo = 3 then IADOProperty(ADOXObject).Value := Value;
end;

procedure TDBProperties.SetParent(const Value: TADOXDataSet);
begin
  if Value = nil
     then MasterSource := nil
     else if (Value is TDBTables)
          or (Value is TDBColumns)
          or (Value is TDBIndexes)
     {$IFDEF ADOX26}
          or (Value is TDBGroups)
          or (Value is TDBUsers)
     {$ENDIF}
             then MasterSource := TADOXDataSet(Value).FDataSource
             else raise Exception.Create(SInvalidPropertyValue);
end;

end.
