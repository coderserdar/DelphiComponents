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

unit cblobdat;

{$I CDEFINES.INC}
interface

uses Windows, ActiveX, db, SysUtils, ComObj, cadodb, cmsdasc, cconsts;

const
  IID_IAccessor: TGUID = '{0c733a8c-2a1c-11ce-ade5-00aa0044773d}';
  IID_IRowset: TGUID = '{0c733a7c-2a1c-11ce-ade5-00aa0044773d}';
  IID_IRowsetChange: TGUID = '{0c733a05-2a1c-11ce-ade5-00aa0044773d}';
  IID_IDBInitialize: TGUID = '{0c733a8b-2a1c-11ce-ade5-00aa0044773d}';

  DBPROPSET_ROWSET: TGUID = '{c8b522be-5cf3-11ce-ade5-00aa0044773d}';


  DBPART_VALUE = $01;
  DBPART_LENGTH	= $02;
  DBPART_STATUS = $04;


  DBMEMOWNER_CLIENTOWNED = 0;

  DBPARAMIO_NOTPARAM = 0;

  DBTYPE_IUNKNOWN = 13;

  STGM_READ = 0;
  STGM_WRITE = 1;
  STGM_READWRITE = 2;

  DBACCESSOR_ROWDATA = $2;

  DBSTATUS_S_OK = 0;
  DBSTATUS_S_ISNULL = 3;

  DBPOSITION_OK	= 0;
  DBPOSITION_NOROW = DBPOSITION_OK + 1;
  DBPOSITION_BOF = DBPOSITION_NOROW + 1;
  DBPOSITION_EOF = DBPOSITION_BOF + 1;

  DBASYNCHOP_OPEN = 0;

  DBASYNCHPHASE_INITIALIZATION = 0;
  DBASYNCHPHASE_POPULATION = DBASYNCHPHASE_INITIALIZATION + 1;
  DBASYNCHPHASE_COMPLETE = DBASYNCHPHASE_POPULATION + 1;
  DBASYNCHPHASE_CANCELED = DBASYNCHPHASE_COMPLETE + 1;

  DBPROP_DELAYSTORAGEOBJECTS = $2b;

type
  DBOBJECT = record
    dwFlags: DWORD;
    iid: TGUID;
  end;

  DBBINDING = record
    iOrdinal: ULONG;
    obValue: ULONG;
    obLength: ULONG;
    obStatus: ULONG;
    pTypeInfo: pointer;
    pObject: pointer;
    pBindExt: pointer;
    dwPart: DWORD;
    dwMemOwner: DWORD;
    eParamIO: DWORD;
    cbMaxLen: ULONG;
    dwFlags: DWORD;
    wType: WORD;
    bPrecision: BYTE;
    bScale: BYTE;
  end;

  PDBBINDING = ^DBBINDING;
  TDBBINDINGARRAY = array [0..65535] of DBBINDING;
  PDBBINDINGARRAY = ^TDBBINDINGARRAY;

  DBPROPID = DWORD;

  DBPROPIDSET = record
    rgPropertyIDs: pointer;
    cPropertyIDs: ULONG;
    guidPropertySet: TGUID;
  end;

  DBPROPIDSETArray = array [0..65535] of DBPROPIDSET;
  PDBPROPIDSETArray = ^DBPROPIDSETArray;

  DBGUID = record
    guid: TGUID;
    pguid: ^TGUID;
  end;

  DBNAME = record
    pwszName: PWideChar;
    ulPropid: ULONG;
  end;

  DBID = record
    uGuid: DBGUID;
    eKind: DWORD;
    uName: DBNAME;
  end;

  DBPROP = record
    dwPropertyID: DWORD;
    dwOptions: DWORD;
    dwStatus: DWORD;
    colid: DBID;
    vValue: OleVariant;
  end;

  DBPROPSET = record
    rgProperties: ^DBPROP;
    cProperties: ULONG;
    guidPropertySet: TGUID;
  end;

  DBPROPSETArray = array [0..65535] of DBPROPSET;
  PDBPROPSETArray = ^DBPROPSETArray;

  PDBPROPSET = ^DBPROPSET;

  PDBPROPIDSET = ^DBPROPIDSET;

  DBBINDSTATUS = DWORD;

  HACCESSOR = ULONG;
  PHACCESSOR = ^HACCESSOR;

  HROW = ULONG;
  HCHAPTER = ULONG;

  TUINTARRAY = array [0..65535] of UINT;
  PUINTARRAY = ^TUINTARRAY;



  IAccessor = interface(IUnknown)
    ['{0c733a8c-2a1c-11ce-ade5-00aa0044773d}']
    function AddRefAccessor(hAccessor: HACCESSOR;
      pcRefCount: PULONG): HRESULT; stdcall;
    function CreateAccessor(dwAccessorFlags: DWORD; cBindings: ULONG;
      rgBindings: PDBBINDINGARRAY; cbRowSize: ULONG;
      var phAccessor: HACCESSOR; rgStatus: PUINTARRAY): HRESULT; stdcall;
    function GetBindings(hAccessor: HACCESSOR; pdwAccessorFlags: PDWORD;
      var pcBindings: ULONG; out prgBindings: PDBBINDING): HRESULT; stdcall;
    function ReleaseAccessor(hAccessor: HACCESSOR;
      pcRefCount: PULONG): HRESULT; stdcall;
  end;

  IRowset = interface(IUnknown)
    ['{0c733a7c-2a1c-11ce-ade5-00aa0044773d}']
        function AddRefRows(cRows: ULONG; rghRows: PUINTARRAY;
      rgRefCounts: PUINTARRAY; rgRowStatus: PUINTARRAY): HRESULT; stdcall;
    function GetData(hRow: HROW; hAccessor: HACCESSOR;
      pData: pointer): HRESULT; stdcall;
    function GetNextRows(hReserved: HCHAPTER; lRowsOffset: Integer;
      cRows: Integer; out pcRowsObtained: ULONG;
      var prghRows: PUINTARRAY): HRESULT; stdcall;
    function ReleaseRows(cRows: ULONG; rghRows: PUINTARRAY;
      rgRowOptions: PUINTARRAY; rgRefCounts: PUINTARRAY;
      rgRowStatus: PUINTARRAY): HRESULT; stdcall;
    function RestartPosition(hReserved: HCHAPTER): HRESULT; stdcall;
  end;

  IRowsetChange = interface(IUnknown)
    ['{0c733a05-2a1c-11ce-ade5-00aa0044773d}']
    function DeleteRows(hReserved: HCHAPTER; cRows: ULONG; rghRows: PUINTARRAY;
      rgRowStatus: PUINTARRAY): HRESULT; stdcall;
    function SetData(hRow: HROW; hAccessor: HACCESSOR;
      pData: pointer): HRESULT; stdcall;
    function InsertRow(hReserved: HCHAPTER; hAccessor: HACCESSOR;
      pData: pointer; out phRow: HROW): HRESULT; stdcall;
  end;

  IRowPosition = interface(IUnknown)
    ['{0c733a94-2a1c-11ce-ade5-00aa0044773d}']
    function ClearRowPosition(): HRESULT; stdcall;
    function GetRowPosition(out phChapter: HCHAPTER; out phRow: HROW;
      out pdwPositionFlags: DWORD): HRESULT; stdcall;
    function GetRowset(const REFIID: TGUID;
      out ppRowset: IRowset): HRESULT; stdcall;
    function Initialize(const pRowset: IUnknown): HRESULT; stdcall;
    function SetRowPosition(hChapter: HCHAPTER; hRow: HROW;
      dwPositionFlags: DWORD): HRESULT; stdcall;
  end;

  ICommandProperties = interface(IUnknown)
    ['{0c733a79-2a1c-11ce-ade5-00aa0044773d}']
    function GetProperties(cPropertyIDSets: ULONG;
      rgPropertyIDSets: PDBPROPIDSETArray; var pcPropertySets: ULONG;
      out prgPropertySets: PDBPROPSET): HRESULT; stdcall;
    function SetProperties(cPropertySets: ULONG;
      rgPropertySets: PDBPROPSETArray): HRESULT; stdcall;
  end;

const DB_NULLID: DBID = (uGuid: (guid: '{00000000-0000-0000-0000-000000000000}'); eKind: 0; uName: (pwszName: nil));

function GetOLEDBStorageObject(RowPos: IRowPosition; Column: Integer; var IsNull: BOOL; var Size: DWORD; Mode: TBlobStreamMode): IUnknown;
function RowsetSetProperty(Obj: IUnknown; PropertyID: DWORD;
  Value: OleVariant): Boolean;

implementation

type
  TInternalOLEBlobStream = class(TInterfacedObject, ISequentialStream)
    function RemoteRead(out pv: Byte; cb: UINT; out pcbRead: UINT): HResult; stdcall;
    function RemoteWrite(var pv: Byte; cb: UINT; out pcbWritten: UINT): HResult; stdcall;
  end;

function GetOLEDBStorageObject(RowPos: IRowPosition; Column: Integer;
  var IsNull: BOOL; var Size: DWORD; Mode: TBlobStreamMode): IUnknown;
var hAcc: HACCESSOR;
    rgStatus: array [0..0] of DBBINDSTATUS;
    ObjectStruct: DBOBJECT;
    rgBinding: array [0..0] of DBBINDING;
    Accessor: IAccessor;
    pData: pointer;
    Row: HROW;
    Chapter: HCHAPTER;
    Status: DWORD;
    Rowset: IRowset;
    hRes: HRESULT;
begin
  pData := nil;
  try
    IsNull := FALSE;
    Size := 0;
    Result := nil;
    if Mode = bmReadWrite then DatabaseError(SOLEBlobStreamModeError);
    if RowPos = nil then DatabaseError(SInvalidRowHandle);
    RowPos.GetRowPosition(Chapter, Row, Status);
    RowPos.GetRowset(IID_IRowset, Rowset);
    Rowset.ReleaseRows(1, @Row, nil, nil, nil);
    if (Mode = bmRead) and ((Status = DBPOSITION_BOF) or (Status = DBPOSITION_EOF)) then
       begin
         Rowset := nil;
         IsNull := TRUE;
         Exit;
       end;
    pData := CoTaskMemAlloc(SizeOf(IUnknown) + SizeOf(ULONG) * 2);
    with rgBinding [0] do
         begin
           iOrdinal := Column;
           obValue := 0;
           obStatus := SizeOf(IUnknown);
           obLength := obStatus + SizeOf(ULONG) ;
           pTypeInfo := nil;
           pObject := @ObjectStruct;
           pBindExt := nil;
           dwPart := DBPART_VALUE or DBPART_STATUS or DBPART_LENGTH;
           dwMemOwner := DBMEMOWNER_CLIENTOWNED;
           eParamIO := DBPARAMIO_NOTPARAM;
           cbMaxLen := 0;
           dwFlags := 0;
           wType := DBTYPE_IUNKNOWN;
           bPrecision := 0;
           bScale := 0;
         end;
    if Mode = bmWrite then
       rgBinding [0].dwPart := DBPART_VALUE or DBPART_STATUS;
    if Mode = bmRead
       then ObjectStruct.dwFlags := OF_READ
       else ObjectStruct.dwFlags := OF_CREATE;
    Rowset.QueryInterface(IID_IAccessor, Accessor);
    ObjectStruct.iid := IStream;
    hRes := Accessor.CreateAccessor(DBACCESSOR_ROWDATA, 1, @rgBinding,
    SizeOf(ISequentialStream) + SizeOf(ULONG) * 2, hAcc, @rgStatus);
    if hRes = S_OK then
       begin
         RowPos.GetRowPosition(Chapter, Row, Status);
         if (Status = DBPOSITION_NOROW) then DatabaseError(SInvalidRowHandle);
         hRes := Rowset.GetData(Row, hAcc, pData);
         RowSet.ReleaseRows(1, @Row, nil, nil, nil);
         Accessor.ReleaseAccessor(hAcc, nil);
         Accessor := nil;
         Rowset := nil;
       end else
             begin
               ObjectStruct.iid := ISequentialStream;
               hRes := Accessor.CreateAccessor(DBACCESSOR_ROWDATA, 1, @rgBinding,
                  SizeOf(ISequentialStream) + SizeOf(ULONG) * 2, hAcc, @rgStatus);
               if hRes <> S_OK then raise EOleException.Create(SysErrorMessage(hRes),
                                          hRes, 'IAccessor', '', 0);
               RowPos.GetRowPosition(Chapter, Row, Status);
               if (Status = DBPOSITION_NOROW) then DatabaseError(SInvalidRowHandle);
               hRes := Rowset.GetData(Row, hAcc, pData);
               Rowset.ReleaseRows(1, @Row, nil, nil, nil);
               Accessor.ReleaseAccessor(hAcc, nil);
               if hRes <> S_OK then raise EOleException.Create(SysErrorMessage(hRes),
                                          hRes, 'IRowset', '', 0);
               Accessor := nil;
               Rowset := nil;
             end;
  if hRes = S_OK then
     begin
       if (PULONG(pointer(DWORD(pData) + rgBinding[0].obStatus))^) = DBSTATUS_S_OK
          then
            begin
              Result := IUnknown(pData^);
              IUnknown(pData^) := nil;
              if Mode = bmRead then
                 Size := PULONG(pointer(DWORD(pData) + rgBinding[0].obLength))^;
            end
          else
            begin
              IsNull := TRUE;
            end;
     end;
  finally
    Accessor := nil;
    Rowset := nil;
    CoTaskMemFree(pData);
  end;
end;


{ TInternalOLEBlobStream }

function TInternalOLEBlobStream.RemoteRead(out pv: Byte; cb: UINT;
  out pcbRead: UINT): HResult;
begin
  Result := E_NOTIMPL;
end;

function TInternalOLEBlobStream.RemoteWrite(var pv: Byte; cb: UINT;
  out pcbWritten: UINT): HResult;
begin
  Result := E_NOTIMPL;
end;


function RowsetSetProperty(Obj: IUnknown; PropertyID: DWORD;
  Value: OleVariant): Boolean;
var CommandProps: ICommandProperties;
    dbp: DBPROP;
    dbpSet: DBPROPSET;
begin
  Result := FALSE;
  if Obj.QueryInterface(ICommandProperties, CommandProps) <> S_OK then Exit;
  dbpSet.rgProperties := @dbp;
  dbpSet.cProperties := 1;
  dbpSet.guidPropertySet := DBPROPSET_ROWSET;
  dbp.dwPropertyID := PropertyID;
  dbp.dwOptions := 0;
  dbp.colid := DB_NULLID;
  dbp.vValue := Value;
  if CommandProps.SetProperties(1, @dbpSet) = S_OK then Result := TRUE;
  CommandProps := nil;
end;

end.
