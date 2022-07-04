{
  Part of Kylix / Delphi open source DbExpress driver for ODBC
  Version 2008.02.17

  Copyright (c) 2006 by "Vadim V.Lopushansky" <pult@ukr.net>

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit SqlExprFixBDS2006;

//
// Delphi 2006.
//

//
// Fixed "SqlExpr.pas":
//
//  qc: ???
//     + procedure TCustomSQLDataSet.LoadFieldDef(...
//  qc: ???
//     - TCustomSQLDataSet.GetFieldData(... for fldTIMESTAMP
//  qc: ???
//     - TWideString Fixed

{$R-,T-,H+,X+}

interface

implementation

uses
  // interface
  Windows, SysUtils, Variants, Classes, DB, DBCommon, DBCommonTypes,
  DBXpress, SqlTimSt, WideStrings,
  // implementation
  Registry, SqlConst, DBConsts, IniFiles, DBConnAdmin, Math, FMTBcd, WideStrUtils,
  // Fixed:
  SqlExpr;

type
  {$hints off}
  TSqlConnectionPro = class(TSqlConnection);
  TCustomSQLDataSetH = class(TWideDataSet)
  private
    FBlobBuffer: TBlobByteData;
    FCalcFieldsBuffer: PChar;
    FCheckRowsAffected: Boolean;
    FClonedConnection: TSqlConnectionPro;
    FCommandText: WideString;
    FCommandType: TSQLCommandType;
    FCurrentBlobSize: LongWord;
    FDataLink: TDataLink;
    FDesignerData: string;
    FGetNextRecordSet: Boolean;
    FIndexDefs: TIndexDefs;
    FIndexDefsLoaded: Boolean;
    FLastError: string;  // DBExpress GetError() clears error; need to save last
    FMaxBlobSize: Integer;
    FMaxColSize: LongWord;
    FNativeCommand: WideString;
    FGetMetadata: Boolean;
    FNumericMapping: Boolean;
    FParamCheck: Boolean;
    FParamCount: Integer;
    FParams: TParams;
    FPrepared: Boolean;
    FProcParams: TList;
    FRecords: Integer;
    FRowsAffected: Integer;
    FSchemaInfo: TSQLSchemaInfo;
    FParseSelectSql: TParseSqlEvent;
    FParseUpdateSql: TParseSqlEvent;
    FParseDeleteSql: TParseSqlEvent;
    FParseInsertSql: TParseInsertSqlEvent;
    FSortFieldNames: WideString;

    FSQLCommand: TISQLCommand;
    FSQLConnection: TSQLConnectionPro;
    FSQLCursor: TISQLCursor;

    FStatementOpen: Boolean;
    FTransactionLevel: SmallInt;
    FSchemaName: string;

    property
      SQLConnection: TSQLConnectionPro read FSQLConnection;
  end;
  {$hints on}
  TCustomSQLDataSetP = class(TCustomSQLDataSet);

  TCustomSQLDataSetFix = class(TCustomSQLDataSet)
  private
    // fixed
    procedure LoadFieldDef(FieldID: Word; var FldDesc: TFLDDesc);
  protected
    procedure InternalInitFieldDefs; override;
  end;

{ TCustomSQLDataSetFix }

procedure TCustomSQLDataSetFix.InternalInitFieldDefs;
var
  FID: Integer;
  FieldDescs: TFieldDescList;
  RequiredFields: TBits;
  Nullable: LongBool;
  FldDescCount: Word;
begin
  with TCustomSQLDataSetH(Self) do begin
  //
  if (FSQLCursor <> nil) then
  begin
    RequiredFields := TBits.Create;
    try
      FSQLCursor.getColumnCount(FldDescCount);
      SetLength(FieldDescs, FldDescCount);
      for FID := 1 to FldDescCount do
        FieldDescs[FID-1] := SQLConnection.Connection.getFldDescClass.Create;
      try
        RequiredFields.Size := FldDescCount + 1;
        FieldDefs.Clear;
        FID := 1;
        FMaxColSize := FldDescCount;
        while FID <= FldDescCount do
        begin
          FSQLCursor.IsNullable(Word(FID), Nullable);
          RequiredFields[FID] := Nullable = False;
          LoadFieldDef(Word(FID), FieldDescs[0]);
          if (FieldDescs[0].iLen > FMaxColSize) and
             (FieldDescs[0].iFldType <> fldBLOB) then
            FMaxColSize := (FMaxColSize + FieldDescs[0].iLen);
          AddFieldDesc(FieldDescs, Integer(0), FID, RequiredFields, FieldDefs);
        end;
      finally
        for FID := 1 to FldDescCount do
          FreeAndNil(FieldDescs[FID-1]);
        FieldDescs := nil;
      end;
    finally
      RequiredFields.Free;
    end;
  end
  else
     DatabaseError(SDataSetClosed, self);
  //
  end;
end;

procedure TCustomSQLDataSetFix.LoadFieldDef(FieldID: Word; var FldDesc: TFLDDesc);
var
  iFldType: Word;
  iSubType: Word;
  iUnits1: SmallInt;
  iUnits2: SmallInt;
  iLen: LongWord;
  ReadOnly: LongBool;
begin
  with TCustomSQLDataSetH(Self) do begin
  //
  FldDesc.iFldNum := FieldID;
  FldDesc.szName :=FSQLCursor.getColumnName(FieldId);
  FSQLCursor.getColumnType(FieldId, iFldType, iSubtype);
  FldDesc.iFldType := iFldType;
  FldDesc.iSubtype := iSubtype;
  FSQLCursor.getColumnLength(FieldId, iLen);
  FldDesc.iLen := iLen;
  FSQLCursor.getColumnPrecision(FieldId, iUnits1);
  FldDesc.iUnits1 := iUnits1;
  FSQLCursor.getColumnScale(FieldId, iUnits2);
  FldDesc.iUnits2 := iUnits2;
  FSQLCursor.isReadOnly(FieldID, ReadOnly);
  if ReadOnly then
  {+}
    FldDesc.efldrRights := fldrREADONLY
  else
    FldDesc.efldrRights := fldrREADWRITE;
  {+.}
  //
  end;
end;

// *********************** RUNTIME PATCH ***********************

Type

  PJumpInstruction = ^TJumpInstruction;
  TJumpInstruction = packed record
    case byte of
      1:(
        Code   :Byte;  // jump instruction ($FF)
        Offset :DWORD; // jump offset
        UnUsed :Byte;
      );
      2:(
        DW1: DWORD;
        DW2: DWORD;
      );
  end;

  PPointer = ^Pointer;
  PPPointer = ^PPointer;

Const
  PageRWFlag = {$IFDEF LINUX} PROT_READ or PROT_WRITE {$ELSE} PAGE_READWRITE {$ENDIF};

function GetNativeAddr(Ptr:Pointer):Pointer;
begin
  Result := Ptr;
  {$IFDEF LINUX}
    // TODO: ???: Need check for LINUX.
  {$ENDIF}
  if  (PJumpInstruction(Result)^.Code=$FF) and            // long jmp to package_address_5b
      (PJumpInstruction(Integer(Result)+6)^.Code=$8B) and // mov
      (PJumpInstruction(Integer(Result)+7)^.Code=$C0)     // eax,eax
  then
    Result := PPPointer(Integer(Result) + 2)^^;
end;

function JumpFromCode( MSrc, MDest:Pointer; SaveJump :PJumpInstruction): Boolean;
 var
  SaveFlagSrc: DWORD;
begin
  Result := False;
  if Assigned(SaveJump) then
    FillChar(SaveJump^, SizeOf(TJumpInstruction), 0);
  if MSrc = nil then
    exit;
  MSrc := GetNativeAddr(MSrc);
  if VirtualProtect(MSrc,  SizeOf(TJumpInstruction), PageRWFlag, @SaveFlagSrc ) then
  try
     if Assigned(SaveJump) then
       SaveJump^ := PJumpInstruction(MSrc)^;
     with PJumpInstruction(MSrc)^ do begin
       Code := $E9; // JMP SIZE = 5b
       Offset := Integer(MDest) - Integer(MSrc) - 5;
     end;
     Result := True;
  finally
    VirtualProtect(MSrc,  SizeOf(TJumpInstruction), SaveFlagSrc,  @SaveFlagSrc );
    {$IFDEF LINUX}
    {$ELSE}
    FlushInstructionCache(GetCurrentProcess, MSrc, SizeOf(TJumpInstruction));
    {$ENDIF}
  end;
end;

function RestoreSavedCode( MSrc:Pointer; var SaveJump :TJumpInstruction): Boolean;
var
  SaveFlagSrc: DWORD;
begin
  Result := False;
  if (SaveJump.Code = 0) or (MSrc = nil) then
    exit;
  MSrc := GetNativeAddr(MSrc);
  try
    if VirtualProtect(MSrc,  SizeOf(TJumpInstruction), PageRWFlag, @SaveFlagSrc ) then
    try
       PJumpInstruction(MSrc)^ := SaveJump;  // Restore Code
       // Clear Buffer
       FillChar(SaveJump, SizeOf(TJumpInstruction), 0);
    finally
      VirtualProtect(MSrc,  SizeOf(TJumpInstruction), SaveFlagSrc, @SaveFlagSrc );
      {$IFDEF LINUX}
      {$ELSE}
      FlushInstructionCache(GetCurrentProcess, MSrc, SizeOf(TJumpInstruction));
      {$ENDIF}
    end;
    Result := True;
  except
  end;
end;

var
  jmp_TCustomSQLDataSet_InternalInitFieldDefs: TJumpInstruction;

procedure InstallFix;
begin
  JumpFromCode(@TCustomSQLDataSetP.InternalInitFieldDefs,
    @TCustomSQLDataSetFix.InternalInitFieldDefs,
    @jmp_TCustomSQLDataSet_InternalInitFieldDefs);
end;

procedure UnInstallFix;
begin
  RestoreSavedCode(@TCustomSQLDataSetP.InternalInitFieldDefs,
    jmp_TCustomSQLDataSet_InternalInitFieldDefs);
end;

initialization
  InstallFix;
finalization
  if IsLibrary then
    UnInstallFix;
end.
