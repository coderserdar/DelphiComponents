{-----------------------------------------------------------------------------}
{ This file is generated automatically by                                     }
{ NCOCI8 PL/SQL Wrapper Objects Generator, (c) 1999-2002, Dmitry Arefiev      }
{-----------------------------------------------------------------------------}
{ Generated classes are:                                                      }
{ TOCINumberTable -> PL/SQL Table "NUMBER_TABLE"                              }
{ TOCIVarchar2Table -> PL/SQL Table "VARCHAR2_TABLE"                          }
{ TOCIDateTable -> PL/SQL Table "DATE_TABLE"                                  }
{ TOCIBlobTable -> PL/SQL Table "BLOB_TABLE"                                  }
{ TOCIClobTable -> PL/SQL Table "CLOB_TABLE"                                  }
{ TOCIBfileTable -> PL/SQL Table "BFILE_TABLE"                                }
{ TOCIUrowidTable -> PL/SQL Table "UROWID_TABLE"                              }
{ TOCIDescTab -> PL/SQL Table "DESC_TAB"                                      }
{ TOCIVarchar2s -> PL/SQL Table "VARCHAR2S"                                   }
{ TOCISysDbmsSql -> PL/SQL Package "SYS.DBMS_SQL"                             }
{ Warning: do not change this unit manually !                                 }
{ Otherwise your changes may be overwited in next time.                       }
{-----------------------------------------------------------------------------}
unit DBMS_SQL;

interface

uses Classes, SysUtils, DB, NCOci, NCOciWrapper, NCOciDB;

type
  // TOCINumberTable
  // Generated for: PL/SQL Table "NUMBER_TABLE"
  TOCINumberTable = class(TOCIPLSQLTable)
  end;

  // TOCIVarchar2Table
  // Generated for: PL/SQL Table "VARCHAR2_TABLE"
  TOCIVarchar2Table = class(TOCIPLSQLTable)
  end;

  // TOCIDateTable
  // Generated for: PL/SQL Table "DATE_TABLE"
  TOCIDateTable = class(TOCIPLSQLTable)
  end;

  // TOCIBlobTable
  // Generated for: PL/SQL Table "BLOB_TABLE"
  TOCIBlobTable = class(TOCIPLSQLTable)
  end;

  // TOCIClobTable
  // Generated for: PL/SQL Table "CLOB_TABLE"
  TOCIClobTable = class(TOCIPLSQLTable)
  end;

  // TOCIBfileTable
  // Generated for: PL/SQL Table "BFILE_TABLE"
  TOCIBfileTable = class(TOCIPLSQLTable)
  end;

  // TOCIUrowidTable
  // Generated for: PL/SQL Table "UROWID_TABLE"
  TOCIUrowidTable = class(TOCIPLSQLTable)
  end;

  // TOCIDescTab
  // Generated for: PL/SQL Table "DESC_TAB"
  TOCIDescTab = class(TOCIPLSQLTable)
  end;

  // TOCIVarchar2s
  // Generated for: PL/SQL Table "VARCHAR2S"
  TOCIVarchar2s = class(TOCIPLSQLTable)
  end;

  // TOCISysDbmsSql
  // Generated for: PL/SQL Package "SYS.DBMS_SQL"
  TOCISysDbmsSql = class(TOCICustomPackage)
  public
    constructor Create(AOwner: TComponent); override;
    procedure BindArray(const AC: Double; const AName: String; const ANTab: TOCINumberTable); overload;
    procedure BindArray(const AC: Double; const AName: String; const ACTab: TOCIVarchar2Table); overload;
    procedure BindArray(const AC: Double; const AName: String; const ADTab: TOCIDateTable); overload;
    procedure BindArray(const AC: Double; const AName: String; const ABlTab: TOCIBlobTable); overload;
    procedure BindArray(const AC: Double; const AName: String; const AClTab: TOCIClobTable); overload;
    procedure BindArray(const AC: Double; const AName: String; const ABfTab: TOCIBfileTable); overload;
    procedure BindArray(const AC: Double; const AName: String; const ANTab: TOCINumberTable; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindArray(const AC: Double; const AName: String; const ACTab: TOCIVarchar2Table; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindArray(const AC: Double; const AName: String; const ADTab: TOCIDateTable; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindArray(const AC: Double; const AName: String; const ABlTab: TOCIBlobTable; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindArray(const AC: Double; const AName: String; const AClTab: TOCIClobTable; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindArray(const AC: Double; const AName: String; const ABfTab: TOCIBfileTable; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindArray(const AC: Double; const AName: String; const AUrTab: TOCIUrowidTable); overload;
    procedure BindArray(const AC: Double; const AName: String; const AUrTab: TOCIUrowidTable; const AIndex1: Double; const AIndex2: Double); overload;
    procedure BindVariable(const AC: Double; const AName: String; const AValue: Double); overload;
    procedure BindVariable(const AC: Double; const AName: String; const AValue: String); overload;
    procedure BindVariable(const AC: Double; const AName: String; const AValue: String; const AOutValueSize: Double); overload;
    procedure BindVariableDT(const AC: Double; const AName: String; const AValue: TDateTime); overload;
    procedure BindVariable(const AC: Double; const AName: String; const AValue: TOCIILOBStream); overload;
//***    procedure BindVariable(const AC: Double; const AName: String; const AValue: TOCIILOBStream); overload;
    procedure BindVariable(const AC: Double; const AName: String; const AValue: TOCIFILEStream); overload;
//***    procedure BindVariable(const AC: Double; const AName: String; const AValue: String); overload;
    procedure BindVariableChar(const AC: Double; const AName: String; const AValue: String); overload;
    procedure BindVariableChar(const AC: Double; const AName: String; const AValue: String; const AOutValueSize: Double); overload;
    procedure BindVariableRaw(const AC: Double; const AName: String; const AValue: TBlobData); overload;
    procedure BindVariableRaw(const AC: Double; const AName: String; const AValue: TBlobData; const AOutValueSize: Double); overload;
    procedure BindVariableRowid(const AC: Double; const AName: String; const AValue: String);
    procedure CloseCursor(var AC: Double);
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: Double); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: String); overload;
    procedure ColumnValueDT(const AC: Double; const APosition: Double; out AValue: TDateTime); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: TOCIILOBStream); overload;
//***    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: TOCIILOBStream); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: TOCIFILEStream); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: Double; out AColumnError: Double; out AActualLength: Double); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: String; out AColumnError: Double; out AActualLength: Double); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: TDateTime; out AColumnError: Double; out AActualLength: Double); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const ANTab: TOCINumberTable); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const ACTab: TOCIVarchar2Table); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const ADTab: TOCIDateTable); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const ABlTab: TOCIBlobTable); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const AClTab: TOCIClobTable); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const ABfTab: TOCIBfileTable); overload;
//***    procedure ColumnValue(const AC: Double; const APosition: Double; out AValue: String); overload;
    procedure ColumnValue(const AC: Double; const APosition: Double; const AUrTab: TOCIUrowidTable); overload;
    procedure ColumnValueChar(const AC: Double; const APosition: Double; out AValue: String); overload;
    procedure ColumnValueChar(const AC: Double; const APosition: Double; out AValue: String; out AColumnError: Double; out AActualLength: Double); overload;
    procedure ColumnValueLong(const AC: Double; const APosition: Double; const ALength: Double; const AOffset: Double; out AValue: String; out AValueLength: Double);
    procedure ColumnValueRaw(const AC: Double; const APosition: Double; out AValue: TBlobData); overload;
    procedure ColumnValueRaw(const AC: Double; const APosition: Double; out AValue: TBlobData; out AColumnError: Double; out AActualLength: Double); overload;
    procedure ColumnValueRowid(const AC: Double; const APosition: Double; out AValue: String); overload;
    procedure ColumnValueRowid(const AC: Double; const APosition: Double; out AValue: String; out AColumnError: Double; out AActualLength: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const ANTab: TOCINumberTable; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const ACTab: TOCIVarchar2Table; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const ADTab: TOCIDateTable; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const ABlTab: TOCIBlobTable; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const AClTab: TOCIClobTable; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const ABfTab: TOCIBfileTable; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineArray(const AC: Double; const APosition: Double; const AUrTab: TOCIUrowidTable; const ACnt: Double; const ALowerBound: Double); overload;
    procedure DefineColumn(const AC: Double; const APosition: Double; const AColumn: Double); overload;
    procedure DefineColumn(const AC: Double; const APosition: Double; const AColumn: String; const AColumnSize: Double); overload;
    procedure DefineColumnDT(const AC: Double; const APosition: Double; const AColumn: TDateTime); overload;
    procedure DefineColumn(const AC: Double; const APosition: Double; const AColumn: TOCIILOBStream); overload;
//***    procedure DefineColumn(const AC: Double; const APosition: Double; const AColumn: TOCIILOBStream); overload;
    procedure DefineColumn(const AC: Double; const APosition: Double; const AColumn: TOCIFILEStream); overload;
    procedure DefineColumn(const AC: Double; const APosition: Double; const AColumn: String); overload;
    procedure DefineColumnChar(const AC: Double; const APosition: Double; const AColumn: String; const AColumnSize: Double);
    procedure DefineColumnLong(const AC: Double; const APosition: Double);
    procedure DefineColumnRaw(const AC: Double; const APosition: Double; const AColumn: TBlobData; const AColumnSize: Double);
    procedure DefineColumnRowid(const AC: Double; const APosition: Double; const AColumn: String);
    function Execute(const AC: Double): Double;
    function ExecuteAndFetch(const AC: Double; const AExact: Boolean): Double;
    function FetchRows(const AC: Double): Double;
    function IsOpen(const AC: Double): Boolean;
    function LastErrorPosition(): Double;
    function LastRowCount(): Double;
    function LastRowId(): String;
    function LastSqlFunctionCode(): Double;
    function OpenCursor(): Double;
    procedure Parse(const AC: Double; const AStatement: String; const ALanguageFlag: Double); overload;
    procedure Parse(const AC: Double; const AStatement: TOCIVarchar2s; const ALb: Double; const AUb: Double; const ALfflg: Boolean; const ALanguageFlag: Double); overload;
    procedure VariableValue(const AC: Double; const AName: String; out AValue: Double); overload;
    procedure VariableValue(const AC: Double; const AName: String; out AValue: String); overload;
    procedure VariableValueDT(const AC: Double; const AName: String; out AValue: TDateTime); overload;
    procedure VariableValue(const AC: Double; const AName: String; out AValue: TOCIILOBStream); overload;
//***    procedure VariableValue(const AC: Double; const AName: String; out AValue: TOCIILOBStream); overload;
    procedure VariableValue(const AC: Double; const AName: String; out AValue: TOCIFILEStream); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCINumberTable); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCIVarchar2Table); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCIDateTable); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCIBlobTable); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCIClobTable); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCIBfileTable); overload;
//***    procedure VariableValue(const AC: Double; const AName: String; out AValue: String); overload;
    procedure VariableValue(const AC: Double; const AName: String; const AValue: TOCIUrowidTable); overload;
    procedure VariableValueChar(const AC: Double; const AName: String; out AValue: String);
    procedure VariableValueRaw(const AC: Double; const AName: String; out AValue: TBlobData);
    procedure VariableValueRowid(const AC: Double; const AName: String; out AValue: String);
  end;

implementation

// TOCINumberTable
// Generated for: PL/SQL Table "NUMBER_TABLE"
// - no methods

// TOCIVarchar2Table
// Generated for: PL/SQL Table "VARCHAR2_TABLE"
// - no methods

// TOCIDateTable
// Generated for: PL/SQL Table "DATE_TABLE"
// - no methods

// TOCIBlobTable
// Generated for: PL/SQL Table "BLOB_TABLE"
// - no methods

// TOCIClobTable
// Generated for: PL/SQL Table "CLOB_TABLE"
// - no methods

// TOCIBfileTable
// Generated for: PL/SQL Table "BFILE_TABLE"
// - no methods

// TOCIUrowidTable
// Generated for: PL/SQL Table "UROWID_TABLE"
// - no methods

// TOCIDescTab
// Generated for: PL/SQL Table "DESC_TAB"
// - no methods

// TOCIVarchar2s
// Generated for: PL/SQL Table "VARCHAR2S"
// - no methods

// TOCISysDbmsSql
// Generated for: PL/SQL Package "SYS.DBMS_SQL"
constructor TOCISysDbmsSql.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetProcCount(123);
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [14]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ANTab: TOCINumberTable);
begin
  with GetQuery(0) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :N_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':N_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ANTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [13]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ACTab: TOCIVarchar2Table);
begin
  with GetQuery(1) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :C_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':C_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otLong;
        ODataSize := 2000;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ACTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [12]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ADTab: TOCIDateTable);
begin
  with GetQuery(2) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :D_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':D_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ADTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [11]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ABlTab: TOCIBlobTable);
begin
  with GetQuery(3) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :BL_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':BL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ABlTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [10]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const AClTab: TOCIClobTable);
begin
  with GetQuery(4) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :CL_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':CL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AClTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [9]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ABfTab: TOCIBfileTable);
begin
  with GetQuery(5) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :BF_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':BF_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ABfTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [8]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ANTab: TOCINumberTable; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(6) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :N_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':N_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ANTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [7]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ACTab: TOCIVarchar2Table; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(7) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :C_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':C_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otLong;
        ODataSize := 2000;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ACTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [6]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ADTab: TOCIDateTable; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(8) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :D_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':D_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ADTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [5]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ABlTab: TOCIBlobTable; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(9) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :BL_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':BL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ABlTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [4]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const AClTab: TOCIClobTable; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(10) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :CL_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':CL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AClTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [3]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const ABfTab: TOCIBfileTable; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(11) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :BF_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':BF_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBFile;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(ABfTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [2]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const AUrTab: TOCIUrowidTable);
begin
  with GetQuery(12) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :UR_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':UR_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AUrTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_ARRAY" [1]
procedure TOCISysDbmsSql.BindArray(const AC: Double; const AName: String; const AUrTab: TOCIUrowidTable; const AIndex1: Double; const AIndex2: Double);
begin
  with GetQuery(13) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_ARRAY(:C, :NAME, :UR_TAB, :INDEX1, :INDEX2);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':UR_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otROWID;
        ODataSize := 18;
      end;
      with Params.Add do begin
        OName := ':INDEX1';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':INDEX2';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AUrTab);
    Params[3].AsFloat := AIndex1;
    Params[4].AsFloat := AIndex2;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [14]
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: Double);
begin
  with GetQuery(14) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsFloat := AValue;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [13]
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: String);
begin
  with GetQuery(15) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsString := AValue;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [12]
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: String; const AOutValueSize: Double);
begin
  with GetQuery(16) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE, :OUT_VALUE_SIZE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':OUT_VALUE_SIZE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsString := AValue;
    Params[3].AsFloat := AOutValueSize;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [11]
procedure TOCISysDbmsSql.BindVariableDT(const AC: Double; const AName: String; const AValue: TDateTime);
begin
  with GetQuery(17) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsDateTime := AValue;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [10]
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: TOCIILOBStream);
begin
  with GetQuery(18) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [9]
{***
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: TOCIILOBStream);
begin
  with GetQuery(19) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [8]
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: TOCIFILEStream);
begin
  with GetQuery(20) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [7]
{***
procedure TOCISysDbmsSql.BindVariable(const AC: Double; const AName: String; const AValue: String);
begin
  with GetQuery(21) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsString := AValue;
    ExecSQL;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [6]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [5]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [4]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [3]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [2]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE" [1]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE_CHAR" [2]
procedure TOCISysDbmsSql.BindVariableChar(const AC: Double; const AName: String; const AValue: String);
begin
  with GetQuery(28) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE_CHAR(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsString := AValue;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE_CHAR" [1]
procedure TOCISysDbmsSql.BindVariableChar(const AC: Double; const AName: String; const AValue: String; const AOutValueSize: Double);
begin
  with GetQuery(29) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE_CHAR(:C, :NAME, :VALUE, :OUT_VALUE_SIZE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':OUT_VALUE_SIZE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsString := AValue;
    Params[3].AsFloat := AOutValueSize;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE_RAW" [2]
procedure TOCISysDbmsSql.BindVariableRaw(const AC: Double; const AName: String; const AValue: TBlobData);
begin
  with GetQuery(30) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE_RAW(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otRaw;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsBlob := AValue;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE_RAW" [1]
procedure TOCISysDbmsSql.BindVariableRaw(const AC: Double; const AName: String; const AValue: TBlobData; const AOutValueSize: Double);
begin
  with GetQuery(31) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE_RAW(:C, :NAME, :VALUE, :OUT_VALUE_SIZE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otRaw;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':OUT_VALUE_SIZE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsBlob := AValue;
    Params[3].AsFloat := AOutValueSize;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."BIND_VARIABLE_ROWID"
procedure TOCISysDbmsSql.BindVariableRowid(const AC: Double; const AName: String; const AValue: String);
begin
  with GetQuery(32) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.BIND_VARIABLE_ROWID(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AsString := AValue;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."CLOSE_CURSOR"
procedure TOCISysDbmsSql.CloseCursor(var AC: Double);
begin
  with GetQuery(33) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.CLOSE_CURSOR(:C);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odInOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    ExecSQL;
    AC := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [23]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: Double);
begin
  with GetQuery(34) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [22]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: String);
begin
  with GetQuery(35) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [21]
procedure TOCISysDbmsSql.ColumnValueDT(const AC: Double; const APosition: Double; out AValue: TDateTime);
begin
  with GetQuery(36) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsDateTime;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [20]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: TOCIILOBStream);
begin
  with GetQuery(37) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    AValue := TOCIILOBStream.CreateAlone(Database, bmReadWrite, otBLOB);
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [19]
{***
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: TOCIILOBStream);
begin
  with GetQuery(38) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    AValue := TOCIILOBStream.CreateAlone(Database, bmReadWrite, otCLOB);
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [18]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: TOCIFILEStream);
begin
  with GetQuery(39) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    AValue := TOCIFILEStream.CreateAlone(Database, bmReadWrite, otBFile);
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [17]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: Double; out AColumnError: Double; out AActualLength: Double);
begin
  with GetQuery(40) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE, :COLUMN_ERROR, :ACTUAL_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN_ERROR';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':ACTUAL_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsFloat;
    AColumnError := Params[3].AsFloat;
    AActualLength := Params[4].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [16]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: String; out AColumnError: Double; out AActualLength: Double);
begin
  with GetQuery(41) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE, :COLUMN_ERROR, :ACTUAL_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':COLUMN_ERROR';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':ACTUAL_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
    AColumnError := Params[3].AsFloat;
    AActualLength := Params[4].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [15]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: TDateTime; out AColumnError: Double; out AActualLength: Double);
begin
  with GetQuery(42) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE, :COLUMN_ERROR, :ACTUAL_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
      with Params.Add do begin
        OName := ':COLUMN_ERROR';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':ACTUAL_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsDateTime;
    AColumnError := Params[3].AsFloat;
    AActualLength := Params[4].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [14]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const ANTab: TOCINumberTable);
begin
  with GetQuery(43) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :N_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':N_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ANTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [13]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const ACTab: TOCIVarchar2Table);
begin
  with GetQuery(44) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :C_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':C_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otLong;
        ODataSize := 2000;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ACTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [12]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const ADTab: TOCIDateTable);
begin
  with GetQuery(45) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :D_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':D_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ADTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [11]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const ABlTab: TOCIBlobTable);
begin
  with GetQuery(46) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :BL_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':BL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ABlTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [10]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const AClTab: TOCIClobTable);
begin
  with GetQuery(47) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :CL_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':CL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(AClTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [9]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const ABfTab: TOCIBfileTable);
begin
  with GetQuery(48) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :BF_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':BF_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ABfTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [8]
{***
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; out AValue: String);
begin
  with GetQuery(49) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [7]
procedure TOCISysDbmsSql.ColumnValue(const AC: Double; const APosition: Double; const AUrTab: TOCIUrowidTable);
begin
  with GetQuery(50) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE(:C, :POSITION, :UR_TAB);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':UR_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(AUrTab);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [6]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [5]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [4]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [3]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [2]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE" [1]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_CHAR" [2]
procedure TOCISysDbmsSql.ColumnValueChar(const AC: Double; const APosition: Double; out AValue: String);
begin
  with GetQuery(57) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_CHAR(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_CHAR" [1]
procedure TOCISysDbmsSql.ColumnValueChar(const AC: Double; const APosition: Double; out AValue: String; out AColumnError: Double; out AActualLength: Double);
begin
  with GetQuery(58) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_CHAR(:C, :POSITION, :VALUE, :COLUMN_ERROR, :ACTUAL_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':COLUMN_ERROR';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':ACTUAL_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
    AColumnError := Params[3].AsFloat;
    AActualLength := Params[4].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_LONG"
procedure TOCISysDbmsSql.ColumnValueLong(const AC: Double; const APosition: Double; const ALength: Double; const AOffset: Double; out AValue: String; out AValueLength: Double);
begin
  with GetQuery(59) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_LONG(:C, :POSITION, :LENGTH, :OFFSET, :VALUE, :VALUE_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LENGTH';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':OFFSET';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsFloat := ALength;
    Params[3].AsFloat := AOffset;
    ExecSQL;
    AValue := Params[4].AsString;
    AValueLength := Params[5].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_RAW" [2]
procedure TOCISysDbmsSql.ColumnValueRaw(const AC: Double; const APosition: Double; out AValue: TBlobData);
begin
  with GetQuery(60) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_RAW(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otRaw;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsBlob;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_RAW" [1]
procedure TOCISysDbmsSql.ColumnValueRaw(const AC: Double; const APosition: Double; out AValue: TBlobData; out AColumnError: Double; out AActualLength: Double);
begin
  with GetQuery(61) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_RAW(:C, :POSITION, :VALUE, :COLUMN_ERROR, :ACTUAL_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otRaw;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':COLUMN_ERROR';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':ACTUAL_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsBlob;
    AColumnError := Params[3].AsFloat;
    AActualLength := Params[4].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_ROWID" [2]
procedure TOCISysDbmsSql.ColumnValueRowid(const AC: Double; const APosition: Double; out AValue: String);
begin
  with GetQuery(62) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_ROWID(:C, :POSITION, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."COLUMN_VALUE_ROWID" [1]
procedure TOCISysDbmsSql.ColumnValueRowid(const AC: Double; const APosition: Double; out AValue: String; out AColumnError: Double; out AActualLength: Double);
begin
  with GetQuery(63) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.COLUMN_VALUE_ROWID(:C, :POSITION, :VALUE, :COLUMN_ERROR, :ACTUAL_LENGTH);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otROWID;
        ODataSize := 18;
      end;
      with Params.Add do begin
        OName := ':COLUMN_ERROR';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':ACTUAL_LENGTH';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
    AValue := Params[2].AsString;
    AColumnError := Params[3].AsFloat;
    AActualLength := Params[4].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [7]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const ANTab: TOCINumberTable; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(64) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :N_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':N_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ANTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [6]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const ACTab: TOCIVarchar2Table; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(65) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :C_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':C_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otLong;
        ODataSize := 2000;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ACTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [5]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const ADTab: TOCIDateTable; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(66) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :D_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':D_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ADTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [4]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const ABlTab: TOCIBlobTable; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(67) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :BL_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':BL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ABlTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [3]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const AClTab: TOCIClobTable; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(68) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :CL_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':CL_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(AClTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [2]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const ABfTab: TOCIBfileTable; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(69) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :BF_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':BF_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBFile;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(ABfTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_ARRAY" [1]
procedure TOCISysDbmsSql.DefineArray(const AC: Double; const APosition: Double; const AUrTab: TOCIUrowidTable; const ACnt: Double; const ALowerBound: Double);
begin
  with GetQuery(70) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_ARRAY(:C, :POSITION, :UR_TAB, :CNT, :LOWER_BOUND);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':UR_TAB';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otROWID;
        ODataSize := 18;
      end;
      with Params.Add do begin
        OName := ':CNT';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LOWER_BOUND';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].Assign(AUrTab);
    Params[3].AsFloat := ACnt;
    Params[4].AsFloat := ALowerBound;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [13]
procedure TOCISysDbmsSql.DefineColumn(const AC: Double; const APosition: Double; const AColumn: Double);
begin
  with GetQuery(71) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsFloat := AColumn;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [12]
procedure TOCISysDbmsSql.DefineColumn(const AC: Double; const APosition: Double; const AColumn: String; const AColumnSize: Double);
begin
  with GetQuery(72) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN, :COLUMN_SIZE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':COLUMN_SIZE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsString := AColumn;
    Params[3].AsFloat := AColumnSize;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [11]
procedure TOCISysDbmsSql.DefineColumnDT(const AC: Double; const APosition: Double; const AColumn: TDateTime);
begin
  with GetQuery(73) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsDateTime := AColumn;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [10]
procedure TOCISysDbmsSql.DefineColumn(const AC: Double; const APosition: Double; const AColumn: TOCIILOBStream);
begin
  with GetQuery(74) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AssignByRef(AColumn);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [9]
{***
procedure TOCISysDbmsSql.DefineColumn(const AC: Double; const APosition: Double; const AColumn: TOCIILOBStream);
begin
  with GetQuery(75) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AssignByRef(AColumn);
    ExecSQL;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [8]
procedure TOCISysDbmsSql.DefineColumn(const AC: Double; const APosition: Double; const AColumn: TOCIFILEStream);
begin
  with GetQuery(76) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AssignByRef(AColumn);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [7]
procedure TOCISysDbmsSql.DefineColumn(const AC: Double; const APosition: Double; const AColumn: String);
begin
  with GetQuery(77) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsString := AColumn;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [6]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [5]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [4]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [3]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [2]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN" [1]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN_CHAR"
procedure TOCISysDbmsSql.DefineColumnChar(const AC: Double; const APosition: Double; const AColumn: String; const AColumnSize: Double);
begin
  with GetQuery(84) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN_CHAR(:C, :POSITION, :COLUMN, :COLUMN_SIZE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':COLUMN_SIZE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsString := AColumn;
    Params[3].AsFloat := AColumnSize;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN_LONG"
procedure TOCISysDbmsSql.DefineColumnLong(const AC: Double; const APosition: Double);
begin
  with GetQuery(85) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN_LONG(:C, :POSITION);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN_RAW"
procedure TOCISysDbmsSql.DefineColumnRaw(const AC: Double; const APosition: Double; const AColumn: TBlobData; const AColumnSize: Double);
begin
  with GetQuery(86) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN_RAW(:C, :POSITION, :COLUMN, :COLUMN_SIZE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otRaw;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':COLUMN_SIZE';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsBlob := AColumn;
    Params[3].AsFloat := AColumnSize;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DEFINE_COLUMN_ROWID"
procedure TOCISysDbmsSql.DefineColumnRowid(const AC: Double; const APosition: Double; const AColumn: String);
begin
  with GetQuery(87) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.DEFINE_COLUMN_ROWID(:C, :POSITION, :COLUMN);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':POSITION';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':COLUMN';
        OParamType := odIn;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsFloat := APosition;
    Params[2].AsString := AColumn;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."DESCRIBE_COLUMNS"
// *** ERROR: dbGen: NOE135/SP - Parameter with type TABLE OF BOOLEAN/RECORD not supported (use TOCIQuery)

// Generated for: PL/SQL function "SYS.DBMS_SQL"."EXECUTE"
function TOCISysDbmsSql.Execute(const AC: Double): Double;
begin
  with GetQuery(89) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.EXECUTE(:C);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[1].AsFloat := AC;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."EXECUTE_AND_FETCH"
function TOCISysDbmsSql.ExecuteAndFetch(const AC: Double; const AExact: Boolean): Double;
begin
  with GetQuery(90) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('declare');
        SQL.Add('v0 boolean;');
        SQL.Add('begin');
        SQL.Add('if :EXACT is null then v0 := null; else v0 := (:EXACT = 1); end if;');
        SQL.Add(':RESULT := SYS.DBMS_SQL.EXECUTE_AND_FETCH(:C, v0);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':EXACT';
        OParamType := odIn;
        ODataType := otSmallInt;
        ODataSize := 4;
      end;
    end;
    Params[1].AsFloat := AC;
    Params[2].AsBoolean := AExact;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."FETCH_ROWS"
function TOCISysDbmsSql.FetchRows(const AC: Double): Double;
begin
  with GetQuery(91) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.FETCH_ROWS(:C);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[1].AsFloat := AC;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."IS_OPEN"
function TOCISysDbmsSql.IsOpen(const AC: Double): Boolean;
begin
  with GetQuery(92) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('declare');
        SQL.Add('v0 boolean;');
        SQL.Add('begin');
        SQL.Add('v0 := SYS.DBMS_SQL.IS_OPEN(:C);');
        SQL.Add('if v0 is null then :RESULT := null; elsif v0 then :RESULT := 1; else :RESULT := 0; end if;');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otSmallInt;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[1].AsFloat := AC;
    ExecSQL;
    Result := Params[0].AsBoolean;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."LAST_ERROR_POSITION"
function TOCISysDbmsSql.LastErrorPosition(): Double;
begin
  with GetQuery(93) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.LAST_ERROR_POSITION;');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."LAST_ROW_COUNT"
function TOCISysDbmsSql.LastRowCount(): Double;
begin
  with GetQuery(94) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.LAST_ROW_COUNT;');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."LAST_ROW_ID"
function TOCISysDbmsSql.LastRowId(): String;
begin
  with GetQuery(95) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.LAST_ROW_ID;');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    ExecSQL;
    Result := Params[0].AsString;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."LAST_SQL_FUNCTION_CODE"
function TOCISysDbmsSql.LastSqlFunctionCode(): Double;
begin
  with GetQuery(96) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.LAST_SQL_FUNCTION_CODE;');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL function "SYS.DBMS_SQL"."OPEN_CURSOR"
function TOCISysDbmsSql.OpenCursor(): Double;
begin
  with GetQuery(97) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add(':RESULT := SYS.DBMS_SQL.OPEN_CURSOR;');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':RESULT';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    ExecSQL;
    Result := Params[0].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."PARSE" [2]
procedure TOCISysDbmsSql.Parse(const AC: Double; const AStatement: String; const ALanguageFlag: Double);
begin
  with GetQuery(98) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.PARSE(:C, :STATEMENT, :LANGUAGE_FLAG);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':STATEMENT';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':LANGUAGE_FLAG';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AStatement;
    Params[2].AsFloat := ALanguageFlag;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."PARSE" [1]
procedure TOCISysDbmsSql.Parse(const AC: Double; const AStatement: TOCIVarchar2s; const ALb: Double; const AUb: Double; const ALfflg: Boolean; const ALanguageFlag: Double);
begin
  with GetQuery(99) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('declare');
        SQL.Add('v0 boolean;');
        SQL.Add('begin');
        SQL.Add('if :LFFLG is null then v0 := null; else v0 := (:LFFLG = 1); end if;');
        SQL.Add('SYS.DBMS_SQL.PARSE(:C, :STATEMENT, :LB, :UB, v0, :LANGUAGE_FLAG);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':STATEMENT';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otLong;
        ODataSize := 256;
      end;
      with Params.Add do begin
        OName := ':LB';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':UB';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':LFFLG';
        OParamType := odIn;
        ODataType := otSmallInt;
        ODataSize := 4;
      end;
      with Params.Add do begin
        OName := ':LANGUAGE_FLAG';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].Assign(AStatement);
    Params[2].AsFloat := ALb;
    Params[3].AsFloat := AUb;
    Params[4].AsBoolean := ALfflg;
    Params[5].AsFloat := ALanguageFlag;
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [20]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; out AValue: Double);
begin
  with GetQuery(100) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsFloat;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [19]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; out AValue: String);
begin
  with GetQuery(101) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [18]
procedure TOCISysDbmsSql.VariableValueDT(const AC: Double; const AName: String; out AValue: TDateTime);
begin
  with GetQuery(102) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsDateTime;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [17]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; out AValue: TOCIILOBStream);
begin
  with GetQuery(103) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    AValue := TOCIILOBStream.CreateAlone(Database, bmReadWrite, otBLOB);
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [16]
{***
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; out AValue: TOCIILOBStream);
begin
  with GetQuery(104) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    AValue := TOCIILOBStream.CreateAlone(Database, bmReadWrite, otCLOB);
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [15]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; out AValue: TOCIFILEStream);
begin
  with GetQuery(105) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    AValue := TOCIFILEStream.CreateAlone(Database, bmReadWrite, otBFile);
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].AssignByRef(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [14]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCINumberTable);
begin
  with GetQuery(106) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otFloat;
        ODataSize := 8;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [13]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCIVarchar2Table);
begin
  with GetQuery(107) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otLong;
        ODataSize := 2000;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [12]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCIDateTable);
begin
  with GetQuery(108) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otDateTime;
        ODataSize := 7;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [11]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCIBlobTable);
begin
  with GetQuery(109) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [10]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCIClobTable);
begin
  with GetQuery(110) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otCLOB;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [9]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCIBfileTable);
begin
  with GetQuery(111) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otBFile;
        ODataSize := 4;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [8]
{***
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; out AValue: String);
begin
  with GetQuery(112) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;
}
// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [7]
procedure TOCISysDbmsSql.VariableValue(const AC: Double; const AName: String; const AValue: TOCIUrowidTable);
begin
  with GetQuery(113) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odIn;
        IsPLSQLTable := True;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    Params[2].Assign(AValue);
    ExecSQL;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [6]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [5]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [4]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [3]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [2]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE" [1]
// *** ERROR: Oracle data type is not mapped to Delphi data type

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE_CHAR"
procedure TOCISysDbmsSql.VariableValueChar(const AC: Double; const AName: String; out AValue: String);
begin
  with GetQuery(120) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE_CHAR(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otString;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE_RAW"
procedure TOCISysDbmsSql.VariableValueRaw(const AC: Double; const AName: String; out AValue: TBlobData);
begin
  with GetQuery(121) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE_RAW(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otRaw;
        ODataSize := 255;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsBlob;
  end;
end;

// Generated for: PL/SQL procedure "SYS.DBMS_SQL"."VARIABLE_VALUE_ROWID"
procedure TOCISysDbmsSql.VariableValueRowid(const AC: Double; const AName: String; out AValue: String);
begin
  with GetQuery(122) do begin
    if SQL.Count = 0 then begin
      try
        SQL.BeginUpdate;
        SQL.Add('begin');
        SQL.Add('SYS.DBMS_SQL.VARIABLE_VALUE_ROWID(:C, :NAME, :VALUE);');
        SQL.Add('end;');
      finally
        SQL.EndUpdate;
      end;
      Params.Clear;
      with Params.Add do begin
        OName := ':C';
        OParamType := odIn;
        ODataType := otFloat;
        ODataSize := 8;
      end;
      with Params.Add do begin
        OName := ':NAME';
        OParamType := odIn;
        ODataType := otString;
        ODataSize := 255;
      end;
      with Params.Add do begin
        OName := ':VALUE';
        OParamType := odOut;
        ODataType := otROWID;
        ODataSize := 18;
      end;
    end;
    Params[0].AsFloat := AC;
    Params[1].AsString := AName;
    ExecSQL;
    AValue := Params[2].AsString;
  end;
end;

end.
