{***************************************************************************}
{                                                                           }
{  Copyright (c) 1999-2015 Sergiy Kurinny                                   }
{                                                                           }
{  This library is free software; you can redistribute it and/or            }
{  modify it under the terms of the GNU Lesser General Public               }
{  License version 2.1 as published by the Free Software Foundation         }
{  and appearing in the file license.txt which is included in the root      }
{  folder of the package.                                                   }
{                                                                           }
{  This library is distributed in the hope that it will be useful,          }
{  but WITHOUT ANY WARRANTY; without even the implied warranty of           }
{  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU        }
{  Lesser General Public License for more details.                          }
{                                                                           }
{***************************************************************************}
unit psc_expreval;

interface
{$I psc_defines.inc}

Uses
{$IFDEF D6}
  SqlTimSt,
  FMTBcd,
  variants,
{$ENDIF}

  Classes,
  Controls,
  DBCommon,
  DB,
  Forms,
  SysUtils,

  myla_system,
  myla_interfaces,
  myla_parser,

  psc_edit,
  psc_edit_date,
  psc_edit_parts,
  psc_calculator,
  psc_parser_date,
  psc_wrapper,
  psc_procs,
  psc_const;

{$Z+}
Const
  fldUNKNOWN = 0;
  fldZSTRING = 1;
  fldDATE = 2;
  fldBLOB = 3;
  fldBOOL = 4;
  fldINT16 = 5;
  fldINT32 = 6;
  fldFLOAT = 7;
  fldBCD = 8;
  fldBYTES = 9;
  fldTIME = 10;
  fldTIMESTAMP = 11;
  fldUINT16 = 12;
  fldUINT32 = 13;
  fldFLOATIEEE = 14;
  fldVARBYTES = 15;
  fldLOCKINFO = 16;
  fldCURSOR = 17;
  fldINT64 = 18;
  fldUINT64 = 19;
  fldADT = 20;
  fldARRAY = 21;
  fldREF = 22;
  fldTABLE = 23;
  fldDATETIME = 24;

  fldUNICODE          = $1007;          { Unicode }
  fldINT8            = 28;           { 8 bit signed number as defined in alctypes.h }
  fldUINT8           = 29;           { Unsigned 8 bit integer (Byte) as defined in alctypes.h }
  fldSINGLE          = 27;              { 32 bit floating point }
  fldDATETIMEOFFSET  = 36;              { DatetimeOffset structure for DBExpress }


Type
  TPSCFilterFunc = (fUnknown,fUpper,fLower,fSubStr,fTrim,fTrimLeft,fTrimRight,
    fYear,fMonth,fDay,fHour,fMinute,fSecond,fGetDate,
    fDate,fTime);

  TPSCFilterOp = (opEQ,opNE,opGE,opLE,opGT,opLT,opBlank,opAND,opOR,opNOT,
    opADD,opMINUS,opSUB,opMUL,opDIV,opMOD,opREM,
    opLike,opIn,
    opIsNull,opIsNotNull,opExtComp,opNotExtComp,opTwoFieldsComp);
  TPSCSupportedOps = Set Of TPSCFilterOp;
  TPSCSupportedFuncs = Set Of TPSCFilterFunc;

  {don't use NODEClass from dbcommon cause other CAN records depend on Z+ size
   of this NODEClass declaration, same with CANOp}
  NODEClass = (
    nodeNULL,
    nodeUNARY,
    nodeBINARY,
    nodeCOMPARE,
    nodeFIELD,
    nodeCONST,
    nodeTUPLE,
    nodeCONTINUE,
    nodeUDF,
    nodeLIST,
    nodeFUNC,
    nodeLISTELEM
    );

  pCANOp = ^CANOp;
  CANOp = (
    canNOTDEFINED,
    canISBLANK,
    canNOTBLANK,
    canEQ,
    canNE,
    canGT,
    canLT,
    canGE,
    canLE,
    canNOT,
    canAND,
    canOR,
    canTUPLE2,
    canFIELD2,
    canCONST2,
    canMINUS,
    canADD,
    canSUB,
    canMUL,
    canDIV,
    canMOD,
    canREM,
    canSUM,
    canCOUNT,
    canMIN,
    canMAX,
    canAVG,
    canCONT,
    canUDF2,
    canCONTINUE2,
    canLIKE,
    canIN,
    canLIST2,
    canUPPER,
    canLOWER,
    canFUNC2,
    canLISTELEM2,
    canASSIGN
    );

  pCANHdr = ^CANHdr;
  CANHdr = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
  End;

  pCANUnary = ^CANUnary;
  CANUnary = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iOperand1: Word;
  End;

  pCANBinary = ^CANBinary;
  CANBinary = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iOperand1: Word;
    iOperand2: Word;
  End;

  pCANField = ^CANField;
  CANField = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iFieldNum: Word;
    iNameOffset: Word;
  End;

  pCANConst = ^CANConst;
  CANConst = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iType: Word;
    iSize: Word;
    iOffset: Word;
  End;

  pCANTuple = ^CANTuple;
  CANTuple = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iSize: Word;
  End;

  pCANContinue = ^CANContinue;
  CANContinue = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iContOperand: Word;
  End;

  pCANCompare = ^CANCompare;
  CANCompare = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    bCaseInsensitive: WordBool;
    iPartialLen: Word;
    iOperand1: Word;
    iOperand2: Word;
  End;

  pCANFunc = ^CANFunc;
  CANFunc = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iNameOffset: Word;
    iElemOffset: Word;
  End;

  pCANListElem = ^CANListElem;
  CANListElem = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iOffset: Word;
    iNextOffset: Word;
  End;

Const
  iLangSQL = 0;
  iDbaseExpr = 2;

Type
  pCANUdf = ^CANUdf;
  CANUdf = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iOffSzFuncName: Word;
    iOperands: Word;
    iDrvDialect: Word;
    iOffSzUDF: Word;
  End;

  pCANList = ^CANList;
  CANList = Packed Record
    nodeClass: NODEClass;
    canOp: CANOp;
    iType: Word;
    iTotalSize: Word;
    iElemSize: Word;
    iElems: Word;
    iOffset: Word;
  End;

{$IFNDEF D6}
Const
  CANEXPRVERSION = 2;
{$ENDIF}

Type
  ppCANExpr = ^pCANExpr;
  pCANExpr = ^CANExpr;
  CANExpr = Packed Record
    iVer: Word;
    iTotalSize: Word;
    iNodes: Word;
    iNodeStart: Word;
    iLiteralStart: Word; 
  End;

Const
  FldTypeMap: TFieldMap = (fldUNKNOWN,fldZSTRING,fldINT16,fldINT32,fldUINT16, // 0..4
    fldBOOL,fldFLOAT,fldFLOAT,fldBCD,fldDATE,fldTIME,fldDateTime,   // 5..11
    fldBYTES,fldVARBYTES,fldINT32,fldBLOB,fldBLOB,fldBLOB,fldBLOB,  // 12..18
    fldBLOB,fldBLOB,fldBLOB,fldCURSOR,fldZSTRING,fldZSTRING,        // 19..24
    fldINT64,fldADT,fldArray,fldREF,fldTABLE,fldBLOB,fldBLOB,       // 25..31
    fldUNKNOWN,fldUNKNOWN,fldUNKNOWN,fldZSTRING{$IFDEF D6},fldTimeStamp,fldBCD{$ENDIF}  // 32..37
    {$IFDEF D2010}, fldZSTRING, fldBLOB, fldTIMESTAMP, fldUNKNOWN, // 38..41
    fldUINT32, fldINT16, fldBYTES, fldFLOAT, fldUNKNOWN, fldUNKNOWN, fldUNKNOWN, //42..48
    fldUNKNOWN, fldBLOB, fldBYTES {$ENDIF} ); //49..51

// RadStudio 2010
//  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
//    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate, ftTime, ftDateTime, // 5..11
//    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo, // 12..18
//    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar, ftWideString, // 19..24
//    ftLargeint, ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, // 25..31
//    ftVariant, ftInterface, ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd, // 32..37
//    ftFixedWideChar, ftWideMemo, ftOraTimeStamp, ftOraInterval, // 38..41
//    ftLongWord, ftShortint, ftByte, ftExtended, ftConnection, ftParams, ftStream, //42..48
//    ftTimeStampOffset, ftObject, ftSingle); //49..51
{$Z-}

Type
  TPSCEmulDataSet = Class;

  TPSCSearchType = (stFirst,stNext,stPrior,stLast);

  TPSCCustomExprEval = Class(TPSCComponent)
  private
    FExprParser: TExprParser;
    FExtExprParser: TExprParser;
    FDataSet: TDataSet;
    FFilter: String;
    FFilterFailed: Boolean;
    FSearchInLoop: Boolean;
    FParams: TPSCFields;
    FFields: TPSCFields;
    FOnChange: TPSCNotifyEvent;

    Function IsFieldsStored: boolean;
    Function IsParamsStored: boolean;
    Procedure SetExprParser(Value: TExprParser);
    Procedure SetFilter(Value: String);
    Procedure SetParams(Value: TPSCFields);
    Procedure SetFields(Value: TPSCFields);
    Procedure UpdCollectEvent(Sender: TObject; Item: TPSCNamedItem);

    Function GetEvalResult: Variant;
    Function GetFilterData: Integer;
    Function GetLiteralStart: Integer;
    Function GetNodeStart: Integer;
    Function UnaryNode(ANode: PCANUnary): Variant;
    Function BinaryNode(ANode: PCANBinary): Variant;
    Function CompareNode(ANode: PCANCompare): Variant;
    Function FieldNode(ANode: pCANField): Variant;
    Function FuncNode(ANode: pCANFunc): Variant;
    Function ListOfValues(ANode: pCANListElem): Variant;
    Function GetNodeByOffset(AOffSet: Integer): PCANHdr;
    Function GetNodeValue(AOffSet: Integer): Variant;
    Function CalcExpression(ANode: PCANHdr): Variant;

    Property FilterData: Integer read GetFilterData;
    Property LiteralStart: Integer read GetLiteralStart;
    Property NodeStart: Integer read GetNodeStart;

    Function LoopDataSet(Search: TPSCSearchType): Boolean;
    Function FindRecord(Search: TPSCSearchType): Boolean;
    Function GetExprParser: TExprParser;
  protected
    Function CreateExprParser: TExprParser; virtual;
    Function FilterFormula: String; virtual;

    Procedure FieldsFromColl(EmulDataSet: TPSCEmulDataSet); virtual;
    Procedure InitParser; virtual;
    Procedure DestroyParser; virtual;
    Procedure SetDataSet(Value: TDataSet); virtual;
    Procedure DoOnGetFieldValue(Const AFieldName: String; Var AValue: Variant);
      virtual;
    Procedure DoOnChange(Sender: TObject); virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    Property FilterFailed: Boolean read FFilterFailed write FFilterFailed;
    Property ExtExprParser: TExprParser read FExtExprParser write
      FExtExprParser;
    Property DataSet: TDataSet read FDataSet write SetDataSet;
    Property Expression: String read FFilter write SetFilter;
    Property Filter: String read FFilter write SetFilter;
    Property Fields: TPSCFields read FFields write SetFields stored
      IsFieldsStored;
    Property Params: TPSCFields read FParams write SetParams stored
      IsParamsStored;
    Property SearchInLoop: Boolean read FSearchInLoop write FSearchInLoop default
      False;
    Property OnChange: TPSCNotifyEvent read FOnChange write FOnChange;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure Assign(ASource: TPersistent); override;

    Function FindFirstRecord: Boolean;
    Function FindNextRecord: Boolean;
    Function FindPriorRecord: Boolean;
    Function FindLastRecord: Boolean;
    Function FindRecordEx(ADirection: TPSCSearchType): Boolean;

    Property EvalResult: Variant read GetEvalResult;
    Property ExprParser: TExprParser read FExprParser write SetExprParser;
  published
  End;

  TPSCExprEval = Class(TPSCCustomExprEval)
  published
    Property DataSet;
    Property Expression;
    Property Params;
    Property Fields;
    Property OnChange;
    Property SearchInLoop;
  End;

  TPSCEmulDataSet = Class(TDataSet)
  protected
    // Function AllocRecordBuffer: PChar; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    // Procedure FreeRecordBuffer(Var Buffer: PChar); override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    // Procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    // Function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    //Function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    //Function GetRecordSize: Word; override;
    function GetRecordSize: Word; override;
    //Procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    Procedure InternalClose; override;
    Procedure InternalDelete; override;
    Procedure InternalFirst; override;
    //Procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    Procedure InternalHandleException; override;
    Procedure InternalInitFieldDefs; override;
    //Procedure InternalInitRecord(Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    Procedure InternalLast; override;
    Procedure InternalOpen; override;
    Procedure InternalPost; override;
    //Procedure InternalSetToRecord(Buffer: PByte); override;   //(Buffer: PChar); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    //Function IsCursorOpen: Boolean; override;
    function IsCursorOpen: Boolean; override;
    //Procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    //Procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    //Procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    //procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); override;
  public
    Function AddField(Const AName: String; AFieldType: TFieldType): TField;
    Procedure AssignDataSet(Value: TDataSet);
  End;

{-------------------------------------------------------------------------}

Function PSCFieldTypeToDBFieldType(AType: TPSCFieldType): TFieldType;
Function PSCCreateFilterParserEx(DataSet: TDataSet; Const Expr: WideString;
  ParserOptions: TParserOptions): TExprParser;

{-------------------------------------------------------------------------}

implementation

{-------------------------------------------------------------------------}

Function PSCPerformCANOp(AOperator: CANOp; AOp1,AOp2: Variant): Variant;forward;
Function PSCPerformInCompare(AOp1,AOp2: Variant): Boolean;forward;
Function PSCCANConstToVariant(ANode: PCANConst; ValuesStart: Pointer;
  Var FieldType: TPSCFieldType): Variant;forward;
Function PSCDecodeFuncName(Const AFuncName: String): TPSCFilterFunc;forward;
Function PSCRunFunction(AFunc: TPSCFilterFunc; Const AParams: Variant): Variant;forward;
Function PSCVariantToDateTime(Const Value: Variant): TDateTime;forward;

{-------------------------------------------------------------------------}

Procedure PSCGetVarAsStringStr(Var Operand: Variant);
Begin
  If varIsNull(Operand) Or varIsEmpty(Operand) Then
    Operand := '';
End;

{-------------------------------------------------------------------------}

Const
  sFunctionName: Array[TPSCFilterFunc] Of String[9] = ('', 'upper', 'lower', //don't resource
    'substring', 'trim', 'trimleft', 'trimright', 'year', 'month',//don't resource
    'day', 'hour', 'minute', 'second', 'getdate', 'date', 'time');//don't resource

{-------------------------------------------------------------------------}

Constructor TPSCCustomExprEval.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FParams := TPSCFields.Create(Self,TPSCField);
  FFields := TPSCFields.Create(Self,TPSCField);
  FParams.OnUpdate := UpdCollectEvent;
  FFields.OnUpdate := UpdCollectEvent;
End;

{-------------------------------------------------------------------------}

Destructor TPSCCustomExprEval.Destroy;
Begin
  DestroyParser;
  FParams.Free;
  FFields.Free;
  Inherited Destroy;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.DoOnChange(Sender: TObject);
Begin
  If Assigned(OnChange) Then
    OnChange(Sender);
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.UpdCollectEvent(Sender: TObject; Item:
  TPSCNamedItem);
Begin
  DestroyParser;
  FFilterFailed := False;
  DoOnChange(Self);
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.SetExprParser(Value: TExprParser);
Begin
  DestroyParser;
  FExprParser := Value;
  FFilter := '';
  FFilterFailed := False;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.SetDataSet(Value: TDataSet);
Begin
  If FDataSet = Value Then
    Exit;
  FDataSet := Value;
  If Assigned(Value) Then
    Value.FreeNotification(Self);
  DestroyParser;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.Notification(AComponent: TComponent; Operation:
  TOperation);
Begin
  Inherited Notification(AComponent,Operation);
  If (Operation = opRemove) And (AComponent = DataSet) Then
    DataSet := Nil;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.SetFilter(Value: String);
Begin
  Value := PSCTrim(Value);
  If FFilter = Value Then
    Exit;
  FFilter := Value;
  If Value <> '' Then
    Begin
      FFilterFailed := False;
      FParams.SafeParseParams(FFilter);
    End;
  DestroyParser;
End;

{-------------------------------------------}

Function PSCFieldTypeToDBFieldType(AType: TPSCFieldType): TFieldType;
Const
  FieldTypeMap: Array[TPSCFieldType] Of TFieldType =
  ( ftUnknown,
    ftInteger,
    ftDateTime,
    ftDateTime,
    ftDateTime,
    ftCurrency,
    ftFloat,
    ftString,
    ftBoolean,
    ftUnknown,
    ftUnknown,
    ftmemo,
    ftWideString
  );
Begin
  Result := FieldTypeMap[AType];
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.FieldsFromColl(EmulDataSet: TPSCEmulDataSet);
Var
  I: Integer;
Begin
  For I := 0 To Fields.Count - 1 Do
    If EmulDataSet.FindField(Fields[I].Name) = Nil Then
      EmulDataSet.AddField(Fields[I].Name,PSCFieldTypeToDBFieldType(Fields[I].DataType));
End;

{-------------------------------------------------------------------------}

{$IFDEF D6}
{$IFNDEF D7}
  {$DEFINE PSCPATCHVARMGR}
{$ENDIF}
{$ENDIF}

{$IFDEF PSCPATCHVARMGR}
var
  _OldVariantManager:TVariantManager;
  _VariantManagerCounter:Integer=0;
  _NewVariantManager:TVariantManager;

function _VarToBool(const V: Variant): Boolean;
begin
  Result:=LongBool(_OldVariantManager.VarToBool(V));
end;
{$ENDIF}

{-------------------------------------------------------------------------}

{$IFDEF PSCPATCHVARMGR}
Procedure PSCPatchVariantManager;
begin
    If _VariantManagerCounter=0 then
    begin
      GetVariantManager(_OldVariantManager);
      _NewVariantManager:=_OldVariantManager;
      _NewVariantManager.VarToBool:=@_VarToBool;
      SetVariantManager(_NewVariantManager);
    end;
    inc(_VariantManagerCounter);
end;
{$ENDIF}

{-------------------------------------------------------------------------}

{$IFDEF PSCPATCHVARMGR}
Procedure PSCUnPatchVariantManager;
begin
{$IFDEF D6}
    If _VariantManagerCounter=1 then
      SetVariantManager(_OldVariantManager);
    dec(_VariantManagerCounter);
{$ENDIF}
end;
{$ENDIF}

{-------------------------------------------------------------------------}

Function PSCCreateFilterParserEx(DataSet: TDataSet; Const Expr: WideString;
  ParserOptions: TParserOptions): TExprParser;
Begin
  Result:=nil;
  Try
    Result := TExprParser.Create(DataSet,Expr,
      [],ParserOptions,'',Nil,FldTypeMap);
  Except
    PSCError(PSCConsts.ErrBadFilter);
  End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.CreateExprParser: TExprParser;
Var
  FalseDataSet: TPSCEmulDataSet;
  Simplified: String;
Begin
  Simplified := PSCReplaceWithParams(FilterFormula,Params);
  If Simplified = '' Then
    PSCError(PSCConsts.ErrEmptyFilter);

  FalseDataSet := TPSCEmulDataSet.Create(Nil);
  Try
    If Assigned(DataSet) Then
      FalseDataSet.AssignDataSet(DataSet);
    FieldsFromColl(FalseDataSet);

    Result := PSCCreateFilterParserEx(FalseDataSet,Simplified,[poExtSyntax]);

  Finally
    FalseDataSet.Free;
  End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FilterFormula: String;
Begin
  Result := FFilter;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.DestroyParser;
Begin
  FExprParser.Free;
  FExprParser:=nil;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.InitParser;
Begin
  If Not Assigned(FExprParser) Then
    FExprParser := CreateExprParser;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetExprParser: TExprParser;
Begin
  If Assigned(ExtExprParser) Then
    Result := ExtExprParser
  Else
    Result := FExprParser;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetEvalResult: Variant;
Begin
  If ((FFilter='') and (ExtExprParser=nil)) or
     ((ExtExprParser<>nil) and (ExtExprParser.FilterData=nil)) then
  begin
    Result:=True;
    exit;
  end;

  If FilterFailed Then
    Begin
      Result := False;
      Exit;
    End;
  Try
    If Not Assigned(ExtExprParser) Then
      InitParser;
    Result := CalcExpression(GetNodeByOffset(NodeStart));
  Except
    Result := False;
    FilterFailed := True;
    Raise;
    Exit;
  End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetFilterData: Integer;
Begin
  Result := Integer(GetExprParser.FilterData);
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetLiteralStart: Integer;
Begin
  Result := PCANExpr(FilterData).iLiteralStart;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetNodeStart: Integer;
Begin
  Result := PCANExpr(FilterData).iNodeStart;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.SetParams(Value: TPSCFields);
Begin
  FParams.Assign(Value);
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.SetFields(Value: TPSCFields);
Begin
  FFields.Assign(Value);
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetNodeByOffset(AOffSet: Integer): PCANHdr;
Begin
  Result := PCANHdr(FilterData + AOffSet);
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.CalcExpression(ANode: PCANHdr): Variant;
Var
  FieldType: TPSCFieldType;
Begin
  Case pCanHdr(ANode).nodeClass Of
    nodeNULL:
      Result := Null;
    nodeUNARY:
      Result := UnaryNode(pCANUnary(ANode));
    nodeBINARY:
      Result := BinaryNode(pCANBinary(ANode));
    nodeCOMPARE:
      Result := CompareNode(pCANCompare(ANode));
    nodeFIELD:
      Result := FieldNode(pCANField(ANode));
    nodeCONST:
      Result := PSCCANConstToVariant(PCANConst(ANode),
        Pointer(FilterData + LiteralStart),
        FieldType);
    nodeTUPLE:
      Result := Null; // Node is a record
    nodeCONTINUE:
      Result := Null;
    nodeUDF:
      Result := Null;
    nodeLIST:
      Result := Null; // Node is a list
    nodeFUNC:
      Result := FuncNode(pCANFunc(ANode));
    nodeLISTELEM:
      Result := ListOfValues(pCANListElem(ANode));
  End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.UnaryNode(ANode: PCANUnary): Variant;
Begin
  With ANode^ Do
    Result := PSCPerformCANOp(canOp,GetNodeValue(iOperand1),UnAssigned);
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.BinaryNode(ANode: PCANBinary): Variant;
Begin
  With ANode^ Do
    Result :=
      PSCPerformCANOp(canOp,GetNodeValue(iOperand1),GetNodeValue(iOperand2));
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.CompareNode(ANode: PCANCompare): Variant;
Var
  Op1,Op2: Variant;
Begin
  With ANode^ Do
    Begin
      Op1 := GetNodeValue(iOperand1);
      Op2 := GetNodeValue(iOperand2);
      If bCaseInsensitive Then
        Begin
          PSCGetVarAsStringStr(Op1);
          Op1 := PSCUpperCase(Op1);
          PSCGetVarAsStringStr(Op2);
          Op2 := PSCUpperCase(Op2);
        End;
      If canOp = canLike Then
        Result := PSCPerformLikeCompare(op1,op2)
      Else
        Begin
          Result := Op1 = Op2;
          If canOp = canNE Then
            Result := Not Result;
        End;
    End;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.DoOnGetFieldValue(Const AFieldName: String; Var
  AValue: Variant);
Begin
  AValue := FFields.FieldValues[AFieldName];
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FieldNode(ANode: pCANField): Variant;
Var
  FieldName: String;
  Field: TField;
Begin
  Result := Unassigned;
  FieldName := String(PAnsiChar{PChar}(FilterData + LiteralStart + ANode.iNameOffset));
  If Assigned(DataSet) And DataSet.Active Then
    Begin
      Field := DataSet.FieldByName(FieldName);
      Result := Field.Value;
    End
  Else
    DoOnGetFieldValue(FieldName,Result);
End;

{-------------------------------------------------------------------------}

Function PSCPerformCANOp(AOperator: CANOp; AOp1,AOp2: Variant): Variant;

   function VariantsEqual(const AOp1,AOp2: Variant):Boolean;
   begin
     Result:= (VarType(AOp1)=VarType(AOp1)) and (AOp1 = AOp2);
   end;

   function VariantIsGreater(const AOp1,AOp2: Variant):Boolean;
   var
     AOp1IsNull,AOp2IsNull:Boolean;
   begin
     AOp1IsNull:=varIsNull(AOp1) Or varIsEmpty(AOp1);
     AOp2IsNull:=varIsNull(AOp2) Or varIsEmpty(AOp2);

     If AOp1IsNull and AOp2IsNull then
       Result:=False
     else
     If AOp1IsNull then
       Result:=False
     else
     If AOp2IsNull then
       Result:=True
     else
       Result:=AOp1>AOp2;
   end;

   function VariantIsLess(const AOp1,AOp2: Variant):Boolean;
   begin
     Result:=VariantIsGreater(AOp2,AOp1) and (not VariantsEqual(AOp2,AOp1));
   end;

Begin
  try
    If AOperator In [canLIKE,canUPPER,canLOWER] Then
      PSCGetVarAsStringStr(AOp1);
    Case AOperator Of
      canNOTDEFINED:
        Result := Null;
      canISBLANK:
        Result := VarIsNull(AOp1);
      canNOTBLANK:
        Result := Not VarIsNull(AOp1);
      canEQ:
        Result := VariantsEqual(AOp1,AOp2);
      canNE:
        Result := not VariantsEqual(AOp1,AOp2);
      canGT:
        Result := VariantIsGreater(AOp1,AOp2);
      canLT:
        Result := VariantIsLess(AOp1,AOp2);
      canGE:
        Result := not VariantIsLess(AOp1,AOp2);
      canLE:
        Result := not VariantIsGreater(AOp1,AOp2);
      canNOT:
        Result := Not AOp1;
      canAND:
        Result := AOp1 And AOp2;
      canOR:
        Result := AOp1 Or AOp2;
      canMINUS:
        Result := -AOp1;
      canADD:
        Result := AOp1 + AOp2;
      canSUB:
        Result := AOp1 - AOp2;
      canMUL:
        Result := AOp1 * AOp2;
      canDIV:
        Result := AOp1 / AOp2;
      canMOD:
        Result := AOp1 Mod AOp2;
      canREM:
        Result := AOp1 Mod AOp2;
      canSUM:
        Result := Null;
      canCONT:
        Result := Null; 
      canLIKE:
        Result := PSCPerformLikeCompare(AOp1,AOp2);
      canIN:
        Result := PSCPerformInCompare(AOp1,AOp2);
      canUPPER:
        Result := PSCUpperCase(AOp1);
      canLOWER:
        Result := PSCLowerCase(AOp1);
      canASSIGN:
        Result := VarIsNull(AOp1);
    Else
      Result := Null;
    End;
  finally
  end;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.GetNodeValue(AOffSet: Integer): Variant;
Begin
  Result := CalcExpression(GetNodeByOffset(NodeStart + AOffset));
End;

{-------------------------------------------------------------------------}

Function PSCPerformInCompare(AOp1,AOp2: Variant): Boolean;
Var
  Save: Variant;
  I,Top: Integer;
Begin
  {----===== Make sure that AOp1 is comparing value and AOp2 is =====----}
  {----=====             set of possible values                 =====----}
  If varType(AOp1) = varArray Then
    Begin
      Save := AOp2;
      AOp2 := AOp1;
      AOp1 := Save;
    End;
  Result := True;
  Top := VarArrayHighBound(AOp2,1);
  For I := VarArrayLowBound(AOp2,1) To Top Do
    If AOp1 = AOp2[I] Then
      Exit;
  Result := False;
End;

{-------------------------------------------------------------------------}

Function PSCVariantToDateTime(Const Value: Variant): TDateTime;
Begin
  Case varType(Value) Of
    varOleStr,varStrArg,varString{$IFDEF D2009},varUString,vtWideString{$ENDIF}:
      Result := PSCStrToDateTime(Value);
  Else
    Result := Value;
  End;
End;

{-------------------------------------------------------------------------}

Function PSCRunFunction(AFunc: TPSCFilterFunc; Const AParams: Variant): Variant;
Var
  Index,Len,ParamCount: Integer;
  Value: Variant;
  StrParam: String;
Begin
  ParamCount := varArrayHighBound(AParams,1);
  Result := Null;
  If (AFunc In [fYear,fMonth,fDay,fHour,fMinute,fSecond,fDate,fTime]) And
    (varIsNull(AParams[1]) Or varIsEmpty(AParams[1])) Then
    Exit;

  If AFunc In [fUpper,fLower,fSubStr,fTrim,fTrimLeft,fTrimRight] Then
    Begin
      Value := AParams[1];
      PSCGetVarAsStringStr(Value);
    End;

  Case AFunc Of
    fUpper:
      Result := PSCUpperCase(Value);
    fLower:
      Result := PSCLowerCase(Value);
    fSubStr:
      Begin
        If ParamCount > 1 Then
          Index := AParams[2]
        Else
          Index := 1;
        StrParam := Value;
        If ParamCount > 2 Then
          Len := AParams[3]
        Else
          Len := Length(StrParam);
        Result := Copy(StrParam,Index,Len);
      End;
    fTrim:
      Result := PSCTrim(Value);
    fTrimLeft:
      Begin
        If ParamCount > 1 Then
          StrParam := AParams[2]
        Else
          StrParam := ' ';
        Result := PSCTrimSeparatorsLeft(Value, [StrParam[1]]);
      End;
    fTrimRight:
      Begin
        If ParamCount > 1 Then
          StrParam := AParams[2]
        Else
          StrParam := ' ';
        Result := PSCTrimSeparatorsRight(Value, [StrParam[1]]);
      End;
    fYear:
      Result := PSCGetDateYear(PSCVariantToDateTime(AParams[1]));
    fMonth:
      Result := PSCGetDateMonth(PSCVariantToDateTime(AParams[1]));
    fDay:
      Result := PSCGetDateDay(PSCVariantToDateTime(AParams[1]));
    fHour:
      Result := PSCGetTimeElement(PSCVariantToDateTime(AParams[1]),1);
    fMinute:
      Result := PSCGetTimeElement(PSCVariantToDateTime(AParams[1]),2);
    fSecond:
      Result := PSCGetTimeElement(PSCVariantToDateTime(AParams[1]),3);
    fGetDate:
      Result := PSCDateOf(PSCNow);
    fDate:
      Result := PSCDateOf(PSCVariantToDateTime(AParams[1]));
    fTime:
      Result := PSCTimeOf(PSCVariantToDateTime(AParams[1]));
  End;
End;

{-------------------------------------------------------------------------}

Function PSCDecodeFuncName(Const AFuncName: String): TPSCFilterFunc;
Var
  I: TPSCFilterFunc;
Begin
  Result := fUnknown;
  For I := Low(TPSCFilterFunc) To High(TPSCFilterFunc) Do
    If sFunctionName[I] = PSCLowerCase(AFuncName) Then
      Begin
        Result := I;
        Exit;
      End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FuncNode(ANode: pCANFunc): Variant;
Var
  FuncName: String;
  Params: Variant;
Begin
  Result := Null;
  With ANode^ Do
    Begin
      FuncName := String(PAnsiChar{PChar}(FilterData + LiteralStart + ANode.iNameOffset));
      Params := ListOfValues(pCANListElem(GetNodeByOffset(NodeStart +
        iElemOffset)));
      Result := PSCRunFunction(PSCDecodeFuncName(FuncName),Params);
    End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.ListOfValues(ANode: pCANListElem): Variant;
Var
  I: Integer;
  CurNode: pCANListElem;
Begin
  CurNode := ANode;
  {----===== Calculating the number of elements in the list =====----}
  I := 0;
  While True Do
    Begin
      Inc(I);
      If CurNode^.iNextOffset = 0 Then
        break;
      CurNode := pCanListElem(GetNodeByOffset(NodeStart +
        CurNode^.iNextOffset));
    End;
  {----===== Calculating the number of elements in the list =====----}
  Result := varArrayCreate([1,I],varVariant);
  I := 1;
  While True Do
    Begin
      Result[I] := CalcExpression(PCANHdr(GetNodeByOffset(NodeStart +
        ANode^.iOffset)));
      If ANode^.iNextOffset = 0 Then
        break;
      ANode := pCanListElem(GetNodeByOffset(NodeStart + ANode^.iNextOffset));
      Inc(I);
    End;
End;

{-------------------------------------------------------------------------}

Procedure TPSCCustomExprEval.Assign(ASource: TPersistent);
Var
  Source: TPSCCustomExprEval;
Begin
  Inherited Assign(ASource);
  If ASource Is TPSCCustomExprEval Then
    Begin
      Source := TPSCCustomExprEval(ASource);
      FDataSet := Source.DataSet;
      FFilter := Source.FFilter;
      ExprParser := Source.ExprParser;

      FParams.Assign(Source.Params);
    End;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.LoopDataSet(Search: TPSCSearchType): Boolean;
Begin
  Result := True;
  If Search >= stPrior Then
    While Not DataSet.BOF Do
      Begin
        If EvalResult Then
          Exit;
        DataSet.Prior
      End
  Else
    While Not DataSet.EOF Do
      Begin
        If EvalResult Then
          Exit;
        DataSet.Next;
      End;
  Result := False;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.IsFieldsStored: boolean;
Begin
  Result := Fields.Count > 0;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.IsParamsStored: boolean;
Begin
  Result := Params.Count > 0;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FindRecord(Search: TPSCSearchType): Boolean;
Var
  BMark: TBytes;  // TBookMarkStr;
  SavedCursor: TCursor;
Begin
  Result := False;
  If Not (Assigned(DataSet) And DataSet.Active) Then
    Exit;

  SavedCursor := Screen.Cursor;
  DataSet.DisableControls;
  Try
    Screen.Cursor := crHourGlass;

    BMark := DataSet.Bookmark;
    Case Search Of
      stFirst: DataSet.First;
      stNext: DataSet.Next;
      stPrior: DataSet.Prior;
      stLast: DataSet.Last;
    End;
    Result := LoopDataSet(Search);
    If Not Result Then
      DataSet.BookMark := BMark;
  Except
    DataSet.BookMark := BMark;
    DataSet.EnableControls;
    Screen.Cursor := SavedCursor;
    Raise;
    Exit;
  End;
  DataSet.EnableControls;
  Screen.Cursor := SavedCursor;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FindFirstRecord: Boolean;
Begin
  Result := FindRecord(stFirst);
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FindNextRecord: Boolean;
Begin
  Result := FindRecord(stNext);
  If Not Result Then
    If SearchInLoop Then
      Result := FindFirstRecord;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FindPriorRecord: Boolean;
Begin
  Result := FindRecord(stPrior);
  If Not Result Then
    If SearchInLoop Then
      Result := FindLastRecord;
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FindRecordEx(ADirection: TPSCSearchType): Boolean;
begin
  Case ADirection Of
  stFirst:
    Result:=FindFirstRecord;
  stNext:
    Result:=FindNextRecord;
  stPrior:
    Result:=FindPriorRecord;
  else
    Result:=FindLastRecord;
  end;  
end;

{-------------------------------------------------------------------------}

Const
  cAnyCharNumber = '%';
  cOneChar = '_';
  cExtendedComparison = '*';

  sListItemDivider = ',';
  sListOpenBracket = '(';
  sListCloseBracket = ')';
  sOperationSepOpenBracket = '(';
  sOperationSepCloseBracket = ')';
  sFunctionParamOpenBracket = '(';
  sFunctionParamCloseBracket = ')';
  sFieldOpenBracket = '[';
  sFieldCloseBracket = ']';

{-------------------------------------------------------------------------}

Function _PSCCANConstToVariant(ANode: PCANConst; ValuePtr: Pointer;
  Var FieldType: TPSCFieldType): Variant;
Type
  PWordBool = ^WordBool;
  PSmallInt = ^SmallInt;
Var
  Offs: Integer;
  TimeStamp: TTimeStamp;
  DateData: Double;
  S: String;
  PBCDValue: ^TBCD;
  CurrValue: Currency;
  MyBoolean: Boolean;
  {$IFDEF D6}
  MyTimeStamp: TSQLTimeStamp;
  {$ENDIF}
Begin
  With ANode^ Do
    Begin
      Offs := Integer(ValuePtr);
      FieldType := FT_UNK;
      Result := Null;

      {$IFNDEF D6}
      If iType=fldTIMESTAMP then   // not sure if really needed
        iType:=fldDateTime;        //
      {$ENDIF}

      Case iType Of
        fldUNICODE:
          begin
            S := String(PWideChar(Offs+2));
            Result := S;
            FieldType := FT_STRING;
          end;
        fldZSTRING:
          Begin
            S := String(PAnsiChar{PChar}(Offs));
            Result := S;
            FieldType := FT_STRING;
          End;
        fldDATE:
          Begin
            Cardinal(TimeStamp.Date) := PCardinal(Offs)^;
            TimeStamp.Time := 0;
            Result:=VarFromDateTime(PSCDateOf(TimeStampToDateTime(TimeStamp)));
            FieldType := FT_DATE;
          End;
        fldTIME:
          Begin
            Cardinal(TimeStamp.Time) := PCardinal(Offs)^;
            TimeStamp.Date := 1;
            Result:=VarFromDateTime(PSCTimeOf(TimeStampToDateTime(TimeStamp)));
            FieldType := FT_TIME;
          End;
        fldDateTime:
          Begin
            DateData := PDouble(Offs)^;
            Result:=VarFromDateTime(TimeStampToDateTime(MSecsToTimeStamp(DateData)));
            FieldType := FT_DATETIME;
          End;
        {$IFDEF D6}
        fldTIMESTAMP:
          Begin
            MyTimeStamp:=PSQLTimeStamp(Offs)^;
            Result:=VarFromDateTime(SQLTimeStampToDateTime(MyTimeStamp));
            FieldType := FT_DATETIME;
          End;
        {$ENDIF}
        fldBOOL:
          Begin
            MyBoolean := PWordBool(Offs)^;
            Result    := MyBoolean;
            FieldType := FT_BOOL;
          End;

        fldINT16:
          Begin
            Result := PSmallInt(Offs)^;
            FieldType := FT_INT;
          End;
        fldINT32:
          Begin
            Result := PInteger(Offs)^;
            FieldType := FT_INT;
          End;
        fldFLOAT:
          Begin
            Result := PDouble(Offs)^;
            FieldType := FT_FLOAT;
          End;
        fldUINT16:
          Begin
            Result := PWord(Offs)^;
            FieldType := FT_INT;
          End;
        fldUINT32:
          Begin
            Result := PInteger(Offs)^;
            FieldType := FT_INT;
          End;
        fldBCD:
          Begin
            PBCDValue := Pointer(Offs);
            If Not BCDToCurr(PBCDValue^,CurrValue) Then
              CurrValue := 0;
            Result := CurrValue;
            FieldType := FT_CURRENCY;
          End;
        //      fldBLOB      : Result := Null;
        //      fldBYTES     : Result := Null;
        //      fldFLOATIEEE : Result := Null;              { 80-bit IEEE float }
        //      fldVARBYTES  : Result := Null;              { Length prefixed var bytes }
        //      fldLOCKINFO  : Result := Null;              { Look for LOCKINFO typedef }
        //      fldCURSOR    : Result := Null;              { For Oracle Cursor type }
        //      fldINT64     : Result := Null;              { 64 bit signed number }
        //      fldUINT64    : Result := Null;              { Unsigned 64 bit integer }
        //      fldADT       : Result := Null;              { Abstract datatype (structure) }
        //      fldARRAY     : Result := Null;              { Array field type }
        //      fldREF       : Result := Null;              { Reference to ADT }
        //      fldTABLE     : Result := Null;              { Nested table (reference) }
      End;
    End;
End;

{-------------------------------------------}

Function PSCCANConstToVariant(ANode: PCANConst; ValuesStart: Pointer;
  Var FieldType: TPSCFieldType): Variant;
Begin
  Result := _PSCCANConstToVariant(ANode,
    Pointer(Integer(ValuesStart) + ANode.iOffSet),FieldType);
End;

{-------------------------------------------------------------------------}

const
  CPSCFieldTypesSupbyExprParser=[ftSmallInt,ftInteger,ftWord,ftAutoInc,ftFloat,
    ftCurrency,ftString,ftWideString,ftFixedChar,ftGuid,ftDate,ftTime,
    ftDateTime,{$IFDEF D6}ftTimeStamp,ftFMTBcd,{$ENDIF}ftBoolean,ftBCD];

{------------------------------------------------------------------}

Function TPSCEmulDataSet.AddField(Const AName: String; AFieldType: TFieldType):
  TField;
Begin
  Result := Nil;
  Try
    Case AFieldType Of
      ftString,ftMemo,ftFmtMemo:
        Result := TStringField.Create(Nil);
      ftSmallint:
        Result := TSmallIntField.Create(Nil);
      ftInteger:
        Result := TIntegerField.Create(Nil);
      ftWord:
        Result := TWordField.Create(Nil);
      ftBoolean:
        Result := TBooleanField.Create(Nil);
      ftFloat:
        Result := TFloatField.Create(Nil);
      ftCurrency:
        Result := TCurrencyField.Create(Nil);
      ftBCD:
        Result := TBCDField.Create(Nil);
      ftDate:
        Result := TDateField.Create(Nil);
      ftTime:
        Result := TTimeField.Create(Nil);
      ftDateTime:
        Result := TDateTimeField.Create(Nil);
      ftBytes:
        Result := TBytesField.Create(Nil);
      ftVarBytes:
        Result := TVarBytesField.Create(Nil);
      ftAutoInc:
        Result := TAutoIncField.Create(Nil);
      ftBlob,
      ftTypedBinary,
      ftParadoxOle,
      ftDBaseOle:
        Result := TBlobField.Create(Nil);
      ftGraphic:
        Result := TGraphicField.Create(Nil);
      {$IFNDEF D2009}
      ftWideString:
        Result := TStringField.Create(Nil);
      {$ENDIF}
      ftLargeInt:
        Result := TIntegerField.Create(Nil);  // not TLargeInt field because it is not sup by ExprParser
      ftADT:
        Result := TADTField.Create(Nil);
      ftArray:
        Result := TArrayField.Create(Nil);
      ftReference:
        Result := TReferenceField.Create(Nil);
      ftDataSet,
      ftOraBlob,
      ftOraClob,
      ftCursor,
      ftFixedChar:
        Begin
          Result := TField.Create(Nil);
          Result.SetFieldType(AFieldType);
        End;
      ftVariant:
        Result := TVariantField.Create(Nil);
      ftInterface,
        ftIDispatch: Result := TIDispatchField.Create(Nil);
      ftGuid:
        Result := TGuidField.Create(Nil);
      {$IFDEF D6}
      ftTimeStamp:
        Result := TSQLTimeStampField.Create(Nil);
      {$ENDIF}

      {$IFDEF D2009}
      ftFixedWideChar:
        Begin
          Result := TField.Create(Nil);
          Result.SetFieldType(AFieldType);
        End;
    	ftWideMemo:
        Result := TWideMemoField.Create(Nil);
      ftWideString:
        Result := TWideStringField.Create(Nil);
    	ftOraTimeStamp,
    	ftOraInterval:
        Begin
          Result := TField.Create(Nil);
          Result.SetFieldType(AFieldType);
        End;
    	ftLongWord:
        Result := TLongWordField.Create(Nil);
    	ftShortint:
        Result := TShortintField.Create(Nil);
    	ftByte:
        Result := TByteField.Create(Nil);
    	ftExtended:
        Result := TExtendedField.Create(Nil);
    	ftConnection,
    	ftParams,
    	ftStream:
        Begin
          Result := TField.Create(Nil);
          Result.SetFieldType(AFieldType);
        End;
    	ftTimeStampOffset:
        Result := TSQLTimeStampOffsetField.Create(Nil);
    	ftObject:
        Result := TObjectField.Create(Nil);
    	ftSingle:
        Result := TSingleField.Create(Nil);
      {$ENDIF}
    Else
      Begin
        Result := TField.Create(Nil);
        Result.SetFieldType(AFieldType);
      End;
    End;

    Result.FieldName := AName;
    Result.Dataset := Self;
  Except
    Result.Free;
    Result := Nil;
  End;
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.AssignDataSet(Value: TDataSet);
Var
  I: Integer;
  Field: TField;
Begin
  Fields.Clear;
  If Value = Nil Then
    exit;
  For I := 0 To Value.FieldCount - 1 Do
    Begin
      Field := AddField(Value.Fields[I].FieldName,Value.Fields[I].DataType);
      Field.Visible := Value.Fields[I].Visible;
    End;
End;

{-------------------------------------------------------------------------}

Function TPSCEmulDataSet.AllocRecordBuffer: TRecordBuffer;  //PChar;
Begin
  Result := Nil;
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.FreeRecordBuffer(Var Buffer: TRecordBuffer);  //PChar);
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.GetBookmarkData(Buffer: TRecordBuffer {PChar}; Data: Pointer);
Begin
End;

{-------------------------------------------------------------------------}

Function TPSCEmulDataSet.GetBookmarkFlag(Buffer: TRecordBuffer {PChar}): TBookmarkFlag;
Begin
  Result := bfCurrent;
End;

{-------------------------------------------------------------------------}

Function TPSCEmulDataSet.GetRecord(Buffer: TRecordBuffer{PChar}; GetMode: TGetMode; DoCheck:
  Boolean): TGetResult;
Begin
  Result := grError;
End;

{-------------------------------------------------------------------------}

Function TPSCEmulDataSet.GetRecordSize: Word;
Begin
  Result := 0;
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalClose;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalDelete;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalFirst;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalGotoBookmark(Bookmark: Pointer);
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalHandleException;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalInitFieldDefs;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalInitRecord(Buffer: TRecordBuffer);  //PChar);
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalLast;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalOpen;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalPost;
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.InternalSetToRecord(Buffer: TRecordBuffer); //PChar);
Begin
End;

{-------------------------------------------------------------------------}

Function TPSCEmulDataSet.IsCursorOpen: Boolean;
Begin
  Result := False;
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.SetBookmarkFlag(Buffer: TRecordBuffer {PChar}; Value: TBookmarkFlag);
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.SetBookmarkData(Buffer: TRecordBuffer {PChar}; Data: Pointer);
Begin
End;

{-------------------------------------------------------------------------}

Procedure TPSCEmulDataSet.SetFieldData(Field: TField; Buffer: Pointer);
Begin
End;

{-------------------------------------------------------------------------}

Function TPSCCustomExprEval.FindLastRecord: Boolean;
Begin
  Result := FindRecord(stLast);
End;

{-------------------------------------------------------------}
end.
