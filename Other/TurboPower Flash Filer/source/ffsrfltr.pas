{*********************************************************}
{* FlashFiler: Server-side filter evaluation             *}
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

unit ffsrfltr;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  Messages,
  ffllbase,
  fflldict,
  ffstdate,
  fftbdict,
  ffsrbase,
  ffsrbde;

type
  PffFilterNode = ^TffFilterNode;
  TffFilterNode = packed record
    Case Integer of
      1:(fnHdr      : CANHdr);
      2:(fnUnary    : CANUnary);
      3:(fnBinary   : CANBinary);
      4:(fnField    : CANField);
      5:(fnConst    : CANConst);
      7:(fnContinue : CANContinue);
      8:(fnCompare  : CANCompare);
  end;

  PffNodeValue = ^TffNodeValue;
  TffNodeValue = packed record
    nvType    : TffWord16;
    nvSize    : TffWord16;
    nvValue   : Pointer;
    nvIsNull  : Boolean;
    nvIsConst : Boolean;
  end;

  TffSrFilter = class(TffObject)
  protected                             {private}
    sfDataDict    : TffServerDataDict;
    sfTimeout     : TffWord32;
    sfExpression  : pCANExpr;
    sfExprSize    : TffWord16;
    sfFilterUntil : TffWord32;
  protected
    protected
      function sfGetLiteralPtr(aOffset : TffWord16) : Pointer;
      function sfGetNodePtr(aOffset : TffWord16) : PffFilterNode;

      function sfEvaluateBinaryNode(aNode   : PffFilterNode;
                                    aRecBuf : Pointer;
                                    aNoCase : Boolean;
                                    aPartial: TffWord16) : Boolean;
      function sfEvaluateConstNode(aNode   : PffFilterNode;
                                   aValue  : PffNodeValue;
                                   aRecBuf : Pointer) : Boolean;
      function sfEvaluateFieldNode(aNode   : PffFilterNode;
                                   aValue  : PffNodeValue;
                                   aRecBuf : Pointer) : Boolean;
      function sfEvaluateLogicalNode(aNode   : PffFilterNode;
                                     aRecBuf : Pointer) : Boolean;
      function sfEvaluateNode(aNode   : PffFilterNode;
                              aValue  : PffNodeValue;
                              aRecBuf : Pointer) : Boolean;
      function sfEvaluateUnaryNode(aNode   : PffFilterNode;
                                   aRecBuf : Pointer) : Boolean;

      function sfCompareValues(var aCompareResult : Integer;
                               var aFirst         : TffNodeValue;
                               var aSecond        : TffNodeValue;
                                   aIgnoreCase    : Boolean;
                                   aPartLen       : Integer) : Boolean;
      function sfCompareValue(var aFirst      : TffNodeValue;
                              var aSecond     : TffNodeValue;
                                  aIgnoreCase : Boolean;
                                  aPartLen    : Integer) : Integer; virtual; {!!.11}
  public
    constructor Create(aCursor     : TObject;                          {!!.11}
                       aDataDict   : TffServerDataDict;
                       aExpression : pCANExpr;
                       aTimeout    : TffWord32); virtual;              {!!.11}
    destructor Destroy; override;

    function MatchesRecord(aRecBuf : Pointer) : Boolean; virtual;      {!!.11}

    procedure BeginTimeout;

    function CheckTimeout(var Res: TffResult) : Boolean;

    property Expression: pCANExpr
      read sfExpression;

    property Timeout: TffWord32
      read sfTimeout;
  end;
  
{Begin !!.11}  
type
  TffSrFilterClass = class of TffSrFilter;
  
const
  ffsrFilterClass : TffSrFilterClass = TffSrFilter;
{End !!.11}

implementation

uses
  ffconst;

{===TffSrFilter==================================================}
constructor TffSrFilter.Create(aCursor     : TObject;                  {!!.11}
                               aDataDict   : TffServerDataDict;
                               aExpression : pCANExpr;
                               aTimeout    : TffWord32);
begin
  inherited Create;
  sfDataDict := aDataDict;
  sfTimeout := aTimeout;
//  if sfTimeout > 5000 then sfTimeout := 5000;                        {Deleted !!.07}
//  if sfTimeout < 50 then sfTimeout := 50;                            {Deleted !!.07}
  if Assigned(aExpression) then begin
    sfExprSize := aExpression^.iTotalSize;
    if (sfExprSize > 0) then begin
      FFGetMem(sfExpression, sfExprSize);
      Move(aExpression^, sfExpression^, sfExprSize);
    end;
  end;
end;
{--------}
destructor TffSrFilter.Destroy;
begin
  if (sfExprSize > 0) and Assigned(sfExpression) then begin
    FFFreeMem(sfExpression, sfExprSize);
    sfExpression := nil;
  end;
  inherited Destroy;
end;
{--------}
function TffSrFilter.sfGetLiteralPtr(aOffset : TffWord16) : Pointer;
var
  i : TffWord16;
begin
  i := sfExpression^.iLiteralStart + aOffset;
  Result := @PByteArray(sfExpression)^[i];
end;
{--------}
function TffSrFilter.sfGetNodePtr(aOffset : TffWord16) : PffFilterNode;
var
  i : TffWord16;
begin
  i := sfExpression^.iNodeStart + aOffset;
  Result := PffFilterNode(@PByteArray(sfExpression)^[i]);
end;
{--------}
function TffSrFilter.MatchesRecord(aRecBuf : Pointer) : Boolean;
var
  Root           : PffFilterNode;
begin
  Result := true;
  if Assigned(sfExpression) then begin
    Root := sfGetNodePtr(0);
    Result := sfEvaluateNode(Root, nil, aRecBuf);
  end;
end;
{--------}
procedure TffSrFilter.BeginTimeout;
begin
  sfFilterUntil := GetTickCount + sfTimeout;
end;
{--------}
function TffSrFilter.CheckTimeout(var Res: TffResult) : Boolean;
begin
  Result := False;
  if GetTickCount > sfFilterUntil then begin
    Res := DBIERR_FF_FilterTimeout;
    Result := True;
  end;
end;
{--------}
function TffSrFilter.sfEvaluateNode(aNode   : PffFilterNode;
                                    aValue  : PffNodeValue;
                                    aRecBuf : Pointer) : Boolean;
begin
  if (aValue <> nil) then
    FillChar(aValue^, sizeof(aValue^), 0);
  case aNode^.fnHdr.NodeClass of
    nodeUNARY:
      Result := sfEvaluateUnaryNode(aNode, aRecBuf);
    nodeBINARY:
      if (aNode^.fnHdr.CANOp in [canAND, canOR]) then
        Result := sfEvaluateLogicalNode(aNode, aRecBuf)
      else
        Result := sfEvaluateBinaryNode(aNode, aRecBuf, false, 0);
    nodeCOMPARE:
      Result := sfEvaluateBinaryNode(aNode, aRecBuf,
                   aNode^.fnCompare.bCaseInsensitive,
                   aNode^.fnCompare.iPartialLen);
    nodeFIELD:
      Result := sfEvaluateFieldNode(aNode, aValue, aRecBuf);
    nodeCONST:
      Result := sfEvaluateConstNode(aNode, aValue, aRecBuf);
    nodeCONTINUE:
      Result := aNode^.fnContinue.iContOperand <> 0;
  else
    {all other node classes cause the node match to fail}
    Result := false;
  end;{case}
end;
{--------}
function TffSrFilter.sfEvaluateUnaryNode(aNode   : PffFilterNode;
                                         aRecBuf : Pointer) : Boolean;
var
  OperandNode : PffFilterNode;
  NodeValue   : TffNodeValue;
begin
  OperandNode := sfGetNodePtr(aNode^.fnUnary.iOperand1);
  if sfEvaluateNode(OperandNode, @NodeValue, aRecBuf) then begin
    case aNode^.fnHdr.CANOp of
      canISBLANK:
        Result := NodeValue.nvIsNull;
      canNOTBLANK:
        Result := not NodeValue.nvIsNull;
    else
      Result := false;
    end;{case}
  end
  else { the node didn't match }
    Result := aNode^.fnHdr.CANOp = canNOT;
end;
{--------}
function TffSrFilter.sfEvaluateLogicalNode(aNode   : PffFilterNode;
                                           aRecBuf : Pointer) : Boolean;
var
  LeftNode   : PffFilterNode;
  RightNode  : PffFilterNode;
begin
  LeftNode := sfGetNodePtr(aNode^.fnBINARY.iOperand1);
  RightNode := sfGetNodePtr(aNode^.fnBINARY.iOperand2);
  case aNode^.fnHdr.CANOp of
    canAND : Result := sfEvaluateNode(LeftNode, nil, aRecBuf) and
                       sfEvaluateNode(RightNode, nil, aRecBuf);
    canOR  : Result := sfEvaluateNode(LeftNode, nil, aRecBuf) or
                       sfEvaluateNode(RightNode, nil, aRecBuf);
  else
    {anything else fails}
    Result := false;
  end;{case}
end;
{--------}
function TffSrFilter.sfEvaluateBinaryNode(aNode    : PffFilterNode;
                                          aRecBuf  : Pointer;
                                          aNoCase  : Boolean;
                                          aPartial : TffWord16) : Boolean;
var
  LeftNode   : PffFilterNode;
  RightNode  : PffFilterNode;
  LeftValue  : TffNodeValue;
  RightValue : TffNodeValue;
  CompareResult : Integer;
begin
  Result := false;
  if (aNode^.fnHdr.NodeClass = nodeCOMPARE) then begin
    LeftNode := sfGetNodePtr(aNode^.fnCompare.iOperand1);
    RightNode := sfGetNodePtr(aNode^.fnCompare.iOperand2);
  end
  else begin
    LeftNode := sfGetNodePtr(aNode^.fnBINARY.iOperand1);
    RightNode := sfGetNodePtr(aNode^.fnBINARY.iOperand2);
  end;
  if not sfEvaluateNode(LeftNode, @LeftValue, aRecBuf) then
    Exit;
  if not sfEvaluateNode(RightNode, @RightValue, aRecBuf) then
    Exit;
  if not sfCompareValues(CompareResult, LeftValue, RightValue,
                                         aNoCase, aPartial) then
    Exit;
  case aNode^.fnHdr.CANOp of
    canLike : Result := CompareResult = 0;                             {!!.11}
    canEQ : Result := CompareResult = 0;
    canNE : Result := CompareResult <> 0;
    canGT : Result := CompareResult > 0;
    canLT : Result := CompareResult < 0;
    canGE : Result := CompareResult >= 0;
    canLE : Result := CompareResult <= 0;
  else
    {anything else fails}
    Result := false;
  end;{case}
end;
{--------}
function TffSrFilter.sfEvaluateConstNode(aNode   : PffFilterNode;
                                         aValue  : PffNodeValue;
                                         aRecBuf : Pointer) : Boolean;
begin
  with aValue^, aNode^.fnCONST do begin
    nvType := iType;
    nvSize := iSize;
    nvValue := sfGetLiteralPtr(iOffset);
    nvIsNull := false;
    nvIsConst := true;
  end;
  Result := true;
end;
{--------}
function TffSrFilter.sfEvaluateFieldNode(aNode   : PffFilterNode;
                                         aValue  : PffNodeValue;
                                         aRecBuf : Pointer) : Boolean;
var
  FieldDesc : PffFieldDescriptor;
begin
  with aNode^.fnFIELD do
    if (0 < iFieldNum) and (iFieldNum <= sfDataDict.FieldCount) then
      FieldDesc := sfDataDict.FieldDescriptor[Pred(iFieldNum)]
    else
      FieldDesc := nil;
{Begin !!.11}
  if aValue <> nil then begin
    with aValue^, FieldDesc^ do begin
      nvType := Ord(fdType);
      nvSize := Ord(fdLength);
      nvValue := @PffByteArray(aRecBuf)^[fdOffset];
      sfDataDict.GetRecordField(Pred(aNode^.fnFIELD.iFieldNum),
                                PffByteArray(aRecBuf), nvIsNull, nil);
      nvIsConst := false;
    end;
    Result := true;
  end
  else
    Result := False;
{End !!.11}
end;
{--------}
function TffSrFilter.sfCompareValues(var aCompareResult : Integer;
                                     var aFirst         : TffNodeValue;
                                     var aSecond        : TffNodeValue;
                                         aIgnoreCase    : Boolean;
                                         aPartLen       : Integer) : Boolean;
begin
  Result := true;
  {Deal with nulls first, we don't have to ask the table to do it
   since null < any value, except null}
  if aFirst.nvIsNull then
    if aSecond.nvIsNull then begin
      aCompareResult := 0;
      Exit;
    end
    else begin
      aCompareResult := -1;
      Exit;
    end
  else {aFirst is not null} if aSecond.nvIsNull then begin
    aCompareResult := 1;
    Exit;
  end;
  {Otherwise let the table deal with it since some translation may be
   required}
  aCompareResult := sfCompareValue(aFirst, aSecond, aIgnoreCase, aPartLen);
end;
{--------}
function TffSrFilter.sfCompareValue(var aFirst      : TffNodeValue;
                                    var aSecond     : TffNodeValue;
                                        aIgnoreCase : Boolean;
                                        aPartLen    : Integer) : Integer;
  {------}
  function ConvertIntValue(var aNode : TffNodeValue; var C : comp) : boolean;
  begin
    Result := true;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldINT16  : C := smallint(nvValue^);
          fldINT32  : C := longint(nvValue^);
          fldUINT16 : C := word(nvValue^);
          fldUINT32 : begin
                        C := longint(nvValue^);
                        if (C < 0) then
                          C := C + $80000000;
                      end;
        else
          Result := false;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftByte   : C := byte(nvValue^);
          fftWord16 : C := word(nvValue^);
          fftWord32 : begin
                        C := longint(nvValue^);
                        if (C < 0) then
                          C := C + $80000000;
                      end;
          fftInt8   : C := shortint(nvValue^);
          fftInt16  : C := smallint(nvValue^);
          fftInt32  : C := longint(nvValue^);
          fftAutoInc: begin
                        C := longint(nvValue^);
                        if (C < 0) then
                          C := C + $80000000;
                      end;
          fftComp   : C := comp(nvValue^);
        else
          Result := false;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertDateTimeValue(var aNode : TffNodeValue; var DT : TDateTime) : boolean;
  begin
    Result := true;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldDATE      : DT := DbiDate(nvValue^);
          fldTIME      : DT := DbiTime(nvValue^) / 86400000.0;
          fldTIMESTAMP : DT := TimeStamp(nvValue^) / 86400000.0;
        else
          Result := false;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftStDate   : DT := StDateToDateTime(TStDate(nvValue^))
                              + 693594;
          fftStTime   : DT := StTimeToDateTime(TStTime(nvValue^));
          fftDateTime : DT := TDateTime(nvValue^);
        else
          Result := false;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertFloatValue(var aNode : TffNodeValue; var F : extended) : boolean;
  begin
    Result := true;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldFLOAT     : F := double(nvValue^);
          fldFLOATIEEE : F := extended(nvValue^);
        else
          Result := false;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftSingle   : F := single(nvValue^);
          fftDouble   : F := double(nvValue^);
          fftExtended : F := extended(nvValue^);
          fftCurrency : F := currency(nvValue^);
        else
          Result := false;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertBooleanValue(var aNode : TffNodeValue; var B : boolean) : boolean;
  begin
    Result := true;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldBOOL : B := WordBool(nvValue^);              
        else
          Result := false;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftBoolean : B := boolean(nvValue^);
        else
          Result := false;
        end;{case}
      end;
    end;
  end;
  {------}
  function ConvertStringValue(var aNode : TffNodeValue; var P : PChar) : boolean;
  var
    StrZ : TffStringZ;
  begin
    Result := true;
    with aNode do begin
      if nvIsConst then begin
        case nvType of
          fldZSTRING : P := nvValue;
        else
          Result := false;
        end;{case}
      end
      else begin
        case TffFieldType(nvType) of
          fftChar :
            begin
              P := StrAlloc(2);
              P[0] := char(nvValue^);
              P[1] := #0;
            end;
          fftShortString,
          fftShortAnsiStr :
            begin
              P := StrNew(StrPCopy(StrZ, TffShStr(nvValue^)));
            end;
          fftNullString,
          fftNullAnsiStr :
            begin
              P := StrNew(nvValue);
            end;
        else
          Result := false;
        end;{case}
      end;
    end;
  end;
  {------}
var
  Bool1, Bool2   : boolean;
  Comp1, Comp2   : comp;
  PChar1, PChar2 : PAnsiChar;
  DT1, DT2       : TDateTime;
  Ext1, Ext2     : extended;
begin
  {Note: there are two types of things to compare: constants and
         fields. In neither case will this routine be called with null
         values - the caller takes care of this}
  {Note: this routine doesn't have to worry about comparing dissimilar
         types (eg dates and strings); this is illegal and will have
         been already excluded by the filter parser; similarly with
         fields that can't be compared (eg, BLOBs)}
  {Note: constant values are stored as logical types, field values as
         physical types}

  {Deal with integer types first}
  if ConvertIntValue(aFirst, Comp1) then begin
    ConvertIntValue(aSecond, Comp2);
    if (Comp1 < Comp2) then      Result := -1
    else if (Comp1 = Comp2) then Result := 0
    else                         Result := 1;
    Exit;
  end;

  {Deal with floating point types next}
  if ConvertFloatValue(aFirst, Ext1) then begin
    ConvertFloatValue(aSecond, Ext2);
    if (Ext1 < Ext2) then      Result := -1
    else if (Ext1 = Ext2) then Result := 0
    else                       Result := 1;
    Exit;
  end;

  {Deal with date/time types next}
  if ConvertDateTimeValue(aFirst, DT1) then begin
    ConvertDateTimeValue(aSecond, DT2);
    if (DT1 < DT2) then      Result := -1
    else if (DT1 = DT2) then Result := 0
    else                     Result := 1;
    Exit;
  end;

  {Deal with boolean types next; false < true}
  if ConvertBooleanValue(aFirst, Bool1) then begin
    ConvertBooleanValue(aSecond, Bool2);
    if Bool1 then
      if Bool2 then Result := 0
      else          Result := 1
    else {Bool1 is false}
      if Bool2 then Result := -1
      else          Result := 0;
    Exit;
  end;

  {Deal with strings next}
  if ConvertStringValue(aFirst, PChar1) then begin
    ConvertStringValue(aSecond, PChar2);
    if aIgnoreCase then
      if (aPartLen = 0) then
        Result := FFAnsiStrIComp(PChar1, PChar2)                      {!!.06}{!!.07}
      else
        Result := FFAnsiStrLIComp(PChar1, PChar2, aPartLen)           {!!.06}{!!.07}
    else
      if (aPartLen = 0) then
        Result := AnsiStrComp(PChar1, PChar2)                         {!!.06}
      else
        Result := AnsiStrLComp(PChar1, PChar2, aPartLen);             {!!.06}
    if not aFirst.nvIsConst then
      StrDispose(PChar1);
    if not aSecond.nvIsConst then
      StrDispose(PChar2);
    Exit;                     
  end;

  {otherwise just compare the bytes}
  Result := FFCmpBytes(Pointer(aFirst.nvValue),
                       Pointer(aSecond.nvValue),
                       FFMinI(aFirst.nvSize, aSecond.nvSize));
end;

end.

