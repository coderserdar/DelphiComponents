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

{$I fsdefine.inc}

Unit fssrfltr;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  Messages,
  fsllbase,
  fslldict,
  fsstdate,
  fsdictserveraccess,
  fssrbase,
  fssrbde,
  fsfunInterp;

Type
  PffFilterNode = ^TffFilterNode;
  TffFilterNode = Packed Record
    Case Integer Of
      1: (fnHdr: CANHdr);
      2: (fnUnary: CANUnary);
      3: (fnBinary: CANBinary);
      4: (fnField: CANField);
      5: (fnConst: CANConst);
      7: (fnContinue: CANContinue);
      8: (fnCompare: CANCompare);
  End;

  PfsNodeValue = ^TfsNodeValue;
  TfsNodeValue = Packed Record
    nvDType: Word;
    nvSize: TffWord16;
    nvDec: Word;
    nvValue: Pointer;
    nvIsNull: Boolean;
    nvIsConst: Boolean;
  End;

  TfsSrcFilter = Class(TFSSpecObject)
  Protected {private}
    sfDataDict: TFSInfoServerDict;
    sfTimeout: TffWord32;
    sfExpression: pCANExpr;
    sfExprSize: TffWord16;
    sfFilterUntil: TffWord32;
  Protected
  Protected
    Function sfGetLiteralPtr(aOffset: TffWord16): Pointer;
    Function sfGetNodePtr(aOffset: TffWord16): PffFilterNode;

    Function sfEvaluateBinaryNode(aNode: PffFilterNode;
      aRecBuf: Pointer;
      aNoCase: Boolean;
      aPartial: TffWord16): Boolean;
    Function sfEvaluateConstNode(aNode: PffFilterNode;
      aValue: PfsNodeValue;
      aRecBuf: Pointer): Boolean;
    Function sfEvaluateFieldNode(aNode: PffFilterNode;
      aValue: PfsNodeValue;
      aRecBuf: Pointer): Boolean;
    Function sfEvaluateLogicalNode(aNode: PffFilterNode;
      aRecBuf: Pointer): Boolean;
    Function sfEvaluateNode(aNode: PffFilterNode;
      aValue: PfsNodeValue;
      aRecBuf: Pointer): Boolean;
    Function sfEvaluateUnaryNode(aNode: PffFilterNode;
      aRecBuf: Pointer): Boolean;

    Function sfCompareValues(Var aCompareResult: Integer;
      Var aFirst: TfsNodeValue;
      Var aSecond: TfsNodeValue;
      aIgnoreCase: Boolean;
      aPartLen: Integer): Boolean;
    Function sfCompareValue(Var aFirst: TfsNodeValue;
      Var aSecond: TfsNodeValue;
      aIgnoreCase: Boolean;
      aPartLen: Integer): Integer; Virtual; {!!.11}
  Public
    Constructor Create(aCursor: TObject; {!!.11}
      aDataDict: TFSInfoServerDict;
      aExpression: pCANExpr;
      aTimeout: TffWord32); Virtual; {!!.11}
    Destructor Destroy; Override;

    Function MatchesRecord(aRecBuf: Pointer): Boolean; Virtual; {!!.11}

    Procedure BeginTimeout;

    Function CheckTimeout(Var Res: TffResult): Boolean;

    Property Expression: pCANExpr
      Read sfExpression;

    Property Timeout: TffWord32
      Read sfTimeout;
  End;

  {Begin !!.11}
Type
  TfsSrcFilterClass = Class Of TfsSrcFilter;

Const
  ffsrFilterClass: TfsSrcFilterClass = TfsSrcFilter;
  {End !!.11}

Implementation

Uses
  fsconst,
  fsutil;

{===TfsSrcFilter==================================================}

Constructor TfsSrcFilter.Create(aCursor: TObject; {!!.11}
  aDataDict: TFSInfoServerDict;
  aExpression: pCANExpr;
  aTimeout: TffWord32);
Begin
  Inherited Create;
  sfDataDict := aDataDict;
  sfTimeout := aTimeout;
  If Assigned(aExpression) Then
    Begin
      sfExprSize := aExpression^.iTotalSize;
      If (sfExprSize > 0) Then
        Begin
          FFGetMem(sfExpression, sfExprSize);
          Move(aExpression^, sfExpression^, sfExprSize);
        End;
    End;
End;
{--------}

Destructor TfsSrcFilter.Destroy;
Begin
  If (sfExprSize > 0) And Assigned(sfExpression) Then
    Begin
      FFFreeMem(sfExpression, sfExprSize);
      sfExpression := Nil;
    End;
  Inherited Destroy;
End;
{--------}

Function TfsSrcFilter.sfGetLiteralPtr(aOffset: TffWord16): Pointer;
Var
  i: TffWord16;
Begin
  i := sfExpression^.iLiteralStart + aOffset;
  Result := @PByteArray(sfExpression)^[i];
End;
{--------}

Function TfsSrcFilter.sfGetNodePtr(aOffset: TffWord16): PffFilterNode;
Var
  i: TffWord16;
Begin
  i := sfExpression^.iNodeStart + aOffset;
  Result := PffFilterNode(@PByteArray(sfExpression)^[i]);
End;
{--------}

Function TfsSrcFilter.MatchesRecord(aRecBuf: Pointer): Boolean;
Var
  Root: PffFilterNode;
Begin
  Result := True;
  If Assigned(sfExpression) Then
    Begin
      Root := sfGetNodePtr(0);
      Result := sfEvaluateNode(Root, Nil, aRecBuf);
    End;
End;
{--------}

Procedure TfsSrcFilter.BeginTimeout;
Begin
  sfFilterUntil := GetTickCount + sfTimeout;
End;
{--------}

Function TfsSrcFilter.CheckTimeout(Var Res: TffResult): Boolean;
Begin
  Result := False;
  If GetTickCount > sfFilterUntil Then
    Begin
      Res := DBIERR_FS_FilterTimeout;
      Result := True;
    End;
End;
{--------}

Function TfsSrcFilter.sfEvaluateNode(aNode: PffFilterNode;
  aValue: PfsNodeValue;
  aRecBuf: Pointer): Boolean;
Begin
  If (aValue <> Nil) Then
    FillChar(aValue^, sizeof(aValue^), 0);
  Case aNode^.fnHdr.NodeClass Of
    nodeUNARY:
      Result := sfEvaluateUnaryNode(aNode, aRecBuf);
    nodeBINARY:
      If (aNode^.fnHdr.CANOp In [canAND, canOR]) Then
        Result := sfEvaluateLogicalNode(aNode, aRecBuf)
      Else
        Result := sfEvaluateBinaryNode(aNode, aRecBuf, False, 0);
    nodeCOMPARE:
      Result := sfEvaluateBinaryNode(aNode, aRecBuf,
        aNode^.fnCompare.bCaseInsensitive,
        aNode^.fnCompare.iPartialLen);
    nodeFIELD {,nodefunc}:
      Result := sfEvaluateFieldNode(aNode, aValue, aRecBuf);
    nodeCONST:
      Result := sfEvaluateConstNode(aNode, aValue, aRecBuf);
    nodeCONTINUE:
      Result := aNode^.fnContinue.iContOperand <> 0;
    Else
      {all other node classes cause the node match to fail}
      Result := False;
  End; {case}
End;
{--------}

Function TfsSrcFilter.sfEvaluateUnaryNode(aNode: PffFilterNode;
  aRecBuf: Pointer): Boolean;
Var
  OperandNode: PffFilterNode;
  NodeValue: TfsNodeValue;
Begin
  OperandNode := sfGetNodePtr(aNode^.fnUnary.iOperand1);
  If sfEvaluateNode(OperandNode, @NodeValue, aRecBuf) Then
    Begin
      Case aNode^.fnHdr.CANOp Of
        canISBLANK:
          Result := NodeValue.nvIsNull;
        canNOTBLANK:
          Result := Not NodeValue.nvIsNull;
        Else
          Result := False;
      End; {case}
    End
  Else { the node didn't match }
    Result := aNode^.fnHdr.CANOp = canNOT;
End;
{--------}

Function TfsSrcFilter.sfEvaluateLogicalNode(aNode: PffFilterNode;
  aRecBuf: Pointer): Boolean;
Var
  LeftNode: PffFilterNode;
  RightNode: PffFilterNode;
Begin
  LeftNode := sfGetNodePtr(aNode^.fnBINARY.iOperand1);
  RightNode := sfGetNodePtr(aNode^.fnBINARY.iOperand2);
  Case aNode^.fnHdr.CANOp Of
    canAND: Result := sfEvaluateNode(LeftNode, Nil, aRecBuf) And
      sfEvaluateNode(RightNode, Nil, aRecBuf);
    canOR: Result := sfEvaluateNode(LeftNode, Nil, aRecBuf) Or
      sfEvaluateNode(RightNode, Nil, aRecBuf);
    Else
      {anything else fails}
      Result := False;
  End; {case}
End;
{--------}

Function TfsSrcFilter.sfEvaluateBinaryNode(aNode: PffFilterNode;
  aRecBuf: Pointer;
  aNoCase: Boolean;
  aPartial: TffWord16): Boolean;
Var
  LeftNode: PffFilterNode;
  RightNode: PffFilterNode;
  LeftValue: TfsNodeValue;
  RightValue: TfsNodeValue;
  CompareResult: Integer;
Begin
  Result := False;
  If (aNode^.fnHdr.NodeClass = nodeCOMPARE) Then
    Begin
      LeftNode := sfGetNodePtr(aNode^.fnCompare.iOperand1);
      RightNode := sfGetNodePtr(aNode^.fnCompare.iOperand2);
    End
  Else
    Begin
      LeftNode := sfGetNodePtr(aNode^.fnBINARY.iOperand1);
      RightNode := sfGetNodePtr(aNode^.fnBINARY.iOperand2);
    End;
  If Not sfEvaluateNode(LeftNode, @LeftValue, aRecBuf) Then
    Exit;
  If Not sfEvaluateNode(RightNode, @RightValue, aRecBuf) Then
    Exit;
  If Not sfCompareValues(CompareResult, LeftValue, RightValue,
    aNoCase, aPartial) Then
    Exit;
  Case aNode^.fnHdr.CANOp Of
    canLike: Result := CompareResult = 0; {!!.11}
    canEQ: Result := CompareResult = 0;
    canNE: Result := CompareResult <> 0;
    canGT: Result := CompareResult > 0;
    canLT: Result := CompareResult < 0;
    canGE: Result := CompareResult >= 0;
    canLE: Result := CompareResult <= 0;
    Else
      {anything else fails}
      Result := False;
  End; {case}
End;
{--------}

Function TfsSrcFilter.sfEvaluateConstNode(aNode: PffFilterNode;
  aValue: PfsNodeValue;
  aRecBuf: Pointer): Boolean;
Begin
  With aValue^, aNode^.fnCONST Do
    Begin
      nvDType := iDType;
      //nvDSType := iDSType;
      nvSize := iSize;
      nvValue := sfGetLiteralPtr(iOffset);
      nvIsNull := False;
      nvIsConst := True;
    End;
  Result := True;
End;
{--------}

Function TfsSrcFilter.sfEvaluateFieldNode(aNode: PffFilterNode;
  aValue: PfsNodeValue;
  aRecBuf: Pointer): Boolean;
Var
  FieldDesc: PffFieldDescriptor;
Begin
  With aNode^.fnFIELD Do
    If (0 < iFieldNum) And (iFieldNum <= sfDataDict.FieldCount) Then
      FieldDesc := sfDataDict.FieldDescriptor[Pred(iFieldNum)]
    Else
      FieldDesc := Nil;
  {Begin !!.11}
  If aValue <> Nil Then
    Begin
      With aValue^, FieldDesc^ Do
        Begin
          nvDType := Ord(fdType);
          nvSize := Ord(fdLength);
          nvDec := fdDecPl;
          nvValue := @PffByteArray(aRecBuf)^[fdOffset];
          sfDataDict.GetRecordField(Pred(aNode^.fnFIELD.iFieldNum),
            PffByteArray(aRecBuf), nvIsNull, Nil);
          nvIsConst := False;
        End;
      Result := True;
    End
  Else
    Result := False;
  {End !!.11}
End;
{--------}

Function TfsSrcFilter.sfCompareValues(Var aCompareResult: Integer;
  Var aFirst: TfsNodeValue;
  Var aSecond: TfsNodeValue;
  aIgnoreCase: Boolean;
  aPartLen: Integer): Boolean;
Begin
  Result := True;
  {Deal with nulls first, we don't have to ask the table to do it
   since null < any value, except null}
  If aFirst.nvIsNull Then
    If aSecond.nvIsNull Then
      Begin
        aCompareResult := 0;
        Exit;
      End
    Else
      Begin
        aCompareResult := -1;
        Exit;
      End
  Else {aFirst is not null}  If aSecond.nvIsNull Then
    Begin
      aCompareResult := 1;
      Exit;
    End;
  {Otherwise let the table deal with it since some translation may be
   required}
  aCompareResult := sfCompareValue(aFirst, aSecond, aIgnoreCase, aPartLen);
End;
{--------}

Function TfsSrcFilter.sfCompareValue(Var aFirst: TfsNodeValue;
  Var aSecond: TfsNodeValue;
  aIgnoreCase: Boolean;
  aPartLen: Integer): Integer;
{------}

  Function ConvertIntValue(Var aNode: TfsNodeValue; Var C: Int64): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of // const right
              fldByte: C := Int64(nvValue^);
              fldWord16: C := Int64(nvValue^);
              fldWord32: C := Int64(nvValue^);
              fldInt8: C := Int64(nvValue^);
              fldInt16: C := Int64(nvValue^);
              fldInt32, fldAutoInc32: C := Int64(nvValue^);
              fldInt64, fldAutoInc64, fldRecVersion: C := Int64(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstUInt8: C := Byte(nvValue^);
              fstUInt16: C := Word(nvValue^);
              fstUInt32: C := Longword(nvValue^);
              fstInt8: C := Shortint(nvValue^);
              fstInt16: C := Smallint(nvValue^);
              fstInt32, fstAutoInc32: C := Longint(nvValue^);
              fstInt64, fstAutoInc64, fstRecVersion: C := Int64(nvValue^);
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertDateTimeValue(Var aNode: TfsNodeValue; Var DT: TDateTime): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldDATE: DT := DbiDate(nvValue^);
              fldTIME: DT := DbiTime(nvValue^) / 86400000.0;
              fldDateTime: DT := TimeStamp(nvValue^) / 86400000.0;
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstDate: DT := StDateToDateTime(TStDate(nvValue^))
                + 693594;
              fstTime: DT := StTimeToDateTime(TStTime(nvValue^));
              fstDateTime:
                Begin
                  DT := TDateTime(nvValue^);
                  fsSetMillisecond(dt, 0);
                End;
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertFloatValue(Var aNode: TfsNodeValue; Var F: Extended): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldSingle: F := Extended(nvValue^);
              fldDouble: F := Extended(nvValue^);
              fldExtended: F := Extended(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstSingle: F := Single(nvValue^);
              fstDouble: F := Double(nvValue^);
              fstExtended: F := Extended(nvValue^);
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertBcdValue(Var aNode: TfsNodeValue; Var F: Currency): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldBcd,
                fldCurrency: F := Currency(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstBcd, fstCurrency:
                Begin
                  F := Currency(nvValue^);
                End;
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertBooleanValue(Var aNode: TfsNodeValue; Var B: boolean): boolean;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldBOOLean: B := WordBool(nvValue^);
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstBoolean: B := boolean(nvValue^);
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;
  {------}

  Function ConvertStringValue(Var aNode: TfsNodeValue; Var P: PChar): boolean;
  Var
    StrZ: TffStringZ;
    WorkString: String;
    Len: Integer;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldSingleChar, fldShortString,
                fldNullString,
                fldVarNullString: P := nvValue;
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstSingleChar:
                Begin
                  P := StrAlloc(2);
                  P[0] := char(nvValue^);
                  P[1] := #0;
                End;
              fstShortString:
                Begin
                  P := StrNew(StrPCopy(StrZ, TffShStr(nvValue^)));
                End;
              fstNullString,
                fstVarNullString:
                Begin
                  P := StrNew(nvValue);
                End;
              fstSingleWideChar:
                Begin
                  P := StrAlloc(2);
                  len := lstrlenw(PWideChar(nvValue^));
                  WorkString := WideCharLenToString(PWideChar(nvValue^), len);
                  P[0] := WorkString[1];
                  P[1] := #0;
                End;
              fstWideString,fstVarWideString:
                Begin
                  len := lstrlenw(PWideChar(nvValue));
                  P := StrAlloc(len);
                  WorkString := WideCharLenToString(PWideChar(nvValue), len);
                  StrPLCopy(P, WorkString, len);
                End;
              {fstUnicode:
                Begin

                End; }
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;

  Function ConvertArrayValue(Var aNode: TfsNodeValue; Var P: PChar): boolean;
  Var
    StrZ: TffStringZ;

    Function EArray(Const aArray: String; Var aPos: Longint): String;
    Var
      I: Longint;
    Begin
      I := aPos;
      While (I <= Length(aArray)) And (aArray[I] <> ',') Do
        Inc(I);
      Result := Trim(Copy(aArray, aPos, I - aPos));
      If (I <= Length(aArray)) And (aArray[I] = ',') Then
        Inc(I);
      aPos := I;
    End;

    Function CoorectStrArray(Const aStrinArray: String; ArrayLength, aDec: Integer): String;
    Var
      iPos: Longint;
      idx, ICode: Integer;
      D: Extended;
      dc: Char;
    Begin
      Result := '';
      iPos := 1;
      idx := 0;
      dc := DecimalSeparator;
      DecimalSeparator := '.';
      Try
        While iPos <= Length(aStrinArray) Do
          Begin
            fsStringToExtended(EArray(aStrinArray, iPos), D, ICode);
            If iCode = 0 Then
              Begin
                If adec > 0 Then
                  Result := Result + fsFloatToStrF(D, ffFixed, 20, adec)
                Else
                  Result := Result + fsFloatToStr(d);
              End
            Else
              Raise Exception.Create('Invalid Float Value');
            inc(idx);
            If iPos <= Length(aStrinArray) Then
              Result := Result + ',';
          End;
        If idx < ArrayLength Then
          While idx < ArrayLength Do
            Begin
              If adec > 0 Then
                Result := Result + ','+fsFloatToStrF(0, ffFixed, 20, adec)
              Else
                Result := Result + ',' + '0';
              inc(idx);
            End;
      Finally
        DecimalSeparator := dc;
      End;
    End;

    Function ArrayAsString: String;

      Function ByteArrayToString(ByteArray: Pointer; ArrayLength: Integer): String;
      Var
        idx: Integer;
        BArr: PffByteArray Absolute ByteArray;
      Begin
        Result := '';
        Result := IntToStr(BArr[0]);
        For idx := 1 To ArrayLength - 1 Do
          Result := Result + ',' + IntToStr(BArr[idx]);
      End;

      Function WordArrayToString(WordArray: Pointer; ArrayLength: Integer): String;
      Var
        idx: Integer;
        BArr: PffWordArray Absolute WordArray;
      Begin
        Result := '';
        Result := IntToStr(BArr[0]);
        For idx := 1 To ArrayLength - 1 Do
          Result := Result + ',' + IntToStr(BArr[idx]);
      End;

      Function IntegerArrayToString(IntArray: Pointer; ArrayLength: Integer): String;
      Var
        idx: Integer;
        BArr: PffIntArray Absolute intArray;
      Begin
        Result := '';
        Result := IntToStr(BArr[0]);
        For idx := 1 To ArrayLength - 1 Do
          Result := Result + ',' + IntToStr(BArr[idx]);
      End;

      Function DoubleArrayToString(DoubleArray: Pointer; ArrayLength: Integer; Decimal: Byte): String;
      Var
        idx: Integer;
        BArr: PffDoubleArray Absolute DoubleArray;
        S: String;
        D: Extended;
        c: Char;
        r: TRound;
      Begin
        Result := '';
        D := BArr[0];
        c := DecimalSeparator;
        DecimalSeparator := '.';
        r := rMathematical;
        Try
          If decimal > 0 Then
            Begin
              d := RoundExtended(d, decimal, r);
              S := fsFloatToStrF(D, ffFixed, 20, decimal);
            End
          Else
            S := fsFloatToStr(D);
          Result := S;
          For idx := 1 To ArrayLength - 1 Do
            Begin
              D := BArr[idx];
              If decimal > 0 Then
                Begin
                  d := RoundExtended(d, decimal, r);
                  S := fsFloatToStrF(D, ffFixed, 20, decimal);
                End
              Else
                S := fsFloatToStr(D);
              Result := Result + ',' + S;
            End;
        Finally
          DecimalSeparator := c;
        End;
      End;

    Begin
      If TfsFieldType(aNode.nvDType) = fstArrayUInt8 Then
        Result := ByteArrayToString(aNode.nvValue, anode.nvSize)
      Else If TfsFieldType(aNode.nvDType) = fstArrayUInt16 Then
        Result := WordArrayToString(aNode.nvValue, anode.nvSize Div 2)
      Else If TfsFieldType(aNode.nvDType) = fstArrayInt32 Then
        Result := IntegerArrayToString(aNode.nvValue, anode.nvSize Div 4)
      Else If TfsFieldType(aNode.nvDType) = fstArrayDouble Then
        Result := DoubleArrayToString(aNode.nvValue, anode.nvSize Div 8, aFirst.nvDec)
      Else
        Result := '';
    End;
  Begin
    Result := True;
    With aNode Do
      Begin
        If nvIsConst Then
          Begin
            Case nvDType Of
              fldByteArray, fldintArray, fldWordArray, fldDoubleArray:
                Begin
                  P := nvValue;
                 { Case TfsFieldType(aFirst.nvDType) Of
                    fstArrayUInt8:
                      WorkString := CoorectStrArray(String(P), aFirst.nvSize, 0);
                    fstArrayInt32:
                      WorkString := CoorectStrArray(String(P), aFirst.nvSize Div 4, 0);
                    fstArrayUInt16:
                      WorkString := CoorectStrArray(String(P), aFirst.nvSize Div 2, 0);
                    fstArrayDouble:
                      WorkString := CoorectStrArray(String(P), aFirst.nvSize Div 8, aFirst.nvDec);
                  End;
                  P := PChar(WorkString);  }
                end;
              Else
                Result := False;
            End; {case}
          End
        Else
          Begin
            Case TfsFieldType(nvDType) Of
              fstArrayUInt16..fstArrayDouble, fstArrayUInt8:
                Begin
                  P := StrNew(StrPCopy(StrZ, ArrayAsString));
                End;
              Else
                Result := False;
            End; {case}
          End;
      End;
  End;

Var
  Bool1, Bool2: boolean;
  Comp1, Comp2: Int64;
  PChar1, PChar2: PAnsiChar;
  DT1, DT2: TDateTime;
  Ext1, Ext2: Extended;
  Cur1, Cur2: Currency;
Begin
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
  If ConvertIntValue(aFirst, Comp1) Then
    Begin
      ConvertIntValue(aSecond, Comp2);
      If (Comp1 < Comp2) Then
        Result := -1
      Else If (Comp1 = Comp2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with floating point types next}
  If ConvertFloatValue(aFirst, Ext1) Then
    Begin
      ConvertFloatValue(aSecond, Ext2);
      If (Ext1 < Ext2) Then
        Result := -1
      Else If (Ext1 = Ext2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with floating point types next}
  If ConvertBcdValue(aFirst, Cur1) Then
    Begin
      ConvertBcdValue(aSecond, Cur2);
      If (Cur1 < Cur2) Then
        Result := -1
      Else If (Cur1 = Cur2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with date/time types next}
  If ConvertDateTimeValue(aFirst, DT1) Then
    Begin
      ConvertDateTimeValue(aSecond, DT2);
      If (DT1 < DT2) Then
        Result := -1
      Else If (DT1 = DT2) Then
        Result := 0
      Else
        Result := 1;
      Exit;
    End;

  {Deal with boolean types next; false < true}
  If ConvertBooleanValue(aFirst, Bool1) Then
    Begin
      ConvertBooleanValue(aSecond, Bool2);
      If Bool1 Then
        If Bool2 Then
          Result := 0
        Else
          Result := 1
      Else {Bool1 is false}  If Bool2 Then
        Result := -1
      Else
        Result := 0;
      Exit;
    End;

  If ConvertArrayValue(aFirst, PChar1) Then
    Begin
      ConvertArrayValue(aSecond, PChar2);
      Result := AnsiStrComp(PChar1, PChar2);

      If Not aFirst.nvIsConst Then
        StrDispose(PChar1);
      If Not aSecond.nvIsConst Then
        StrDispose(PChar2);
      Exit;
    End;

  {Deal with strings next}
  If ConvertStringValue(aFirst, PChar1) Then
    Begin
      ConvertStringValue(aSecond, PChar2);
      If aIgnoreCase Then
        If (aPartLen = 0) Then
          Result := FFAnsiStrIComp(PChar1, PChar2)
        Else
          Result := FFAnsiStrLIComp(PChar1, PChar2, aPartLen)
      Else If (aPartLen = 0) Then
        Result := AnsiStrComp(PChar1, PChar2)
      Else
        Result := AnsiStrLComp(PChar1, PChar2, aPartLen);
      If Not aFirst.nvIsConst Then
        StrDispose(PChar1);
      If Not aSecond.nvIsConst Then
        StrDispose(PChar2);
      Exit;
    End;
  Result := FFCmpBytes(Pointer(aFirst.nvValue),
    Pointer(aSecond.nvValue),
    FFMinI(aFirst.nvSize, aSecond.nvSize));
End;

End.

