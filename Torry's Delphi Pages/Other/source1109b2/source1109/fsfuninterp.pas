{*********************************************************}
{* FSSQL: Interpreter Class Definitions                  *}
{* Programming: Krzysztof Winnicki                       *}
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
 * The Original Code is Krzysztof Winnicki
 *
 * The Initial Developer of the Original Code is
 * Krzysztof Winnicki
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

Unit fsfuninterp;

{$I fsdefine.inc}

Interface

Uses Classes,
  fsstdate,
  SysUtils
  {$IFDEF DCC6OrLater}
  ,
  Variants
  {$ENDIF};

Const
  DefaultBlank: Char = '_';
  MaskFieldSeparator: Char = ';';
  MaskNoSave: Char = '0';

  mDirReverse = '!';
  mDirUpperCase = '>';
  mDirLowerCase = '<';

  mDirLiteral = '\';

  mMskAlpha = 'L';
  mMskAlphaOpt = 'l';
  mMskAlphaNum = 'A';
  mMskAlphaNumOpt = 'a';
  mMskAscii = 'C';
  mMskAsciiOpt = 'c';
  mMskNumeric = '0';
  mMskNumericOpt = '9';
  mMskNumSymOpt = '#';

  { intl literals }
  mMskTimeSeparator = ':';
  mMskDateSeparator = '/';

Type
  TRound = (rNone, rMathematical, rMatAfter1, rMatAfter2, rMatAfter3,
    rMatAfter4, rMatAfter5, rMatAfter6, rMatAfter7, rMatAfter8, rMatAfter9);
  TMaskCharType = (mcNone, mcLiteral, mcIntlLiteral, mcDirective, mcMask,
    mcMaskOpt, mcFieldSeparator, mcField);
  TMaskDirectives = Set Of (mdReverseDir, mdUpperCase, mdLowerCase,
    mdLiteralChar);

Type
  TGetValueEvent = Procedure(Const Name: String; Var Value: Variant) Of Object;
  TGetSetValueFunction = Procedure(Const Name: String; p1, p2, p3, p4, p5, p6, p7, p8, p9, p10: Variant;
    Var Value: Variant) Of Object;

  TFunParser = Class
  Private
    FOnGetValue: TGetValueEvent;
    FOnFunction: TGetSetValueFunction;
    Function GetIdentifier(Const s: String; Var i: Integer): String;
    Function GetString(Const s: String; Var i: Integer): String;
    Procedure Get10Parameters(Const s: String; Var i: Integer;
      Var s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: String);
  Public
    Function String2OPZ(s: String): String;
    Function CalculateOPZ(Const s: String): Variant;
    Function Calculate(Const s: String): Variant;
    Property OnGetValue: TGetValueEvent Read FOnGetValue Write FOnGetValue;
    Property OnFunction: TGetSetValueFunction Read FOnFunction Write FOnFunction;
  End ;

  // TIntVariables is tool class intended for storing variable name and its
  // value. Value is of type Variant.
  // Call TIntVariables['VarName'] := VarValue to set variable value and
  // VarValue := TIntVariables['VarName'] to retrieve it.

  TIntVariables = Class(TObject)
  Private
    FList: TStringList;
    Procedure SetVariable1(Const Name: String; Value: Variant);
    Function GetVariable1(Const Name: String): Variant;
    Procedure SetVariable2(Const Name: String; Value: Variant);
    Function GetVariable2(Const Name: String): Variant;
    Procedure SetVariable3(Const Name: String; Value: Variant);
    Function GetVariable3(Const Name: String): Variant;
    Procedure SetVariable4(Const Name: String; Value: Variant);
    Function GetVariable4(Const Name: String): Variant;
    Procedure SetVariable5(Const Name: String; Value: Variant);
    Function GetVariable5(Const Name: String): Variant;
    Procedure SetVariable6(Const Name: String; Value: Variant);
    Function GetVariable6(Const Name: String): Variant;
    Procedure SetVariable7(Const Name: String; Value: Variant);
    Function GetVariable7(Const Name: String): Variant;
    Procedure SetVariable8(Const Name: String; Value: Variant);
    Function GetVariable8(Const Name: String): Variant;
    Procedure SetVariable9(Const Name: String; Value: Variant);
    Function GetVariable9(Const Name: String): Variant;
    Procedure SetVariable10(Const Name: String; Value: Variant);
    Function GetVariable10(Const Name: String): Variant;

    Procedure SetValue1(Index: Integer; Value: Variant);
    Function GetValue1(Index: Integer): Variant;
    Procedure SetValue2(Index: Integer; Value: Variant);
    Function GetValue2(Index: Integer): Variant;
    Procedure SetValue3(Index: Integer; Value: Variant);
    Function GetValue3(Index: Integer): Variant;
    Procedure SetValue4(Index: Integer; Value: Variant);
    Function GetValue4(Index: Integer): Variant;
    Procedure SetValue5(Index: Integer; Value: Variant);
    Function GetValue5(Index: Integer): Variant;
    Procedure SetValue6(Index: Integer; Value: Variant);
    Function GetValue6(Index: Integer): Variant;
    Procedure SetValue7(Index: Integer; Value: Variant);
    Function GetValue7(Index: Integer): Variant;
    Procedure SetValue8(Index: Integer; Value: Variant);
    Function GetValue8(Index: Integer): Variant;
    Procedure SetValue9(Index: Integer; Value: Variant);
    Function GetValue9(Index: Integer): Variant;
    Procedure SetValue10(Index: Integer; Value: Variant);
    Function GetValue10(Index: Integer): Variant;

    Procedure SetName(Index: Integer; Value: String);
    Function GetName(Index: Integer): String;
    Function GetCount: Integer;
    Procedure SetSorted(Value: Boolean);
    Function GetSorted: Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Assign(Value: TIntVariables);
    Procedure Clear;
    Procedure Delete(Index: Integer);
    Procedure DeleteName(Const Name: String);
    Function IndexOf(Const Name: String): Integer;
    Procedure Insert(Position: Integer; Const Name: String);

    Property Variable1[Const Name: String]: Variant Read GetVariable1 Write SetVariable1; Default;
    Property Variable2[Const Name: String]: Variant Read GetVariable2 Write SetVariable2;
    Property Variable3[Const Name: String]: Variant Read GetVariable3 Write SetVariable3;
    Property Variable4[Const Name: String]: Variant Read GetVariable4 Write SetVariable4;
    Property Variable5[Const Name: String]: Variant Read GetVariable5 Write SetVariable5;
    Property Variable6[Const Name: String]: Variant Read GetVariable6 Write SetVariable6;
    Property Variable7[Const Name: String]: Variant Read GetVariable7 Write SetVariable7;
    Property Variable8[Const Name: String]: Variant Read GetVariable8 Write SetVariable8;
    Property Variable9[Const Name: String]: Variant Read GetVariable9 Write SetVariable9;
    Property Variable10[Const Name: String]: Variant Read GetVariable10 Write SetVariable10;

    Property Value1[Index: Integer]: Variant Read GetValue1 Write SetValue1;
    Property Value2[Index: Integer]: Variant Read GetValue2 Write SetValue2;
    Property Value3[Index: Integer]: Variant Read GetValue3 Write SetValue3;
    Property Value4[Index: Integer]: Variant Read GetValue4 Write SetValue4;
    Property Value5[Index: Integer]: Variant Read GetValue5 Write SetValue5;
    Property Value6[Index: Integer]: Variant Read GetValue6 Write SetValue6;
    Property Value7[Index: Integer]: Variant Read GetValue7 Write SetValue7;
    Property Value8[Index: Integer]: Variant Read GetValue8 Write SetValue8;
    Property Value9[Index: Integer]: Variant Read GetValue9 Write SetValue9;
    Property Value10[Index: Integer]: Variant Read GetValue10 Write SetValue10;

    Property Name[Index: Integer]: String Read GetName Write SetName;
    Property Count: Integer Read GetCount;
    Property Sorted: Boolean Read GetSorted Write SetSorted;
    Property List: TStringList Read FList Write FList;
  End ;

  TSplitFunction = Class
  Protected
    FMatchFuncs, FSplitTo: TStrings;
    FParser: TFunParser;
    FVariables: TIntVariables;
  Public
    Constructor Create(MatchFuncs, SplitTo: TStrings; Variables: TIntVariables);
    Destructor Destroy; Override;
    Procedure Split(s: String);
  End ;

  TFunInterpretator = Class(TObject)
  Private
    FScript, FScript1,
      STextError, FFunction: TStringList;
  Public
    Parser: TFunParser;
    InternalVariables, InternalConsts: TIntVariables;
    InternalStrList: TStringList;
    InternalArrayList: TStringList;
    ErrorResult: Longword;
    ErrorString: String;
    MaxRecurseVar: Integer;
    InternalParamPosition,
      InternalParamPosition2: Longint;

    Constructor Create;
    Destructor Destroy; Override;
    Procedure GetValue(Const Name: String; Var Value: Variant); Virtual;
    Procedure SetValue(Const Name: String; Value: Variant); Virtual;
    Procedure GetSetValueFunction(Const Name: String; p1, p2, p3, p4, p5, p6, p7, p8, p9, p10: Variant; Var Value: Variant); Virtual;
    Procedure SplitExpressions(Memo, MatchFuncs, SplitTo: TStrings;
      Variables: TIntVariables);

    Function ExpandExpression(Const Text: AnsiString): AnsiString;
    Procedure PrepareScript(MemoFrom, MemoTo, MemoErr: TStrings); Virtual;
    Procedure ExecuteScript(Memo: TStrings); Virtual;

  End ;

Function ExpandVar(Const s: String; Var i, j: Integer): String;
Function IsValidTimestamp(Const S: ShortString): boolean;
Function VrStrToTimestamp(Const S: ShortString): TDateTime;
Function IsValidDate(Const S: ShortString): Boolean;
Function VrStrToDate(Const S: ShortString): TDateTime;
Function IsValidTime(Const S: ShortString): Boolean;
Function VrStrToTime(Const S: ShortString): TDateTime;
Function FormatMaskText(Const EditMask: String; Const Value: String): String;
Function MaskGetMaskSave(Const EditMask: String): Boolean;
Function MaskGetMaskBlank(Const EditMask: String): Char;
Function MaskGetFldSeparator(Const EditMask: String): Integer;

Implementation

Type
  TCharArray = Array[0..31999] Of Char;
  PCharArray = ^TCharArray;
  lrec = Record
    Name: String[16];
    n: Integer;
  End ;

  TIVariable = Record
    Name: Variant;
    Value1: Variant;
    Value2: Variant;
    Value3: Variant;
    Value4: Variant;
    Value5: Variant;
    Value6: Variant;
    Value7: Variant;
    Value8: Variant;
    Value9: Variant;
    Value10: Variant;
  End ;
  PIVariable = ^TIVariable;

Const
  ttIf = #1;
  ttGoto = #2;
  ttProc = #3;

  ttGe = #1;
  ttLe = #2;
  ttNe = #3;
  ttOr = #4;
  ttAnd = #5;
  ttInt = #6;
  ttFrac = #7;
  ttUnMinus = #9;
  ttUnPlus = #10;
  ttStr = #11;
  ttNot = #12;
  ttMod = #13;
  ttDiv = #14;
  ttShl = #15;
  ttShr = #16;
  ttXor = #17;

  ttLn = #18;
  ttExp = #19;
  ttRandom = #20;
  ttRound = #21;
  //ttInt64 = #22;

Var
  labels: Array[0..100] Of lrec;
  labc: Integer;

Function MaskGetCharType(Const EditMask: String; MaskOffset: Integer): TMaskCharType;
Var
  MaskChar: Char;
Begin
  Result := mcLiteral;
  MaskChar := #0;
  If MaskOffset <= Length(EditMask) Then
    MaskChar := EditMask[MaskOffset];
  If MaskOffset > Length(EditMask) Then
    Result := mcNone

  Else If ByteType(EditMask, MaskOffset) <> mbSingleByte Then
    Result := mcLiteral

  Else If (MaskOffset > 1) And (EditMask[MaskOffset - 1] = mDirLiteral) And
    (ByteType(EditMask, MaskOffset - 1) = mbSingleByte) And
    Not ((MaskOffset > 2) And (EditMask[MaskOffset - 2] = mDirLiteral) And
    (ByteType(EditMask, MaskOffset - 2) = mbSingleByte)) Then
    Result := mcLiteral

  Else If (MaskChar = MaskFieldSeparator) And
    (Length(EditMask) >= 4) And
    (MaskOffset > Length(EditMask) - 4) Then
    Result := mcFieldSeparator

  Else If (Length(EditMask) >= 4) And
    (MaskOffset > (Length(EditMask) - 4)) And
    (EditMask[MaskOffset - 1] = MaskFieldSeparator) And
    Not ((MaskOffset > 2) And (EditMask[MaskOffset - 2] = mDirLiteral) And
    (ByteType(EditMask, MaskOffset - 2) <> mbTrailByte)) Then
    Result := mcField

  Else If MaskChar In [mMskTimeSeparator, mMskDateSeparator] Then
    Result := mcIntlLiteral

  Else If MaskChar In [mDirReverse, mDirUpperCase, mDirLowerCase,
    mDirLiteral] Then
    Result := mcDirective

  Else If MaskChar In [mMskAlphaOpt, mMskAlphaNumOpt, mMskAsciiOpt,
    mMskNumSymOpt, mMskNumericOpt] Then
    Result := mcMaskOpt

  Else If MaskChar In [mMskAlpha, mMskAlphaNum, mMskAscii, mMskNumeric] Then
    Result := mcMask;
End ;

Function MaskGetCurrentDirectives(Const EditMask: String;
  MaskOffset: Integer): TMaskDirectives;
Var
  I: Integer;
  MaskChar: Char;
Begin
  Result := [];
  For I := 1 To Length(EditMask) Do
    Begin
      MaskChar := EditMask[I];
      If (MaskChar = mDirReverse) Then
        Include(Result, mdReverseDir)
      Else If (MaskChar = mDirUpperCase) And (I < MaskOffset) Then
        Begin
          Exclude(Result, mdLowerCase);
          If Not ((I > 1) And (EditMask[I - 1] = mDirLowerCase)) Then
            Include(Result, mdUpperCase);
        End
      Else If (MaskChar = mDirLowerCase) And (I < MaskOffset) Then
        Begin
          Exclude(Result, mdUpperCase);
          Include(Result, mdLowerCase);
        End ;
    End ;
  If MaskGetCharType(EditMask, MaskOffset) = mcLiteral Then
    Include(Result, mdLiteralChar);
End ;

Function MaskIntlLiteralToChar(IChar: Char): Char;
Begin
  Result := IChar;
  Case IChar Of
    mMskTimeSeparator: Result := TimeSeparator;
    mMskDateSeparator: Result := DateSeparator;
  End ;
End ;

Function MaskDoFormatText(Const EditMask: String; Const Value: String;
  Blank: Char): String;
Var
  I: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  Dir: TMaskDirectives;
Begin
  Result := Value;
  Dir := MaskGetCurrentDirectives(EditMask, 1);

  If Not (mdReverseDir In Dir) Then
    Begin
      { starting at the beginning, insert literal chars in the string
        and add spaces on the end }
      Offset := 1;
      For MaskOffset := 1 To Length(EditMask) Do
        Begin
          CType := MaskGetCharType(EditMask, MaskOffset);

          If CType In [mcLiteral, mcIntlLiteral] Then
            Begin
              Result := Copy(Result, 1, Offset - 1) +
                MaskIntlLiteralToChar(EditMask[MaskOffset]) +
                Copy(Result, Offset, Length(Result) - Offset + 1);
              Inc(Offset);
            End
          Else If CType In [mcMask, mcMaskOpt] Then
            Begin
              If Offset > Length(Result) Then
                Result := Result + Blank;
              Inc(Offset);
            End ;
        End ;
    End
  Else
    Begin
      { starting at the end, insert literal chars in the string
        and add spaces at the beginning }
      Offset := Length(Result);
      For I := 0 To (Length(EditMask) - 1) Do
        Begin
          MaskOffset := Length(EditMask) - I;
          CType := MaskGetCharType(EditMask, MaskOffset);
          If CType In [mcLiteral, mcIntlLiteral] Then
            Begin
              Result := Copy(Result, 1, Offset) +
                MaskIntlLiteralToChar(EditMask[MaskOffset]) +
                Copy(Result, Offset + 1, Length(Result) - Offset);
            End
          Else If CType In [mcMask, mcMaskOpt] Then
            Begin
              If Offset < 1 Then
                Result := Blank + Result
              Else
                Dec(Offset);
            End ;
        End ;
    End ;
End ;

Function MaskGetMaskSave(Const EditMask: String): Boolean;
Var
  I: Integer;
  Sep1, Sep2: Integer;
Begin
  Result := True;
  If Length(EditMask) >= 4 Then
    Begin
      Sep1 := -1;
      Sep2 := -1;
      I := Length(EditMask);
      While Sep2 < 0 Do
        Begin
          If (MaskGetCharType(EditMask, I) = mcFieldSeparator) Then
            Begin
              If Sep1 < 0 Then
                Sep1 := I
              Else
                Sep2 := I;
            End ;
          Dec(I);
          If (I <= 0) Or (I < Length(EditMask) - 4) Then
            Break;
        End ;
      If Sep2 < 0 Then
        Sep2 := Sep1;
      If Sep2 <> Length(EditMask) Then
        Result := Not (EditMask[Sep2 + 1] = MaskNoSave);
    End ;
End ;

Function MaskGetMaskBlank(Const EditMask: String): Char;
Begin
  Result := DefaultBlank;
  If Length(EditMask) >= 4 Then
    Begin
      If (MaskGetCharType(EditMask, Length(EditMask) - 1) =
        mcFieldSeparator) Then
        Begin
          {in order for blank specifier to be valid, there
           must also be a save specifier }
          If (MaskGetCharType(EditMask, Length(EditMask) - 2) =
            mcFieldSeparator) Or
            (MaskGetCharType(EditMask, Length(EditMask) - 3) =
            mcFieldSeparator) Then
            Begin
              Result := EditMask[Length(EditMask)];
            End ;
        End ;
    End ;
End ;

Function MaskGetFldSeparator(Const EditMask: String): Integer;
Var
  I: Integer;
Begin
  Result := -1;
  If Length(EditMask) >= 4 Then
    Begin
      For I := (Length(EditMask) - 4) To Length(EditMask) Do
        Begin
          If (MaskGetCharType(EditMask, I) = mcFieldSeparator) Then
            Begin
              Result := I;
              Exit;
            End ;
        End ;
    End ;
End ;

Function MaskOffsetToOffset(Const EditMask: String; MaskOffset: Integer): Integer;
Var
  I: Integer;
  CType: TMaskCharType;
Begin
  Result := 0;
  For I := 1 To MaskOffset Do
    Begin
      CType := MaskGetCharType(EditMask, I);
      If Not (CType In [mcDirective, mcField, mcFieldSeparator]) Then
        Inc(Result);
    End ;
End ;

Function OffsetToMaskOffset(Const EditMask: String; Offset: Integer): Integer;
Var
  I: Integer;
  Count: Integer;
  MaxChars: Integer;
Begin
  MaxChars := MaskOffsetToOffset(EditMask, Length(EditMask));
  If Offset > MaxChars Then
    Begin
      Result := -1;
      Exit;
    End ;

  Result := 0;
  Count := Offset;
  For I := 1 To Length(EditMask) Do
    Begin
      Inc(Result);
      If Not (mcDirective = MaskGetCharType(EditMask, I)) Then
        Begin
          Dec(Count);
          If Count < 0 Then
            Exit;
        End ;
    End ;
End ;

Function IsLiteralChar(Const EditMask: String; Offset: Integer): Boolean;
Var
  MaskOffset: Integer;
  CType: TMaskCharType;
Begin
  Result := False;
  MaskOffset := OffsetToMaskOffset(EditMask, Offset);
  If MaskOffset >= 0 Then
    Begin
      CType := MaskGetCharType(EditMask, MaskOffset);
      Result := CType In [mcLiteral, mcIntlLiteral];
    End ;
End ;

Function PadSubField(Const EditMask: String; Const Value: String;
  StartFld, StopFld, Len: Integer; Blank: Char): String;
Var
  Dir: TMaskDirectives;
  StartPad: Integer;
  K: Integer;
Begin
  If (StopFld - StartFld) < Len Then
    Begin
      { found literal at position J, now pad it }
      Dir := MaskGetCurrentDirectives(EditMask, 1);
      StartPad := StopFld - 1;
      If mdReverseDir In Dir Then
        StartPad := StartFld - 1;
      Result := Copy(Value, 1, StartPad);
      For K := 1 To (Len - (StopFld - StartFld)) Do
        Result := Result + Blank;
      Result := Result + Copy(Value, StartPad + 1, Length(Value));
    End
  Else If (StopFld - StartFld) > Len Then
    Begin
      Dir := MaskGetCurrentDirectives(EditMask, 1);
      If mdReverseDir In Dir Then
        Result := Copy(Value, 1, StartFld - 1) +
          Copy(Value, StopFld - Len, Length(Value))
      Else
        Result := Copy(Value, 1, StartFld + Len - 1) +
          Copy(Value, StopFld, Length(Value));
    End
  Else
    Result := Value;
End ;

Function PadInputLiterals(Const EditMask: String; Const Value: String;
  Blank: Char): String;
Var
  J: Integer;
  LastLiteral, EndSubFld: Integer;
  Offset, MaskOffset: Integer;
  CType: TMaskCharType;
  MaxChars: Integer;
Begin
  LastLiteral := 0;

  Result := Value;
  For MaskOffset := 1 To Length(EditMask) Do
    Begin
      CType := MaskGetCharType(EditMask, MaskOffset);
      If CType In [mcLiteral, mcIntlLiteral] Then
        Begin
          Offset := MaskOffsetToOffset(EditMask, MaskOffset);
          EndSubFld := Length(Result) + 1;
          For J := LastLiteral + 1 To Length(Result) Do
            Begin
              If Result[J] = MaskIntlLiteralToChar(EditMask[MaskOffset]) Then
                Begin
                  EndSubFld := J;
                  Break;
                End ;
            End ;
          { we have found a subfield, ensure that it complies }
          If EndSubFld > Length(Result) Then
            Result := Result + MaskIntlLiteralToChar(EditMask[MaskOffset]);
          Result := PadSubField(EditMask, Result, LastLiteral + 1, EndSubFld,
            Offset - (LastLiteral + 1), Blank);
          LastLiteral := Offset;
        End ;
    End ;

  {ensure that the remainder complies, too }
  MaxChars := MaskOffsetToOffset(EditMask, Length(EditMask));
  If Length(Result) <> MaxChars Then
    Result := PadSubField(EditMask, Result, LastLiteral + 1, Length(Result) + 1,
      MaxChars - LastLiteral, Blank);

  { replace non-literal blanks with blank char }
  For Offset := 1 To Length(Result) Do
    Begin
      If Result[Offset] = ' ' Then
        Begin
          If Not IsLiteralChar(EditMask, Offset - 1) Then
            Result[Offset] := Blank;
        End ;
    End ;
End ;

Function FormatMaskText(Const EditMask: String; Const Value: String): String;
Begin
  If MaskGetMaskSave(EditMask) Then
    Result := PadInputLiterals(EditMask, Value, ' ')
  Else
    Result := MaskDoFormatText(EditMask, Value, ' ');
End ;

Function Remain(Const S: String; From: Integer): String;
Begin
  Result := Copy(s, From, MaxInt);
End ;

Function GetIdentifier(Const s: String; Var i: Integer): String;
Var
  k: Integer;
Begin
  While (i <= Length(s)) And (s[i] <= ' ') Do
    Inc(i);
  k := i;
  While (i <= Length(s)) And (s[i] > ' ') Do
    Inc(i);
  Result := Copy(s, k, i - k);
End ;

Function ExpandVar(Const s: String; Var i, j: Integer): String;
Var
  c: Integer;
  fl1, fl2: Boolean;
Begin
  j := i;
  fl1 := True;
  fl2 := True;
  c := 0;
  Result := '';
  If (s = '') Or (j > Length(s)) Then
    Exit;
  Dec(j);
  Repeat
    Inc(j);
    If fl1 And fl2 Then
      If s[j] = '[' Then
        Begin
          If c = 0 Then
            i := j;
          Inc(c);
        End
      Else If s[j] = ']' Then
        Dec(c);
    If fl1 Then
      If s[j] = '"' Then
        fl2 := Not fl2;
    If fl2 Then
      If s[j] = '''' Then
        fl1 := Not fl1;
  Until (c = 0) Or (j >= Length(s));
  Result := Copy(s, i + 1, j - i - 1);
End ;

Function IsValidTimestamp(Const S: ShortString): boolean;
Begin
  If (length(S) < 19)
    Or Not (S[5] In ['-', '.', '/'])
    Or (S[8] <> S[5])
    Or (S[11] <> ' ')
    Or (S[14] <> ':')
    Or (S[17] <> ':') Then
    Result := False
  Else
    Try
      EncodeDate(
        StrToInt(copy(S, 1, 4)),
        StrToInt(copy(S, 6, 2)),
        StrToInt(copy(S, 9, 2)));
      EncodeTime(
        StrToInt(copy(S, 12, 2)),
        StrToInt(copy(S, 15, 2)),
        StrToInt(copy(S, 18, 2)),
        0);
      Result := True;
    Except
      Result := False;
    End ;
End ;

Function VrStrToTimestamp(Const S: ShortString): TDateTime;
Begin
  Result := EncodeDate(
    StrToInt(copy(S, 1, 4)),
    StrToInt(copy(S, 6, 2)),
    StrToInt(copy(S, 9, 2)))
    +
    EncodeTime(
    StrToInt(copy(S, 12, 2)),
    StrToInt(copy(S, 15, 2)),
    StrToInt(copy(S, 18, 2)),
    0);
  fsSetMillisecond(Result, 0);
End ;

Function IsValidDate(Const S: ShortString): Boolean;
Begin
  If (length(S) < 10)
    Or Not (S[5] In ['-', '.', '/'])
    Or (S[8] <> S[5]) Then
    Result := False
  Else
    Try
      EncodeDate(
        StrToInt(copy(S, 1, 4)),
        StrToInt(copy(S, 6, 2)),
        StrToInt(copy(S, 9, 2)));
      Result := True;
    Except
      Result := False;
    End ;
End ;

Function VrStrToDate(Const S: ShortString): TDateTime;
Begin
  Result := EncodeDate(
    StrToInt(copy(S, 1, 4)),
    StrToInt(copy(S, 6, 2)),
    StrToInt(copy(S, 9, 2)));
End ;

Function IsValidTime(Const S: ShortString): Boolean;
Begin
  If (length(S) < 8)
    Or (S[3] <> ':')
    Or (S[6] <> ':') Then
    Result := False
  Else
    Try
      EncodeTime(
        StrToInt(copy(S, 1, 2)),
        StrToInt(copy(S, 4, 2)),
        StrToInt(copy(S, 7, 2)),
        0);
      Result := True;
    Except
      Result := False;
    End ;
End ;

Function VrStrToTime(Const S: ShortString): TDateTime;
Begin
  Result := EncodeTime(
    StrToInt(copy(S, 1, 2)),
    StrToInt(copy(S, 4, 2)),
    StrToInt(copy(S, 7, 2)),
    0);
End ;
{ TIntVariables }

Constructor TIntVariables.Create;
Begin
  Inherited Create;
  FList := TStringList.Create;
  FList.Duplicates := dupIgnore;
End ;

Destructor TIntVariables.Destroy;
Begin
  Clear;
  FList.Free;
  Inherited Destroy;
End ;

Procedure TIntVariables.Assign(Value: TIntVariables);
Var
  i: Integer;
Begin
  Clear;
  For i := 0 To Value.Count - 1 Do
    Begin
      SetVariable1(Value.Name[i], Value.Value1[i]);
      SetVariable2(Value.Name[i], Value.Value2[i]);
      SetVariable3(Value.Name[i], Value.Value3[i]);
      SetVariable4(Value.Name[i], Value.Value4[i]);
      SetVariable5(Value.Name[i], Value.Value5[i]);
      SetVariable6(Value.Name[i], Value.Value6[i]);
      SetVariable7(Value.Name[i], Value.Value7[i]);
      SetVariable8(Value.Name[i], Value.Value8[i]);
      SetVariable9(Value.Name[i], Value.Value9[i]);
      SetVariable10(Value.Name[i], Value.Value10[i]);
    End ;
End ;

Procedure TIntVariables.Clear;
Begin
  While FList.Count > 0 Do
    Delete(0);
End ;

Procedure TIntVariables.SetVariable1(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value1 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value1 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable1(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value1;
End ;

Procedure TIntVariables.SetVariable2(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value2 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value2 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable2(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value2;
End ;

Procedure TIntVariables.SetVariable3(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value3 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value3 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable3(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value3;
End ;

Procedure TIntVariables.SetVariable4(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value4 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value4 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable4(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value4;
End ;

Procedure TIntVariables.SetVariable5(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value5 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value5 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable5(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value5;
End ;

Procedure TIntVariables.SetVariable6(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value6 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value6 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable6(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value6;
End ;

Procedure TIntVariables.SetVariable7(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value7 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value7 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable7(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value7;
End ;

Procedure TIntVariables.SetVariable8(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value8 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value8 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable8(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value8;
End ;

Procedure TIntVariables.SetVariable9(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value9 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value9 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable9(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value9;
End ;

Procedure TIntVariables.SetVariable10(Const Name: String; Value: Variant);
Var
  i: Integer;
  p: PIVariable;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      PIVariable(FList.Objects[i])^.Value10 := Value;
      PIVariable(FList.Objects[i])^.Name := Name;
    End
  Else
    Begin
      New(p);
      p^.Value10 := Value;
      p^.Name := Name;
      FList.AddObject(Name, TObject(p));
    End ;
End ;

Function TIntVariables.GetVariable10(Const Name: String): Variant;
Var
  i: Integer;
Begin
  Result := Null;
  i := IndexOf(Name);
  If i <> -1 Then
    Result := PIVariable(FList.Objects[i])^.Value10;
End ;

Procedure TIntVariables.SetValue1(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value1 := Value;
End ;

Function TIntVariables.GetValue1(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value1;
End ;

Procedure TIntVariables.SetValue2(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value2 := Value;
End ;

Function TIntVariables.GetValue2(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value2;
End ;

Procedure TIntVariables.SetValue3(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value3 := Value;
End ;

Function TIntVariables.GetValue3(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value3;
End ;

Procedure TIntVariables.SetValue4(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value4 := Value;
End ;

Function TIntVariables.GetValue4(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value4;
End ;

Procedure TIntVariables.SetValue5(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value5 := Value;
End ;

Function TIntVariables.GetValue5(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value5;
End ;

Procedure TIntVariables.SetValue6(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value6 := Value;
End ;

Function TIntVariables.GetValue6(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value6;
End ;

Procedure TIntVariables.SetValue7(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value7 := Value;
End ;

Function TIntVariables.GetValue7(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value7;
End ;

Procedure TIntVariables.SetValue8(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value8 := Value;
End ;

Function TIntVariables.GetValue8(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value8;
End ;

Procedure TIntVariables.SetValue9(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value9 := Value;
End ;

Function TIntVariables.GetValue9(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value9;
End ;

Procedure TIntVariables.SetValue10(Index: Integer; Value: Variant);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  PIVariable(FList.Objects[Index])^.Value10 := Value;
End ;

Function TIntVariables.GetValue10(Index: Integer): Variant;
Begin
  Result := 0;
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := PIVariable(FList.Objects[Index])^.Value10;
End ;

Function TIntVariables.IndexOf(Const Name: String): Integer;
Begin
  Result := FList.IndexOf(Name);
End ;

Procedure TIntVariables.Insert(Position: Integer; Const Name: String);
Begin
  SetVariable1(Name, 0);
  FList.Move(FList.IndexOf(Name), Position);
End ;

Function TIntVariables.GetCount: Integer;
Begin
  Result := FList.Count;
End ;

Procedure TIntVariables.SetName(Index: Integer; Value: String);
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  FList[Index] := Value;
End ;

Function TIntVariables.GetName(Index: Integer): String;
Begin
  Result := '';
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  Result := FList[Index];
End ;

Procedure TIntVariables.Delete(Index: Integer);
Var
  p: PIVariable;
Begin
  If (Index < 0) Or (Index >= FList.Count) Then
    Exit;
  p := PIVariable(FList.Objects[Index]);
  Dispose(p);
  FList.Delete(Index);
End ;

Procedure TIntVariables.DeleteName(Const Name: String);
Var
  p: PIVariable;
  i: Integer;
Begin
  i := IndexOf(Name);
  If i <> -1 Then
    Begin
      p := PIVariable(FList.Objects[I]);
      Dispose(p);
      FList.Delete(I);
    End ;
End ;

Procedure TIntVariables.SetSorted(Value: Boolean);
Begin
  FList.Sorted := Value;
End ;

Function TIntVariables.GetSorted: Boolean;
Begin
  Result := FList.Sorted;
End ;

{ TFunParser }
{$IFDEF DCC6OrLater}
  {$WARNINGS OFF}
{$ENDIF}

Function TFunParser.CalculateOPZ(Const s: String): Variant;
Var
  i, j, k, i1, st, ci, cn: Integer;
  ds, s1, s2, s3, s4, s5, p4, p5, p6, p7, p8, p9, p10: String;
  nm: Array[1..32] Of Variant;
  v: Double;
  I64: Int64;
  Ok: boolean;
Begin
  For I := 1 to 32 do
    nm[I] := 0 ;
  nm[1] := 0 ;

  Try
    st := 1;
    i := 1;
    nm[1] := 0;
    While i <= Length(s) Do
      Begin
        j := i;
        Case s[i] Of
          '+':
            nm[st - 2] := nm[st - 2] + nm[st - 1];
          ttOr:
            nm[st - 2] := nm[st - 2] Or nm[st - 1];
          '-':
            nm[st - 2] := nm[st - 2] - nm[st - 1];
          '*':
            nm[st - 2] := nm[st - 2] * nm[st - 1];
          ttAnd:
            nm[st - 2] := nm[st - 2] And nm[st - 1];
          '/':
            If nm[st - 1] <> 0 Then
              nm[st - 2] := nm[st - 2] / nm[st - 1]
            Else
              nm[st - 2] := 0;
          '>':
            If nm[st - 2] > nm[st - 1] Then
              nm[st - 2] := 1
            Else
              nm[st - 2] := 0;
          '<':
            If nm[st - 2] < nm[st - 1] Then
              nm[st - 2] := 1
            Else
              nm[st - 2] := 0;
          '=':
            If nm[st - 2] = nm[st - 1] Then
              nm[st - 2] := 1
            Else
              nm[st - 2] := 0;
          ttNe:
            If nm[st - 2] <> nm[st - 1] Then
              nm[st - 2] := 1
            Else
              nm[st - 2] := 0;
          ttGe:
            If nm[st - 2] >= nm[st - 1] Then
              nm[st - 2] := 1
            Else
              nm[st - 2] := 0;
          ttLe:
            If nm[st - 2] <= nm[st - 1] Then
              nm[st - 2] := 1
            Else
              nm[st - 2] := 0;
          ttInt:
            Begin
              v := nm[st - 1];
              If Abs(Round(v) - v) < 1E-10 Then
                v := Round(v)
              Else
                v := Int(v);

              nm[st - 1] := v;
            End ;
          ttFrac:
            Begin
              v := nm[st - 1];
              If Abs(Round(v) - v) < 1E-10 Then
                v := Round(v);

              nm[st - 1] := Frac(v);
            End ;
          ttRound:
            Begin
              v := Round(nm[st - 1]);
              nm[st - 1] := v;
            End ;
          ttUnMinus:
            nm[st - 1] := -nm[st - 1];
          ttUnPlus: ;
          ttStr:
            Begin
              Try
                If nm[st - 1] <> Null Then
                  s1 := nm[st - 1]
                Else
                  s1 := '';
                nm[st - 1] := s1;
              Except
              End ;
            End ;
          ttNot:
            If nm[st - 1] = 0 Then
              nm[st - 1] := 1
            Else
              nm[st - 1] := 0;
          ttMod: nm[st - 2] := nm[st - 2] Mod nm[st - 1];
          ttDiv: nm[st - 2] := nm[st - 2] Div nm[st - 1];
          ttXor: nm[st - 2] := nm[st - 2] Xor nm[st - 1];
          ttShl: nm[st - 2] := nm[st - 2] Shl nm[st - 1];
          ttShr: nm[st - 2] := nm[st - 2] Shr nm[st - 1];

          ttExp: nm[st - 1] := Exp(nm[st - 1]);
          ttLn: nm[st - 1] := Ln(nm[st - 1]);
          ttRandom:
            Begin
              nm[st - 1] := Random(Integer(Round(nm[st - 1])));
            End ;
          ' ': ;
          '[':
            Begin
              k := i;
              s1 := ExpandVar(s, k, i);
              If Assigned(FOnGetValue) Then
                Begin
                  nm[st] := Null;
                  FOnGetValue(s1, nm[st]);
                End ;
              Inc(st);
            End
          Else
            Begin
              If s[i] = '''' Then
                Begin
                  s1 := GetString(s, i);
                  s1 := Copy(s1, 2, Length(s1) - 2);
                  While Pos('''' + '''', s1) <> 0 Do
                    Delete(s1, Pos('''' + '''', s1), 1);
                  nm[st] := s1;
                  k := i;
                End
              Else
                Begin
                  k := i;
                  s1 := GetIdentifier(s, k);
                  If (s1 <> '') And (s1[1] In ['0'..'9', '.', ',']) Then
                    Begin
                      For i1 := 1 To Length(s1) Do
                        If s1[i1] In ['.', ','] Then
                          s1[i1] := DecimalSeparator;
                      nm[st] := StrToFloat(s1);
                    End
                  Else If AnsiCompareText(s1, 'TRUE') = 0 Then
                    nm[st] := True
                  Else If AnsiCompareText(s1, 'FALSE') = 0 Then
                    nm[st] := False
                  Else If s[k] = '[' Then
                    Begin
                      s1 := 'GETARRAY(' + s1 + ', ' + ExpandVar(s, k, i) + ')';
                      nm[st] := Calculate(s1);
                      k := i;
                    End
                  Else If s[k] = '(' Then
                    Begin
                      s1 := AnsiUpperCase(s1);
                      Get10Parameters(s, k, s2, s3, s4, p4, p5, p6, p7, p8, p9, p10);
                      If s1 = 'COPY' Then
                        Begin
                          ci := StrToInt(Calculate(s3));
                          cn := StrToInt(Calculate(s4));
                          s5 := trimright(Calculate(s2));
                          If length(s5) >= ci Then
                            nm[st] := Copy(s5, ci, cn)
                          Else
                            nm[st] := '';
                        End
                      Else If s1 = 'IF' Then
                        Begin
                          If Int(StrToFloat(Trim(Calculate(s2)))) <> 0 Then
                            s1 := s3
                          Else
                            s1 := s4;
                          nm[st] := Calculate(s1);
                        End
                      Else If s1 = 'STRTODATE' Then
                        Begin
                          Try
                            ds := Trim(Calculate(s2));
                            If IsValidDate(ds) Then
                              nm[st] := VrStrToDate(ds)
                            Else
                              nm[st] := StrToDate(ds);
                          Except
                            nm[st] := null;
                          End ;
                        End
                      Else If s1 = 'STRTOTIME' Then
                        Begin
                          Try
                            ds := Trim(Calculate(s2));
                            If IsValidTime(ds) Then
                              nm[st] := VrStrToTime(ds)
                            Else
                              nm[st] := StrToTime(ds);
                          Except
                            nm[st] := null;
                          End ;
                        End
                      Else If s1 = 'STRTODATETIME' Then
                        Begin
                          Try
                            ds := Trim(Calculate(s2));
                            If IsValidTimestamp(ds) Then
                              nm[st] := VrStrToTimeStamp(ds)
                            Else
                              nm[st] := StrToDateTime(ds);
                          Except
                            nm[st] := null;
                          End ;
                        End
                      Else If Assigned(FOnFunction) Then
                        Begin
                          nm[st] := Null;
                          FOnFunction(s1, s2, s3, s4, p4, p5, p6, p7, p8, p9, p10, nm[st]);
                        End ;
                      Dec(k);
                    End
                  Else If Assigned(FOnGetValue) Then
                    Begin
                      nm[st] := Null;
                      FOnGetValue(AnsiUpperCase(s1), nm[st]);
                    End ;
                End ;
              i := k;
              Inc(st);
            End ;
        End ;
        If s[j] In ['+', '-', '*', '/', '>', '<', '=', ttGe, ttLe, ttNe,
          ttOr, ttAnd, ttMod, ttDiv, ttXor, ttShl, ttShr] Then
          Dec(st);
        Inc(i);
      End ;
  Finally
    Result := nm[1] ;
  End ;
End ;
{$IFDEF DCC6OrLater}
  {$WARNINGS ON}
{$ENDIF}

Function TFunParser.GetIdentifier(Const s: String; Var i: Integer): String;
Var
  k, n: Integer;
Begin
  n := 0;
  While (i <= Length(s)) And (s[i] <= ' ') Do
    Inc(i);
  k := i;
  Dec(i);
  Repeat
    Inc(i);
    While (i <= Length(s)) And
      Not (s[i] In [' ', #13, '+', '-', '*', '/', '>', '<', '=', '(', ')', '[']) Do
      Begin
        If s[i] = '"' Then
          Inc(n);
        Inc(i);
      End ;
  Until (n Mod 2 = 0) Or (i >= Length(s));
  Result := Copy(s, k, i - k);
End ;

Function TFunParser.GetString(Const s: String; Var i: Integer): String;
Var
  k: Integer;
  f: Boolean;
Begin
  k := i;
  Inc(i);
  Repeat
    While (i <= Length(s)) And (s[i] <> '''') Do
      Inc(i);
    f := True;
    If (i < Length(s)) And (s[i + 1] = '''') Then
      Begin
        f := False;
        Inc(i, 2);
      End ;
  Until f;
  Result := Copy(s, k, i - k + 1);
  Inc(i);
End ;

Procedure TFunParser.Get10Parameters(Const s: String; Var i: Integer;
  Var s1, s2, s3, s4, s5, s6, s7, s8, s9, s10: String);
Var
  c, d, oi, ci: Integer;
Begin
  s1 := '';
  s2 := '';
  s3 := '';
  s4 := '';
  s5 := '';
  s6 := '';
  s7 := '';
  s8 := '';
  s9 := '';
  s10 := '';
  c := 1;
  d := 1;
  oi := i + 1;
  ci := 1;
  Repeat
    Inc(i);
    If s[i] = '''' Then
      If d = 1 Then
        Inc(d)
      Else
        d := 1;
    If d = 1 Then
      Begin
        If s[i] = '(' Then
          Inc(c)
        Else If s[i] = ')' Then
          Dec(c);
        If (s[i] = ',') And (c = 1) Then
          Begin
            {If ci = 1 Then
              s1 := Copy(s, oi, i - oi)
            Else
              s2 := Copy(s, oi, i - oi); }
            Case ci Of
              1: s1 := Copy(s, oi, i - oi);
              2: s2 := Copy(s, oi, i - oi);
              3: s3 := Copy(s, oi, i - oi);
              4: s4 := Copy(s, oi, i - oi);
              5: s5 := Copy(s, oi, i - oi);
              6: s6 := Copy(s, oi, i - oi);
              7: s7 := Copy(s, oi, i - oi);
              8: s8 := Copy(s, oi, i - oi);
              9: s9 := Copy(s, oi, i - oi);
              10: s10 := Copy(s, oi, i - oi);
            End ;
            oi := i + 1;
            Inc(ci);
          End ;
      End ;
  Until (c = 0) Or (i >= Length(s));
  Case ci Of
    1: s1 := Copy(s, oi, i - oi);
    2: s2 := Copy(s, oi, i - oi);
    3: s3 := Copy(s, oi, i - oi);
    4: s4 := Copy(s, oi, i - oi);
    5: s5 := Copy(s, oi, i - oi);
    6: s6 := Copy(s, oi, i - oi);
    7: s7 := Copy(s, oi, i - oi);
    8: s8 := Copy(s, oi, i - oi);
    9: s9 := Copy(s, oi, i - oi);
    10: s10 := Copy(s, oi, i - oi);
  End ;
  If c <> 0 Then
    Raise Exception.Create('');
  Inc(i);
End ;

Function TFunParser.String2OPZ(s: String): String;
Label
  1;
Var
  i, i1, j, p: Integer;
  stack: String;
  res, s1, s2, s3, s4, p4, p5, p6, p7, p8, p9, p10: String;
  vr: Boolean;
  c: Char;

  Function Priority(c: Char): Integer;
  Begin
    Case c Of
      '(': Priority := 5;
      ')': Priority := 4;
      '=', '>', '<', ttGe, ttLe, ttNe: Priority := 3;
      '+', '-', ttUnMinus, ttUnPlus: Priority := 2;
      '*', '/', ttOr, ttAnd, ttNot, ttMod: Priority := 1;
      ttInt, ttFrac, ttRound, ttExp, ttLn, ttStr, ttRandom, ttDiv, ttXor, ttShl, ttShr: Priority := 0;
      Else
        Priority := 0;
    End ;
  End ;

  Procedure ProcessQuotes(Var s: String);
  Var
    i: Integer;
  Begin
    If (Length(s) = 0) Or (s[1] <> '''') Then
      Exit;
    i := 2;
    If Length(s) > 2 Then
      While i <= Length(s) Do
        Begin
          If (s[i] = '''') And (i < Length(s)) Then
            Begin
              Insert('''', s, i);
              Inc(i);
            End ;
          Inc(i);
        End ;
  End ;

Begin
  res := '';
  stack := '';
  i := 1;
  vr := False;
  While i <= Length(s) Do
    Begin
      Case s[i] Of
        '(':
          Begin
            stack := '(' + stack;
            vr := False;
          End ;
        ')':
          Begin
            p := Pos('(', stack);
            res := res + Copy(stack, 1, p - 1);
            stack := Copy(stack, p + 1, Length(stack) - p);
          End ;
        '+', '-', '*', '/', '>', '<', '=':
          Begin
            If (s[i] = '<') And (s[i + 1] = '>') Then
              Begin
                Inc(i);
                s[i] := ttNe;
              End
            Else If (s[i] = '>') And (s[i + 1] = '=') Then
              Begin
                Inc(i);
                s[i] := ttGe;
              End
            Else If (s[i] = '<') And (s[i + 1] = '=') Then
              Begin
                Inc(i);
                s[i] := ttLe;
              End ;

            1: If Not vr Then
              Begin
                If s[i] = '-' Then
                  s[i] := ttUnMinus;
                If s[i] = '+' Then
                  s[i] := ttUnPlus;
              End ;
            vr := False;
            If stack = '' Then
              stack := s[i] + stack
            Else If Priority(s[i]) < Priority(stack[1]) Then
              stack := s[i] + stack
            Else
              Begin
                Repeat
                  res := res + stack[1];
                  stack := Copy(stack, 2, Length(stack) - 1);
                Until (stack = '') Or (Priority(stack[1]) > Priority(s[i]));
                stack := s[i] + stack;
              End ;
          End ;
        ';': break;
        ' ', #13: ;
        Else
          Begin
            vr := True;
            s2 := '';
            i1 := i;
            If s[i] = '%' Then
              Begin
                s2 := '%' + s[i + 1];
                Inc(i, 2);
              End ;
            If s[i] = '''' Then
              s2 := s2 + GetString(s, i)
            Else If s[i] = '[' Then
              Begin
                s2 := s2 + '[' + ExpandVar(s, i, j) + ']';
                i := j + 1;
              End
            Else
              Begin
                s2 := s2 + GetIdentifier(s, i);
                If s[i] = '[' Then
                  Begin
                    s2 := s2 + '[' + ExpandVar(s, i, j) + ']';
                    i := j + 1;
                  End ;
              End ;
            c := s[i];
            If (Length(s2) > 0) And (s2[1] In ['0'..'9', '.', ',']) Then
              res := res + s2 + ' '
            Else
              Begin
                s1 := AnsiUpperCase(s2);
                If s1 = 'INT' Then
                  Begin
                    s[i - 1] := ttInt;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'FRAC' Then
                  Begin
                    s[i - 1] := ttFrac;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'ROUND' Then
                  Begin
                    s[i - 1] := ttRound;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'LN' Then
                  Begin
                    s[i - 1] := ttLn;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'EXP' Then
                  Begin
                    s[i - 1] := ttExp;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'RANDOM' Then
                  Begin
                    s[i - 1] := ttRandom;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'OR' Then
                  Begin
                    s[i - 1] := ttOr;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'AND' Then
                  Begin
                    s[i - 1] := ttAnd;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'NOT' Then
                  Begin
                    s[i - 1] := ttNot;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'STR' Then
                  Begin
                    s[i - 1] := ttStr;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'MOD' Then
                  Begin
                    s[i - 1] := ttMod;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'DIV' Then
                  Begin
                    s[i - 1] := ttDiv;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'SHL' Then
                  Begin
                    s[i - 1] := ttShl;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'SHR' Then
                  Begin
                    s[i - 1] := ttShr;
                    Dec(i);
                    Goto 1;
                  End
                Else If s1 = 'XOR' Then
                  Begin
                    s[i - 1] := ttXor;
                    Dec(i);
                    Goto 1;
                  End
                Else If c = '(' Then
                  Begin
                    Get10Parameters(s, i, s2, s3, s4, p4, p5, p6, p7, p8, p9, p10);
                    res := res + Copy(s, i1, i - i1);
                  End
                Else
                  res := res + s2 + ' ';
              End ;
            Dec(i);
          End ;
      End ;
      Inc(i);
    End ;
  If stack <> '' Then
    res := res + stack;
  Result := res;
End ;

Function TFunParser.Calculate(Const s: String): Variant;
Begin
  Result := CalculateOPZ(String2OPZ(s));
End ;

{ TSplitFunction }

Constructor TSplitFunction.Create(MatchFuncs, SplitTo: TStrings;
  Variables: TIntVariables);
Begin
  Inherited Create;
  FParser := TFunParser.Create;
  FMatchFuncs := MatchFuncs;
  FSplitTo := SplitTo;
  FVariables := Variables;
End ;

Destructor TSplitFunction.Destroy;
Begin
  FParser.Free;
  Inherited Destroy;
End ;

Procedure TSplitFunction.Split(s: String);
Var
  i, k: Integer;
  s1, s2, s3, s4, p4, p5, p6, p7, p8, p9, p10: String;
Begin
  i := 1;
  s := Trim(s);
  If (Length(s) > 0) And (s[1] = '''') Then
    Exit;
  While i <= Length(s) Do
    Begin
      k := i;
      If s[1] = '[' Then
        Begin
          s1 := ExpandVar(s, k, i);
          If FVariables.IndexOf(s1) <> -1 Then
            s1 := FVariables[s1];
          Split(s1);
          k := i + 1;
        End
      Else
        Begin
          s1 := FParser.GetIdentifier(s, k);
          If s[k] = '(' Then
            Begin
              FParser.Get10Parameters(s, k, s2, s3, s4, p4, p5, p6, p7, p8, p9, p10);
              Split(s2);
              Split(s3);
              Split(s4);
              Split(p4);
              Split(p5);
              Split(p6);
              Split(p7);
              Split(p8);
              Split(p9);
              Split(p10);

              If FMatchFuncs.IndexOf(s1) <> -1 Then
                FSplitTo.Add(Copy(s, i, k - i));
            End
          Else If FVariables.IndexOf(s1) <> -1 Then
            Begin
              s1 := FVariables[s1];
              Split(s1);
            End
          Else If s[k] In [' ', #13, '+', '-', '*', '/', '>', '<', '='] Then
            Inc(k)
          Else If s1 = '' Then
            break;
        End ;
      i := k;
    End ;
End ;

{ TFunInterpretator }

Constructor TFunInterpretator.Create;
Begin
  Inherited Create;
  FScript := TStringList.Create;
  FScript1 := TStringList.Create;
  STextError := TStringList.Create;
  Parser := TFunParser.Create;
  Parser.OnGetValue := GetValue;
  Parser.OnFunction := GetSetValueFunction;
  InternalArrayList := TStringList.Create;
  InternalStrList := TStringList.Create;
  InternalVariables := TIntVariables.Create;
  InternalConsts := TIntVariables.Create;
  InternalConsts.Sorted := True;
  FFunction := TStringList.Create;
  InternalParamPosition := 1;
  InternalParamPosition2 := 1;
  ErrorResult := 0;
  ErrorString := '';
  MaxRecurseVar := 10;

  With FFunction Do
    Begin
      Add('FORMATDATETIME');
      Add('FORMATFLOAT');
      Add('LOWERCASE');
      Add('NAMECASE');
      Add('DAYOF'); //4
      Add('FORMATTEXT'); //5
      Add('UPPERCASE');
      Add('NUMBERTOSTRPL'); // delete
      Add('ROUNDFLOAT');
      Add('DAY');
      Add('MONTH');
      Add('YEAR'); // 20
      Add('WEEKNO');
      Add('MAXNUM');
      Add('INTEGERTOSTRPL'); // delete
      Add('MONTHTOSTRPL');
      Add('CAPITALIZE');
      Add('REPLACE');
      Add('DATEDIFF');
      Add('ABS');
      Add('CHR');
      Add('COMPARESTR');
      Add('CREATEDATE');
      Add('CREATENUM');
      Add('CREATESTR');
      Add('DATETOSTR');
      Add('DAYSPERMONTH');
      Add('DELETE');
      Add('ENDPOS');
      Add('EXTRACTWORD');
      Add('FIRSTDAYOFNEXTMONTH'); // 20
      Add('FIRSTDAYOFPREVMONTH');
      Add('INCDATE');
      Add('INCDAY');
      Add('INCMONTH');
      Add('INCTIME');
      Add('INCYEAR');
      Add('INSERT');
      Add('ISLEAPYEAR');
      Add('ISRANGEDATE');
      Add('ISRANGENUM');
      Add('ISWORDPRESENT');
      Add('LASTDAYOFPREVMONTH');
      Add('LEFTCOPY');
      Add('NPOS');
      Add('PADCENTER');
      Add('PADLEFT');
      Add('PADRIGHT');
      Add('REPLACESTR');
      Add('REPLICATE');
      Add('RIGHTCOPY'); // 20
      Add('STRTODATEDEF');
      Add('STRTOFLOAT');
      Add('STRTOFLOATDEF');
      Add('STRTOINT');
      Add('STRTOINTDEF');
      Add('TIMETOSTR');
      Add('TRIMLEFT');
      Add('TRIMRIGHT');
      Add('VALIDDATE');
      Add('VALIDFLOAT');
      Add('VALIDINT');
      Add('WORDCOUNT');
      Add('WORDPOSITION');
      Add('DATETIMETOSTR');
      Add('INTTOSTR');
      Add('DATETOFLOAT');
      Add('FLOATTODATE');
      Add('LENGTH');
      Add('TRIM'); // 20
      Add('IFF');
      Add('POS');
      Add('ISNULL');
      Add('MINNUM');
      Add('MONTHOF');
      Add('YEAROF');
    End ;
End ;

Destructor TFunInterpretator.Destroy;
Begin
  Parser.Free;
  Parser := Nil;
  FScript.free;
  FScript := Nil;
  FScript1.free;
  FScript1 := Nil;
  STextError.free;
  STextError := Nil;
  InternalVariables.free;
  InternalConsts.free;
  InternalVariables := Nil;
  InternalConsts := Nil;
  FFunction.free;
  FFunction := Nil;
  InternalArrayList.free;
  InternalArrayList := Nil;
  Try
    While internalStrList.Count > 0 Do
      Begin
        TStringList(internalStrList.Objects[0]).free;
        internalStrList.Delete(0);
      End ;
  Except
  End ;
  InternalStrList.free;
  InternalStrList := Nil;

  Inherited Destroy;
End ;

Procedure DeleteEmptyLines(Memo: TStrings);
Var
  i: Integer;
  s: String;
Begin
  Memo.BeginUpdate;
  Try
    i := 0;
    While i < Memo.Count Do
      Begin
        s := Memo[i];
        If s = '' Then
          Memo.Delete(i)
        Else
          inc(i);
      End ;
  Finally
    Memo.EndUpdate;
  End ;
End ;

Procedure TFunInterpretator.PrepareScript(MemoFrom, MemoTo, MemoErr: TStrings);
Var
  i, j, cur, lastp: Integer;
  s, bs: String;
  len: Integer;
  buf: PCharArray;
  Error: Boolean;
  CutList: TStringList;

  Procedure ExecuteCommand; Forward;
  Procedure ExecuteBegin; Forward;
  Procedure ExecuteIf; Forward;
  Procedure ExecuteRepeat; Forward;
  Procedure ExecuteWhile; Forward;
  Procedure ExecuteGoto; Forward;
  Procedure ExecuteEqual; Forward;
  Procedure ExecuteExpression; Forward;
  Procedure Execute_Expression; Forward;
  Procedure ExecuteTerm; Forward;
  Procedure ExecuteFactor; Forward;
  Procedure ExecuteVars; Forward;
  Procedure ExecuteConstants; Forward;
  Procedure ExecuteLabel; Forward;
  Procedure ExecuteFunction; Forward;
  Procedure ExecuteFunctionId; Forward;

  Function last: Integer;
  Begin
    Result := MemoTo.Count;
  End ;

  Function CopyArr(cur, n: Integer): String;
  Begin
    SetLength(Result, n);
    Move(buf^[cur], Result[1], n);
  End ;

  Procedure AddLabel(s: String; n: Integer);
  Var
    i: Integer;
    f: Boolean;
  Begin
    f := True;
    For i := 0 To labc - 1 Do
      If labels[i].Name = s Then
        f := False;
    If f Then
      Begin
        labels[labc].Name := s;
        labels[labc].n := n;
        Inc(labc);
      End ;
  End ;

  Procedure SkipSpace;
  Begin
    While (buf^[cur] <= ' ') And (cur < len) Do
      Inc(cur);
  End ;

  Function GetToken: String;
  Var
    n, j: Integer;
  Label
    1;
  Begin
    1: SkipSpace;
    j := cur;
    While (buf^[cur] > ' ') And (cur < len) Do
      Begin
        If (buf^[cur] = '{') And (buf^[j] <> #$27) Then
          Begin
            n := cur;
            While (buf^[cur] <> '}') And (cur < len) Do
              Inc(cur);
            CutList.Add(IntToStr(n) + ' ' + IntToStr(cur - n + 1));
            Move(buf^[cur + 1], buf^[n], len - cur);
            Dec(len, cur - n + 1);
            cur := n;
            Goto 1;
          End
        Else If (buf^[cur] = '/') And (buf^[cur + 1] = '/') And (buf^[j] <> #$27) Then
          Begin
            n := cur;
            While (buf^[cur] <> #13) And (cur < len) Do
              Inc(cur);
            CutList.Add(IntToStr(n) + ' ' + IntToStr(cur - n + 1));
            Move(buf^[cur + 1], buf^[n], len - cur);
            Dec(len, cur - n + 1);
            cur := n;
            Goto 1;
          End ;
        Inc(cur);
      End ;
    Result := AnsiUpperCase(CopyArr(j, cur - j));
    If Result = '' Then
      Result := ' ';
  End ;

  Procedure AddError(s: String);
  Var
    i, j, c: Integer;
    s1: String;
  Begin
    Error := True;
    cur := lastp;
    SkipSpace;
    For i := 0 To CutList.Count - 1 Do
      Begin
        s1 := CutList[i];
        j := StrToInt(Copy(s1, 1, Pos(' ', s1) - 1));
        c := StrToInt(Copy(s1, Pos(' ', s1) + 1, 255));
        If lastp >= j Then
          Inc(cur, c);
      End ;

    Inc(cur);
    i := 0;
    c := 0;
    j := 0;
    While i < MemoFrom.Count Do
      Begin
        s1 := MemoFrom[i];
        If c + Length(s1) + 1 < cur Then
          c := c + Length(s1) + 1
        Else
          Begin
            j := cur - c;
            break;
          End ;
        Inc(i);
      End ;
    MemoErr.Add('Line ' + IntToStr(i + 1) + '/' + IntToStr(j) + ': ' + s);
    cur := lastp;
  End ;

  Procedure ProcessBrackets(Var i: Integer);
  Var
    c: Integer;
    fl1, fl2: Boolean;
  Begin
    fl1 := True;
    fl2 := True;
    c := 0;
    Dec(i);
    Repeat
      Inc(i);
      If fl1 And fl2 Then
        If buf^[i] = '[' Then
          Inc(c)
        Else If buf^[i] = ']' Then
          Dec(c);
      If fl1 Then
        If buf^[i] = '"' Then
          fl2 := Not fl2;
      If fl2 Then
        If buf^[i] = '''' Then
          fl1 := Not fl1;
    Until (c = 0) Or (i >= len);
  End ;

  {----------------------------------------------}

  Procedure ExecuteDigitNumber;
  Begin
    While (buf^[cur] <= ' ') And (cur < len) Do
      Inc(cur);
    If buf^[cur] In ['0'..'9'] Then
      While (buf^[cur] In ['0'..'9']) And (cur < len) Do
        Inc(cur)
    Else
      Error := True;
  End ;

  Procedure ExecuteBegin;
  Label
    1;
  Begin
    1: ExecuteCommand;
    If Error Then
      Exit;
    lastp := cur;
    bs := GetToken;
    If (bs = '') Or (bs[1] = ';') Then
      Begin
        cur := cur - Length(bs) + 1;
        Goto 1;
      End
    Else If (bs = 'END') Or (bs = 'End ;') Then
      cur := cur - Length(bs) + 3
    Else
      AddError('Need ";" or "end" here');
  End ;

  Procedure ExecuteIf;
  Var
    nsm, nl, nl1: Integer;
  Begin
    nsm := cur;
    ExecuteExpression;
    If Error Then
      Exit;
    bs := ttIf + '  ' + CopyArr(nsm, cur - nsm);
    nl := last;
    MemoTo.Add(bs);
    lastp := cur;
    If GetToken = 'THEN' Then
      Begin
        ExecuteCommand;
        If Error Then
          Exit;
        nsm := cur;
        If GetToken = 'ELSE' Then
          Begin
            nl1 := last;
            MemoTo.Add(ttGoto + '  ');
            bs := MemoTo[nl];
            bs[2] := Chr(last);
            bs[3] := Chr(last Div 256);
            MemoTo[nl] := bs;
            ExecuteCommand;
            bs := MemoTo[nl1];
            bs[2] := Chr(last);
            bs[3] := Chr(last Div 256);
            MemoTo[nl1] := bs;
          End
        Else
          Begin
            bs := MemoTo[nl];
            bs[2] := Chr(last);
            bs[3] := Chr(last Div 256);
            MemoTo[nl] := bs;
            cur := nsm;
          End ;
      End
    Else
      AddError('Need "then" here');
  End ;

  Procedure ExecuteRepeat;
  Label
    1;
  Var
    nl, nsm: Integer;
  Begin
    nl := last;
    1: ExecuteCommand;
    If Error Then
      Exit;
    lastp := cur;
    bs := GetToken;
    If bs = 'UNTIL' Then
      Begin
        nsm := cur;
        ExecuteExpression;
        MemoTo.Add(ttIf + Chr(nl) + Chr(nl Div 256) + CopyArr(nsm, cur - nsm));
      End
    Else If bs[1] = ';' Then
      Begin
        cur := cur - Length(bs) + 1;
        Goto 1;
      End
    Else
      AddError('Need ";" or "until" here');
  End ;

  Procedure ExecuteWhile;
  Var
    nl, nsm: Integer;
  Begin
    nl := last;
    nsm := cur;
    ExecuteExpression;
    If Error Then
      Exit;
    MemoTo.Add(ttIf + '  ' + CopyArr(nsm, cur - nsm));
    lastp := cur;
    If GetToken = 'DO' Then
      Begin
        ExecuteCommand;
        MemoTo.Add(ttGoto + Chr(nl) + Chr(nl Div 256));
        bs := MemoTo[nl];
        bs[2] := Chr(last);
        bs[3] := Chr(last Div 256);
        MemoTo[nl] := bs;
      End
    Else
      AddError('Need "do" here');
  End ;

  Procedure DoFor;
  Var
    nsm, nl: Integer;
    loopvar: String;
  Begin
    nsm := cur;
    ExecuteEqual;
    If Error Then
      Exit;
    bs := Trim(CopyArr(nsm, cur - nsm));
    loopvar := Copy(bs, 1, Pos(':=', bs) - 1);
    lastp := cur;
    If GetToken = 'TO' Then
      Begin
        nsm := cur;
        ExecuteExpression;
        If Error Then
          Exit;
        nl := last;
        MemoTo.Add(ttIf + '  ' + loopvar + '<=' + CopyArr(nsm, cur - nsm));

        lastp := cur;
        If GetToken = 'DO' Then
          Begin
            ExecuteCommand;
            If Error Then
              Exit;
            MemoTo.Add(loopvar + ' ' + loopvar + '+1');
            MemoTo.Add(ttGoto + Chr(nl) + Chr(nl Div 256));
            bs := MemoTo[nl];
            bs[2] := Chr(last);
            bs[3] := Chr(last Div 256);
            MemoTo[nl] := bs;
          End
        Else
          AddError('Need "do" here');
      End
    Else
      AddError('Need "to" here');
  End ;

  Procedure ExecuteGoto;
  Var
    nsm: Integer;
  Begin
    SkipSpace;
    nsm := cur;
    lastp := cur;
    ExecuteDigitNumber;
    If Error Then
      AddError('"goto" label must be a number');
    MemoTo.Add(ttGoto + Trim(CopyArr(nsm, cur - nsm)));
  End ;

  Procedure ExecuteEqual;
  Var
    s: String;
    n, nsm: Integer;
  Begin
    nsm := cur;
    ExecuteVars;
    s := Trim(CopyArr(nsm, cur - nsm)) + ' ';
    lastp := cur;
    bs := GetToken;
    If (bs = ';') Or (bs = '') Or (bs = #0) Or (bs = 'END') Or (bs = 'ELSE') Then
      Begin
        s := Trim(CopyArr(nsm, lastp - nsm));
        MemoTo.Add(ttProc + s + '(0)');
        cur := lastp;
      End
    Else If Pos(':=', bs) = 1 Then
      Begin
        cur := cur - Length(bs) + 2;
        nsm := cur;
        ExecuteExpression;
        n := Pos('[', s);
        If n <> 0 Then
          Begin
            s := ttProc + 'SETARRAY(' + Copy(s, 1, n - 1) + ', ' +
              Copy(s, n + 1, Length(s) - n - 2) + ', ' + CopyArr(nsm, cur - nsm) + ')';
          End
        Else
          s := s + CopyArr(nsm, cur - nsm);
        MemoTo.Add(s);
      End
    Else
      AddError('Need ":=" here');
  End ;
  {-------------------------------------}

  Procedure ExecuteExpression;
  Var
    nsm: Integer;
  Begin
    Execute_Expression;
    nsm := cur;
    bs := GetToken;
    If (Pos('>=', bs) = 1) Or (Pos('<=', bs) = 1) Or (Pos('<>', bs) = 1) Then
      Begin
        cur := cur - Length(bs) + 2;
        Execute_Expression;
      End
    Else If (bs[1] = '>') Or (bs[1] = '<') Or (bs[1] = '=') Then
      Begin
        cur := cur - Length(bs) + 1;
        Execute_Expression;
      End
    Else
      cur := nsm;
  End ;

  Procedure Execute_Expression;
  Var
    nsm: Integer;
  Begin
    ExecuteTerm;
    nsm := cur;
    bs := GetToken;
    If (bs[1] = '+') Or (bs[1] = '-') Then
      Begin
        cur := cur - Length(bs) + 1;
        Execute_Expression;
      End
    Else If Pos('OR', bs) = 1 Then
      Begin
        cur := cur - Length(bs) + 2;
        Execute_Expression;
      End
    Else
      cur := nsm;
  End ;

  Procedure ExecuteTerm;
  Var
    nsm: Integer;
  Begin
    ExecuteFactor;
    nsm := cur;
    bs := GetToken;
    If (bs[1] = '*') Or (bs[1] = '/') Then
      Begin
        cur := cur - Length(bs) + 1;
        ExecuteTerm;
      End
    Else If (Pos('AND', bs) = 1) Or (Pos('MOD', bs) = 1) Then
      Begin
        cur := cur - Length(bs) + 3;
        ExecuteTerm;
      End
    Else
      cur := nsm;
  End ;

  Procedure ExecuteFactor;
  Var
    nsm: Integer;
  Begin
    nsm := cur;
    bs := GetToken;
    If bs[1] = '(' Then
      Begin
        cur := cur - Length(bs) + 1;
        ExecuteExpression;
        SkipSpace;
        lastp := cur;
        If buf^[cur] = ')' Then
          Inc(cur)
        Else
          AddError('Need ")" here');
      End
    Else If bs[1] = '[' Then
      Begin
        cur := cur - Length(bs);
        ProcessBrackets(cur);
        SkipSpace;
        lastp := cur;
        If buf^[cur] = ']' Then
          Inc(cur)
        Else
          AddError('Need "]" here');
      End
    Else If (bs[1] = '+') Or (bs[1] = '-') Then
      Begin
        cur := cur - Length(bs) + 1;
        ExecuteExpression;
      End
    Else If bs = 'NOT' Then
      Begin
        cur := cur - Length(bs) + 3;
        ExecuteExpression;
      End
    Else
      Begin
        cur := nsm;
        ExecuteVars;
        If Error Then
          Begin
            Error := False;
            cur := nsm;
            ExecuteConstants;
            If Error Then
              Begin
                Error := False;
                cur := nsm;
                ExecuteFunction;
              End ;
          End ;
      End ;
  End ;

  Procedure ExecuteVars;
  Begin
    SkipSpace;
    If (buf^[cur] In ['@', '#', '$', 'a'..'z', 'A'..'Z']) Then
      Begin
        Inc(cur);
        While buf^[cur] In ['0'..'9', '_', '.', '@', '#', '$', 'A'..'Z', 'a'..'z'] Do
          Inc(cur);
        If buf^[cur] = '(' Then
          Error := True
        Else If buf^[cur] = '[' Then
          Begin
            Inc(cur);
            ExecuteExpression;
            If buf^[cur] <> ']' Then
              Error := True
            Else
              Inc(cur);
          End ;
      End
    Else
      Error := True;
  End ;

  Procedure ExecuteConstants;
  Label
    1;
  Begin
    SkipSpace;
    If buf^[cur] = #$27 Then
      Begin
        1: Inc(cur);
        While (buf^[cur] <> #$27) And (cur < len) Do
          Inc(cur);
        If (cur < len) And (buf^[cur + 1] = #$27) Then
          Begin
            Inc(cur);
            Goto 1;
          End ;
        If cur = len Then
          Error := True
        Else
          Inc(cur);
      End
    Else
      Begin
        ExecuteDigitNumber;
        If buf^[cur] = '.' Then
          Begin
            Inc(cur);
            ExecuteDigitNumber;
          End ;
      End ;
  End ;

  Procedure ExecuteLabel;
  Begin
    ExecuteDigitNumber;
    If buf^[cur] = ':' Then
      Inc(cur)
    Else
      Error := True;
  End ;

  Procedure ExecuteFunction;
  Label
    1;
  Begin
    ExecuteFunctionId;
    If buf^[cur] = '(' Then
      Begin
        Inc(cur);
        SkipSpace;
        If buf^[cur] = ')' Then
          Begin
            Inc(cur);
            Exit;
          End ;
        1: ExecuteExpression;
        lastp := cur;
        SkipSpace;
        If buf^[cur] = ',' Then
          Begin
            Inc(cur);
            Goto 1;
          End
        Else If buf^[cur] = ')' Then
          Inc(cur)
        Else
          AddError('Need "," or ")" here');
      End ;
  End ;

  Procedure ExecuteFunctionId;
  Begin
    SkipSpace;
    If buf^[cur] In ['@', '#', '$', 'A'..'Z', 'a'..'z'] Then
      While buf^[cur] In ['0'..'9', '_', '.', '@', '#', '$', 'A'..'Z', 'a'..'z'] Do
        Inc(cur)
    Else
      Error := True;
  End ;

  Procedure ExecuteCommand;
  Label
    1;
  Var
    nsm: Integer;
  Begin
    1: Error := False;
    nsm := cur;
    lastp := cur;
    bs := GetToken;
    If bs = 'BEGIN' Then
      ExecuteBegin
    Else If bs = 'IF' Then
      ExecuteIf
    Else If bs = 'REPEAT' Then
      ExecuteRepeat
    Else If bs = 'WHILE' Then
      ExecuteWhile
    Else If bs = 'FOR' Then
      DoFor
    Else If bs = 'GOTO' Then
      ExecuteGoto
    Else If (bs = 'END') Or (bs = 'End ;') Then
      Begin
        cur := nsm;
        Error := False;
      End
    Else If bs = 'UNTIL' Then
      Begin
        cur := nsm;
        Error := False;
      End
    Else
      Begin
        cur := nsm;
        ExecuteLabel;
        If Error Then
          Begin
            Error := False;
            cur := nsm;
            ExecuteVars;
            If Not Error Then
              Begin
                cur := nsm;
                ExecuteEqual;
              End
            Else
              Begin
                cur := nsm;
                Error := False;
                ExecuteExpression;
                MemoTo.Add(ttProc + Trim(CopyArr(nsm, cur - nsm)));
              End ;
          End
        Else
          Begin
            AddLabel(Trim(CopyArr(nsm, cur - nsm)), last);
            Goto 1;
          End ;
      End ;
  End ;

Begin
  Try
    CutList := TStringList.Create;
    Try
      Error := False;
      GetMem(buf, 64000);
      FillChar(buf^, 64000, 0);
      len := 0;
      For i := 0 To MemoFrom.Count - 1 Do
        Begin
          s := MemoFrom[i] + #13;
          While Pos(#9, s) <> 0 Do
            s[Pos(#9, s)] := ' ';
          Move(s[1], buf^[len], Length(s));
          Inc(len, Length(s));
        End ;

      cur := 0;
      labc := 0;
      MemoTo.Clear;
      MemoErr.Clear;
      If len > 0 Then
        ExecuteCommand;
    Finally
      FreeMem(buf, 64000);
      If CutList <> Nil Then
        CutList.Free;
      CutList := Nil;
    End ;

    For i := 0 To MemoTo.Count - 1 Do
      If MemoTo[i][1] = ttGoto Then
        Begin
          s := Remain(MemoTo[i], 2) + ':';
          For j := 0 To labc Do
            If labels[j].Name = s Then
              Begin
                s := MemoTo[i];
                s[2] := Chr(labels[j].n);
                s[3] := Chr(labels[j].n Div 256);
                MemoTo[i] := s;
                break;
              End ;
        End
      Else If MemoTo[i][1] = ttIf Then
        Begin
          s := Parser.String2OPZ(Remain(MemoTo[i], 4));
          MemoTo[i] := Copy(MemoTo[i], 1, 3) + s;
        End
      Else If MemoTo[i][1] = ttProc Then
        Begin
          s := Parser.String2OPZ(Remain(MemoTo[i], 2));
          MemoTo[i] := Copy(MemoTo[i], 1, 1) + s;
        End
      Else
        Begin
          j := 1;
          GetIdentifier(MemoTo[i], j);
          len := j;
          s := Parser.String2OPZ(Remain(MemoTo[i], j));
          MemoTo[i] := Copy(MemoTo[i], 1, len) + s;
        End ;
  Except
    If ErrorResult = 0 Then
      ErrorResult := 50002;
  End ;
End ;

Procedure TFunInterpretator.ExecuteScript(Memo: TStrings);
Var
  i, j: Integer;
  s, s1: String;
Begin
  If Self.ErrorResult <> 0 Then Exit;
  Try
    i := 0;
    While i < Memo.Count Do
      Begin
        s := Memo[i];
        j := 1;
        If s[1] = ttIf Then
          Begin
            If Parser.CalculateOPZ(Remain(s, 4)) = 0 Then
              Begin
                i := Ord(s[2]) + Ord(s[3]) * 256;
                continue;
              End ;
          End
        Else If s[1] = ttGoto Then
          Begin
            i := Ord(s[2]) + Ord(s[3]) * 256;
            continue;
          End
        Else If s[1] = ttProc Then
          Begin
            s1 := Remain(s, 2);
            If AnsiUpperCase(s1) = 'EXIT(0)' Then
              Exit;
            Parser.CalculateOPZ(s1);
          End
        Else
          Begin
            s1 := GetIdentifier(s, j);
            SetValue(s1, Parser.CalculateOPZ(Remain(s, j)));
          End ;
        Inc(i);
      End ;
  Except
    If ErrorResult = 0 Then
      ErrorResult := 50002;
  End ;
End ;

Function TFunInterpretator.ExpandExpression(Const Text: AnsiString): AnsiString;
Var
  v1: AnsiString;

  Procedure GetData(Var s: String; MaxVarRecurse: Integer = 10);
  Var
    i, j, k, l, m: Longint;
    s1, s2: String;
  Begin
    i := 1;
    k := 0;
    Repeat
      While (i < Length(s)) And (s[i] <> '[') Do
        Inc(i);
      s1 := ExpandVar(s, i, j);
      If i <> j Then
        Begin
          Delete(s, i, j - i + 1);
          s2 := '';
          s2 := Parser.Calculate(s1);

          Insert(s2, s, i);
          l := pos('[', s);
          m := pos(']', s);
          If k < MaxVarRecurse Then
            Begin
              If (l > 0) And (m > 0) Then
                Begin
                  If l > m Then
                    Inc(i, Length(s2))
                  Else
                    Inc(k);
                End
              Else
                Inc(i, Length(s2));
            End
          Else
            Inc(i, Length(s2));
          j := 0;
        End ;
    Until i = j;
  End ;

  Procedure RemoveQuotes(Var s: String);
  Begin
    If (s[1] = '"') And (s[Length(s)] = '"') Then
      s := Copy(s, 2, Length(s) - 2);
    If (s[1] = '''') And (s[Length(s)] = '''') Then
      s := Copy(s, 2, Length(s) - 2);
  End ;

Begin
  Result := Text;
  v1 := Text;
  Try
    GetData(v1, MaxRecurseVar);
    Result := v1;
  Except
    If ErrorResult = 0 Then
      ErrorResult := 50002;
  End ;
End ;

Procedure TFunInterpretator.SplitExpressions(Memo, MatchFuncs, SplitTo: TStrings;
  Variables: TIntVariables);
Var
  i, j: Integer;
  s: String;
  FuncSplitter: TSplitFunction;
Begin
  FuncSplitter := TSplitFunction.Create(MatchFuncs, SplitTo, Variables);
  Try
    i := 0;
    While i < Memo.Count Do
      Begin
        s := Memo[i];
        j := 1;
        If s[1] = ttIf Then
          FuncSplitter.Split(Remain(s, 4))
        Else If s[1] = ttProc Then
          FuncSplitter.Split(Remain(s, 2))
        Else
          Begin
            GetIdentifier(s, j);
            FuncSplitter.Split(Remain(s, j));
          End ;
        Inc(i);
      End ;
  Finally
    FuncSplitter.Free;
  End ;
End ;

Type
  TVrCharSet = Set Of Char;

Function fsWordPosition(Const N: Longint; Const S: String; Const WordDelims: TvrCharSet): Longint;
Var
  Count, I: Longint;
Begin
  Count := 0;
  I := 1;
  Result := 0;
  While (I <= Length(S)) And (Count <> N) Do
    Begin
      While (I <= Length(S)) And (S[I] In WordDelims) Do
        Inc(I);
      If I <= Length(S) Then
        Inc(Count);
      If Count <> N Then
        While (I <= Length(S)) And Not (S[I] In WordDelims) Do
          Inc(I)
      Else
        Result := I;
    End ;
End ;

Function fsExtractWord(N: Longint; Const S: String; Const WordDelims: TvrCharSet): String;
Var
  I: Longint;
  Len: Longint;
Begin
  Len := 0;
  I := fsWordPosition(N, S, WordDelims);
  If I <> 0 Then
    While (I <= Length(S)) And Not (S[I] In WordDelims) Do
      Begin
        Inc(Len);
        SetLength(Result, Len);
        Result[Len] := S[I];
        Inc(I);
      End ;
  SetLength(Result, Len);
End ;

Function fsWordCount(Const S: String; Const WordDelims: TvrCharSet): Longint;
Var
  SLen, I: Cardinal;
Begin
  Result := 0;
  I := 1;
  SLen := Length(S);
  While I <= SLen Do
    Begin
      While (I <= SLen) And (S[I] In WordDelims) Do
        Inc(I);
      If I <= SLen Then
        Inc(Result);
      While (I <= SLen) And Not (S[I] In WordDelims) Do
        Inc(I);
    End ;
End ;

Function fsIsWordPresent(Const W, S: String; Const WordDelims: TvrCharSet): Boolean;
Var
  Count, I: Longint;
Begin
  Result := False;
  Count := fsWordCount(S, WordDelims);
  For I := 1 To Count Do
    If fsExtractWord(I, S, WordDelims) = W Then
      Begin
        Result := True;
        Exit;
      End ;
End ;

Function fsNPos(Const C: String; S: String; N: Longint): Longint;
Var
  I, P, K: Longint;
Begin
  Result := 0;
  K := 0;
  For I := 1 To N Do
    Begin
      P := Pos(C, S);
      Inc(K, P);
      If (I = N) And (P > 0) Then
        Begin
          Result := K;
          Exit;
        End ;
      If P > 0 Then
        Delete(S, 1, P)
      Else
        Exit;
    End ;
End ;

Function fsReplaceStr(Const S, Srch, Replace: String): String;
Var
  I: Longint;
  Source: String;
Begin
  Source := S;
  Result := '';
  Repeat
    I := Pos(Srch, Source);
    If I > 0 Then
      Begin
        Result := Result + Copy(Source, 1, I - 1) + Replace;
        Source := Copy(Source, I + Length(Srch), MaxInt);
      End
    Else
      Result := Result + Source;
  Until I <= 0;
End ;

Function fsReplicate(cStr: String; nLen: Longint): String;
Var
  nCou: Longint;
Begin
  Result := '';
  For nCou := 1 To nLen Do
    Result := Result + cStr;
End ;

Function fsPadLeft(cStr: String; nLen: Longint; cChar: String): String;
Var
  S: String;
Begin
  S := Trim(cStr);
  Result := fsReplicate(cChar, nLen - Length(S)) + S;
End ;

Function fsPadRight(cStr: String; nLen: Longint; cChar: String): String;
Var
  S: String;
Begin
  S := Trim(cStr);
  Result := S + fsReplicate(cChar, nLen - Length(S));
End ;

Function fsPadCenter(cStr: String; nWidth: Longint; cChar: String): String;
Var
  nPerSide: Longint;
  cResult: String;

Begin
  nPerSide := (nWidth - Length(cStr)) Div 2;
  cResult := fsPadLeft(cStr, (Length(cStr) + nPerSide), cChar);
  Result := fsPadRight(cResult, nWidth, cChar);
End ;

Function fsEndPos(cStr, cSubStr: String): Longint;
Var
  nCou: Longint;
  nLenSS: Longint;
  nLenS: Longint;

Begin
  nLenSS := Length(cSubStr);
  nLenS := Length(cStr);
  Result := 0;

  If nLenSS > nLenS Then
    Exit;

  For nCou := nLenS Downto 1 Do
    If Copy(cStr, nCou, nLenSS) = cSubStr Then
      Begin
        Result := nCou;
        Exit;
      End ;
End ;

Function fsLeftCopy(cStr: String; nNum: Longint): String;
Begin
  Result := Copy(cStr, 1, nNum);
End ;

Function fsRightCopy(cStr: String; nNum: Longint): String;
Begin
  Result := '';
  If nNum > Length(cStr) Then
    Exit;
  Result := Copy(cStr, (Length(cStr) - nNum + 1), Length(cStr));
End ;

Function fsDelete(cStr: String; nIndex, nCount: Longint): String;
Begin
  Delete(cStr, nIndex, nCount);
  Result := cStr;
End ;

Function fsInsert(cStr1, cStr2: String; nIndex: Longint): String;
Begin
  Insert(cStr1, cStr2, nIndex);
  Result := cStr2;
End ;

Function fsCompareStr(cStr1, cStr2: String): Longint;
Var
  nLenMax: Longint;
  nCou: Longint;

Begin
  Result := 0;
  If Length(cStr1) > Length(cStr2) Then
    nLenMax := Length(cStr1)
  Else
    nLenMax := Length(cStr2);
  For nCou := 1 To nLenMax Do
    If Copy(cStr1, nCou, 1) <> Copy(cStr2, nCou, 1) Then
      Begin
        Result := nCou;
        Exit;
      End ;
End ;

Function fsCreateStr(cStr: String): String;
Begin
  If Trim(cStr) = '' Then
    Result := 'null'
  Else
    Result := CHR(39) + cStr + CHR(39);
End ;

Function fsCreateNum(cNum: String): String;
Begin
  If Trim(cNum) = '' Then
    Result := 'null'
  Else
    Result := fsReplaceStr(cNum, DecimalSeparator, '.');
End ;

Function fsValidDate(cDate: String): Boolean;
Begin
  Result := True;
  Try
    StrToDate(cDate)
  Except
    Result := False;
  End ;
End ;

Function fsCreateDate(cDate: String; cFFormatDate: String): String;
Begin
  If Not fsValidDate(cDate) Then
    Result := 'null'
  Else
    Result := CHR(39) + FormatDateTime(cFFormatDate, StrToDateTime(cDate)) + CHR(39);
End ;

Function fsIsLeapYear(AYear: Longint): Boolean;
Begin
  Result := (AYear Mod 4 = 0) And ((AYear Mod 100 <> 0) Or (AYear Mod 400 = 0));
End ;

Function fsDaysPerMonth(nYear, nMonth: Longint): Longint;
Const
  DaysInMonth: Array[1..12] Of Longint =
  (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
Begin
  Result := DaysInMonth[nMonth];
  If (nMonth = 2) And fsIsLeapYear(nYear) Then
    Inc(Result); // leap-year Feb is special
End ;

Function fsFirstDayOfNextMonth(dDate: TDateTime): TDateTime;
Var
  Year, Month, Day: Word;
Begin
  DecodeDate(dDate, Year, Month, Day);
  Day := 1;
  If Month < 12 Then
    Inc(Month)
  Else
    Begin
      Inc(Year);
      Month := 1;
    End ;
  Result := EncodeDate(Year, Month, Day);
End ;

Function fsFirstDayOfPrevMonth(dDate: TDateTime): TDateTime;
Var
  Year, Month, Day: Word;

Begin

  DecodeDate(dDate, Year, Month, Day);
  Day := 1;
  If Month > 1 Then
    Dec(Month)
  Else
    Begin
      Dec(Year);
      Month := 12;
    End ;
  Result := EncodeDate(Year, Month, Day);

End ;

Function fsLastDayOfPrevMonth(dDate: TDateTime): TDateTime;
Var
  D: TDateTime;
  Year, Month, Day: Word;

Begin

  D := fsFirstDayOfPrevMonth(dDate);
  DecodeDate(D, Year, Month, Day);
  Day := fsDaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day);

End ;

Function fsIncDate(dDate: TDateTime; nDays, nMonths, nYears: Longint): TDateTime;
Var
  D, M, Y: Word;
  Day, Month, Year: Longint;

Begin

  DecodeDate(dDate, Y, M, D);
  Year := Y;
  Month := M;
  Day := D;
  Inc(Year, nYears);
  Inc(Year, nMonths Div 12);
  Inc(Month, nMonths Mod 12);

  If Month < 1 Then
    Begin
      Inc(Month, 12);
      Dec(Year);
    End
  Else If Month > 12 Then
    Begin
      Dec(Month, 12);
      Inc(Year);
    End ;

  If Day > fsDaysPerMonth(Year, Month) Then
    Day := fsDaysPerMonth(Year, Month);
  Result := EncodeDate(Year, Month, Day) + nDays + Frac(dDate);

End ;

Function fsIncDay(dDate: TDateTime; nDelta: Longint): TDateTime;
Begin
  Result := dDate + nDelta;
End ;

Function fsIncYear(dDate: TDateTime; nDelta: Longint): TDateTime;
Begin
  Result := fsIncDate(dDate, 0, 0, nDelta);
End ;

Function fsIncMonth(dDate: TDateTime; nDelta: Longint): TDateTime;
Begin
  Result := fsIncDate(dDate, 0, nDelta, 0);
End ;

Procedure fsDateDiffEx(dDate1, dDate2: TDateTime; Var cDelta: String);
Var
  DtSwap: TDateTime;
  Day1, Day2, Month1, Month2, Year1, Year2: Word;
  Days, Months, Years: Word;

Begin

  If dDate1 > dDate2 Then
    Begin
      DtSwap := dDate1;
      dDate1 := dDate2;
      dDate2 := DtSwap;
    End ;
  DecodeDate(dDate1, Year1, Month1, Day1);
  DecodeDate(dDate2, Year2, Month2, Day2);
  Years := Year2 - Year1;
  Months := 0;
  Days := 0;
  If Month2 < Month1 Then
    Begin
      Inc(Months, 12);
      Dec(Years);
    End ;
  Inc(Months, Month2 - Month1);
  If Day2 < Day1 Then
    Begin
      Inc(Days, fsDaysPerMonth(Year1, Month1));
      If Months = 0 Then
        Begin
          Dec(Years);
          Months := 11;
        End
      Else
        Dec(Months);
    End ;
  Inc(Days, Day2 - Day1);

  cDelta := IntToStr(Days) + ';' + IntToStr(Months) + ';' + IntToStr(Years);

End ;

Function fsIsRangeDate(dBegDate, dEndDate, dDate: TDateTime): Boolean;
Begin

  If (dDate >= dBegDate) And (dDate <= dEndDate) Then
    Result := True
  Else
    Result := False

End ;

Function fsStrToDateDef(cDate: String; dDefault: TDateTime): TDateTime;
Begin

  Try
    Result := StrToDate(cDate)
  Except
    Result := dDefault;
  End ;

End ;

Function fsIncDateEx(dDate: TDateTime; cDelta: String): TDateTime;
Var
  nDay, nMonth, nYear: Longint;

Begin
  nDay := StrToInt(fsExtractWord(1, cDelta, [';']));
  nMonth := StrToInt(fsExtractWord(2, cDelta, [';']));
  nYear := StrToInt(fsExtractWord(3, cDelta, [';']));

  Result := fsIncDate(dDate, nDay, nMonth, nYear);

End ;

Function fsIncTimeEx(dTime: TDateTime; cDelta: String): TDateTime;
Var
  nHours, nMinutes, nSeconds, nMSecs: Longint;

Begin
  nHours := StrToInt(fsExtractWord(1, cDelta, [';']));
  nMinutes := StrToInt(fsExtractWord(2, cDelta, [';']));
  nSeconds := StrToInt(fsExtractWord(3, cDelta, [';']));
  nMSecs := StrToInt(fsExtractWord(4, cDelta, [';']));

  Result := dTime + (nHours Div 24) + (((nHours Mod 24) * 3600000 +
    nMinutes * 60000 + nSeconds * 1000 + nMSecs) / MSecsPerDay);

  If Result < 0 Then
    Result := Result + 1;
End ;

Function VrDelete(cStr: String; nIndex, nCount: Longint): String;
Begin
  Delete(cStr, nIndex, nCount);
  Result := cStr;
End ;

Function VrInsert(cStr1, cStr2: String; nIndex: Longint): String;
Begin
  Insert(cStr1, cStr2, nIndex);
  Result := cStr2;
End ;

Function fsIsRangeNum(nBeg, nEnd, nValue: Extended): Boolean;
Begin

  If (nValue >= nBeg) And (nValue <= nEnd) Then
    Result := True
  Else
    Result := False

End ;

Function fsValidInt(cInt: String): Boolean;
Begin

  Result := True;
  Try
    StrToInt(cInt);
  Except
    Result := False;
  End ;

End ;

Function fsValidFloat(cFlt: String): Boolean;
Begin

  Result := True;
  Try
    StrToFloat(cFlt);
  Except
    Result := False;
  End ;

End ;

Function fsStrToFloatDef(cFlt: String; nFltDef: Extended): Extended;
Begin
  Try
    Result := StrToFloat(cFlt);
  Except
    Result := nFltDef;
  End ;
End ;

Procedure TFunInterpretator.GetSetValueFunction(Const Name: String; p1, p2, p3, p4, p5, p6, p7, p8, p9, p10: Variant;
  Var Value: Variant);
Var
  SName, SVar1, SVar2, SVar3: String;
  i: Integer;
  s1: String;
  v: Extended;
  fno: Integer;
  s: String;
  v1: variant;
  vv: Variant;
  dt: TDatetime;
  Dd: Extended;
  cStr: String;

  Function ReplaceS(Const S, Srch, Replace: String): String;
  Var
    I: Longint;
    Source: String;
    SourceTemp, SrchTemp: String;
  Begin
    Source := S;
    SourceTemp := AnsiUpperCase(S);
    SrchTemp := AnsiUpperCase(Srch);
    Result := '';
    Repeat
      I := Pos(SrchTemp, SourceTemp);
      If I > 0 Then
        Begin
          Result := Result + Copy(Source, 1, I - 1) + Replace;
          Source := Copy(Source, I + Length(Srch), MaxInt);
          SourceTemp := Copy(SourceTemp, I + Length(SrchTemp), MaxInt);
        End
      Else
        Result := Result + Source;
    Until I <= 0;
  End ;

  Function ConvCS(cStr: String): TvrCharSet;
  Var
    i: Longint;
  Begin
    Result := [];
    For i := 1 To Length(cStr) Do
      Include(Result, cStr[i]);
  End ;

  Function CapitalizeWord(Const S: AnsiString): AnsiString;
  Var
    I: Longint;
    CapitalizeNextLetter: Boolean;
    Tmpstr: AnsiString;
  Begin
    Result := AnsiLowerCase(S);
    Tmpstr := AnsiUpperCase(S);
    CapitalizeNextLetter := True;
    For I := 1 To Length(Result) Do
      Begin
        If CapitalizeNextLetter Then
          Result[I] := Tmpstr[I];
        CapitalizeNextLetter := (Result[I] = ' ') Or (Result[I] = #13) Or (Result[I] = #10);
      End ;
  End ;

  Function Miesiac(m: Longint): String;
  Begin
    Case m Of
      1: Result := 'styczeñ';
      2: Result := 'luty';
      3: Result := 'marzec';
      4: Result := 'kwiecieñ';
      5: Result := 'maj';
      6: Result := 'czerwiec';
      7: Result := 'lipiec';
      8: Result := 'sierpieñ';
      9: Result := 'wrzesieñ';
      10: Result := 'paŸdziernik';
      11: Result := 'listopad';
      12: Result := 'grudzieñ';
      Else
        Result := '';
    End ;
  End ;

  Function RoundExtended(E: Extended; Precision, Decimals: Integer; aRound: TRound): Extended;
    Var
      Dz : Extended ;

    Function Pad(Const Sv: String; PadLen: Smallint; c: char = ' '): String;
    Begin
      Result := Sv;
      If PadLen < 0 Then
        Begin
          While Length(Result) < abs(PadLen) Do
            Result := c + Result;
        End
      Else
        Begin
          While Length(Result) < PadLen Do
            Result := Result + c;
        End ;
    End ;

    Function _IntPower(Const Base: Extended; Const Exponent: Integer): Extended;
    Asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
    End ;

    Function Sign(Const AValue: Extended): Integer;
    Type
      TSign = -1..1;
    Const
      NegativeV = Low(TSign);
      PositiveV = High(TSign);
    Begin
      If AValue < 0 Then
        Sign := NegativeV
      Else
        Sign := PositiveV;
    End ;

    Function RoundEInt(AValue: Extended; ADigits: Integer): Extended;
    Var
      E1: Extended;
    Begin
      E1 := _IntPower(10.0, ADigits);
      RoundEInt := Int((AValue * E1) + (Sign(AValue) * 0.5)) / E1;
    End ;

    Function RoundEInt2(AValue: Extended; ADigits: Integer; Add: Extended): Extended;
    Var
      E1: Extended;
    Begin
      E1 := _IntPower(10.0, ADigits);
      RoundEInt2 := Int((AValue * E1) + (Sign(AValue) * Add)) / E1;
    End ;

    Function RoundE(Const AValue: Extended; Const ADigit: Integer): Extended;
    Var
      LFactor: Extended;
    Begin
      LFactor := _IntPower(10, ADigit * -1);
      Result := Round(AValue / LFactor) * LFactor;
    End ;

  Begin
    Result := E;
    If (Decimals > -1) And (aRound <> rNone) Then
      Begin
        Case aRound Of
          rMathematical:
            Begin
              Try
                Result := RoundEInt(E, Decimals);
              Except
                Raise;
              End ;
            End ;

          rMatAfter1..rMatAfter9:
            Begin
              Case aRound Of
                rMatAfter1: Dz := 0.9;
                rMatAfter2: Dz := 0.8;
                rMatAfter3: Dz := 0.7;
                rMatAfter4: Dz := 0.6;
                rMatAfter5: Dz := 0.5;
                rMatAfter6: Dz := 0.4;
                rMatAfter7: Dz := 0.3;
                rMatAfter8: Dz := 0.2;
                rMatAfter9: Dz := 0.1;
              Else
                Dz := 0.0 ;
              End ;
              Try
                Result := RoundEInt2(E, Decimals, Dz);
              Except
                Raise;
              End ;
            End ;
        End ;
      End
    Else
      Result := E;
  End ;

  Function WeekNo(ADate: TDateTime): Longint;
  Var
    AYear, AMonth, ADay: Word;
    //TheYear : Word;      // week, to which the week belongs
    AWeekDay: Word; // Day of week for 1. Jan
    ANumDays: Longint; // Days since 1. Jan
    AFirstDayOfYear: TDateTime; // Date of 1. Jn
  Begin
    Try
      DecodeDate(ADate, AYear, AMonth, ADay);
      //TheYear := AYear;
      AFirstDayOfYear := EncodeDate(AYear, 1, 1);
      AWeekDay := SysUtils.DayOfWeek(AFirstDayOfYear);
      ANumDays := Trunc(Int(ADate) - AFirstDayOfYear) + (7 - SysUtils.DayOfWeek(ADate - 1)) +
        (7 * Ord(AWeekDay In [2..5]));
      Result := ANumDays Div 7;
      If Result = 0 Then
        Begin
          If (SysUtils.DayOfWeek(EncodeDate(AYear - 1, 1, 1)) > 5) Or
            (SysUtils.DayOfWeek(EncodeDate(AYear - 1, 12, 31)) < 5) Then
            Result := 52
          Else
            Result := 53;
          //TheYear := AYear - 1;
        End
      Else If Result = 53 Then
        If (AWeekDay > 5) Or (SysUtils.DayOfWeek(EncodeDate(AYear, 12, 31)) < 5) Then
          Begin
            Result := 1;
            //TheYear := AYear + 1;
          End ;
    Except
      Result := 0;
    End ;
  End ;

  Function OnlyYear(ADate: TDateTime): Longint;
  Var
    WDay, WMonth, WYear: Word;
  Begin
    DecodeDate(ADate, WYear, WMonth, WDay);
    Result := WYear;
  End ;

  Function OnlyMonth(ADate: TDateTime): Longint;
  Var
    WDay, WMonth, WYear: Word;
  Begin
    DecodeDate(ADate, WYear, WMonth, WDay);
    Result := WMonth;
  End ;

  Function OnlyDay(ADate: TDateTime): Longint;
  Var
    WDay, WMonth, WYear: Word;
  Begin
    DecodeDate(ADate, WYear, WMonth, WDay);
    Result := WDay;
  End ;

  Procedure ExecScript(Script: String);
  Var
    sl, sl1, ESc: TStringList;
  Begin
    If Length(Script) > 0 Then
      Begin
        sl  := TStringList.Create ;
        sl1 := TStringList.Create ;
        esc := TStringList.Create ;
        esc.text := Script;
        Try
          PrepareScript(Esc , sl , sl1) ;
          executeScript(sl) ;
        Finally
          FreeAndNil(esc) ;
          FreeAndNil(sl)  ;
          FreeAndNil(sl1) ;
        End ;
      End ;
  End ;

  Function IsValidBoolean(Const S: ShortString): Boolean;
  Var
    st: String;
  Begin
    st := S;
    st := UpperCase(Trim(st));
    If (length(S) < 1) then
      Result := False
    Else
      If (st[1] = 'T') or (st[1] = 'F') Then
        Result := True
      Else
        Result := False ;
  End ;

  Function VrStrToBoolean(Const S: ShortString): Boolean;
  Var
    st: String;
  Begin
    st := S;
    st := UpperCase(Trim(st));
    If (st[1] = 'T') Then
      Result := True
    Else
      Result := False;
  End ;

  Function IsValidTimestamp(Const S: ShortString): boolean;
  Begin
    If (length(S) < 19)
      Or Not (S[5] In ['-', '.', '/'])
      Or (S[8] <> S[5])
      Or (S[11] <> ' ')
      Or (S[14] <> ':')
      Or (S[17] <> ':') Then
      Result := False
    Else
      Try
        EncodeDate(
          StrToInt(copy(S, 1, 4)),
          StrToInt(copy(S, 6, 2)),
          StrToInt(copy(S, 9, 2)));
        EncodeTime(
          StrToInt(copy(S, 12, 2)),
          StrToInt(copy(S, 15, 2)),
          StrToInt(copy(S, 18, 2)),
          0);
        Result := True;
      Except
        Result := False;
      End ;
  End ;

  Function VrStrToTimestamp(Const S: ShortString): TDateTime;
  Begin
    Result := EncodeDate(
      StrToInt(copy(S, 1, 4)),
      StrToInt(copy(S, 6, 2)),
      StrToInt(copy(S, 9, 2)))
      +
      EncodeTime(
      StrToInt(copy(S, 12, 2)),
      StrToInt(copy(S, 15, 2)),
      StrToInt(copy(S, 18, 2)),
      0);
    fsSetMillisecond(Result, 0);
  End ;

  Function IsValidDate(Const S: ShortString): Boolean;
  Begin
    If (length(S) < 10)
      Or Not (S[5] In ['-', '.', '/'])
      Or (S[8] <> S[5]) Then
      Result := False
    Else
      Try
        EncodeDate(
          StrToInt(copy(S, 1, 4)),
          StrToInt(copy(S, 6, 2)),
          StrToInt(copy(S, 9, 2)));
        Result := True;
      Except
        Result := False;
      End ;
  End ;

  Function VrStrToDate(Const S: ShortString): TDateTime;
  Begin
    Result := EncodeDate(
      StrToInt(copy(S, 1, 4)),
      StrToInt(copy(S, 6, 2)),
      StrToInt(copy(S, 9, 2)));
  End ;

  Function IsValidTime(Const S: ShortString): Boolean;
  Begin
    If (length(S) < 8)
      Or (S[3] <> ':')
      Or (S[6] <> ':') Then
      Result := False
    Else
      Try
        EncodeTime(
          StrToInt(copy(S, 1, 2)),
          StrToInt(copy(S, 4, 2)),
          StrToInt(copy(S, 7, 2)),
          0);
        Result := True;
      Except
        Result := False;
      End ;
  End ;

  Function VrStrToTime(Const S: ShortString): TDateTime;
  Begin
    Result := EncodeTime(
      StrToInt(copy(S, 1, 2)),
      StrToInt(copy(S, 4, 2)),
      StrToInt(copy(S, 7, 2)),
      0);
  End ;

  Function VrRemoveQuotes(Const St: String): String;
  Var
    S: String;
  Begin
    S := St;
    If (Length(s) > 2) And (s[1] = '"') Then
      Begin
        If (Length(s) > 2) And (s[1] = '"') And (s[Length(s)] = '"') Then
          Result := Copy(s, 2, Length(s) - 2)
        Else
          Result := s;
      End
    Else
      Begin
        If (Length(s) > 2) And (s[1] = '''') And (s[Length(s)] = '''') Then
          Result := Copy(s, 2, Length(s) - 2)
        Else
          Result := s;
      End ;
  End ;

  {$IFDEF DCC6OrLater}
    {$HINTS OFF}
  {$ENDIF}
  Function IsValidDouble(Const S : ShortString) : Boolean ;
    Var
      code : Integer  ;
      e    : Extended ;

    Begin
      Val(s , e , Code);
      Result := (Code = 0) ;  
    End ;
  {$IFDEF DCC6OrLater}
    {$HINTS OFF}        
  {$ENDIF}

  Function VrStrToFloat(s: String): Extended;
  Var
    i: Integer;
  Begin
    Result := 0;
    s := Trim(s);
    If s = '' Then Exit;
    For i := 1 To Length(s) Do
      If s[i] In [',', '.'] Then
        s[i] := DecimalSeparator;
    Result := StrToFloat(s);
  End ;

  Function VrStrToInteger(s: String): Integer;
  Var
    i: Integer;
    e: Extended;
    flag: Boolean;
  Begin
    Result := 0;
    flag := False;
    s := Trim(s);
    If s = '' Then Exit;
    For i := 1 To Length(s) Do
      If s[i] In [',', '.'] Then
        Begin
          s[i] := DecimalSeparator;
          flag := True;
        End ;
    If flag Then
      Begin
        e := StrToFloat(s);
        Result := Round(e);
      End
    Else
      Result := StrToInt(s);
  End ;

  Function VrStrToInteger64(s: String): Extended;
  Var
    i: Integer;
    e: Extended;
  Begin
    Result := 0;
    s := Trim(s);
    If s = '' Then Exit;
    For i := 1 To Length(s) Do
      If s[i] In [',', '.'] Then
        s[i] := DecimalSeparator;
    e := StrToFloat(s);
    Result := Round(e);
  End ;

Begin
  Value := null;
  If Self.ErrorResult <> 0 Then Exit;
  SName := Name;
  SName := Trim(UpperCase(SName));

  // for stringlist and dynamicarray
  If Pos('.', SName) <> 0 Then
    Begin
      SVar1 := Copy(SName, 1, Pos('.', SName) - 1);
      SVar2 := Copy(SName, Pos('.', SName) + 1, length(SName));
      SVar3 := '';
      If Pos('.', SVar2) <> 0 Then
        Begin
          SVar3 := Copy(SVar2, Pos('.', SVar2) + 1, length(SVar2));
          SVar2 := Copy(SVar2, 1, Pos('.', SVar2) - 1);
        End ;
      SVar1 := Trim(SVar1);
      SVar2 := Trim(SVar2);
      SVar3 := Trim(SVar3);
      If SVar1 = '' Then
        If ErrorResult = 0 Then
          Begin
            Self.ErrorResult := 5002;
            Exit;
          End ;

      If SVar1 = 'CREATEARRAYLIST' Then
        Begin
        End
      Else If SVar1 = 'CREATESTRINGLIST' Then
        Begin
          If SVar2 <> '' Then
            internalStrList.AddObject(SVar2, TStringList.Create);
        End
      Else If InternalArrayList.IndexOf(SVar1) > -1 Then
        Begin

        End
      Else If InternalStrList.IndexOf(SVar1) > -1 Then
        Begin
          i := InternalStrList.IndexOf(SVar1);
          If SVar2 = 'DESTROY' Then
            Begin
              TStringList(internalStrList.Objects[i]).free;
              internalStrList.Delete(i);
            End
          Else If SVar2 = 'ADD' Then // Add(S: String)
            TStringList(internalStrList.Objects[i]).Add(Parser.Calculate(p1))
          Else If SVar2 = 'CLEAR' Then // Clear
            TStringList(internalStrList.Objects[i]).Clear
          Else If SVar2 = 'BEGINUPDATE' Then // BeginUpdate
            TStringList(internalStrList.Objects[i]).BeginUpdate
          Else If SVar2 = 'ENDUPDATE' Then // EndUpdate
            TStringList(internalStrList.Objects[i]).EndUpdate
          Else If SVar2 = 'INSERT' Then //Insert(Index: Integer; const S: string)
            TStringList(internalStrList.Objects[i]).Insert(Parser.Calculate(p1), Parser.Calculate(p2))
          Else If SVar2 = 'DELETE' Then //Delete(Index: Integer)
            Begin
              i := Parser.Calculate(p1);
              If (i <= TStringList(internalStrList.Objects[i]).Count - 1) And (i >= 0) Then
                TStringList(internalStrList.Objects[i]).Delete(i);
            End
          Else If SVar2 = 'SAVETOFILE' Then
            Begin
              s1 := Parser.Calculate(p1);
              If s1 <> '' Then
                TStringList(internalStrList.Objects[i]).SaveToFile(s1);
            End
          Else If SVar2 = 'LOADFROMFILE' Then
            Begin
              s1 := Parser.Calculate(p1);
              If s1 <> '' Then
                TStringList(internalStrList.Objects[i]).LoadFromFile(s1);
            End
          Else If SVar2 = 'ITEMS' Then //Delete(Index: Integer)
            Begin
              i := Parser.Calculate(p1);
              If (i <= TStringList(internalStrList.Objects[i]).Count - 1) And (i >= 0) Then
                Value := TStringList(internalStrList.Objects[i])[Parser.Calculate(p1)]
              Else
                Value := null;
            End
          Else If SVar2 = 'SORT' Then //Sorted:= t/f
            TStringList(internalStrList.Objects[i]).Sorted := Parser.Calculate(p1)
          Else If SVar2 = 'FIND' Then //Find(S: String, Var I: Integer)
            Begin
              i := -1;
              TStringList(internalStrList.Objects[i]).Find(Parser.Calculate(p1), i);
              Value := i;
            End
          Else If SVar2 = 'INDEXOF' Then //IndexOf(S: String)
            Begin
              i := -1;
              i := TStringList(internalStrList.Objects[i]).IndexOf(Parser.Calculate(p1));
              Value := i;
            End ;
        End
      Else If SVar1 = 'INPUTVARIABLE' Then
        Begin
          If SVar2 = '' Then ErrorResult := 50016; // name
          If SVar3 = '' Then ErrorResult := 50016; // asString...
          If ErrorResult <> 0 Then Exit;
          Try
            If InternalVariables.IndexOf('_@#$-PARAM' + IntToStr(InternalParamPosition)) > -1 Then
              Begin
                s1 := InternalVariables['_@#$-PARAM' + IntToStr(InternalParamPosition)];

                If (s1[1] = '[') And (s1[length(s1)] = ']') Then
                  s1 := copy(s1, 2, length(s1) - 2);

                If SVar3 = 'ASSTRING' Then
                  InternalVariables[SVar2] := s1;
                If SVar3 = 'ASDOUBLE' Then
                  If IsValidDouble(s1) Then
                    InternalVariables[SVar2] := VrStrToFloat(s1);
                If SVar3 = 'ASINTEGER' Then
                  If IsValidDouble(s1) Then
                    InternalVariables[SVar2] := VrStrTointeger(s1);
                If SVar3 = 'ASINTEGER64' Then
                  If IsValidDouble(s1) Then
                    InternalVariables[SVar2] := VrStrToInteger64(s1);
                If SVar3 = 'ASDATETIME' Then
                  If isValidTimestamp(s1) Then
                    InternalVariables[SVar2] := VrStrToTimeStamp(s1);
                If SVar3 = 'ASDATE' Then
                  If isValidDate(s1) Then
                    InternalVariables[SVar2] := VrStrToDate(s1);
                If SVar3 = 'ASTIME' Then
                  If isValidTime(s1) Then
                    InternalVariables[SVar2] := VrStrToTime(s1);
                If SVar3 = 'ASBOOLEAN' Then
                  If isValidBoolean(s1) Then
                    InternalVariables[SVar2] := VrStrToBoolean(s1);

                inc(InternalParamPosition);
              End ;
          Except
            ErrorResult := 50016;
          End ;
        End ;
    End
  Else
    Begin
      If SName = 'EXPANDVAR' Then
        Begin
          If (TVarData(p1).VType = varString) Or (TVarData(p1).VType = varOlestr) Then
            Value := Self.ExpandExpression(Parser.Calculate(p1))
          Else
            Value := p1;
        End
      Else If SName = 'INC' Then
        Begin
          Parser.OnGetValue(p1, v1);
          SetValue(p1, v1 + 1);
        End
      Else If SName = 'DEC' Then
        Begin
          Parser.OnGetValue(p1, v1);
          SetValue(p1, v1 - 1);
        End
      Else If SName = 'SQLSTRDATETIME' Then
        Value := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Parser.Calculate(p1)) + ''''
      Else If SName = 'SQLSTRDATE' Then
        Value := '''' + FormatDateTime('yyyy-mm-dd', Parser.Calculate(p1)) + ''''
      Else If SName = 'SQLSTRTIME' Then
        Value := '''' + FormatDateTime('hh:nn:ss', Parser.Calculate(p1)) + ''''
      Else If SName = 'SQLSTRDOUBLE' Then
        Value := fsReplaceStr(Parser.Calculate(p1), DecimalSeparator, '.')
      Else If SName = 'SQLSTRINTEGER' Then
        Begin
          s1 := Parser.Calculate(p1);
          Value := s1;
        End
      Else If SName = 'SQLSTRBOOLEAN' Then
        Begin
          If Parser.Calculate(p1) Then
            Value := 'True'
          Else
            Value := 'False';
        End
      Else If SName = 'SETARRAY' Then
        Begin
          v := Parser.Calculate(p2);
          If (TVarData(p1).VType = varString) Or (TVarData(p1).VType = varOlestr) Then
            s := v1
          Else
            s := IntToStr(v1);
          InternalVariables['Array_' + p1 + '_' + s] := Parser.Calculate(p3);
        End
      Else If SName = 'GETARRAY' Then
        Begin
          v := Parser.Calculate(p2);
          If (TVarData(p1).VType = varString) Or (TVarData(p1).VType = varOlestr) Then
            s := v1
          Else
            s := IntToStr(v1);
          Value := InternalVariables['Array_' + p1 + '_' + s];
        End
      Else
        Begin
          FNo := FFunction.IndexOf(SName);
          If fno < 0 Then Exit;

          Case FNo Of
            0: Value := FormatDateTime(Parser.Calculate(p1), Parser.Calculate(p2));
            1: Value := FormatFloat(Parser.Calculate(p1), Parser.Calculate(p2));
            2: Value := AnsiLowerCase(Parser.Calculate(p1));
            3:
              Begin
                s1 := AnsiLowerCase(Parser.Calculate(p1));
                If Length(s1) > 0 Then
                  Value := AnsiUpperCase(s1[1]) + Copy(s1, 2, Length(s1) - 1)
                Else
                  Value := '';
              End ;
            4: Value := StrToInt(FormatDateTime('d', Parser.Calculate(p1)));
            5: Value := FormatMaskText(Parser.Calculate(p1) + ';0; ', Parser.Calculate(p2));
            6: Value := AnsiUpperCase(Parser.Calculate(p1));
            7: Value := p1;//fsslownie.slowniemon(Parser.Calculate(Trim(p1)));
            8:
              Begin
                Value := RoundExtended(Parser.Calculate(Trim(p1)),
                  Parser.Calculate(Trim(p2)), Parser.Calculate(Trim(p3)), Parser.Calculate(Trim(p4)));
              End ;
            9:
              Begin
                Try
                  dt := StrToDateTime(Trim(p1));
                Except
                  dt := Parser.Calculate(p1);
                End ;
                Value := IntToStr(OnlyDay(dt)); //Add('DAY');
              End ;
            10:
              Begin
                Try
                  dt := StrToDateTime(Trim(p1));
                Except
                  dt := Parser.Calculate(p1);
                End ;
                Value := IntToStr(OnlyMonth(dt)); //Add('MONTH');
              End ;
            11:
              Begin
                Try
                  dt := StrToDateTime(Trim(p1));
                Except
                  dt := Parser.Calculate(p1);
                End ;
                Value := IntToStr(OnlyYear(Dt)); //Add('YEAR');
              End ;
            12:
              Begin
                Try
                  dt := StrToDateTime(Trim(p1));
                Except
                  dt := Parser.Calculate(p1);
                End ;
                Value := IntToStr(WeekNo(dt)); //Add('WEEKNO');
              End ;
            13:
              Begin
                v := Parser.Calculate(p1);
                v1 := Parser.Calculate(p2);
                If v > v1 Then
                  Value := v
                Else
                  Value := v1;
              End ;
            14: //Add('LongintTOSTRPL');
              Begin
              Value := p1;
                {v164 := Parser.Calculate(p1);
                If vartype(v164) = varstring Then
                  Begin
                    Try
                      kk := strtoint64(Trim(v164));
                    Except
                      kk := 0;
                    End ;
                    Value := fsslownie.slownieint(kk);
                  End
                Else
                  Value := fsslownie.slownieint(Round(V164));   }
              End ;
            15:
              Begin
                Try
                  Value := miesiac(OnlyMonth(Parser.Calculate(Trim(p1))));
                Except
                  Try
                    Value := miesiac(Parser.Calculate(Trim(p1)));
                  Except
                    Value := '';
                  End ;
                End ;
              End ;
            16:
              Begin
                Try
                  vv := Parser.Calculate(p1);
                  Value := CapitalizeWord(vv);
                Except
                  Value := vv;
                End ;
              End ;

            17: Value := ReplaceS(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            18:
              Begin
                cStr := Parser.Calculate(p3);
                fsDateDiffEx(Parser.Calculate(p1), Parser.Calculate(p2), cStr);
                ExecScript(p3 + ':=' + CHR(39) + cStr + CHR(39));
              End ;

            19: Value := ABS(Parser.Calculate(p1));
            20: Value := CHR(Byte(Parser.Calculate(p1)));
            21: Value := fsCompareStr(Parser.Calculate(p1), Parser.Calculate(p2));
            22: Value := fsCreateDate(Parser.Calculate(p1), Parser.Calculate(p2));
            23: Value := fsCreateNum(Parser.Calculate(p1));
            24: Value := fsCreateStr(Parser.Calculate(p1));
            25: Value := DateToStr(Parser.Calculate(p1));
            26: Value := fsDaysPerMonth(Parser.Calculate(p1), Parser.Calculate(p2));
            27: Value := VrDelete(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            28: Value := fsEndPos(Parser.Calculate(p1), Parser.Calculate(p2));
            29: Value := fsExtractWord(Parser.Calculate(p1), Parser.Calculate(p2), ConvCS(Parser.Calculate(p3)));
            30: Value := fsFirstDayOfNextMonth(Parser.Calculate(p1));

            31: Value := fsFirstDayOfPrevMonth(Parser.Calculate(p1));
            32: Value := fsIncDateEx(Parser.Calculate(p1), Parser.Calculate(p2));
            33: Value := fsIncDay(Parser.Calculate(p1), Parser.Calculate(p2));
            34: Value := fsIncMonth(Parser.Calculate(p1), Parser.Calculate(p2));
            35: Value := fsIncTimeEx(Parser.Calculate(p1), Parser.Calculate(p2));
            36: Value := fsIncYear(Parser.Calculate(p1), Parser.Calculate(p2));
            37: Value := VrInsert(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            38: Value := fsIsLeapYear(Parser.Calculate(p1));

            39: Value := fsIsRangeDate(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            40: Value := fsIsRangeNum(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));

            41: Value := fsIsWordPresent(Parser.Calculate(p1), Parser.Calculate(p2), ConvCS(Parser.Calculate(p3)));
            42: Value := fsLastDayOfPrevMonth(Parser.Calculate(p1));
            43: Value := fsLeftCopy(Parser.Calculate(p1), Parser.Calculate(p2));
            44: Value := fsNPos(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            45: Value := fsPadCenter(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            46: Value := fsPadLeft(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            47: Value := fsPadRight(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            48: Value := fsReplaceStr(Parser.Calculate(p1), Parser.Calculate(p2), Parser.Calculate(p3));
            49: Value := fsReplicate(Parser.Calculate(p1), Parser.Calculate(p2));
            50: Value := fsRightCopy(Parser.Calculate(p1), Parser.Calculate(p2));
            51: Value := fsStrToDateDef(Parser.Calculate(p1), Parser.Calculate(p2));
            52: Value := StrToFloat(Parser.Calculate(p1));
            53: Value := fsStrToFloatDef(Parser.Calculate(p1), Parser.Calculate(p2));
            54: Value := StrToInt(Parser.Calculate(p1));
            55: Value := StrToIntDef(Parser.Calculate(p1), Parser.Calculate(p2));
            56: Value := TimeToStr(Parser.Calculate(p1));
            57: Value := TrimLeft(Parser.Calculate(p1));
            58: Value := TrimRight(Parser.Calculate(p1));
            59: Value := fsValidDate(Parser.Calculate(p1));
            60: Value := fsValidFloat(Parser.Calculate(p1));
            61: Value := fsValidInt(Parser.Calculate(p1));
            62: Value := fsWordCount(Parser.Calculate(p1), ConvCS(Parser.Calculate(p2)));
            63: Value := fsWordPosition(Parser.Calculate(p1), Parser.Calculate(p2), ConvCS(Parser.Calculate(p3)));
            64: Value := DateTimeToStr(Parser.Calculate(p1));
            65:
              Begin
                Value := IntToStr(Parser.Calculate(Trim(p1)));
              End ;
            66:
              Begin
                Try
                  dt := Parser.Calculate(p1);
                  dd := dt;
                  Value := dd;
                Except
                  Value := Parser.Calculate(p1);
                End ;
              End ;
            67:
              Begin
                Try
                  dd := Parser.Calculate(p1);
                  dt := dd;
                  Value := dt;
                Except
                  Value := Parser.Calculate(p1);
                End ;
              End ;

            68: Value := Length(Parser.Calculate(p1));
            69: Value := Trim(Parser.Calculate(p1));
            70:
              Begin
                If Parser.Calculate(p1) = 1 Then
                  Value := Parser.Calculate(p2)
                Else
                  Value := Parser.Calculate(p3);
              End ;
            71: Value := Pos(Parser.Calculate(p1), Parser.Calculate(p2));
            72: Value := VarIsNull(Parser.Calculate(p1));
            73:
              Begin
                v := Parser.Calculate(p1);
                v1 := Parser.Calculate(p2);
                If v < v1 Then
                  Value := v
                Else
                  Value := v1;
              End ;
            74: Value := StrToInt(FormatDateTime('m', Parser.Calculate(p1)));
            75: Value := StrToInt(FormatDateTime('yyyy', Parser.Calculate(p1)));

            // next function
          End ;
        End ;
    End ;
End ;

Procedure TFunInterpretator.GetValue(Const Name: String; Var Value: Variant);
Var
  SName, SVar1, SVar2, SVar3: String;
Begin
  Value := null;
  If Self.ErrorResult <> 0 Then Exit;
  SName := Name;
  SName := Trim(UpperCase(SName));
  Try
    // for stringlist and dynamicarray
    If Pos('.', SName) <> 0 Then
      Begin
        SVar1 := Copy(SName, 1, Pos('.', SName) - 1);
        SVar2 := Copy(SName, Pos('.', SName) + 1, length(SName));
        SVar3 := '';
        If Pos('.', SVar2) <> 0 Then
          Begin
            SVar3 := Copy(SVar2, Pos('.', SVar2) + 1, length(SVar2));
            SVar2 := Copy(SVar2, 1, Pos('.', SVar2) - 1);
          End ;
        SVar1 := Trim(SVar1);
        SVar2 := Trim(SVar2);
        SVar3 := Trim(SVar3);
        If SVar1 = '' Then
          If ErrorResult = 0 Then
            Begin
              Self.ErrorResult := 5002;
              Exit;
            End ;

        If InternalArrayList.IndexOf(SVar1) > -1 Then
          Begin
            //If SVar2 = 'ROWCOUNT' Then
              //Value :=
          End
        Else If InternalStrList.IndexOf(SVar1) > -1 Then
          Begin
            If SVar2 = 'COUNT' Then
              Value := TStringList(internalStrList.Objects[internalStrList.IndexOf(SVar1)]).Count;
          End ;
      End
    Else
      Begin
        If InternalConsts.IndexOf(SName) > -1 Then
          Value := InternalConsts[SName]
        Else If InternalVariables.IndexOf(SName) > -1 Then
          Value := InternalVariables[SName]
        Else If SName = 'NULL' Then
          Value := Null
        Else If SName = 'NOW' Then
          Value := Now
        Else If SName = 'DATE' Then
          Value := Date
        Else If SName = 'TIME' Then
          Value := Time
        Else If SName = 'MAXRECURSEVAR' Then
          Value := MaxRecurseVar
        Else If SName = 'ERRORRESULT' Then
          Value := ErrorResult
        Else If SName = 'SQLNOW' Then
          Value := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', now) + ''''
        Else If SName = 'SQLDATE' Then
          Value := '''' + FormatDateTime('yyyy-mm-dd', now) + ''''
        Else If SName = 'SQLTIME' Then
          Value := '''' + FormatDateTime('yyyy-mm-dd hh:nn:ss', now) + '''';
      End ;
  Except
    If ErrorResult = 0 Then
      Self.ErrorResult := 5002;
  End ;
End ;

Procedure TFunInterpretator.SetValue(Const Name: String; Value: Variant);
Var
  SName, SVar1, SVar2, SVar3: String;
Begin
  If Self.ErrorResult <> 0 Then Exit;
  SName := Name;
  SName := Trim(UpperCase(SName));
  Try
    // for stringlist and dynamicarray
    If Pos('.', SName) <> 0 Then
      Begin
        SVar1 := Copy(SName, 1, Pos('.', SName) - 1);
        SVar2 := Copy(SName, Pos('.', SName) + 1, length(SName));
        SVar3 := '';
        If Pos('.', SVar2) <> 0 Then
          Begin
            SVar3 := Copy(SVar2, Pos('.', SVar2) + 1, length(SVar2));
            SVar2 := Copy(SVar2, 1, Pos('.', SVar2) - 1);
          End ;
        SVar1 := Trim(SVar1);
        SVar2 := Trim(SVar2);
        SVar3 := Trim(SVar3);
        If SVar1 = '' Then
          If ErrorResult = 0 Then
            Begin
              Self.ErrorResult := 5002;
              Exit;
            End ;
      End
    Else
      Begin
        If InternalVariables.IndexOf(SName) > -1 Then
          InternalVariables[SName] := Value
        Else If SName = 'MAXRECURSEVAR' Then
          MaxRecurseVar := Value
        Else If SName = 'ERRORRESULT' Then
          ErrorResult := Value;
        // if new var
        InternalVariables[SName] := Value;
      End ;
  Except
    If ErrorResult = 0 Then
      Self.ErrorResult := 5002;
  End ;
End ;

End.

