{********************************************************************}
{* CARRCONV.PAS - character array conversion                        *}
{********************************************************************}

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
 * The Original Code is TurboPower B-Tree Filer
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Based in part on code written by Ralf Nagel
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{--Conditional defines and compiler options}
{$I BTDefine.Inc}
{$IFDEF UseSymFlex}
  {$I DDDefine.Inc}
{$ENDIF}
{$IFDEF CanAllowOverlays}
  {$O+,F+}
{$ENDIF}
{$N+}

{$IFDEF Win32}
  !! Error - this unit cannot be compiled for 32-bit
{$ENDIF}

Unit CArrConv;

Interface
{$IFDEF SymFlexGE20}
Uses
  FLibSupp;
{$ELSE}
 {$IFDEF Windows}
Uses
  {$IFDEF VER80}                                       {!!.51}
  SysUtils;                                            {!!.51}
  {$ELSE}                                              {!!.51}
  Strings;
  {$ENDIF}                                             {!!.51}
 {$ENDIF}
{$ENDIF}


  Function Char2Boolean ( Var Value : Boolean;
                              Chr   : Char ) : Integer;

  Function CArr2LBStr ( Var LBStr : String;
                            CArr  : Pointer;
                            Size  : Word ) : Integer;

  Function CArr2AZStr ( AZStr,
                        CArr : Pointer;
                        Size : Word ) : Integer;

  Function CArr2Date ( Var TheDate : Longint;
                           CArr    : Pointer ) : Integer;

  Function CArr2Time ( Var TheTime : Longint;
                           CArr    : Pointer ) : Integer;

  Function CArr2Byte ( Var Value : Byte;
                           CArr  : Pointer;
                           Size  : Word ) : Integer;

  Function CArr2ShortInt ( Var Value : Shortint;
                               CArr  : Pointer;
                               Size  : Word ) : Integer;

  Function CArr2Integer ( Var Value : Integer;
                              CArr  : Pointer;
                              Size  : Word ) : Integer;

  Function CArr2LongInt ( Var Value : Longint;
                              CArr  : Pointer;
                              Size  : Word ) : Integer;

  Function CArr2Word ( Var Value : Word;
                           CArr  : Pointer;
                           Size  : Word ) : Integer;

  Function CArr2Single ( Var Value : Single;
                             CArr  : Pointer;
                             Size  : Word ) : Integer;

  Function CArr2Real ( Var Value : Real;
                           CArr  : Pointer;
                           Size  : Word ) : Integer;

  Function CArr2Comp ( Var Value : Comp;
                           CArr  : Pointer;
                           Size  : Word ) : Integer;

  Function CArr2Double ( Var Value : Double;
                             CArr  : Pointer;
                             Size  : Word ) : Integer;

  Function CArr2Extended ( Var Value : Extended;
                               CArr  : Pointer;
                               Size  : Word ) : Integer;


  Function Date2CArr ( CArr    : Pointer;
                       TheDate : Longint ) : Integer;

  Function Time2CArr ( CArr    : Pointer;
                       TheTime : Longint ) : Integer;

  Function Boolean2Char ( Var Chr   : Char;
                              Value : Boolean ) : Integer;

  Function LBStr2CArr ( CArr : Pointer;
                        Str  : String;
                        Size : Word ) : Integer;

  Function AZStr2CArr ( CArr,
                        Str  : Pointer;
                        Size : Word ) : Integer;

  Function ShortInt2CArr ( CArr  : Pointer;
                           Value : Shortint;
                           Width : Word ) : Integer;

  Function Byte2CArr ( CArr  : Pointer;
                       Value : Byte;
                       Width : Integer ) : Integer;

  Function Integer2CArr ( CArr  : Pointer;
                          Value : Integer;
                          Width : Integer ) : Integer;

  Function Word2CArr ( CArr  : Pointer;
                       Value : Word;
                       Width : Integer ) : Integer;

  Function LongInt2CArr ( CArr  : Pointer;
                          Value : Longint;
                          Width : Integer ) : Integer;

  Function Single2CArr ( CArr     : Pointer;
                         Value    : Single;
                         Width    : Integer;
                         Decimals : Integer ) : Integer;

  Function Real2CArr ( CArr     : Pointer;
                       Value    : Real;
                       Width    : Integer;
                       Decimals : Integer ) : Integer;

  Function Double2CArr ( CArr     : Pointer;
                         Value    : Double;
                         Width    : Integer;
                         Decimals : Integer ) : Integer;

  Function Comp2CArr ( CArr  : Pointer;
                       Value : Comp;
                       Width : Integer ) : Integer;

  Function Extended2CArr ( CArr     : Pointer;
                           Value    : Extended;
                           Width    : Integer;
                           Decimals : Integer ) : Integer;


Implementation {=========================================================}


{$IFNDEF SymFlexGE20}
  Const
    Offset1900 : Word = 20;
      {-Must be in range 0..99;
        dates less than 100 are interpreted to be in the 20th century when
        they are greater than Offset1900, else in the 21st century}
    BadDate = $FFFFFFFF;
    BadTime = $FFFFFFFF;
    MaxDateVal = 876581; {31.12.3999}

 {$IFNDEF Windows}
  Type
    PChar = ^Char;
 {$ENDIF}

  Type
    CharArr = Array [0 .. $FFFE] Of Char;
    PCharArr = ^CharArr;
    Date = LongInt;
    Time = LongInt;
{$ENDIF}


{$IFNDEF SymFlexGE20}
  Function Trim ( S : String ) : String;

  Var
    I    : Word;
    SLen : Byte Absolute S;
    OK   : Boolean;

  Begin
    While (SLen > 0) And (S [SLen] <= ' ') Do Begin
      Dec (SLen);
    End;

    I := 1;
    OK := True;
    While (I <= SLen) And OK Do Begin
      If S [I] <= ' ' Then Begin
        Inc (I);
      End Else Begin
        OK := False;
      End;
    End;

    If I > 1 Then Delete (S, 1, Pred (I));

    Trim := S;
  End;


  Function GetAZSLength ( AZSPtr : PChar ) : Word;

  Var
    L : Word;

  Begin
  {$IFDEF Windows}
    GetAZSLength := StrLen (AZSPtr);
  {$ELSE}
    Inline (
               $FC /                          { Cld }
               $C4 / $BE / AZSPtr /           { Les DI, AZSPtr [BP] }
               $B9 / $FF / $FF /              { Mov CX, $FFFF }
               $31 / $C0 /                    { Xor AX, AX }
               $F2 /                          { Repne }
               $AE /                          { Scasb }
               $B8 / $FE / $FF /              { Mov AX, $FFFE }
               $29 / $C8 /                    { Sub AX, CX }
               $89 / $86 / L                  { Mov [BP+Ofs(L)], AX }
                                                                       );
    GetAZSLength := L;
  {$ENDIF}
  End;
{$ENDIF}


  Procedure ExpandYear ( Var Year : Word );

  Begin
    If Year < 100 Then Begin
      If Year < Offset1900 Then Begin
        Inc (Year, 2000);
      End Else Begin
        Inc (Year, 1900);
      End;
    End;
  End;


{$IFNDEF SymFlexGE20}
  Function IsLeapYear ( Year : Word ) : Boolean;

  Begin
    IsLeapYear := (Year Mod 4 = 0) And (Year Mod 4000 <> 0)
        And ((Year Mod 100 <> 0) Or (Year Mod 400 = 0));
  End;


  Function DaysInMonth ( Month, Year : Word ) : Word;

  Begin
    Case Month Of
      1, 3, 5, 7, 8, 10, 12 :
        DaysInMonth := 31;
      4, 6, 9, 11 :
        DaysInMonth := 30;
      2 : Begin
        ExpandYear ( Year );
        DaysInMonth := 28 + Ord (IsLeapYear ( Year ));
      End;
      Else DaysInMonth := 0;
    End; {Case}
  End;


  Function SplitDateIsOk ( Day, Month, Year : Word ) : Boolean;

  Begin
    SplitDateIsOk := False;
    ExpandYear ( Year );
    If (Day = 0) Or (Year < 1600) Or (Year > 3999) Then Exit;
    Case Month Of
      1..12 : Begin
        SplitDateIsOk := Day <= DaysInMonth ( Month, Year );
      End;
    End; {Case}
  End;


  Function DMYToDateVal ( Day, Month, Year : Word ) : Date;

  Var
    I        : Word;
    NrOfDays : LongInt;

  Begin
    ExpandYear ( Year );
    If Not SplitDateIsOk ( Day, Month, Year ) Then Begin
      DMYtoDateVal := BadDate;
      Exit;
    End;

    NrOfDays := (LongInt (Year) - 1600) * 365
        + (LongInt (Year) - 1597) Div 4
        - (LongInt (Year) - 1601) Div 100
        + (LongInt (Year) - 1601) Div 400;

    For I := 1 To Pred (Month) Do Begin
      NrOfDays := NrOfDays + DaysInMonth ( I, Year );
    End;

    DMYToDateVal := NrOfDays + Pred (Day);
  End;


  Function DateIsOk ( TheDate : Date ) : Boolean;

  Begin
    DateIsOk := (TheDate >= 0) And (TheDate <= MaxDateVal);
  End;


  Function DaysInYear ( Year : Word ) : Word;

  Begin
    DaysInYear := 365 + Ord (IsLeapYear ( Year ));
  End;


  Procedure DateValToDMY (     TheDate : Date;
                           Var Day,
                               Month,
                               Year    : Word );

  Var
    RemainingDays : LongInt;

  Begin
    If Not DateIsOk ( TheDate ) Then Begin
      Day := 0;
      Month := 0;
      Year := 0;
      Exit;
    End;
  {--TheDate div number of days in a 4 years block multiplied by 4 is
     used as an estimate for the year which may be too small}
    Year := Word (TheDate Div 1461 * 4) + 1600;
    RemainingDays := TheDate - DMYToDateVal ( 1, 1, Year );

    While RemainingDays >= DaysInYear ( Year ) Do Begin
      RemainingDays := RemainingDays - DaysInYear ( Year );
      Inc (Year);
    End;

    Month := 1;
    While RemainingDays >= DaysInMonth ( Month, Year ) Do Begin
      RemainingDays := RemainingDays - DaysInMonth ( Month, Year );
      Inc (Month);
    End;

    Day := Succ (Word (RemainingDays));
  End;


  Function SplitTimeIsOk ( Hour, Min, Sec : Word ) : Boolean;

  Begin
    SplitTimeIsOk := (Hour < 24) And (Min < 60) And (Sec < 60);
  End;


  Function HMSToTimeVal ( Hour, Min, Sec : Word ) : Time;

  Begin
    If SplitTimeIsOk ( Hour, Min, Sec ) Then Begin
      HMSToTimeVal := (LongInt (Hour) * 3600) + (LongInt (Min) * 60) + Sec;
    End Else Begin
      HMSToTimeVal := BadTime;
    End;
  End;


  Function TimeIsOk ( TheTime : Time ) : Boolean;

  Begin
    TimeIsOk := (TheTime >= 0) And (TheTime < 86400);
  End;


  Procedure TimeValToHMS ( TheTime : Time; Var Hour, Min, Sec : Word );

  Begin
    If Not TimeIsOk ( TheTime ) Then Begin
      Hour := 0;
      Min := 0;
      Sec := 0;
      Exit;
    End;
    Hour := TheTime Div 3600;
    TheTime := TheTime Mod 3600;
    Min := TheTime Div 60;
    TheTime := TheTime Mod 60;
    Sec := TheTime;
  End;
{$ENDIF}


  Function LBStr2CArr ( CArr : Pointer;
                        Str : String;
                        Size : Word ) : Integer;

  Var
    Len : Integer;

  Begin
    Len := Length ( Str );
    If Len > Size Then
      Len := Integer ( Size );
    Move ( Str [1], CArr^, Len);
    If Len < Size Then
      FillChar ( PCharArr ( CArr )^ [Len], Size - Len, #32 );
    LBStr2CArr := 0;
  End;


  Function AZStr2CArr ( CArr,
                        Str : Pointer;
                        Size : Word ) : Integer;

  Var
    Len : Word;

  Begin
    Len := GetAZSLength ( Str );
    If Len > Size Then
      Len := Size;
    Move ( Str^, CArr^, Len );
    If Len < Size Then
      FillChar ( PCharArr ( CArr )^ [Len], Size - Len, #32 );
    AZStr2CArr := 0;
  End;


  Function CArr2LBStr ( Var LBStr : String;
                            CArr  : Pointer;
                            Size  : Word ) : Integer;

  Begin
    CArr2LBStr := -1;
    If Size > 255 Then Exit;
    Move ( CArr^, LBStr [1], Size );
    LBStr [0] := Char ( Size );
    CArr2LBStr := 0;
  End;


  Function CArr2AZStr ( AZStr,
                        CArr : Pointer;
                        Size : Word ) : Integer;

  Begin
    Move ( CArr^, AZStr^, Size );
    PCharArr ( AZStr )^ [Size] := #0;
    CArr2AZStr := 0;
  End;


  Function Char2Boolean ( Var Value : Boolean;
                              Chr : Char ) : Integer;
  Begin
    Char2Boolean := 0;
    Case Upcase ( Chr ) Of
      'T', 'Y', 'J': Value := True;
      'F', 'N':      Value := False;
      Else Begin
        Value := False;
        Char2Boolean := -1;
      End;
    End;
  End;


  Function Str2DMY (     Str : String;
                     Var Day,
                         Month,
                         Year : Word ) : Boolean;

  Var
    Res : Integer;
    TStr : String;

  Begin
    Str2DMY := False;

    If Length ( Str ) < 8 Then Exit;

    TStr := Str [1] + Str [2] + Str [3] + Str [4];
    Val ( TStr, Year, Res );
    If Res <> 0 Then Exit;

    TStr := Str [5] + Str [6];
    Val ( TStr, Month, Res );
    If Res <> 0 Then Exit;

    TStr := Str [7] + Str [8];
    Val ( TStr, Day, Res );
    If Res <> 0 Then Exit;

    Str2DMY := True;
  End;


  Function CArr2Date ( Var TheDate : Longint;
                           CArr    : Pointer ) : Integer;

  Var
    TStr : String;
    Year, Month, Day : Word;

  Begin
    CArr2Date := -1;
    TheDate := 0;

    If CArr2LBStr ( TStr, CArr, 8 ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      CArr2Date := 0;
      Exit;
    End;
    If Not Str2DMY ( TStr, Day, Month, Year ) Then Exit;
    TheDate := DMYToDateVal ( Day, Month, Year );
    If TheDate = BadDate Then Exit;

    CArr2Date := 0;
  End;


  Function TimeStr2HMS ( Var Str : String;
                         Var Hour,
                             Min,
                             Sec : Integer ) : Boolean;

  Var
    Res : Integer;
    TStr : String;

  Begin
    TimeStr2HMS := False;

    If Length ( Str ) < 8 Then Exit;

    TStr := Str [1] + Str [2];
    Val ( TStr, Hour, Res );
    If Res <> 0 Then Exit;

    TStr := Str [4] + Str [5];
    Val ( TStr, Min, Res );
    If Res <> 0 Then Exit;

    TStr := Str [7] + Str [8];
    Val ( TStr, Sec, Res );
    If Res <> 0 Then Exit;

    TimeStr2HMS := True;
  End;


  Function CArr2Time ( Var TheTime : Longint;
                           CArr    : Pointer ) : Integer;

  Var
    TStr : String;
    Hour,
    Min,
    Sec : Integer;

  Begin
    CArr2Time := -1;

    If CArr2LBStr ( TStr, CArr, 8 ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      TheTime := 0;
      CArr2Time := 0;
      Exit;
    End;
    If Not TimeStr2HMS ( TStr, Hour, Min, Sec ) Then Exit;
    TheTime := HMSToTimeVal ( Hour, Min, Sec );
    If TheTime = BadTime Then Exit;

    CArr2Time := 0;
  End;


  Function IsNumStr ( Str : String;
                      MaxLen : Integer;
                      Signed,
                      Float,
                      Empty : Boolean ) : Boolean;

  Var
    I : Integer;
    CSet : Set Of Char;

  Begin
    If Str <> '' Then Begin
      IsNumStr := False;
      If Length ( Str ) > MaxLen Then Exit;
      If Str [1] = '-' Then Begin
        If Signed Then Begin
          Delete ( Str, 1, 1 );
        End Else Begin
          Exit;
        End;
      End;
      CSet := [];
      For I := 1 To Length ( Str ) Do
        CSet := CSet + [ Str [I] ];
      If Float Then Begin
        IsNumStr := CSet <= ['.', '0' .. '9'];
      End Else Begin
        IsNumStr := CSet <= ['0' .. '9'];
      End;
      Exit;
    End;
    IsNumStr := Empty;
  End;


  Function CArr2Byte ( Var Value : Byte;
                           CArr  : Pointer;
                           Size  : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Byte := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0;
      CArr2Byte := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, False, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Byte := 0;
  End;


  Function CArr2ShortInt ( Var Value : Shortint;
                               CArr : Pointer;
                               Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2ShortInt := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0;
      CArr2ShortInt := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, False, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2ShortInt := 0;
  End;


  Function CArr2Integer ( Var Value : Integer;
                              CArr : Pointer;
                              Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Integer := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0;
      CArr2Integer := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, False, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Integer := 0;
  End;


  Function CArr2Word ( Var Value : Word;
                           CArr : Pointer;
                           Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Word := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0;
      CArr2Word := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, False, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Word := 0;
  End;


  Function CArr2LongInt ( Var Value : Longint;
                              CArr  : Pointer;
                              Size  : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2LongInt := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0;
      CArr2LongInt := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, False, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2LongInt := 0;
  End;


  Function CArr2Comp ( Var Value : Comp;
                           CArr : Pointer;
                           Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Comp := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0.0;
      CArr2Comp := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, True, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Comp := 0;
  End;


  Function CArr2Single ( Var Value : Single;
                             CArr : Pointer;
                             Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Single := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0.0;
      CArr2Single := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, True, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Single := 0;
  End;


  Function CArr2Real ( Var Value : Real;
                           CArr : Pointer;
                           Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Real := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0.0;
      CArr2Real := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, True, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Real := 0;
  End;


  Function CArr2Double ( Var Value : Double;
                             CArr : Pointer;
                             Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Double := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0.0;
      CArr2Double := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, True, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Double := 0;
  End;


  Function CArr2Extended ( Var Value : Extended;
                               CArr : Pointer;
                               Size : Word ) : Integer;

  Var
    Res : Integer;
    TStr : String;

  Begin
    CArr2Extended := -1;

    If CArr2LBStr ( TStr, CArr, Size ) <> 0 Then Exit;
    TStr := Trim ( TStr );
    If TStr = '' Then Begin
      Value := 0.0;
      CArr2Extended := 0;
      Exit;
    End;
    If Not IsNumStr ( TStr, Size, True, True, True ) Then Exit;
    Val ( TStr, Value, Res );
    If Res <> 0 Then Exit;

    CArr2Extended := 0;
  End;


  Function DMY2DateStr ( Var DateStr : String;
                             Day,
                             Month,
                             Year : Word ) : Integer;

  Var
    TStr        : String;
    TempOff1900 : Word;                                              {!!.42}
    I           : Integer;                                           {!!.42}

  Begin
    DMY2DateStr := -1;

    TempOff1900 := Offset1900;                                       {!!.42}
    Offset1900 := 0;
    ExpandYear ( Year );
    Offset1900 := TempOff1900;                                       {!!.42}

    Str ( Year : 4 ,  TStr );
    If Length ( TStr ) <> 4 Then Exit;
    DateStr := TStr;
    Str ( Month : 2,  TStr );
    If Length ( TStr ) <> 2 Then Exit;
    DateStr := DateStr + TStr;
    Str ( Day : 2, TStr );
    If Length ( TStr ) <> 2 Then Exit;
    DateStr := DateStr + TStr;

    For I := 1 To Length ( DateStr ) Do Begin                        {!!.42}
      If DateStr [I] = ' ' Then DateStr [I] := '0'                   {!!.42}
    End;                                                             {!!.42}

    DMY2DateStr := 0;
  End;


  Function Date2CArr ( CArr    : Pointer;
                       TheDate : Longint ) : Integer;

  Var
    TStr : String;
    Month, Day, Year : Word;

  Begin
    Date2CArr := -1;

    DateValToDMY ( TheDate, Day, Month, Year );
    If DMY2DateStr ( TStr, Day, Month, Year ) <> 0 Then Exit;
    If LBStr2CArr ( CArr, TStr, 8 ) <> 0 Then Exit;

    Date2CArr := 0;
  End;


  Function HMS2TimeStr ( Var TimeStr : String;
                             Hour,
                             Min,
                             Sec     : Integer ) : Boolean;

  Var
    TStr : String;

  Begin
    HMS2TimeStr := False;

    Str ( Hour : 2 ,  TStr );
    If Length ( TStr ) <> 2 Then Exit;
    TimeStr := TStr + ':';
    Str ( Min : 2,  TStr );
    If Length ( TStr ) <> 2 Then Exit;
    TimeStr := TimeStr + TStr + ':';
    Str ( Sec : 2, TStr );
    If Length ( TStr ) <> 2 Then Exit;
    TimeStr := TimeStr + TStr;

    HMS2TimeStr := True;
  End;


  Function Time2CArr ( CArr    : Pointer;
                       TheTime : Longint ) : Integer;

  Var
    TStr : String;
    Hour,
    Min,
    Sec : Word;

  Begin
    Time2CArr := -1;

    If Not TimeIsOk ( TheTime ) Then Exit;
    TimeValToHMS ( TheTime, Hour, Min, Sec );
    If Not HMS2TimeStr ( TStr, Hour, Min, Sec ) Then Exit;

    Time2CArr := LBStr2CArr ( CArr, TStr, 8 );
  End;


  Function Boolean2Char ( Var Chr : Char; Value : Boolean ) : Integer;

  Begin
    If Value Then Begin
      Chr := 'T';
    End Else Begin
      Chr := 'F';
    End;

    Boolean2Char := 0;
  End;


  Function Byte2CArr ( CArr : Pointer;
                       Value : Byte;
                       Width : Integer ) : Integer;

  Var
    TStr : String;

  Begin
    Byte2CArr := -1;
    Str ( Value : Width, TStr );
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Byte2CArr := 0;
  End;


  Function ShortInt2CArr ( CArr : Pointer;
                           Value : Shortint;
                           Width : Word ) : Integer;

  Var
    TStr : String;

  Begin
    ShortInt2CArr := -1;
    Str ( Value : Width, TStr );
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    ShortInt2CArr := 0;
  End;


  Function Integer2CArr ( CArr : Pointer;
                          Value : Integer;
                          Width : Integer ) : Integer;

  Var
    TStr : String;

  Begin
    Integer2CArr := -1;
    Str ( Value : Width, TStr );
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Integer2CArr := 0;
  End;


  Function Word2CArr ( CArr : Pointer;
                       Value : Word;
                       Width : Integer ) : Integer;

  Var
    TStr : String;

  Begin
    Word2CArr := -1;
    Str ( Value : Width, TStr );
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Word2CArr := 0;
  End;


  Function LongInt2CArr ( CArr : Pointer;
                          Value : Longint;
                          Width : Integer ) : Integer;

  Var
    TStr : String;

  Begin
    LongInt2CArr := -1;
    Str ( Value : Width, TStr );
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    LongInt2CArr := 0;
  End;


  Function Comp2CArr ( CArr : Pointer;
                       Value : Comp;
                       Width : Integer ) : Integer;

  Var
    TStr : String;

  Begin
    Comp2CArr := -1;
    Str ( Value : Width : 0, TStr );
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Comp2CArr := 0;
  End;


  Function Single2CArr ( CArr : Pointer;
                         Value : Single;
                         Width : Integer;
                         Decimals : Integer ) : Integer;

  Var
    I : Integer;
    TStr : String;

  Begin
    Single2CArr := -1;
    If Decimals >= 0 Then Begin
      Str ( Value : Width : Decimals, TStr );
    End Else Begin
      Str ( Value : Width : 0, TStr );
      Decimals := Abs ( Decimals );
      Delete ( TStr, Width - Decimals + 1, Decimals );
      For I := 1 To Decimals Do
        Insert ( ' ', TStr, 1 );
    End;
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Single2CArr := 0;
  End;


  Function Real2CArr ( CArr : Pointer;
                       Value : Real;
                       Width : Integer;
                       Decimals : Integer ) : Integer;

  Var
    I : Integer;
    TStr : String;

  Begin
    Real2CArr := -1;
    If Decimals >= 0 Then Begin
      Str ( Value : Width : Decimals, TStr );
    End Else Begin
      Str ( Value : Width : 0, TStr );
      Decimals := Abs ( Decimals );
      Delete ( TStr, Width - Decimals + 1, Decimals );
      For I := 1 To Decimals Do
        Insert ( ' ', TStr, 1 );
    End;
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Real2CArr := 0;
  End;


  Function Double2CArr ( CArr : Pointer;
                         Value : Double;
                         Width : Integer;
                         Decimals : Integer ) : Integer;

  Var
    I : Integer;
    TStr : String;

  Begin
    Double2CArr := -1;
    If Decimals >= 0 Then Begin
      Str ( Value : Width : Decimals, TStr );
    End Else Begin
      Str ( Value : Width : 0, TStr );
      Decimals := Abs ( Decimals );
      Delete ( TStr, Width - Decimals + 1, Decimals );
      For I := 1 To Decimals Do
        Insert ( ' ', TStr, 1 );
    End;
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Double2CArr := 0;
  End;


  Function Extended2CArr ( CArr : Pointer;
                           Value : Extended;
                           Width : Integer;
                           Decimals : Integer ) : Integer;

  Var
    I : Integer;
    TStr : String;

  Begin
    Extended2CArr := -1;
    If Decimals >= 0 Then Begin
      Str ( Value : Width : Decimals, TStr );
    End Else Begin
      Str ( Value : Width : 0, TStr );
      Decimals := Abs ( Decimals );
      Delete ( TStr, Width - Decimals + 1, Decimals );
      For I := 1 To Decimals Do
        Insert ( ' ', TStr, 1 );
    End;
    If Width <> Length ( TStr ) Then Exit;
    Move ( TStr [1], CArr^, Width );
    Extended2CArr := 0;
  End;

{$IFDEF InitAllUnits}
Begin
{$ENDIF}
End.
