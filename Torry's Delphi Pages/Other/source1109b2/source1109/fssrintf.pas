{*********************************************************}
{* FlashFiler: Server interface unit for DLLs            *}
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

{=====================================================================
NOTES:

Using this unit, you will be able to write a DLL that provides two
special routines. These two routines used as a pair will identify what
FlashFiler calls a user-defined index. A user-defined index does not
generate its keys automatically like the normal (composite) indexes in
FlashFiler, and since it does not do that it cannot compare two keys
for their sorted order either. However, if you provide both of these
routines (a build key routine and a compare key routine) then
FlashFiler Server will be able to store your user-defined keys and
index records in the table for you.

The first routine is the Build Key routine. It must be of the
TffKeyBuildFunc type

  TffKeyBuildFunc = function (Index     : integer;
                              DataRec   : PffByteArray;
                          var Key;
                              KeyLen    : integer) : boolean;

Index is the number of the index for which the key must be generated.
DataRec is a pointer to the record from which you must generate the
key. It is assumed that you will know the record length. Key is an
untyped var where you must store the generated key, and KeyLen is the
length of the key to be generated.

The second routine is the Compare Key routine. It must be of the
TffKeyCompareFunc type:

  TffKeyCompareFunc = function (const Key1, Key2;
                                aData : PffCompareData) : integer;

Key1 and Key2 are untyped const variables holding the keys to be
compared. The object of the routine is to compare the two keys and
return an integer value depending on the collating sequence of the
keys (a negative number means 'less than', zero means 'equal' and
a positive number means 'greater than'). aData is a pointer to a
special record holding information about the comparison to be done.

  PffCompareData = ^TffCompareData;
  TffCompareData = packed record
    cdKeyLen  : longint;
    cdDict    : pointer;
    cdIndex   : longint;
    cdFldCnt  : longint;
    cdPartLen : longint;
    cdAscend  : boolean;
    cdNoCase  : boolean;
  end;

The cdKeyLen field is the length of the key in bytes. cdDict is a
field for internal purposes: it is not guaranteed to point to anything
when your routine gets called. cdIndex is the number of the index
where the keys reside. cdFldCnt will either be 0 or 1: 0 means that
the routine must perform a 'partial' comparison and cdPartLen is the
number of bytes to compare of the two keys. If cdFldCnt is 1, the
comparison you make must be done over the whole of the keys. cdAscend
determines whether the index is in ascending order or not, and hence
affects the result that you return. If cdAscend is true then you must
return a value <0 if Key1 is less than Key2, 0 if they are equal and
>0 otherwise. If it is false you must reverse the sign of the result
(ie, return <0 if Key1 > Key2, 0 if equal, >0 otherwise. cdNoCase
defines whether case-insensitivity is an issue with the compare: if
true the keys must be compared in a case-insensitive manner, if false
in a case-sensitive manner. Obviously cdNoCase only has meaning for
character or string fields.

In 16-bit Delphi the routines you write will have to be exported from
the DLL and must therefore (a) be marked with the export keyword and
(b) be explicitly exported via the exports clause.

In 32-bit Delphi the routines you write must be marked with the
stdcall directive. The fastcall type (and others) is not available.

Once you have created the DLL, exporting these two routines, you can
then configure the server (via the configuration dialog) to go and use
this DLL whenever it needs to index records belonging to a particular
database alias, table and index.

=====================================================================}

Unit fssrintf;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase;

Type
  PffCompareData = ^TffCompareData;
  TffCompareData = Packed Record {Data for comparison operations}
    cdKeyLen: Longint; {..max length of key to compare}
    cdDict: pointer; {..dictionary (to be typecast)}
    cdIndex: Longint; {..index number}
    cdFldCnt: Longint; {..field count (partial searches)}
    cdPartLen: Longint; {..partial length (partial searches)}
    cdAscend: boolean; {..true if keys are to be compared in ascending order}
    cdNoCase: boolean; {..true if keys are to be case-insensitive compared}
  End;

Type
  TffKeyCompareFunc = Function(Const Key1, Key2; aData: PffCompareData): Integer
    Stdcall;
  {-Type of the key comparison routine for building an index.
    Returns negative value if Key1 < Key2, 0 if equal, positive
    value otherwise, aData defines the comparison criteria}

  TffKeyBuildFunc = Function(Index: Integer;
    DataRec: PffByteArray;
    Var Key;
    KeyLen: Integer): boolean
    Stdcall;
  {-Type of the generation routine for generating a key.
    Returns true if a key was generated. KeyLen defines the number
    of bytes required in the generated key}

{---Useful Key Comparison routines---}
Function FFKeyCompareLongint(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as longints, compare}
Function FFKeyCompareBytes(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
// not yet
{-Treat Key1 and Key2 as longints, compare}
Function FFKeyCompareInt(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as longints, compare}
Function FFKeyCompareWord(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as longints, compare}
Function FFKeyCompareDouble(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as array of bytes, compare}
Function FFKeyCompareStr(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as short strings, compare}
Function FFKeyCompareStrZ(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as null-terminated strings, compare}
Function FFKeyCompareAnsiStr(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as ANSI short strings, compare}
Function FFKeyCompareAnsiStrZ(Const Key1, Key2; aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as ANSI null-terminated strings, compare}
Function FFKeyCompareWideChar(Const Key1, Key2; aData: PffCompareData): Integer; stdcall;
{-Treat Key1 and Key2 as wide UNICODE characters, compare}
Function FFKeyCompareWideStr(Const Key1, Key2; aData: PffCompareData): Integer; stdcall;
{-Treat Key1 and Key2 as wide UNICODE null-terminated strings, compare}

Function FFKeyCompareDWord(Const Key1, Key2;
  aData: PffCompareData): Integer;
stdcall;
{-Treat Key1 and Key2 as DWord, compare}

Function FFKeyCompareI64(Const Key1, Key2;
  aData: PffCompareData): Integer; stdcall;
{-Treat Key1 and Key2 as TffInt64, compare}
Implementation

{===Key Comparison routines==========================================}

Function FFKeyCompareDWord(Const Key1, Key2;
  aData: PffCompareData): Integer;
Begin
  Result := FFCheckDescend(aData^.cdAscend, FFCmpDW(DWord(Key1),
    DWord(Key2)));
End;
{--------}

Function FFKeyCompareI64(Const Key1, Key2;
  aData: PffCompareData): Integer;
Begin
  Result := FFCheckDescend(aData^.cdAscend, FFCmpI64(TffInt64(Key1),
    TffInt64(Key2)));
End;
{--------}

Function FFKeyCompareLongint(Const Key1, Key2; aData: PffCompareData): Integer;
Begin
  Result := FFCheckDescend(aData^.cdAscend, FFCmpI32(Longint(Key1), Longint(Key2)));
End;
{--------}

Function FFKeyCompareBytes(Const Key1, Key2; aData: PffCompareData): Integer;
Begin
  With aData^ Do
    Result := FFCheckDescend(cdAscend,
      FFCmpBytes(@Key1, @Key2, FFForceNonZero(cdPartLen, cdKeyLen)));
End;

Function FFKeyCompareWord(Const Key1, Key2; aData: PffCompareData): Integer;
Begin
  With aData^ Do
    Result := FFCheckDescend(cdAscend,
      FFCmpBytes(@Key1, @Key2, FFForceNonZero(cdPartLen, cdKeyLen)));
End;

Function FFKeyCompareInt(Const Key1, Key2; aData: PffCompareData): Integer;
Begin
  With aData^ Do
    Result := FFCheckDescend(cdAscend,
      FFCmpBytes(@Key1, @Key2, FFForceNonZero(cdPartLen, cdKeyLen)));
End;

Function FFKeyCompareDouble(Const Key1, Key2; aData: PffCompareData): Integer;
Begin
  With aData^ Do
    Result := FFCheckDescend(cdAscend,
      FFCmpBytes(@Key1, @Key2, FFForceNonZero(cdPartLen, cdKeyLen)));
End;
{--------}

Function FFKeyCompareStr(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  S1: TffShStr Absolute Key1;
  S2: TffShStr Absolute Key2;
Begin
  With aData^ Do
    If cdNoCase Then
      Result := FFCheckDescend(cdAscend, FFCmpShStrUC(S1, S2, FFForceNonZero(cdPartLen, cdKeyLen)))
    Else
      Result := FFCheckDescend(cdAscend, FFCmpShStr(S1, S2, FFForceNonZero(cdPartLen, cdKeyLen)));
End;
{--------}

Function FFKeyCompareStrZ(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  S1: Array[0..pred(fscl_MaxKeyLength)] Of AnsiChar Absolute Key1;
  S2: Array[0..pred(fscl_MaxKeyLength)] Of AnsiChar Absolute Key2;
Begin
  With aData^ Do
    If cdNoCase Then
      Result := FFCheckDescend(cdAscend,
        SysUtils.StrLIComp(S1, S2, FFForceNonZero(cdPartLen, cdKeyLen)))
    Else
      Result := FFCheckDescend(cdAscend,
        SysUtils.StrLComp(S1, S2, FFForceNonZero(cdPartLen, cdKeyLen)));
End;
{--------}

Function FFKeyCompareAnsiStr(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  S1: String[255]Absolute Key1;
  S2: String[255]Absolute Key2;
Begin
  With aData^ Do
    If cdNoCase Then
      Result := FFCheckDescend(cdAscend,
        Windows.CompareStringA(LOCALE_USER_DEFAULT,
        NORM_IGNORECASE + SORT_STRINGSORT,
        PAnsiChar(@S1[1]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), length(S1)),
        PAnsiChar(@S2[1]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), length(S2))) - 2)
    Else
      Result := FFCheckDescend(cdAscend,
        Windows.CompareStringA(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
        PAnsiChar(@S1[1]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), length(S1)),
        PAnsiChar(@S2[1]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), length(S2))) - 2);
End;
{--------}

Function FFKeyCompareAnsiStrZ(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  S1: Array[0..pred(fscl_MaxKeyLength)] Of AnsiChar Absolute Key1;
  S2: Array[0..pred(fscl_MaxKeyLength)] Of AnsiChar Absolute Key2;
Begin
  With aData^ Do
    If cdNoCase Then
      Result := FFCheckDescend(cdAscend,
        Windows.CompareStringA(LOCALE_USER_DEFAULT,
        NORM_IGNORECASE + SORT_STRINGSORT,
        PAnsiChar(@S1[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), StrLen(S1)),
        PAnsiChar(@S2[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), StrLen(S2))) - 2)
    Else
      Result := FFCheckDescend(cdAscend,
        Windows.CompareStringA(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
        PAnsiChar(@S1[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), StrLen(S1)),
        PAnsiChar(@S2[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), StrLen(S2))) - 2);
End;
{--------}

Function FFKeyCompareWideChar(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  S1: WideChar Absolute Key1;
  S2: WideChar Absolute Key2;
Begin
  If aData^.cdNoCase Then
    Result := FFCheckDescend(aData^.cdAscend,
      Windows.CompareStringW
      (LOCALE_USER_DEFAULT,
      NORM_IGNORECASE + SORT_STRINGSORT,
      PWideChar(@S1), 1,
      PWideChar(@S2), 1) - 2)
  Else
    Result := FFCheckDescend(aData^.cdAscend,
      Windows.CompareStringW
      (LOCALE_USER_DEFAULT, SORT_STRINGSORT,
      PWideChar(@S1), 1,
      PWideChar(@S2), 1) - 2);
End;
{--------}

Function FFKeyCompareWideStr(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  S1: Array[0..pred(fscl_MaxKeyLength Div 2)] Of WideChar Absolute Key1;
  S2: Array[0..pred(fscl_MaxKeyLength Div 2)] Of WideChar Absolute Key2;
Begin
  With aData^ Do
    If cdNoCase Then
      Result := FFCheckDescend(
        cdAscend,
        Windows.CompareStringW(LOCALE_USER_DEFAULT,
        NORM_IGNORECASE + SORT_STRINGSORT,
        PWideChar(@S1[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), lstrlenW(S1)),
        PWideChar(@S2[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), lstrlenW(S2))) - 2)
    Else
      Result := FFCheckDescend(
        cdAscend,
        Windows.CompareStringW(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
        PWideChar(@S1[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), lstrlenW(S1)),
        PWideChar(@S2[0]),
        FFMinI(FFForceNonZero(cdPartLen, cdKeyLen), lstrlenW(S2))) - 2);
End;
{====================================================================}

End.

