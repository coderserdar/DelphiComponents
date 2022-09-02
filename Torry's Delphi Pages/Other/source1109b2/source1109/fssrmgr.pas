{*********************************************************}
{* FlashFiler: String resource manager                   *}
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

{include the resource compiled using BRCC32.EXE and SRMC.EXE}
{$R fssrmgr.res}

Unit fssrmgr;

Interface

Uses
  Windows,
  Classes,
  SysUtils, {!!.03}
  fsllbase; {!!.03}

Const
  DefReportError = False;

  {id at start of binary resource; must match SRMC}
  ResID: Array[0..3] Of char = 'STR0';

Type
  EffStringResourceError = Class(Exception);

  TInt32 = Integer;

  PIndexRec = ^TIndexRec;
  TIndexRec = Record
    id: TInt32;
    ofs: TInt32;
    len: TInt32;
  End;
  TIndexArray = Array[0..(MaxInt Div SizeOf(TIndexRec)) - 2] Of TIndexRec;

  PResourceRec = ^TResourceRec;
  TResourceRec = Record
    id: Array[0..3] Of char;
    Count: Longint;
    Index: TIndexArray;
  End;

  TfsStringResource = Class
  Private
    {property variables}
    FReportError: Boolean; {true to raise exception if string not found}

    {internal variables}
    srHandle: THandle; {handle for TPStrings resource}
    srP: PResourceRec; {pointer to start of resource}
    srPadlock: TfsPadlock; {!!.03}

    {internal methods}
    Procedure srCloseResource;
    Function srFindIdent(Ident: TInt32): PIndexRec;
    Function srGetCount: Longint;
    Procedure srLock;
    Procedure srLoadResource(Instance: THandle; Const ResourceName: String);
    Procedure srOpenResource(Instance: THandle; Const ResourceName: String);
    Procedure srUnLock;

  Public
    Constructor Create(Instance: THandle; Const ResourceName: String); Virtual;
    Destructor Destroy; Override;
    Procedure ChangeResource(Instance: THandle; Const ResourceName: String);

    Function GetAsciiZ(Ident: TInt32; Buffer: PChar; BufChars: Integer): PChar;

    Function GetIdentAtIndex(Const anIndex: Longint): Integer;

    Function GetString(Ident: TInt32): String;
    Function GetStringAtIndex(Const anIndex: Longint): String;

    Property Strings[Ident: TInt32]: String
    Read GetString; Default;
    Function GetWideChar(Ident: TInt32; Buffer: PWideChar; BufChars: Integer): PWideChar;

    Property Count: Longint Read srGetCount;
    {-Returns the number of strings managed by this resource. }

    Property ReportError: Boolean
      Read FReportError
      Write FReportError;
  End;

Var
  ffResStrings: TfsStringResource; {error strings for this unit}

Implementation

{===TfsStringResource================================================}
{*** TfsStringResource ***}

Procedure TfsStringResource.ChangeResource(Instance: THandle; Const ResourceName: String);
Begin
  srCloseResource;
  If ResourceName <> '' Then
    srOpenResource(Instance, ResourceName);
End;
{--------}

Constructor TfsStringResource.Create(Instance: THandle; Const ResourceName: String);
Begin
  Inherited Create;
  srPadlock := TfsPadlock.Create; {!!.03}
  FReportError := DefReportError;
  ChangeResource(Instance, ResourceName);
End;
{--------}

Destructor TfsStringResource.Destroy;
Begin
  srCloseResource;
  srPadlock.Free; {!!.03}
  Inherited Destroy;
End;
{--------}

Procedure WideCopy(Dest, Src: PWideChar; Len: Integer);
Begin
  While Len > 0 Do
    Begin
      Dest^ := Src^;
      inc(Dest);
      inc(Src);
      dec(Len);
    End;
  Dest^ := #0;
End;
{--------}

Function TfsStringResource.GetWideChar(Ident: TInt32;
  Buffer: PWideChar; BufChars: Integer): PWideChar;
Var
  OLen: Integer;
  P: PIndexRec;
Begin
  srLock;
  Try
    P := srFindIdent(Ident);
    If P = Nil Then
      Buffer[0] := #0

    Else
      Begin
        OLen := P^.len;
        If OLen >= BufChars Then
          OLen := BufChars - 1;
        WideCopy(Buffer, PWideChar(PChar(srP) + P^.ofs), OLen);
      End;
  Finally
    srUnLock;
  End;

  Result := Buffer;
End;
{--------}

Function TfsStringResource.GetAsciiZ(Ident: TInt32;
  Buffer: PChar; BufChars: Integer): PChar;
Var
  P: PIndexRec;
  Src: PWideChar;
  Len, OLen: Integer;
Begin
  srLock;
  Try
    P := srFindIdent(Ident);
    If P = Nil Then
      OLen := 0

    Else
      Begin
        Src := PWideChar(PChar(srP) + P^.ofs);
        Len := P^.len;

        {see if entire string fits in Buffer}
        OLen := WideCharToMultiByte(CP_ACP, 0, Src, Len, Nil, 0, Nil, Nil);

        While OLen >= BufChars Do
          Begin
            {reduce length to get what will fit}
            dec(Len);
            OLen := WideCharToMultiByte(CP_ACP, 0, Src, Len, Nil, 0, Nil, Nil);
          End;

        {copy to buffer}
        OLen := WideCharToMultiByte(CP_ACP, 0, Src, Len, Buffer, BufChars, Nil, Nil)
      End;
  Finally
    srUnLock;
  End;

  {null terminate the result}
  Buffer[OLen] := #0;
  Result := Buffer;
End;
{--------}

Function TfsStringResource.GetIdentAtIndex(Const anIndex: Longint): Integer;
Begin
  Result := -1;
  srLock;
  Try
    If anIndex > pred(srP^.Count) Then
      Raise EffStringResourceError.CreateFmt(ffResStrings[6], [anIndex]);
    Result := PIndexRec(@srP^.Index[anIndex])^.id;
  Finally
    srUnLock;
  End;
End;
{--------}

Function TfsStringResource.GetString(Ident: TInt32): String;
Var
  P: PIndexRec;
  Src: PWideChar;
  Len, OLen: Integer;
Begin
  srLock;
  Try
    P := srFindIdent(Ident);
    If P = Nil Then
      Result := ''

    Else
      Begin
        Src := PWideChar(PChar(srP) + P^.ofs);
        Len := P^.len;
        OLen := WideCharToMultiByte(CP_ACP, 0, Src, Len, Nil, 0, Nil, Nil);
        SetLength(Result, OLen);
        WideCharToMultiByte(CP_ACP, 0, Src, Len, PChar(Result), OLen, Nil, Nil);
      End;
  Finally
    srUnLock;
  End;
End;
{--------}

Function TfsStringResource.GetStringAtIndex(Const anIndex: Longint): String;
Var
  P: PIndexRec;
  Src: PWideChar;
  Len, OLen: Integer;
Begin
  srLock;
  Try
    If anIndex > pred(srP^.Count) Then
      Raise EffStringResourceError.CreateFmt(ffResStrings[6], [anIndex]);

    P := @srP^.Index[anIndex];
    If P = Nil Then
      Result := ''

    Else
      Begin
        Src := PWideChar(PChar(srP) + P^.ofs);
        Len := P^.len;
        OLen := WideCharToMultiByte(CP_ACP, 0, Src, Len, Nil, 0, Nil, Nil);
        SetLength(Result, OLen);
        WideCharToMultiByte(CP_ACP, 0, Src, Len, PChar(Result), OLen, Nil, Nil);
      End;
  Finally
    srUnLock;
  End;
End;
{--------}

Procedure TfsStringResource.srCloseResource;
Begin
  While Assigned(srP) Do
    srUnLock;

  If srHandle <> 0 Then
    Begin
      FreeResource(srHandle);
      srHandle := 0;
    End;
End;
{--------}

Function TfsStringResource.srFindIdent(Ident: TInt32): PIndexRec;
Var
  L, R, M: TInt32;
Begin
  Assert(srP <> Nil, 'Lock not obtained on string resource');
  {binary search to find matching index record}
  L := 0;
  R := srP^.Count - 1;
  While L <= R Do
    Begin
      M := (L + R) Shr 1;
      Result := @srP^.Index[M];
      If Ident = Result^.id Then
        Exit;
      If Ident > Result^.id Then
        L := M + 1
      Else
        R := M - 1;
    End;

  {not found}
  Result := Nil;
  If FReportError Then
    Raise EffStringResourceError.CreateFmt(ffResStrings[1], [Ident]);
End;
{--------}

Function TfsStringResource.srGetCount: Longint;
Begin
  srLock;
  Try
    Result := srP^.Count;
  Finally
    srUnlock;
  End;
End;
{--------}

Procedure TfsStringResource.srLock;
Begin
  srPadlock.Lock; {!!.03}
  Try {!!.03}
    srP := LockResource(srHandle);
    If Not Assigned(srP) Then
      Raise EffStringResourceError.Create(ffResStrings[2]);
  Except {!!.03}
    srPadlock.Unlock; {!!.03}
    Raise; {!!.03}
  End; {!!.03}
End;
{--------}

Procedure TfsStringResource.srLoadResource(Instance: THandle; Const ResourceName: String);
Var
  H: THandle;
  Buf: Array[0..255] Of Char;
Begin
  StrPLCopy(Buf, ResourceName, SizeOf(Buf) - 1);
  {$IFDEF UsesCustomDataSet}
  Instance := FindResourceHInstance(Instance);
  {$ENDIF}
  H := FindResource(Instance, Buf, RT_RCDATA);
  If H = 0 Then
    Begin
      Raise EffStringResourceError.CreateFmt(ffResStrings[3], [ResourceName]);
    End
  Else
    Begin
      srHandle := LoadResource(Instance, H);
      If srHandle = 0 Then
        Raise EffStringResourceError.CreateFmt(ffResStrings[4], [ResourceName]);
    End;
End;
{--------}

Procedure TfsStringResource.srOpenResource(Instance: THandle; Const ResourceName: String);
Begin
  {find and load the resource}
  srLoadResource(Instance, ResourceName);

  {confirm it's in the correct format}
  srLock;
  Try
    If srP^.id <> ResId Then
      Raise EffStringResourceError.Create(ffResStrings[5]);
  Finally
    srUnLock;
  End;
End;
{--------}

Procedure TfsStringResource.srUnLock;
Begin
  Try {!!.03}
    If Not UnLockResource(srHandle) Then
      srP := Nil;
  Finally {!!.03}
    srPadlock.Unlock; {!!.03}
  End; {!!.03}
End;
{--------}

Procedure FreeTpsResStrings; Far;
Begin
  ffResStrings.Free;
End;
{====================================================================}

Initialization
  ffResStrings := TfsStringResource.Create(HInstance, 'FSSRMGR_STRINGS');

Finalization
  FreeTpsResStrings;

End.

