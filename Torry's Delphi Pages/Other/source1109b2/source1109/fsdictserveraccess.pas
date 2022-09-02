{NOTES:
   1. Have verification as optional--IFDEF'd out}

{*********************************************************}
{* FlashFiler: Table data dictionary access (server)     *}
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

Unit fsdictserveraccess;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fssrmgr,
  fsllexcp,
  fslldict,
  fssrintf,
  fssrbase,
  fsfile,
  fsindexhelper,
  fstablehelper,
  fsstreamaccess;

{---Data dictionary class---}
Type
  TFSInfoServerDict = Class(TFSInfoDict)
  Protected {private}
  Protected
  Public
    Procedure ForceOffReadOnly;
    {-Make dictionary writable}
    Procedure ReadFromFile(aFI: PffFileInfo; aTI: PffTransInfo);
    {-Read the data dictionary from the file}
    Procedure WriteToFile(aFI: PffFileInfo; aTI: PffTransInfo);
    {-Write the data dictionary to the file
      Note: the data dictionary can only be written once}
  End;

  {---Compare routine for composite keys---}
Function FSKeyCompareComposite(Const Key1, Key2; aData: PffCompareData): Integer
  Stdcall;
{-Treat Key1 and Key2 as composite keys, compare}

Implementation

Uses
  fssrlock,
  fshash;

{===TFSInfoServerDict================================================}

Procedure TFSInfoServerDict.ForceOffReadOnly;
Begin
  ddReadOnly := False;
End;
{--------}

Procedure TFSInfoServerDict.ReadFromFile(aFI: PffFileInfo; aTI: PffTransInfo);
Var
  FileHeader: PffBlockHeaderFile;
  S: TMemoryStream;
  aRelMethod: TffReleaseMethod;
Begin

  { Get the file header, block 0.  Assume that we only need the lock for the
    duration of this call. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_ReadOnly,
    aRelMethod, fsoNone));
  Try
    { Is there a data dictionary?}
    If (FileHeader^.bhfDataDict = 0) Then
      FSRaiseException(EfsServerException, fsStrResServer, fserrDictMissing,
        [aFI^.fiName^]);
    { Read the data dictionary from the file via a stream}
    S := TMemoryStream.Create;
    Try
      FFTblReadStream(aFI, aTI, FileHeader^.bhfDataDict, S);
      S.Seek(0, soFromBeginning);
      ReadFromStream(S);
      If Self.VersionStream >= 1039 Then
        If Self.VersionStream > FileHeader^.bhfFSVersion Then
          FSRaiseException(EfsException, fsStrResGeneral, fserrDictMissing, [FBaseName]);
    Finally
      S.Free;
    End; {try..finally}
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;

  { Because this method is only called for a pre-existing File group,
    that means we cannot alter it any more. }
  ddReadOnly := True;
End;
{--------}

Procedure TFSInfoServerDict.WriteToFile(aFI: PffFileInfo; aTI: PffTransInfo);
Var
  FileHeader: PffBlockHeaderFile;
  S: TMemoryStream;
  aRelMethod: TffReleaseMethod;

Begin
  { Verify the data dictionary. }
  CheckValid;

  { Get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod, fsoNone));
  Try
    { Write the data dictionary to the file via a stream. }
    S := TMemoryStream.Create;
    Try
      WriteToStream(S);
      FFTblWriteStream(aFI, aTI, FileHeader^.bhfDataDict, S,
        (FileHeader^.bhfDataDict = 0),
        fsc_SigDictStream);
    Finally
      S.Free;
    End; {try..finally}
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;
{====================================================================}

{===Composite Key Compare routine====================================}

Function FSKeyCompareComposite(Const Key1, Key2; aData: PffCompareData): Integer;
Var
  K1: TffByteArray Absolute Key1;
  K2: TffByteArray Absolute Key2;
  IndexDesc: PffIndexDescriptor;
  FieldDesc: PffFieldDescriptor;
  KeyOffset: Integer;
  FieldNumber: Integer;
  CurIndex: Integer;
  CurDict: TFSInfoServerDict;
  CurFldCount: Integer;
  CurPartLen: Integer;
  CurKeyLen: Integer;
  FldCnt: Integer;
  LenToUse, FieldsSize: Integer;
  CurAscend: boolean;
  CurNoCase: boolean;
  Fld1Null: boolean;
  Fld2Null: boolean;
  NullTop: boolean;
Begin
  NullTop := False;

  With aData^ Do
    Begin
      CurIndex := cdIndex;
      CurKeyLen := cdKeyLen;
      CurDict := TFSInfoServerDict(cdDict);
      CurFldCount := cdFldCnt;
      CurPartLen := cdPartLen;
      CurAscend := cdAscend;
      CurNoCase := cdNoCase;
    End;

  Result := 0;
  KeyOffset := 0;
  {get the index descriptor}
  IndexDesc := CurDict.IndexDescriptor^[CurIndex];
  With IndexDesc^ Do
    Begin
      {calculate the number of complete fields we can compare}
      If (CurFldCount = 0) Then
        If (CurPartLen = 0) Then
          FldCnt := idCount
        Else {partial key}
          FldCnt := 0
      Else If (CurPartLen = 0) Then
        FldCnt := FFMinI(CurFldCount, idCount)
      Else {partial key}
        FldCnt := FFMinI(CurFldCount, pred(idCount));

      {compare each field in the key until we get a non-zero (ie not
       equal) result}
      If (FldCnt > 0) Then
        For FieldNumber := 0 To pred(FldCnt) Do
          Begin
            Fld1Null := FSIsKeyFieldNull(@K1, CurKeyLen, idCount, FieldNumber);
            Fld2Null := FSIsKeyFieldNull(@K2, CurKeyLen, idCount, FieldNumber);
            FieldDesc := CurDict.FieldDescriptor^[idFields[FieldNumber]];

            CurAscend := Boolean(idFieldsAscDesc[FieldNumber]);
            CurNoCase := Boolean(idFieldsCase[FieldNumber]);
            NullTop := Boolean(idFieldsNullTop[FieldNumber]);
            FieldsSize := idFieldsSize[FieldNumber];
            If FieldsSize > FieldDesc^.fdLength Then
              FieldsSize := FieldDesc^.fdLength;
            With FieldDesc^ Do
              Begin
                If Fld1Null Then
                  Begin
                    If Fld2Null Then
                      Result := 0
                    Else If NullTop Then
                      Result := -1
                    Else
                      Result := 1;
                  End
                Else {Fld1Null is false}
                  Begin
                    If Fld2Null Then
                      Begin
                        If NullTop Then
                          Result := 1
                        Else
                          Result := -1;
                      End
                    Else
                      Begin
                        If (FieldsSize > 0) And (FieldDesc^.fdType In [fstShortString, fstVarNullString,
                          fstVarWideString, fstNullString, fstWideString {, fstUnicode}]) Then
                          Begin
                            If (FieldDesc^.fdType In [fstWideString, fstVarWideString {, fstUnicode}]) Then
                              LenToUse := sizeof(WideChar) * (FieldsSize)
                            Else If (FieldDesc^.fdType In [fstShortString]) Then
                              LenToUse := FieldsSize + 1
                            Else If (FieldDesc^.fdType In [fstVarNullString, fstNullString]) Then
                              LenToUse := FieldsSize
                            Else
                              LenToUse := -1;
                            Result := FFCheckDescend
                              (CurAscend,
                              CurDict.IndexHelpers[CurIndex, FieldNumber].
                              CompareKey(K1[KeyOffset], K2[KeyOffset], FieldDesc,
                              LenToUse, CurNoCase));
                          End
                        Else
                          Result := FFCheckDescend
                            (CurAscend,
                            CurDict.IndexHelpers[CurIndex, FieldNumber].CompareKey(K1[KeyOffset],
                            K2[KeyOffset], FieldDesc, -1, CurNoCase));
                      End;
                  End;
                If (Result = 0) Then
                  Begin
                    If (FieldsSize > 0) And (FieldDesc^.fdType In [fstShortString, fstVarNullString,
                      fstVarNullString]) Then
                      inc(KeyOffset, FieldsSize + 1)
                    Else If (FieldsSize > 0) And (FieldDesc^.fdType In [fstWideString, fstVarWideString]) Then
                      inc(KeyOffset, sizeof(WideChar) * (FieldsSize))
                    Else
                      inc(KeyOffset, fdLength);
                  End
                Else
                  Break; {out of for loop}
              End;
          End;

      {partially compare the last field if required}
      If (CurPartLen > 0) Then
        Begin
          FieldDesc := CurDict.FieldDescriptor^[idFields[FldCnt]];

          With FieldDesc^ Do
            If (fdType >= fstShortString) Then
              Begin
                Fld1Null := FSIsKeyFieldNull(@K1, CurKeyLen, idCount, FldCnt);
                Fld2Null := FSIsKeyFieldNull(@K2, CurKeyLen, idCount, FldCnt);
                If Fld1Null Then
                  Begin
                    If Fld2Null Then
                      Result := 0
                    Else If NullTop Then
                      Result := -1
                    Else
                      Result := 1;
                  End
                Else {Fld1Null is false}
                  Begin
                    If Fld2Null Then
                      Begin
                        If NullTop Then
                          Result := 1
                        Else
                          Result := -1;
                      End
                    Else
                      Begin
                        FieldsSize := idFieldsSize[FldCnt];
                        If FieldsSize > 0 Then
                          Begin
                            If CurPartLen > FieldsSize Then
                              CurPartLen := FieldsSize;
                          End;
                        If (fdType In [fstWideString, fstVarWideString {, fstUnicode}]) Then
                          LenToUse := sizeof(WideChar) * CurPartLen
                        Else If (fdType = fstShortString) Then
                          LenToUse := CurPartLen + 1
                        Else
                          LenToUse := CurPartLen;
                        Result := FFCheckDescend
                          (CurAscend,
                          CurDict.IndexHelpers[CurIndex, FldCnt].
                          CompareKey(K1[KeyOffset], K2[KeyOffset], FieldDesc,
                          LenToUse, CurNoCase));
                      End;
                  End;
              End;
        End;
    End;
End;
{====================================================================}

End.

