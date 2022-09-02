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

{$I ffdefine.inc}

unit fftbdict;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  ffsrmgr,
  ffllexcp,
  fflldict,
  ffsrintf,
  ffsrbase,
  fffile,
  ffsrixhl,
  fftbbase,
  fftbstrm;


{---Data dictionary class---}
type
  TffServerDataDict = class(TffDataDictionary)
    protected {private}
    protected
    public
      procedure ForceOffReadOnly;
        {-Make dictionary writable}
      procedure ReadFromFile(aFI : PffFileInfo; aTI : PffTransInfo);
        {-Read the data dictionary from the file}
      procedure WriteToFile(aFI : PffFileInfo; aTI : PffTransInfo);
        {-Write the data dictionary to the file
          Note: the data dictionary can only be written once}
  end;

{---Compare routine for composite keys---}
function FFKeyCompareComposite(const Key1, Key2; aData : PffCompareData) : integer
  stdcall;
  {-Treat Key1 and Key2 as composite keys, compare}


implementation

uses
  ffsrlock;


{===TffServerDataDict================================================}
procedure TffServerDataDict.ForceOffReadOnly;           
begin
  ddReadOnly := false;
end;
{--------}
procedure TffServerDataDict.ReadFromFile(aFI : PffFileInfo; aTI : PffTransInfo);
var
  FileHeader : PffBlockHeaderFile;
  S          : TMemoryStream;
  aRelMethod : TffReleaseMethod;
begin

  { Get the file header, block 0.  Assume that we only need the lock for the
    duration of this call. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_ReadOnly,
                                                aRelMethod));
  try
    { Is there a data dictionary?}
    if (FileHeader^.bhfDataDict = 0) then
      FFRaiseException(EffServerException, ffStrResServer, fferrDictMissing,
                       [aFI^.fiName^]);

    { Read the data dictionary from the file via a stream}
    S := TMemoryStream.Create;
    try
      FFTblReadStream(aFI, aTI, FileHeader^.bhfDataDict, S);
      S.Seek(0, soFromBeginning);
      ReadFromStream(S);
    finally
      S.Free;
    end;{try..finally}
  finally
    aRelMethod(PffBlock(FileHeader));
  end;

  { Because this method is only called for a pre-existing File group,
    that means we cannot alter it any more. }
  ddReadOnly := true;
end;
{--------}
procedure TffServerDataDict.WriteToFile(aFI : PffFileInfo; aTI : PffTransInfo);
var
  FileHeader : PffBlockHeaderFile;
  S          : TMemoryStream;
  aRelMethod : TffReleaseMethod;
begin
  { Verify the data dictionary. }
  CheckValid;

  { Get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));
  try
    { Write the data dictionary to the file via a stream. }
    S := TMemoryStream.Create;
    try
      WriteToStream(S);
      FFTblWriteStream(aFI, aTI, FileHeader^.bhfDataDict, S,
                       (FileHeader^.bhfDataDict = 0),
                       ffc_SigDictStream);
    finally
      S.Free;
    end;{try..finally}
  finally
    aRelMethod(PffBlock(FileHeader));
  end;
end;
{====================================================================}


{===Composite Key Compare routine====================================}
function FFKeyCompareComposite(const Key1, Key2; aData : PffCompareData) : integer;
var
  K1 : TffByteArray absolute Key1;
  K2 : TffByteArray absolute Key2;
  IndexDesc   : PffIndexDescriptor;
  FieldDesc   : PffFieldDescriptor;
  KeyOffset   : integer;
  FieldNumber : integer;
  CurIndex    : integer;
  CurDict     : TffServerDataDict;
  CurFldCount : integer;
  CurPartLen  : integer;
  CurKeyLen   : integer;
  FldCnt      : integer;
  LenToUse    : integer;
  CurAscend   : boolean;
  CurNoCase   : boolean;
  Fld1Null    : boolean;
  Fld2Null    : boolean;
begin
  with aData^ do begin
    CurIndex := cdIndex;
    CurKeyLen := cdKeyLen;
    CurDict := TffServerDataDict(cdDict);
    CurFldCount := cdFldCnt;
    CurPartLen := cdPartLen;
    CurAscend := cdAscend;
    CurNoCase := cdNoCase;
  end;

  Result := 0;
  KeyOffset := 0;
  {get the index descriptor}
  IndexDesc := CurDict.IndexDescriptor^[CurIndex];
  with IndexDesc^ do begin
    {calculate the number of complete fields we can compare}
    if (CurFldCount = 0) then
      if (CurPartLen = 0) then
        FldCnt := idCount
      else {partial key}
        FldCnt := 0
    else
      if (CurPartLen = 0) then
        FldCnt := FFMinI(CurFldCount, idCount)
      else {partial key}
        FldCnt := FFMinI(CurFldCount, pred(idCount));

    {compare each field in the key until we get a non-zero (ie not
     equal) result}
    if (FldCnt > 0) then
      for FieldNumber := 0 to pred(FldCnt) do begin
        Fld1Null := FFIsKeyFieldNull(@K1, CurKeyLen, idCount, FieldNumber);
        Fld2Null := FFIsKeyFieldNull(@K2, CurKeyLen, idCount, FieldNumber);
        FieldDesc := CurDict.FieldDescriptor^[idFields[FieldNumber]];
        with FieldDesc^ do begin
          if Fld1Null then begin
            if Fld2Null then
              Result := 0
            else
              Result := -1;
          end
          else {Fld1Null is false} begin
            if Fld2Null then
              Result := 1
            else
              Result := FFCheckDescend
                          (CurAscend,
                           CurDict.IndexHelpers[CurIndex, FieldNumber].CompareKey(K1[KeyOffset],
                           K2[KeyOffset], FieldDesc, -1, CurNoCase));
          end;
          if (Result = 0) then
            inc(KeyOffset, fdLength)
          else
            Break;{out of for loop}
        end;
      end;

    {partially compare the last field if required}
    if (CurPartLen > 0) then begin
      FieldDesc := CurDict.FieldDescriptor^[idFields[FldCnt]];
      with FieldDesc^ do
        if (fdType >= fftShortString) then begin
          Fld1Null := FFIsKeyFieldNull(@K1, CurKeyLen, idCount, FldCnt);
          Fld2Null := FFIsKeyFieldNull(@K2, CurKeyLen, idCount, FldCnt);
          if Fld1Null then begin
            if Fld2Null then
              Result := 0
            else
              Result := -1;
          end
          else {Fld1Null is false} begin
            if Fld2Null then
              Result := 1
            else begin
              if (fdType = fftWideString) then
                LenToUse := sizeof(WideChar) * CurPartLen
              else if (fdType = fftShortString) or                    
                      (fdType = fftShortAnsiStr) then                 
                LenToUse := CurPartLen + 1                            
              else                                                    
                LenToUse := CurPartLen;
              Result := FFCheckDescend
                          (CurAscend,
                           CurDict.IndexHelpers[CurIndex, FldCnt].
                             CompareKey(K1[KeyOffset], K2[KeyOffset], FieldDesc,
                                        LenToUse, CurNoCase));
            end;
          end;
        end;
    end;
  end;
end;
{====================================================================}

end.
