{*********************************************************}
{* Journal Transaction Recovery                          *}
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

{$I fsdefine.Inc}

Unit fssrjour;

Interface

Uses
  fsllbase,
  fssrbase,
  fslleng, {!!.13}
  fssrjrn; {!!.13}

Type
  TffRecoveryClass = Class Of TffBaseRecoveryEngine;

  TffBaseRecoveryEngine = Class(TFSSpecObject)
  Protected
  Public
    Procedure Check(anEngine: TFSBaseServerEngine); Virtual; Abstract;
    { Use this method to check each database for failsafe transactions
      that were written to disk but not fully applied to the table(s).}
  End;

  TffRecoveryEngine = Class(TffBaseRecoveryEngine)
  Protected
    Procedure reWritePages(aJnlFile: PffFileInfo; Commit: Boolean);
    {Begin !!.13}
    Function reReportJournalState(JournalState: TJournalState;
      Alias, Path, FileName,
      ExceptionString: String): Boolean; Virtual;
    {End !!.13}
  Public
    Procedure Check(anEngine: TFSBaseServerEngine); Override;
    { Use this method to check each database for failsafe transactions
      that were written to disk but not fully applied to the table(s).

      Requirements: The server engine must have already established its list
        of aliases.  The recovery engine scans through the list of aliases. }
  End;

Var
  FFRecoveryClass: TffRecoveryClass = TffRecoveryEngine;

Implementation

Uses
  Classes,
  Controls,
  Dialogs,
  Forms,
  SysUtils,
  fsconst,
  fsllexcp,
  fssrbde,
  fsserverclass,
  fssrcfg,
  fssrcvex; {!!.13}

{===TffRecoveryEngine================================================}

Procedure TffRecoveryEngine.Check(anEngine: TFSBaseServerEngine);
Var
  aAliasItem: PffAliasDescriptor;
  aClientID: TffClientID;
  aHash: TffWord32;
  aRights: TffUserRights;
  aList: TList;
  aJnlFile: PffFileInfo;
  aResult: TffResult;
  Commit: Boolean;
  FindRes: Integer;
  Hdr: TffJournalFileHeader;
  iAlias: Integer;
  iJournal: Integer;
  JournalList: TFSSpecStringList;
  SearchMask: TffPath;
  SearchRec: TffSearchRec;
  aSecurityEnabled: boolean;
Begin
  JournalList := TFSSpecStringList.Create;
  aList := TList.Create;
  Try
    { Add a client session to the server engine. }
    aResult := anEngine.ClientAdd(aClientID, '', fsc_AdminUserID, 1000, aHash, aRights,aSecurityEnabled);
    Try
      If aResult <> DBIERR_NONE Then
        FSRaiseExceptionNoData(EfsException, fsStrResServer, aResult);
      { Note: Information will have already been logged. We are simply
          raising the exception to make sure that somebody notices. }

  { Get the list of aliases from the server engine. }
      aResult := anEngine.RecoveryAliasList(aList, aClientID);
      If aResult <> DBIERR_NONE Then
        FSRaiseExceptionNoData(EfsException, fsStrResServer, aResult);
      { Note: Information will have already been logged. We are simply
          raising the exception to make sure that somebody notices. }

      For iAlias := 0 To pred(aList.Count) Do
        Begin

          { Get the alias descriptor. }
          aAliasItem := aList.Items[iAlias];

          { Alias name is in aAliasItem^.adAlias, path is in aAliasItem^.adPath.
            We could have multiple journal files to process. }

          { Empty the journal list. }
          JournalList.Empty;

          { Now, search the alias path for any journal files (*.FF$). }
          SearchMask := aAliasItem^.adPath;
          If (SearchMask[length(SearchMask)] <> '\') Then
            FFShStrAddChar(SearchMask, '\');
          FFShStrConcat(SearchMask, '*.');
          FFShStrConcat(SearchMask, fsc_ExtForTrans);
          FindRes := FFFindFirst(SearchMask, [ditFile], diaAnyAttr,
            SearchRec);
          While (FindRes = 0) Do
            Begin
              JournalList.Insert(SearchRec.srName);
              FindRes := FFFindNext(SearchRec);
            End;
          FFFindClose(SearchRec);

          For iJournal := 0 To pred(JournalList.Count) Do
            Begin
              Try
                {allocate the file info}
                aJnlFile := FFAllocFileInfo(
                  FFMakeFullFileName(aAliasItem^.adPath,
                  ExtractFileName(JournalList[iJournal])),
                  FFExtractExtension(JournalList[iJournal]),
                  Nil);
                Try
                  FFOpenFile(aJnlFile, omReadOnly, smExclusive, False, False);
                  Try
                    FFReadFileExact(aJnlFile, sizeof(Hdr), Hdr);
                    If (Hdr.jfhSignature = fsc_SigJnlHeader) Then
                      Begin
                        If (Hdr.jfhState = 0) Then
                          Begin
                            {incomplete header - show message and then delete the file}
        {Begin !!.13}
                            reReportJournalState(jsIncomplete,
                              aAliasItem^.adAlias,
                              aAliasItem^.adPath,
                              JournalList[iJournal],
                              '');
                            {End !!.13}
                          End
                        Else
                          Begin
                            {complete header}
                            Try
                              {Begin !!.13}

                              Commit := reReportJournalState(jsComplete,
                                aAliasItem^.adAlias,
                                aAliasItem^.adPath,
                                JournalList[iJournal],
                                '');
                              {End !!.13}
                              reWritePages(aJnlFile, Commit);
                            Except
                              On E: Exception Do
                                Begin
                                  {major problem here - found a valid FF Journal file with
                                   a complete header, and then hit an exception trying to
                                   either commit it or rollback}
          {Begin !!.13}
                                  reReportJournalState(jsTrash,
                                    aAliasItem^.adAlias,
                                    aAliasItem^.adPath,
                                    JournalList[iJournal],
                                    E.Message);
                                  {End !!.13}
                                  Application.Terminate;
                                End;
                            End;
                          End;
                      End
                    Else
                      Raise Exception.CreateResFmt(fserrNotAnFFFile,
                        [JournalList[iJournal]]);
                  Finally
                    FFCloseFile(aJnlFile);
                  End;
                Finally
                  FFFreeFileInfo(aJnlFile);
                End;
                FFDeleteFile(FFMakeFullFileName(aAliasItem^.adPath,
                  JournalList[iJournal]));
              Except
                On E: Exception Do
                  Begin
                    {show a message, but don't stop processing...}
      {Begin !!.13}
                    reReportJournalState(jsSkipping,
                      aAliasItem^.adAlias,
                      aAliasItem^.adPath,
                      JournalList[iJournal],
                      E.Message);
                    {End !!.13}
                  End;
              End; {try..except}
            End; {for iJournal}
        End; {for iAlias}
    Finally
      anEngine.ClientRemove(aClientID);
    End;
  Finally
    For iAlias := pred(aList.Count) Downto 0 Do
      Begin
        aAliasItem := PffAliasDescriptor(aList.Items[iAlias]);
        FFFreeMem(aAliasItem, sizeOf(TffAliasDescriptor));
      End;
    aList.Free;
    JournalList.Free;
  End; {try..finally}
End;
{Begin !!.13}
{--------}

Function TffRecoveryEngine.reReportJournalState(
  JournalState: TJournalState; Alias, Path, Filename,
  ExceptionString: String): Boolean;
Begin
  Result := ShowJournalForm(JournalState,
    Alias,
    Path,
    Filename,
    ExceptionString) = mrOK;
End;
{End !!.13}
{--------}

Procedure TffRecoveryEngine.reWritePages(aJnlFile: PffFileInfo;
  Commit: Boolean);
Var
  JFRH: TffJournalFileRecordHeader;
  Block: Pointer;
  After: Boolean;
  TargetFile: PffFileInfo;
  tfName: String;
  FileSize: TffInt64;
  FFHeader: Array[0..4] Of Longint;
  TempI64: TffInt64;
Begin
  FileSize := FFGetFileSize(aJnlFile);
  FFGetZeroMem(Block, fscl_64k);
  Try
    {as long as we're not at EOF, }
    While (ffCmpI64(FFGetPositionFile(aJnlFile), FileSize) <> 0) Do
      Begin
        {get a record header from the journal file}
        FFReadFileExact(aJnlFile, sizeof(JFRH), JFRH);
        {read a page into BLOCK}
        FFReadFileExact(aJnlFile, JFRH.jfrhBlockSize, Block^);
        {deal with the page}
        {after images have jfrhBeforeImg = 0}
        After := (JFRH.jfrhBeforeImg = 0);
        If Commit = After Then
          Begin
            {Writes after images on commit, before images on rollback}
            {allocate the file info}
            tfName := StrPas(JFRH.jfrhFileName);
            TargetFile := FFAllocFileInfo(
              FFMakeFullFileName(FFExtractPath(tfName),
              FFExtractFileName(tfName)),
              FFExtractExtension(tfName),
              Nil);
            Try
              FFOpenFile(TargetFile,
                omReadWrite, smExclusive, True, False);
              Try
                {check to see whether the target file is encrypted or not}
                FFReadFile(TargetFile, sizeof(FFHeader), FFHeader);
                TargetFile^.fiEncrypted := FFHeader[4] = 1;
                {write the data}
                TempI64.iLow := JFRH.jfrhBlockSize;
                TempI64.iHigh := 0;
                ffI64MultInt(TempI64, JFRH.jfrhBlockNumber, TempI64);
                FFWriteEncryptFileExactAt(TargetFile,
                  TempI64,
                  JFRH.jfrhBlockSize,
                  Block^);
              Finally
                FFCloseFile(TargetFile);
              End; {try..finally}
            Finally
              FFFreeFileInfo(TargetFile);
            End;
          End;
      End;
  Finally
    FreeMem(Block, fscl_64k);
  End;
End;
{====================================================================}

End.

