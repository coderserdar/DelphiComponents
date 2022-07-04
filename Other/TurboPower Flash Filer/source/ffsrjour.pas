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

{$I ffdefine.Inc}

unit ffsrjour;

interface

uses
  ffllbase,
  ffsrbase,
  fflleng,                                                             {!!.13}
  uFFSRJrn;                                                            {!!.13}

type
  TffRecoveryClass = class of TffBaseRecoveryEngine;

  TffBaseRecoveryEngine = class(TffObject)
  protected
  public
    procedure Check(anEngine : TffBaseServerEngine); virtual; abstract;
      { Use this method to check each database for failsafe transactions
        that were written to disk but not fully applied to the table(s).}
  end;

  TffRecoveryEngine = class(TffBaseRecoveryEngine)
  protected
    procedure reWritePages(aJnlFile : PffFileInfo; Commit : Boolean);
{Begin !!.13}
    function reReportJournalState( JournalState : TJournalState;
                                    Alias, Path, FileName,
                                    ExceptionString : string): Boolean; virtual;
{End !!.13}
  public
    procedure Check(anEngine : TffBaseServerEngine); override;
      { Use this method to check each database for failsafe transactions
        that were written to disk but not fully applied to the table(s).

        Requirements: The server engine must have already established its list
          of aliases.  The recovery engine scans through the list of aliases. }
  end;

var
  FFRecoveryClass : TffRecoveryClass = TffRecoveryEngine;

implementation

uses
  classes,
  controls,
  dialogs,
  forms,
  sysutils,
  ffconst,
  ffllexcp,
  ffsrbde,
  ffsreng,
  ffsrcfg,
  ffsrcvex;                                                            {!!.13}

{===TffRecoveryEngine================================================}
procedure TffRecoveryEngine.Check(anEngine : TffBaseServerEngine);
var
  aAliasItem : PffAliasDescriptor;
  aClientID : TffClientID;
  aHash : TffWord32;
  aList : TList;
  aJnlFile : PffFileInfo;
  aResult : TffResult;
  Commit : Boolean;
  FindRes : Integer;
  Hdr : TffJournalFileHeader;
  iAlias : Integer;
  iJournal : Integer;
  JournalList : TffStringList;
  SearchMask : TffPath;
  SearchRec : TffSearchRec;
begin
  JournalList := TffStringList.Create;
  aList := TList.Create;
  try
    { Add a client session to the server engine. }
    aResult := anEngine.ClientAdd(aClientID, '', ffc_AdminUserID, 1000, aHash);
    try
      if aResult <> DBIERR_NONE then
        FFRaiseExceptionNoData(EffException, ffStrResServer, aResult);
          { Note: Information will have already been logged. We are simply
              raising the exception to make sure that somebody notices. }

      { Get the list of aliases from the server engine. }
      aResult := anEngine.RecoveryAliasList(aList, aClientID);
      if aResult <> DBIERR_NONE then
        FFRaiseExceptionNoData(EffException, ffStrResServer, aResult);
          { Note: Information will have already been logged. We are simply
              raising the exception to make sure that somebody notices. }

      for iAlias := 0 to pred(aList.Count) do begin

        { Get the alias descriptor. }
        aAliasItem := aList.Items[iAlias];

        { Alias name is in aAliasItem^.adAlias, path is in aAliasItem^.adPath.
          We could have multiple journal files to process. }

        { Empty the journal list. }
        JournalList.Empty;

        { Now, search the alias path for any journal files (*.FF$). }
        SearchMask := aAliasItem^.adPath;
        if (SearchMask[length(SearchMask)] <> '\') then
          FFShStrAddChar(SearchMask, '\');
        FFShStrConcat(SearchMask, '*.');
        FFShStrConcat(SearchMask, ffc_ExtForTrans);
        FindRes := FFFindFirst(SearchMask, [ditFile], diaAnyAttr,
                                SearchRec);
        while (FindRes = 0) do begin
          JournalList.Insert(SearchRec.srName);
          FindRes := FFFindNext(SearchRec);
        end;
        FFFindClose(SearchRec);

        for iJournal := 0 to pred(JournalList.Count) do begin
          try
            {allocate the file info}
            aJnlFile := FFAllocFileInfo(
               FFMakeFullFileName(aAliasItem^.adPath,
                                   ExtractFileName(JournalList[iJournal])),
               FFExtractExtension(JournalList[iJournal]),
               nil);
            try
              FFOpenFile(aJnlFile, omReadOnly, smExclusive, False, False);
              try
                FFReadFileExact(aJnlFile, sizeof(Hdr), Hdr);
                if (Hdr.jfhSignature = ffc_SigJnlHeader) then begin
                  if (Hdr.jfhState = 0) then begin
                    {incomplete header - show message and then delete the file}
{Begin !!.13}
                    reReportJournalState(jsIncomplete,
                                         aAliasItem^.adAlias,
                                         aAliasItem^.adPath,
                                         JournalList[iJournal],
                                         '');
{End !!.13}
                  end
                  else begin
                    {complete header}
                    try
{Begin !!.13}

                      Commit := reReportJournalState(jsComplete,
                                                     aAliasItem^.adAlias,
                                                     aAliasItem^.adPath,
                                                     JournalList[iJournal],
                                                     '');
{End !!.13}
                      reWritePages(aJnlFile, Commit);
                    except
                      on E : Exception do begin
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
                      end;
                    end;
                  end;
                end
                else
                  raise Exception.CreateResFmt(fferrNotAnFFFile,
                                                [JournalList[iJournal]]);
              finally
                FFCloseFile(aJnlFile);
              end;
            finally
              FFFreeFileInfo(aJnlFile);
            end;
            FFDeleteFile(FFMakeFullFileName(aAliasItem^.adPath,
                                              JournalList[iJournal]));
          except
            on E: Exception do begin
              {show a message, but don't stop processing...}
{Begin !!.13}
              reReportJournalState(jsSkipping,
                                   aAliasItem^.adAlias,
                                   aAliasItem^.adPath,
                                   JournalList[iJournal],
                                   E.Message);
{End !!.13}
            end;
          end; {try..except}
        end; {for iJournal}
      end; {for iAlias}
    finally
      anEngine.ClientRemove(aClientID);
    end;
  finally
    for iAlias := pred(aList.Count) downto 0 do begin
      aAliasItem := PffAliasDescriptor(aList.items[iAlias]);
      FFFreeMem(aAliasItem, sizeOf(TffAliasDescriptor));
    end;
    aList.Free;
    JournalList.Free;
  end; {try..finally}
end;
{Begin !!.13}
{--------}
function TffRecoveryEngine.reReportJournalState(
  JournalState: TJournalState; Alias, Path, Filename,
  ExceptionString: String): Boolean;
begin
  Result := ShowJournalForm(JournalState,
                            Alias,
                            Path,
                            Filename,
                            ExceptionString ) = mrOK;
end;
{End !!.13}
{--------}
procedure TffRecoveryEngine.reWritePages(aJnlFile : PffFileInfo;
                                         Commit : Boolean);
var
  JFRH : TffJournalFileRecordHeader;
  Block : Pointer;
  After : Boolean;
  TargetFile : PffFileInfo;
  tfName : String;
  FileSize : TffInt64;
  FFHeader : array [0..4] of longint;
  TempI64  : TffInt64;
begin
  FileSize := FFGetFileSize(aJnlFile);
  FFGetZeroMem(Block, ffcl_64k);
  try
    {as long as we're not at EOF, }
    while (ffCmpI64(FFGetPositionFile(aJnlFile), FileSize) <> 0) do begin
      {get a record header from the journal file}
      FFReadFileExact(aJnlFile, sizeof(JFRH), JFRH);
      {read a page into BLOCK}
      FFReadFileExact(aJnlFile, JFRH.jfrhBlockSize, Block^);
      {deal with the page}
      {after images have jfrhBeforeImg = 0}
      After := (JFRH.jfrhBeforeImg = 0);
      if Commit = After then begin
        {Writes after images on commit, before images on rollback}
        {allocate the file info}
        tfName := StrPas(JFRH.jfrhFileName);
        TargetFile := FFAllocFileInfo(
                      FFMakeFullFileName(FFExtractPath(tfName),
                                          FFExtractFileName(tfName)),
                      FFExtractExtension(tfName),
                      nil);
        try
          FFOpenFile(TargetFile,
                      omReadWrite, smExclusive, true, False);
          try
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
          finally
            FFCloseFile(TargetFile);
          end;{try..finally}
        finally
          FFFreeFileInfo(TargetFile);
        end;
      end;
    end;
  finally
    FreeMem(Block, ffcl_64k);
  end;
end;
{====================================================================}

end.
