{*********************************************************}
{* FlashFiler: TffDataConvertClass used to convert a     *}
{*             FlashFiler 1.xx table to a FlashFiler 2   *}
{*             table.                                    *}
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

unit FFConvrt;

{$I FFDEFINE.INC}

{$IFDEF DCC6OrLater}
!!! Conversion utilities should be compiled only with Delphi 5 or lower, and
!!! C++Builder 5 or lower. Using Delphi 6 or higher, or C++Builder 6 or higher
!!! would lead to an error because the D6 streams are incompatible with streams
!!! from D5 and lower.
{$ENDIF}

interface

uses
  WinTypes, Classes, DB, FFLLDict, FFLLBase, FFLLEng, FFDB, FFLLExcp,
  FFSRMgr;

type
  TffDataConverter = class; {forward declaration}

  { FlashFiler v1.x DLL function types. }
  TFF1TableDataDictionary = procedure(var aDict   : TStream); stdcall;
  TFF1TableFirst = procedure; stdcall;
  TFF1TableNext = procedure; stdcall;
  TFF1TableFieldValue = function(aFieldNo : Integer) : Variant; stdcall;
  TFF1DirOpen = procedure(aPath   : PChar); stdcall;
  TFF1TableOpen = function(aTableName : PChar) : Integer; stdcall;
  TFF1TableClose = procedure; stdcall;
  TFF1TableEOF = function : boolean; stdcall;
  TFF1TableRecordCount = function : Integer; stdcall;
  TFF1IsFileBLOB = function(aFieldNo : Integer;
                        var aBuffer  : array of Byte) : Boolean; stdcall;
  TFF1SetNewMemMgr = function(aMemManager  : TMemoryManager) : TMemoryManager; stdcall;
  TFF1SetOldMemMgr = procedure(aMemMgr : TMemoryManager); stdcall;
  TFF1GetAutoInc = function : Longint; stdcall;


  { TProtOptions is a record that holds settings for all the protocol
    options.}
  TffProtOptions = packed record
    IsSingleUser  : Boolean;
    IsIPXSPX      : Boolean;
    IPXSPXLFB     : Boolean;
    IsTCPIP       : Boolean;
    TCPIPLFB      : Boolean;
    TCPIPPort     : Longint;
    UDPPortSr     : Longint;
    UDPPortCl     : Longint;
    IPXSocketSr   : Longint;
    IPXSocketCl   : Longint;
    SPXSocket     : Longint;
    TCPIntf       : Longint;
  end;

  EffConverterException = class(EffException);

  { Event Types }
  TffDataConverterEvent = procedure(aSender : TffDataConverter) of object;
  { Event type used for status events during the execution of the
    converter}
  TffDCNetBiosEvent = procedure(aSender   : TffDataConverter;
                            var aCanceled : Boolean;
                            var aOptions  : TffProtOptions) of object;
  { Since the NetBIOS protocol isn't supported in FF2, we raise this
    type of event to give the application a chance to change the
    protocol and provide options for the new protocol.}


  {---FF1 to FF2 Converter Class---}

  { This class contains the business logic for converting a FlashFiler 1.x
    file to the FlashFiler 2.0 file format.
    Call the Convert method to convert a file.  The converter opens the source
    file in exclusive mode hence the file may not be opened by a server.
  }
  TffDataConverter = class
  private
    FAfterConvert : TffDataConverterEvent;
      { The method called after successfully completing the Convert Records
        stage. }
    FBeforeConvert : TffDataConverterEvent;
      { The method called before starting the Convert Records stage. }
    FCanceled : Boolean;
      { Flag to stop the conversion process.}
    FClient : TffClient;
      { The FF2 client used for the conversion. }
    FCommitFrequency : TffWord32;
      { The number of records that must be converted before a
        transaction is committed.}
    FDatabase : TffDatabase;
      { The FF2 database used for the conversion. }
    FDLLHandle : THandle;
      { Handle to the FF1 DLL.}
    FFF2Table : TffTable;
      { The new FF2 table.}
    FOnCancel : TffDataConverterEvent;
      { Event called if a conversion is aborted.}
    FOnComplete : TffDataConverterEvent;
      { The method called after all operations are complete on a single
        table.}
    FOnNetBios  : TffDCNetBiosEvent;
      { Since the NetBIOS protocol isn't supported in FF2, we raise
        this event to give the application a chance to change the
        protocol and provide options for the new protocol.}
    FOnProgress : TffDataConverterEvent;
      { The method called during the conversion of records.  It is
        raised after converting the number of records specified by
        ProgressFrequency.  This event is raised at the very end of
        the conversion if less than ProgressFrequency records were
        processed since the last OnProgress event. }
    FProgressFrequency : TffWord32;
      { The number of records that must be converted before the
        OnProgress event may be raised. }
    FBufferSize : TffWord32;
      { How big of a buffer to allow the converter to use. This is
        used to determine how often transactions are committed.}
    FRecordsProcessed : TffWord32;
      { This is the total number of records converted.}
    FServerEngine : TffBaseServerEngine;
      { The FF2 server used for the conversion. }
    FSession : TffSession;
      { The FF2 session used for the conversion. }
    FSource : string;
      { The directory and name of the file being converted. }
    FDestination : string;
      { The directory and name of the new file being created from the old
        file. }
    FTotalRecords : TffWord32;
      { The total number of records in the table that must be converted. }

    procedure FFTableAfterOpen(aDataSet : TDataSet);
      { Used to get access to the FF2 table after it's opened.}
    function  IsFileBLOB(aField : TField; aFieldNo : Integer) : Boolean;
      { Fields that are stored as file BLOBs must be converted in a
        different way than other fields. This function is used to
        check for file-BLOB field types.}
    procedure LoadFF1DLL;
      { Load the FF1 server from a DLL since we can't have a FF1 and
        FF2 server in the same application.}
    procedure ProcessGenInfo(const aFileName : string);
      { The FFSINFO is a FlashFiler system table that can't be handled
        by the standard routine below.  This procedure will convert
        the FFSINFO table correctly.}
    procedure SetBufferSize(aSize : TffWord32);
      { This function is called by the BufferSize property to set the
        buffer size.}

  {==FF1 Routine Types==}
  protected
  public
    constructor Create(aServerEngine : TffBaseServerEngine);
    destructor Destroy; override;

    procedure Cancel;
      { Call this method to abort the conversion process.}
    procedure Convert(const aSource : string;
                      const aDest   : string);
      { Call this method to convert a file in the old format to a file
        in the new format.  This method raises an exception if an error
        occurs.
               aSource - The absolute path to an existing FFD file
                         in the old format. (Ex: c:\MyApp\MyTable.FFD)
               aDest   - The absolute path of the directory to which
                         aSource is being converted to.  If a file
                         exists in aDest with the same filename that
                         is in aSource it will be overwritten.
                         (Ex: c:\MyNewApp) }
    property AfterConvert : TffDataConverterEvent
                                     read FAfterConvert
                                     write FAfterConvert;
      { This event is raised after the record conversion stage has successfully
        finished.  If an error occurs during convert records then this event is
        not raised. }
    property BeforeConvert : TffDataConverterEvent
                                      read FBeforeConvert
                                      write FBeforeConvert;
      { This event is raised before the file is converted.  When this method
        is called, the converter will have opened the file and determined
        how many records need to be converted. }
    property BufferSize : TffWord32
                          read FBufferSize
                          write SetBufferSize
                          default 1024 * 1024;
      { Size of the buffer used by the converter. This number is used
        to determine how often transactions are committed.}
    property Canceled : Boolean read FCanceled;
      { Check if conversion was canceled.}
    property OnCancel : TffDataConverterEvent
                        read FOnCancel
                        write FOnCancel;
      { The event called when a conversion is aborted.}
    property OnComplete : TffDataConverterEvent
                          read FOnComplete
                          write FOnComplete;
      { The method called after all operations are complete on a table.}
    property OnProgress : TffDataConverterEvent
                          read FOnProgress
                          write FOnProgress;
      { This event is raised after converting the number of records
        specified by ProgressFrequency. This event is also raised at
        the end of the conversion if fewer then ProgressFrequency
        records were processed since the last OnProgress event. }
    property OnNetBios : TffDCNetBiosEvent
                         read FOnNetBios
                         write FOnNetBios;
      { Since the NetBIOS protocol isn't supported in FF2, we raise
        this event to give the application a chance to change the
        protocol and provide options for the new protocol.}
    property ProgressFrequency : TffWord32
                                 read FProgressFrequency
                                 write FProgressFrequency default 100;
      { The number of records that must be converted before the
        OnProgress event will be raised. }
    property RecordsProcessed : TffWord32 read FRecordsProcessed;
      { The number of records converted. This number is accurate at
        the time OnProgress is raised. }
    property Source : string read FSource;
      { The directory and name of the file being converted. }
    property Destination : string read FDestination;
      { The drive and path of the location to place the new FF2 tables.}
    property TotalRecords : TffWord32 read FTotalRecords;
      { The total number of records to be processed in the Convert Records
        stage. }
    property ServerEngine : TffBaseServerEngine read FServerEngine;
      { The FF2 server engine used to make the new (converted) table.}
  end;

implementation

uses
  SysUtils,
  Dialogs,
  Winsock,
  {$IFDEF DCC6OrLater}                                                 {!!.06 - Start}
  Variants,
  {$ENDIF}                                                             {!!.06 - End}
  FFClintf;

const
  ffc_ConvAlias = 'ConvAlias';

var
  ffStrResConverter : TffStringResource;

  { Functions mapped to FF1 DLL}
  FF1DirOpen             : TFF1DirOpen;
  FF1TableClose          : TFF1TableClose;
  FF1TableDataDictionary : TFF1TableDataDictionary;
  FF1TableEOF            : TFF1TableEOF;
  FF1TableFieldValue     : TFF1TableFieldValue;
  FF1TableFirst          : TFF1TableFirst;
  FF1TableNext           : TFF1TableNext;
  FF1TableOpen           : TFF1TableOpen;
  FF1TableRecordCount    : TFF1TableRecordCount;
  FF1IsFileBLOB          : TFF1IsFileBLOB;
  FF1SetNewMemMgr        : TFF1SetNewMemMgr;
  FF1SetOldMemMgr        : TFF1SetOldMemMgr;
  FF1GetAutoInc          : TFF1GetAutoInc;

{$I FFCvCNST.INC}
{$R FFCVCNST.RES}

{===TffDataConverter=================================================}
procedure TffDataConverter.Cancel;
begin
  FCanceled := True;
end;
{--------}
procedure TffDataConverter.Convert(const aSource : string;
                                   const aDest   : string);
var
  FF2Dict       : TffDataDictionary;
  FF1DictStream : TMemoryStream;
  Value         : Variant;
  OldFileName   : AnsiString;
  SourceDir     : AnsiString;
  Msg           : TMsg;
  FieldNumber   : Integer;
  FieldCount    : Integer;
  Data          : Pointer;
begin
  FTotalRecords := 0;
  FRecordsProcessed := 0;
  FSource := aSource;
  OldFileName := ExtractFileName(aSource);
  FDestination := aDest + '\' + ChangeFileExt(OldFileName,             {!!.03}
                                              '.' + ffc_ExtForData);   {!!.03}
  FCanceled := False;

  {setup a FF2 table}
  FFF2Table := TffTable.Create(nil);
  FFF2Table.AfterOpen := FFTableAfterOpen;
  try
    FFF2Table.DatabaseName := FDatabase.DatabaseName;
    FFF2Table.SessionName := FSession.SessionName;
    FFF2Table.Timeout := -1;

    {parse out the directory to the source file(s)}
    SourceDir := ExtractFilePath(aSource);
    {remove the trailing backslash from the directory}
    Delete(SourceDir, Length(SourceDir), 1);
    FF1DirOpen(PChar(SourceDir));
    {extract the FF1 table name and remove its extension}
    Delete(OldFileName, Length(OldFileName) - 3, 4);
    {if we are able to open the FF1 table we'll start the conversion
     process}
    if FF1TableOpen(PChar(OldFileName)) <> 0 then begin
      FFRaiseExceptionNoData(EffConverterException,
                             ffStrResConverter,
                             ffcverrFF1TableOpen)
    end else begin
      {add our alias if we haven't added it already}
      if not FSession.IsAlias(ffc_ConvAlias) then begin
        FSession.AddAlias(ffc_ConvAlias, PChar(aDest), False);         {!!.11}
        FDatabase.AliasName := ffc_ConvAlias;
      end;
      FDatabase.Open;

      FTotalRecords := FF1TableRecordCount;

      { the rest of this routine will not properly convert a FF1
        FFSINFO system table so we'll convert it in a separate procedure}
      if UpperCase(OldFileName) = 'FFSINFO' then begin
        ProcessGenInfo(OldFileName);
        exit;
      end;
      {create a dictionary from the FF1 table that will be used in our
       new FF2 table}
      FF2Dict := TffDataDictionary.Create(4096);
      {read the FF1 dictionary into a stream and then read it into the
       new dictionary}
      FF1DictStream := TMemoryStream.Create;
      FF1TableDataDictionary(TStream(FF1DictStream));
      FF1DictStream.Position := 0;
      FF2Dict.ReadFromStream(FF1DictStream);
      FF2Dict.FileDescriptor[0]^.fdExtension := ffc_ExtForData;

      try
        {create the new table}
        if FFDbiCreateTable(FDatabase, True, OldFileName, FF2Dict) = 0 then begin
          try
            {don't prceed if the conversion has been canceled}
            if not FCanceled then begin
              {execute the BeforeConvert event if assigned}
              if Assigned(FBeforeConvert) then
                FBeforeConvert(self);
              {name and open the new table}
              FFF2Table.TableName := OldFileName;
              FFF2Table.Exclusive := True;
              FFF2Table.Open;
              {now move to the first record in the FF1 table and iterate
               through them - adding each record to the FF2 table, field-
               by-field}
              FF1TableFirst;
              FDatabase.StartTransaction;
              while ((not FF1TableEOF) and (not FCanceled)) do begin
                FFF2Table.Insert;
                {copy the value of each field to the FF2 record we're
                 inserting}
                FieldCount := pred(FFF2Table.FieldCount);
                for FieldNumber := 0 to FieldCount do begin
                  {we have to handle file BLOBs differently than other
                   field types else they will be added to the new table
                   as "normal" BLOBs -- and folks wouldn't like that. The
                   file BLOB process is contained within the call to
                   IsFileBLOB(..) for efficiency.}
                  if (not IsFileBLOB(FFF2Table.Fields[FieldNumber], FieldNumber)) then
                    try                                                {!!.01}
                      if (FFF2Table.Dictionary.FieldType[FieldNumber] <> fftByteArray) then {!!.06 - Start}
                        FFF2Table.Fields[FieldNumber].Value :=
                          FF1TableFieldValue(FieldNumber)
                      else begin
                        Value := FF1TableFieldValue(FieldNumber);
                        if (Value <> NULL) then begin                  {!!.07 - Start}
                          Data := VarArrayLock(Value);
                          try
                            FFF2Table.Fields[FieldNumber].SetData(Data);
                          finally
                            VarArrayUnlock(Value);
                          end;
                        end;                                           {!!.07 - End}
                      end;                                             {!!.06 - End}
                    except                                             {!!.01}
                      FCanceled := False;                              {!!.01}
                      raise;                                           {!!.01}
                    end;                                               {!!.01}
                end; {for}
                {post the new record}
                FFF2Table.Post;
                inc(FRecordsProcessed);
                {move to the next record}
                FF1TableNext;
                {execute the OnProgress event if assigned and we're at one
                 of the progress points}
                if ((Assigned(FOnProgress)) and (FProgressFrequency <> 0) and
                    (FRecordsProcessed mod FProgressFrequency = 0)) then begin
                  FOnProgress(self);
                end;
                if ((FCommitFrequency <> 0) and
                    (FRecordsProcessed mod FCommitFrequency = 0)) then begin
                  try
                    FDatabase.Commit;
                  except
                    {no need to rollback because we're deleting the table}
                    FCanceled := True;
                    raise;
                  end;
                  {process messages: there could have been a Cancel raised.}
                  if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
                    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
                      DispatchMessage(Msg);
                  FDatabase.StartTransaction;
                end;
              end; {while}

              {we have to commit the outstanding transaction even if it
               was canceled}
              try
                if FDatabase.InTransaction then
                  FDatabase.Commit;
                  if FFF2Table.Dictionary.HasAutoIncField(FieldNumber) then
                    FFDbiSetTableAutoIncValue(FFF2Table, FF1GetAutoInc);
              except
                {no need to rollback because we're deleting the table}
                FCanceled := True;
                raise;
              end;

              {only proceed if not canceled}
              if not FCanceled then begin
                {execute the OnProgress event if assigned to ensure we get
                 a final count on the number of records converted}
                if ((Assigned(FOnProgress)) and
                    (FProgressFrequency <> 0) and
                    (FRecordsProcessed mod FProgressFrequency > 0)) then
                  FOnProgress(self);
                {now we need to call the AfterConvert event}
                if Assigned(FAfterConvert) then
                  FAfterConvert(self);
              end; {if not canceled}
            end; {if not canceled}
          finally
            {if an exception was raised during a conversion, it's
             possible to have an open transaction. We need to see if
             there's an open transaction and roll it back if so}
            if FDatabase.InTransaction then
              FDatabase.Rollback;
            FFF2Table.Close;
            FDatabase.Close;
            if not FCanceled then begin
              {we didn't complete the conversion if it was canceled.}
              if Assigned(FOnComplete) then
                FOnComplete(self);
            end else begin
              {if canceled, we raise the Canceled event, delete the
               aborted table, and reset the canceled flag.}
              if Assigned(FOnCancel) then
                FOnCancel(self);
              FDatabase.Open;
              FFF2Table.DeleteTable;
              FFF2Table.Close;
              FDatabase.Close;
              FCanceled := False;
            end; {if..else}
            FFF2Table.Free;
            FFF2Table := nil;                                         {!!.01}
            FSession.DeleteAlias(ffc_ConvAlias);
            FF1TableClose;
            FF1DictStream.Free;
            FF2Dict.Free;
          end; {try..finally}
        end else
          FFRaiseException(EffConverterException, ffStrResConverter,
                           ffcverrFF2TableCreate,
                           [format('Couldn''t create new %s', [FDestination])])
      except
        on E: Exception do
          if E.ClassType <> EffConverterException then
            FFRaiseException(EffConverterException,
                             ffStrResConverter,
                             ffcverrFF2TableCreate,
                             [E.Message])
          else
            raise;
      end;
    end; {if}
  except
    on E: Exception do begin
      FFF2Table.Free;
      if E.ClassType <> EffConverterException then begin
        FFRaiseExceptionNoData(EffConverterException,
                               ffStrResConverter,
                               ffcverrFF1TableOpen)
      end else
        raise;
    end;
  end;
end;
{--------}
constructor TffDataConverter.Create(aServerEngine: TffBaseServerEngine);
begin
  FCanceled := False;
  FServerEngine := aServerEngine;
  LoadFF1DLL;
  BufferSize := 1024 * 1024;
  FCommitFrequency := 1000;
  {setup our client}
  FClient := TffClient.Create(nil);
  FClient.ClientName := 'ConvClient' + IntToStr(GetCurrentThreadID);
  FClient.ServerEngine := aServerEngine;
  {setup our session}
  FSession := TffSession.Create(nil);
  FSession.ClientName := FClient.ClientName;
  FSession.SessionName := 'ConvSess' + IntToStr(GetCurrentThreadID);
  FSession.Open;
  {setup a database}
  FDatabase := TffDatabase.Create(nil);
  FDatabase.SessionName := FSession.SessionName;
  FDatabase.DatabaseName := ffc_ConvAlias;
end;
{--------}
destructor TffDataConverter.Destroy;
begin
  {free the database}
  FDatabase.Free;
  {free the session}
  FSession.Free;
  {free the client}
  FClient.Free;

  if FDLLHandle <> 0 then
    FreeLibrary(FDLLHandle);

  inherited;
end;
{--------}
procedure TffDataConverter.FFTableAfterOpen(aDataSet : TDataSet);
var
  TempFreq : Integer;
begin
  if ((FBufferSize <= 0) or
      (aDataSet = nil)) then
    Exit;
  if aDataSet.Active then begin
    TempFreq := Integer(FBufferSize) div
                TffTable(aDataSet).Dictionary.RecordLength;
{Begin !!.03}
    {ensure we have a min commit freq of 10 records}
    if TempFreq > 10 then begin
      if TffTable(aDataSet).Dictionary.HasBLOBFields then
        FCommitFrequency := 10
      else
        FCommitFrequency := TempFreq;
    end
    else
      FCommitFrequency := 10;
{End !!.03}
  end else
    FCommitFrequency := 1000;
end;
{--------}
function  TffDataConverter.IsFileBLOB(aField   : TField;
                                      aFieldNo : Integer) : Boolean;
var
  FileName : string[255];
  Buffer   : array[0..255] of Byte;
begin
  Result := False;
  if aField is TBLOBField then begin
    Result := FF1IsFileBLOB(aFieldNo, Buffer);
    if Result then begin
      SetLength(FileName, Buffer[0]);
      Move(Buffer[1], FileName[1], Buffer[0]);
      FFDbiAddFileBLOB(FFF2Table, succ(aFieldNo), FileName);
    end;
  end; {if}
end;
{--------}
procedure TffDataConverter.LoadFF1DLL;
var
  Msg, Msg2  : string;
  ErrorMode  : Word;
begin
  { Use setErrorMode to prohibit the Windows error dialog that appears if the
    DLL is not found.  Load the DLL dynamically. }
  ErrorMode := SetErrorMode(SEM_NoOpenFileErrorBox);
  FDllHandle := LoadLibrary('FF1Intfc.DLL');
  SetErrorMode(ErrorMode);

  FDLLHandle := GetModuleHandle('FF1Intfc.DLL');

  if FDllHandle = 0 then
    begin
      Msg := 'Unable to load DLL FF1Intfc. ';
      case GetLastError of
        0 : Msg2 := 'System out of memory, executable corrupt, ' +
                    'or relocations invalid.';
        2 : Msg2 := 'File not found.';
        3 : Msg2 := 'Path not found.';
        8 : Msg2 := 'There is insufficient memory to load the DLL.';
       10 : Msg2 := 'The Windows version of the DLL is incorrect.';
       else
         Msg2 := '';
      end;  { case }
      raise Exception.Create(Msg + Msg2 + ' Unable to run conversion.');
    end  { if dll not loaded }
  else begin
    {map our function calls to the FF1 DLL}
    @FF1TableDataDictionary := GetProcAddress(FDLLHandle, 'FF1TableDataDictionary');
    @FF1TableFirst := GetProcAddress(FDLLHandle, 'FF1TableFirst');
    @FF1TableNext := GetProcAddress(FDLLHandle, 'FF1TableNext');
    @FF1TableFieldValue := GetProcAddress(FDLLHandle, 'FF1TableFieldValue');
    @FF1DirOpen := GetProcAddress(FDLLHandle, 'FF1DirOpen');
    @FF1TableOpen := GetProcAddress(FDLLHandle, 'FF1TableOpen');
    @FF1TableClose := GetProcAddress(FDLLHandle, 'FF1TableClose');
    @FF1TableEOF := GetProcAddress(FDLLHandle, 'FF1TableEOF');
    @FF1TableRecordCount := GetProcAddress(FDLLHandle, 'FF1TableRecordCount');
    @FF1IsFileBLOB := GetProcAddress(FDLLHandle, 'FF1IsFileBLOB');
    @FF1SetNewMemMgr := GetProcAddress(FDLLHandle, 'FF1SetNewMemManager');
    @FF1SetOldMemMgr := GetProcAddress(FDLLHandle, 'FF1SetOldMemManager');
    @FF1GetAutoInc := GetProcAddress(FDLLHandle, 'FF1GetAutoInc');
  end;
end;
{--------}
procedure TffDataConverter.ProcessGenInfo(const aFileName : string);
var
  FF1DictStream  : TMemoryStream;
  FF1Dict        : TffDataDictionary;
  FF2Dict        : TffDataDictionary;
  ProtocolString : string;
  NewFileName    : string;
  FieldNumber    : Integer;
  IsNotCanceled  : Boolean;
  SkipProtocols  : Boolean;
  ProtOptions    : TffProtOptions;
begin
  {since some of the earlier FF1 tables don't have all the fields that
   v1.56 has we need FF1's dictionary so we can get its field count.}
  FF1DictStream := TMemoryStream.Create;
  FF1TableDataDictionary(TStream(FF1DictStream));
  FF1Dict := TffDataDictionary.Create(4096);
  FF1DictStream.Position := 0;
  FF1Dict.ReadFromStream(FF1DictStream);
  {we'll build the dictionary to build our new FF2 table}
  FF2Dict := TffDataDictionary.Create(4096);
  with FF2Dict do begin
    AddField('ServerName',    '', fftShortString,
             pred(sizeof(TffNetName)), 0, true, nil);
    AddField('MaxPages',      '', fftWord32,  0, 0, True, nil);
    AddField('IsSecure',      '', fftBoolean, 0, 0, True, nil);
    AddField('AutoUp',        '', fftBoolean, 0, 0, True, nil);
    AddField('AutoMini',      '', fftBoolean, 0, 0, True, nil);
    AddField('DebugLog',      '', fftBoolean, 0, 0, True, nil);
    AddField('UseSingleUser', '', fftBoolean, 0, 0, True, nil);
    AddField('UseIPXSPX',     '', fftBoolean, 0, 0, True, nil);
    AddField('IPXSPXLFB',     '', fftBoolean, 0, 0, True, nil);
    AddField('UseTCPIP',      '', fftBoolean, 0, 0, True, nil);
    AddField('TCPIPLFB',      '', fftBoolean, 0, 0, True, nil);
    AddField('TCPPort',       '', fftInt32,   0, 0, True, nil);
    AddField('UDPPortSr',     '', fftInt32,   0, 0, True, nil);
    AddField('UDPPortCl',     '', fftInt32,   0, 0, True, nil);
    AddField('IPXSocketSr',   '', fftInt32,   0, 0, True, nil);
    AddField('IPXSocketCl',   '', fftInt32,   0, 0, True, nil);
    AddField('SPXSocket',     '', fftInt32,   0, 0, True, nil);
    AddField('UseEncrypt',    '', fftBoolean, 0, 0, True, nil);
    AddField('ReadOnly',      '', fftBoolean, 0, 0, True, nil);
    AddField('LstMsgIntvl',   '', fftInt32,   0, 0, True, nil);
    AddField('KAInterval',    '', fftInt32,   0, 0, True, nil);
    AddField('KARetries',     '', fftInt32,   0, 0, True, nil);
    AddField('Priority',      '', fftInt32,   0, 0, True, nil);
    AddField('TCPInterface',  '', fftInt32,   0, 0, True, nil);
    AddField('NoAutoSaveCfg', '', fftBoolean, 0, 0, True, nil);
    Addfield('TempStoreSize', '', fftInt32,   0, 0, True, nil);
    AddField('CollectEnabld', '', fftBoolean, 0, 0, True, nil);       {!!.01}
    AddField('CollectFreq',   '', fftInt32,   0, 0, True, nil);       {!!.01}
  end;
  {create the new table}
  NewFileName := ExtractFileName(FDestination);
  if FFDbiCreateTable(FDatabase, True, aFileName, FF2Dict) = 0 then begin
    try
      {execute the BeforeConvert event if assigned}
      if Assigned(FBeforeConvert) then
        FBeforeConvert(self);
      {name and open the new table}
      FFF2Table.TableName := NewFileName;
      FFF2Table.Open;
      {now we'll move to the first record in the FF1 table and
       iterate through them - adding each record to the FF2 table}
      FF1TableFirst;

      FFF2Table.Insert;
      {we know the first six fields will match so we'll just copy
       those over to the new table.}
      FFF2Table.Fields[0].Value := FF1TableFieldValue(0);   {ServerName}
      {we are going to assume that all the old RAM pages were for a
       4K block size and then round up to turn the memory used for
       the old RAM pages into megabytes of RAM in the new table.}
      FFF2Table.Fields[1].Value := (((FF1TableFieldValue(1) * 4096) +
                                    pred(1024 * 1024)) {to prevent 0 MB RAM}
                                   div (1024 * 1024));
      for FieldNumber := 2 to 5 do
        FFF2Table.Fields[FieldNumber].Value := FF1TableFieldValue(FieldNumber);
      {setup the protocols}
      SkipProtocols := False;
      ProtocolString := FF1TableFieldValue(6);
      if ProtocolString = '' then begin
        FFF2Table.Fields[6].Value := True;                 {SingleUser}
        FFF2Table.Fields[7].Value := False;                {IPXSPX}
        FFF2Table.Fields[8].Value := False;                {IPXSPXLFB}
        FFF2Table.Fields[9].Value := False;               {TCPIP}
        FFF2Table.Fields[10].Value := False;               {TCPIPLFB}
      end else if ProtocolString = 'TCP/IP' then begin
        FFF2Table.Fields[6].Value := False;
        FFF2Table.Fields[7].Value := False;
        FFF2Table.Fields[8].Value := False;
        FFF2Table.Fields[9].Value := True;
        FFF2Table.Fields[10].Value := FF1TableFieldValue(7);
      end else if ProtocolString = 'IPX/SPX' then begin
        FFF2Table.Fields[6].Value := False;
        FFF2Table.Fields[7].Value := True;
        FFF2Table.Fields[8].Value := FF1TableFieldValue(7);
        FFF2Table.Fields[9].Value := False;
        FFF2Table.Fields[10].Value := False;
      end else if ProtocolString = 'SINGLE' then begin
        FFF2Table.Fields[6].Value := True;
        FFF2Table.Fields[7].Value := False;
        FFF2Table.Fields[8].Value := False;
        FFF2Table.Fields[9].Value := False;
        FFF2Table.Fields[10].Value := False;
      end else if ProtocolString = 'NETBIOS' then begin
        {NetBios has been removed from FF2 so we need to have the
         user select a new protocol before converting the table or
         find a way to have the application select new protocol and
         assign it during the conversion.}
        SkipProtocols := True;
        if Assigned(FOnNetBios) then begin
          {yes. initialize ProtOptions and raise the FOnNetBIOS event
           so the using application can get updated protocol options
           and update ProtOptions.  We will use ProtOptions to
           initialize the protocol options of the table.}
          with ProtOptions do begin
            IsSingleUser  := False;
            IsIPXSPX      := False;
            IPXSPXLFB     := False;
            IsTCPIP       := False;
            TCPIPLFB      := False;
            {FF1 stored the TCPIP port incorrectly, so we'll convert
             it now. We are also changing the defaults in FF2.}
            TCPIPPort     := htons(FF1TableFieldValue(8));
            if TCPIPPort = 24677 then
              TCPIPPort := 25445;
            UDPPortSr     := htons(FF1TableFieldValue(9));
            if UDPPortSr = 24677 then
              UDPPortSr := 25445;
            UDPPortCl     := htons(FF1TableFieldValue(10));
            if UDPPortCl = 24933 then
              UDPPortCl := 25701;
            IPXSocketSr   := htons(FF1TableFieldValue(11));
            if IPXSocketSr = 24677 then
              IPXSocketSr := 25445;
            IPXSocketCl   := htons(FF1TableFieldValue(12));
            if IPXSocketCl = 24933 then
              IPXSocketCl := 25701;
            SPXSocket     := htons(FF1TableFieldValue(13));
            if SPXSocket = 25189 then
              SPXSocket := 25957;
            if FF1Dict.FieldCount > 20 then
              TCPIntf := FF1TableFieldValue(20)
            else
              TCPIntf := 0;

            {now that we've setup the previous protocol options we
             can raise the event with the previous settings}
            FOnNetBIOS(self, IsNotCanceled, ProtOptions);

            {assign the values returned to the appropriate FF2 field}
            FFF2Table.Fields[6].Value := IsSingleUser;
            FFF2Table.Fields[7].Value := IsIPXSPX;
            FFF2Table.Fields[8].Value := IPXSPXLFB;
            FFF2Table.Fields[9].Value := IsTCPIP;
            FFF2Table.Fields[10].Value := TCPIPLFB;
            FFF2Table.Fields[11].Value := TCPIPPort;
            FFF2Table.Fields[12].Value := UDPPortSr;
            FFF2Table.Fields[13].Value := UDPPortCl;
            FFF2Table.Fields[14].Value := IPXSocketSr;
            FFF2Table.Fields[15].Value := IPXSocketCl;
            FFF2Table.Fields[16].Value := SPXSocket;
            FFF2Table.Fields[23].Value := TCPIntf;
          end; {with}
        end else begin
          {if the FOnNetBIOS isn't assigned, setup all protocol
           settings to defaults.}
          FFF2Table.Fields[6].Value := True;
          FFF2Table.Fields[7].Value := False;
          FFF2Table.Fields[8].Value := False;
          FFF2Table.Fields[9].Value := False;
          FFF2Table.Fields[10].Value := False;
          FFF2Table.Fields[11].Value := 25445;
          FFF2Table.Fields[12].Value := 25445;
          FFF2Table.Fields[13].Value := 25701;
          FFF2Table.Fields[14].Value := 25445;
          FFF2Table.Fields[15].Value := 25701;
          FFF2Table.Fields[16].Value := 25957;
          FFF2Table.Fields[23].Value := 0;
        end;
      end;
      {we can match up FF1 fields 8 through 13 with FF2 fields
       12 through 17.  We will skip this section if we've already
       setup the protocols.}
      if not SkipProtocols then begin
        {since FF1 stored the TCP/IP port incorrectly, correct it now}
        FFF2Table.Fields[11].Value := htons(FF1TableFieldValue(8));
        if FFF2Table.Fields[11].Value = 24677 then
          FFF2Table.Fields[11].Value := 25445;
        FFF2Table.Fields[12].Value := htons(FF1TableFieldValue(9));
        if FFF2Table.Fields[12].Value = 24677 then
          FFF2Table.Fields[12].Value := 25445;
        FFF2Table.Fields[13].Value := htons(FF1TableFieldValue(10));
        if FFF2Table.Fields[13].Value = 24933 then
          FFF2Table.Fields[13].Value := 25701;
        FFF2Table.Fields[14].Value   := htons(FF1TableFieldValue(11));
        if FFF2Table.Fields[14].Value = 24677 then
          FFF2Table.Fields[14].Value := 25445;
        FFF2Table.Fields[15].Value := htons(FF1TableFieldValue(12));
        if FFF2Table.Fields[15].Value = 24933 then
          FFF2Table.Fields[15].Value := 25701;
        FFF2Table.Fields[16].Value := htons(FF1TableFieldValue(13));
        if FFF2Table.Fields[16].Value = 25189 then
          FFF2Table.Fields[16].Value := 25957;
      end;
      {we may be able to match up the rest of the FF1 fields, but
       all fields may not be present in all FF1 tables depending on
       what version of FF the tables were created with. We will
       assign default values for any fields not in the FF1 table.}

      {AllowEncrypt?}
      if FF1Dict.FieldCount > 14 then
        FFF2Table.Fields[17].Value := FF1TableFieldValue(14)
      else
        FFF2Table.Fields[17].Value := False;
      {ReadOnly? - Although this is the same name as the old setting
       it a new setting to turn off all output from the server}
      FFF2Table.Fields[18].Value := False;
      if FF1Dict.FieldCount > 16 then begin
        for FieldNumber := 19 to 21 do
          FFF2Table.Fields[FieldNumber].Value :=
            FF1TableFieldValue(FieldNumber - 3);
      end else begin
        {set to defaults if they weren't in the FF1 table}
        FFF2Table.Fields[19].Value := 5000; {LastMsgInterval}
        FFF2Table.Fields[20].Value := 2500; {KAInterval}
        FFF2Table.Fields[21].Value := 5;    {KARetries}
      end;
      if FF1Dict.FieldCount > 19 then
        FFF2Table.Fields[22].Value := FF1TableFieldValue(19)
      else
        {set the priority to "normal" if it wasn't in the FF1 table}
        FFF2Table.Fields[22].Value := 2;
      {set the default TCP and IPX interfaces}
      if not SkipProtocols then begin
        if FF1Dict.FieldCount > 20 then
          FFF2Table.Fields[23].Value := FF1TableFieldValue(20)
        else
          FFF2Table.Fields[23].Value := 0;
      end;
      {NoAutoSaveCfg - we set this value according to the old
       ReadOnly setting since the functionality matches}
      FFF2Table.Fields[24].Value := FF1TableFieldValue(15);
      {New settings added for FF2 and their defaults}
      FFF2Table.Fields[25].Value := ffcl_TempStorageSize;       {Temp storage size (MB)}
      FFF2Table.Fields[26].Value := True;                       {Garbage collection enabled}
      FFF2Table.Fields[27].Value := ffcl_CollectionFrequency;   {Garbage collection frequency (ms)}
      {post the new record}
      FFF2Table.Post;
      inc(FRecordsProcessed);

      {execute the OnProgress event if assigned and we're at one
       of the progress points}
      if ((Assigned(FOnProgress)) and
          (FRecordsProcessed mod FProgressFrequency = 0)) then
        FOnProgress(self);
      {now we need to call the AfterConvert event}
      if Assigned(FAfterConvert) then
        FAfterConvert(self);
    finally
      FFF2Table.Close;
      FDatabase.Close;                                                {!!.01}       
      if not FCanceled then begin
        {we didn't complete the conversion if it was canceled.}
        if Assigned(FOnComplete) then
          FOnComplete(self);
      end else begin
        {if canceled, we raise the Canceled event, delete the
         aborted table, and reset the canceled flag.}
        if Assigned(FOnCancel) then
          FOnCancel(self);
        FFF2Table.DeleteTable;
        FCanceled := False;
      end; {if..else}
      FFF2Table.Free;
      {FDatabase.Close;}                                              {!!.01 Moved above}
      FSession.DeleteAlias(ffc_ConvAlias);
      FF1TableClose;
      FF2Dict.Free;
      FF1DictStream.Free;
      FF1Dict.Free;
    end;
  end else
    FFRaiseException(EffConverterException, ffStrResConverter,
                     ffcverrFF2TableCreate,
                     [format('Couldn''t create new %s', [FDestination])])
end;
{--------}
procedure TffDataConverter.SetBufferSize(aSize : TffWord32);
begin
  FBufferSize := aSize;
  if aSize <= 0 then
    FFRaiseExceptionNoData(EffConverterException,
                           ffStrResConverter,
                           FFCvErrZeroCommitFreq);
  FFTableAfterOpen(FFF2Table);
end;
{====================================================================}
procedure InitializeUnit;
begin
  ffStrResConverter := nil;
  ffStrResConverter := TffStringResource.Create(hInstance, 'FF_CONVERTER_STRINGS');
end;

procedure FinalizeUnit;
begin
  ffStrResConverter.Free;
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;
{====================================================================}
end.
