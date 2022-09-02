{$I fsdefine.inc}

Unit fsBackup;
{TFSBackupTable
ver 0.0  31/12/98
ver 0.1  5/2/2000
ver 0.2  24/6/2002
- AutoInc set on restore
- TStringList to TStrings
- Restore uses Dictionary of new table
 & builds a restore buffer(rather than copy)
- replaced fsDBI calls
- added Cancelled property to abort method. anticipate using this
  with engine monitor to abort a copy if changes occur in the database.

 Ian Peck
 RMB M 362
 Balllarat, Australia
 email   ianpeck@netconnect.com.au

 no guarantees! - use at your own peril}
Interface
Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,

  Forms,
  Dialogs,
  Db,
  FsDB,
  FsDBBase,
  FsSRBDE {error codes},
  FsLLDict,
  FsClIntf,
  FsLLBase ;

Type
  TfsCopyingEvent = Procedure(Sender: TObject; TableNo, Percent: Integer) Of Object;
  TfsCopyBeginEvent = Procedure(Sender: TObject; TableNo: Integer;
    TableName: String) Of Object;

  TfsEncryptEvent = Procedure(Sender: TObject; TableNo: Integer;
    TableName: String;
    IsBLOB: boolean;
    {var } aBlock: pointer; aBlockLen: TffWord32) Of Object;

  TFSBackupTable = Class(TFsTable)
  Private
    { Private declarations }
    FBackupDir: String;
    FBufSize: Longint;
    FCancelled: boolean;
    FCompareFailTable: String;
    FCopyToBak2: boolean;
    FStopOnCopyFail: boolean;
    FTableList: TStrings;

    FOnEncrypt: TfsEncryptEvent;
    FOnDecrypt: TfsEncryptEvent;
    FOnTableCopying: TfsCopyingEvent;
    FOnTableCopyBegin: TfsCopyBeginEvent;
    FOnTableComparing: TfsCopyingEvent;
    FOnTableCompareBegin: TfsCopyBeginEvent;
    FOnFileB1ToB2Begin: TfsCopyBeginEvent;
    FOnFileB2ToB1Begin: TfsCopyBeginEvent;
    FOnTableRestoring: TfsCopyingEvent;
    FOnTableRestoreBegin: TfsCopyBeginEvent;

    Procedure SetBufSize(ASize: Integer);
  Protected
    { Protected declarations }
  Public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Function CopyDatabase: boolean;
    Function CompareDatabase: boolean;
    Procedure CopyB1ToB2;
    Procedure CopyB2ToB1;
    Procedure RestoreDatabase;

    Property Cancelled: boolean Read FCancelled Write FCancelled;
    Property CompareFailTable: String Read FCompareFailTable;
  Published
    { Published declarations }
    Property BackupDir: String Read FBackupDir Write FBackupDir;
    Property BufSize: Longint Read FBufSize Write SetBufSize Default 10000;
    Property CopyToBak2: boolean Read FCopyToBak2 Write FCopyToBak2;
    Property StopOnCopyFail: boolean
      Read FStopOnCopyFail Write FStopOnCopyFail Default True;
    Property TableList: TStrings Read FTableList Write FTableList;

    Property OnEncrypt: TfsEncryptEvent Read FOnEncrypt Write FOnEncrypt;
    Property OnDecrypt: TfsEncryptEvent Read FOnDecrypt Write FOnDecrypt;
    Property OnTableCopying: TfsCopyingEvent
      Read FOnTableCopying Write FOnTableCopying;
    Property OnTableCopyBegin: TfsCopyBeginEvent
      Read FOnTableCopyBegin Write FOnTableCopyBegin;
    Property OnTableComparing: TfsCopyingEvent
      Read FOnTableComparing Write FOnTableComparing;
    Property OnTableCompareBegin: TfsCopyBeginEvent
      Read FOnTableCompareBegin Write FOnTableCompareBegin;
    Property OnFileB1ToB2Begin: TfsCopyBeginEvent
      Read FOnTableCompareBegin Write FOnTableCompareBegin;
    Property OnFileB2ToB1Begin: TfsCopyBeginEvent
      Read FOnTableCompareBegin Write FOnTableCompareBegin;
    Property OnTableRestoring: TfsCopyingEvent
      Read FOnTableRestoring Write FOnTableRestoring;
    Property OnTableRestoreBegin: TfsCopyBeginEvent
      Read FOnTableRestoreBegin Write FOnTableRestoreBegin;
  End;

Function CheckTable(Alias, ATableName, aSessionName: String; Var ErrorMsg: String): Longint;
Function CheckAlias(Alias, aSessionName: String): boolean;

Procedure Register;

Implementation
uses fsutil;

{helpers}

Function CheckTable(Alias, ATableName, aSessionName: String; Var ErrorMsg: String): Longint;
Var
  ATable: TFsTable;

Begin
  Result := 0;
  ATable := TFsTable.Create(nil) ;
  With ATable Do
    Begin
      DataBaseName := Alias        ;
      TableName    := ATableName   ;
      SessionName  := aSessionName ;
      Try
        Try {to open it}
          Active := True;
        Except
          {DBIERR_NOSUCHTABLE             =  10024;}
          On E: EfsDatabaseError Do
            Begin
              Result := E.ErrorCode; {only 1 in FF error stack}
              ErrorMsg := E.Message;
            End;
        End;
      Finally
        ATable.Free;
      End;
    End;
End;

Function CheckAlias(Alias, aSessionName: String): boolean;
Var
  Error: Longint;
  ErrorMsg: String;
Begin
  Error := CheckTable(Alias, 'XXXXX' {use any table name}, aSessionName, ErrorMsg);
  If (Error = DBIERR_INVALIDDIR) Or (Alias = '') Then
    Result := False
  Else
    Result := True;
End;

Function BufsEqual(Const M1, M2; Size: Longint): boolean;
Asm
  push edi
  push esi
  push ebx
  mov edi, eax
  mov esi, edx
  mov bx, cx
  mov eax,1 {assume bufs equal}
  cld
  shr ecx,2 {divide by 4}
  jz @@LessThan4
  repe cmpsd
  je @@LessThan4
  mov eax,0{bufs not equal}
  jmp @@Done
@@LessThan4:
  mov cx,bx
  and cx,3 {rest to do}
  jz @@Done {none left}
  repe cmpsb
  je @@Done
  mov eax,0 {not equal}
@@Done:
  pop ebx
  pop esi
  pop edi
End;

Constructor TFSBackupTable.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FBufSize := 10000;
  FTableList := TStringList.Create;
  FStopOnCopyFail := True;
End;

Destructor TFSBackupTable.Destroy;
Begin
  FTableList.Free;
  Inherited Destroy;
End;

Procedure TFSBackupTable.SetBufSize(ASize: Integer);
Begin
  If ASize > 65000 Then
    FBufSize := 65000
  Else
    FBufSize := ASize;
End;

Function TFSBackupTable.CopyDatabase: boolean;
Var
  I, TableNum, NumTables, FldNum, NumFlds: Integer;
  ATableName: String;
  DictStream, BAKStream, BLOBStream: TFileStream;
  BLOBMemStream: TMemoryStream;
  NumRecs, RecNum: Longint;
  HasBLOBField: boolean;
  FType: TfsFieldType;
  Dict: TFSInfoDict;
  RecLen: Integer;
  BufP: PChar;
  BLOBFld: TBLOBField;
  FldSize: Longint;
  pRecBuf, pCurrRec: PByteArray;
  IsEOF: boolean;
  RequestCount, ReturnCount, Error: Longint;
  ErrorMsg: String;
  Percent, PriorPercent: Integer;
Begin
  BLOBMemStream := nil ;
  BLOBStream    := nil ; 

  If Not CheckAlias(Self.DataBaseName, Self.SessionName) Then
    Begin
      Result := False;
      Exit;
    End;
  Result := True;
  NumTables := FTableList.Count;
  If NumTables = 0 Then
    Begin
      Result := False;
      Exit;
    End;
  If Not fsDirectoryExists(FBackupDir) Then fsForceDirectories(FBackupDir);
  FCancelled := False;
  For TableNum := 0 To NumTables - 1 Do
    Begin
      If (Result = False) And FStopOnCopyFail Then Break;
      If FCancelled Then
        Begin
          Result := False;
          Break;
        End;
      ATableName := ExtractFileName(FTableList.Strings[TableNum]);
      Error := CheckTable(Self.DataBaseName, ATableName, Self.SessionName, ErrorMsg);
      If Error <> 0 Then
        Begin
          Result := False;
          Continue; {possible exclusive lock??}
        End;
      Active := False;
      TableName := ATableName;
      IndexName := '';
      Active := True;
      NumRecs := RecordCount;
      If Assigned(FOnTableCopyBegin) Then
        FOnTableCopyBegin(Self, TableNum + 1, ATableName);
      Dict := TFSInfoDict(Dictionary);
      HasBLOBField := False;
      NumFlds := Dict.FieldCount;
      For I := 0 To NumFlds - 1 Do
        Begin
          FType := Dict.FieldType[I];
          If (FType In [fstBLOB..ffcLastBLOBType]) Then
            Begin
              HasBLOBField := True;
              Break;
            End;
        End;
      DictStream := TFileStream.Create(BackupDir + '\' + ATableName + '.DCT',
        fmCreate Or fmShareExclusive);
      BAKStream := TFileStream.Create(BackupDir + '\' + ATableName + '.DT1',
        fmCreate Or fmShareExclusive);
      If HasBLOBField Then
        Begin
          BLOBStream := TFileStream.Create(BackupDir + '\' + ATableName + '.BL1',
            fmCreate Or fmShareExclusive);
          BLOBMemStream := TMemoryStream.Create;
        End;
      Try
        RecLen := Dict.RecordLength;
        Dict.WriteToStream(DictStream);
        RecNum := 0;
        PriorPercent := 0;
        BAKStream.WriteBuffer(NumRecs, sizeof(Longint));
        If HasBLOBField Then
          Begin {process one by one}
            First;
            While Not EOF Do
              Begin
                Application.ProcessMessages;
                If FCancelled Then Break;
                inc(RecNum);
                Percent := (RecNum * 100) Div NumRecs;
                If PriorPercent < Percent Then
                  Begin
                    PriorPercent := Percent;
                    If Assigned(FOnTableCopying) Then
                      FOnTableCopying(Self, TableNum, Percent);
                  End;
                BufP := ActiveBuffer;
                For FldNum := 0 To NumFlds - 1 Do
                  Begin
                    If Dict.FieldType[FldNum] In [fstBLOB..ffcLastBLOBType] Then
                      Begin
                        BLOBFld := TBLOBField(Fields[FldNum]);
                        If BLOBFld.IsNull Then
                          FldSize := 0
                        Else
                          FldSize := Longint(BLOBFld.BLOBSize);
                        BLOBStream.Write(FldSize, sizeof(Longint));
                        If FldSize > 0 Then
                          Begin
                            BLOBMemStream.Clear;
                            BLOBFld.SaveToStream(BLOBMemStream);
                            If Assigned(FOnEncrypt) Then
                              FOnEncrypt(Self, TableNum, ATableName, True {BLOB},
                                BLOBMemStream.Memory, FldSize);
                            BLOBMemStream.Position := 0;
                            BLOBMemStream.SaveToStream(BLOBStream);
                          End; {if FldSize>0}
                      End;
                  End; {for FldNum}
                {MUST encrypt AFTER BLOBS processed!}
                If Assigned(FOnEncrypt) Then
                  FOnEncrypt(Self, TableNum, ATableName, False {BLOB}, BufP, RecLen);
                BAKStream.WriteBuffer(BufP^, RecLen); {save record buffer}
                Next;
              End; {while not EOF}
          End
        Else
          Begin {use Batch method}
            {65000 large buffer is slower then 10000!}
            If FBufSize < RecLen Then FBufSize := RecLen;
            RequestCount := FBufSize Div RecLen;
            GetMem(pRecBuf, RequestCount * RecLen);
            IsEOF := False;
            Try
              InternalFirst;
              While Not IsEOF Do
                Begin
                  Application.ProcessMessages;
                  If NumRecs = 0 Then Break; {while loop}
                  If FCancelled Then Break;
                  Error := GetRecordBatch(RequestCount, ReturnCount, pRecBuf);
                  inc(RecNum, ReturnCount);
                  Percent := (RecNum * 100) Div NumRecs;
                  If PriorPercent < Percent Then
                    Begin
                      PriorPercent := Percent;
                      If Assigned(FOnTableCopying) Then
                        FOnTableCopying(Self, TableNum, Percent);
                    End;
                  If Error = DBIERR_NONE Then
                    Begin
                      If ReturnCount > 0 Then
                        Begin
                          If Assigned(FOnEncrypt) Then
                            FOnEncrypt(Self, TableNum, ATableName, False {BLOB},
                              pRecBuf, ReturnCount * RecLen);
                          BAKStream.WriteBuffer(pRecBuf^, ReturnCount * RecLen);
                        End;
                      If ReturnCount < RequestCount Then IsEOF := True;
                    End;
                End; {while}
            Finally
              FreeMem(pRecBuf, RequestCount * RecLen);
            End;
          End; {use batch}
        {reset NumRecs in case we read different from first RecCount}
        BAKStream.Position := 0;
        BAKStream.WriteBuffer(RecNum, sizeof(Longint));
        If RecNum <> Self.RecordCount Then Result := False;
      Finally
        DictStream.Free;
        BAKStream.Free;
        If HasBLOBField Then
          Begin
            BLOBStream.Free;
            BLOBMemStream.Free;
          End;
      End;
    End; {for TableNum}
  Active := False;
End;

Function TFSBackupTable.CompareDatabase: boolean;
Var
  I, TableNum, NumTables, FldNum, NumFlds: Integer;
  ATableName: String;
  BAKStream, BLOBStream: TFileStream;
  ThisBLOBStream, CopyBLOBStream: TMemoryStream;
  NumRecs, CopyNumRecs, RecNum: Longint;
  HasBLOBField: boolean;
  FType: TfsFieldType;
  Dict: TFSInfoDict;
  RecLen: Integer;
  BufP, CopyBufP: PChar;
  BLOBFld: TBLOBField;
  FldSize, CopyFldSize: Longint;
  pRecBuf, pCopyRecBuf: PByteArray;
  IsEOF: boolean;
  RequestCount, ReturnCount, Error: Longint;
  ErrorMsg: String;
  Percent, PriorPercent: Integer;
Begin
  BLOBStream := nil ;
  
  If Not CheckAlias(Self.DataBaseName, Self.SessionName) Then
    Begin
      Result := False;
      Exit;
    End;
  Result := True; {assume equal}
  FCompareFailTable := '';
  FCancelled := False;
  NumTables := FTableList.Count;
  If NumTables = 0 Then
    Begin
      Result := False;
      Exit;
    End;
  For TableNum := 0 To NumTables - 1 Do
    Begin
      If FCancelled Then
        Begin
          Result := False;
          Break;
        End;
      ATableName := ExtractFileName(FTableList.Strings[TableNum]);
      Error := CheckTable(Self.DataBaseName, ATableName, Self.SessionName, ErrorMsg);
      If Error <> 0 Then
        Begin
          FCompareFailTable := ATableName;
          Result := False; {possible exclusive lock??}
          Break;
        End;
      Active := False;
      TableName := ATableName;
      IndexName := '';
      Active := True;
      NumRecs := RecordCount;
      If Assigned(FOnTableCompareBegin) Then
        FOnTableCompareBegin(Self, TableNum + 1, ATableName);
      Dict := TFSInfoDict(Dictionary);
      HasBLOBField := False;
      NumFlds := Dict.FieldCount;
      For I := 0 To NumFlds - 1 Do
        Begin
          FType := Dict.FieldType[I];
          If (FType In [fstBLOB..ffcLastBLOBType]) Then
            Begin
              HasBLOBField := True;
              Break;
            End;
        End;
      BAKStream := TFileStream.Create(BackupDir + '\' + ATableName + '.DT1',
        fmOpenRead Or fmShareExclusive);
      BAKStream.ReadBuffer(CopyNumRecs, sizeof(Longint));
      If CopyNumRecs <> NumRecs Then
        Begin {no point progressing}
          BAKStream.Free;
          Break;
        End;
      If HasBLOBField Then
        BLOBStream := TFileStream.Create(BackupDir + '\' + ATableName + '.BL1',
          fmOpenRead Or fmShareExclusive);
      Try
        RecLen := Dict.RecordLength;
        RecNum := 0;
        PriorPercent := 0;
        If HasBLOBField Then
          Begin {process one by one}
            GetMem(CopyBufP, RecLen);
            ThisBLOBStream := TMemoryStream.Create;
            CopyBLOBStream := TMemoryStream.Create;
            First;
            Try
              While Not EOF Do
                Begin
                  Application.ProcessMessages;
                  If FCancelled Then Break;
                  inc(RecNum);
                  Percent := (RecNum * 100) Div NumRecs;
                  If PriorPercent < Percent Then
                    Begin
                      PriorPercent := Percent;
                      If Assigned(FOnTableComparing) Then
                        FOnTableComparing(Self, TableNum, Percent);
                    End;
                  BufP := ActiveBuffer;
                  BAKStream.ReadBuffer(CopyBufP^, RecLen); {get copy record buffer}
                  If Assigned(FOnDecrypt) Then
                    FOnDecrypt(Self, TableNum, ATableName, False {BLOB}, CopyBufP, RecLen);
                  Result := BufsEqual(CopyBufP^, BufP^, RecLen);
                  If Result = False Then Break; {while not EOF}
                  For FldNum := 0 To NumFlds - 1 Do
                    Begin
                      If Dict.FieldType[FldNum] In [fstBLOB..ffcLastBLOBType] Then
                        Begin
                          BLOBFld := TBLOBField(Fields[FldNum]);
                          FldSize := BLOBFld.BLOBSize;
                          BLOBStream.Read(CopyFldSize, sizeof(Longint));
                          If FldSize <> CopyFldSize Then
                            Begin
                              Result := False;
                              Break; {for FldNum}
                            End;
                          If FldSize > 0 Then
                            Begin
                              ThisBLOBStream.SetSize(FldSize);
                              CopyBLOBStream.SetSize(FldSize);
                              ThisBLOBStream.Position := 0;
                              CopyBLOBStream.Position := 0;
                              BLOBFld.SaveToStream(ThisBLOBStream);
                              BLOBStream.ReadBuffer(CopyBLOBStream.Memory^, FldSize);
                              If Assigned(FOnDecrypt) Then
                                FOnDecrypt(Self, TableNum, ATableName, True {BLOB},
                                  CopyBLOBStream.Memory, FldSize);
                              Result := BufsEqual(ThisBLOBStream.Memory^,
                                CopyBLOBStream.Memory^, FldSize);
                            End; {if FldSize}
                          If Result = False Then Break; {for FldNum}
                        End;
                    End; {for FldNum}
                  If Result = False Then Break; {while not EOF}
                  Next;
                End; {while not EOF}
            Finally
              FreeMem(CopyBufP, RecLen);
              ThisBLOBStream.Free;
              CopyBLOBStream.Free;
            End;
          End
        Else
          Begin {use Batch method}
            {65000 large buffer is slower then 10000!}
            If FBufSize < RecLen Then FBufSize := RecLen;
            RequestCount := FBufSize Div RecLen;
            GetMem(pRecBuf, RequestCount * RecLen);
            GetMem(pCopyRecBuf, RequestCount * RecLen);
            IsEOF := False;
            Try
              InternalFirst;
              While Not IsEOF Do
                Begin
                  Application.ProcessMessages;
                  If NumRecs = 0 Then Break; {while-nothing to do}
                  If FCancelled Then Break;
                  GetRecordBatch(RequestCount, ReturnCount, pRecBuf);
                  inc(RecNum, ReturnCount);
                  Percent := (RecNum * 100) Div NumRecs;
                  If PriorPercent < Percent Then
                    Begin
                      PriorPercent := Percent;
                      If Assigned(FOnTableComparing) Then
                        FOnTableComparing(Self, TableNum, Percent);
                    End;
                  If Error = DBIERR_NONE Then
                    Begin
                      If ReturnCount > 0 Then
                        Begin
                          BAKStream.ReadBuffer(pCopyRecBuf^, ReturnCount * RecLen);
                          If Assigned(FOnDecrypt) Then
                            FOnDecrypt(Self, TableNum, ATableName, False {BLOB},
                              pCopyRecBuf, ReturnCount * RecLen);
                          Result := BufsEqual(pRecBuf^, pCopyRecBuf^, ReturnCount * RecLen);
                        End;
                      If ReturnCount < RequestCount Then IsEOF := True;
                    End;
                  If Result = False Then Break; {while}
                End; {while}
            Finally
              FreeMem(pRecBuf, RequestCount * RecLen);
              FreeMem(pCopyRecBuf, RequestCount * RecLen);
            End;
          End; {use batch}
      Finally
        BAKStream.Free;
        If HasBLOBField Then
          BLOBStream.Free;
      End;
      If Result = False Then
        Begin
          FCompareFailTable := ATableName;
          Break; {don't proceed if a compare failed}
        End;
    End; {for TableNum}
  Active := False;
End;

Procedure TFSBackupTable.CopyB1ToB2;
Var
  TableNum, NumTables: Integer;
  ATableName: String;
Begin
  NumTables := FTableList.Count;
  If NumTables = 0 Then Exit;
  If Not fsDirectoryExists(FBackupDir) Then fsForceDirectories(FBackupDir);
  For TableNum := 0 To NumTables - 1 Do
    Begin
      Application.ProcessMessages;
      ATableName := ExtractFileName(FTableList.Strings[TableNum]);
      If Assigned(FOnFileB1ToB2Begin) Then
        FOnFileB1ToB2Begin(Self, TableNum + 1, ATableName);
      If FileExists(BackupDir + '\' + ATableName + '.DT1') Then
        CopyFile(PChar(BackupDir + '\' + ATableName + '.DT1'),
          PChar(BackupDir + '\' + ATableName + '.DT2'), False {overwrite});
      If FileExists(BackupDir + '\' + ATableName + '.BL1') Then
        CopyFile(PChar(BackupDir + '\' + ATableName + '.BL1'),
          PChar(BackupDir + '\' + ATableName + '.BL2'), False);

    End;
End;

Procedure TFSBackupTable.CopyB2ToB1;
Var
  TableNum, NumTables: Integer;
  ATableName: String;
Begin
  NumTables := FTableList.Count;
  If NumTables = 0 Then Exit;
  If Not fsDirectoryExists(FBackupDir) Then fsForceDirectories(FBackupDir);
  For TableNum := 0 To NumTables - 1 Do
    Begin
      Application.ProcessMessages;
      ATableName := ExtractFileName(FTableList.Strings[TableNum]);
      If Assigned(FOnFileB2ToB1Begin) Then
        FOnFileB2ToB1Begin(Self, TableNum + 1, ATableName);
      If FileExists(BackupDir + '\' + ATableName + '.DT2') Then
        CopyFile(PChar(BackupDir + '\' + ATableName + '.DT2'),
          PChar(BackupDir + '\' + ATableName + '.DT1'), False {overwrite});
      If FileExists(BackupDir + '\' + ATableName + '.BL2') Then
        CopyFile(PChar(BackupDir + '\' + ATableName + '.BL2'),
          PChar(BackupDir + '\' + ATableName + '.BL1'), False);

    End;
End;

Procedure TFSBackupTable.RestoreDatabase;
Var
  I, TableNum, NumTables, FldNum, NumFlds, AutoFld: Integer;
  ATableName: String;
  DictStream, BAKStream, BLOBStream: TFileStream;
  BLOBMemStream: TMemoryStream;
  RecNum, NumRecs: Longint;
  HasBLOBField: boolean;
  FType: TfsFieldType;
  Dict, RestoreDict: TFSInfoDict;
  RecLen, RestoreRecLen: Integer;
  BufP: PChar;
  BLOBFld: TBLOBField;
  FldSize: Longint;
  pRecBuf, pCurrRec, pRestoreRecBuf: PffByteArray;
  RequestCount, ReturnCount, Error: Longint;
  ErrorMsg: String;
  Percent, PriorPercent: Integer;
  DB: TFsDatabase;
  pErrors: PffLongintArray;
  AutoVal, AutoValHigh, ati: Int64;
  Step: Longint;
  IsNull: boolean;
Begin
  BLOBMemStream := nil ;
  BLOBStream    := nil ;  

  If Not CheckAlias(Self.DataBaseName, Self.SessionName) Then Exit;
  NumTables := FTableList.Count;
  If NumTables = 0 Then Exit;
  If Not fsDirectoryExists(FBackupDir) Then fsForceDirectories(FBackupDir);
  FCancelled := False;
  For TableNum := 0 To NumTables - 1 Do
    Begin
      If FCancelled Then Break;
      ATableName := ExtractFileName(FTableList.Strings[TableNum]);
      DictStream := TFileStream.Create(BackupDir + '\' + ATableName + '.DCT',
        fmOpenRead Or fmShareExclusive);
      Dict := TFSInfoDict.Create(4096);
      Dict.ReadFromStream(DictStream);
      {check tables existence!!!}
      Error := CheckTable(Self.DataBaseName, ATableName, Self.SessionName, ErrorMsg);
      If Error = DBIERR_NOSUCHTABLE Then
        Begin
          DB := TFsDatabase.Create(Nil);
          Try
            DB.AliasName := Self.DataBaseName;
            DB.DataBaseName := 'appdb'; {needed???}
            DB.Connected := True;
            {generates an error if it already exists-}
            If DB.CreateTable({Overwrite} True, ATableName, Dict) <> 0 Then
              Raise Exception.Create('Error creating table ' + ATableName);
          Finally
            DB.Free;
          End;
        End; {need to create new Orig table}
      Active := False;
      TableName := ATableName;
      IndexName := '';
      Exclusive := True;
      Active := True;
      EmptyTable;
      If Assigned(FOnTableRestoreBegin) Then
        FOnTableRestoreBegin(Self, TableNum + 1, ATableName);
      HasBLOBField := False;
      {Must get dict from new table for conversion to account for BLOBs}
      RestoreDict := TFSInfoDict(Dictionary);
      NumFlds := Dict.FieldCount;
      For I := 0 To NumFlds - 1 Do
        Begin
          FType := Dict.FieldType[I];
          If (FType In [fstBLOB..ffcLastBLOBType]) Then
            Begin
              HasBLOBField := True;
              Break;
            End;
        End;
      {determine AutoInc field}
      If Not Dict.HasAutoIncField(AutoFld) Then AutoFld := -1;
      AutoValHigh := 0;
      Step := 1;
      BAKStream := TFileStream.Create(BackupDir + '\' + ATableName + '.DT1',
        fmOpenRead Or fmShareExclusive);
      If HasBLOBField Then
        Begin
          BLOBStream := TFileStream.Create(BackupDir + '\' + ATableName + '.BL1',
            fmOpenRead Or fmShareExclusive);
          BLOBMemStream := TMemoryStream.Create;
        End;
      Try
        RecLen := Dict.RecordLength;
        RestoreRecLen := RestoreDict.RecordLength;
        RecNum := 0;
        BAKStream.ReadBuffer(NumRecs, sizeof(Longint));
        PriorPercent := 0;
        If HasBLOBField Then
          Begin {process one by one}
            pRecBuf := AllocMem(RecLen);
            pRestoreRecBuf := AllocMem(RestoreRecLen);
            Try
              While RecNum < NumRecs Do
                Begin
                  {won't get done if zero recs}
                  Application.ProcessMessages;
                  If FCancelled Then Break;
                  inc(RecNum);
                  Percent := (RecNum * 100) Div NumRecs;
                  If PriorPercent < Percent Then
                    Begin
                      PriorPercent := Percent;
                      If Assigned(FOnTableRestoring) Then
                        FOnTableRestoring(Self, TableNum, Percent);
                    End;
                  BAKStream.ReadBuffer(pRecBuf^, RecLen);
                  If Assigned(FOnDecrypt) Then
                    FOnDecrypt(Self, TableNum, ATableName, False {BLOB},
                      pRecBuf, RecLen);
                  RestoreDict.InitRecord(pRestoreRecBuf);
                  (*
                  {null out BLOB stuff before sending to ActiveBuffer}
                  for FldNum:=0 to NumFlds-1 do begin
                    FType:=Dict.FieldType[FldNum];
                    if (FType in [fstBLOB..ffcLastBLOBType]) then
                      Dict.SetRecordField(FldNum,pRecBuf,nil);
                  end;
                  *)
                  Insert;
                  BufP := ActiveBuffer;
                  If AutoFld > -1 Then
                    Begin
                      Dict.GetRecordField(AutoFld, pRecBuf, IsNull, @AutoVal);
                      If AutoVal > AutoValHigh Then AutoValHigh := AutoVal;
                    End;
                  {create a new Restore buf}
                  For FldNum := 0 To NumFlds - 1 Do
                    Begin
                      FType := Dict.FieldType[FldNum];
                      If (FType In [fstBLOB..ffcLastBLOBType]) Then Continue;
                      {Copy non BLOB fields}
                      RestoreDict.SetRecordField(FldNum, pRestoreRecBuf,
                        @pRecBuf^[Dict.FieldDescriptor[FldNum]^.fdOffset]);
                    End;
                  Move(pRestoreRecBuf^, BufP^, RestoreRecLen);
                  {add any non null BLOBS}
                  For FldNum := 0 To NumFlds - 1 Do
                    Begin
                      FType := Dict.FieldType[FldNum];
                      If (FType In [fstBLOB..ffcLastBLOBType]) Then
                        Begin
                          BLOBFld := TBLOBField(Fields[FldNum]);
                          BLOBStream.ReadBuffer(FldSize, sizeof(Longint));
                          If FldSize > 0 Then
                            Begin
                              BLOBMemStream.SetSize(FldSize);
                              BLOBMemStream.Position := 0;
                              BLOBStream.ReadBuffer(BLOBMemStream.Memory^, FldSize);
                              BLOBMemStream.Position := 0;
                              If Assigned(FOnDecrypt) Then
                                FOnDecrypt(Self, TableNum, ATableName, True {BLOB},
                                  BLOBMemStream.Memory, FldSize);
                              BLOBFld.LoadFromStream(BLOBMemStream);
                            End;
                        End;
                    End;
                  Post;
                End; {while RecNum}
            Finally
              FreeMem(pRecBuf, RecLen);
              FreeMem(pRestoreRecBuf, RestoreRecLen);
            End;
          End
        Else
          Begin {use Batch method}
            {65000 large buffer is slower then 10000!}
            If FBufSize < RecLen Then FBufSize := RecLen;
            RequestCount := FBufSize Div RecLen;
            GetMem(pRecBuf, RequestCount * RecLen);
            pErrors := AllocMem(RequestCount * sizeof(Longint));
            Try
              While RecNum < NumRecs Do
                Begin
                  Application.ProcessMessages;
                  If FCancelled Then Break;
                  If NumRecs - RecNum >= RequestCount Then
                    ReturnCount := RequestCount
                  Else
                    ReturnCount := NumRecs - RecNum;
                  If ReturnCount > 0 Then
                    Begin
                      BAKStream.ReadBuffer(pRecBuf^, ReturnCount * RecLen);
                      If AutoFld > -1 Then
                        Begin
                          {need to run the buffer and dig out  AutoInc}
                          pCurrRec := pRecBuf;
                          For I := 0 To ReturnCount - 1 Do
                            Begin
                              Dict.GetRecordField(AutoFld, pCurrRec, IsNull, @AutoVal);
                              If AutoVal > AutoValHigh Then AutoValHigh := AutoVal;
                              inc(PChar(pCurrRec), RecLen);
                            End;
                        End;
                      If Assigned(FOnDecrypt) Then
                        FOnDecrypt(Self, TableNum, ATableName, False {BLOB},
                          pRecBuf, ReturnCount * RecLen);
                      Error := InsertRecordBatch(ReturnCount, pRecBuf, pErrors);
                      If Error <> DBIERR_NONE Then
                        Begin
                          {?????}
                        End;
                      inc(RecNum, ReturnCount);
                      Percent := (RecNum * 100) Div NumRecs;
                      If PriorPercent < Percent Then
                        Begin
                          PriorPercent := Percent;
                          If Assigned(FOnTableRestoring) Then
                            FOnTableRestoring(Self, TableNum, Percent);
                        End;
                    End; {if ReturnCount>0}
                End; {while}
            Finally
              FreeMem(pRecBuf, RequestCount * RecLen);
              FreeMem(pErrors, RequestCount * sizeof(Longint));
            End;
          End; {use batch}
        {is there an AutoInc to set-need to find highest value from incoming data}
        If AutoFld > -1 Then
          Begin
            GetTableAutoIncValue(Ati, Step);
            SetTableAutoIncValue(AutoValHigh, Step);
          End;
      Finally
        Dict.Free;
        DictStream.Free;
        BAKStream.Free;
        If HasBLOBField Then
          Begin
            BLOBStream.Free;
            BLOBMemStream.Free;
          End;
      End;
    End; {for TableNum}
  Active := False;
  Exclusive := False;
End;

Procedure Register;
Begin
  RegisterComponents('FSSQL Utils', [TFSBackupTable]);
End;

End.

