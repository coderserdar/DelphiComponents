{Error code convert is not completed}
unit CakSqxSdk;

interface

uses
  Windows, Messages, SysUtils, Classes, CakDefs2, CakArchiver, uSQX_Ctrl,uSQX_Errors;

const CanExtract = TRUE;
      CanAdd = TRUE;
      CanList = TRUE;
      CanSFX = FALSE;
      Dllname = 'Sqx.dll';
type
  TCakSqxSdk = class(TCakArchiver)
  private
    { Private declarations }
    Stopping : boolean;

  protected
    { Protected declarations }
  public
    { Public declarations }
    SqxDir: SQX_ARCER_HANDLE; 
    SqxComment: PChar;
    Cakdir : TComponent;
    procedure SetCakdir(pCakdir : TComponent); override;
    procedure Process(dowhat : WorkType); override;
    function Cando(aWork: WorkType): Boolean; override;
    function Def_TreatAs : string; override;
    procedure Load_DLL; override;
    procedure UnLoad_DLL; override;
    function DllExists : Boolean; override;
    procedure DoStop(Stopp: Boolean); override;

    function HandleSqxCallback(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
    //function SqxNewDisk(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
    function SqxReplace(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
    function SqxCryptKey(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
    function SqxProgress(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
    //function SqxCallback(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;

  published
    { Published declarations }
    property Stop: Boolean read Stopping write DoStop;
  end;

procedure Register;

implementation
uses CakUtils2, Cakdir2, Forms;
procedure TCakSqxSdk.SetCakdir(pCakdir : TComponent);
begin
    Cakdir := TCakdir2(pCakdir);         
end;
procedure TCakSqxSdk.DoStop(Stopp: Boolean);
begin
end;
function SqxCallBackProc(lpVoid: pointer; var CallBackStruct: SQX_CALLBACK_STRUCT): SLONG32;
  stdcall;
begin
  Result := TCakSqxSdk(lpVoid).HandleSqxCallback(CallBackStruct);
end;


procedure TCakSqxSdk.Process(dowhat : WorkType);
var
  AFL:      ARC_FILE_LIST;
  FL:       FILE_LIST;
  ExtractSettings: SQX_EXTRACT_SETTINGS;
  CompressSettings: SQX_COMPRESS_SETTINGS;
  SqxInfo:  SQX_INFO_STRUCT;
  AlPtr:    ARC_LIST_NODE_PTR;
  DelphiDT: TDateTime;
  TempList, templist2: TStrings;
  //stDateTimeStr	: string;
  lRes:     Longint;
  //SaveCursor    	: TCursor;
  i, j:     Integer;
  procedure PresetCompressSettings(var CompressSettings: SQX_COMPRESS_SETTINGS);
   begin
    FillChar(CompressSettings, SizeOf(SQX_COMPRESS_SETTINGS), #0);
    CompressSettings.lRetainFolders := OPTION_SET;
    CompressSettings.uStructSize    := SizeOf(SQX_COMPRESS_SETTINGS);
   end;
  procedure PresetExtraxtSettings(var ExtractSettings: SQX_EXTRACT_SETTINGS);
   begin
    FillChar(ExtractSettings, SizeOf(SQX_EXTRACT_SETTINGS), #0);
    ExtractSettings.lCreateDirs := OPTION_SET;
    ExtractSettings.uStructSize := SizeOf(SQX_EXTRACT_SETTINGS);
   end;
  function handleerror(error: Longint): Boolean;
   begin
    Result := False;
    with (Cakdir as TCakdir2) do
    if (error <> SQX_EC_OK) then
     begin
      if Assigned(FOnMsg) then
        FOnMsg(NIL, Error, CODE_UNDEFERROR, Msg_Error, ERR_UNDEFERROR);
     end
    else 
      Result := True;
   end;
  procedure prepareflist;
   begin
    lRes := SqxLoad(SqxDir, GrabProgramPath + 'SQX.dll');
    with (Cakdir as TCakdir2) do
    if Handleerror(lRes) then
     begin
      PresetExtraxtSettings(ExtractSettings);
      SqxInitFileList(SqxDir, FL);
      lRes := SqxAddFileList(SqxDir, '*.*', FL);
      if not Handleerror(lRes) then
        SqxFree(SqxDir)
      else
       begin
        SqxInitArcFileList(SqxDir, AFL);
        if (SqxComment <> NIL) then
         begin
          FreeMem(SqxComment);
          SqxComment := NIL;
         end;
        lRes := SqxListFiles(SqxDir, ArchiveName, FL, AFL,
          SqxInfo, ExtractSettings, SqxCallBackProc, Self);
        if not Handleerror(lRes) then
         begin
          SqxDoneArcFileList(SqxDir, AFL);
          SqxDoneFileList(SqxDir, FL);
          SqxFree(SqxDir);
         end
        else
         begin
          if (SqxInfo.szMainComment <> NIL) then
           begin
            SqxComment := AllocMem(StrLen(SqxInfo.szMainComment) + 1);
            if (SqxComment <> NIL) then
              StrCopy(SqxComment, SqxInfo.szMainComment);
            SqxDoneArcFileList(SqxDir, AFL);
            SqxDoneFileList(SqxDir, FL);
            SqxFree(SqxDir);
           end;
         end;
       end;
     end;
   end;
begin
  Load_DLL;
  with (Cakdir as TCakdir2) do
  case doWhat of
    wtTest: 
     begin
      lRes := SqxLoad(SqxDir, GrabProgramPath + 'SQX.dll');
      HandleError(lRes);
      lRes := SqxTestFiles(SqxDir, ArchiveName, SqxCallBackProc, Self);
      HandleError(lRes);
     end;
    wtDelete: 
     begin
      lRes := SqxLoad(SqxDir, GrabProgramPath + 'SQX.dll');
      SqxInitFileList(SqxDir, FL);
      PresetExtraxtSettings(ExtractSettings);
      for i := 0 to Total_Contents - 1 do
        with Archive_Contents[i] do
          if _Selected or ExtractOptions.Extr_ExtractAll then
            SqxAddFileList(SqxDir, _FileDefPath+_FileName, FL);

      lRes := SqxDeleteFiles(SqxDir, ArchiveName, FL,
        ExtractSettings, SqxCallBackProc, Self);
      HandleError(lRes);
      SqxDoneFileList(SqxDir, FL);
      SqxFree(SqxDir);
     end;
    wtAdd: 
     begin
      lRes := SqxLoad(SqxDir, GrabProgramPath + 'SQX.dll');
      if Handleerror(lRes) then
       begin
        PresetCompressSettings(CompressSettings);
        if AddOptions.Add_DictSize = 32 then
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_32K 
        else if AddOptions.Add_DictSize = 64 then
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_64K 
        else if AddOptions.Add_DictSize = 128 then
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_128K 
        else if AddOptions.Add_DictSize = 256 then
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_256K 
        else if AddOptions.Add_DictSize = 512 then
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_512K 
        else if AddOptions.Add_DictSize = 1024 then
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_1024K 
        else
          CompressSettings.lDictionarySizeIndex := SQX_INDEX_DIC_SIZE_32K;

        CompressSettings.lMethod         := SQX_C1_BEST;
        CompressSettings.lExeCompression := 0;
        CompressSettings.lMultimediaCompression := 0;
        CompressSettings.lSolidFlag      := 0;
        if AddOptions.Add_UseEncrypt then
          StrPCopy(CompressSettings.szFilePassWord, AddOptions.Add_Encrypt);
        if AddOptions.Add_SubDir then
          CompressSettings.lRetainFolders := 1 
        else
          CompressSettings.lRetainFolders := 1;
        CompressSettings.lRecoveryLevel := 0;
        CompressSettings.lSolidFlag     := 0;
        CompressSettings.SfxCommand.lSfxVolumeSize := 0;
        //CompressSettings.szRelPath := 'c:\';
        //if AddOptions.add_relative then
        // StrPCopy(CompressSettings.szRelPath,ExtractfilePath(Archivename));

        SqxInitFileList(SqxDir, FL);

        templist  := TStringList.Create;
        templist2 := TStringList.Create;
        try
          for i := 0 to AddOptions.Add_Files.Count - 1 do
           begin
            templist2 := PollFileList(AddOptions.Add_Files.Strings[i],
              AddOptions.Add_SubDir);
            for j := 0 to templist2.Count - 1 do
              if templist.IndexOf(templist2.Strings[j]) = -1 then
                TempList.Add(templist2.Strings[j]);
           end;

          for i := 0 to templist.Count - 1 do
            SqxAddFileList(SqxDir, templist.Strings[i], FL);

        finally
          templist.Free;
          templist2.Free;
         end;

        lRes := SqxAddFiles(SqxDir, ArchiveName, FL,
          CompressSettings, SqxCallBackProc, Self);
        Handleerror(lRes);
        SqxDoneFileList(SqxDir, FL);
        SqxFree(SqxDir);
       end;
     end;
    wtExtract: 
     begin
      PresetExtraxtSettings(ExtractSettings);
      lRes := SqxLoad(SqxDir, GrabProgramPath + 'SQX.dll');
      if Handleerror(lRes) then
       begin
        StrPCopy(ExtractSettings.szTempDir, GrabTempPath);
        StrPCopy(ExtractSettings.szOutPutPath, ExtractOptions.Extr_to);
                  
        if ExtractOptions.Extr_DirNames then
          ExtractSettings.lCreateDirs := 1 
        else
          ExtractSettings.lCreateDirs := 0;
        ExtractSettings.lKeepBrokenFiles := 1;

        SqxInitFileList(SqxDir, FL);

        for i := 0 to Total_Contents - 1 do
          with Archive_Contents[i] do
            if _Selected or ExtractOptions.Extr_ExtractAll then
              SqxAddFileList(SqxDir, _FileDefPath+_FileName, FL);

        lRes := SqxExtractFiles(SqxDir, ArchiveName, FL,
          ExtractSettings, SqxCallBackProc, Self);
        HandleError(lRes);
        SqxDoneFileList(SqxDir, FL);
        SqxFree(SqxDir);
       end;
     end;

    wtLoadContents: 
     begin
      prepareflist;
      AlPtr := AFL.pHead;
      while (AlPtr <> NIL) do
       begin
        DelphiDT := Now;
        with Archive_Contents[Total_Contents - 1] do
          if ((AlPtr^.lTagged = OPTION_SET) and
            ((AlPtr^.lAttr and FILE_ATTRIBUTE_DIRECTORY) = 0)) then
           begin
            try
              DelphiDT  := FileDateToDateTime(AlPtr^.lFileTime);
              //stDateTimeStr := DateToStr(DelphiDT) + ' ' + TimeToStr(DelphiDT);
            except
              //stDateTimeStr := 'Unknown';
             end;
            AddContents(Archivename,ExtractFilePath(StrPas(AlPtr^.szFName)),
              ExtractFileName(StrPas(AlPtr^.szFName)),
              AlPtr^.l64OrigSize, AlPtr^.l64NewSize,
              not (AlPtr^.lCryptFlag = OPTION_NONE),
              DelphiDT,'FFFFFFFF'           );
           end;
        AlPtr := AlPtr^.pNext;
       end;
     end;

   end;
  if doWhat <> wtLoadContents then
   with (Cakdir as TCakdir2) do
   begin
    if Assigned(FOnMsg) then
      FOnMsg(NIL, Error, CODE_NOERROR, Msg_OK, ERR_NOERROR);
    if Assigned(FOnProg) then
      FOnProg(Self, '', 1, Get_target_Size);
   end;
end;

function TCakSqxSdk.DllExists : boolean;
begin
        Result := Fileexists(GrabProgrampath+Dllname);
end;

function TCakSqxSdk.Cando(aWork: WorkType): Boolean;
begin
        Case aWork of
        wtNone : Result := true;
        wtTest, wtExtract : Result := CanExtract;
        wtAdd, wtDelete : Result := CanAdd;
        wtSFX :  Result := CanSFX;
        wtLoadContents: Result := CanList;
        else Result := false;
        end;
        if Result then
          if not DLLExists then
            Result := false;
end;

function TCakSqxSdk.Def_TreatAs : string;
begin
end;

procedure TCakSqxSdk.Load_DLL;
begin
  FillChar(SqxDir, SizeOf(SQX_ARCER_HANDLE), #0);
end;

procedure TCakSqxSdk.UnLoad_DLL;
begin
  //nil;
end;

function TCakSqxSdk.HandleSqxCallback(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
begin
  if Stop then
   begin
    Result := 0;
    Exit;
   end;
  Result := 1;
  Application.ProcessMessages;
  if (SqxCallbackStruct.uAction = SQX_ACTION_PROGRESS) then
   begin
    Result := SqxProgress(SqxCallbackStruct);
    Exit;
   end;
  if (SqxCallbackStruct.uAction = SQX_ACTION_NEED_CRYPT_KEY) then
   begin
    Result := SqxCryptKey(SqxCallbackStruct);
    Exit;
   end;
  if (SqxCallbackStruct.uAction = SQX_ACTION_FILE_REPLACE) then
   begin
    Result := SqxReplace(SqxCallbackStruct);
    Exit;
   end;
  if (SqxCallbackStruct.uAction = SQX_ACTION_NEED_NEXT_VOLUME) then
   begin
    //result := HandleSqxNewDisk(SqxCallbackStruct);
    //exit;
   end;
  if (SqxCallbackStruct.uAction = SQX_ACTION_PREPARE_COMPRESS) then
   begin
    //- The archiver might execute a command in the context of another command:
    //- For example, deleting files from a solid archive results in decompressing
    //- of all files of an archive and re-compressing the files that are *not*
    //- deleted from the archive.
    //-
    //- Adding files to a solid archive reults in decompressing of all files of an
    //- archive and re-compressing these files togehter with the new files.
    with (Cakdir as TCakdir2) do
    if (SqxCallbackStruct.uCommand <> SQX_COMMAND_COMPRESS) then
     begin 
      if Assigned(FOnMsg) then
          FOnMsg(NIL, Error, CODE_PROCESSREADD, Msg_OK,
              Format(ERR_PROCESSREADD,[ExtractFileName(StrPas(SqxCallbackStruct.szString))]));
      end
    else
     begin 
      if Assigned(FOnMsg) then
          FOnMsg(NIL, Error, CODE_PROCESSREADD, Msg_OK,
              Format(ERR_PROCESSADD,[ExtractFileName(StrPas(SqxCallbackStruct.szString))]));
     end;
    Application.ProcessMessages;
    Exit;
   end;
  if (SqxCallbackStruct.uAction = SQX_ACTION_PREPARE_EXTRACT) then
   with (Cakdir as TCakdir2) do
   begin
    //- The archiver might execute a command in the context of another command,
    //- see above...
    if (SqxCallbackStruct.uCommand = SQX_COMMAND_EXTRACT) then
    begin
      if Assigned(FOnMsg) then
          FOnMsg(NIL, Error, CODE_PROCESSEXTR, Msg_OK,
              Format(ERR_PROCESSEXTR,[ExtractFileName(StrPas(SqxCallbackStruct.szString))]));
    end
    else if (SqxCallbackStruct.uCommand = SQX_COMMAND_TEST_ARCHIVE) then
     begin 
      if Assigned(FOnMsg) then
          FOnMsg(NIL, Error, CODE_PROCESSTEST, Msg_OK,
              Format(ERR_PROCESSTEST,[ExtractFileName(StrPas(SqxCallbackStruct.szString))]));
     end
    else
     begin
      if Assigned(FOnMsg) then
          FOnMsg(NIL, Error, CODE_PROCESSTEST, Msg_OK,
              Format(ERR_PROCESSTEST,[ExtractFileName(StrPas(SqxCallbackStruct.szString))]));
     end;
    Application.ProcessMessages;
    Exit;
   end;
  with (Cakdir as TCakdir2) do
  if (SqxCallbackStruct.uAction = SQX_ACTION_FINISH_DELETE) then
   begin
    if Assigned(FOnMsg) then
          FOnMsg(NIL, Error, CODE_PROCESSDEL, Msg_OK,
              Format(ERR_PROCESSDEL,[ExtractFileName(StrPas(SqxCallbackStruct.szString))]));
    FOnProg(Self, '', 1, 1);
    Application.ProcessMessages;
    Exit;
   end;
end;

function TCakSqxSdk.SqxReplace(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
var 
  k: String;
begin
  k := StrPas(SQX_REPLACE_STRUCT_PTR(SqxCallbackStruct.lpVariable)^.szFileName);
  with (Cakdir as TCakdir2) do
  if AskOverwrite(k) then
    Result := 2 
  else
    Result := 1;
        {
        if assigned(FOnOver) then
                FOnOver(Self,k,overwrite,apply2all);
        if overwrite then Result := 2 Else Result := 1;
        }
end;

function TCakSqxSdk.SqxProgress(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnProg) then
    FOnProg(Self, '', Get_Target_Size,
      Get_Target_Size div 100 * Longint(SqxCallbackStruct.lpVariable));
  Result := 1;
end;

function TCakSqxSdk.SqxCryptKey(var SqxCallbackStruct: SQX_CALLBACK_STRUCT): Longint;
var
  k: String;
begin
  with (Cakdir as TCakdir2) do
  if Assigned(FOnPwd) then
    FOnPwd(Self, ArchiveName, '', k);
  Result := 0;
  if k <> '' then
   begin
    StrPCopy(SqxCallbackStruct.szString2, k);
    Result := 1;
   end;
end;


procedure Register;
begin
  //RegisterComponents('QZip', [TCakSqxSdk]);
end;

end.
