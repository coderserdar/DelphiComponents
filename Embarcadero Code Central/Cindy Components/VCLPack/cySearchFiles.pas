{   Component(s):
    tcySearchFiles

    Description:
    Component to search files from a directory.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cySearchFiles;

{$I ..\Core\cyCompilerDefines.inc}

interface

Uses Classes, Forms, SysUtils, StrUtils;

type
  TcyFileAttributeMode = (faYes, faNo, faBoth);

  TcyFileAttributes=class(TPersistent)
  private
    FArchive: TcyFileAttributeMode;
    FReadOnly: TcyFileAttributeMode;
    FHidden: TcyFileAttributeMode;
    FSystem: TcyFileAttributeMode;
    FTemporary: TcyFileAttributeMode;
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
  published
    property Archive: TcyFileAttributeMode read FArchive write FArchive default faBoth;
    property ReadOnly: TcyFileAttributeMode read FReadOnly write FReadOnly default faBoth;
    property Hidden: TcyFileAttributeMode read FHidden write FHidden default faBoth;
    property System: TcyFileAttributeMode read FSystem write FSystem default faBoth;
    {$IFDEF UNICODE} property Temporary: TcyFileAttributeMode read FTemporary write FTemporary default faBoth; {$ENDIF}
  end;

  TSearchRecInstance=class
  private
    FSuspendedSearchRec: TSearchRecInstance;
    FSRecFound: boolean;
    FSRec: TSearchRec;
    FPath: String;
    FMask: String;
  protected
  public
    property SuspendedSearchRec: TSearchRecInstance read FSuspendedSearchRec;
    property SearchRec: TSearchRec read FSRec;
    property Path: String read FPath;
  published
  end;

  TOption = (soOnlyDirs, soIgnoreAttributes, soIgnoreMaskInclude, soIgnoreMaskExclude, soIgnoreDirectoryInclude, soIgnoreDirectoryExclude);
  TOptions = Set of TOption;
  TSearchState = (ssIdle, ssPaused, ssSearch, ssPausing, ssResuming, ssAborting);
  TProcOnValidateFileEvent = procedure (Sender: TObject; ValidMaskInclude, ValidMaskExclude, ValidAttributes: boolean; var Accept: boolean) of object;
  TProcOnValidateDirectoryEvent = procedure (Sender: TObject; Directory: String; ValidDirectoryInclude, ValidDirectoryExclude: Boolean; var Accept: boolean) of object;

  TcyCustomSearchFiles = class(TComponent)
  private
    FFromPath: String;
    FSearchState: TSearchState;
    FFileAttributes: TcyFileAttributes;
    FSubDirectories: boolean;
    FOnValidateFile: TProcOnValidateFileEvent;
    FOnValidateDirectory: TProcOnValidateDirectoryEvent;
    FActiveSearchRec: TSearchRecInstance;
    FOnTerminate: TNotifyEvent;
    FMaskInclude: TStrings;
    FMaskExclude: TStrings;
    FOnResume: TNotifyEvent;
    FOnAbort: TNotifyEvent;
    FOnPause: TNotifyEvent;
    FAborted: boolean;
    FOnExitDirectory: TNotifyEvent;
    FMatchedDirectories: Integer;
    FMatchedFiles: Integer;
    FOptions: TOptions;
    FDirectoryExclude: TStrings;
    FDirectoryInclude: TStrings;
    procedure SetFileAttributes(const Value: TcyFileAttributes);
    function GetCurrentDirectory: String;
    function GetCurrentFileName: String;
    procedure CreateSearchRecInstance(aPath, aMask: String);
    procedure AnalyseActiveSearchRec;
    procedure FreeActiveSearchRecInstance;
    procedure SetOptions(const Value: TOptions);
    procedure SetMaskExclude(const Value: TStrings);
    procedure SetMaskInclude(const Value: TStrings);
    procedure SetDirectoryExclude(const Value: TStrings);
    procedure SetDirectoryInclude(const Value: TStrings);
  protected
    function  SearchRecValidateFile: boolean; virtual;
    function SearchRecValidateFolder: Boolean; virtual;
    procedure SearchRecExitFolder; virtual;
    procedure SearchRecAcceptedFile(var DoFindNext: boolean); virtual;
    procedure Terminate;
    // Public properties :
    property ActiveSearchRec: TSearchRecInstance read FActiveSearchRec;
    property Aborted: boolean read FAborted;
    property CurrentFileName: String read GetCurrentFileName;
    property CurrentDirectory: String read GetCurrentDirectory;
    property MatchedDirectories: Integer read FMatchedDirectories;
    property MatchedFiles: Integer read FMatchedFiles;
    property SearchState: TSearchState read FSearchState;
    // Published properties :
    property FileAttributes: TcyFileAttributes read FFileAttributes write SetFileAttributes;
    property DirectoryInclude: TStrings read FDirectoryInclude write SetDirectoryInclude;
    property DirectoryExclude: TStrings read FDirectoryExclude write SetDirectoryExclude;
    property MaskInclude: TStrings read FMaskInclude write SetMaskInclude;
    property MaskExclude: TStrings read FMaskExclude write SetMaskExclude;
    property FromPath: String read FFromPath write FFromPath;
    property Options: TOptions read FOptions write SetOptions default [];
    property SubDirectories: boolean read FSubDirectories write FSubDirectories;
    property OnAbort: TNotifyEvent read FOnAbort write FOnAbort;
    property OnPause: TNotifyEvent read FOnPause write FOnPause;
    property OnResume: TNotifyEvent read FOnResume write FOnResume;
    property OnValidateFile: TProcOnValidateFileEvent read FOnValidateFile write FOnValidateFile;
    property OnValidateDirectory: TProcOnValidateDirectoryEvent read FOnValidateDirectory write FOnValidateDirectory;
    property OnExitDirectory: TNotifyEvent read FOnExitDirectory write FOnExitDirectory;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Abort: boolean;
    function Pause: boolean;
    function Resume: boolean;
    function Execute: boolean; virtual;
  published
  end;

  TcySearchFiles = class(TcyCustomSearchFiles)
  private
  protected
  public
    property ActiveSearchRec;
    property Aborted;
    property CurrentFileName;
    property CurrentDirectory;
    property MatchedDirectories;
    property MatchedFiles;
    property SearchState;
  published
    property FileAttributes;
    property FromPath;
    property DirectoryInclude;
    property DirectoryExclude;
    property MaskInclude;
    property MaskExclude;
    property Options;
    property SubDirectories;
    property OnAbort;
    property OnPause;
    property OnResume;
    property OnValidateFile;
    property OnValidateDirectory;
    property OnExitDirectory;
    property OnTerminate;
  end;


function FileNameRespondToMask(aFileName: String; aMask: String): Boolean;
function IsFolder(aSRec: TSearchrec): Boolean;
function FindFile(FromPath: string; const aFileMask: string; const IncludeSubDirectories: Boolean): String;

implementation

function IsFolder(aSRec: TSearchrec): Boolean;
begin
  Result := aSRec.Attr and faDirectory <> 0;
end;

function FindFile(FromPath: string; const aFileMask: string; const IncludeSubDirectories: Boolean): String;
var
  SRec : TSearchRec;
  Cont : Boolean;
begin
  Result := '';
  FromPath := IncludeTrailingPathDelimiter(FromPath);

  if IncludeSubDirectories
  then Cont := FindFirst(FromPath + '*', faAnyFile, SRec) = 0
  else Cont := FindFirst(FromPath + aFileMask, faAnyFile, SRec) = 0;

  while Cont do
  begin
    if isFolder(SRec) then
    begin
      if IncludeSubDirectories then
        if (SRec.Name <> '.') and (SRec.Name <> '..') then
          Result := FindFile(FromPath + SRec.Name, aFileMask, true);
    end
    else
      if FileNameRespondToMask(SRec.Name, aFileMask) then
        Result := FromPath + SRec.Name;

    if Result = ''
    then Cont := FindNext(SRec) = 0
    else Break;
  end;

  FindClose(SRec);
end;

function FileNameRespondToMask(aFileName: String; aMask: String): Boolean;
var
  PortionMask: String;
  AnyPosition: Boolean;
  LengthMask, PortionPos, PortionLength: Integer;

      function GetPortionMask: string;
      var c: integer;
      begin
        Result := '';
        for c := 1 to lengthMask do
          if aMask[c] = '*'
          then Break
          else Result := Result + aMask[c];
      end;

begin
  Result := true;
  if aMask = '*' then EXIT;
  if aMask = '' then EXIT;
  aFileName := AnsiUpperCase(aFileName);
  aMask := AnsiUpperCase(aMask);
  LengthMask := Length(aMask);
  AnyPosition := false;

  while (Result) and (LengthMask <> 0) do
    if aMask[1] = '*'
    then begin
      Delete(aMask, 1, 1);
      LengthMask := LengthMask - 1;
      AnyPosition := true;
    end
    else begin
      PortionMask := GetPortionMask;
      PortionLength := Length(PortionMask);

      if AnyPosition
      then begin
        AnyPosition := false;

        if LengthMask - PortionLength = 0 // No more mask, so, aFileName must end with PortionMask!
        then begin
          Result := RightStr(aFileName, PortionLength) = PortionMask;
        end
        else begin
          PortionPos := Pos(PortionMask, aFileName);
          Result := PortionPos <> 0;
        end;
      end
      else begin
        PortionPos := Pos(PortionMask, aFileName);
        Result := PortionPos = 1;
      end;

      if Result
      then begin
        Delete(aMask, 1, PortionLength);
        LengthMask := LengthMask - PortionLength;        
        Delete(aFileName, 1, PortionPos + PortionLength - 1);
      end;
    end;
end;

{ TcyFileAttributes }
constructor TcyFileAttributes.Create(AOwner: TComponent);
begin
  FArchive := faBoth;
  FReadOnly := faBoth;
  FHidden := faBoth;
  FSystem := faBoth;
  FTemporary := faBoth;
end;

{ TcyCustomSearchFiles }
constructor TcyCustomSearchFiles.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptions := [];
  FFileAttributes := TcyFileAttributes.Create(self);
  FDirectoryInclude := TStringList.Create;
  FDirectoryExclude := TStringList.Create;
  FMaskInclude := TStringList.Create;
  FMaskExclude := TStringList.Create;
  FSubDirectories := true;
  FSearchState := ssIdle;
  FActiveSearchRec := nil;
  FAborted := false;
end;

destructor TcyCustomSearchFiles.Destroy;
begin
  FDirectoryInclude.Free;
  FDirectoryExclude.Free;
  FMaskInclude.Free;
  FMaskExclude.Free;
  FFileAttributes.Free;
  inherited Destroy;
end;

procedure TcyCustomSearchFiles.SetFileAttributes(const Value: TcyFileAttributes);
begin
  FFileAttributes := Value;
end;

procedure TcyCustomSearchFiles.SetDirectoryExclude(const Value: TStrings);
begin
  if Assigned(FDirectoryExclude) then
    FDirectoryExclude.Assign(Value)
  else
    FDirectoryExclude := Value;
end;

procedure TcyCustomSearchFiles.SetDirectoryInclude(const Value: TStrings);
begin
  if Assigned(FDirectoryInclude) then
    FDirectoryInclude.Assign(Value)
  else
    FDirectoryInclude := Value;
end;

procedure TcyCustomSearchFiles.SetMaskExclude(const Value: TStrings);
begin
  if Assigned(FMaskExclude) then
    FMaskExclude.Assign(Value)
  else
    FMaskExclude := Value;
end;

procedure TcyCustomSearchFiles.SetMaskInclude(const Value: TStrings);
begin
  if Assigned(FMaskInclude) then
    FMaskInclude.Assign(Value)
  else
    FMaskInclude := Value;
end;

procedure TcyCustomSearchFiles.SetOptions(const Value: TOptions);
begin
  FOptions := Value;
end;

procedure TcyCustomSearchFiles.CreateSearchRecInstance(aPath, aMask: String);
var aSearchRecInstance: TSearchRecInstance;
begin
  if RightStr(aPath, 1) <> '\' then aPath := aPath + '\';
  aSearchRecInstance := TSearchRecInstance.Create;

  with aSearchRecInstance do
  begin
    FPath := aPath;
    FMask := aMask;
    FSuspendedSearchRec := FActiveSearchRec;
    FActiveSearchRec := aSearchRecInstance;

    FSRecFound := FindFirst(FPath + FMask, FaAnyFile, FSRec) = 0;
  end;
end;

procedure TcyCustomSearchFiles.AnalyseActiveSearchRec;
Label
  BeginningProcedure, ProcessNext;
var
  CanProcessNext: boolean;
begin
  if FSearchState = ssResuming
  then begin
    if Assigned(FOnResume)
    then FOnResume(Self);
    FSearchState := ssSearch;    
  end;

  BeginningProcedure:
  CanProcessNext := true;

  if (FActiveSearchRec.FSRec.Name + '.')[1] <> '.'    // Ignore FSRec.Name = '.' or FSRec.Name = '..'
  then
    if IsFolder(FActiveSearchRec.FSRec)
    then begin
      if FSubDirectories
      then
        if SearchRecValidateFolder          // Ignore sub-folder ?
        then begin
          // Create another instance of TSearchRecInstance :
          CreateSearchRecInstance(FActiveSearchRec.FPath + FActiveSearchRec.FSRec.Name, FActiveSearchRec.FMask);

          if FActiveSearchRec.FSRecFound
          then Goto BeginningProcedure
          else FreeActiveSearchRecInstance;
        end;
    end
    else
      if not (soOnlyDirs In FOptions)
      then
        if SearchRecValidateFile
        then SearchRecAcceptedFile(CanProcessNext);

  ProcessNext:
  if CanProcessNext
  then FActiveSearchRec.FSRecFound := FindNext(FActiveSearchRec.FSRec) = 0;

  if not FActiveSearchRec.FSRecFound
  then begin
    FreeActiveSearchRecInstance;

    if FActiveSearchRec <> nil           // Continue with prior TSearchRecInstance ...
    then begin
      SearchRecExitFolder;
      Goto ProcessNext;
    end;
  end;

  if FSearchState = ssPausing
  then begin
    if Assigned(FOnPause)
    then FOnPause(Self);
    FSearchState := ssPaused;
  end;

  if FSearchState = ssAborting
  then begin
    if Assigned(FOnAbort)
    then FOnAbort(Self);
    FSearchState := ssIdle;
  end;

  case FSearchState of
    ssSearch:
    begin
      if FActiveSearchRec <> nil           // Continue with another TSearchRecInstance ...
      then Goto BeginningProcedure;
    end;

    ssIdle:                                  // Process aborted ...
      while FActiveSearchRec <> nil do
        FreeActiveSearchRecInstance;

    // ssPaused: Let exit code ...  
  end;
end;

procedure TcyCustomSearchFiles.FreeActiveSearchRecInstance;
var CurSearchRecInstance: TSearchRecInstance;
begin
  CurSearchRecInstance := FActiveSearchRec;
  FActiveSearchRec := CurSearchRecInstance.FSuspendedSearchRec;
  FindClose(CurSearchRecInstance.FSRec);
  CurSearchRecInstance.Free;

  if FActiveSearchRec = nil   // No more TSearchRecInstance to process ...
  then Terminate;
end;

function TcyCustomSearchFiles.Execute: boolean;
begin
  Result := false;
  if FSearchState <> ssIdle then EXIT;
  if FFromPath = '' then EXIT;
  if not DirectoryExists(FFromPath) then EXIT;

  Result := true;
  FActiveSearchRec := nil;
  FAborted := false;
  FMatchedDirectories := 0;
  FMatchedFiles := 0;
  FSearchState := ssSearch;
  CreateSearchRecInstance(FFromPath, '*');

  if FActiveSearchRec.FSRecFound
  then AnalyseActiveSearchRec
  else FreeActiveSearchRecInstance;
end;

function TcyCustomSearchFiles.Pause: boolean;
begin
  if FSearchState = ssSearch
  then begin
    Result := true;
    FSearchState := ssPausing;
  end
  else
    Result := false;
end;

function TcyCustomSearchFiles.Resume: boolean;
begin
  if FSearchState = ssPaused
  then begin
    Result := true;
    FSearchState := ssResuming;
    AnalyseActiveSearchRec;
  end
  else
    Result := false;
end;

function TcyCustomSearchFiles.Abort: boolean;
begin
  if FSearchState in [ssSearch, ssPaused]
  then begin
    Result := true;
    FAborted := true;

    if FSearchState = ssPaused
    then begin
      FSearchState := ssAborting;
      if Assigned(FOnAbort)
      then FOnAbort(Self);

      FSearchState := ssIdle;

      while FActiveSearchRec <> nil do
        FreeActiveSearchRecInstance;
    end
    else
      FSearchState := ssAborting;
  end
  else
    Result := false;
end;

function TcyCustomSearchFiles.SearchRecValidateFile: boolean;
var
  MaskIncludeMatch, MaskExcludeMatch, AttributesMatch: boolean;
  SearchRecAttr: Integer;
  SearchRecName: String;

    function FxAttributesMatch: boolean;

        function RespondToAttrMode(aFileAttrMode: TcyFileAttributeMode; aAttr: Integer): boolean;
        begin
          case aFileAttrMode of
            faYes:  Result := (SearchRecAttr and aAttr) <> 0;
            faNo:   Result := (SearchRecAttr and aAttr) = 0;
            faBoth: Result := true;
          end;
        end;

    begin
      Result := true;
      if Result then Result := RespondToAttrMode(FFileAttributes.FArchive,   SysUtils.faArchive);
      if Result then Result := RespondToAttrMode(FFileAttributes.FReadOnly,  SysUtils.faReadOnly);
      if Result then Result := RespondToAttrMode(FFileAttributes.FHidden,    SysUtils.faHidden);
      if Result then Result := RespondToAttrMode(FFileAttributes.FSystem,    SysUtils.faSysFile);
      {$IFDEF UNICODE}
      if Result then Result := RespondToAttrMode(FFileAttributes.FTemporary, SysUtils.faTemporary);
      {$ENDIF}
    end;

    function FxMaskIncludeMatch: boolean;
    var
      i, c: Integer;
    begin
      c := FMaskInclude.Count;

      if c <> 0 then
      begin
        Result := false;
        for i := 0 to c - 1 do
          if FileNameRespondToMask(SearchRecName, FMaskInclude[i]) then
          begin
            Result := true;
            Break;
          end;
      end
      else
        Result := true;
    end;

    function FxMaskExcludeMatch: boolean;
    var
      i, c: Integer;
    begin
      Result := false;
      c := FMaskExclude.Count;

      if c <> 0 then
        for i := 0 to c - 1 do
          if FileNameRespondToMask(SearchRecName, FMaskExclude[i]) then
          begin
            Result := true;
            Break;
          end;
    end;

begin
  SearchRecAttr := FActiveSearchRec.FSRec.Attr;
  SearchRecName := FActiveSearchRec.FSRec.Name;

  // File attributes :
  if soIgnoreAttributes in FOptions
  then AttributesMatch := true
  else AttributesMatch := FxAttributesMatch;

  // MaskInclude :
  if soIgnoreMaskInclude in FOptions
  then MaskIncludeMatch := true
  else MaskIncludeMatch := FxMaskIncludeMatch;

  // MaskExclude :
  if soIgnoreMaskExclude in FOptions
  then MaskExcludeMatch := false
  else MaskExcludeMatch := FxMaskExcludeMatch;

  Result := MaskIncludeMatch and (not MaskExcludeMatch) and AttributesMatch;

  if Assigned(FOnValidateFile) then
    FOnValidateFile(Self, MaskIncludeMatch, MaskExcludeMatch, AttributesMatch, Result);

  if Result then
    Inc(FMatchedFiles, 1);
end;

function TcyCustomSearchFiles.SearchRecValidateFolder: boolean;
var
  DirectoryIncludeMatch, DirectoryExcludeMatch: boolean;
  SearchRecPath_Name: String;

    function FxDirectoryIncludeMatch: boolean;
    var i, c: Integer;
    begin
      c := FDirectoryInclude.Count;

      if c <> 0 then
      begin
        Result := false;
        for i := 0 to c - 1 do
          if FileNameRespondToMask(SearchRecPath_Name, FDirectoryInclude[i]) then // if AnsiLowerCase(SearchRecPath_Name) = AnsiLowerCase(FDirectoryInclude[i]) then
          begin
            Result := true;
            Break;
          end;
      end
      else
        Result := true;
    end;

    function FxDirectoryExcludeMatch: boolean;
    var i, c: Integer;
    begin
      Result := false;
      c := FDirectoryExclude.Count;

      if c <> 0 then
        for i := 0 to c - 1 do
          if FileNameRespondToMask(SearchRecPath_Name, FDirectoryExclude[i]) then // if AnsiLowerCase(SearchRecPath_Name) = AnsiLowerCase(FDirectoryExclude[i]) then
          begin
            Result := true;
            Break;
          end;
    end;

begin
  SearchRecPath_Name := FActiveSearchRec.FPath + FActiveSearchRec.FSRec.Name;

  // DirectoryInclude :
  if soIgnoreDirectoryInclude in FOptions
  then DirectoryIncludeMatch := true
  else DirectoryIncludeMatch := FxDirectoryIncludeMatch;

  // DirectoryExclude :
  if soIgnoreMaskExclude in FOptions
  then DirectoryExcludeMatch := false
  else DirectoryExcludeMatch := FxDirectoryExcludeMatch;

  Result := DirectoryIncludeMatch and (not DirectoryExcludeMatch);


  if Assigned(FOnValidateDirectory) then
    FOnValidateDirectory(Self, SearchRecPath_Name, DirectoryIncludeMatch, DirectoryExcludeMatch, Result);

  if Result then
    Inc(FMatchedDirectories, 1);
end;

procedure TcyCustomSearchFiles.SearchRecExitFolder;
begin
  if Assigned(FOnExitDirectory)
  then FOnExitDirectory(Self);
end;

procedure TcyCustomSearchFiles.SearchRecAcceptedFile(var DoFindNext: boolean);
begin
  DoFindNext := true;
end;

procedure TcyCustomSearchFiles.Terminate;
begin
  FSearchState := ssIdle;

  if Assigned(FOnTerminate)
  then FOnTerminate(Self);
end;

function TcyCustomSearchFiles.GetCurrentDirectory: String;
begin
  if FActiveSearchRec = nil
  then Result := ''
  else Result := FActiveSearchRec.FPath;
end;

function TcyCustomSearchFiles.GetCurrentFileName: String;
begin
  if FActiveSearchRec = nil
  then Result := ''
  else Result := FActiveSearchRec.FSRec.Name;
end;

end.
