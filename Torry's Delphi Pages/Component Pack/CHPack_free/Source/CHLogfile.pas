unit CHLogfile;

{ ##############################################################################
  TCHLogfile

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 15.02.2003    - First Release
  1.0.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed
  1.1.0 - 29.12.2004    - NEW: Blockwrite and TFileMode


  ############################################################################ }

interface

uses
  Windows, Forms, SysUtils, Classes, extctrls;


type
  TLogMode = (logDirectWrite, logMemoryWrite, logBlockWrite);
  TFileMode = (flAppend, flRewrite);

  TCHLogfile = class;

  TCHLogTimer = class(TPersistent)
  private
    FOwner : TCHLogfile;
    FTimer : TTimer;
  public
    constructor Create(AOwner: TCHLogfile); virtual;
    destructor Destroy; override;
  end;

  TCHLogfile = class(TComponent)
  private
    FLogfile: TFileName;
    FLogTimer : TCHLogTimer;
    FDefaultDir : string;
    FDefaultLog : string;
    FDefaultExt : string;
    FEnable: Boolean;
    FMaxLogSize: Integer;
    FLogMemSize: Integer;
    FOnMaxLogSize: TNotifyEvent;
    FWriteMode: TLogMode;
    FLogList : TStringList;
    FWriteOnEnd: Boolean;
    FWriteTime: Cardinal;
    FWriteOnException: Boolean;
    FOnWriteMemory: TNotifyEvent;
    FFileMode: TFileMode;
    procedure SetLogfile(const Value: TFileName);
    procedure AppException(Sender: TObject; E: Exception);
    function CheckLogSize(nMem : Integer) : Boolean;
    function GetFileSize(Filename: string): Integer;
    procedure WriteDirectExt(Text : string);
    procedure WriteMemoryExt(Text : string);
    procedure FlushMemory(Sender : TObject);
    procedure SetWriteMode(const Value: TLogMode);
    procedure SetWriteTime(const Value: Cardinal);
  public
    Linecounter : Integer;
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure Loaded; override;
    procedure Write(Text : string);
    procedure WriteMemory;
    procedure ClearMemory;
  published
    property OnMaxLogSize : TNotifyEvent read FOnMaxLogSize write FOnMaxLogSize;
    property OnWriteMemory : TNotifyEvent read FOnWriteMemory write FOnWriteMemory;

    property Enabled : Boolean read FEnable Write FEnable;
    property Logfile : TFileName read FLogfile Write SetLogfile;
    property FileMode : TFileMode read FFileMode Write FFileMode;
    property WriteMode : TLogMode read FWriteMode Write SetWriteMode;
    property WriteMemoryTime : Cardinal read FWriteTime Write SetWriteTime;
    property WriteMemoryOnEnd : Boolean read FWriteOnEnd Write FWriteOnEnd;
    property WriteMemoryOnException : Boolean read FWriteOnException Write FWriteOnException;
    property MaxLogSize : Integer read FMaxLogSize Write FMaxLogSize;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHLogfile]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHLogfile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefaultDir := ExtractFilePath(Application.ExeName);
  FDefaultLog := 'CHLogfile';
  FDefaultExt := '.txt';
  FEnable := True;
  FWriteTime := 5000;
  FLogTimer := TCHLogtimer.Create(Self);
  FLogList := TStringList.Create;
  FLogMemSize := 0;
  FFileMode := flAppend;
  Application.OnException := AppException;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHLogfile.Destroy;
begin
  if FWriteOnEnd then
    WriteMemory;

  FLogList.Free;
  FLogTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.AppException(Sender: TObject; E: Exception);
begin
  if FWriteOnException then
    WriteMemory;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
  begin
    if Trim(FLogfile) = '' then
      FLogfile := FDefaultDir + FDefaultLog + FDefaultExt;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.SetLogfile(const Value: TFileName);
var
  tFile : TextFile;
begin
  if FLogfile <> Value then
  begin
    FLogfile := Value;
  end;

  // count lines, if file exists
  if not (csDesigning in ComponentState) then
  begin
    Linecounter := 1;
    AssignFile(tFile, FLogfile);
    if FileExists(FLogfile) then
    begin
      Reset(tFile);
      try
        while not Eof(tFile) do
        begin
          Readln(tFile);
          Inc(Linecounter);
        end;
      finally
        CloseFile(tFile);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.SetWriteMode(const Value: TLogMode);
begin
  if FWriteMode <> Value then
  begin
  	FWriteMode := Value;
    if (FWriteMode = logMemoryWrite) and not (csDesigning in ComponentState) then
       FLogTimer.FTimer.Enabled := True;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.SetWriteTime(const Value: Cardinal);
begin
  if FWriteTime <> Value then
  begin
  	FWriteTime := Value;
    FLogTimer.FTimer.Interval := Value;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHLogfile.CheckLogSize(nMem : Integer): Boolean;
var
  nLogSize : Integer;
begin
  Result := True;
  if FMaxLogSize > 0 then
  begin
    nLogSize := GetFileSize(FLogfile);
    if FMaxLogSize < (nLogSize + nMem) then
      Result := False;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.Write(Text: string);

begin
  if FEnable then
  begin
    if FWriteMode = logDirectWrite then
      WriteDirectExt(Text)
    else
      WriteMemoryExt(Text);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.WriteDirectExt(Text: string);
var
  tFile : TextFile;
  nTextLen : Integer;
begin
  if FFileMode = flRewrite then
    DeleteFile(FLogfile);

  nTextLen := Length(Text);
  if CheckLogSize(nTextLen) then
  begin
    AssignFile(tFile, FLogfile);
    if not FileExists(FLogfile) then
    begin
      Rewrite(tFile);
      Linecounter := 1;
    end
    else
      Append(tFile);

    try
      Writeln(tFile, Text);
      Flush(tFile);
      Inc(Linecounter);
    finally
      CloseFile(tFile);
    end;
  end
  else
  begin
    // Event MaxLogSize
    if Assigned(FOnMaxLogSize) then
      FOnMaxLogSize(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.WriteMemoryExt(Text: string);
begin
  FLogList.Add(Text);
  FLogMemSize := FLogMemSize + Length(Text);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.FlushMemory(Sender : TObject);
var
  tFile : TextFile;
  I : Integer;
begin
  if FFileMode = flRewrite then
    DeleteFile(FLogfile);
    
  if CheckLogSize(FLogMemSize) then
  begin
    AssignFile(tFile, FLogfile);
    if not FileExists(FLogfile) then
    begin
      Rewrite(tFile);
      Linecounter := 1;
    end
    else
      Append(tFile);

    try
      for I := 0 to FLogList.Count - 1 do
      begin
        Writeln(tFile, FLogList.strings[I] );
        Flush(tFile);
      	Inc(Linecounter);
      end;
      ClearMemory;

      // Event WriteMemory
      if Assigned(FOnWriteMemory) then
        FOnWriteMemory(Self);

    finally
      CloseFile(tFile);
    end;
  end
  else
  begin
    // Event MaxLogSize
    if Assigned(FOnMaxLogSize) then
      FOnMaxLogSize(Self);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.ClearMemory;
begin
  FLogList.Clear;
  FLogMemSize := 0;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHLogfile.WriteMemory;
begin
  FlushMemory(Self);
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TLogTimer }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHLogTimer.Create(AOwner: TCHLogfile);
begin
  inherited Create;
  FOwner := AOwner;

  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := FOwner.FWriteTime;
  FTimer.Enabled := False;
  FTimer.OnTimer := FOwner.FlushMemory;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHLogTimer.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;



function TCHLogfile.GetFileSize(Filename: string): Integer;
var
  SR: TSearchRec;
begin
  if FindFirst(Filename, faAnyFile, SR) = 0 then
    Result:=SR.Size
  else
    Result:= -1;
  FindClose(SR);

end;



end.
