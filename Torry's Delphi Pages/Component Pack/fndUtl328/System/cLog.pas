{                                                                              }
{                              Log unit v3.02                                  }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                     Its original file name is cLog.pas                       }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Revision history:                                                            }
{   2002/02/07  2.01  Added TLog component from cDebug to cSysUtils.           }
{   2002/09/04  3.02  Moved TLog component to cLog unit.                       }
{                                                                              }

{$INCLUDE ..\cDefines.inc}
unit cLog;

interface

uses
  { Delphi }
  SysUtils,
  Classes;



{                                                                              }
{ Log Component                                                                }
{                                                                              }
{$TYPEINFO ON}
type
  TLogClass = (lcInfo, lcError, lcWarning, lcDebug, lcEndRepeat,
               lcUserEventBegin, lcUserEventEnd, lcInit);
  TLogEvent = procedure (Sender: TObject; LogClass: TLogClass;
      LogMsg: String) of object;
  TLogEditMessageEvent = procedure (Sender: TObject; LogClass: TLogClass;
      var LogMsg: String) of object;
  TLogFileEvent = procedure (Sender: TObject; LogClass: TLogClass;
      var LogMsg: String; var LogToFile: Boolean) of object;
  TLogOptions = Set of (loLogToFile,           // Output log to a file
                        loKeepFileOpen,        // Keep log file open between messages
                        loLogToDebugLog,       // Log to system debug log (IDE)
                        loNoLogEvent,          // Don't generate log event
                        loLogDate,             // Include date in log message
                        loLogTime,             // Include time in log message
                        loLogMilliSecDiff,     // Include milliseconds since last log in message
                        loIgnoreLogFailure,    // Ignore log failures
                        loCheckRepeats,        // Log first and last of repeated messages
                        loIgnoreClassDebug,    // Ignore messages of class Debug
                        loIgnoreClassError,    // Ignore messages of class Error
                        loIgnoreClassWarning,  // Ignore messages of class Warning
                        loIgnoreClassInfo);    // Ignore messages of class Info
  TLog = class(TComponent)
  protected
    FOnLog          : TLogEvent;
    FOnEditMessage  : TLogEditMessageEvent;
    FOnLogFile      : TLogFileEvent;
    FLogFile        : TFileStream;
    FLogFileName    : String;
    FLogOptions     : TLogOptions;
    FLastLog        : Cardinal;
    FLastLogMsg     : String;
    FLogRepeatCount : Integer;
    FLogTo          : TLog;
    FLevels         : TStringList;
    FMaxLogLevel    : Integer;

    procedure SetLogFileName(const LogFileName: String);
    procedure SetLogOptions(const LogOptions: TLogOptions);
    procedure SetLogTo(const LogTo: TLog);

    procedure Init; virtual;
    procedure RaiseError(const Msg: String);

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function  GetLogLevel: Integer;

    procedure TriggerLogMsg(const Sender: TObject; const LogClass: TLogClass;
              const LogMsg: String); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property  LogFileName: String read FLogFileName write SetLogFileName;
    property  LogOptions: TLogOptions read FLogOptions write SetLogOptions default [];
    property  LogTo: TLog read FLogTo write SetLogTo;

    property  LogLevel: Integer read GetLogLevel;
    property  Levels: TStringList read FLevels;
    procedure Enter(const Level: String = '');
    procedure Leave;
    property  MaxLogLevel: Integer read FMaxLogLevel write FMaxLogLevel default -1;

    procedure Log(const Sender: TObject; const LogClass: TLogClass;
              const LogMsg: String); overload; virtual;
    procedure Log(const LogClass: TLogClass; const LogMsg: String); overload;
    procedure Log(const LogMsg: String); overload;
    procedure LogDebug(const LogMsg: String);
    procedure LogError(const LogMsg: String);
    procedure LogWarning(const LogMsg: String);

    procedure DeleteLogFile;
    procedure LoadLogFileInto(const Destination: TStrings; const Size: Integer = -1);

    property  OnLog: TLogEvent read FOnLog write FOnLog;
    property  OnEditMessage: TLogEditMessageEvent read FOnEditMessage write FOnEditMessage;
    property  OnLogFile: TLogFileEvent read FOnLogFile write FOnLogFile;
  end;
  ELog = class(Exception);

  { TfndLog                                                                    }
  TfndLog = class(TLog)
  published
    property  LogFileName;
    property  LogOptions;
    property  LogTo;
    property  MaxLogLevel;

    property  OnLog;
    property  OnEditMessage;
    property  OnLogFile;
  end;



{                                                                              }
{ Application Log                                                              }
{                                                                              }
function AppLog: TLog;



implementation

uses
  { Delphi }
  Windows,

  { Fundamentals }
  cUtils,
  cStrings;



{                                                                              }
{ Log Component                                                                }
{                                                                              }
constructor TLog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevels := TStringList.Create;
  Init;
end;

destructor TLog.Destroy;
begin
  FreeAndNil(FLogFile);
  FreeAndNil(FLevels);
  inherited Destroy;
end;

procedure TLog.Init;
begin
  FLogFileName := StrExclPrefix(ObjectClassName(self) + '.log', 'T');
  FMaxLogLevel := -1;
  FLogOptions := [{$IFDEF DEBUG}loLogToDebugLog{$ENDIF}];
  {$IFDEF OS_WIN32}
  FLastLog := GetTickCount;
  {$ENDIF}
end;

procedure TLog.RaiseError(const Msg: String);
begin
  raise ELog.Create(Msg);
end;

procedure TLog.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = FLogTo then
      FLogTo := nil;
end;

procedure TLog.SetLogFileName(const LogFileName: String);
begin
  if LogFileName = FLogFileName then
    exit;
  FreeAndNil(FLogFile);
  FLogFileName := LogFileName;
end;

procedure TLog.SetLogOptions(const LogOptions: TLogOptions);
begin
  if LogOptions = FLogOptions then
    exit;
  FLogOptions := LogOptions;
  if not (loLogToFile in LogOptions) or not (loKeepFileOpen in LogOptions) then
    FreeAndNil(FLogFile);
end;

procedure TLog.SetLogTo(const LogTo: TLog);
var L : TLog;
begin
  if LogTo = FLogTo then
    exit;
  if LogTo = nil then
    begin
      FLogTo := nil;
      exit;
    end;

  L := LogTo;
  Repeat
    if L = self then
      RaiseError('Circular LogTo reference');
    L := L.FLogTo;
  Until not Assigned(L);

  FLogTo := LogTo;
end;

procedure TLog.TriggerLogMsg(const Sender: TObject; const LogClass: TLogClass; const LogMsg: String);
begin
end;

procedure TLog.Log(const Sender: TObject; const LogClass: TLogClass; const LogMsg: String);
var S : String;
    N : TDateTime;
    I : Integer;
    T : Cardinal;
    R, F : Boolean;
begin
  if [csDesigning, csLoading] * ComponentState <> [] then
    exit;

  if (FMaxLogLevel >= 0) and (FLevels.Count > FMaxLogLevel) then
    exit;

  if Assigned(FLogTo) then
    try
      FLogTo.Log(Sender, LogClass, LogMsg);
    except
      if not (loIgnoreLogFailure in FLogOptions) then
        raise;
    end;

  Case LogClass of
    lcDebug   : if loIgnoreClassDebug in FLogOptions then exit;
    lcInfo    : if loIgnoreClassInfo in FLogOptions then exit;
    lcError   : if loIgnoreClassError in FLogOptions then exit;
    lcWarning : if loIgnoreClassWarning in FLogOptions then exit;
  end;

  try
    if loCheckRepeats in FLogOptions then
      begin
        if LogMsg = FLastLogMsg then
          begin
            Inc(FLogRepeatCount);
            exit;
          end;
        if FLogRepeatCount > 0 then
          begin
            I := FLogRepeatCount + 1;
            FLogRepeatCount := 0;
            Log(self, lcEndRepeat, IntToStr(I) + ' times');
          end;
        FLastLogMsg := LogMsg;
      end;

    S := LogMsg;
    if Assigned(FOnEditMessage) then
      FOnEditMessage(Sender, LogClass, S);

    if not (loNoLogEvent in FLogOptions) and Assigned(FOnLog) then
      FOnLog(Sender, LogClass, S);

    {$IFDEF OS_WIN32}
    if loLogMilliSecDiff in FLogOptions then
      begin
        T := GetTickCount;
        S := PadLeft(IntToStr(T - FLastLog), ' ', 4, False) + ' ' + S;
        FLastLog := T;
      end;
    {$ENDIF}

    if [loLogDate, loLogTime] * FLogOptions <> [] then
      begin
        N := Now;
        if loLogTime in FLogOptions then
          S := FormatDateTime('hhnnss', N) + ' ' + S;
        if loLogDate in FLogOptions then
          S := FormatDateTime('yymmdd', N) + ' ' + S;
      end;

    TriggerLogMsg(Sender, LogClass, S);

    {$IFDEF OS_WIN32}
    if loLogToDebugLog in FLogOptions then
      OutputDebugString(PChar(S));
    {$ENDIF}

    if loLogToFile in FLogOptions then
      begin
        if FLogFileName = '' then
          exit;
        F := True;
        if Assigned(FOnLogFile) then
          FOnLogFile(Sender, LogClass, S, F);
        if not F then
          exit;

        R := False;
        if not Assigned(FLogFile) then
          try
            FLogFile := TFileStream.Create(FLogFileName, fmOpenReadWrite);
            R := True;
          except
            FLogFile := TFileStream.Create(FLogFileName, fmCreate);
          end;
        if R then
          FLogFile.Seek(0, soFromEnd);

        try
          if S <> '' then
            FLogFile.Write(Pointer(S)^, Length(S));
          FLogFile.Write(CRLF, Length(CRLF));
        finally
          if not (loKeepFileOpen in FLogOptions) then
            FreeAndNil(FLogFile);
        end;
      end;
  except
    if not (loIgnoreLogFailure in FLogOptions) then
      raise;
  end;
end;

procedure TLog.Log(const LogClass: TLogClass; const LogMsg: String);
begin
  Log(self, LogClass, LogMsg);
end;

procedure TLog.Log(const LogMsg: String);
begin
  Log(lcInfo, LogMsg);
end;

procedure TLog.LogDebug(const LogMsg: String);
begin
  Log(lcDebug, LogMsg);
end;

procedure TLog.LogError(const LogMsg: String);
begin
  Log(lcError, LogMsg);
end;

procedure TLog.LogWarning(const LogMsg: String);
begin
  Log(lcWarning, LogMsg);
end;

procedure TLog.DeleteLogFile;
begin
  if FLogFileName = '' then
    exit;
  FreeAndNil(FLogFile);
  SysUtils.DeleteFile(FLogFileName);
end;

procedure TLog.LoadLogFileInto(const Destination: TStrings; const Size: Integer);
var S : Int64;
    C : Integer;
    L : String;
begin
  Destination.Clear;
  if Size = 0 then
    exit;
  // Open file
  FreeAndNil(FLogFile);
  try
    FLogFile := TFileStream.Create(FLogFileName, fmOpenReadWrite);
  except
    exit;
  end;
  S := FLogFile.Size;
  if S = 0 then
    exit;
  // Read
  if Size < 0 then
    C := S else
    C := MinI(Size, S);
  FLogFile.Position := S - C;
  SetLength(L, C);
  FLogFile.Read(Pointer(L)^, C);
  // Remove incomplete first line
  TrimLeftInPlace(L, csComplete - [#13, #10]);
  TrimLeftInPlace(L, [#13, #10]);
  // Set text
  Destination.Text := L;
end;

function TLog.GetLogLevel: Integer;
begin
  Result := FLevels.Count;
end;

procedure TLog.Enter(const Level: String);
begin
  if Assigned(FLogTo) then
    FLogTo.Enter(Level);
  FLevels.Add(Level);
end;

procedure TLog.Leave;
var I: Integer;
begin
  if Assigned(FLogTo) then
    FLogTo.Leave;
  I := FLevels.Count;
  if I <= 0 then
    exit;
  FLevels.Delete(I - 1);
end;



{                                                                              }
{ Application Log                                                              }
{                                                                              }
var
  FAppLog: TLog = nil;

function AppLog: TLog;
begin
  if not Assigned(FAppLog) then
    begin
      FAppLog := TLog.Create(nil);
      FAppLog.LogFileName := ChangeFileExt(ParamStr(0), '.log');
      FAppLog.LogOptions := [
          loLogToFile,
          loLogDate, loLogTime
          {$IFNDEF DEBUG}, loIgnoreLogFailure, loIgnoreClassDebug{$ENDIF}
                            ];
    end;
  Result := FAppLog;
end;



initialization
finalization
  FreeAndNil(FAppLog);
end.

