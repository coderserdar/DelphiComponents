unit LogFile;

{* Program : LogFile.Pas
   Purpose : TLogFile Component

   Author            Version       Last Changed
   ------            -------       ------------
   Song Weng Sam     1.00          Aug. 14, 1996
   Jaro Benes        1.01          Apr. 04, 2011
 *}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs;

type

  TOnLogEvent = procedure(Sender: TObject; var MsgStr: string) of object;
  TLogFile = class(TComponent)
  private
    { Private declarations }
    FLogFile: string;
    FLogName: string;
    FEnabled: Boolean;
    FOldFormat: Boolean;
    FOnLog: TOnLogEvent; //JB.
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure LogToFile(outstr: string);
  published
    { Published declarations }
    property OldFormat: Boolean read FOldFormat write FOldFormat default False;
    property LogFile: string read FLogFile write FLogFile;
    property LogName: string read FLogName write FLogName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property OnLog: TOnLogEvent read FOnLog write FOnLog; //JB.
  end;

procedure Register;

implementation

{  TLogFile  }

constructor TLogFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogFile := '';
  FOldFormat := False;
end;

procedure TLogFile.LogToFile(outstr: string);
var
  LogFile: Text;
  S: string;
begin
  if not FEnabled then Exit;
  if FLogFile = '' then Exit;
  AssignFile(LogFile, FLogFile);
  if FileExists(FLogFile) then
    Append(LogFile)
  else
    Rewrite(LogFile);
    //for better format with miliseconds //JB.
    if not FOldFormat then
      S := Format('%s "%s" > %s', [SysUtils.FormatDateTime('dd.mm.yyyy hh:nn:ss.zzz', Now), FLogName, outstr])
    else
      S := DateToStr(date) + ' ' + TimeToStr(Time) + ' "' + FLogName + '" >' + outstr;
  if Assigned(FOnLog) then
    FOnLog(Self, S);
  Writeln(LogFile, S);
  Flush(LogFile);
  CloseFile(LogFile);
end;

procedure Register;
begin
  RegisterComponents('System', [TLogFile]);
end;

end.