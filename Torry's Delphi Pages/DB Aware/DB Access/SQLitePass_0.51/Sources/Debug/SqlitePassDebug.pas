unit SqlitePassDebug;

interface

uses

  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  {$IFDEF FPC}
  LResources,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSqlitePassDebuggerOutputType = (otScreen, otFile);

  TFormDebugOutput = class(TForm)
    PanelTop: TPanel;
    BtnClearMemo: TButton;
    MemoDebug: TMemo;
    CbLockScreen: TCheckBox;
    procedure BtnClearMemoClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  TSqlitePassDebugger = class(TObject)
  private
   FDebugForm: TFormDebugOutput;
   FOutputType: TSqlitePassDebuggerOutputType;
   FDisplayTime: Boolean;
  public
   constructor Create;
   destructor Destroy; override;
   Procedure Display(OutputStr: String);
   Procedure SaveToFile(FileName: String);
   Property Form: TFormDebugOutput read FDebugForm;
   Property DisplayTime: Boolean read FDisplayTime write FDisplayTime;
   Property OutputType: TSqlitePassDebuggerOutputType read FOutputType write FOutputType;
  end;
var
  FormDebugOutput: TFormDebugOutput;

implementation
{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TFormDebugOutput.BtnClearMemoClick(Sender: TObject);
begin
MemoDebug.Clear;
end;

{ TSqlitePassDebugger }

constructor TSqlitePassDebugger.Create;
begin
inherited;
FDebugForm := TFormDebugOutput.Create(nil);
FDebugForm.Show;
FDisplayTime := True;
FOutputType := otScreen;
end;

destructor TSqlitePassDebugger.Destroy;
begin
FDebugForm.Free;
inherited;
end;

procedure TSqlitePassDebugger.Display(OutputStr: String);
begin
if Form.CbLockScreen.Checked then Exit;
if FDisplayTime
   then OutputStr := '['+ TimeToStr(Now) + '] ' + OutputStr;
if FOutputType = otScreen
   then FDebugForm.MemoDebug.Lines.Add(OutputStr);
end;

procedure TSqlitePassDebugger.SaveToFile(FileName: String);
begin
FDebugForm.MemoDebug.Lines.SaveToFile(FileName);
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassDebug.lrs}
 {$ENDIF}
end.