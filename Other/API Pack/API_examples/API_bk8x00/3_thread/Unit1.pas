unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_abform, API_bk8x00, ComCtrls,
  StdCtrls, API_memo, API_msgdlg, API_tileimage, API_progressbar, API_base;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    API_bk8x001: TAPI_bk8x00;
    API_abform1: TAPI_abform;
    API_grbutton1: TAPI_grbutton;
    API_grbutton2: TAPI_grbutton;
    API_memo1: TAPI_memo;
    API_msgdlg1: TAPI_msgdlg;
    API_tileimage1: TAPI_tileimage;
    API_progressbar1: TAPI_progressbar;
    procedure API_grbutton1Click(Sender: TObject);
    procedure API_grbutton2Click(Sender: TObject);
    procedure API_bk8x001Close(Sender: TObject);
    procedure API_bk8x001AfterOpen(Sender: TObject);
    procedure API_bk8x001ThreadEvent(thread: TThread);
    procedure API_bk8x001Error(Sender: TObject; ErrNo: Integer;
      ErrorMsg: String);
  private
  public
    input: tbuffer;
    output: tbuffer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.API_grbutton1Click(Sender: TObject);
begin
  // show settings window
  api_bk8x001.showsettings;
  api_memo1.lines.insert(0,'settings modified.');
end;

procedure TForm1.API_grbutton2Click(Sender: TObject);
begin
  // set open state to opposite
  api_bk8x001.open:=not api_bk8x001.open;

  // set button names according to state
  api_grbutton2.LedState:=api_bk8x001.open;
  if api_bk8x001.open then
  begin
    api_grbutton2.Caption:='Stop';
    api_memo1.Lines.insert(0,'port opened.');
  end else
  begin
    api_grbutton2.Caption:='Start';
    api_memo1.lines.insert(0,'port closed.');
  end;
end;

procedure TForm1.API_bk8x001Close(Sender: TObject);
begin
  // deactivate thread before port is closed
  api_bk8x001.ThreadActive:=false;
  api_memo1.lines.insert(0,'thread deactivated.');
end;

procedure TForm1.API_bk8x001ThreadEvent(thread: TThread);
begin
  // readwrite in thread
  api_bk8x001.readwrite(input, output);
  api_progressbar1.Position:=api_progressbar1.position+1;
  if api_progressbar1.position>99 then api_progressbar1.position:=0;
end;

procedure TForm1.API_bk8x001AfterOpen(Sender: TObject);
begin
  // activate thread if when port was opened
  api_bk8x001.ThreadActive:=true;
  api_memo1.lines.insert(0,'thread activated.');
end;

procedure TForm1.API_bk8x001Error(Sender: TObject; ErrNo: Integer;
  ErrorMsg: String);
begin
  // add error to log
  api_memo1.Lines.insert(0,'error: '+errormsg);
  // show error as dialog
  api_msgdlg1.Caption:='Error';
  api_msgdlg1.Msg.Text:=errormsg;
  api_msgdlg1.Execute;
end;

end.
