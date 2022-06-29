unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ATFileNotificationSimple, TntStdCtrls, TntDialogs;

type
  TFormMain = class(TForm)
    edFileName: TTntEdit;
    btnBrowseFile: TButton;
    Label2: TLabel;
    OpenDialog1: TTntOpenDialog;
    btnWatchFile: TButton;
    btnClose: TButton;
    Label1: TLabel;
    Label3: TLabel;
    edDelay: TEdit;
    procedure btnBrowseFileClick(Sender: TObject);
    procedure btnWatchFileClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FileChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Notif: TATFileNotificationSimple;
    procedure NotifyFile;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Notif := TATFileNotificationSimple.Create(Self);
  Notif.OnChanged := FileChanged; 
end;

procedure TFormMain.NotifyFile;
begin
  with Notif do
    begin
    Timer.Enabled := False;
    Timer.Interval := StrToIntDef(edDelay.Text, Timer.Interval);
    FileName := edFileName.Text;
    Timer.Enabled := True;
    end;
end;

procedure TFormMain.FileChanged(Sender: TObject);
begin
  Application.MessageBox(
    'File has been changed.'#13+
    'To continue file watching, close this message box.',
    'Notification',
    MB_ICONWARNING or MB_TOPMOST);
end;

procedure TFormMain.btnBrowseFileClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      edFileName.Text := FileName;
end;

procedure TFormMain.btnWatchFileClick(Sender: TObject);
begin
  if edFileName.Text<>'' then
    NotifyFile;
end;

procedure TFormMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
