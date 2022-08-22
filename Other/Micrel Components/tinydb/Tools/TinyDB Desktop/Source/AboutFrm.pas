unit AboutFrm;

interface

uses
  Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ShellApi, BaseFrm;

type
  TAboutForm = class(TBaseForm)
    OkButton: TButton;
    GroupBox1: TGroupBox;
    SoftNameLabel: TLabel;
    VersionInfoLabel: TLabel;
    Image3: TImage;
    Image4: TImage;
    CompanyLabel: TLabel;
    EMailPanel: TPanel;
    URLPanel: TPanel;
    DateLabel: TLabel;
    procedure EMailPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure URLPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure EMailPanelClick(Sender: TObject);
    procedure URLPanelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

procedure ShowAboutForm;

implementation

uses MainFrm, LangMgr;

{$R *.DFM}

procedure ShowAboutForm;
var
  Frm: TAboutForm;
begin
  Frm := TAboutForm.Create(Application);
  Frm.ShowModal;
  Frm.Free;
end;

procedure TAboutForm.EMailPanelMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if (X >= 0) and (X < EMailPanel.Width) and (Y >= 0) and (Y < EMailPanel.Height) then
  begin
    EMailPanel.Font.Style := [fsUnderline];
    EMailPanel.Font.Color := clRed;
    SetCapture(EMailPanel.Handle);
  end
  else
  begin
    EMailPanel.Font.Style := [];
    EMailPanel.Font.Color := clBlue;
    ReleaseCapture;
  end;
end;

procedure TAboutForm.URLPanelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if (X >= 0) and (X < URLPanel.Width) and (Y >= 0) and (Y < URLPanel.Height) then
  begin
    URLPanel.Font.Style := [fsUnderline];
    URLPanel.Font.Color := clRed;
    SetCapture(URLPanel.Handle);
  end
  else
  begin
    URLPanel.Font.Style := [];
    URLPanel.Font.Color := clBlue;
    ReleaseCapture;
  end;
end;

procedure TAboutForm.EMailPanelClick(Sender: TObject);
var
  S: string;
begin
  S := 'About TinyDB ' + STDBVersion;
  ShellExecute(Handle, 'Open', PChar('mailto:haoxg@21cn.com?subject="' + S + '"'), '', '', 1);
end;

procedure TAboutForm.URLPanelClick(Sender: TObject);
begin
  ShellExecute(Self.Handle, 'Open', PChar('http://www.tinydb.com'), '', '', 1);
end;

end.
