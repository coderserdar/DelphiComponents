unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, ceflib, FMX.Edit, ceffmx;

type
  TMainForm = class(TForm)
    crm: TChromiumFMX;
    btPrev: TButton;
    btNext: TButton;
    btHome: TButton;
    btReload: TButton;
    btLaunch: TButton;
    edAddress: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btLaunchClick(Sender: TObject);
    procedure btHomeClick(Sender: TObject);
    procedure btNextClick(Sender: TObject);
    procedure btPrevClick(Sender: TObject);
    procedure btReloadClick(Sender: TObject);
    procedure crmAddressChange(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; const url: ustring; out Result: Boolean);
    procedure crmLoadEnd(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame; httpStatusCode: Integer; out Result: Boolean);
    procedure crmLoadStart(Sender: TObject; const browser: ICefBrowser;
      const frame: ICefFrame);
    procedure crmTitleChange(Sender: TObject; const browser: ICefBrowser;
      const title: ustring; out Result: Boolean);
    procedure edAddressKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    FLoading: Boolean;
    procedure doOnIdleEvent(Sender: TObject; var Done: Boolean);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.btHomeClick(Sender: TObject);
begin
  if crm.Browser <> nil then
    crm.Browser.MainFrame.LoadUrl(crm.DefaultUrl);
end;

procedure TMainForm.btLaunchClick(Sender: TObject);
begin
  if crm.Browser <> nil then
    crm.Browser.MainFrame.LoadUrl(edAddress.Text);
end;

procedure TMainForm.btNextClick(Sender: TObject);
begin
  if crm.Browser <> nil then
    crm.Browser.GoForward;
end;

procedure TMainForm.btPrevClick(Sender: TObject);
begin
  if crm.Browser <> nil then
    crm.Browser.GoBack;
end;

procedure TMainForm.btReloadClick(Sender: TObject);
begin
  if crm.Browser <> nil then
    if FLoading then
      crm.Browser.StopLoad else
      crm.Browser.Reload;
end;

procedure TMainForm.crmAddressChange(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; const url: ustring; out Result: Boolean);
begin
  if ((frame = nil) or (frame.IsMain)) then
    edAddress.Text := url;
end;

procedure TMainForm.crmLoadEnd(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame; httpStatusCode: Integer; out Result: Boolean);
begin
  if (browser <> nil) and ((frame = nil) or (frame.IsMain)) then
  begin
    btReload.Text := 'R';
    FLoading := False;
  end;
end;

procedure TMainForm.crmLoadStart(Sender: TObject; const browser: ICefBrowser;
  const frame: ICefFrame);
begin
  if (browser <> nil) and ((frame = nil) or (frame.IsMain)) then
  begin
    btReload.Text := 'X';
    FLoading := True;
  end;
end;

procedure TMainForm.crmTitleChange(Sender: TObject; const browser: ICefBrowser;
  const title: ustring; out Result: Boolean);
begin
  Caption := title;
end;

procedure TMainForm.doOnIdleEvent(Sender: TObject; var Done: Boolean);
begin
  CefDoMessageLoopWork;
end;

procedure TMainForm.edAddressKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    if crm.Browser <> nil then
      crm.Browser.MainFrame.LoadUrl(edAddress.Text);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnIdle := doOnIdleEvent;
  FLoading := False;
end;

end.
