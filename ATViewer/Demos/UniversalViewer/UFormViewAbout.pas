{$I ViewerOptions.inc}

unit UFormViewAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormViewAbout = class(TForm)
    PanelText: TPanel;
    PanelCaption: TPanel;
    labCaption: TLabel;
    labCopyright: TLabel;
    labHomepage: TLabel;
    labVersion: TLabel;
    PanelCredits: TPanel;
    labCredits: TLabel;
    memoCredits: TMemo;
    PanelButton: TPanel;
    btnOK: TButton;
    PanelImage: TPanel;
    Image1: TImage;
    labLicense: TLabel;
    PanelLogo: TPanel;
    Image2: TImage;
    procedure FormShow(Sender: TObject);
    procedure labHomepageClick(Sender: TObject);
    procedure labLicenseClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  ATxMsgProc, ATxFProc;

{$R *.DFM}

procedure TFormViewAbout.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewAbout.inc}
  {$ifndef HELP}
  labLicense.Enabled:= false;
  {$endif}
end;

procedure TFormViewAbout.labHomepageClick(Sender: TObject);
begin
  FOpenURL('http://www.uvviewsoft.com', Handle);
end;

procedure TFormViewAbout.labLicenseClick(Sender: TObject);
begin
  ShowHelp(Handle, 'LicenseFree.html');
end;

procedure TFormViewAbout.Image2Click(Sender: TObject);
begin
  FOpenURL('http://atorg.net.ru/delphi/', Handle);
end;

end.
