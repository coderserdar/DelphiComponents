unit radabout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TradAboutForm = class(TForm)
    btOk: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Bevel1: TBevel;
    lbSite: TLabel;
    lbMail: TLabel;
    Image1: TImage;
    procedure lbSiteClick(Sender: TObject);
    procedure lbMailClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses shdocvw;

procedure TradAboutForm.lbSiteClick(Sender: TObject);
var
    Browser: TWebBrowser;
    flags: OleVariant;
begin
    flags := navOpenInNewWindow;
    Browser := TWebBrowser.Create(Application);
    Browser.Navigate('http://www.geocities.com/truerad_2000', flags);
end;

procedure TradAboutForm.lbMailClick(Sender: TObject);
var
    Browser: TWebBrowser;
begin
    Browser := TWebBrowser.Create(Application);
    Browser.Navigate('mailto:truerad2000@yahoo.com');
end;

end.
