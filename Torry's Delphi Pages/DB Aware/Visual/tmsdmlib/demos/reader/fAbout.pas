unit fAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfmAbout = class(TForm)
    Memo1: TMemo;
    lbTMS: TLabel;
    Label1: TLabel;
    lbDevgems: TLabel;
    Label3: TLabel;
    btOk: TButton;
    Bevel1: TBevel;
    procedure lbTMSClick(Sender: TObject);
    procedure lbDevgemsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAbout: TfmAbout;

implementation
uses
  ShellApi;

{$R *.DFM}

procedure TfmAbout.lbTMSClick(Sender: TObject);
begin
 ShellExecute(0, 'open', PChar('http://www.tmssoftware.com'), nil, nil, SW_NORMAL);
end;

procedure TfmAbout.lbDevgemsClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar('http://www.tmssoftware.com/site/tmsdm.asp'), nil, nil, SW_NORMAL);
end;

end.

