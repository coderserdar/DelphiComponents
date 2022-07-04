unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ShellAPI;

type
  TAboutBox = class(TForm)
    pnlBottom: TPanel;                 
    pbOk: TBitBtn;
    pnlMain: TPanel;
    pnlLeft: TPanel;
    pnlClient: TPanel;
    lbWWW1: TLabel;
    lbEmail1: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    LogoImage: TImage;
    procedure lbWWWClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation
{$R *.DFM}


procedure TAboutBox.lbWWWClick(Sender: TObject);
begin
 ShellExecute(Application.Handle, 'open', PChar((Sender as TLabel).Caption),nil, nil, SW_SHOW);
end;


end.
