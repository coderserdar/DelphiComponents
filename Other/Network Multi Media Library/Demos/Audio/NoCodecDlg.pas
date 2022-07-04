unit NoCodecDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TfrmNoCodec = class(TForm)
    BitBtn1: TBitBtn;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lbCodecUrl: TLabel;
    procedure lbCodecUrlClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNoCodec: TfrmNoCodec;

implementation
uses ShellApi;
 
{$R *.dfm}

procedure TfrmNoCodec.lbCodecUrlClick(Sender: TObject);
begin
  ShellExecute((Self as TControl).Handle,PChar('open'),PChar(lbCodecUrl.Caption),nil,nil,0);
end;

end.
