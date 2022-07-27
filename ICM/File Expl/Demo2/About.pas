unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, ShellAPI, Buttons;

type
  TForm2 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Panel2: TPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}


procedure TForm2.BitBtn1Click(Sender: TObject);
begin
  close;
end;

procedure TForm2.Label8Click(Sender: TObject);
var
  param: ansistring;
begin
  param := Label8.Caption + #0;
  ShellExecute(0,
               'open',
               pChar(Param),
               nil,
               nil,
               SW_SHOWNORMAL);
end;  //884

end.

