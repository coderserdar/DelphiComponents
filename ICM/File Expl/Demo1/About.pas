unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Buttons, ShellAPI;

type
  TAboutForm = class(TForm)
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
    procedure BitBtn1Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.DFM}


procedure TAboutForm.BitBtn1Click(Sender: TObject);
begin
  close;
end;

procedure TAboutForm.Label7Click(Sender: TObject);  //880>
var
  param: ansistring;
begin
  param := Label7.Caption + #0;
  ShellExecute(0,
               'open',
               pChar(Param),
               nil,
               nil,
               SW_SHOWNORMAL);
end;  //880


end.

