unit DemoAb;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TfrmAbout = class(TForm)
    Bevel1: TBevel;
    OKBtn: TButton;
    HelpBtn: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    Label4: TLabel;
    procedure HelpBtnClick(Sender: TObject);
    procedure Label4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses DemoReg;

procedure TfrmAbout.HelpBtnClick(Sender: TObject);
begin
   Application.HelpCommand(3,0);
end;

procedure TfrmAbout.Label4Click(Sender: TObject);
begin
   WriteToUs3;
end;

end.
