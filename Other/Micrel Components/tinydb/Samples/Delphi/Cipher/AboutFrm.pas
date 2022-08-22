unit AboutFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TAboutForm = class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutForm: TAboutForm;

procedure ShowAboutForm;

implementation

{$R *.DFM}

procedure ShowAboutForm;
var
  Frm: TAboutForm;
begin
  Frm := TAboutForm.Create(Application);
  Frm.ShowModal;
  Frm.Free;
end;

end.
