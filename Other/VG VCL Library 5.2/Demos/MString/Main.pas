{ TMoneyString demo }

{ To change active language copy vgVCLRes.pas and vgVCLRes.res }
{ from \RESOURSE\ into project directory                       }

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgStndrt;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    edValue: TEdit;
    Label2: TLabel;
    edVerbal: TEdit;
    ms: TMoneyString;
    procedure edValueChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.edValueChange(Sender: TObject);
begin
  try
    edVerbal.Text := ms.MoneyToString(StrToFloat(edValue.Text));
  except
    edVerbal.Text := Exception(ExceptObject).Message;
  end;
end;

end.
