unit ErrorDlg;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, VQBLocalize;

type
  TSQLErrorDlg = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Label2: TLabel;
    Memo1: TMemo;
    Label1: TLabel;
  Public
    Constructor Create(AOwner: TComponent); override;
  end;

Procedure ShowSQLError(ErrorCode, Discription: String);

implementation

{$R *.DFM}

Procedure ShowSQLError(ErrorCode, Discription: String);
var
  ErrorDlg: TSQLErrorDlg;
begin
   ErrorDlg:= TSQLErrorDlg.Create(Application);
   With ErrorDlg do
   begin
      Label2.Caption:= ErrorCode;
      Memo1.Text:= Discription;
      MessageBeep(MB_ICONEXCLAMATION);
      ShowModal;
      Free;
   end;
end;

{ TSQLErrorDlg }

constructor TSQLErrorDlg.Create(AOwner: TComponent);
begin
  inherited;
  Label1.Caption:= resErrDetailMessage;
  Caption:= resErrDlgTitle;
end;

end.
