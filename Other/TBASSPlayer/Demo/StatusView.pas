// unit StatusView
//
// This unit takes charge of showing user the latest status of Demo program.
//
//       written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.0                        19 Jan 2009
//   - Initial release


unit StatusView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TStatusViewForm = class(TForm)
    InfoMemo: TMemo;
    btnClose: TButton;
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StatusViewForm: TStatusViewForm;

implementation

{$R *.DFM}

procedure TStatusViewForm.btnCloseClick(Sender: TObject);
begin
   Close;
end;

end.
