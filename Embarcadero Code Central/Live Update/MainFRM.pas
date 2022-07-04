unit MainFRM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, NetUpdate;

type
  TMainForm = class(TForm)
    btnUpdate: TSpeedButton;
    Label1: TLabel;
    NetUpdate: TNetUpdate;
    procedure btnUpdateClick(Sender: TObject);
    procedure NetUpdateUpdateEnd(Sender: TObject; FName: String;
      Error: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}


procedure TMainForm.btnUpdateClick(Sender: TObject);
begin
  NetUpdate.Execute;
end;

procedure TMainForm.NetUpdateUpdateEnd(Sender: TObject; FName: String; Error: Integer);

begin
  ShowMessage('Download finished' + #13#10 + 'file name: ' + FName + #13#10 + 'error: ' + IntToStr(Error));
end;

end.
