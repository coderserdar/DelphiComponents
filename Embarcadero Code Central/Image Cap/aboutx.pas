unit aboutx;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,shellapi;

type
  TAboutDlg = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Label7Click(Sender: TObject);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  AboutDlg: TAboutDlg;

implementation

{$R *.DFM}

procedure TAboutDlg.Label7Click(Sender: TObject);
begin
 shellexecute(handle,'open',Pchar('mailto:'+TLabel(Sender).Caption),nil,nil,SW_SHOW);
end;

end.
