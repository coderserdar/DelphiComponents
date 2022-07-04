unit fMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cSocketHostLookup;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    eHost: TEdit;
    Label2: TLabel;
    cLookupMethod: TComboBox;
    fndSocketHostLookup: TfndSocketHostLookup;
    bLookup: TButton;
    Memo1: TMemo;
    bCancel: TButton;
    procedure bLookupClick(Sender: TObject);
    procedure fndSocketHostLookupComplete(const Sender: TSocketHostLookup);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bLookupClick(Sender: TObject);
begin
  fndSocketHostLookup.Host := eHost.Text;
  fndSocketHostLookup.Method := TSocketHostLookupMethod (cLookupMethod.ItemIndex);
  fndSocketHostLookup.Lookup;
end;

procedure TForm1.fndSocketHostLookupComplete(
  const Sender: TSocketHostLookup);
var I : Integer;
begin
  if Sender.Success then
    begin
      Memo1.Lines.Add ('Success');
      For I := 0 to Sender.AddressCount - 1 do
        Memo1.Lines.Add (Sender.AddressStr [I]);
    end else
    Memo1.Lines.Add ('Error: ' + Sender.ErrorMessage);
end;

procedure TForm1.bCancelClick(Sender: TObject);
begin
  fndSocketHostLookup.Cancel;
end;

end.
