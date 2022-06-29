//  FastReport 2.3 demo.
//
//  Demonstrates end-user reporting framework.
//  Requires TfrDataStorage & TfrDesigner components.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_Desgn, FR_Class, FRD_Mngr;

type
  TForm1 = class(TForm)
    frReport1: TfrReport;
    frDataStorage1: TfrDataStorage;
    frDesigner1: TfrDesigner;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  frVariables['From'] := 1000;
  frVariables['To'] := 2000;
end;

end.
