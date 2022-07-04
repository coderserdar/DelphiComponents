unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, StdCtrls, SmExport, ComCtrls;

type
  TExportExcel = class(TForm)
    SmExportToExcel1: TSmExportToExcel;
    Button1: TButton;
    ADOTable1: TADOTable;
    PB: TProgressBar;
    procedure Button1Click(Sender: TObject);
    procedure SmExportToExcel1ExportProgress(Sender: TObject;
      Progress: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ExportExcel: TExportExcel;

implementation

{$R *.dfm}

procedure TExportExcel.Button1Click(Sender: TObject);
begin
  cursor := crHourGlass;
  ADOTable1.Active:= true;
  SmExportToExcel1.Execute;
  cursor := crDefault;
end;

procedure TExportExcel.SmExportToExcel1ExportProgress(Sender: TObject; Progress: Integer);
begin
  PB.Position := Progress;
end;

end.
