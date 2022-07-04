{ Copyright Federico Firenze }
{ webmaster@ffirenze.com.ar  }

{ Envíe un mail registrando su copia y recibirá novedades y actualizaciones  }

unit unMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DataToXLS, DataToAscii, DataExport, DataToDbf, DB, DBTables,
  StdCtrls, Grids, DBGrids, DataToWK1, DataToHTML;

type
  TfrmMain = class(TForm)
    tblContacts: TTable;
    dsContacts: TDataSource;
    DataToDbf1: TDataToDbf;
    DataToAscii1: TDataToAscii;
    DataToXLS1: TDataToXLS;
    DataToWK11: TDataToWK1;
    SaveDialog: TSaveDialog;
    dbgContacts: TDBGrid;
    btnExport: TButton;
    lbHelp: TLabel;
    chkCopyColumns: TCheckBox;
    DataToHTML1: TDataToHTML;
    procedure btnExportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnExportClick(Sender: TObject);
  procedure DoExportFile(AExport: TDataExport; AExtencion : String);
  begin
    if chkCopyColumns.Checked Then
      AExport.GetFields.Assign( dbgContacts.Columns )
    else
      AExport.GetFields.Clear; { Dynamic Fields }

    AExport.SaveToFile( ChangeFileExt( SaveDialog.FileName, AExtencion ) );
  end;
begin
  if SaveDialog.Execute Then
    case SaveDialog.FilterIndex of
      1: DoExportFile( DataToDbf1,   '.dbf' );
      2: DoExportFile( DataToAscii1, '.csv' );
      3: DoExportFile( DataToXLS1,   '.xls' );
      4: DoExportFile( DataToWK11,   '.wk1' );
      5: DoExportFile( DataToHTML1,  '.html' );
    end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  tblContacts.Open ;
end;

end.
