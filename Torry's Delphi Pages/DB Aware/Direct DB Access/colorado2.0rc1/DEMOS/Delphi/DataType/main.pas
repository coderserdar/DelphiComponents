unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  colorado, Db, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids, ExtDlgs;

type
  TMainForm = class(TForm)
    Connection1: TConnection;
    CTable1: TCTable;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBMemo1: TDBMemo;
    DBImage1: TDBImage;
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
     begin
       CTable1.Edit;
       TBlobField(CTable1.FieldByName('Blob')).
            LoadFromFile(OpenPictureDialog1.FileName);
     end;
end;

end.
