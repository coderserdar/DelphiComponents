unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ArchiverRoot, CustExtractor, CustArchiver, Archiver, StdCtrls, Buttons,
  Db, DBTables, Grids, DBGrids, DBCtrls, ExtCtrls;

type
  TMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    Table1: TTable;
    DataSource1: TDataSource;
    Table1Id_Archive: TAutoIncField;
    Table1Name: TStringField;
    Table1Path: TMemoField;
    Table1Date: TDateTimeField;
    Table1Size: TIntegerField;
    Table1Data: TBlobField;
    Table1PathStr: TStringField;
    BitBtn1: TBitBtn;
    btnAdd: TBitBtn;
    btnExtract: TBitBtn;
    btnDisplay: TBitBtn;
    Archiver1: TArchiver;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnDisplayClick(Sender: TObject);
    procedure Archiver1DisplayMessage(Sender: TObject; const msg: String);
    procedure Archiver1FileProgress(Sender: TObject; Percent: Integer);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
    procedure FormCreate(Sender: TObject);
    procedure Table1CalcFields(DataSet: TDataSet);
    procedure Archiver1StartOperation(Sender: TObject);
    procedure Archiver1FinishOperation(Sender: TObject);
    procedure Archiver1Enumeration(Sender: TObject;
      const FileEntry: TFileEntry);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Main: TMain;

implementation
uses fmProgress, FileCtrl, ArchiverMisc, fmDisplayArchive;
{$R *.DFM}

procedure TMain.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Archiver1.IsBusy;
end;

procedure TMain.btnAddClick(Sender: TObject);
var
  dir : String;
  B : TBlobStream;
begin
  if Archiver1.IsBusy then
    begin
      Progress.Show;
      Exit;
    end;
  if SelectDirectory( dir, [], 0 ) then
    begin
      if UpperCase(AppendSlash(dir)) = UpperCase(ExtractFilePath(Application.ExeName)) then
        begin
          MessageDlg( 'You can''t add the folder where your database is located !', mtWarning, [mbOk], 0 );
          Exit;
        end;
      Table1.Append;
      try
        Table1Date.Value := Now;
        Table1Path.Value := dir;
        Table1Name.Value := ExtractFileName(RemoveSlash(dir));
        B := TBlobStream.Create( Table1Data, bmReadWrite );
        try
          B.Truncate;
          Archiver1.Stream := B;
          Archiver1.Open;
          try
            Archiver1.AddDirectory( dir );
            Table1Size.Value := Round(Archiver1.Header.ArchiveInfo.Size);
          finally
            Archiver1.Close;
          end;
        finally
          B.Free;
        end;
      except
        Table1.Cancel;
        raise;
      end;
    end;
end;

procedure TMain.btnExtractClick(Sender: TObject);
var
  dir : String;
  B : TBlobStream;
begin
  if Archiver1.IsBusy then
    begin
      Progress.Show;
      Exit;
    end;
  if SelectDirectory( dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0 ) then
    begin
      Archiver1.ExtractPath := dir;
      B := TBlobStream.Create( Table1Data, bmRead );
      try
        Archiver1.Stream := B;
        try
          Archiver1.Open;
          Archiver1.ExtractFiles;
        finally
          Archiver1.Close;
        end;
      finally
        B.Free;
      end;
    end;
end;

procedure TMain.btnDisplayClick(Sender: TObject);
var
  B : TBlobStream;
begin
  if Archiver1.IsBusy then
    begin
      Progress.Show;
      Exit;
    end;
  B := TBlobStream.Create( Table1Data, bmRead );
  try
    Archiver1.Stream := B;
    try
      Archiver1.Open;
      Archiver1.EnumerateFiles;
    finally
      Archiver1.Close;
    end;
  finally
    B.Free;
  end;
  DisplayArchive.Caption := Format('Content of archive %s', [Table1Name.AsString]);
  DisplayArchive.ShowModal;
end;

procedure TMain.Archiver1DisplayMessage(Sender: TObject;
  const msg: String);
begin
  Progress.Label1.Caption := msg;
end;

procedure TMain.Archiver1FileProgress(Sender: TObject; Percent: Integer);
begin
  Progress.ProgressBar1.Position := Percent;
  Application.ProcessMessages;
end;

procedure TMain.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  with (Sender as TDatasource).Dataset do
    begin
      btnExtract.Enabled := not FieldByName('Id_Archive').IsNull;
      btnDisplay.Enabled := btnExtract.Enabled;
    end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  Table1.DatabaseName := ExtractFilePath(Application.ExeName);
  Table1.Open;
end;

procedure TMain.Table1CalcFields(DataSet: TDataSet);
begin
  Table1PathStr.Value := Table1Path.Value;
end;

procedure TMain.Archiver1StartOperation(Sender: TObject);
begin
  Progress.Show;
  DisplayArchive.ListBox1.Clear;
end;

procedure TMain.Archiver1FinishOperation(Sender: TObject);
begin
  Progress.Hide;
end;

procedure TMain.Archiver1Enumeration(Sender: TObject;
  const FileEntry: TFileEntry);
begin
  DisplayArchive.ListBox1.Items.Add(
     Format('%s (%d)', [FileEntry.Name, FileEntry.ArchiveInfo.Size]) );
end;

end.
