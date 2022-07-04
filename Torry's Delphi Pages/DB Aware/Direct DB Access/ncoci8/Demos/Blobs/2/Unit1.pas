// --------------------------------------------------------
// To run this demo you must at first run script: BLOBS2.SQL
// Also replace hard coded file name to load.
// --------------------------------------------------------
// This demo shows how to manually load files into BLOB.
// Take a look on follows two event handlers:
// Button1Click - Manual method (it works properly)
// Button2Click - Automatic method (it does not work, due to
//                bug in NCOCI8)

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, ExtCtrls, DBCtrls, Grids, DBGrids,
  StdCtrls, NCOciUpdateSQL, NCSQLMon;

type
  TForm1 = class(TForm)
    Database: TOCIDatabase;
    DBNavigator1: TDBNavigator;
    Query: TOCIQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Button1: TButton;
    StoredProc: TOCIStoredProc;
    Button2: TButton;
    OCIUpdateSQL1: TOCIUpdateSQL;
    NCSQLMonitorClient1: TNCSQLMonitorClient;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
    s: TStream;
    fs: TFileStream;
begin
    // -----------------------------------
    // Manual method (it works properly)
    // -----------------------------------
    Database.StartTransaction;
    try
        fs := TFileStream.Create('j:\db.zip', fmOpenRead);
        try
            StoredProc.Params[0].AsFloat := Query.Fields[0].AsFloat;
            StoredProc.ExecProc;
            try
                s := StoredProc.Params[1].CreateBlobStream(0, bmWrite);
                s.CopyFrom(fs, 0);
            finally
                s.Free;
            end;
        finally
            fs.free;
        end;
        Database.Commit;
    except
        Database.Rollback;
        raise;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    s: TStream;
    fs: TFileStream;
begin
    // -----------------------------------
    // Automatic method (it does not work)
    // -----------------------------------
    fs := TFileStream.Create('j:\db.zip', fmOpenRead);
    try
        Query.Edit;
        try
            try
                s := Query.CreateBlobStream(Query.Fields[1], bmWrite);
                s.CopyFrom(fs, 0);
            finally
                s.Free;
            end;
            Query.Post;
        except
            Query.Cancel;
            raise;
        end;
    finally
        fs.free;
    end;
end;

end.
