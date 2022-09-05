unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_DSet, FR_Class, DB;

type
  TForm4 = class(TForm)
    frReport1: TfrReport;
    MasterDataset: TFRUserDataset;
    DetailDataset: TFRUserDataset;
    procedure MasterDatasetFirst(Sender: TObject);
    procedure MasterDatasetCheckEOF(Sender: TObject; var EOF: Boolean);
    procedure DetailDatasetFirst(Sender: TObject);
    procedure DetailDatasetNext(Sender: TObject);
    procedure DetailDatasetCheckEOF(Sender: TObject; var EOF: Boolean);
    procedure MasterDatasetNext(Sender: TObject);
  private
    { Private declarations }
    Bookmark: TBookmarkStr;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

uses datasets;

{$R *.DFM}

var LastLetter: Char;

procedure TForm4.MasterDatasetFirst(Sender: TObject);
begin
  CustomerData.Customers.Open;
  CustomerData.Customers.First;
  Bookmark := CustomerData.Customers.Bookmark;
end;

procedure TForm4.MasterDatasetNext(Sender: TObject);
begin
  Bookmark := CustomerData.Customers.Bookmark;
end;

procedure TForm4.MasterDatasetCheckEOF(Sender: TObject; var EOF: Boolean);
begin
  EOF := EOF or CustomerData.Customers.EOF;
end;

procedure TForm4.DetailDatasetFirst(Sender: TObject);
begin
  CustomerData.Customers.Bookmark := Bookmark;
  LastLetter := CustomerData.CustomersCompany.AsString[1];
end;

procedure TForm4.DetailDatasetNext(Sender: TObject);
begin
  CustomerData.Customers.Next;
end;

procedure TForm4.DetailDatasetCheckEOF(Sender: TObject; var EOF: Boolean);
begin
  EOF := EOF or (CustomerData.Customers.EOF or
    (CustomerData.CustomersCompany.AsString[1] <> LastLetter));
end;

end.
