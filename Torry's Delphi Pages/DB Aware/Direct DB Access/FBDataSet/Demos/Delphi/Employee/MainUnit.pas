unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, mydbUnit, FBCustomDataSet, jvuib, ComCtrls, ActnList, Grids,
  DBGrids;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    PageControl1: TPageControl;
    ActionList1: TActionList;
    TabSheet1: TTabSheet;
    tabDirectories: TTabSheet;
    PageControl2: TPageControl;
    tabSprCountry: TTabSheet;
    tabCustomer: TTabSheet;
    JvUIBDataBase1: TJvUIBDataBase;
    trRead: TJvUIBTransaction;
    trWrite: TJvUIBTransaction;
    quSprCountry: TFBDataSet;
    quSprCustomer: TFBDataSet;
    dsSprCountry: TDataSource;
    dsSprCustomer: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    procedure PageControl1Change(Sender: TObject);
    procedure PageControl2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.PageControl1Change(Sender: TObject);
begin
  PageControl2Change(nil);
end;

procedure TForm1.PageControl2Change(Sender: TObject);
begin
  quSprCountry.Active:=(PageControl2.ActivePage = tabSprCountry) and (PageControl1.ActivePage = tabDirectories);
  quSprCustomer.Active:=(PageControl2.ActivePage = tabCustomer) and (PageControl1.ActivePage = tabDirectories);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1Change(nil);
end;

end.
