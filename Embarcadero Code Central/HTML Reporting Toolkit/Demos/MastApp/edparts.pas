unit Edparts;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, DBTables, DB, StdCtrls, ExtCtrls, Mask, DBCtrls, DBLookup,
  Buttons;

type
  TEdPartsForm = class(TForm)
    Panel1: TPanel;
    Navigator: TDBNavigator;
    Panel2: TPanel;
    Label1: TLabel;
    DBEdit2: TDBEdit;
    DBEdit4: TDBEdit;
    DBEdit5: TDBEdit;
    DBEdit7: TDBEdit;
    DBEdit8: TDBEdit;
    DBEdPartNo: TDBEdit;
    Label6: TLabel;
    DBEdit3: TDBEdit;
    PrintBtn: TSpeedButton;
    DataComboBox1: TDBLookupComboBox;
    OKButton: TButton;
    CancelButton: TButton;
    Bevel1: TBevel;
    PartsSource1: TDataSource;
    Vendors: TTable;
    VendorSource: TDataSource;
    procedure PrintBtnClick(Sender: TObject);
  public
    procedure Edit(PartNo: Double);
  end;

var
  EdPartsForm: TEdPartsForm;

implementation

{$R *.DFM}

uses BrPartsImpl;


procedure TEdPartsForm.Edit(PartNo: Double);
begin
  PartsSource1.DataSet.Open;
  PartsSource1.DataSet.Locate('PartNo', PartNo, []);
  ShowModal;
end;

procedure TEdPartsForm.PrintBtnClick(Sender: TObject);
begin
  if MessageDlg('   Print this form?', mtConfirmation, [mbYes,mbNo], 0)
    = mrYes then Print;
end;

end.
