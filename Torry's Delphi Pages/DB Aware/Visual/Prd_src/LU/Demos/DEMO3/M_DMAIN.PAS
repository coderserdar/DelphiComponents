unit M_dmain;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Dblup2, DB, DBTables, ExtCtrls, DBCtrls, Grids,
  DBGrids, Mask;

type
  TFormMain = class(TForm)
    GroupBox1: TGroupBox;
    DBLookupComboPlus1: TDBLookupComboPlus;
    DataSourceVendors: TDataSource;
    TableVendors: TTable;
    GroupBox2: TGroupBox;
    DBLookupComboPlus2: TDBLookupComboPlus;
    DataSourceParts: TDataSource;
    TableParts: TTable;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label2: TLabel;
    Label1: TLabel;
    DBText1: TDBText;
    DBText2: TDBText;
    DBText4: TDBText;
    DBText5: TDBText;
    DBText6: TDBText;
    Label3: TLabel;
    Label4: TLabel;
    DBText7: TDBText;
    Label5: TLabel;
    DBText8: TDBText;
    Label6: TLabel;
    DBText9: TDBText;
    DBText12: TDBText;
    Label7: TLabel;
    GroupBox3: TGroupBox;
    Button1: TButton;
    Button2: TButton;
    Bevel3: TBevel;
    Label8: TLabel;
    Label9: TLabel;
    procedure DataSourceVendorsDataChange(Sender: TObject; Field: TField);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.DataSourceVendorsDataChange(Sender: TObject;
  Field: TField);
begin
 {This one line is cosmetic and is necessary because no DataSource
 or DataField is attached to the lookup combos for this demo. It
 insures that detail combo is blank when a new master record is choosen.}
  DBLookupComboPlus2.Text := '';
end;

procedure TFormMain.Button1Click(Sender: TObject);
{Create the index needed for the LookupIndex in the parts
lookupcombo. This index is removed by the other button so that
the demo files are left the way they were to begin with}
begin
  TableVendors.Active := False;
  TableParts.Active := False;
  TableParts.AddIndex('byVendorDescription',
                      'VendorNo;Description',
                      [ixCaseInsensitive]);
  MessageDlg('The byVendorDescription index was successfully created for PARTS.DB',
             mtConfirmation,
             [mbOK],0);
  TableParts.AddIndex('byVendorNoPartNo',
                      'VendorNo;PartNo',
                      [ixCaseInsensitive]);
  MessageDlg('The byVendorNoPartNo index was successfully created for PARTS.DB',
             mtConfirmation,
             [mbOK],0);

  TableVendors.AddIndex('byVendorName',
                      'VendorName',
                      [ixCaseInsensitive]);
  MessageDlg('The byVendorName index was successfully created for VENDORS.DB',
             mtConfirmation,
             [mbOK],0);
  TableVendors.Active := True;
  TableParts.Active := True;
end;

procedure TFormMain.Button2Click(Sender: TObject);
begin
  TableVendors.Active := False;
  TableParts.Active := False;

  TableParts.DeleteIndex('byVendorDescription');
  MessageDlg('The byVendorDescription index was successfully deleted from PARTS.DB',
             mtConfirmation,
             [mbOK],0);

  TableParts.DeleteIndex('byVendorNoPartNo');
  MessageDlg('The byVendorNoPartNo index was successfully deleted from PARTS.DB',
             mtConfirmation,
             [mbOK],0);
  TableVendors.DeleteIndex('byVendorName');
  MessageDlg('The byVendorName index was successfully deleted from VENDORS.DB',
             mtConfirmation,
             [mbOK],0);
end;

end.
