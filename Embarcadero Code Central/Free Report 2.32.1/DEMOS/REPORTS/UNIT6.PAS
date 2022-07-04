unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_DSet, FR_DBSet, DB, DBTables, FR_Class;

type
  TForm6 = class(TForm)
    frReport1: TfrReport;
    Bio: TTable;
    BioSpeciesNo: TFloatField;
    BioCategory: TStringField;
    BioCommon_Name: TStringField;
    BioSpeciesName: TStringField;
    BioLengthcm: TFloatField;
    BioLength_In: TFloatField;
    BioNotes: TMemoField;
    BioGraphic: TGraphicField;
    BioDataSource: TDataSource;
    BioDataSet: TFRDBDataSet;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.DFM}

end.
