unit Unit8;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FR_DSet, FR_Class;

type
  TForm8 = class(TForm)
    frReport1: TfrReport;
    MasterDataset: TfrUserDataset;
    Detail1Dataset: TfrUserDataset;
    Detail2Dataset: TfrUserDataset;
    frReport2: TfrReport;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

{$R *.DFM}

end.
