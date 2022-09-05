// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  25396: uFormImage2DBCtrlGrid.pas 
//
//   Rev 1.1    28-09-2005 17:08:54  mcm    Version: IMG 2.9

//
//   Rev 1.0    13-02-2005 19:48:38  mcm    Version: IMG 2.8

unit uFormImage2DBCtrlGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DBCtrls, DBCGrids, Db, DBTables, mcmImage, mcmImageDB;

type
  TForm1 = class(TForm)
    DBCtrlGrid1: TDBCtrlGrid;
    DataSource1: TDataSource;
    Table1: TTable;
    mcmImageDB1: TmcmImageDB;
    DBImage1: TDBImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormActivate(Sender: TObject);
begin
  mcmImageDB1.Image.SetStretchMode(COLORONCOLOR);
end;

end.
