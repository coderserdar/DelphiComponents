unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, vgCtrls, ExtCtrls;

type
  TMainForm = class(TForm)
    lb: TvgLabel;
    vgLabel2: TvgLabel;
    vgLabel3: TvgLabel;
    vgLabel4: TvgLabel;
    vgLabel5: TvgLabel;
    vgLabel1: TvgLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

end.
