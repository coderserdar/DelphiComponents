unit Prgrs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls;

type
  TFPrgrs = class(TForm)
    Panel1: TPanel;
    PB: TProgressBar;
    Accion: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FPrgrs: TFPrgrs;

implementation

{$R *.DFM}

end.
