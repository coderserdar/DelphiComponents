unit prgform;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TProgForm = class(TForm)
    ProgressBar1: TProgressBar;
    LFileName: TLabel;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ProgForm: TProgForm;

implementation

{$R *.DFM}

end.

