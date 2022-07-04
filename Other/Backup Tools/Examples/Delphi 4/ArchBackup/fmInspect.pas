unit fmInspect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TInspect = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lUser: TLabel;
    lCompany: TLabel;
    lSerial: TLabel;
    lDate: TLabel;
    lName: TLabel;
    lVersion: TLabel;
    BitBtn1: TBitBtn;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Inspect: TInspect;

implementation

{$R *.DFM}

end.
