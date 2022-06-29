unit EanAbout;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, EanKod, ExtCtrls, EanSpecs;

type
  TPSoftAbout = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    Label3: TLabel;
    Ean1: TEan;
    Timer1: TTimer;
    Label4: TLabel;
    Ean2: TEan;
    BCName: TLabel;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure About;


var
  PSoftAbout: TPSoftAbout;

implementation

{$R *.DFM}

procedure TPSoftAbout.Timer1Timer(Sender: TObject);
var i:Integer;
begin
     i:=Integer(Ean1.TypBarCode);
     Inc(i);   if i>14 then i:=0;
     Ean1.TypBarCode := TTypBarCode(i);
     BCName.Caption  := Ean1.GetBarcodeInfo.Name;
     Ean2.Angle := Ean2.Angle + 90;
end;


procedure About;
begin
     PSoftAbout := TPsoftAbout.Create(Application);
     try
            PSoftAbout.ShowModal;
     finally
            PSoftAbout.Free;
     end;
end;
end.
