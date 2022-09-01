unit fltbox_demo_join_datastruct;

interface
{$I psc_defines.inc}

uses
  {$IFDEF D6}
  Variants, 
  {$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  TDataStructureForm = class(TForm)
    Image1: TImage;
    BitBtn1: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataStructureForm: TDataStructureForm;

implementation

{$R *.dfm}

end.
