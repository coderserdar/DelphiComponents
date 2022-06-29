unit Demo6Frm;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RVScroll, RichView, ExtCtrls,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  RVStyle;

type
  TfrmDemo6 = class(TForm)
    Close: TButton;
    rvs: TRVStyle;
    tmr: TTimer;
    rv: TRichView;
    il: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure tmrTimer(Sender: TObject);
  private
    { Private declarations }
    Red: Byte;
    DRed: Integer;
    ImageIndex: Integer;    
  public
    { Public declarations }
  end;

var
  frmDemo6: TfrmDemo6;

implementation

{$R *.DFM}

procedure TfrmDemo6.FormCreate(Sender: TObject);
const crlf:String = chr(13)+chr(10);
begin
  ImageIndex := 0;
  Red  := 150;
  DRed := 5;
  rvs.TextStyles[rvsHeading].Color := RGB(Red,0,0);
  // How many items the following statements adds? (adding string with 10 crlfs?)
  // Answer: 11! (since RichView 1.1) because it now considered like
  // ''+crlf+''+crlf+''+crlf+''+crlf+''+crlf+''+crlf+''+crlf+''+crlf+''+crlf+''+crlf+''
  rv.AddTextNL(
        crlf+crlf+crlf+crlf+crlf+crlf+crlf+crlf+crlf+crlf, 0,0,0);
  // And since v1.1 you can use TopMargin and BottomMargin properties instead of
  // adding empty lines!

  rv.AddBulletEx('',0,il,1);
  rv.Add('Credits Demo', 1);
  rv.AddTextNL(
        'Roberto Nelson'+crlf+
        'Bruce Young'+crlf+
        'Kim Lambert'+crlf+
        'Leslie Johnson'+crlf+
        'Phil Forest'+crlf+
        'K.J. Weston'+crlf+
        'Lee Terry'+crlf+
        'Stewart Hall'+crlf+
        'Katherine Young'+crlf+
        'Chris Papadopulos'+crlf+
        'Pete Fisher'+crlf+
        'Ann Bennet'+crlf+
        'Roger De Sousa'+crlf+
        'Janet Boldwin'+crlf+
        'Roger Reeves'+crlf+
        'Willie Stansbury'+crlf+
        'Leslie Phong'+crlf+
        'Ashok Ramanathan',0,0,0);
  rv.AddNL('and other people from Employee.db',2,0);
  rv.AddTextNL(crlf+crlf+crlf+crlf+crlf+crlf+
          crlf+crlf+crlf+crlf+crlf,0,0,0);
  rv.VSmallStep := 1;
  rv.Format;
end;

procedure TfrmDemo6.tmrTimer(Sender: TObject);
begin
   if rv.VScrollPos<>rv.VScrollMax then
     rv.VScrollPos := rv.VScrollPos+1
   else
     rv.VScrollPos := 0;
   inc(Red, DRed);
   rvs.TextStyles[rvsHeading].Color := RGB(Red,0,0);
   if (Red=255) or (Red=100) then DRed := -DRed;
   inc(ImageIndex);
   if ImageIndex=il.Count then
     ImageIndex := 0;
   rv.SetBulletInfo(11,'',ImageIndex,nil,0);
end;

end.
