unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, API_grbutton, API_skin, API_base;

type
  TForm1 = class(TForm)
    API_skin1: TAPI_skin;
    API_grbutton1: TAPI_grbutton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure API_grbutton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  if not api_skin.InitScreen then
  begin
    messagedlg('Failed to get screen handle.', mterror, [mbok], 0);
    application.terminate;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  api_skin.FreeScreen;
end;

procedure TForm1.API_grbutton1Click(Sender: TObject);
const
  TESTTEXT = 'This is just test!';
var
  w,h: integer;
  rect: trect;
  ftempback: tbitmap;
  foldback: tbitmap;
  i,c: integer;
begin
  if ScreenCanvas<>nil then
  begin
    // set some parameters for the
    // whole screen canvas
    screencanvas.Font.Color:= clred;
    screencanvas.font.Size:= 24;
    screencanvas.Brush.Style:= bsclear;

    // calculate area to draw something on
    w:= screencanvas.TextWidth(TESTTEXT);
    h:= screencanvas.TextHeight(TESTTEXT);
    rect.Left:= random(screencanvas.ClipRect.Right-w);
    rect.Top:= random(screencanvas.ClipRect.bottom-h);
    rect.Right:= rect.Left + w;
    rect.Bottom:= rect.top + h;

    // because we don't need to save
    // energy here -- we'll take the
    // whole screen capture behind the
    // text that we'll draw..
    foldback:= tbitmap.create;
    try
      screencapture(foldback, rect);

      // next we draw the box for the text
      // we are going to show with some
      // effects exported in the api_skin..
      ftempback:= tbitmap.create;
      try

        ftempback.canvas.font.Assign(screencanvas.font);
        ftempback.canvas.Brush.style:=bsclear;

        c:= $ffffff;
        for i:=255 downto 2 do
        begin
          c:= i or (i shl 8) or (i shl 16);
          ftempback.Assign(foldback);
//          api_skin1.ColorScale(ftempback,c);
          ftempback.Canvas.textout(0,0,TESTTEXT);
          screencanvas.Draw(rect.Left,rect.Top,ftempback);
          sleep(5);
        end;

        // wait a sec
        sleep(1000);

        // we'll wait some seconds
        for i:=2 to 255 do
        begin
          c:= i or (i shl 8) or (i shl 16);
          ftempback.Assign(foldback);
//          api_skin1.ColorScale(ftempback,c);
          ftempback.canvas.TextOut(0,0,TESTTEXT);
          screencanvas.Draw(rect.Left,rect.Top,ftempback);
          sleep(5);
        end;

      finally
        ftempback.Free;
      end;

      // now return background of above
      // area used..
      screencanvas.Draw(rect.left,rect.Top,foldback);

    finally
      foldback.free;
    end;
  end;
end;

end.
