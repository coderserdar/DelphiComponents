unit TraceDemo1;
{Simple prototype demo project for using new features in DelphiX}
{Can be modify for your project}
{(c)2005 Jaro Benes}
interface

//How simple use traces

{$INCLUDE DelphiXcfg.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs, Math,
  DXDraws, DXClass, DIB, StdCtrls, RxCombos, Buttons, ExtCtrls; {DelphiX units...}

type
  TForm1 = class(TDXForm)
    DXDraw: TDXDraw;
    DXImageList: TDXImageList;
    DXTimer: TDXTimer;
    procedure DXDrawTraces2Move(Sender: TObject; LagCount: Integer;
      var MoveIt: Boolean);
    procedure DXDrawTraces0GetImage(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DXDrawMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DXDrawInitialize(Sender: TObject);
    procedure DXDrawFinalize(Sender: TObject);
    procedure DXTimerTimer(Sender: TObject; LagCount: Integer);
  private
    { Private declarations }
    HardwareSwitch: Boolean; {simple variables}
    FontChanged: Boolean;
    mX, mY: Integer;
    i, f, c, b: Single;
    smer: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DXDrawMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  {for mouse move as pointing - code here}
  mX := X;
  mY := Y;
end;

procedure TForm1.DXDrawInitialize(Sender: TObject);
begin
  Dxtimer.Enabled := true;
end;

procedure TForm1.DXDrawFinalize(Sender: TObject);
begin
  Dxtimer.Enabled := false;
end;

procedure TForm1.DXTimerTimer(Sender: TObject; LagCount: Integer);
//var
//  blended: Byte;
begin
  if not DXDraw.CanDraw then Exit;

  DXDraw.BeginScene;
  try
    {clear surface with predefined windows color}
    DXDraw.Surface.Fill(DXDraw.Surface.ColorMatch(clBlack));

    //----------------------------------------------------------------------------
    {All drawing here like}

    DXImageList.Items.Find('Image').DrawRotate(DXDraw.Surface, 300, 200, 256, 256, 0, 0.5, 0.5, i);

//    DXDraw_Bump(DXDraw, DXImageList.Items.Find('Img128x128'), nil, DXImageList.Items.Find('Shine'),
//      mX, mY, Bounds(100, 100, 128, 128), 0, 0, 0, [], rtBlend, i, Round(b));
    DXDraw.Render(LagCount);
    //DXImageList.Items[0].Draw(DXDraw.surface,0,0,0);
    //----------------------------------------------------------------------------
    i := i + LagCount / 1000 * 20; if i > 255 then i := i - 256;
    if smer then
      b := b + LagCount / 1000 * 33
    else
      b := b - LagCount / 1000 * 33;
    if (b > 255) or (b < 0) then smer := not smer;
    if b > 255 then b := 255;
    if b < 0 then b := 0;


    f := f - (LagCount / 1000) * 1000 / 6;
    if f < 0 then begin
      f := 1000 / 6;
      c := c + 1;
      //Inc(c);
      if c > 9 then c := 0;
      //if not (c in [0..9]) then c := 0;
    end;

  finally
    DXDraw.EndScene;
  end;

  { Draw FrameRate }
  with DXDraw.Surface.Canvas do
  try
    Brush.Style := bsClear;
    Font.Color := clWhite;
    Font.Size := 10;
    Textout(3, 3, 'FPS: ' + inttostr(DXTimer.FrameRate));
    Textout(3, 3 + 11 + 11, 'b = ' + inttostr(round(b)));
    Textout(3, 3 + 11 + 11 + 11, 'c = ' + inttostr(round(c)));
    Textout(3, 3 + 11 + 11 + 11 + 11, 'f = ' + inttostr(round(f)));
    if doHardware in DXDraw.NowOptions then begin
      Textout(3, 3 + 11, 'Device: Hardware');
      Textout(3, DXDraw.Height - 16 - 14, 'Change device mode to software press SPACE.')
    end
    else begin
      Textout(3, 3 + 11, 'Device: Software');
      Textout(3, DXDraw.Height - 16 - 14, 'Change device mode to hardware press SPACE.')
    end;
    if doFullScreen in DXDraw.NowOptions then
      TextOut(3, DXDraw.Height - 16, 'For windowed mode press ALT+Enter.')
    else
      TextOut(3, DXDraw.Height - 16, 'For fullscreen press ALT+Enter.');
  finally
    Release; {  Indispensability  }
  end;

  DXDraw.Flip;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Application end }
  if Key = VK_ESCAPE {ESC} then
    Close;
  { Switch hardware-software mode }
  if Key = VK_SPACE {mezernik} then begin
    DXDraw.Finalize;
    HardwareSwitch := not HardwareSwitch;
    if HardwareSwitch then
    {hardware}
    begin
      if not (doDirectX7Mode in DXDraw.Options) then
        DXDraw.Options := DXDraw.Options + [doDirectX7Mode];
      if not (doHardware in DXDraw.Options) then
        DXDraw.Options := DXDraw.Options + [doHardware];
      if not (do3D in DXDraw.Options) then
        DXDraw.Options := DXDraw.Options + [do3D];
      if doSystemMemory in DXDraw.Options then
        DXDraw.Options := DXDraw.Options - [doSystemMemory];
    end
    else
    {software}
    begin
      if do3D in DXDraw.Options then
        DXDraw.Options := DXDraw.Options - [do3D];
      if doHardware in DXDraw.Options then
        DXDraw.Options := DXDraw.Options - [doHardware];
      if not (doSystemMemory in DXDraw.Options) then
        DXDraw.Options := DXDraw.Options + [doSystemMemory];
      if doDirectX7Mode in DXDraw.Options then
        DXDraw.Options := DXDraw.Options - [doDirectX7Mode];
    end;

    DXDraw.Initialize;

    Exit;
  end;

  {  Screen mode change  }
  if (ssAlt in Shift) and (Key = VK_RETURN) {ALT+ENTER} then
  begin
    DXDraw.Finalize;

    if doFullScreen in DXDraw.Options then
    begin
      RestoreWindow;

      DXDraw.Cursor := crDefault;
      BorderStyle := bsSingle;
      DXDraw.Options := DXDraw.Options - [doFullScreen];
      DXDraw.Options := DXDraw.Options + [doFlip];
    end else
    begin
      StoreWindow;

      DXDraw.Cursor := crNone;
      BorderStyle := bsNone;
      DXDraw.Options := DXDraw.Options + [doFullScreen];
      DXDraw.Options := DXDraw.Options - [doFlip];
    end;

    DXDraw.Initialize;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {be sure color table}
  DXImageList.Items.MakeColorTable;
  DXDraw.ColorTable := DXImageList.Items.ColorTable;
  DXDraw.DefColorTable := DXImageList.Items.ColorTable;
  DXDraw.UpdatePalette;
  {switch for easy change mode}
  HardwareSwitch := False; {initialize, start in software mode}
  {when changed can be call FormKeyDown with 'space' parameter}
  f := 1000 div 6; b := 0; c := 0; smer := True;
  //how simple create clone
  DXDraw.Traces.Find('Test').Clone('Retest', 60, 180);
  //go away and back
  DXDraw.Traces.Find('Retest').Blit.Bustrofedon := True;
end;

procedure TForm1.DXDrawTraces0GetImage(Sender: TObject);
begin
  TBlit(Sender).Image := DXImageList.Items[0];
end;

procedure TForm1.DXDrawTraces2Move(Sender: TObject; LagCount: Integer;
  var MoveIt: Boolean);
begin
  with TBlit(Sender) do begin
    case Tag of
      0:
        if Alpha = $FF then begin
          Alpha := Alpha - 1;
          Tag := 1; //dolu
        end
        else
          Alpha := Alpha + 1;
      1:
        if Alpha = 0 then begin
          Alpha := Alpha + 1;
          Tag := 0; //nahoru
        end
        else
          Alpha := Alpha - 1;
    end;
  end;
end;

end.

