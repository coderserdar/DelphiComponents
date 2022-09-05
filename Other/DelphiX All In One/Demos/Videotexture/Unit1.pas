unit Unit1;
{Simple prototype demo project for using new features in DelphiX}
{Can be modify for your project}
{(c)2005-2008 Jaro Benes}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtDlgs,
  DXDraws, DXClass, DIB; {DelphiX units...}

type
  TForm1 = class(TDXForm)
    DXDraw: TDXDraw;
    DXImageList: TDXImageList;
    DXTimer: TDXTimer;
    DXDIB1: TDXDIB;
    procedure DXDrawUpdateTextures(const Sender: TD2DTextures;
      var Changed: Boolean);
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
  public
    { Public declarations }
    gRotationAngle: Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.DXDrawMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  {for mouse move as pointing - code here}
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
begin
  if not DXDraw.CanDraw then Exit;

  DXDraw.BeginScene;
  try
    {clear surface with predefined windows color}
    DXDraw.Surface.Fill(DXDraw.Surface.ColorMatch(clBlack));

    //----------------------------------------------------------------------------
    {All drawing here like}
    with DXImageList.Items[0] do
      DrawRotate(DXDraw.Surface, 150, 150, Width, Height, 0, 0.5, 0.5, gRotationAngle);
    gRotationAngle := gRotationAngle + 3 * (LagCount / 1000);
    //----------------------------------------------------------------------------
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
    if doHardware in DXDraw.NowOptions then begin
      Textout(3, 14, 'Device: Hardware');
      Textout(3, DXDraw.Height - 16 - 14, 'Change device mode to software press SPACE.')
    end
    else begin
      Textout(3, 14, 'Device: Software');
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
  //--update afrer flip--
  //-in software mode do nothing-
  DXDraw.UpdateTextures; //all update can be do there
  //---------------------
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
  gRotationAngle := 0;
end;

procedure TForm1.DXDrawUpdateTextures(const Sender: TD2DTextures;
  var Changed: Boolean);
var
  T: TDIB;
begin
  Changed := False;
  {can be updated many textures there when it is need}
  {search the layout of texture image as DIB}
  {texture has to name, it is indispensable for human identify}
  {sender is substitute for full texture object used internally}
  T := Sender.TexLayoutByName['Blue2'];
  {changes image layout}
  if Assigned(T) then begin
    {context instance T mustn't be lost, image mustn't be recreated}
    T.Fill(clBlue); //fill all image by color
    //T.DrawOn(SrcCanvas, T.ClientRect, T.Canvas, 0, 0); //other method
    T.DrawTo(DXDIB1.DIB,0,0,T.Width,T.Height,0,0);
    T.Canvas.Font.Color := clWhite;
    T.Canvas.Brush.Style := bsClear;
    T.Canvas.TextOut(5, 5, DateTimetoStr(Now)); //draw current date and time demonstrate videotexture
    {texture has to updated after changes, it is indispensable!!!}
    Sender.GetTextureByName('Blue2').Load;
    {and signal to subsystem - any change there}
    Changed := True;
  end;
end;

end.

