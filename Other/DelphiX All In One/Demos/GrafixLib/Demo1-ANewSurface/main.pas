{ GrafixDX Demo1 - Surfaces (and also how to load a jpeg).  By Entity }
unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DXClass, DXDraws, Directx, DXSounds, GrafixDX, DIB;

type
  TForm1 = class(TDXForm)
    DXDraw1: TDXDraw;
    DXTimer1: TDXTimer;
    DXImageList1: TDXImageList;
    procedure DXDraw1Initialize(Sender: TObject);
    procedure DXDraw1Finalize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure DXTimer1Timer(Sender: TObject; LagCount: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    HardwareSwitch: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  MySurface: TGrafixSurface;

implementation

{$R *.DFM}

procedure TForm1.DXDraw1Initialize(Sender: TObject);
begin
  // Create a GrafixDX surface
  MySurface := TGrafixSurface.Create(DXDraw1.Surface.DDraw);
  // Initialize the surface
  MySurface.Init(DXDraw1, DXImageList1, DXDraw1.Width, DXDraw1.Height, 0);
  MySurface.Surface := MySurface;

  // Uncomment the code below to load a jpeg to the surface.. EASY EH?? ;o]
  // Load a jpeg.. if true then the surface will be set to the Width/Height of jpg
  // If false the jpeg is stretch to size of the surface
  // MySurface.LoadFromJpeg(<FILENAME.JPG>, false);

  DXDraw1.Cursor := crNone;

  DXTimer1.Enabled := True;
end;

procedure TForm1.DXDraw1Finalize(Sender: TObject);
begin
  DXTimer1.Enabled := False;

  DXDraw1.Cursor := crDefault;

  MySurface.Free;
end;

procedure TForm1.DXTimer1Timer(Sender: TObject; LagCount: Integer);
begin
  // Vital for entering FullScreen mode at runtime
  if not DXDraw1.CanDraw then Exit;

  DXDraw1.BeginScene;
  try
    {clear surface with predefined windows color}
    DXDraw1.Surface.Fill(DXDraw1.Surface.ColorMatch(clBlack));

    //----------------------------------------------------------------------------
    {All drawing here like}

    // Draw the surface to the backbuffer of DXDraw before flipping
    MySurface.DrawToDXDraw(0, 0, False);

    //----------------------------------------------------------------------------
  finally
    DXDraw1.EndScene;
  end;

  { Draw FrameRate }
  with DXDraw1.Surface.Canvas do
  try
    Brush.Style := bsClear;
    Font.Color := clWhite;
    Font.Size := 10;
    Textout(3, 3, 'FPS: ' + IntToStr(DXTimer1.FrameRate));
    if doHardware in DXDraw1.NowOptions then begin
      Textout(3, 14, 'Device: Hardware');
      Textout(3, DXDraw1.Height - 16 - 14, 'Change device mode to software press SPACE.')
    end
    else begin
      Textout(3, 14, 'Device: Software');
      Textout(3, DXDraw1.Height - 16 - 14, 'Change device mode to hardware press SPACE.')
    end;
    if doFullScreen in DXDraw1.NowOptions then
      Textout(3, DXDraw1.Height - 16, 'For windowed mode press ALT+Enter.')
    else
      Textout(3, DXDraw1.Height - 16, 'For fullscreen press ALT+Enter.');
  finally
    Release; {  Indispensability  }
  end;

  DXDraw1.Flip;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then Application.Terminate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {be sure color table}
  DXImageList1.Items.MakeColorTable;
  DXDraw1.ColorTable := DXImageList1.Items.ColorTable;
  DXDraw1.DefColorTable := DXImageList1.Items.ColorTable;
  DXDraw1.UpdatePalette;
  {switch for easy change mode}
  HardwareSwitch := False; {initialize, start in software mode}
  {when changed can be call FormKeyDown with 'space' parameter}
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  { Application end }
  if Key = VK_ESCAPE {ESC} then
    Close;
  { Switch hardware-software mode }
  if Key = VK_SPACE {mezernik} then begin
    DXDraw1.Finalize;
    HardwareSwitch := not HardwareSwitch;
    if HardwareSwitch then
    {hardware}
    begin
     {$IFDEF D3D_deprecated}
      if not (doDirectX7Mode in DXDraw1.Options) then
        DXDraw1.Options := DXDraw1.Options + [doDirectX7Mode];
      if not (do3D in DXDraw1.Options) then
        DXDraw1.Options := DXDraw1.Options + [do3D];
     {$ENDIF}
      if not (doHardware in DXDraw1.Options) then
        DXDraw1.Options := DXDraw1.Options + [doHardware];
      if doSystemMemory in DXDraw1.Options then
        DXDraw1.Options := DXDraw1.Options - [doSystemMemory];
    end
    else
    {software}
    begin
     {$IFDEF D3D_deprecated}
      if doDirectX7Mode in DXDraw1.Options then
        DXDraw1.Options := DXDraw1.Options - [doDirectX7Mode];
      if do3D in DXDraw1.Options then
        DXDraw1.Options := DXDraw1.Options - [do3D];
     {$ENDIF}
      if doHardware in DXDraw1.Options then
        DXDraw1.Options := DXDraw1.Options - [doHardware];
      if not (doSystemMemory in DXDraw1.Options) then
        DXDraw1.Options := DXDraw1.Options + [doSystemMemory];
    end;

    DXDraw1.Initialize;

    Exit;
  end;

  {  Screen mode change  }
  if (ssAlt in Shift) and (Key = VK_RETURN) {ALT+ENTER} then
  begin
    DXDraw1.Finalize;

    if doFullScreen in DXDraw1.Options then
    begin
      RestoreWindow;

      DXDraw1.Cursor := crDefault;
      BorderStyle := bsSingle;
      DXDraw1.Options := DXDraw1.Options - [doFullScreen];
      DXDraw1.Options := DXDraw1.Options + [doFlip];
    end else
    begin
      StoreWindow;

      DXDraw1.Cursor := crNone;
      BorderStyle := bsNone;
      DXDraw1.Options := DXDraw1.Options + [doFullScreen];
      DXDraw1.Options := DXDraw1.Options - [doFlip];
    end;

    DXDraw1.Initialize;
  end;
end;

end.

