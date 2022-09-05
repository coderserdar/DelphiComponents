{*******************************************************}
{                                                       }
{       DIB effects demonstrate application             }
{                                                       }
{       Copyright (C) 2008 Jaro.Benes                   }
{       All Rights Reserved                             }
{                                                       }
{*******************************************************}

unit TestDIBEffects1;
{
Application for demonstrate all added DIB-effect in unit DIB

Written in Delphi 2007

RxLibrary is used...

}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DIB, Mask, RxToolEdit, ExtDlgs;

type
  TForm1 = class(TForm)
    FlowPanel1: TFlowPanel;
    btnSpray: TButton;
    btnEmboss: TButton;
    btnMonoNoise: TButton;
    btnGradientNoise: TButton;
    btnTwist: TButton;
    btnFishEye: TButton;
    btnSmoothRotWr: TButton;
    btnLightness: TButton;
    btnSaturation: TButton;
    btnContrast: TButton;
    btnAddRGB: TButton;
    btnFilter: TButton;
    btnSharpen: TButton;
    btnRotate: TButton;
    btnSplitBlur: TButton;
    btnGaussianBlur: TButton;
    btnDoInvert: TButton;
    btnDoAddColorNoise: TButton;
    btnDoAddMonoNoise: TButton;
    btnDoAntiAlias: TButton;
    btnDoContrast: TButton;
    btnDoFishEye: TButton;
    btnDoGrayScale: TButton;
    btnDoLightness: TButton;
    btnDoDarkness: TButton;
    btnDoSaturation: TButton;
    btnDoSplitBlur: TButton;
    btnDoGaussianBlur: TButton;
    btnDoMosaic: TButton;
    btnDoTwist: TButton;
    btnDoSplitlight: TButton;
    btnDoTile: TButton;
    btnDoSpotLight: TButton;
    btnDoTrace: TButton;
    btnDoEmboss: TButton;
    btnDoSolorize: TButton;
    btnDoPosterize: TButton;
    btnDoColorize: TButton;
    btnDoBrightness: TButton;
    btnDoResample: TButton;
    btnDoSmoothRotate: TButton;
    btnNova: TButton;
    btnMandel: TButton;
    btnOldPhotos: TButton;
    btnBlendPixel: TButton;
    btnLinePolar: TButton;
    btnDarker: TButton;
    btnLighter: TButton;
    DXDIB1: TDXDIB;
    Panel1: TPanel;
    Image2: TImage;
    FilenameEdit1: TFilenameEdit;
    StaticText2: TStaticText;
    StaticText1: TStaticText;
    Label1: TLabel;
    Image1: TImage;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    btnEncrypt: TButton;
    btnDecrypt: TButton;
    btnDoZoom: TButton;
    btnInk: TButton;
    btnDoRotate: TButton;
    btnDistort: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
    procedure FilenameEdit1AfterDialog(Sender: TObject; var Name: string;
      var Action: Boolean);
    procedure Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
    DIB, QD: TDIB;
    mX, mY: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnAutoClick(Sender: TObject);
  procedure DrawAs(DIB: TDIB; Sleeping: Word = 1);
  begin
    Image2.Picture.Assign(DIB);
    Application.ProcessMessages;
    if Sleeping <> 0 then Sleep(Sleeping);
  end;
var
  D, D1, D2: TDIB;
  I, tX, tY: Integer;
  a, b, c: Integer;
  xc, yc: Integer;
  InitSpray: Boolean;
  AllBlack: Boolean;
  zr: Double;
begin
  Screen.Cursor := crHourGlass;
  FlowPanel1.Enabled := False;
  Image2.OnMouseMove := nil;
  try
    DIB.Assign(DXDIB1.DIB);
    Image1.Picture.Assign(DXDIB1.DIB);
    Application.ProcessMessages;
    //DIB.CreateDIBFromBitmap(Image1.Picture.Bitmap);
    {some added function are specific participation on own object}
    {all undocumented still, demo only}
    case (Sender as TButton).Tag of
      1:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.Spray(I);
              DrawAs(D,3);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.Spray(I);
              DrawAs(D,3);
            end;
          finally
            D.Free;
          end;
        end;
      2: DIB.Emboss;
      3:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 256 do
            begin
              D.Assign(DIB);
              D.AddMonoNoise(I);
              DrawAs(D);
            end;
            for I := 256 downto 1 do
            begin
              D.Assign(DIB);
              D.AddMonoNoise(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      4:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 256 do
            begin
              D.Assign(DIB);
              D.AddGradiantNoise(I);
              DrawAs(D);
            end;
            for I := 256 downto 1 do
            begin
              D.Assign(DIB);
              D.AddGradiantNoise(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      5:
        begin
          D := TDIB.Create;
          D1 := TDIB.Create;
          try
            for I := 0 to 255 do
            begin
              D1.Assign(DIB);
              D.Assign(D1);
              D.Twist(D1, I); //please see how parameters are returned
              DrawAs(D1);
            end;
            for I := 255 downto 0 do
            begin
              D1.Assign(DIB);
              D.Assign(D1);
              D.Twist(D1, I); //please see how parameters are returned
              DrawAs(D1);
            end;
          finally
            D.Free;
          end;
        end;
      6:
        begin
          D := TDIB.Create;
          try
            D.Assign(DIB);
            D.FishEye(DIB);
          finally
            D.Free;
          end;
        end;
      7:
        begin
          D := TDIB.Create;
          try
            {classic effect}
            D.Assign(DIB);
            for I := 0 to 359 do
            begin
              if D.SmoothRotateWrap(DIB, DIB.Width div 2, DIB.Height div 2, I) then
                DrawAs(DIB, 0);
            end;
            {enhanced effect}
            D.Assign(DIB);
            for I := 0 to 359 do
            begin
              if D.SmoothRotateWrap(D, D.Width div 2, D.Height div 2, I) then
                DrawAs(D, 0);
            end;
          finally
            D.Free;
          end;
        end;
      8:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.Lightness(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.Lightness(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      9:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.Saturation(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.Saturation(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      10:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.Contrast(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.Contrast(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      11: DIB.AddRGB(30, 30, 15);
      12:
        begin
          D := TDIB.Create;
          try
            D.Assign(DIB);
            D.Filter(D, EdgeFilter);
            DrawAs(D,100);
            D.Assign(DIB);
            D.Filter(D, StrongOutlineFilter);
            DrawAs(D,100);
            D.Assign(DIB);
            D.Filter(D, Enhance3DFilter);
            DrawAs(D,100);
            D.Assign(DIB);
            D.Filter(D, LinearFilter);
            DrawAs(D,100);
            D.Assign(DIB);
            D.Filter(D, GranularFilter);
            DrawAs(D,100);
            D.Assign(DIB);
            D.Filter(D, SharpFilter);
            DrawAs(D,100);
          finally
            D.Free;
          end;
        end;
      13:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.Sharpen(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.Sharpen(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      14:
        begin
          D := TDIB.Create;
          try
            for I := 0 to 360 do
            begin
              DIB.Rotate(D, DIB.Width div 2, DIB.Height div 2, I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      15:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.SplitBlur(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.SplitBlur(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      16:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 20 do
            begin
              D.Assign(DIB);
              D.GaussianBlur(D, I);
              DrawAs(D);
            end;
            for I := 20 downto 1 do
            begin
              D.Assign(DIB);
              D.GaussianBlur(D, I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      //another added functions
      17: DIB.DoInvert;
      18:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 256 do
            begin
              D.Assign(DIB);
              D.DoAddColorNoise(I);
              DrawAs(D);
            end;
            for I := 256 downto 1 do
            begin
              D.Assign(DIB);
              D.DoAddColorNoise(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      19:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 256 do
            begin
              D.Assign(DIB);
              D.DoAddMonoNoise(I);
              DrawAs(D);
            end;
            for I := 256 downto 1 do
            begin
              D.Assign(DIB);
              D.DoAddMonoNoise(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      20: DIB.DoAntiAlias;
      21:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoContrast(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoContrast(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      22:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoFishEye(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoFishEye(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      23: DIB.DoGrayScale;
      24:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoLightness(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoLightness(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      25:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoDarkness(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoDarkness(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      26:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoSaturation(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoSaturation(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      27:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoSplitBlur(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoSplitBlur(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      28: DIB.DoGaussianBlur(2);
      29:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoMosaic(I);
              DrawAs(D,20);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoMosaic(I);
              DrawAs(D,20);
            end;
          finally
            D.Free;
          end;
        end;
      30:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoTwist(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoTwist(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      31:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 8 do
            begin
              D.Assign(DIB);
              D.DoSplitlight(I);
              DrawAs(D);
            end;
            for I := 8 downto 1 do
            begin
              D.Assign(DIB);
              D.DoSplitlight(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      //Simple tilling image
      32:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoTile(I);
              DrawAs(D, 20);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoTile(I);
              DrawAs(D, 20);
            end;
          finally
            D.Free;
          end;
        end;
      //Effect shows spot light on specified place:
      33:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoSpotLight(200, Bounds(15+I, 15+I, DIB.Width div 3, DIB.Height div 3));
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoSpotLight(200, Bounds(15+I, 15+I, DIB.Width div 3, DIB.Height div 3));
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      34:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoTrace(I);
              DrawAs(D);
            end;
//            for I := 64 downto 1 do begin
//              D.Assign(DIB);
//              D.DoTrace(I);
//              DrawAs(D);
//            end;
          finally
            D.Free;
          end;
        end;
      35: DIB.DoEmboss;
      36:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoSolorize(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoSolorize(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      37:
        begin
          DIB.DoPosterize(4);
//          D := TDIB.Create;
//          try
//            for I := 1 to 4 do begin
//              D.Assign(DIB);
//              D.DoPosterize(I);
//              DrawAs(D);
//            end;
//            for I := 4 downto 1 do begin
//              D.Assign(DIB);
//              D.DoPosterize(I);
//              DrawAs(D);
//            end;
//          finally
//            D.Free;
//          end;
        end;
      38: DIB.DoColorize(clYellow, clBlue);
      39:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 64 do
            begin
              D.Assign(DIB);
              D.DoBrightness(I);
              DrawAs(D);
            end;
            for I := 64 downto 1 do
            begin
              D.Assign(DIB);
              D.DoBrightness(I);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      //Effect for resizing image layer with using sophisticated resample filters, very useable:
      40: 
        begin
          D := TDIB.Create;
          try
            for I := 256 downto 33 do
            begin
              D.Assign(DIB);
              D.DoResample(I, I, ftrLanczos3);
              DrawAs(D);
            end;
            for I := 33 to 256 do
            begin
              D.Assign(DIB);
              D.DoResample(I, I, ftrLanczos3);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      //Another version of smooth rotate, very useable:
      41: begin
          D := TDIB.Create;
          try
            D.Assign(DIB);
            D.Fill(clBlack);
            D.DoSmoothRotate(DIB, DIB.Width div 2, DIB.Height div 2, 15);
            DIB.Assign(D);
          finally
            D.Free;
          end;
        end;
      42:
        begin
          D := TDIB.Create;
          try
            D.Assign(DIB);
            D.DoNovaEffect(16, 48, 255, DIB.Width div 3, DIB.Height div 3, 18, 1, 1, 2782, 2585, nil);
            DIB.Assign(D);
            QD.Assign(DXDIB1.DIB);
            Image2.OnMouseMove := Image2MouseMove;
          finally
            D.Free;
          end;
        end;
      43:
        begin
          D := TDIB.Create;
          try
            D.Assign(DIB); D.Fill(clBlack);
            D.DrawMandelbrot(1, -2, 1.5, -1.5);
            DIB.Assign(D);
          finally
            D.Free;
          end;
        end;
      44:
        begin
          D := TDIB.Create;
          try
            for I := 1 to 20 do
            begin
              D.Assign(DIB);
              D.SephiaEffect(I);
              DrawAs(D,20);
            end;
            for I := 20 downto 1 do
            begin
              D.Assign(DIB);
              D.SephiaEffect(I);
              DrawAs(D,20);
            end;
          finally
            D.Free;
          end;
        end;
      45:
        begin
          D1 := TDIB.Create;
          try
            D1.Assign(DIB);
            D1.Darker(33);
            for I := 0 to 9999 do
            begin
              tX := Random(256); tY := Random(256);
              D1.BlendPixel(tX + 1, tY + 1, DIB.Canvas.Pixels[tX, tY], 128);
              DrawAs(D1);
            end;
          finally
            D1.Free;
          end;
        end;
      46: DIB.LinePolar(1, 1, 33, 185, clRed);
      47: DIB.Darker(50);
      48: DIB.Lighter(50);
      49: DIB.EncryptDecrypt(788558);
      50:
      begin
        DIB.EncryptDecrypt(788558);
        Image1.Picture.Bitmap := DIB.CreateBitmapFromDIB;
        DIB.EncryptDecrypt(788558);
      end;
      51:
        begin
          D1 := TDIB.Create;
          D2 := TDIB.Create;
          try
            D1.Assign(DIB);
            D1.SetSize(DIB.Width-1, DIB.Height-1, 8);
            D1.Canvas.Draw(0, 0, DIB);
            D2.Assign(D1);
            zr := 2;
            for c := 0 to 255 do
            begin
              D2.DoZoom(D1, zr);
              zr := zr - 0.05;
              DrawAs(D1);
            end;
            DIB.Assign(D1);
          finally
            D1.Free;
            D2.Free;
          end;
        end;
      52:
        begin
          D := TDIB.Create;
          try
            D.SetSize(DIB.Width, DIB.Height, 8);
            D.Canvas.Draw(0, 0, DIB);

            InitSpray := True;
            repeat
              { effect }
              AllBlack := DIB.Ink(D, InitSpray, 500);
              { initialized }
              InitSpray := False;
              DrawAs(D);
            until AllBlack;
            DIB.Assign(D);
          finally
            D.Free;
          end;
        end;
      53:
        begin
          D := TDIB.Create;
          try
            D.Assign(DIB);
            xc := DIB.Width div 2;
            yc := DIB.Height div 2;
            for a := 0 to 64 do
            begin
              DIB.DoRotate(D, xc, yc, 8*a);
              DrawAs(D);
            end;
          finally
            D.Free;
          end;
        end;
      54:
        begin
          D := TDIB.Create;
          try
            D.Assign(DIB);
            xc := DIB.Width div 2;
            yc := DIB.Height div 2;
            for a := 0 to 16 do
            begin
              b := a * 8;
              DIB.Distort(D, dtSlow, xc, yc, b, 1);
              DrawAs(D);
            end;

            for a := 16 downto 0 do
            begin
              b := a * 8;
              DIB.Distort(D, dtSlow, xc, yc, b, 1);
              DrawAs(D);
            end;

            //

            for a := 0 to 63 do
            begin
              b := a * 8;
              DIB.Distort(D, dtFast, xc, yc, b, 0.75);
              DrawAs(D);
            end;

            for a := 64 downto 0 do
            begin
              b := a * 8;
              DIB.Distort(D, dtFast, xc, yc, b, 0.75);
              DrawAs(D);
            end;

          finally
            D.Free;
          end;
        end;
    end;
    Image2.Picture.Bitmap := DIB.CreateBitmapFromDIB;
    //for documentation
    //DIB.SaveToFile('c:\'+IntToStr((Sender as TButton).Tag)+'.dib');
  finally
    FlowPanel1.Enabled := True;
    Screen.Cursor := crDefault
  end;
end;

procedure TForm1.FilenameEdit1AfterDialog(Sender: TObject; var Name: string;
  var Action: Boolean);
var DIB: TDIB;
begin
  Image2.OnMouseMove := nil;
  if FileExists(Name) then
  begin
    Image1.AutoSize := False;
    Image1.Stretch := True;
    Image1.Width := 256;
    Image1.Height := 256;
    Image1.Picture.LoadFromFile(Name);
    Image2.Picture.Bitmap := nil;
    Image2.AutoSize := False;
    Image2.Stretch := True;
    DIB := TDIB.Create;
    try
      DIB.SetSize(Image1.Picture.Width, Image1.Picture.Height, 24);
      DIB.Canvas.Draw(0, 0, Image1.Picture.Graphic);
      DXDIB1.DIB.Assign(DIB);
    finally
      DIB.Free
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  DIB := TDIB.Create;
  QD := TDIB.Create;
  if FileExists(ExtractFilePath(Application.ExeName) + 'imagedx.dib') then
  begin
    DXDIB1.DIB.LoadFromFile(ExtractFilePath(Application.ExeName) + 'imagedx.dib');
    Image1.Picture.Assign(DXDIB1.DIB);
    Image2.Picture.Assign(DXDIB1.DIB);
  end
  else
  begin
    if OpenPictureDialog1.Execute then
    begin
      DXDIB1.DIB.LoadFromFile(OpenPictureDialog1.FileName);
      Image1.Picture.Assign(DXDIB1.DIB);
      Image2.Picture.Assign(DXDIB1.DIB);
    end
    else
    begin
      MessageDlg('Image have to selected!'#13#10'Application cannot to continue.', mtError, [mbOK], 0);
      Application.Terminate;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DIB.Free;
  QD.Free;
end;

procedure TForm1.Image2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mX := X;
  mY := Y;
  QD.DoNovaEffect(16, 48, 255, mX, mY, 18, 1, 1, 2782, 2585, nil);
  Image2.Picture.Assign(QD);
  //Image2.Picture.Bitmap := QD.CreateBitmapFromDIB;
  QD.Assign(DXDIB1.DIB);
  Application.ProcessMessages;
end;

end.