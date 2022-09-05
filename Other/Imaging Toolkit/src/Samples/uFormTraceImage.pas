// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  27709: uFormTraceImage.pas 
//
//    Rev 1.4    2014-04-06 13:07:38  mcm
//
//    Rev 1.3    25-10-2009 17:53:42  mcm    Version: IMG 3.3
//
//    Rev 1.2    05-11-2006 19:00:24  mcm    Version: IMG 3.1
// Minor changes, added comments.
//
//    Rev 1.1    05-06-2006 22:44:26  mcm    Version: IMG 3.0
//
//    Rev 1.0    21-05-2006 12:36:34  mcm
// Initial revision
unit uFormTraceImage;

interface

{$IFDEF VER100} {$DEFINE DCB3} {$ENDIF} { DELPHI 3  = VER100 }
{$IFDEF VER110} {$DEFINE DCB3} {$ENDIF} { BUILDER 3 = VER110 }

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  mcmImage, Menus, mcmFileDialogues, mcmTrace, mcmGraphics, mcmShape, ComCtrls,
  StdCtrls, umcmIntE, ToolWin, ExtCtrls, Grids;

const
  WM_LATEPAINT = WM_USER + 1;

type
  TForm1 = class(TForm)
    OpenDialog    : TmcmOpenDialog;
    SaveDialog    : TmcmSaveDialog;

    MainMenu1     : TMainMenu;
    FileMenu      : TMenuItem;
    OpenItem      : TMenuItem;
    SaveItem      : TMenuItem;
    N1            : TMenuItem;
    ExitItem      : TMenuItem;
    EditMenu      : TMenuItem;
    CopyItem      : TMenuItem;
    PasteItem     : TMenuItem;

    StatusBar     : TStatusBar;
    ClearItem     : TMenuItem;
    Panel1        : TPanel;
    lMethod       : TLabel;
    cbMethod      : TComboBox;
    lTolerance    : TLabel;
    isTolerance   : TmcmIntSpin;
    cbTraceAll    : TCheckBox;
    lMinLength    : TLabel;
    isMinLength   : TmcmIntSpin;
    sgData        : TStringGrid;
    cbTraceOnEdge : TCheckBox;
    cbClosedOnly  : TCheckBox;
    PageControl1: TPageControl;
    tsInput: TTabSheet;
    tsThreshold: TTabSheet;
    mcmImageCtrlThreshold: TmcmImageCtrl;
    Splitter1: TSplitter;
    tsOutline: TTabSheet;
    mcmImageCtrlOutline: TmcmImageCtrl;
    ScrollBox1: TScrollBox;
    mcmImageCtrl: TmcmImageCtrl;
    Label1: TLabel;
    rsScale: TmcmRealSpin;

    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure mcmImageCtrlMouseUp(Sender : TObject; Button : TMouseButton;
                                  Shift : TShiftState; X, Y : Integer);
    procedure OpenItemClick(Sender : TObject);
    procedure SaveItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);
    procedure EditMenuClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
    procedure ClearItemClick(Sender : TObject);
    procedure mcmImageCtrlMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure Panel1Resize(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure sgDataSelectCell(Sender : TObject; Col, Row : Integer; var CanSelect : Boolean);
    procedure cbTraceAllClick(Sender: TObject);
    procedure mcmImageCtrlChange(Sender: TObject);
    procedure rsScaleChange(Sender: TObject);

  private
    { Private declarations }
    mcmImage         : TmcmImage;
    mcmTrace         : TmcmTrace;
    mcmTraceObject   : TmcmPolygon; // Current selection
    OldTraceObject   : TmcmPolygon; // Previous selection.

    // Row index - Selects which Shape parameter to display.
    FShapeIndex      : integer;

    // Timing variables.
    {$IFDEF DCB3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    FPerformanceFreq : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    FPerformanceFreq : int64;
    {$ENDIF}
  public
    { Public declarations }
    procedure InitTrace;
    procedure SetMouseEvent;
    procedure ShowData;
    procedure ShowTime;
    procedure ShowShape(Shape : TmcmPolygon);
    procedure ResetShowShape(Shape : TmcmPolygon);
  end;

var Form1 : TForm1;

implementation

uses ClipBrd, mcmImageFile, mcmImageColor, mcmImageMorph, mcmImageTypeDef;

{$R *.DFM}

procedure TForm1.FormCreate(Sender : TObject);
begin
  //----------------------------------------------------------------------------
  // Specify which formats to support!
  ImageFileManager.UnregisterFileFormat(TmcmICONImage);

  ImageFileManager.YCbCrMode := JYCC_AUTO;
  OpenDialog.FilterIndex := 0;

  //----------------------------------------------------------------------------
  // Assign the global ImageFileManager to both the mcmOpenDialog and
  // mcmSaveDialog.
  // This enables both dialogues to automatically obtain the supported and
  // enabled file formats & compressions.
  OpenDialog.ImageFileManager := ImageFileManager;
  SaveDialog.ImageFileManager := ImageFileManager;
  OpenDialog.InitialDir := 'E:\IMAGES';
  //----------------------------------------------------------------------------
  // Set-up time measurement of processes.
  QueryPerformanceFrequency(FPerformanceFreq);
  QueryPerformanceCounter(FStartTime);
  QueryPerformanceCounter(FEndTime);

  cbMethod.ItemIndex := 0;

  mcmTrace := Nil;
  mcmImage := Nil;

  PageControl1.ActivePage := tsInput;
end; // TForm1.FormCreate.


procedure TForm1.FormActivate(Sender: TObject);
begin
  if (mcmTrace = Nil)
  then mcmTrace := TmcmTrace.Create(tsInput);//Self);
end;

procedure TForm1.FormDestroy(Sender : TObject);
begin
  mcmTrace.Free;
end; // TForm1.FormDestroy.


procedure TForm1.OpenItemClick(Sender : TObject);
begin
  if (OpenDialog.Execute)
  then begin
       mcmImageCtrl.Image.FileOpen(OpenDialog.FileName);
       mcmImageCtrl.Image.XResolution := 1;
       mcmImageCtrl.Image.YResolution := 1;
  end;
  InitTrace;
end; // TForm1.OpenItemClick.


procedure TForm1.SaveItemClick(Sender : TObject);
begin
  SaveDialog.Image       := mcmImageCtrl.Image;
  SaveDialog.FileName    := mcmImageCtrl.Image.ImageInfo.FileName;
  SaveDialog.Compression := mcmImageCtrl.Image.Compression;
  SaveDialog.Quality     := mcmImageCtrl.Image.Quality;
  SaveDialog.Interlaced  := mcmImageCtrl.Image.Interlaced;
  if (SaveDialog.Execute)
  then begin
       // Set compression matching the file extension CP_xxx ref. TmcmCompress.
       mcmImageCtrl.Image.Compression := SaveDialog.Compression;
       // Set Quality, ref. TmcmImage/TmcmImageFileMgr.Quality
       mcmImageCtrl.Image.Quality     := SaveDialog.Quality;
       // Set Interlaced mode, ref. TmcmImage/TmcmImageFileMgr.Interlaced
       mcmImageCtrl.Image.Interlaced  := SaveDialog.Interlaced;
       // Save image to disk
       mcmImageCtrl.Image.FileSave(SaveDialog.FileName);
  end;
end; // TForm1.SaveItemClick.


procedure TForm1.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TForm1.ExitItemClick.


procedure TForm1.EditMenuClick(Sender : TObject);
begin
  CopyItem.Enabled := Not(mcmImageCtrl.Image.Empty);
  PasteItem.Enabled := Clipboard.HasFormat(CF_DIB);
end; // TForm1.EditMenuClick.


procedure TForm1.CopyItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  AFormat := CF_DIB;
  mcmImageCtrl.Image.SaveToClipboardFormat(AFormat, AData);
  if (AData <> 0)
  then begin
       Clipboard.Open;
       Clipboard.SetAsHandle(AFormat, AData);
       Clipboard.Close;
  end;
end; // TForm1.CopyItemClick.


procedure TForm1.PasteItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_DIB; // NOTE: CF_BITMAP is not supported yet.
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then mcmImageCtrl.Image.LoadFromClipboardFormat(AFormat, AData);
    InitTrace;
  finally
    Clipboard.Close;
  end;
end; // TForm1.PasteItemClick.


procedure TForm1.InitTrace;
begin
  // Set the source image.
  mcmTrace.Clear;

  if Assigned(mcmImage)
  then mcmImage.Free;
  mcmImage := Nil;
end; // TForm1.InitTrace.


procedure TForm1.ClearItemClick(Sender : TObject);
begin
  mcmTraceObject := Nil;
  OldTraceObject := Nil;
  mcmTrace.Clear; // Delete all Trace-objects.
  InitTrace;
  InvalidateRect(Handle, Nil, False);
end; // TForm1.ClearItemClick.


procedure TForm1.mcmImageCtrlMouseUp(Sender : TObject;
                                     Button : TMouseButton;
                                     Shift  : TShiftState;
                                     X, Y   : Integer);
var mcmImageColor : TmcmImageColor;
    mcmImageMorph : TmcmImageMorph;
    Color         : TColorRef;
    Lum           : integer;
    Hue, S, V     : integer;
begin
  if (mcmImageCtrl.Image.Empty)
  then Exit;

  X := Round(X / mcmImageCtrl.Scale);
  Y := Round(Y / mcmImageCtrl.Scale);
  
  mcmImageCtrlThreshold.Clear;
  mcmImageCtrlOutline.Clear;

  mcmTrace.TraceDir := TCD_HORIZ;
  mcmTrace.MinimumPoints := isMinLength.Value;
  mcmTrace.TraceOnEdge := cbTraceOnEdge.Checked;
  mcmTrace.OnlyClose := cbClosedOnly.Checked;

  QueryPerformanceCounter(FStartTime);

  // NOTE:
  // The image assigned to mcmTrace.SourceImage[1] is automatically passed to
  // the traced objects and used to calculate the density/Luminance dependend
  // "shape descriptors".
  // The X and Y resolution used by the traced objects is also taken from this
  // image, ref. mcmTrace.XResolution/YResolution.
  mcmTrace.SourceImage[1] := mcmImageCtrl.Image;

  // The image assigned to mcmTrace.SourceImage[0] is the image that we will
  // trace.
  // How should we pre-process the image?
  case cbMethod.ItemIndex of
  0 : begin // None - No pre-process. Image is already 8 bit Black and White.
        if Assigned(mcmImage)
        then mcmImage.Free;
        mcmImage := Nil;
        mcmTrace.SourceImage[0] := mcmImageCtrl.Image;
      end;
  1 : begin // Color RGB - Threshold colors at "Color" and neighbour colors
            //             within the Tolerance distance.
        Color := mcmImageCtrl.Image.Pixel[x, y];

        mcmImageColor := TmcmImageColor.Create(Self);
        mcmImageColor.SourceImage[0] := mcmImageCtrl.Image;
        mcmImageColor.ThresholdColorBand(Color, isTolerance.Value);
        mcmImage := mcmImageColor.ResultImage;
        mcmImageCtrlThreshold.Image.Assign(mcmImage);
        mcmImageColor.Free;
        mcmTrace.SourceImage[0] := mcmImage;
      end;
  2 : begin // Luminance - Get the luminance image and threshold at "Lum" and
            //             neighbour luminance values within the Tolerance
            //             distance.
        Color := mcmImageCtrl.Image.Pixel[x, y];
        Lum := Round(0.2989 * GetRValue(Color) +
                     0.5867 * GetGValue(Color) +
                     0.1144 * GetBValue(Color));

        mcmImageColor := TmcmImageColor.Create(Self);
        mcmImageColor.SourceImage[0] := mcmImageCtrl.Image;
        mcmImageColor.GetLuminanceChannel;
        mcmImageColor.SourceImage[0] := mcmImageColor.ResultImage;
        mcmImageColor.ThresholdColorBand(RGB(Lum, Lum, Lum), isTolerance.Value);
        mcmImage := mcmImageColor.ResultImage;
        mcmImageCtrlThreshold.Image.Assign(mcmImage);
        mcmImageColor.Free;
        mcmTrace.SourceImage[0] := mcmImage;
      end;
  3 : begin // Hue - Get the Hue image and threshold at "Hue" and neighbour hue
            //       values within the Tolerance distance.
        Color := mcmImageCtrl.Image.Pixel[x, y];
        RGBToHSV(GetRValue(Color), GetGValue(Color), GetBValue(Color), Hue, S, V);
        Hue := Round(255.0 * Hue / 359.0);

        mcmImageColor := TmcmImageColor.Create(Self);
        mcmImageColor.SourceImage[0] := mcmImageCtrl.Image;
        mcmImageColor.GetHueChannel;
        mcmImageColor.SourceImage[0] := mcmImageColor.ResultImage;
        mcmImageColor.ThresholdColorBand(RGB(Hue, Hue, Hue), isTolerance.Value);
        mcmImage := mcmImageColor.ResultImage;
        mcmImageCtrlThreshold.Image.Assign(mcmImage);
        mcmImageColor.Free;
        mcmTrace.SourceImage[0] := mcmImage;
      end;
  end;

  // Should we trace all features in the image?
  if cbTraceAll.Checked
  then begin
       mcmImageMorph := TmcmImageMorph.Create(Self);
       if (mcmImage <> Nil)
       then mcmImageMorph.SourceImage[0] := mcmImage
       else mcmImageMorph.SourceImage[0] := mcmImageCtrl.Image;
       mcmImageMorph.ResultImage := mcmImage;
       mcmImageMorph.EdgeOutline := mcmTrace.TraceOnEdge;
       mcmImageMorph.Outline;
       mcmImage := mcmImageMorph.ResultImage;
       mcmImageCtrlOutline.Image.Assign(mcmImage);
       mcmImageMorph.Free;
       mcmTrace.SourceImage[0] := mcmImage;
  end;

  if cbTraceAll.Checked
  then mcmTrace.ChainCodeAll(255) // Trace all object.
  else mcmTrace.ChainCode(X, Y, 255); // Trace the object nearest (x,y)

  QueryPerformanceCounter(FEndTime);

  if (mcmTrace.Error <> EC_OK)
  then ShowMessage('Error Tracing image: ' + CErrorStrings[word(mcmTrace.Error)]);

  if Assigned(mcmImage)
  then ;//mcmImageCtrl.Image.Assign(mcmImage);

  // Update statusbar with processing time and polygon size.
  ShowTime;

  // Display number of traced objects.
  StatusBar.Panels[2].Text := 'Objects: ' + IntToStr(mcmTrace.Count);

  // Update view, but don't erase background.
  mcmTrace.ShowOn(ScrollBox1);
  InvalidateRect(Handle, Nil, False);

  // Set-up Traced objects OnMouseEvent.
  SetMouseEvent;
end; // TForm1.mcmImageCtrlMouseUp.


procedure TForm1.SetMouseEvent;
var Index : integer;
begin
  // Set each of the traced object OnMouseMove to mcmImageCtrlMouseMove.
  for Index := 0 to (mcmTrace.Count - 1)
  do begin
     mcmTrace.TraceObject[Index].OnMouseMove := mcmImageCtrlMouseMove;
     mcmTrace.TraceObject[Index].Index := Index + 1;
    // mcmTrace.TraceObject[Index].Scale := 2;
  end;
//  mcmImageCtrl.Scale := 1;
end; // TForm1.SetMouseEvent.


procedure TForm1.mcmImageCtrlChange(Sender : TObject);
begin
  if Not(mcmImageCtrl.Center or mcmImageCtrl.ScaleToFit)
  then begin
       ScrollBox1.HorzScrollBar.Range := Round(mcmImageCtrl.Scale * mcmImageCtrl.Image.Width);
       ScrollBox1.VertScrollBar.Range := Round(mcmImageCtrl.Scale * mcmImageCtrl.Image.Height);

       // These two lines prevents ScrollBox from causing flicker, when either
       // scrollbar is at it maximum.
       // Otherwise ScrollBox doesn't cause flicker.
       ValidateRect(ScrollBox1.Handle, Nil);
       mcmImageCtrl.Repaint;
  end
  else begin
       ScrollBox1.HorzScrollBar.Range := 0;
       ScrollBox1.VertScrollBar.Range := 0;
  end;

end;

procedure TForm1.mcmImageCtrlMouseMove(Sender: TObject; Shift: TShiftState; X, Y : Integer);
var xPos, yPos : double;
begin
  // Show the x,y cooredinate scaled by the resolution.
  if (Sender is TmcmImageCtrl)
  then begin
       // OnMouseMove was fired by the TmcmImageCtrl.
       if Not(mcmImageCtrl.Image.Empty)
       then begin
            if (mcmImageCtrl.Image.XResolution > 0)
            then xPos := x / mcmImageCtrl.Image.XResolution
            else xPos := x;
            if (mcmImageCtrl.Image.YResolution > 0)
            then yPos := y / mcmImageCtrl.Image.YResolution
            else yPos := y;
            StatusBar.Panels[0].Text := 'Pixel (x,y): ' +
                                        FloatToStrF(xPos, ffFixed, 16, 2) + ',' +
                                        FloatToStrF(yPos, ffFixed, 16, 2);
       end;
  end;

  if (Sender is TmcmPolygon)
  then begin
       // OnMouseMove was fired by a TmcmPolygon.
       mcmTraceObject := TmcmPolygon(Sender);
       StatusBar.Panels[0].Text := 'Pixel (x,y): ' +
                                   FloatToStrF((x + mcmTraceObject.BoundingRect.Left) / mcmImageCtrl.Image.XResolution, ffFixed, 16, 2) + ',' +
                                   FloatToStrF((y + mcmTraceObject.BoundingRect.Top) / mcmImageCtrl.Image.YResolution, ffFixed, 16, 2);

       // Show where the objects was hit.
       case mcmTraceObject.HitIndex of
       THT_EDGE   : StatusBar.Panels[3].Text := 'Edge';
       THT_INSIDE : StatusBar.Panels[3].Text := 'Inside';
       end;

       ShowData; // Update result grid.
  end
  else StatusBar.Panels[3].Text := '';
end; // TForm1.mcmImageCtrlMouseMove.


procedure TForm1.ShowData;
var SaveCursor : TCursor;
begin
  if Assigned(mcmTraceObject)
  then begin
       if (OldTraceObject = mcmTraceObject)
       then Exit;
       SaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;

       ResetShowShape(OldTraceObject);
       ShowShape(mcmTraceObject);

       OldTraceObject := mcmTraceObject;

       //
       // mcmTraceObject.Image := mcmImageCtrl.Image;
       // mcmTraceObject.XResolution := 0.5;
       // mcmTraceObject.YResolution := 1.0;

       QueryPerformanceCounter(FStartTime);

       sgData.RowCount := 36;
       sgData.Cells[0,0] := 'Description';
       sgData.Cells[1,0] := 'Value';

       sgData.Cells[0,1] := 'Outline Centroid X';
       sgData.Cells[1,1] := FloatToStrF(mcmTraceObject.GetOutlineCentroid.x, ffFixed, 16, 4);
       sgData.Cells[0,2] := 'Outline Centroid Y';
       sgData.Cells[1,2] := FloatToStrF(mcmTraceObject.GetOutlineCentroid.y, ffFixed, 16, 4);

       sgData.Cells[0,3] := 'Area Centroid X';
       sgData.Cells[1,3] := FloatToStrF(mcmTraceObject.GetAreaCentroid.x, ffFixed, 16, 4);
       sgData.Cells[0,4] := 'Area Centroid Y';
       sgData.Cells[1,4] := FloatToStrF(mcmTraceObject.GetAreaCentroid.y, ffFixed, 16, 4);

       sgData.Cells[0,5] := 'Density Centroid X';
       sgData.Cells[1,5] := FloatToStrF(mcmTraceObject.GetDensityCentroid.x, ffFixed, 16, 4);
       sgData.Cells[0,6] := 'Density Centroid Y';
       sgData.Cells[1,6] := FloatToStrF(mcmTraceObject.GetDensityCentroid.y, ffFixed, 16, 4);

       sgData.Cells[0,7] := 'Luminance Centroid X';
       sgData.Cells[1,7] := FloatToStrF(mcmTraceObject.GetLuminanceCentroid.x, ffFixed, 16, 4);
       sgData.Cells[0,8] := 'Luminance Centroid Y';
       sgData.Cells[1,8] := FloatToStrF(mcmTraceObject.GetLuminanceCentroid.y, ffFixed, 16, 4);

       sgData.Cells[0,9] := 'Area';
       sgData.Cells[1,9] := FloatToStrF(mcmTraceObject.GetArea, ffFixed, 16, 4);
       sgData.Cells[0,10] := 'Perimeter';
       sgData.Cells[1,10] := FloatToStrF(mcmTraceObject.GetPerimeter, ffFixed, 16, 4);

       sgData.Cells[0,11] := 'Breath';
       sgData.Cells[1,11] := FloatToStrF(mcmTraceObject.GetBreadth, ffFixed, 16, 4);
       sgData.Cells[0,12] := 'Length';
       sgData.Cells[1,12] := FloatToStrF(mcmTraceObject.GetLength, ffFixed, 16, 4);
       sgData.Cells[0,13] := 'Max. Feret';
       sgData.Cells[1,13] := FloatToStrF(mcmTraceObject.GetMaxFeret, ffFixed, 16, 4);

       sgData.Cells[0,14] := 'Aspect Ratio';
       sgData.Cells[1,14] := FloatToStrF(mcmTraceObject.GetAspectRatio, ffFixed, 16, 4);

       sgData.Cells[0,15] := 'Orientation';
       sgData.Cells[1,15] := FloatToStrF(mcmTraceObject.GetOrientation, ffFixed, 16, 4);


       sgData.Cells[0,16] := 'Max. Radius';
       sgData.Cells[1,16] := FloatToStrF(mcmTraceObject.GetMaxRadius, ffFixed, 16, 4);
       sgData.Cells[0,17] := 'Min. Radius';
       sgData.Cells[1,17] := FloatToStrF(mcmTraceObject.GetMinRadius, ffFixed, 16, 4);

       sgData.Cells[0,18] := 'Compactness';
       sgData.Cells[1,18] := FloatToStrF(mcmTraceObject.GetCompactness, ffFixed, 16, 4);
       sgData.Cells[0,19] := 'Convexity';
       sgData.Cells[1,19] := FloatToStrF(mcmTraceObject.GetConvexity, ffFixed, 16, 4);

       sgData.Cells[0,20] := 'Elongation';
       sgData.Cells[1,20] := FloatToStrF(mcmTraceObject.GetElongation, ffFixed, 16, 4);

       sgData.Cells[0,21] := 'Form Factor';
       sgData.Cells[1,21] := FloatToStrF(mcmTraceObject.GetFormFactor, ffFixed, 16, 4);
       sgData.Cells[0,22] := 'Heywood Diameter';
       sgData.Cells[1,22] := FloatToStrF(mcmTraceObject.GetHeywoodDiameter, ffFixed, 16, 4);

       sgData.Cells[0,23] := 'Roundness';
       sgData.Cells[1,23] := FloatToStrF(mcmTraceObject.GetRoundness, ffFixed, 16, 4);
       sgData.Cells[0,24] := 'Rounghness';
       sgData.Cells[1,24] := FloatToStrF(mcmTraceObject.GetRoughness, ffFixed, 16, 4);
       sgData.Cells[0,25] := 'Solidity';
       sgData.Cells[1,25] := FloatToStrF(mcmTraceObject.GetSolidity, ffFixed, 16, 4);

       sgData.Cells[0,26] := 'Average Density';
       sgData.Cells[1,26] := FloatToStrF(mcmTraceObject.GetAverageDensity, ffFixed, 16, 4);
       sgData.Cells[0,27] := 'Average Luminance';
       sgData.Cells[1,27] := FloatToStrF(mcmTraceObject.GetAverageLuminance, ffFixed, 16, 4);
       sgData.Cells[0,28] := 'Sum Density';
       sgData.Cells[1,28] := FloatToStrF(mcmTraceObject.GetSumDensity, ffFixed, 16, 4);
       sgData.Cells[0,29] := 'Sum Luminance';
       sgData.Cells[1,29] := FloatToStrF(mcmTraceObject.GetSumLuminance, ffFixed, 16, 4);

       sgData.Cells[0,30] := 'Pixel Count';
       sgData.Cells[1,30] := IntToStr(mcmTraceObject.PixelArea);

       QueryPerformanceCounter(FEndTime);

       // Update statusbar with processing time and polygon size.
       ShowTime;
       Screen.Cursor := SaveCursor;
  end;
end; // TForm1.ShowData.


procedure TForm1.ShowTime;
var Duration : double;
begin
  // Display the processing time in the status bar.
  {$IFDEF DCB3}
    Duration := (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart;
  {$ELSE}
    Duration := (FEndTime - FStartTime) / FPerformanceFreq;
  {$ENDIF}

  if (Duration < 0.001)
  then StatusBar.Panels[1].Text := 'Time: ' + FloatToStrF(1000000.0 * Duration, ffFixed, 15, 4) + ' us'
  else if (Duration < 1.0)
       then StatusBar.Panels[1].Text := 'Time: ' + FloatToStrF(1000.0 * Duration, ffFixed, 15, 4) + ' ms'
       else StatusBar.Panels[1].Text := 'Time: ' + FloatToStrF(Duration, ffFixed, 15, 4) + ' s';
end; // TForm1.ShowTime


procedure TForm1.Panel1Resize(Sender: TObject);
begin
  sgData.Top := cbTraceAll.Top + cbTraceAll.Height + 8;
  sgData.Height := Panel1.Height - sgData.Top;
end; // TForm1.Panel1Resize.


procedure TForm1.sgDataSelectCell(Sender : TObject; Col, Row : Integer; var CanSelect : Boolean);
begin
  // We clicked in a cell, let us show its "Shape factor" of screen.
  // (see mcmImageCtrlPainted below)
  FShapeIndex := Row;
  ResetShowShape(mcmTraceObject);
  ShowShape(mcmTraceObject);
  Windows.InvalidateRect(Handle, Nil, False);
end; // TForm1.sgDataSelectCell.


procedure TForm1.ShowShape(Shape : TmcmPolygon);
begin
  if Assigned(Shape)
  then begin
       Shape.ShowIndex           := True; //(FShapeIndex = );
       Shape.ShowOutlineCenter   := (FShapeIndex = 1) or (FShapeIndex = 2);
       Shape.ShowAreaCenter      := (FShapeIndex = 3) or (FShapeIndex = 4);
       Shape.ShowDensityCenter   := (FShapeIndex = 5) or (FShapeIndex = 6);
       Shape.ShowLuminanceCenter := (FShapeIndex = 7) or (FShapeIndex = 8);
       Shape.ShowBreadth         := (FShapeIndex = 11);
       Shape.ShowLength          := (FShapeIndex = 12);
       Shape.ShowMaxFeret        := (FShapeIndex = 13);
       // Orientation = (FShapeIndex = 15);
       Shape.ShowMaxRadius       := (FShapeIndex = 16);
       Shape.ShowMinRadius       := (FShapeIndex = 17);
       Shape.ShowConvex          := (FShapeIndex = 19);
       Shape.ShowSmallestCircle  := (FShapeIndex = 23);
  end;
end; // TForm1.ShowShape.


procedure TForm1.ResetShowShape(Shape : TmcmPolygon);
begin
  if Assigned(Shape)
  then begin
       Shape.ShowAreaCenter      := False;
       Shape.ShowBreadth         := False;
       Shape.ShowConvex          := False;
       Shape.ShowDensityCenter   := False;
//       Shape.ShowIndex           := False;
       Shape.ShowLuminanceCenter := False;
       Shape.ShowLength          := False;
       Shape.ShowMaxFeret        := False;
       Shape.ShowMaxRadius       := False;
       Shape.ShowMinRadius       := False;
       Shape.ShowOutlineCenter   := False;
       Shape.ShowSmallestCircle  := False;
  end;
end; // TForm1.ResetShowShape.


procedure TForm1.rsScaleChange(Sender: TObject);
var Index : integer;
begin
  // Scale/zoom image in and out.
  if Assigned(mcmImageCtrl)
  then begin
       InvalidateRect(Self.Handle, Nil, false);
       mcmImageCtrl.Scale := rsScale.Value;
       mcmImageCtrlChange(Sender);
       for Index := 0 to (mcmTrace.Count - 1)
       do mcmTrace.TraceObject[Index].Scale := rsScale.Value;
  end;
end;

procedure TForm1.cbTraceAllClick(Sender : TObject);
begin
  tsOutline.Visible := cbTraceAll.Checked;
end;

end.
