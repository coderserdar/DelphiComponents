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
// $Log:  17611: uFormOCRLearn.pas 
//
//    Rev 1.4    05-03-2006 10:52:50  mcm    Version: IMG 2.16
// Added check for image being 8 bit grey scale.
//
//   Rev 1.3    30-01-2004 20:47:42  mcm    Version: IMG 2.3
// Corrected the width value displayed for separated glyphs.

//
//   Rev 1.2    17-11-2003 10:08:40  mcm    Version: OCR 1.0

//
//   Rev 1.1    16-10-2003 11:33:28  mcm    Version: OCR 1.0

unit uFormOCRLearn;

{$IFDEF VER100} {$DEFINE DCB3_5}
                {$DEFINE DCB3_6}
                {$DEFINE DCB3}   {$ENDIF} { DELPHI 3  = VER100 }
{$IFDEF VER110} {$DEFINE DCB3_5}
                {$DEFINE DCB3_6}
                {$DEFINE DCB3}   {$ENDIF} { BUILDER 3 = VER110 }
{$IFDEF VER120} {$DEFINE DCB3_5}
                {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER125} {$DEFINE DCB3_5}
                {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER130} {$DEFINE DCB3_5}
                {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER135} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER140} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER145} {$DEFINE DCB3_6} {$ENDIF}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGEOFF}{$R-}{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Buttons, StdCtrls, checklst, Spin, ComCtrls,
  mcmOCR, mcmImage, mcmImageTypeDef, ExtCtrls;

type
  TOnShowGlyphEvent = procedure(Sender    : TObject;
                                GlyphRect : TRect) of object;

  TFormOCRLearn = class(TForm)
    btnClose         : TButton;
    btnOpen          : TButton;
    btnSave          : TButton;
    btnNew           : TButton;
    btnRead          : TButton;
    mcmOCR           : TmcmOCR;
    OpenDialog       : TOpenDialog;
    SaveDialog       : TSaveDialog;
    ImageList        : TImageList;
    pcOCR            : TPageControl;
    tsParameters     : TTabSheet;
    tsLearn          : TTabSheet;
    tsTemplates      : TTabSheet;
    tsResult         : TTabSheet;

    gbGlyphSize      : TGroupBox;
    lGlyphWidth      : TLabel;
    lGlyphHeight     : TLabel;
    seGlyphWidth     : TSpinEdit;
    seGlyphHeight    : TSpinEdit;

    gbGlyphColors    : TGroupBox;
    lGlyphColor      : TLabel;
    lThreshold       : TLabel;
    cbGlyphColor     : TComboBox;
    seThreshold      : TSpinEdit;

    gbGlyphSeg       : TGroupBox;
    lMinWidth        : TLabel;
    lMaxWidth        : TLabel;
    lMinHeight       : TLabel;
    lMaxHeight       : TLabel;
    lMethod          : TLabel;
    cbCutLarge       : TCheckBox;
    seMinWidth       : TSpinEdit;
    seMaxWidth       : TSpinEdit;
    seMinHeight      : TSpinEdit;
    seMaxHeight      : TSpinEdit;
    cbAspectRatio    : TCheckBox;
    cbMethod         : TComboBox;

    gbGlyph          : TGroupBox;
    icActual         : TmcmImageCtrl;
    lLHeightWidth    : TLabel;
    lLWidthVal       : TLabel;
    lGlyph           : TLabel;
    lGlyphGroup      : TLabel;
    lSeparate        : TLabel;
    icResized        : TmcmImageCtrl;
    lActual          : TLabel;
    lResized         : TLabel;
    lMatchError      : TLabel;
    lMatchErrorVal   : TLabel;
    lGuess           : TLabel;
    lGuessVal        : TLabel;
    eGlyph           : TEdit;
    btnNext          : TButton;
    slbGlyphCase     : TCheckListBox;
    btnAdd           : TButton;
    cbSeparators     : TComboBox;
    btnStart         : TButton;

    gbTemplates      : TGroupBox;
    icTemplate       : TmcmImageCtrl;
    lGlyphTemplate   : TLabel;
    sbFirst          : TSpeedButton;
    sbPrevious       : TSpeedButton;
    sbNext           : TSpeedButton;
    sbLast           : TSpeedButton;
    sbDelete         : TSpeedButton;
    eGlypgTemplate   : TEdit;
    sgResults        : TStringGrid;

    cbThreshold      : TComboBox;
    lLevel           : TLabel;


    tsAutoLearn      : TTabSheet;
    gbFonts          : TGroupBox;
    clbFonts         : TCheckListBox;
    btnCheckAll      : TButton;
    btnUncheckAll    : TButton;
    ImageCtrlFonts   : TmcmImageCtrl;
    btnAutoLearn     : TButton;

    lFonts           : TLabel;
    lLearningFont    : TLabel;
    lLearningFontVal : TLabel;
    lGlyphIndex      : TLabel;
    lGlyphIndexVal   : TLabel;
    pReadError: TPanel;
    lSumError: TLabel;
    lSumErrorVal: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    clbGlyphGroup: TCheckListBox;
    cbNormal: TCheckBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;

    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormActivate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : Boolean);

    procedure btnCloseClick(Sender : TObject);
    procedure btnNewClick(Sender : TObject);
    procedure btnOpenClick(Sender : TObject);
    procedure btnSaveClick(Sender : TObject);
    procedure btnReadClick(Sender : TObject);

    procedure seGlyphWidthChange(Sender : TObject);
    procedure seGlyphHeightChange(Sender : TObject);

    procedure cbGlyphColorChange(Sender : TObject);
    procedure seThresholdChange(Sender : TObject);

    procedure cbMethodChange(Sender : TObject);
    procedure seMinWidthChange(Sender : TObject);
    procedure seMinHeightChange(Sender : TObject);
    procedure seMaxWidthChange(Sender : TObject);
    procedure seMaxHeightChange(Sender : TObject);
    procedure cbCutLargeClick(Sender : TObject);
    procedure cbAspectRatioClick(Sender : TObject);

    procedure cbSeparatorsChange(Sender : TObject);
    procedure eGlyphChange(Sender : TObject);
    procedure slbGlyphCaseClickCheck(Sender : TObject);
    procedure btnStartClick(Sender : TObject);
    procedure btnAddClick(Sender : TObject);
    procedure btnNextClick(Sender : TObject);

    procedure sbFirstClick(Sender : TObject);
    procedure sbPreviousClick(Sender : TObject);
    procedure sbLastClick(Sender : TObject);
    procedure sbDeleteClick(Sender : TObject);
    procedure sbNextClick(Sender : TObject);
    procedure cbThresholdChange(Sender : TObject);
    procedure btnCheckAllClick(Sender : TObject);
    procedure btnUncheckAllClick(Sender : TObject);
    procedure btnAutoLearnClick(Sender : TObject);
    procedure gbFontsExit(Sender : TObject);
    procedure sgResultsSelectCell(Sender : TObject; Col, Row : Integer; var CanSelect : Boolean);
    procedure clbGlyphGroupClickCheck(Sender: TObject);
    procedure mcmOCRDuplicateGlyph(Sender: TObject; Index : integer; var Replace: Boolean);
    procedure cbFontStyleClick(Sender: TObject);
  private
    { Private declarations }
    FOnShowGlyph   : TOnShowGlyphEvent;
    FStartTime     : int64;
    FEndTime       : int64;
    FOnProcessTime : TOnProcessTime;
    FOCRFileName   : string;
    FGlyphIndex    : integer;
    FGlyphRect     : TRect;
    FGroup         : TmcmGlyphGroup;
    FSeparateAt    : integer;
    FError         : TmcmErrorCode;
    procedure BuilderFontList;
    procedure InitImage(AnImage : TmcmImage);
    procedure ClearResults;
    function  GetImage : TmcmImage;
    function  GetRegion : TRect;
    procedure GetOCRInfo;
    procedure SetImage(Value : TmcmImage);
    procedure SetOCRFileName(Value : string);
    procedure SetRegion(Value : TRect);
    procedure ShowGlyph;
    procedure ShowResizedGlyph(AnImageCtrl : TmcmImageCtrl);
    procedure ShowTemplate(AnImageCtrl : TmcmImageCtrl; AGlyph : PmcmGlyph);
  public
    { Public declarations }
    property  Error : TmcmErrorCode
      read    FError;
    property  Image : TmcmImage
      read    GetImage
      write   SetImage;
    property  OCRFileName : string
      read    FOCRFileName
      write   SetOCRFileName;
    property  Region : TRect
      read    GetRegion
      write   SetRegion;
    property  OnProcessTime : TOnProcessTime
      read    FOnProcessTime
      write   FOnProcessTime;
    property  OnShowGlyph : TOnShowGlyphEvent
      read    FOnShowGlyph
      write   FOnShowGlyph;
  end;

var FormOCRLearn : TFormOCRLearn;

implementation

uses mcmImageResStr;

{$R *.DFM}
procedure TFormOCRLearn.FormCreate(Sender : TObject);
begin
  FOnProcessTime := Nil;
  pcOCR.ActivePage := tsParameters;
  ClearResults;

  // Set-up default settings for buttons etc.
  btnAdd.Enabled := False;
  slbGlyphCase.Checked[0] := True;

  icActual.Image.ImageFormat := IF_GREY8;
  icActual.Image.Height := 25;
  icActual.Image.Width  := 75;
  InitImage(icActual.Image);

  icResized.Image.ImageFormat := IF_GREY8;
  icResized.Image.Height := 8;
  icResized.Image.Width  := 6;
  InitImage(icResized.Image);

  icTemplate.Image.ImageFormat := IF_GREY8;
  icTemplate.Image.Height := 8;
  icTemplate.Image.Width  := 6;
  InitImage(icTemplate.Image);

  cbSeparators.Clear;
  cbSeparators.Items.AddObject('None', Pointer(0));
  cbSeparators.ItemIndex := 0;
  cbThreshold.ItemIndex  := 1;

  cbMethod.ItemIndex := 1;

  FOCRFileName := '';
  FOnShowGlyph := Nil;
  GetOCRInfo;

  lLearningFontVal.Caption := '';
  FError := EC_OK;
end; // TFormOCRLearn.FormCreate.


procedure TFormOCRLearn.FormDestroy(Sender : TObject);
begin
  FormOCRLearn := Nil;
end; // TFormOCRLearn.FormDestroy.


procedure TFormOCRLearn.FormActivate(Sender : TObject);
var HasImage : boolean;
begin
  if Not(Assigned(mcmOCR.Image))
  then HasImage := False
  else HasImage := Not(mcmOCR.Image.Empty);

  btnNew.Enabled := HasImage;
  btnOpen.Enabled := HasImage;
  btnStart.Enabled := HasImage;
  pcOCR.Enabled := HasImage;
end; // TFormOCRLearn.FormActivate.


procedure TFormOCRLearn.FormShow(Sender : TObject);
begin
  BuilderFontList;
end; // TFormOCRLearn.FormShow.


procedure TFormOCRLearn.FormClose(    Sender : TObject;
                                  var Action : TCloseAction);
begin
  Action := caFree;
end; // TFormOCRLearn.FormClose.


procedure TFormOCRLearn.FormCloseQuery(    Sender   : TObject;
                                       var CanClose : Boolean);
begin
  CanClose := True;
end; // TFormOCRLearn.FormCloseQuery.


procedure TFormOCRLearn.btnCloseClick(Sender : TObject);
var TmpRect : TRect;
begin
  // Was the OCR file saved?
  if btnSave.Enabled
  then begin
       
  end;

  if Assigned(FOnShowGlyph)
  then begin
       TmpRect.Left   := -1;
       TmpRect.Top    := -1;
       TmpRect.Right  := -1;
       TmpRect.Bottom := -1;
       FOnShowGlyph(Self, TmpRect);
  end;
  Close;
end; // TFormOCRLearn.btnCloseClick.


procedure TFormOCRLearn.SetOCRFileName(Value : string);
begin
  if FileExists(Value)
  then begin
       FOCRFileName := Value;
       mcmOCR.LoadFromFile(FOCRFileName);
       Caption := 'OCR Learn [' + ExtractFilename(FOCRFileName) + ']';

       // Update OCR data.
       GetOCRInfo;

       btnSave.Enabled       := False;
       btnRead.Enabled       := True;
       seGlyphWidth.Enabled  := False;
       seGlyphHeight.Enabled := False;
       sbFirstClick(Self);
  end;
end; // TFormOCRLearn.SetOCRFileName.


procedure TFormOCRLearn.btnOpenClick(Sender : TObject);
begin
  if OpenDialog.Execute
  then begin
       // Load OCR patterns from file
       SetOCRFileName(OpenDialog.FileName);
  end;
end; // TFormOCRLearn.btnOpenClick.


procedure TFormOCRLearn.btnSaveClick(Sender : TObject);
begin
  SaveDialog.FileName := FOCRFileName;
  if SaveDialog.Execute
  then begin
       // Save OCR patterns from file
       FOCRFileName := SaveDialog.FileName;
       mcmOCR.SaveToFile(FOCRFileName);
       Caption := 'OCR Learn [' + ExtractFilename(FOCRFileName) + ']';
  end;
  btnSave.Enabled := False;
end; // TFormOCRLearn.btnSaveClick.


procedure TFormOCRLearn.btnNewClick(Sender : TObject);
begin
  btnSave.Enabled       := False;
  btnRead.Enabled       := False;
  seGlyphWidth.Enabled  := True;
  seGlyphHeight.Enabled := True;

  FOCRFileName := '';
  mcmOCR.Clear;
  GetOCRInfo;
end; // TFormOCRLearn.btnNewClick.


procedure TFormOCRLearn.btnReadClick(Sender : TObject);
var i, j     : integer;
    Error    : single;
    SumError : single;
begin
  pcOCR.ActivePage := tsResult;
  ClearResults;
  QueryPerformanceCounter(FStartTime);
  mcmOCR.ClearLines;
  mcmOCR.ThresholdLevel := seThreshold.Value;
  mcmOCR.ThresholdMethod := TmcmThreshold(cbThreshold.ItemIndex);
  mcmOCR.Threshold;
  mcmOCR.LocateGlyphs;

  SumError := 0.0;
  lSumErrorVal.Caption := '';
  j := 1;
  if (mcmOCR.GetFirstGlyph(FGlyphRect) <> 0)
  then begin
       i := mcmOCR.MatchAndCut(Error, FGlyphRect);
       if (i >= 0)
       then sgResults.Cells[0, j] := mcmOCR.Glyph[i].Identifier
       else sgResults.Cells[0, j] := '???';
       if (SumError < Error)
       then SumError := Error;
       sgResults.Cells[1, j] := FloatToStrF(Error, ffFixed, 7, 4);
       sgResults.Cells[2, j] := IntToStr(FGlyphRect.Left);
       sgResults.Cells[3, j] := IntToStr(FGlyphRect.Top);
       sgResults.Cells[4, j] := IntToStr(1 + FGlyphRect.Right - FGlyphRect.Left);
       sgResults.Cells[5, j] := IntToStr(1 + FGlyphRect.Bottom - FGlyphRect.Top);
       inc(j);

       while (mcmOCR.GetNextGlyph(FGlyphRect) <> 0)
       do begin
           i := mcmOCR.MatchAndCut(Error, FGlyphRect);
           if (i >= 0)
           then sgResults.Cells[0, j] := mcmOCR.Glyph[i].Identifier
           else sgResults.Cells[0, j] := '???';
           if (SumError < Error)
           then SumError := Error;
           sgResults.Cells[1, j] := FloatToStrF(Error, ffFixed, 7, 4);
           sgResults.Cells[2, j] := IntToStr(FGlyphRect.Left);
           sgResults.Cells[3, j] := IntToStr(FGlyphRect.Top);
           sgResults.Cells[4, j] := IntToStr(1 + FGlyphRect.Right - FGlyphRect.Left);
           sgResults.Cells[5, j] := IntToStr(1 + FGlyphRect.Bottom - FGlyphRect.Top);
           inc(j);
       end;
       sgResults.RowCount := j + 1;
       lSumErrorVal.Caption := FloatToStrF(SumError, ffFixed, 7, 4);
  end;

  QueryPerformanceCounter(FEndTime);
  if Assigned(FOnProcessTime)
  then FOnProcessTime(Self, FStartTime, FEndTime);
end; // TFormOCRLearn.btnReadClick.


function TFormOCRLearn.GetImage : TmcmImage;
begin
  Result := mcmOCR.Image;
end; // TFormOCRLearn.GetImage.


procedure TFormOCRLearn.SetImage(Value : TmcmImage);
begin
  mcmOCR.Image := Value;
  FError := mcmOCR.Error;
end; // TFormOCRLearn.SetImage.


function TFormOCRLearn.GetRegion : TRect;
begin
  // Returned search region.
  Result := mcmOCR.Region;
end; // TFormOCRLearn.GetRegion.


procedure TFormOCRLearn.SetRegion(Value : TRect);
begin
  mcmOCR.Region := Value;
end; // TFormOCRLearn.SetRegion.


procedure TFormOCRLearn.GetOCRInfo;
begin
  seGlyphWidth.Value      := mcmOCR.GlyphWidth;
  seGlyphHeight.Value     := mcmOCR.GlyphHeight;
  cbGlyphColor.ItemIndex  := integer(mcmOCR.GlyphColor);
  cbThreshold.ItemIndex   := -1;
  cbThreshold.ItemIndex   := integer(mcmOCR.ThresholdMethod);
  seThreshold.Value       := mcmOCR.ThresholdLevel;

  cbMethod.ItemIndex      := integer(mcmOCR.MatchMethod);

  seMinHeight.Value       := mcmOCR.GlyphMinHeight;
  seMaxHeight.Value       := mcmOCR.GlyphMaxHeight;
  seMinWidth.Value        := mcmOCR.GlyphMinWidth;
  seMaxWidth.Value        := mcmOCR.GlyphMaxWidth;

  cbCutLarge.Checked      := mcmOCR.CutLargeGlyphs;
  cbAspectRatio.Checked   := mcmOCR.UseAspectRatio;

  icTemplate.Image.Height := mcmOCR.GlyphHeight;
  icTemplate.Image.Width  := mcmOCR.GlyphWidth;
  InitImage(icTemplate.Image);
end; // TFormOCRLearn.GetOCRInfo.


procedure TFormOCRLearn.InitImage(AnImage : TmcmImage);
var Palette     : PLogPalette;
    NoColors, i : integer;
begin
  AnImage.CreateGreyPalette;
  NoColors := AnImage.GetPaletteEntries(Nil);
  if (NoColors > 0)
  then begin
       GetMem(Palette, NoColors * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
       if Assigned(Palette)
       then begin
            AnImage.GetPaletteEntries(Palette);
            Palette^.palNumEntries := NoColors;

            // Birght grey entry.
            i := 251;
            Palette^.palPalEntry[i].peRed   := 192;
            Palette^.palPalEntry[i].peGreen := 192;
            Palette^.palPalEntry[i].peBlue  := 192;
            Palette^.palPalEntry[i].peFlags := 0;

            // Blue entry.
            i := 252;
            Palette^.palPalEntry[i].peRed   := 0;
            Palette^.palPalEntry[i].peGreen := 0;
            Palette^.palPalEntry[i].peBlue  := 255;
            Palette^.palPalEntry[i].peFlags := 0;

            // Green entry.
            i := 253;
            Palette^.palPalEntry[i].peRed   := 0;
            Palette^.palPalEntry[i].peGreen := 255;
            Palette^.palPalEntry[i].peBlue  := 0;
            Palette^.palPalEntry[i].peFlags := 0;

            // Red entry.
            i := 254;
            Palette^.palPalEntry[i].peRed   := 255;
            Palette^.palPalEntry[i].peGreen := 0;
            Palette^.palPalEntry[i].peBlue  := 0;
            Palette^.palPalEntry[i].peFlags := 0;

            AnImage.SetPaletteEntries(Palette);
            FreeMem(Palette, NoColors * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
       end;
  end;
  AnImage.FillAll(255);
end; // TFormOCRLearn.InitImage.


//------------------------------------------------------------------------------
// Parameters page
//------------------------------------------------------------------------------

procedure TFormOCRLearn.seGlyphWidthChange(Sender : TObject);
begin
  if (seGlyphWidth.Text <> '')
  then begin
       mcmOCR.GlyphWidth := seGlyphWidth.Value;
       if (icResized.Image.Width <> mcmOCR.GlyphWidth)
       then begin
            icResized.Image.Width := mcmOCR.GlyphWidth;
            InitImage(icResized.Image);
            icTemplate.Image.Width := mcmOCR.GlyphWidth;
            InitImage(icTemplate.Image);
            btnSave.Enabled := True;
       end;
  end;
end; // TFormOCRLearn.seGlyphWidthChange.


procedure TFormOCRLearn.seGlyphHeightChange(Sender : TObject);
begin
  if (seGlyphHeight.Text <> '')
  then begin
       mcmOCR.GlyphHeight := seGlyphHeight.Value;
       if (icResized.Image.Height <> mcmOCR.GlyphHeight)
       then begin
            icResized.Image.Height := mcmOCR.GlyphHeight;
            InitImage(icResized.Image);
            icTemplate.Image.Height := mcmOCR.GlyphHeight;
            InitImage(icTemplate.Image);
            btnSave.Enabled := True;
       end;
  end;
end; // TFormOCRLearn.seGlyphHeightChange.


procedure TFormOCRLearn.cbGlyphColorChange(Sender : TObject);
begin
  mcmOCR.GlyphColor := TmcmGlyphColor(cbGlyphColor.ItemIndex);
  btnSave.Enabled := True;
end; // TFormOCRLearn.cbGlyphColorChange.


procedure TFormOCRLearn.cbThresholdChange(Sender: TObject);
begin
  if (mcmOCR.ThresholdMethod <> TmcmThreshold(cbThreshold.ItemIndex))
  then begin
       mcmOCR.ThresholdMethod := TmcmThreshold(cbThreshold.ItemIndex);
       seThreshold.Value := mcmOCR.ThresholdLevel;
       btnSave.Enabled := True;
  end;
  if (mcmOCR.ThresholdMethod in [TH_LEVEL,TH_SYMMETRY])
  then seThreshold.Enabled := True
  else seThreshold.Enabled := False;
end;

procedure TFormOCRLearn.seThresholdChange(Sender : TObject);
begin
  if (mcmOCR.ThresholdLevel <> seThreshold.Value)
  then begin
       mcmOCR.ThresholdLevel := seThreshold.Value;
       btnSave.Enabled := True;
  end;
end; // TFormOCRLearn.seThresholdChange.


procedure TFormOCRLearn.cbMethodChange(Sender : TObject);
begin
  mcmOCR.MatchMethod := TmcmGlyphMatch(cbMethod.ItemIndex);
  btnSave.Enabled := True;
end; // TFormOCRLearn.cbMethodChange.


procedure TFormOCRLearn.seMinWidthChange(Sender : TObject);
begin
  if (seMinWidth.Text <> '')
  then if (mcmOCR.GlyphMinWidth <> seMinWidth.Value)
       then begin
            mcmOCR.GlyphMinWidth := seMinWidth.Value;
            btnSave.Enabled := True;
       end;
end; // TFormOCRLearn.seMinWidthChange.


procedure TFormOCRLearn.seMinHeightChange(Sender : TObject);
begin
  if (seMinHeight.Text <> '')
  then if (mcmOCR.GlyphMinHeight <> seMinHeight.Value)
       then begin
            mcmOCR.GlyphMinHeight := seMinHeight.Value;
            btnSave.Enabled := True;
       end;
end; // TFormOCRLearn.seMinHeightChange.


procedure TFormOCRLearn.seMaxWidthChange(Sender : TObject);
begin
  if (seMaxWidth.Text <> '')
  then if (mcmOCR.GlyphMaxWidth <> seMaxWidth.Value)
       then begin
            mcmOCR.GlyphMaxWidth := seMaxWidth.Value;
            btnSave.Enabled := True;
       end;
end; // TFormOCRLearn.seMaxWidthChange.


procedure TFormOCRLearn.seMaxHeightChange(Sender : TObject);
begin
  if (seMaxHeight.Text <> '')
  then if (mcmOCR.GlyphMaxHeight <> seMaxHeight.Value)
       then begin
            mcmOCR.GlyphMaxHeight := seMaxHeight.Value;
            btnSave.Enabled := True;
       end;
end; // TFormOCRLearn.seMaxHeightChange.


procedure TFormOCRLearn.cbCutLargeClick(Sender : TObject);
begin
  mcmOCR.CutLargeGlyphs := cbCutLarge.Checked;
  btnSave.Enabled := True;
end; // TFormOCRLearn.cbCutLargeClick.


procedure TFormOCRLearn.cbAspectRatioClick(Sender : TObject);
begin
  mcmOCR.UseAspectRatio := cbAspectRatio.Checked;
  btnSave.Enabled := True;
end; // TFormOCRLearn.cbAspectRatioClick.


//------------------------------------------------------------------------------
// Learn page
//------------------------------------------------------------------------------


procedure TFormOCRLearn.ShowGlyph;
var x, y    : integer;
    TmpRect : TRect;
    Feature : TColor;
begin
  // Show found glyph(s).
  if (mcmOCR.GlyphColor = GC_BLACK_ON_WHITE)
  then Feature := 0
  else Feature := RGB(255, 255, 255);
  icActual.Image.Height := (FGlyphRect.Bottom - FGlyphRect.Top) + 1;
  icActual.Image.Width  := (FGlyphRect.Right - FGlyphRect.Left) + 1;
  InitImage(icActual.Image);
  icActual.Image.FillAll(255);

  for y := 0 to (FGlyphRect.Bottom - FGlyphRect.Top)
  do begin
     for x := 0 to (FGlyphRect.Right - FGlyphRect.Left)
     do if (mcmOCR.ImageThreshold.Canvas.Pixels[x+FGlyphRect.Left,y+FGlyphRect.Top] = Feature)
        then begin
             if ((x + FGlyphRect.Left) < longint(FSeparateAt))
             then icActual.Image.Canvas.Pixels[x,y] := RGB(255, 0, 0)
             else icActual.Image.Canvas.Pixels[x,y] := RGB(192, 192, 192);
        end
        else icActual.Image.Canvas.Pixels[x,y] := RGB(255, 255, 255);
  end;
  icActual.Invalidate;

  if Assigned(FOnShowGlyph)
  then begin
       TmpRect := FGlyphRect;
       TmpRect.Right := longint(FSeparateAt);
       TmpRect.Bottom := TmpRect.Bottom + 1;
       FOnShowGlyph(Self, TmpRect);
  end;
end; // TFormOCRLearn.ShowGlyph.


procedure TFormOCRLearn.ShowResizedGlyph(AnImageCtrl : TmcmImageCtrl);
var ResizeRect : TRect;
    AGlyph     : PmcmGlyph;
begin
  ResizeRect := FGlyphRect;
  ResizeRect.Right := FSeparateAt - 1;
  AnImageCtrl.Image.FillAll(128);
  mcmOCR.GetGlyphTopBottom(ResizeRect);
  AGlyph := mcmOCR.ResizeGlyph(ResizeRect);
  ShowTemplate(AnImageCtrl, AGlyph);
  AnImageCtrl.Invalidate;
end; // TFormOCRLearn.ShowResizedGlyph.


procedure TFormOCRLearn.ShowTemplate(AnImageCtrl : TmcmImageCtrl; AGlyph : PmcmGlyph);
var i, j, k : integer;
begin
  if Assigned(AGlyph)
  then begin
       k := 0;
       for j := 0 to (AGlyph^.Height - 1)
       do begin
          for i := 0 to (AGlyph^.Width - 1)
          do begin
             case AGlyph^.Pattern^[k] of
             0 : AnImageCtrl.Image.Canvas.Pixels[i,j] := RGB(0, 0, 0);       // Feature
             1 : AnImageCtrl.Image.Canvas.Pixels[i,j] := RGB(128, 128, 128); // holes
             2 : AnImageCtrl.Image.Canvas.Pixels[i,j] := RGB(255, 255, 255); // Background
             end;
             inc(k);
          end;
       end;

       if (AnImageCtrl = icTemplate)
       then begin
            eGlypgTemplate.Text := AGlyph^.Identifier;
      end;
  end;
end; // TFormOCRLearn.ShowTemplate.


procedure TFormOCRLearn.eGlyphChange(Sender : TObject);
begin
  if eGlyph.Enabled
  then if (Length(eGlyph.Text) > 0)
       then begin
            btnAdd.Enabled := True;
       end;
end; // TFormOCRLearn.eGlyphChange.


procedure TFormOCRLearn.slbGlyphCaseClickCheck(Sender : TObject);
var i, j : integer;
begin
  i := slbGlyphCase.ItemIndex;
  if (slbGlyphCase.State[i] = cbChecked)
  then begin
       for j := 0 to (slbGlyphCase.Items.Count - 1)
       do begin
          if (j <> i)
          then slbGlyphCase.State[j] := cbUnChecked;
       end;
  end
  else slbGlyphCase.State[i] := cbChecked;
  case i of
  -1, 0 : FGroup := OGG_ALL;
  else FGroup := TmcmGlyphGroup(i);
  end;
end; // TFormOCRLearn.slbGlyphCaseClickCheck.


procedure TFormOCRLearn.cbSeparatorsChange(Sender : TObject);
var i     : integer;
    Error : Single;
begin
  if (cbSeparators.ItemIndex <= 0)
  then begin
       mcmOCR.SeparateAt := 0;
       FSeparateAt := FGlyphRect.Right + 1;
  end
  else begin
       FSeparateAt := cardinal(cbSeparators.Items.Objects[cbSeparators.ItemIndex]);
       mcmOCR.SeparateAt := FSeparateAt;
  end;
  ShowGlyph;
  ShowResizedGlyph(icResized);

  // Try a match
  i := mcmOCR.Match(Error, FGlyphRect);
  lMatchErrorVal.Caption := FloatToStrF(Error, ffFixed, 7, 4);
  if (i >= 0)
  then lGuessVal.Caption := mcmOCR.Glyph[i].Identifier;
  lLWidthVal.Caption  := IntToStr(FSeparateAt - FGlyphRect.Left) + ' x ' +
                         IntToStr(FGlyphRect.Bottom - FGlyphRect.Top + 1);
end; // TFormOCRLearn.cbSeparatorsChange.


procedure TFormOCRLearn.btnStartClick(Sender : TObject);
begin
  pcOCR.ActivePage := tsLearn;
  QueryPerformanceCounter(FStartTime);
  mcmOCR.ClearLines;
  mcmOCR.ThresholdMethod := TmcmThreshold(cbThreshold.ItemIndex);
  mcmOCR.Threshold;
  mcmOCR.LocateGlyphs;
  QueryPerformanceCounter(FEndTime);

  if (icActual.Image.Height <> mcmOCR.MaxFoundHeight)
  then icActual.Image.Height := mcmOCR.MaxFoundHeight;
  if (icActual.Image.Width <> mcmOCR.MaxFoundWidth)
  then icActual.Image.Width := mcmOCR.MaxFoundWidth;
  InitImage(icActual.Image);

  btnNextClick(Nil);
end; // TFormOCRLearn.btnStartClick.


procedure TFormOCRLearn.btnAddClick(Sender : TObject);
var Id : WideChar;
begin
  btnSave.Enabled       := True;
  btnRead.Enabled       := True;
  seGlyphWidth.Enabled  := False;
  seGlyphHeight.Enabled := False;

  btnAdd.Enabled := False;
  Id := WideChar(eGlyph.Text[1]);

  if (FGlyphRect.Right > FSeparateAt)
  then FGlyphRect.Right := FSeparateAt - 1;

  mcmOCR.AddGlyph(Id, FGroup, FGlyphRect);

  // Automatically go to next glyph.
  btnNextClick(Sender);
end; // TFormOCRLearn.btnAddClick.


procedure TFormOCRLearn.btnNextClick(Sender : TObject);
var Found      : boolean;
    i, Count   : integer;
    Separators : array[0..9] of cardinal;
    Ranking    : array[0..9] of byte;
    Error      : single;
begin
  btnAdd.Enabled := False;
  eGlyph.Text := '';
  if (Sender = Nil)
  then Found := (mcmOCR.GetFirstGlyph(FGlyphRect) <> 0)
  else begin
       if (FGlyphRect.Right > longint(FSeparateAt))
       then FGlyphRect.Right := FSeparateAt;
       Found := (mcmOCR.GetNextGlyph(FGlyphRect) <> 0);
  end;

  if Found
  then begin
       // Get possible glyph separators.
       Count := mcmOCR.GetSeparatorGlyph(FGlyphRect, @Separators, @Ranking, 10);

       // Add glyph ranks & separaters to list.
       cbSeparators.Clear;
       cbSeparators.Items.AddObject('None', Pointer(FGlyphRect.Right+1));
       for i := 0 to (Count - 1)
       do cbSeparators.Items.AddObject(IntToStr(Ranking[i]) + ' Pos ' + IntToStr(Separators[i]),
                                       Pointer(Separators[i]));
       cbSeparators.ItemIndex := 0;
       if (Count = 0)
       then cbSeparators.Enabled := False
       else cbSeparators.Enabled := True;
       FSeparateAt := FGlyphRect.Right + 1;

       ShowGlyph;
       ShowResizedGlyph(icResized);

       i := mcmOCR.Match(Error, FGlyphRect);
       lMatchErrorVal.Caption := FloatToStrF(Error, ffFixed, 7, 4);
       if (i >= 0)
       then lGuessVal.Caption :=  mcmOCR.Glyph[i].Identifier;
       lLWidthVal.Caption  := IntToStr(FGlyphRect.Right - FGlyphRect.Left + 1) + ' x ' +
                              IntToStr(FGlyphRect.Bottom - FGlyphRect.Top + 1);
  end;
  btnNext.Enabled := Found;
  eGlyph.SetFocus;
end; // TFormOCRLearn.btnNextClick.


procedure TFormOCRLearn.mcmOCRDuplicateGlyph(Sender : TObject; Index : integer; var Replace : Boolean);
begin
  if (MessageDlg(resOCRDUPLICATE + chr($0D) + resOCRDUPLICATE2, mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then Replace := True
  else Replace := False;
end; // TFormOCRLearn.mcmOCRDuplicateGlyph.


//------------------------------------------------------------------------------
// Templates page
//------------------------------------------------------------------------------


procedure TFormOCRLearn.clbGlyphGroupClickCheck(Sender : TObject);
var i, j   : integer;
    AGlyph : PmcmGlyph;
begin
  if (Sender <> Nil)
  then btnSave.Enabled := True;
  
  i := clbGlyphGroup.ItemIndex;
  if (clbGlyphGroup.State[i] = cbChecked)
  then begin
       for j := 0 to (clbGlyphGroup.Items.Count - 1)
       do begin
          if (j <> i)
          then clbGlyphGroup.State[j] := cbUnChecked;
       end;
  end
  else clbGlyphGroup.State[i] := cbChecked;

  AGlyph := mcmOCR.Glyph[FGlyphIndex];
  if Assigned(AGlyph)
  then AGlyph.Group := i;
end; // TFormOCRLearn.clbGlyphGroupClickCheck.


procedure TFormOCRLearn.sbFirstClick(Sender : TObject);
var AGlyph : PmcmGlyph;
begin
  FGlyphIndex := 0;
  AGlyph := mcmOCR.Glyph[FGlyphIndex];
  if Assigned(AGlyph)
  then begin
       clbGlyphGroup.Checked[AGlyph.Group] := True;
       clbGlyphGroup.ItemIndex := AGlyph.Group;
       clbGlyphGroupClickCheck(Nil);
  end;
  ShowTemplate(icTemplate, AGlyph);
  icTemplate.Invalidate;
  lGlyphIndexVal.Caption := IntToStr(FGlyphIndex+1) + ' of ' + IntToStr(mcmOCR.NoGlyph);
end; // TFormOCRLearn.sbFirstClick.


procedure TFormOCRLearn.sbPreviousClick(Sender : TObject);
var AGlyph : PmcmGlyph;
begin
  dec(FGlyphIndex);
  if (FGlyphIndex < 0)
  then FGlyphIndex := 0;
  AGlyph := mcmOCR.Glyph[FGlyphIndex];
  if Assigned(AGlyph)
  then begin
       clbGlyphGroup.Checked[AGlyph.Group] := True;
       clbGlyphGroup.ItemIndex := AGlyph.Group;
       clbGlyphGroupClickCheck(Nil);
  end;
  ShowTemplate(icTemplate, AGlyph);
  icTemplate.Invalidate;
  lGlyphIndexVal.Caption := IntToStr(FGlyphIndex+1) + ' of ' + IntToStr(mcmOCR.NoGlyph);
end; // TFormOCRLearn.sbPreviousClick.


procedure TFormOCRLearn.sbNextClick(Sender : TObject);
var AGlyph : PmcmGlyph;
begin
  inc(FGlyphIndex);
  if (FGlyphIndex >= mcmOCR.NoGlyph)
  then FGlyphIndex := mcmOCR.NoGlyph - 1;
  AGlyph := mcmOCR.Glyph[FGlyphIndex];
  if Assigned(AGlyph)
  then begin
       clbGlyphGroup.Checked[AGlyph.Group] := True;
       clbGlyphGroup.ItemIndex := AGlyph.Group;
       clbGlyphGroupClickCheck(Nil);
  end;
  ShowTemplate(icTemplate, AGlyph);
  icTemplate.Invalidate;
  lGlyphIndexVal.Caption := IntToStr(FGlyphIndex+1) + ' of ' + IntToStr(mcmOCR.NoGlyph);
end; // TFormOCRLearn.sbNextClick.


procedure TFormOCRLearn.sbLastClick(Sender : TObject);
var AGlyph : PmcmGlyph;
begin
  FGlyphIndex := mcmOCR.NoGlyph - 1;
  AGlyph := mcmOCR.Glyph[FGlyphIndex];
  if Assigned(AGlyph)
  then begin
       clbGlyphGroup.Checked[AGlyph.Group] := True;
       clbGlyphGroup.ItemIndex := AGlyph.Group;
       clbGlyphGroupClickCheck(Nil);
  end;
  ShowTemplate(icTemplate, AGlyph);
  icTemplate.Invalidate;
  lGlyphIndexVal.Caption := IntToStr(FGlyphIndex+1) + ' of ' + IntToStr(mcmOCR.NoGlyph);
end; // TFormOCRLearn.sbLastClick.


procedure TFormOCRLearn.sbDeleteClick(Sender : TObject);
var AGlyph : PmcmGlyph;
begin
  btnSave.Enabled := True;
  mcmOCR.DeleteGlyph(FGlyphIndex);
  if (FGlyphIndex >= mcmOCR.NoGlyph)
  then FGlyphIndex := mcmOCR.NoGlyph - 1;
  AGlyph := mcmOCR.Glyph[FGlyphIndex];
  ShowTemplate(icTemplate, AGlyph);
  icTemplate.Invalidate;
  lGlyphIndexVal.Caption := IntToStr(FGlyphIndex+1) + ' of ' + IntToStr(mcmOCR.NoGlyph);
end; // TFormOCRLearn.sbDeleteClick.


//------------------------------------------------------------------------------
// Result page
//------------------------------------------------------------------------------

procedure TFormOCRLearn.ClearResults;
begin
  sgResults.RowCount := 2;
  sgResults.DefaultColWidth := 44;

  // Set header.
  sgResults.Cells[0,0] := 'Char';
  sgResults.Cells[1,0] := 'Error';
  sgResults.Cells[2,0] := 'x';
  sgResults.Cells[3,0] := 'y';
  sgResults.Cells[4,0] := 'dx';
  sgResults.Cells[5,0] := 'dy';

  // Clear first row.
  sgResults.Cells[0,1] := '';
  sgResults.Cells[1,1] := '';
  sgResults.Cells[2,1] := '';
  sgResults.Cells[3,1] := '';
  sgResults.Cells[4,1] := '';
  sgResults.Cells[5,1] := '';

  Update;
end; // TFormOCRLearn.ClearResults.


//------------------------------------------------------------------------------
// Auto learn windows font.
//------------------------------------------------------------------------------

function FontCallBack(lpnlf    : PEnumLogFont;
                      lpntm    : PNewTextMetric;
                      FontType : integer;
                      Data     : pointer) : integer; stdcall;
begin
  // FontType
  // 1 -> Raster
  // 2 -> PS
  // 4 -> TTF
  // else -> ?
  TCheckListBox(Data^).Items.AddObject(StrPas(lpnlf.elfLogFont.lfFaceName), Pointer(FontType));
  Result := 1;
end; // End FontCallBack.


procedure TFormOCRLearn.BuilderFontList;
var DC : HDC;
begin
  clbFonts.Clear;
  clbFonts.Visible := False;
  DC := GetDC(0);
  if Not(EnumFontFamilies(DC, Nil, @FontCallBack, integer(@clbFonts)))
  then begin
       // we had an error!
  end;
  ReleaseDC(0, DC);
  clbFonts.Visible := True;
end; // TFormOCRLearn.BuilderFontList.


procedure TFormOCRLearn.btnCheckAllClick(Sender : TObject);
var i : integer;
begin
  for i := 0 to (clbFonts.Items.Count - 1)
  do clbFonts.Checked[i] := True;
end; // TFormOCRLearn.btnCheckAllClick.


procedure TFormOCRLearn.btnUncheckAllClick(Sender : TObject);
var i : integer;
begin
  for i := 0 to (clbFonts.Items.Count - 1)
  do clbFonts.Checked[i] := False;
end; // TFormOCRLearn.btnUncheckAllClick.


procedure TFormOCRLearn.btnAutoLearnClick(Sender : TObject);
var i, j       : integer;
    Styles     : integer;
    FontStyles : TFontStyles;
    CharStr    : WideString;
    Id         : WideChar;
    PalEntry   : TPaletteEntry;
    SaveImage  : TmcmImage;
begin
  seGlyphWidth.Enabled  := False;
  seGlyphHeight.Enabled := False;

  mcmOCR.OnDuplicateGlyph := Nil;
  SaveImage := mcmOCR.Image;

  ImageCtrlFonts.Image.Width  := 3 * mcmOCR.GlyphWidth + 4;
  ImageCtrlFonts.Image.Height := 3 * mcmOCR.GlyphHeight + 4;
  ImageCtrlFonts.Image.ImageFormat := IF_GREY8;
  ImageCtrlFonts.Image.CreateGreyPalette;
  FillChar(PalEntry, SizeOf(TPaletteEntry), 0);
  PalEntry.peRed := 255;
  ImageCtrlFonts.Image.SetPaletteEntry(254, @PalEntry);

  ImageCtrlFonts.Image.FillAll(255);


  mcmOCR.ClearGlyphs;
  mcmOCR.Image := ImageCtrlFonts.Image;

  Styles := 0;
  if cbNormal.Checked
  then Styles := 1;
  if cbBold.Checked
  then Styles := Styles or 2;
  if cbItalic.Checked
  then Styles := Styles or 4;

  while (Styles > 0)
  do begin
     case Styles of
     1, 3,
     7    : begin
              Styles := Styles and $6;
              FontStyles := [];
            end;
     2, 6 : begin
              Styles := Styles and $5;
              FontStyles := [fsBold];
            end;
     4    : begin
              Styles := Styles and $3;
              FontStyles := [fsItalic];
            end;
     end;

     for i := 0 to (clbFonts.Items.Count - 1)
     do begin
        if clbFonts.Checked[i]
        then begin
             lLearningFontVal.Caption := clbFonts.Items.Strings[i];
             lLearningFontVal.Update;
             ImageCtrlFonts.Image.Canvas.Font.Name := clbFonts.Items.Strings[i];
             ImageCtrlFonts.Image.Canvas.Font.Style := FontStyles;
             ImageCtrlFonts.Image.Canvas.Font.Color := RGB(0,0,0);
             ImageCtrlFonts.Image.Canvas.Font.Size  := 3 * mcmOCR.GlyphHeight div 2;

             for j := 0 to 255
             do begin
                Id := WideChar(j);

                CharStr := Chr(j);
                if (CharStr <> '') and (CharStr <> ' ') and (CharStr <> '|')
                then begin
                     ImageCtrlFonts.Image.FillAll(255);
                     ImageCtrlFonts.Image.Canvas.TextOut(1, 1, CharStr);
                     ImageCtrlFonts.DrawImage;
                     ImageCtrlFonts.Update;

                     mcmOCR.LocateGlyphs;

                     if (mcmOCR.GetFirstGlyph(FGlyphRect) <> 0)
                     then begin
                          if (FGlyphRect.Left <> FGlyphRect.Right) and
                             (FGlyphRect.Top <> FGlyphRect.Bottom)
                          then begin
                               FGroup := OGG_ALL;
                               if IsCharAlpha(Char(Id))
                               then begin
                                    if IsCharLower(Char(Id))  // alternative IsCharUpper
                                    then FGroup := OGG_LOWERCASE
                                    else FGroup := OGG_UPPERCASE;
                               end
                               else begin
                                    if IsCharAlphaNumeric(Char(Id))
                                    then FGroup := OGG_DIGITS
                                    else FGroup := OGG_SPECIAL;
                               end;

                               mcmOCR.AddGlyph(Id, FGroup, FGlyphRect);

                               ImageCtrlFonts.Image.Canvas.Brush.Style := BSCLEAR;
                               ImageCtrlFonts.Image.Canvas.Pen.Color := RGB(255,0,0);
                               ImageCtrlFonts.Image.Canvas.Rectangle(FGlyphRect.Left, FGlyphRect.Top, FGlyphRect.Right+1, FGlyphRect.Bottom+1);
                               ImageCtrlFonts.DrawImage;
                               ImageCtrlFonts.Update;

                               sleep(0);
                               Update;
                          end;
                     end;
                end;
             end;
        end;
     end;

  end;
  if (mcmOCR.NoGlyph > 0)
  then begin
       btnSave.Enabled := True;
       btnRead.Enabled := True;
  end;

  lLearningFontVal.Caption := 'Completed scanning fonts';
  Update;
  mcmOCR.Image := SaveImage;
  mcmOCR.OnDuplicateGlyph := mcmOCRDuplicateGlyph;
end; // TFormOCRLearn.btnAutoLearnClick.


procedure TFormOCRLearn.gbFontsExit(Sender : TObject);
begin
  ImageCtrlFonts.Image.FillAll(255);
end; // TFormOCRLearn.gbFontsExit.


//------------------------------------------------------------------------------
// Read Result.
//------------------------------------------------------------------------------

procedure TFormOCRLearn.sgResultsSelectCell(Sender : TObject; Col, Row : Integer; var CanSelect : Boolean);
var TmpRect : TRect;
begin
  if Assigned(FOnShowGlyph) and (Row > 0)
  then begin
       if (sgResults.Rows[Row].Strings[2] <> '')
       then begin
            TmpRect.Left   := StrToInt(sgResults.Rows[Row].Strings[2]);
            TmpRect.Top    := StrToInt(sgResults.Rows[Row].Strings[3]);
            TmpRect.Right  := TmpRect.Left + StrToInt(sgResults.Rows[Row].Strings[4]);
            TmpRect.Bottom := TmpRect.Top + StrToInt(sgResults.Rows[Row].Strings[5]);
            FOnShowGlyph(Self, TmpRect);
       end;
  end;
end;


procedure TFormOCRLearn.cbFontStyleClick(Sender : TObject);
begin
  if Not(cbNormal.Checked or cbBold.Checked or cbItalic.Checked)
  then TCheckBox(Sender).Checked := True;
end;

initialization
  FormOCRLearn := Nil;

  {$IFDEF RANGEOFF}{$UNDEF RANGEOFF}{$R+}{$ENDIF}
end.
