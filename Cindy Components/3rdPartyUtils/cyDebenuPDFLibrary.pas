{   Unit cyDebenuPDFLibrary

    Description:
    Unit with functions to use with DebenuPDFLibrary components.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyDebenuPDFLibrary;

interface

uses Windows, Classes, DebenuPDFLibrary, SysUtils, cyStrUtils, VCl.cySysUtils;

type
  TPageRotateOrientation = (prToTheRight, prToTheLeft, pr180Degree);

  // Return line exemple :
  // "TimesNewRoman,BoldItalic",#000000,14.04,56.2795,802.6424,113.4556,802.6424,113.4556,815.3346,56.2795,815.3346,"Cinderela"
  rQuickPdfWordInfo = record
    FontName: String;   // "TimesNewRoman,BoldItalic"
    Color: String;      // #000000
    TextSize: String;   //  14.04
    X1: String;         //  56.2795  =>  56.2795 / 72 =  0,78165 inches =>  0,78165 x 2.5 =  1.95 cm
    Y1: String;         // 802.6424  => 802.6424 / 72 = 11,14781 inches => 11,14781 x 2.5 = 27.87 cm
    X2: String;         // 113.4556
    Y2: String;         // 802.6424
    X3: String;         // 113.4556
    Y3: String;         // 815.3346
    X4: String;         //  56.2795
    Y4: String;         // 815.3346
    Text: String;       // "Cindy"
  end;

  ArrayQuickPdfWordsInfo = Array of rQuickPdfWordInfo;


var DebenuLicenceKey: String;

const
  cPdfMeasurementDefaultUserSpace = 0; // Default, Default user space, same as pixel
  cPdfMeasurementMillimeters = 1;
  cPdfMeasurementInch = 2;

  cPdfOriginBottomLeft = 0;            // Default
  cPdfOriginTopLeft = 1;
  cPdfOriginTopRight = 2;
  cPdfOriginBottomRight = 3;

function CalcDebenuPageRotationValue(const CurrentPageRotationValue: Integer; const PageRotateOrientation: TPageRotateOrientation): Integer;

function GetPdfFilePages(const PdfFile: string): Integer;
function PdfFileMovePage(const PdfFile: string; const FromPosition, ToPosition: Integer): Boolean;
function PdfFileMovePages(const PdfFile: string; Pages: Array of Integer; const ToPageNumber: Integer): Boolean;
function PdfFileSavePagesToFile(const SourcePdfFile, DestinationPdfFile: string; Pages: Array of Integer): Boolean;
function PdfFileCopyPagesToFile(const SourcePdfFile, DestinationPdfFile: string; const SourcePages: Array of Integer; const ToPageNumberPosition: Integer = 0): Boolean;
function PdfFileDeletePages(const PdfFile: string; const FromPage, PageCount: Integer): Boolean; overload;
function PdfFileDeletePages(const PdfFile: string; Pages: Array of Integer): Boolean; overload;
function PdfFileRotatePages(const PdfFile: string; const RelativeAngleDegree: TPageRotateOrientation; const RotatePage: Integer = 0): Boolean;

procedure ExtractedPageTextToWords(PageText: String; var aPdfWordsInfo: ArrayQuickPdfWordsInfo; const ExtractFilePageText_Options: Integer = 4);

// Need Licence info (set var DebenuLicenceKey before):
function QuickPdf_ConvertPdf2Tif(const SrcPdfFile, DestTifFile: String; const Resolution: Integer): Boolean;



implementation

function GetPdfFilePages(const PdfFile: string): Integer;
var
  PDFLibrary: TDebenuPDFLibrary;
  UnlockResult: Integer;
begin
  Result := 0;

  if not FileExists(PdfFile) then
    Exit;

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      PDFLibrary.LoadFromFile(PdfFile, '');
      Result := PDFLibrary.PageCount;
    finally
      PDFLibrary.Free;
    end;
end;

function PdfFileMovePage(const PdfFile: string; const FromPosition, ToPosition: Integer): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  UnlockResult: Integer;
begin
  Result := false;

  if not FileExists(PdfFile) then
    Exit;

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      PDFLibrary.LoadFromFile(PdfFile, '');
      PDFLibrary.SelectPage(FromPosition);
      if PDFLibrary.MovePage(ToPosition) = 1 then
        Result := PDFLibrary.SaveToFile(PdfFile) = 1;
    finally
      PDFLibrary.Free;
    end;
end;

function PdfFileMovePages(const PdfFile: string; Pages: Array of Integer; const ToPageNumber: Integer): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  UnlockResult, i: Integer;
  MoveOk: Boolean;
begin
  Result := false;

  if not FileExists(PdfFile) then
    Exit;

  VCl.cySysUtils.SortArrayOfIntegers(Pages);

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      MoveOk := true;
      PDFLibrary.LoadFromFile(PdfFile, '');

      for i := Length(Pages)-1 downto 0 do
      begin
        PDFLibrary.SelectPage(Pages[i]);

        if PDFLibrary.MovePage(ToPageNumber) = 0 then
        begin
          MoveOk := false;
          Break;
        end;
      end;

        if MoveOk then
          Result := PDFLibrary.SaveToFile(PdfFile) = 1;
    finally
      PDFLibrary.Free;
    end;
end;

function PdfFileSavePagesToFile(const SourcePdfFile, DestinationPdfFile: string; Pages: Array of Integer): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  UnlockResult, i: Integer;
  StrRange: String;
begin
  Result := false;

  if not FileExists(SourcePdfFile) then
    Exit;

  if Length(Pages) = 0 then
  begin
    Result := Windows.CopyFile(PChar(SourcePdfFile), PChar(DestinationPdfFile), false);
    Exit;
  end;

  StrRange := '';

  for i := 0 to Length(Pages)-1 do
  begin
    if i <> 0 then
      StrRange := StrRange + ',';

    StrRange := StrRange + IntToStr(Pages[i]);
  end;

  if StrRange = '' then
    Exit;

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      PDFLibrary.LoadFromFile(SourcePdfFile, '');
      Result := PDFLibrary.ExtractFilePages(SourcePdfFile, '', DestinationPdfFile, StrRange) = 1;
    finally
      PDFLibrary.Free;
    end;
end;

function PdfFileCopyPagesToFile(const SourcePdfFile, DestinationPdfFile: string; const SourcePages: Array of Integer; const ToPageNumberPosition: Integer = 0): Boolean;
var
  Pages: Array of Integer;
  SourcePDFLibrary, PDFLibrary: TDebenuPDFLibrary;
  SourceID, DestID, OriginalDestPageCount, UnlockResult, p: Integer;
  StrRange: String;
  Save: Boolean;
begin
  Result := false;

  if not FileExists(SourcePdfFile) then
    Exit;

  if not FileExists(DestinationPdfFile) then
  begin
    Result := PdfFileSavePagesToFile(SourcePdfFile, DestinationPdfFile, Pages);
    Exit;
  end;

  // Pass Pages to local variable :
  SetLength(Pages, Length(SourcePages));
  for p := 0 to Length(SourcePages)-1 do
    Pages[p] := SourcePages[p];

  VCl.cySysUtils.SortArrayOfIntegers(Pages);

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      Save := false;

      // Open Source file :
      PDFLibrary.LoadFromFile(SourcePdfFile, '');
      SourceID := PDFLibrary.SelectedDocument;

      // All pages if nothing specified :
      if Length(Pages) = 0 then
      begin
        SetLength(Pages, PDFLibrary.PageCount);

        for p := 0 to PDFLibrary.PageCount-1 do
          Pages[p] := p + 1;
      end;

      StrRange := '';
      for p := 0 to Length(Pages)-1 do
      begin
        if p <> 0 then
          StrRange := StrRange + ',';

        StrRange := StrRange + intToStr(Pages[p]);
      end;

      // Open destination file :
      PDFLibrary.LoadFromFile(DestinationPdfFile, '');
      DestID := PDFLibrary.SelectedDocument;
      OriginalDestPageCount := PDFLibrary.PageCount;

      PDFLibrary.CopyPageRanges(SourceID, StrRange);

      // Page positions :
      if (ToPageNumberPosition > 0) and (ToPageNumberPosition < OriginalDestPageCount) then
        for p := 1 to Length(Pages) do
        begin
          PDFLibrary.SelectPage(PDFLibrary.PageCount);
          PDFLibrary.MovePage(ToPageNumberPosition);
        end;

      Save := true;
    finally
      if Save then
        Result := PDFLibrary.SaveToFile(DestinationPdfFile) = 1;

      PDFLibrary.Free;
    end;
end;

function PdfFileDeletePages(const PdfFile: string; const FromPage, PageCount: Integer): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  UnlockResult: Integer;
begin
  Result := false;

  if not FileExists(PdfFile) then
    Exit;

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      PDFLibrary.LoadFromFile(PdfFile, '');

      if PDFLibrary.DeletePages(FromPage, PageCount) = 0     // The function return page number remaining after deletion, must remain at least one page !
      then Result := DeleteFile(PdfFile)
      else Result := PDFLibrary.SaveToFile(PdfFile) = 1;
    finally
      PDFLibrary.Free;
    end;
end;

function PdfFileDeletePages(const PdfFile: string; Pages: Array of Integer): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  UnlockResult, i: Integer;
  Save: Boolean;
begin
  Result := false;

  if not FileExists(PdfFile) then
    Exit;

  VCl.cySysUtils.SortArrayOfIntegers(Pages);

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      Save := true;
      PDFLibrary.LoadFromFile(PdfFile, '');

      for i := Length(Pages)-1 downto 0 do
        if PDFLibrary.DeletePages(Pages[i], 1) = 0 then  // The function return page number remaining after deletion, must remain at least one page !
        begin
          Save := false;
          Result := DeleteFile(PdfFile);
          Break;
        end;

        if Save then
          Result := PDFLibrary.SaveToFile(PdfFile) = 1;
    finally
      PDFLibrary.Free;
    end;
end;

function CalcDebenuPageRotationValue(const CurrentPageRotationValue: Integer; const PageRotateOrientation: TPageRotateOrientation): Integer;
begin
  Result := CurrentPageRotationValue;

  case PageRotateOrientation of
    prToTheRight: Result := Result + 90;  // if Result = 270 then Result := 0 else Result := Result + 90;
    prToTheLeft:  Result := Result - 90;
    pr180Degree:  Result := Result + 180;
  end;

  while Result >= 360 do
    Result := Result - 360;

  while Result < 0 do
    Result := Result + 360;
end;

function PdfFileRotatePages(const PdfFile: string; const RelativeAngleDegree: TPageRotateOrientation; const RotatePage: Integer = 0): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  PageRotation: Integer;
  UnlockResult, p: Integer;
  AllPagesRotated: Boolean;
begin
  Result := false;

  if not FileExists(PdfFile) then
    Exit;

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  if UnlockResult <> 0 then
    try
      PDFLibrary.LoadFromFile(PdfFile, '');

      if RotatePage = 0 then
      begin
        AllPagesRotated := true;
        for p := 1 to PDFLibrary.PageCount do
        begin
          PDFLibrary.SelectPage(p);
          PageRotation := CalcDebenuPageRotationValue(PDFLibrary.PageRotation, RelativeAngleDegree);

          if PDFLibrary.RotatePage(PageRotation) <> 1 then
          begin
            AllPagesRotated := false;
            Break;
          end;
        end;

        if AllPagesRotated then
          Result := true;
      end
      else begin
        PDFLibrary.SelectPage(RotatePage);
        PageRotation := CalcDebenuPageRotationValue(PDFLibrary.PageRotation, RelativeAngleDegree);
        Result := PDFLibrary.RotatePage(PageRotation) = 1;
      end;

      if Result then
        Result := PDFLibrary.SaveToFile(PdfFile) = 1;
    finally
      PDFLibrary.Free;
    end;
end;

procedure ExtractedPageTextToWords(PageText: String; var aPdfWordsInfo: ArrayQuickPdfWordsInfo; const ExtractFilePageText_Options: Integer = 4);

(* Returned by QuickPdf :
   Font Name, Text Color, Text Size, X1, Y1, X2, Y2, X3, Y3, X4, Y4, Text

   The co-ordinates are the four points bounding the text, measured in points
   (1/72 inch, 1 inch = 2,54 cm) with the bottom-left corner of the page as the origin. Co-ordinate
   order is anti-clockwise with the bottom left corner first.

   Exemple :
  "TimesNewRoman,BoldItalic",#000000,14.04,56.2795,802.6424,113.4556,802.6424,113.4556,815.3346,56.2795,815.3346,"Cindy"      *)

var
  WordList: TStrings;
  i: Integer;

      function ExtractSubString(WordIndex: Integer; Separator: String): string;
      var
        PosSepar: Integer;
        StrLine: String;
      begin
        Result := '';

        StrLine := WordList[WordIndex];
        PosSepar := pos(Separator, StrLine);

        if posSepar <> 0 then
        begin
          Result := Copy(StrLine, 1, PosSepar-1);
          Delete(StrLine, 1, PosSepar);
          WordList[WordIndex] := StrLine;
        end
        else begin
          Result := StrLine;
          WordList[WordIndex] := '';
        end;
      end;

      function CorrectPdfString(aStr: string): string;
      begin
        Result := aStr;
        String_SubstFast('…', 'è', Result);
        String_SubstFast('\"', '"', Result);   // QuickPdf exporte les " en \" ...
        String_SubstFast('´e', 'é', Result);

        // 2013/12/09 Convertir ´ en [vide] (ce caractère semble renvoyé par QuickPdf au lieu d' un enter) :
        if Result = '"´"' then
           Result := '';
      end;

begin
  if ExtractFilePageText_Options <> 4 then
  begin
    raise Exception.Create('Only ExtractFilePageText_Options = 4 supported for now !');
    Exit;
  end;

  // Remove some chars :
  for i := length(PageText) downto 1 do
    if PageText[i] = #0 then
      Delete(PageText, i, 1);

  WordList := TStringList.Create;
  WordList.Text := PageText;

  SetLength(aPdfWordsInfo, WordList.Count);

  for i := 0 to WordList.Count-1 do
  begin
      aPdfWordsInfo[i].FontName  := ExtractSubString(i, ',#');
      aPdfWordsInfo[i].Color     := ExtractSubString(i, ',');
      aPdfWordsInfo[i].TextSize  := ExtractSubString(i, ',');
      aPdfWordsInfo[i].X1        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].Y1        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].X2        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].Y2        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].X3        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].Y3        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].X4        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].Y4        := ExtractSubString(i, ',');
      aPdfWordsInfo[i].Text      := CorrectPdfString(WordList[i]);   // Note: le reste peut avoir une virgule!;
  end;

  WordList.Free;
end;



function QuickPdf_ConvertPdf2Tif(const SrcPdfFile, DestTifFile: String; const Resolution: Integer): Boolean;
var
  PDFLibrary: TDebenuPDFLibrary;
  Pages: String;
  UnlockResult, Option, Dpis: Integer;
begin
  Result := false;

  PDFLibrary := TDebenuPDFLibrary.Create;
  UnlockResult := PDFLibrary.UnlockKey(DebenuLicenceKey);

  Dpis := Resolution;
  Pages := '1-99';
  Option := 0;    // 0 = 24-bit RGB TIFF, 1 = 1-bit G4 TIFF -> Not working ...

  try
    PDFLibrary.LoadFromFile(SrcPdfFile, '');

    (* Options
    0 = BMP output
    1 = JPEG output
    2 = WMF output
    3 = EMF output
    4 = EPS output
    5 = PNG output
    6 = GIF output
    7 = TIFF (LZW) output               !!! Not working in v13.x !!!
    8 = EMF+ output
    9 = HTML5 output
    10 = TIFF (G4) output     *)

    if PDFLibrary.RenderAsMultipageTIFFToFile(Dpis, Pages, Option, 0, DestTifFile) = 1 then
      Result := true;
  finally
    PDFLibrary.Free;
  end;
end;

end.
