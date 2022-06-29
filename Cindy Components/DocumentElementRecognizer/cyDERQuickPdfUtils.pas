unit cyDERQuickPdfUtils;

{ cyDERDBUtils
  Description: Document elements recognition with QuickPdf tool utilities

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

interface

uses Classes, SysUtils, cyDocER, cyDERUtils, cyDebenuPDFLibrary;

function InchesToPixels(const Inches: Extended; const Resolution: Integer): Integer;
function ConvertToFloat(QuickPdfFloat: String): Extended;
procedure ConvertQuickPdfWordsInfoToExpressions(const aPdfWordsInfo: ArrayQuickPdfWordsInfo; PageNumber: Integer; aDocER: TcyDocER; var CorrectedCoordinatesCount: Integer);

implementation


function InchesToPixels(const Inches: Extended; const Resolution: Integer): Integer;
begin
  Result := Round(Inches * Resolution);
end;

function ConvertToFloat(QuickPdfFloat: String): Extended;
begin
  if cyDERUtils.LocalFormatSettings.DecimalSeparator <> '.' then
    QuickPdfFloat := StringReplace(QuickPdfFloat, '.', cyDERUtils.LocalFormatSettings.DecimalSeparator, []);

  if not TryStrToFloat(QuickPdfFloat, Result) then
    Result := 0;
end;

procedure ConvertQuickPdfWordsInfoToExpressions(const aPdfWordsInfo: ArrayQuickPdfWordsInfo; PageNumber: Integer; aDocER: TcyDocER; var CorrectedCoordinatesCount: Integer);
var
  i, LengthPdfWordsInfo: Integer;
  aText: String;
  PageResolution, PagePixelsHeight, pxSav, pxLeft, pxTop, pxRight, pxBottom: Integer;
  v: Extended;
begin
  CorrectedCoordinatesCount := 0;
  LengthPdfWordsInfo := Length(aPdfWordsInfo);
  PageResolution := aDocER.GetPageResolution(PageNumber);
  PagePixelsHeight := aDocER.GetPagePixelsHeight(PageNumber);


  for i := 0 to LengthPdfWordsInfo -1 do
  begin
    aText := aPdfWordsInfo[i].Text;

    aText := Copy(aText, 2, length(aText)-2);

    v := ConvertToFloat(aPdfWordsInfo[i].X4);
    pxLeft := InchesToPixels(v / 72, PageResolution);

    v := ConvertToFloat(aPdfWordsInfo[i].Y4);
    pxTop := PagePixelsHeight - InchesToPixels(v / 72, PageResolution);  // We need to revert

    v := ConvertToFloat(aPdfWordsInfo[i].X2);
    pxRight := InchesToPixels(v / 72, PageResolution);

    v := ConvertToFloat(aPdfWordsInfo[i].Y2);
    pxBottom := PagePixelsHeight - InchesToPixels(v / 72, PageResolution);  // We need to revert

    // 2017-05-08 Never encountered :
    if pxLeft > pxRight then
    begin
      pxSav := pxLeft;
      pxLeft := pxRight;
      pxRight := pxSav;

      Inc(CorrectedCoordinatesCount);
    end;

    // 2017-05-08 Already encountered :
    if pxTop > pxBottom then
    begin
      pxSav := pxTop;
      pxTop := pxBottom;
      pxBottom := pxSav;

      Inc(CorrectedCoordinatesCount);
    end;

    if aText <> '' then
      aDocER.AddExpression(aText, PageNumber, 1, Rect(pxLeft, pxTop, pxRight, pxBottom));
  end;
end;

end.
