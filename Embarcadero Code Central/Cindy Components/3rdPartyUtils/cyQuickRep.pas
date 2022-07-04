{   Unit cyQuickrep

    Description:
    Unit with functions to use with QuickReport components.

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

unit cyQuickRep;

interface

uses
  Classes, Forms, Graphics, ExtCtrls, SysUtils, DBGrids, QuickRpt, QRCtrls, QRPrntr, qrExport, QRPDFFilt, QRXMLSFilt, QRWebFilt,
    vcl.cyImage;

type
  TExportReportType = (erPdf, erCsv, erXls, erHtml, erXml, erRtf, erTxt);

  // ****************************** NOTE ******************************//
  // CurrentY/10 represent the current vertical position (on printing) //
  //        in unit measure espcified on QuickRp.Unit property         //
  // ***************************************************************** //

  procedure Qr_Export(aQuickRep: TQuickRep; aFileName: String; ExportReportType: TExportReportType);

  procedure Qr_ExportPageToBitmap(aQuickRep: TQuickRep; Page: Word; ToPicture: TPicture; _PixelFormat: TPixelFormat);

  procedure Qr_SavePageToBmpFile(aQuickRep: TQuickRep; Page: Word; _PixelFormat: TPixelformat;
                                   aFile: TFilename; BeforeSavePicture: TNotifyEvent);

  procedure Qr_SavePageToJpegFile(aQuickRep: TQuickRep; Page: Word; _PixelFormat: TPixelformat;
                                    aFile: TFilename; QualityPercent: Word; BeforeSavePicture: TNotifyEvent);

  // Convert milimeters to quickReport pixel value :
  function MmToQrPixels(aQuickRep: TQuickRep; mmValue: Extended): Integer;

  // Convert quickReport pixel value to milimeters :
  function QrPixelsToMm(aQuickRep: TQuickRep; pxValue: Integer): Extended;

  // Convert pixel value to quickreport pixel value (seeing zoom) and finally to mm :
  function PixelsToMm(aQuickRep: TQuickRep; pxValue: Integer): Extended;

implementation

uses DateUtils;

// Need to call aQuickRep.Prepare before using this function ...
procedure Qr_Export(aQuickRep: TQuickRep; aFileName: String; ExportReportType: TExportReportType);
var
  aPdf: TQRPDFDocumentFilter;
  aXls: TQRXLSFilter;
  aRtf: TQRRTFExportFilter;
  aXml: TQRXDocumentFilter;
  aTxt: TQRAsciiExportFilter;
  aHtml: TQRGHTMLDocumentFilter;
  aCsv: TQRCommaSeparatedFilter;
begin
  case ExportReportType of
    erPdf:
    begin
      aPDF := TQRPDFDocumentFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aPDF);
      aPDF.Free;
    end;

    erXls:
    begin
      aXls := TQRXLSFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aXls);
      aXls.Free;
    end;

    erHtml:
    begin
      aHtml := TQRGHTMLDocumentFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aHtml);
      aHtml.Free;
    end;

    erXml:
    begin
      aXml := TQRXDocumentFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aXml);
      aXml.Free;
    end;

    erRtf:
    begin
      aRtf := TQRRTFExportFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aRtf);
      aRtf.Free;
    end;

    erCsv:
    begin
      aCsv := TQRCommaSeparatedFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aCsv);
      aCsv.Free;
    end;

    erTxt:
    begin
      aTxt := TQRAsciiExportFilter.Create(aFileName);
      aQuickRep.ExportToFilter(aTxt);
      aTxt.Free;
    end;
  end;
end;

procedure Qr_ExportPageToBitmap(aQuickRep: TQuickRep; Page: Word; ToPicture: TPicture; _PixelFormat: TPixelFormat);
var
  mf: TMetaFile;
  aBmp: TBitmap;
begin
  mf := aQuickRep.QRPrinter.GetPage(Page);
  aBmp := BitmapCreate(mf.Width, mf.Height, ClWhite, _PixelFormat);
  ToPicture.Assign(aBmp);
  aBmp.Free;
  ToPicture.Bitmap.Canvas.Draw( 0, 0, mf);
end;

procedure Qr_SavePageToBmpFile(aQuickRep: TQuickRep; Page: Word; _PixelFormat: TPixelformat;
                                 aFile: TFilename; BeforeSavePicture: TNotifyEvent);
var pc: TPicture;
begin
  if (Page > 0) and (Page <= aQuickRep.QRPrinter.PageCount)
  then begin
    pc := TPicture.Create;
    Qr_ExportPageToBitmap(aQuickRep, Page, pc, _PixelFormat);

    if Addr(BeforeSavePicture) <> nil
    then BeforeSavePicture(pc);

    pc.Bitmap.SaveToFile(aFile);
    pc.Free;
  end
  else
    raise Exception.Create('Page not exists !');
end;

procedure Qr_SavePageToJpegFile(aQuickRep: TQuickRep; Page: Word; _PixelFormat: TPixelformat;
                                  aFile: TFilename; QualityPercent: Word; BeforeSavePicture: TNotifyEvent);
var pc: TPicture;
begin
  if (Page > 0) and (Page <= aQuickRep.QRPrinter.PageCount)
  then begin
    pc := TPicture.Create;
    Qr_ExportPageToBitmap(aQuickRep, Page, pc, _PixelFormat);

    if Addr(BeforeSavePicture) <> nil
    then BeforeSavePicture(pc);

    BitmapSaveToJpegFile(pc.Bitmap, aFile, QualityPercent);
    pc.Free;
  end
  else
    raise Exception.Create('Page not exists !');
end;

function MmToQrPixels(aQuickRep: TQuickRep; mmValue: Extended): Integer;
begin
  Result := Round(aQuickRep.Height / aQuickRep.Page.Length * mmValue);
end;

function QrPixelsToMm(aQuickRep: TQuickRep; pxValue: Integer): Extended;
begin
  Result := aQuickRep.Page.Length / aQuickRep.Height * pxValue;
end;

// Convert pixel value to quickreport pixel (seeing zoom) and finally to mm :
function PixelsToMm(aQuickRep: TQuickRep; pxValue: Integer): Extended;
begin
  Result := (aQuickRep.Page.Length / aQuickRep.Height) * (pxValue * aQuickRep.Zoom / 100);
end;

end.
