(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
unit DemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  XpvXSLPr, XpBase, XpDom, StdCtrls, FileCtrl, XpvFlBas, XpvFlPrt, ExtCtrls,
  XpvXSLT;

type
  TForm1 = class(TForm)
    gbXML: TGroupBox;
    gbXSL: TGroupBox;
    Panel1: TPanel;
    PrintFilter: TXpFilterPrint;
    dirXML: TDirectoryListBox;
    fileXML: TFileListBox;
    btnShow: TButton;
    dirXSL: TDirectoryListBox;
    fileXSL: TFileListBox;
    XpObjModel: TXpObjModel;
    XSLProc: TXpXSLProcessor;
    drvXML: TDriveComboBox;
    drvXSL: TDriveComboBox;
    ScrollBar: TScrollBar;
    lblPage: TLabel;
    btnPrint: TButton;
    procedure btnShowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBarKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnPrintClick(Sender: TObject);
    procedure drvXMLChange(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateDisplay;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.DFM}

procedure TForm1.btnShowClick(Sender: TObject);
var
  OldCursor : TCursor;
begin
  btnShow.Enabled := False;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    if (XpObjModel.LoadDataSource(fileXML.FileName)) then begin
        XSLProc.StyleURL := fileXSL.FileName;
        if (not XSLProc.ApplyStyle) then
          ShowMessage('Unable to transform XML document')
      end else
        ShowMessage('Unable to parse XML document: ' +
                    XpObjModel.GetErrorMsg(0));
  finally
    Screen.Cursor := OldCursor;
    btnShow.Enabled := True;
  end;
  lblPage.Caption := format('Page %d of %d',
                            [PrintFilter.PageCurrent, PrintFilter.PageCount]);
  UpdateDisplay;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  TrackHeight : Integer; { the size of the scroll bar track }
  MinHeight   : Integer; { the default size of the thumb tab }
begin
  MinHeight := GetSystemMetrics(SM_CYVTHUMB); { save default size }
  with ScrollBar do begin
    TrackHeight := ClientHeight - 2 * GetSystemMetrics(SM_CYVSCROLL);
    {$IFNDEF VER100}
    PageSize := TrackHeight div (Max - Min + 1);
    if (PageSize < MinHeight) then
      PageSize := MinHeight;
    {$ENDIF}
  end;
end;

procedure TForm1.ScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if ((ScrollPos = ScrollBar.Max) and
      (PrintFilter.PageCurrent <> PrintFilter.PageCount)) then begin
    ScrollPos := 0;
    PrintFilter.VSlidePosition := 0;
    PrintFilter.PageCurrent := Succ(PrintFilter.PageCurrent);
  end else if ((ScrollPos = ScrollBar.Min) and
               (PrintFilter.PageCurrent <> 1) and
               (ScrollCode <> scPageUp)) then begin
    ScrollPos := 0;
    PrintFilter.VSlidePosition := 0;
    PrintFilter.PageCurrent := Pred(PrintFilter.PageCurrent);
  end;
  PrintFilter.VSlidePosition := ScrollPos;
  lblPage.Caption := format('Page %d of %d',
                            [PrintFilter.PageCurrent, PrintFilter.PageCount]);
end;

procedure TForm1.ScrollBarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TempSize : Integer;
begin
  case Key of
    VK_NEXT :
      begin
        TempSize := ScrollBar.Position + ScrollBar.LargeChange;
        if (ScrollBar.Max < TempSize) then
          TempSize := ScrollBar.Max;
        ScrollBarScroll(Sender, scPageDown, TempSize);
      end;
    VK_PRIOR :
      begin
        TempSize := 1;
        ScrollBarScroll(Sender, scPageUp, TempSize);
      end;
  end;
end;

procedure TForm1.UpdateDisplay;
begin
  btnShow.Enabled := ((fileXML.FileName <> '') and
                      (fileXSL.FileName <> ''));
  btnPrint.Enabled := PrintFilter.PageCount <> 0;
end;

procedure TForm1.btnPrintClick(Sender: TObject);
begin
  PrintFilter.Print('');
end;

procedure TForm1.drvXMLChange(Sender: TObject);
begin
  UpdateDisplay;
end;

end.
