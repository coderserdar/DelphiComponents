unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
{$IFDEF VER140} Variants, {$ENDIF}
{$IFDEF VER150} Variants, {$ENDIF}
{$IFDEF CIL}
  Types, System.ComponentModel, Variants, System.Runtime.InteropServices,
{$ELSE}
{$ENDIF}
  StdCtrls, DBCtrls, ExtCtrls, Grids, DBGridEh, ComCtrls, Db, DBTables,
  Buttons, DBGrids, PrViewEh, PrnDbgeh, ToolWin, Menus, DBGridEhImpExp,
  Mask, DBLookupEh, DBCtrlsEh, ImgList, EhLibBDE, PropStorageEh;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGridEh1: TDBGridEh;
    Panel1: TPanel;
    TabSheet2: TTabSheet;
    DBGridEh2: TDBGridEh;
    TabSheet3: TTabSheet;
    DBGridEh3: TDBGridEh;
    DBGridEh4: TDBGridEh;
    DBNavigator2: TDBNavigator;
    DBNavigator3: TDBNavigator;
    TabSheet4: TTabSheet;
    DBGridEh5: TDBGridEh;
    TabSheet5: TTabSheet;
    Panel3: TPanel;
    cbClearSelection: TCheckBox;
    cbShowIndicator: TCheckBox;
    cbTitle: TCheckBox;
    cbHighlightFocus: TCheckBox;
    cbMultiselect: TCheckBox;
    PreviewSetupPanel: TPanel;
    bPrint: TButton;
    bPrinterSetup: TButton;
    bPrevPage: TButton;
    bNextPage: TButton;
    bStop: TButton;
    bClosePreview: TButton;
    PreviewBox1: TPreviewBox;
    bInpPreview: TButton;
    bPreview: TButton;
    bOpenClose: TButton;
    lPageinfo: TLabel;
    cCustomPreview: TButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    dbgList1: TDBGridEh;
    dbgList: TDBGridEh;
    cbDragNDrop: TCheckBox;
    cbDichromatic: TCheckBox;
    bbCopy: TBitBtn;
    cbInterAppDragNDrop: TCheckBox;
    TabSheet6: TTabSheet;
    DBEditEh1: TDBEditEh;
    DBDateTimeEditEh1: TDBDateTimeEditEh;
    DBNumberEditEh1: TDBNumberEditEh;
    DBComboBoxEh1: TDBComboBoxEh;
    DBLookupComboboxEh3: TDBLookupComboboxEh;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    DBGridEh6: TDBGridEh;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Bevel1: TBevel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    DBLookupComboboxEh4: TDBLookupComboboxEh;
    Label20: TLabel;
    DBDateTimeEditEh2: TDBDateTimeEditEh;
    Label21: TLabel;
    DBNumberEditEh2: TDBNumberEditEh;
    Label22: TLabel;
    DBEditEh2: TDBEditEh;
    Label23: TLabel;
    DBComboBoxEh2: TDBComboBoxEh;
    DBNavigator1: TDBNavigator;
    PrintDBGridEh1: TPrintDBGridEh;
    PopupMenu1: TPopupMenu;
    ppmCut: TMenuItem;
    ppmCopy: TMenuItem;
    ppmPaste: TMenuItem;
    ppmDelete: TMenuItem;
    ppmSelectAll: TMenuItem;
    N1: TMenuItem;
    ppmPreview: TMenuItem;
    ppmSaveSelection: TMenuItem;
    pmNoVisibleCols: TPopupMenu;
    SaveDialog1: TSaveDialog;
    DBCheckBoxEh1: TDBCheckBoxEh;
    Label24: TLabel;
    DBCheckBoxEh2: TDBCheckBoxEh;
    PropStorageEh1: TPropStorageEh;
    procedure bOpenCloseClick(Sender: TObject);
    procedure Query1UpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure DBGridEh2DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumnEh; State: TGridDrawState);
    procedure DBGridEh2DrawFooterCell(Sender: TObject; DataCol,
      Row: Integer; Column: TColumnEh; Rect: TRect; State: TGridDrawState);
    procedure DBGridEh2GetCellParams(Sender: TObject; Column: TColumnEh;
      AFont: TFont; var Background: TColor; State: TGridDrawState);
    procedure DBGridEh2GetFooterParams(Sender: TObject; DataCol,
      Row: Integer; Column: TColumnEh; AFont: TFont;
      var Background: TColor; var Alignment: TAlignment;
      State: TGridDrawState; var Text: String);
    procedure DBGridEh1TitleBtnClick(Sender: TObject; ACol: Integer;
      Column: TColumnEh);
    procedure cbClearSelectionClick(Sender: TObject);
    procedure cbShowIndicatorClick(Sender: TObject);
    procedure cbTitleClick(Sender: TObject);
    procedure cbHighlightFocusClick(Sender: TObject);
    procedure cbMultiselectClick(Sender: TObject);
    procedure DBGridEh1SortMarkingChanged(Sender: TObject);
    procedure bPrintClick(Sender: TObject);
    procedure bPrinterSetupClick(Sender: TObject);
    procedure bPrevPageClick(Sender: TObject);
    procedure bNextPageClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure bClosePreviewClick(Sender: TObject);
    procedure bPreviewClick(Sender: TObject);
    procedure bInpPreviewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PreviewBox1PrinterPreviewChanged(Sender: TObject);
    procedure cCustomPreviewClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure dbgListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dbgListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure dbgListStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure dbgList1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dbgList1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure cbDragNDropClick(Sender: TObject);
    procedure cbDichromaticClick(Sender: TObject);
    procedure dbgListGetCellParams(Sender: TObject; Column: TColumnEh;
      AFont: TFont; var Background: TColor; State: TGridDrawState);
    procedure bbCopyClick(Sender: TObject);
    procedure DBGridEh1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbInterAppDragNDropClick(Sender: TObject);
    procedure DBGridEh2EditButtonClick(Sender: TObject);
    procedure ppmCutClick(Sender: TObject);
    procedure ppmCopyClick(Sender: TObject);
    procedure ppmPasteClick(Sender: TObject);
    procedure ppmDeleteClick(Sender: TObject);
    procedure ppmSelectAllClick(Sender: TObject);
    procedure ppmPreviewClick(Sender: TObject);
    procedure ppmSaveSelectionClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DBGridEh1ColWidthsChanged(Sender: TObject);
    procedure DBLookupComboboxEh1KeyValueChanged(Sender: TObject);
    procedure DBLookupComboboxEh2KeyValueChanged(Sender: TObject);
    procedure DBGridEh1Columns1DropDownBoxGetCellParams(Sender: TObject;
      Column: TColumnEh; AFont: TFont; var Background: TColor;
      State: TGridDrawState);
    procedure DBComboBoxEh1GetItemImageIndex(Sender: TObject;
      ItemIndex: Integer; var ImageIndex: Integer);
  private
    { Private declarations }
  public
    FilterControlList: TStringList;
    procedure InplacePreviewSetupDialog(Sender: TObject);
    procedure ColumnMenuItem(Sender: TObject);
    function GridSelectionAsText(AGrid:TDBGridEh):String;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure UpdateQuery1Filter;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CustPrev, clipbrd, Unit2, DM1;

{$R *.DFM}

{$IFDEF CIL}
function DataSetCompareBookmarks(DataSet: TDataSet; Bookmark1, Bookmark2: TBookmarkStr): Integer;
var
  I1, I2: IntPtr;
begin
  try
    I1 := Marshal.StringToHGlobalAnsi(Bookmark1);
    I2 := Marshal.StringToHGlobalAnsi(Bookmark1);
    Result := DataSet.CompareBookmarks(TBookmark(I1), TBookmark(I2));
  finally
    Marshal.FreeHGlobal(I1);
    if Assigned(I2) then
      Marshal.FreeHGlobal(I2);
  end;
end;
{$ELSE}
function DataSetCompareBookmarks(DataSet: TDataSet; Bookmark1, Bookmark2: TBookmarkStr): Integer;
begin
  Result := DataSet.CompareBookmarks(TBookmark(Bookmark1), TBookmark(Bookmark2));
end;
{$ENDIF}

procedure TForm1.bOpenCloseClick(Sender: TObject);
begin
  if (DataModule1.Query1.Active = False) then begin
    DataModule1.Query1.Active := True;
    bOpenClose.Caption := 'Close';
  end else begin
    DataModule1.Query1.Active := False;
    bOpenClose.Caption := 'Open';
  end;
end;

procedure TForm1.Query1UpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  //
end;

procedure TForm1.DBGridEh2DrawColumnCell(Sender: TObject;
  const Rect: TRect; DataCol: Integer; Column: TColumnEh;
  State: TGridDrawState);
begin
  if (Rect.Top = DBGridEh2.CellRect(DBGridEh2.Col,DBGridEh2.Row).Top) and (not (gdFocused in State) or not DBGridEh2.Focused) then
    DBGridEh2.Canvas.Brush.Color := clAqua;
  DBGridEh2.DefaultDrawColumnCell(Rect,DataCol,Column,State);
end;

procedure TForm1.DBGridEh2DrawFooterCell(Sender: TObject; DataCol,
  Row: Integer; Column: TColumnEh; Rect: TRect; State: TGridDrawState);
begin
  DBGridEh2.DefaultDrawFooterCell(Rect,DataCol,Row,Column,State);
end;

procedure TForm1.DBGridEh2GetCellParams(Sender: TObject; Column: TColumnEh;
  AFont: TFont; var Background: TColor; State: TGridDrawState);
begin
  if DataModule1.Query1.FieldByName('IQty').Text = '17' then
    AFont.Style := AFont.Style + [fsBold];
end;

procedure TForm1.DBGridEh2GetFooterParams(Sender: TObject; DataCol,
  Row: Integer; Column: TColumnEh; AFont: TFont; var Background: TColor;
  var Alignment: TAlignment; State: TGridDrawState; var Text: String);
begin
  if (Column.Field.FieldName = 'PDescription') then Text := 'Qty = ' + Text
  else if (Column.Field.FieldName = 'VName') then Text := Text + ' records';
end;

procedure TForm1.DBGridEh1TitleBtnClick(Sender: TObject; ACol: Integer;
  Column: TColumnEh);
begin
{  case Column.Title.SortMarker of
    smNoneEh: Column.Title.SortMarker := smDownEh;
    smDownEh: Column.Title.SortMarker := smUpEh;
    smUpEh: Column.Title.SortMarker := smNoneEh;
  end;}
end;

procedure TForm1.cbClearSelectionClick(Sender: TObject);
begin
  if cbClearSelection.Checked then
    dbgList.OptionsEh := dbgList.OptionsEh + [dghClearSelection]
  else
    dbgList.OptionsEh := dbgList.OptionsEh - [dghClearSelection];
end;

procedure TForm1.cbShowIndicatorClick(Sender: TObject);
begin
  if cbShowIndicator.Checked then
    dbgList.Options := dbgList.Options + [dgIndicator]
  else
    dbgList.Options := dbgList.Options - [dgIndicator];
end;

procedure TForm1.cbTitleClick(Sender: TObject);
begin
  if cbTitle.Checked then
    dbgList.Options := dbgList.Options + [dgTitles]
  else
    dbgList.Options := dbgList.Options - [dgTitles];
end;

procedure TForm1.cbHighlightFocusClick(Sender: TObject);
begin
  if cbHighlightFocus.Checked then
    dbgList.OptionsEh := dbgList.OptionsEh + [dghHighlightFocus]
  else
    dbgList.OptionsEh := dbgList.OptionsEh - [dghHighlightFocus];
end;

procedure TForm1.cbMultiselectClick(Sender: TObject);
begin
  if cbMultiselect.Checked then
    dbgList.Options := dbgList.Options + [dgMultiselect]
  else
    dbgList.Options := dbgList.Options - [dgMultiselect];
end;

procedure TForm1.DBGridEh1SortMarkingChanged(Sender: TObject);
var i :Integer;
    s:String;
   function DeleteStr(str:String; sunstr:String): String;
   var i:Integer;
   begin
     i := Pos(sunstr,str);
     if i <> 0 then Delete(str,i,Length(sunstr));
     Result := str;
   end;
begin
  s := '';
  for i := 0 to DBGridEh1.SortMarkedColumns.Count-1 do
   if DBGridEh1.SortMarkedColumns[i].Title.SortMarker = smUpEh then
     s := s + DBGridEh1.SortMarkedColumns[i].FieldName + ' DESC , '
   else
     s := s + DBGridEh1.SortMarkedColumns[i].FieldName + ', ';
  if s <> '' then s := ' ORDER BY ' + Copy(s,1,Length(s)-2);
  s := DeleteStr(s,'1');
  DataModule1.Query1.SQL.Strings[DataModule1.Query1.SQL.Count-2] := s;
  DataModule1.Query1.Close;
  DataModule1.Query1.Open;
end;

procedure TForm1.bPreviewClick(Sender: TObject);
begin
  PrintDBGridEh1.DBGridEh := DBGridEh1;
  PrintDBGridEh1.SetSubstitutes(['%[Today]',DateToStr(Now)]);
  PrintDBGridEh1.Preview;
end;

procedure TForm1.bInpPreviewClick(Sender: TObject);
begin
  DBGridEh1.Visible := False;
  Panel1.Visible := False;
  PreviewBox1.Visible := True;
  PreviewSetupPanel.Visible := True;
  PreviewBox1.Printer.PrinterSetupOwner := DBGridEh1;
  PreviewBox1.Printer.OnPrinterSetupDialog := InplacePreviewSetupDialog;
  PrintDBGridEh1.DBGridEh := DBGridEh1;
  PrintDBGridEh1.PrintTo(PreviewBox1.Printer);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PreviewBox1.Align := alClient;
  Application.OnIdle := ApplicationIdle;
  FilterControlList := TStringList.Create;
//  FilterControlList.AddObject('VNo',Label1);
//  FilterControlList.AddObject('VName1',DBLookupComboboxEh1);
//  FilterControlList.AddObject('PDescription',DBLookupComboboxEh2);
  DBDateTimeEditEh2.Value := Now;
end;

procedure TForm1.bPrintClick(Sender: TObject);
begin
  PreviewBox1.PrintDialog;
end;

procedure TForm1.bPrinterSetupClick(Sender: TObject);
begin
  PreviewBox1.PrinterSetupDialog;
end;

procedure TForm1.bPrevPageClick(Sender: TObject);
begin
  PreviewBox1.PageIndex := Pred(PreviewBox1.PageIndex);
end;

procedure TForm1.bStopClick(Sender: TObject);
begin
  PreviewBox1.Printer.Abort;
end;

procedure TForm1.PreviewBox1PrinterPreviewChanged(Sender: TObject);
begin
  bStop.Enabled := PreviewBox1.Printer.Printing;
  bClosePreview.Enabled := not PreviewBox1.Printer.Printing;
  bPrint.Enabled := not PreviewBox1.Printer.Printing;
  bPrinterSetup.Enabled := not PreviewBox1.Printer.Printing;
  bPrevPage.Enabled:=PreviewBox1.PageIndex>1;
  bNextPage.Enabled:=PreviewBox1.PageIndex<PreviewBox1.PageCount;
  lPageInfo.Caption := 'Page '+IntToStr(PreviewBox1.PageIndex)+' of '+IntToStr(PreviewBox1.PageCount);
end;

procedure TForm1.bClosePreviewClick(Sender: TObject);
begin
    PreviewBox1.Visible := False;
    PreviewSetupPanel.Visible := False;
    Panel1.Visible := True;
    DBGridEh1.Visible := True;
end;

procedure TForm1.bNextPageClick(Sender: TObject);
begin
  PreviewBox1.PageIndex := Succ(PreviewBox1.PageIndex);
end;


procedure TForm1.InplacePreviewSetupDialog(Sender: TObject);
begin
  PreviewBox1.Printer.OnPrinterSetupDialog := InplacePreviewSetupDialog;
  PreviewBox1.Printer.PrinterSetupOwner := DBGridEh1;
  if PrintDBGridEh1.PrinterSetupDialog then
    PrintDBGridEh1.PrintTo(PreviewBox1.Printer);
end;

procedure TForm1.cCustomPreviewClick(Sender: TObject);
{$IFDEF CIL}
{$ELSE}
var
  FormImage: TBitmap;
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
  StdPrinterPreview:TPrinterPreview;
  i:Integer;
{$ENDIF}
begin
{$IFDEF CIL}
{$ELSE}
  StdPrinterPreview := SetPrinterPreview(fCustomPreview.PreviewBox1.Printer);
  try
  PrinterPreview.BeginDoc;
  try
    i := 0;
    while True do begin
      PageControl1.ActivePage := PageControl1.Pages[i];
      FormImage := GetFormImage;
      Canvas.Lock;
      try
        { Paint bitmap to the printer }
        with PrinterPreview, Canvas do
        begin
          Bits := FormImage.Handle;
          GetDIBSizes(Bits, InfoSize, ImageSize);
          Info := AllocMem(InfoSize);
          try
            Image := AllocMem(ImageSize);
            try
              GetDIB(Bits, 0, Info^, Image^);
              with Info^.bmiHeader do
              begin
                DIBWidth := biWidth;
                DIBHeight := biHeight;
              end;
              case PrintScale of
                poProportional:
                  begin
                    PrintWidth := MulDiv(DIBWidth, GetDeviceCaps(Printer.Handle,
                      LOGPIXELSX), PixelsPerInch);
                    PrintHeight := MulDiv(DIBHeight, GetDeviceCaps(Printer.Handle,
                      LOGPIXELSY), PixelsPerInch);
                  end;
                poPrintToFit:
                  begin
                    PrintWidth := MulDiv(DIBWidth, PagEheight, DIBHeight);
                    if PrintWidth < PageWidth then
                      PrintHeight := PagEheight
                    else
                    begin
                      PrintWidth := PageWidth;
                      PrintHeight := MulDiv(DIBHeight, PageWidth, DIBWidth);
                    end;
                  end;
              else
                PrintWidth := DIBWidth;
                PrintHeight := DIBHeight;
              end;
              StretchDIBits(Canvas.Handle, 0, 0, PrintWidth, PrintHeight, 0, 0,
                DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
            finally
              FreeMem(Image, ImageSize);
            end;
          finally
            FreeMem(Info, InfoSize);
          end;
        end;
      finally
        Canvas.Unlock;
        FormImage.Free;
      end;
      Inc(i);
      if PageControl1.PageCount = i then Exit;
      PrinterPreview.NewPage;
    end;
  finally
    PrinterPreview.EndDoc;
  end;
  finally
    SetPrinterPreview(StdPrinterPreview);
    PageControl1.ActivePage := PageControl1.Pages[0];
  end;
{$ENDIF}
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
var mi:TMenuItem;
    S:String;
    p:Integer;
  function GetBackCharPos(S:String; C:Char; N:Integer):Integer;
  var i:Integer;
  begin
    Result := 1;
    for i := Length(S) downto 1 do
      if S[i] = C then begin
        Dec(N);
        if N = 0 then begin
          Result := i+1;
          Exit;
        end;
      end;
  end;
  type
    TReplaceFlags = set of (rfReplaceAll, rfIgnoreCase);

  function StringReplace(const S, OldPattern, NewPattern: string;
    Flags: TReplaceFlags): string;
  var
    SearchStr, Patt, NewStr: string;
    Offset: Integer;
  begin
    if rfIgnoreCase in Flags then
    begin
      SearchStr := AnsiUpperCase(S);
      Patt := AnsiUpperCase(OldPattern);
    end else
    begin
      SearchStr := S;
      Patt := OldPattern;
    end;
    NewStr := S;
    Result := '';
    while SearchStr <> '' do
    begin
     Offset := AnsiPos(Patt, SearchStr);
      if Offset = 0 then
      begin
        Result := Result + NewStr;
        Break;
      end;
      Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
      if not (rfReplaceAll in Flags) then
      begin
        Result := Result + NewStr;
        Break;
      end;
     SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
    end;
  end;
begin
  mi := TMenuItem.Create(nil);
  with DBGridEh1.VisibleColumns do begin
    S := Items[Count-1].Title.Caption;
    p := GetBackCharPos(S,'|',2);
    S := Copy(S,p,Length(S));
    mi.Caption := StringReplace(S,'|',#9,[rfReplaceAll]);
{$IFDEF CIL}
    mi.Tag := Variant(Items[Count-1]);
{$ELSE}
    mi.Tag := Integer(Items[Count-1]);
{$ENDIF}
    mi.OnClick := ColumnMenuItem;
  end;
  pmNoVisibleCols.Items.Insert(0,mi);
  DBGridEh1.VisibleColumns.Items[DBGridEh1.VisibleColumns.Count-1].Visible := False;
  if (DBGridEh1.VisibleColumns.Count = 0) then ToolButton2.Enabled := False;
  ToolButton1.Enabled := True;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  pmNoVisibleCols.Items[0].Free;
  DBGridEh1.Columns[DBGridEh1.VisibleColumns.Count].Visible := True;
  if (DBGridEh1.Columns.Count = DBGridEh1.VisibleColumns.Count) then
    ToolButton1.Enabled := False;
  ToolButton2.Enabled := True;
end;

procedure TForm1.ColumnMenuItem(Sender: TObject);
begin
  TColumnEh(TMenuItem(Sender).Tag).Index := DBGridEh1.VisibleColumns.Count;
  TColumnEh(TMenuItem(Sender).Tag).Visible := True;
  Sender.Free;
  if (DBGridEh1.Columns.Count = DBGridEh1.VisibleColumns.Count) then
    ToolButton1.Enabled := False;
end;

function TForm1.GridSelectionAsText(AGrid: TDBGridEh): String;
var //bm:TBookmarkStr;
    i,j :Integer;
    ss: TStringStream;
    function StringTab(s:String; Index, Count:Integer):String;
    begin
      if Index <> Count then
        Result := s + #09
      else
        Result := s;
    end;
begin
  Result := '';
  with AGrid do begin
    if Selection.SelectionType = gstNon then Exit;
    ss := TStringStream.Create('');
    with Datasource.Dataset do
    try
      // BM := Bookmark;
      SaveBookmark;
      DisableControls;
      try
        case Selection.SelectionType of
          gstRecordBookmarks:
          begin
            for I := 0 to Selection.Rows.Count-1 do
            begin
              Bookmark := Selection.Rows[I];
              for j := 0 to VisibleColumns.Count - 1 do
                ss.WriteString(StringTab(VisibleColumns[j].DisplayText,j,VisibleColumns.Count - 1));
              ss.WriteString(#13#10);
            end;
          end;
          gstRectangle: begin
             Bookmark := Selection.Rect.TopRow;
             while True do begin
               for j := Selection.Rect.LeftCol to Selection.Rect.RightCol do
                 if Columns[j].Visible then
                   ss.WriteString(StringTab(Columns[j].DisplayText,j,Selection.Rect.RightCol));
               if DataSetCompareBookmarks(Datasource.Dataset,Selection.Rect.BottomRow,Bookmark) = 0 then Break;
               Next;
               if Eof then Break;
               ss.WriteString(#13#10);
             end;
          end;
          gstColumns: begin
             for j := 0 to Selection.Columns.Count-1 do
                 ss.WriteString(StringTab(Selection.Columns[j].Title.Caption,j,Selection.Columns.Count-1));
             ss.WriteString(#13#10);
             First;
             while  EOF = False do begin
               for j := 0 to Selection.Columns.Count-1 do
                 ss.WriteString(StringTab(Selection.Columns[j].DisplayText,j,Selection.Columns.Count-1));
               ss.WriteString(#13#10);
               Next;
             end;
             for i := 0 to FooterRowCount-1 do begin
               for j := 0 to Selection.Columns.Count-1 do
                   ss.WriteString(StringTab(GetFooterValue(i,Selection.Columns[j]),j,Selection.Columns.Count-1));
               ss.WriteString(#13#10);
             end;
          end;
          gstAll: begin
             for j := 0 to VisibleColumns.Count-1 do
                 ss.WriteString(StringTab(VisibleColumns[j].Title.Caption,j,VisibleColumns.Count-1));
             ss.WriteString(#13#10);
             First;
             while  EOF = False do begin
               for j := 0 to VisibleColumns.Count-1 do
                 ss.WriteString(StringTab(VisibleColumns[j].DisplayText,j,VisibleColumns.Count-1));
               ss.WriteString(#13#10);
               Next;
             end;
             for i := 0 to FooterRowCount-1 do begin
               for j := 0 to VisibleColumns.Count-1 do
                   ss.WriteString(StringTab(GetFooterValue(i,VisibleColumns[j]),j,VisibleColumns.Count-1));
               ss.WriteString(#13#10);
             end;
          end;
        end;
        Result := ss.DataString;
      finally
        //Bookmark := BM;
        RestoreBookmark;
        EnableControls;
      end;
    finally
      ss.Free;
    end;
  end;
end;

procedure TForm1.dbgListDragDrop(Sender, Source: TObject; X, Y: Integer);
var i,j:Integer;
begin
  if Source = dbgList1 then begin
    dbgList.DataSource.DataSet.DisableControls;
    dbgList1.DataSource.DataSet.DisableControls;
    dbgList.SaveBookmark;
    if dbgList1.Selection.SelectionType = gstRecordBookmarks then
      for i := 0 to dbgList1.SelectedRows.Count-1 do
      begin
        dbgList1.DataSource.DataSet.Bookmark := dbgList1.SelectedRows[I];
        dbgList.DataSource.DataSet.Append;
        dbgList.DataSource.DataSet.Edit;
        for j := 0 to dbgList.DataSource.DataSet.FieldCount-1 do
          dbgList.DataSource.DataSet.Fields[j].Value := dbgList1.DataSource.DataSet.Fields[j].Value;
        dbgList.DataSource.DataSet.Post;
      end
    else if dbgList1.Selection.SelectionType = gstAll then begin
      dbgList1.DataSource.DataSet.First;
      while  dbgList1.DataSource.DataSet.EOF = False do begin
        dbgList.DataSource.DataSet.Append;
        dbgList.DataSource.DataSet.Edit;
        for j := 0 to dbgList.DataSource.DataSet.FieldCount-1 do
          dbgList.DataSource.DataSet.Fields[j].Value := dbgList1.DataSource.DataSet.Fields[j].Value;
        dbgList.DataSource.DataSet.Post;
        dbgList1.DataSource.DataSet.Delete;
      end;
      dbgList1.Selection.Clear;
    end;
    dbgList.RestoreBookmark;
    dbgList1.SelectedRows.Delete;
    dbgList1.DataSource.DataSet.Refresh;
    dbgList1.DataSource.DataSet.EnableControls;
    dbgList.DataSource.DataSet.EnableControls;
  end;
end;

procedure TForm1.dbgListDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source = dbgList1 then Accept := True else Accept := False;
end;

procedure TForm1.dbgListStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
{
              ABOUT DRAG & DROP DATA BETWEEN APPLICATIONS.
  Standard drag and drop capacity don't support interapplication interaction.
  To ensure drag and drop from one application to over need use over tools.
  One of such tools is DRAG & DROP COMPONENT SUITE VERSION by Angus Johnson &
  Anders Melander.
  This is a set of components that implements Dragging & Dropping of data
  between applications.
  These components implement the COM interfaces - IDataObject, IDropSource and
  IDropTarget which are the backbone of Windows drag-and-drop.
  The homesite for the Drag and Drop Component Suite is http://www.melander.dk.
  To make use this component download it, install DRAG & DROP COMPONENT SUITE
  VERSION to Delphi , drop TDropTextSource on this Form, do visible
  cbInterAppDragNDrop checkbox and uncomment below text.
  It give you capacity to drag grid info in such applications as Excel or Word
}

//  if not cbInterAppDragNDrop.Checked then Exit;
//  CancelDrag;
//  DropTextSource1.Text := GridSelectionAsText(dbgList);
//  DropTextSource1.Execute;

end;

procedure TForm1.dbgList1DragDrop(Sender, Source: TObject; X, Y: Integer);
var i,j:Integer;
begin
  if Source = dbgList then begin
    dbgList1.DataSource.DataSet.DisableControls;
    dbgList.DataSource.DataSet.DisableControls;
    dbgList1.SaveBookmark;
    if dbgList.Selection.SelectionType = gstRecordBookmarks then
      for i := 0 to dbgList.SelectedRows.Count-1 do
      begin
        dbgList.DataSource.DataSet.Bookmark := dbgList.SelectedRows[I];
        dbgList1.DataSource.DataSet.Append;
        dbgList1.DataSource.DataSet.Edit;
        for j := 0 to dbgList1.DataSource.DataSet.FieldCount-1 do
          dbgList1.DataSource.DataSet.Fields[j].Value := dbgList.DataSource.DataSet.Fields[j].Value;
        dbgList1.DataSource.DataSet.Post;
      end
    else if dbgList.Selection.SelectionType = gstAll then begin
      dbgList.DataSource.DataSet.First;
      while  dbgList.DataSource.DataSet.EOF = False do begin
        dbgList1.DataSource.DataSet.Append;
        dbgList1.DataSource.DataSet.Edit;
        for j := 0 to dbgList1.DataSource.DataSet.FieldCount-1 do
          dbgList1.DataSource.DataSet.Fields[j].Value := dbgList.DataSource.DataSet.Fields[j].Value;
        dbgList1.DataSource.DataSet.Post;
        dbgList.DataSource.DataSet.Delete;
      end;
      dbgList.Selection.Clear;
    end;
    dbgList1.RestoreBookmark;
    dbgList.SelectedRows.Delete;
    dbgList.DataSource.DataSet.Refresh;
    dbgList.DataSource.DataSet.EnableControls;
    dbgList1.DataSource.DataSet.EnableControls;
  end;
end;

procedure TForm1.dbgList1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source = dbgList then Accept := True else Accept := False;
end;

procedure TForm1.cbDragNDropClick(Sender: TObject);
begin
  if cbDragNDrop.Checked then begin
    dbgList.DragMode := dmAutomatic;
    dbgList1.Visible := True and not cbInterAppDragNDrop.Checked;
  end
  else begin
    dbgList.DragMode := dmManual;
    dbgList1.Visible := False;
  end;
end;

procedure TForm1.cbDichromaticClick(Sender: TObject);
begin
  dbgList.Invalidate;
end;

procedure TForm1.dbgListGetCellParams(Sender: TObject; Column: TColumnEh;
  AFont: TFont; var Background: TColor; State: TGridDrawState);
begin
  if cbDichromatic.Checked then
    if dbgList.SumList.RecNo mod 2 = 1 then
      Background := $00FFC4C4
    else
      Background := $00FFDDDD;
end;

procedure TForm1.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  // Under Delphi 4 and upper better to user Actions to determine
  // enablitity buttons and menus
  bbCopy.Enabled := DBGridEh1.Selection.SelectionType <> gstNon;
  if ActiveControl is TDBGridEh then
  with TDBGridEh(ActiveControl) do
    begin
      ppmCut.Enabled := CheckCutAction and (geaCutEh in EditActions);
      ppmCopy.Enabled := CheckCopyAction and (geaCopyEh in EditActions);
      ppmPaste.Enabled := CheckPasteAction and (geaPasteEh in EditActions);
      ppmDelete.Enabled := CheckDeleteAction and (geaDeleteEh in EditActions);
      ppmSelectAll.Enabled := CheckSelectAllAction and (geaSelectAllEh in EditActions);
      ppmSaveSelection.Enabled := CheckCopyAction and (geaCopyEh in EditActions);
      ppmPreview.Enabled := True;
    end;
end;

procedure TForm1.bbCopyClick(Sender: TObject);
begin
  // old style Clipboard.AsText := GridSelectionAsText(DBGridEh1);
  DBGridEh_DoCopyAction(DBGridEh1,False);
end;

procedure TForm1.DBGridEh1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_INSERT) and ([ssCtrl] = Shift) then
    Clipboard.AsText := GridSelectionAsText(DBGridEh1);
end;

procedure TForm1.cbInterAppDragNDropClick(Sender: TObject);
begin
  dbgList1.Visible := True and not cbInterAppDragNDrop.Checked;
end;

procedure TForm1.DBGridEh2EditButtonClick(Sender: TObject);
var vn,vs:String;
begin
  vn := DataModule1.Query1.FieldByName('VNo').AsString;
  if Form2.Execute(DBGridEh2.InplaceEditor,vn,vs) then
  begin
    DataModule1.Query1.Edit;
    DataModule1.Query1.FieldByName('VNo').AsString := vn;
    DataModule1.Query1.FieldByName('VName').AsString := vs;
    DataModule1.Query1.Post;
  end;
end;

procedure TForm1.ppmCutClick(Sender: TObject);
begin
  if (ActiveControl is TDBGridEh) then
    with TDBGridEh(ActiveControl) do
      if CheckCutAction and (geaCutEh in EditActions) then
        DBGridEh_DoCutAction(TDBGridEh(ActiveControl),False);
end;

procedure TForm1.ppmCopyClick(Sender: TObject);
begin
  if (ActiveControl is TDBGridEh) then
    with TDBGridEh(ActiveControl) do
      if CheckCopyAction and (geaCopyEh in EditActions) then
        DBGridEh_DoCopyAction(TDBGridEh(ActiveControl),False);
end;

procedure TForm1.ppmPasteClick(Sender: TObject);
begin
  if (ActiveControl is TDBGridEh) then
    with TDBGridEh(ActiveControl) do
      if CheckPasteAction and (geaPasteEh in EditActions) then
        DBGridEh_DoPasteAction(TDBGridEh(ActiveControl),False);
end;

procedure TForm1.ppmDeleteClick(Sender: TObject);
begin
  if (ActiveControl is TDBGridEh) then
    with TDBGridEh(ActiveControl) do
      if CheckDeleteAction and (geaDeleteEh in EditActions) then
        DBGridEh_DoDeleteAction(TDBGridEh(ActiveControl),False);
end;

procedure TForm1.ppmSelectAllClick(Sender: TObject);
begin
  if (ActiveControl is TDBGridEh) then
    with TDBGridEh(ActiveControl) do
      if CheckSelectAllAction and (geaSelectAllEh in EditActions) then
        Selection.SelectAll;
end;

procedure TForm1.ppmPreviewClick(Sender: TObject);
begin
  if (ActiveControl is TDBGridEh) then
  begin
    PrintDBGridEh1.DBGridEh := TDBGridEh(ActiveControl);
    PrintDBGridEh1.SetSubstitutes(['%[Today]',DateToStr(Now)]);
    PrintDBGridEh1.Preview;
  end;
end;

procedure TForm1.ppmSaveSelectionClick(Sender: TObject);
var ExpClass:TDBGridEhExportClass;
    Ext:String;
begin
  SaveDialog1.FileName := 'file1';
  if (ActiveControl is TDBGridEh) then
    if SaveDialog1.Execute then
    begin
      case SaveDialog1.FilterIndex of
        1: begin ExpClass := TDBGridEhExportAsText; Ext := 'txt'; end;
        2: begin ExpClass := TDBGridEhExportAsCSV; Ext := 'csv'; end;
        3: begin ExpClass := TDBGridEhExportAsHTML; Ext := 'htm'; end;
        4: begin ExpClass := TDBGridEhExportAsRTF; Ext := 'rtf'; end;
        5: begin ExpClass := TDBGridEhExportAsXLS; Ext := 'xls'; end;
      else
        ExpClass := nil; Ext := '';
      end;
      if ExpClass <> nil then
      begin
        if UpperCase(Copy(SaveDialog1.FileName,Length(SaveDialog1.FileName)-2,3)) <>
           UpperCase(Ext) then
          SaveDialog1.FileName := SaveDialog1.FileName + '.' + Ext;
        SaveDBGridEhToExportFile(ExpClass,TDBGridEh(ActiveControl),
             SaveDialog1.FileName,False);
      end;
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FilterControlList.Free;
end;

procedure TForm1.DBGridEh1ColWidthsChanged(Sender: TObject);
var Indent,i,k:Integer;
    control:TControl;
begin
    if not Assigned(FilterControlList) then Exit;
    Indent := IndicatorWidth+2;
    for i := 0 to DBGridEh1.Columns.Count-1 do
    begin
      k := FilterControlList.IndexOf(DBGridEh1.Columns.Items[i].FieldName);
      if (k <> -1) then
      begin
        control := (TControl(FilterControlList.Objects[k]));
        control.Left := Indent+1;
        control.Width := DBGridEh1.Columns.Items[i].Width-1;
      end;
      Indent := Indent+DBGridEh1.Columns.Items[i].Width+1;
    end;
end;

procedure TForm1.DBLookupComboboxEh1KeyValueChanged(Sender: TObject);
begin
  UpdateQuery1Filter;
end;

procedure TForm1.DBLookupComboboxEh2KeyValueChanged(Sender: TObject);
begin
  UpdateQuery1Filter;
end;

procedure TForm1.UpdateQuery1Filter;
var s:String;
begin
  s := '';
{  if DBLookupComboboxEh1.KeyValue <> Null then
    s := 'VNo = ' + VarToStr(DBLookupComboboxEh1.KeyValue);
  if DBLookupComboboxEh2.KeyValue <> Null then
    if s <> '' then
      s := s + ' And  PDescription = ''' + VarToStr(DBLookupComboboxEh2.KeyValue) + ''''
    else
      s := 'PDescription = ''' + VarToStr(DBLookupComboboxEh2.KeyValue) + '''';}
  DataModule1.Query1.Filter := s;
end;

procedure TForm1.DBGridEh1Columns1DropDownBoxGetCellParams(Sender: TObject;
  Column: TColumnEh; AFont: TFont; var Background: TColor;
  State: TGridDrawState);
begin
 if DataModule1.qrVendors.FieldValues['State'] = 'CA' then
    AFont.Color := clRed;
end;

var
  IniPropStorageMan: TIniPropStorageManEh;

procedure TForm1.DBComboBoxEh1GetItemImageIndex(Sender: TObject;
  ItemIndex: Integer; var ImageIndex: Integer);
begin
  ImageIndex := -1;
  case ItemIndex of
   2: ImageIndex := 1;
   5: ImageIndex := 3;
   6: ImageIndex := 0;
   4: ImageIndex := 2;
  end;
end;

initialization
  IniPropStorageMan := TIniPropStorageManEh.Create(nil);
  IniPropStorageMan.IniFileName := ExtractFileDir(ParamStr(0)) + '\Demo1.Ini';
  SetDefaultPropStorageManager(IniPropStorageMan);
  DBGridEhDefaultStyle.FilterEditCloseUpApplyFilter := True;
end.
