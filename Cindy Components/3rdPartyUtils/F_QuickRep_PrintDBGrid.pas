{   Unit F_QuickRep_PrintDBGrid

    Description:
    Unit for Dataset/DBGrid print with graphic, memo and summary features.

    I will try in future to implement graphic field printing

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

unit F_QuickRep_PrintDBGrid;

{$I ..\Core\cyCompilerDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Grids, DBGrids, DB, DBClient, Provider, ExtCtrls, QuickRpt,
  qrpctrls, QRCtrls, qrprntr, cyBaseDBGrid, cyDBAdvGrid, cyDBGrid, QRPDFFilt, vcl.cyTypes, VCL.cyGraphics;

type
  TFrmQuickRep_PrintDBGrid = class(TForm)
    DBGridPrint: TcyDBAdvGrid;
    DSPQuickRepSource: TDataSetProvider;
    CdsQuickRep: TClientDataSet;
    QuickRep1: TQuickRep;
    QRGridReport1: TQRGridReport;
    dsQuickRep: TDataSource;
    SummaryBand1: TQRBand;
    PageHeaderBand1: TQRBand;
    qrlTitle: TQRLabel;
    QRPDFFilter1: TQRPDFFilter;
    QRExpr1: TQRExpr;
    QRGraphicControlsBand: TQRWildBand;
    QRRichTextControlsBand: TQRWildBand;
    procedure FormCreate(Sender: TObject);
    procedure QuickRep1BeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
    procedure CdsQuickRepAfterOpen(DataSet: TDataSet);
    procedure RecordBandBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
  private
    fOnCustomRecordBandBeforePrint: TQRBandBeforePrintEvent;
    { Private declarations }
    function  GraphicControlsBand_FindImage: TQRImage;
    function  RichTextControlsBand_FindRichText: TQRRichText;
    procedure Get_Image_Parent_and_master(aQRImage: TQRImage; var RsltBand: TQRCustomBand; var RsltLeft, RsltWidth: Integer);
    procedure Get_RichText_Parent_and_master(aQRRichText: TQRRichText; var RsltBand: TQRCustomBand; var RsltLeft, RsltWidth: Integer);

    function  SummaryBand1_FindExpr: TQRExpr;
    procedure Get_Expr_Parent_and_master(aQRExpr: TQRExpr; var RsltBand: TWinControl; var RsltMaster: TComponent; var RsltLeft, RsltWidth: Integer);
  public
    { Public declarations }
    IncColWidth: Integer;
    AdjustDBGridColumnsToContent: Boolean;
    DefaultImageFieldContent: TFieldContentRendering;

    procedure PrepareField(aField: TField);
    procedure QuickReport_Prepare(aDBGrid: dbGrids.TDBGrid; aTitle: String; SpanPagesHorizontally: Boolean); overload;
    procedure QuickReport_Prepare(aDataset: TDataset; aTitle: String; SpanPagesHorizontally: Boolean); overload;
    procedure QuickReport_Prepare(aDBGrid: TcyDBGrid; aTitle: String; CheckedRowsOnly: Boolean; SpanPagesHorizontally: Boolean); overload;
    procedure QuickReport_Prepare(aDBGrid: TcyDBAdvGrid; aTitle: String; CheckedRowsOnly: Boolean; SpanPagesHorizontally: Boolean); overload;

    function  AddColumnSummary(ColumnFieldName: String; Mask: String; IncColumn: Integer): TQrExpr;
    function  AddColumnImage(ColumnFieldName: String): TQrImage;
    function  AddColumnRichText(ColumnFieldName: String): TQrRichText;

    procedure QuickReport_Print;
    procedure QuickReport_PreviewModal;
    procedure QuickReport_ExportToPdf(Filename: String; CompressionOn: Boolean);

    property OnCustomRecordBandBeforePrint: TQRBandBeforePrintEvent read fOnCustomRecordBandBeforePrint write fOnCustomRecordBandBeforePrint;
  end;

var
  FrmQuickRep_PrintDBGrid: TFrmQuickRep_PrintDBGrid;


  function isDetailBand(aControl: TControl): Boolean;
  function GetGraphicFromField(Field: TBlobField; FieldContent: TGraphicClass): TGraphic;  // -> Move to cyDB.pas !

implementation

{$R *.dfm}

function isDetailBand(aControl: TControl): Boolean;
begin
  Result := false;
  if aControl is TQRBand then
    if TQRBand(aControl).BandType = rbDetail then
      Result := true;
end;

function GetGraphicFromField(Field: TBlobField; FieldContent: TGraphicClass): TGraphic;  // -> Move to cyDB.pas !
var
  aStream: TMemoryStream;
begin
  Result := Nil;
  if Field.IsNull then Exit;

  Result := FieldContent.Create;    // FieldContent = TBitmap etc ...

  if Field is TGraphicField then         // TGraphicField is descendent of TBlobField and can contain bitmap, ico etc ...
  begin
    try
      aStream := TMemoryStream.Create;
      Field.SaveToStream(aStream);

      // Does not work with TBlobField:
      aStream.Position := 0;
      Result.LoadFromStream(aStream);
    finally
      aStream.Free;
    end;
  end
  else
    Result.Assign(Field);  // TBlobField handling
end;

procedure TFrmQuickRep_PrintDBGrid.CdsQuickRepAfterOpen(DataSet: TDataSet);
var
  f: Integer;
begin
  // Avoid error when ReadOnly set on imported dataset :
  for f := 0 to CdsQuickRep.FieldCount-1 do
  begin
    CdsQuickRep.Fields[f].ReadOnly := false;
    CdsQuickRep.Fields[f].Required := false;   // 2017-07-28 ..
  end;
end;

procedure TFrmQuickRep_PrintDBGrid.FormCreate(Sender: TObject);
begin
  IncColWidth := 8;
  AdjustDBGridColumnsToContent := true;
  DBGridPrint.ContentFieldsRender.BooleanField := bfDefault;
  DBGridPrint.ContentFieldsRender.GraphicField := gfDefault;
  DBGridPrint.ContentFieldsRender.MemoField    := mfDefault;
  DefaultImageFieldContent := fcBitmap;
  fOnCustomRecordBandBeforePrint := Nil;
end;

procedure TFrmQuickRep_PrintDBGrid.PrepareField(aField: TField);
var
  AddQrImage: Boolean;
  AddQrMemo: Boolean;
begin
  if not Assigned(aField) then Exit;

  AddQrImage := false;
  AddQrMemo  := false;

  if aField is TGraphicField then    // TGraphicField is descendent of TBlobField and can contain bitmap, ico etc ...
    AddQrImage := true
  else
    if aField is TBlobField then
      case TBlobField(aField).BlobType of
        ftGraphic, ftTypedBinary, ftBlob:
          AddQrImage := true;

        ftFmtMemo {$IFDEF UNICODE} , ftWideMemo {$ENDIF} :
          AddQrMemo := true;
      end;

  if aField is TMemoField then
    AddQrMemo := true;

  if AddQrMemo then
    AddColumnRichText(aField.FieldName);

  if AddQrImage then
    AddColumnImage(aField.FieldName);
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_Prepare(aDBGrid: dbGrids.TDBGrid; aTitle: String; SpanPagesHorizontally: Boolean);
var
  c: Integer;
begin
  QRGridReport1.DBGrid := aDBGrid;
  QuickRep1.ReportTitle := aTitle;
  qrlTitle.Caption := aTitle;
  QRGridReport1.SpanPagesHorizontally := SpanPagesHorizontally;

  // Add image/memo field handling :
  for c := 0 to aDBGrid.Columns.Count-1 do
    if aDBGrid.Columns[c].Visible then
      PrepareField(aDBGrid.DataSource.DataSet.FindField(aDBGrid.Columns[c].FieldName));
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_Prepare(aDataset: TDataset; aTitle: String; SpanPagesHorizontally: Boolean);
var
  c: Integer;
begin
  CdsQuickRep.Active := false;
  DSPQuickRepSource.DataSet := aDataset;
  aDataset.First;
  CdsQuickRep.PacketRecords := -1; // Import all records
  CdsQuickRep.Active := true;

  // Adjust columns :
  if AdjustDBGridColumnsToContent then
    DBGridPrint.AdjustColumnsToContent(0, IncColWidth); // Max rows = 0

  QuickRep1.ReportTitle := aTitle;
  qrlTitle.Caption := aTitle;
  QRGridReport1.SpanPagesHorizontally := SpanPagesHorizontally;

  // Add image/memo field handling :
  for c := 0 to DBGridPrint.Columns.Count-1 do
    if DBGridPrint.Columns[c].Visible then
      PrepareField(DBGridPrint.DataSource.DataSet.FindField(DBGridPrint.Columns[c].FieldName));
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_Prepare(aDBGrid: TcyDBGrid; aTitle: String; CheckedRowsOnly: Boolean; SpanPagesHorizontally: Boolean);
var
  r, c, f: Integer;
begin
  CdsQuickRep.Active := false;
  DSPQuickRepSource.DataSet := aDBGrid.DataSource.DataSet;    // -> BUG : Remove any Range on BDE dataset linked to the aDBGrid !

  // Records to print :
  if CheckedRowsOnly then
  begin
    CdsQuickRep.PacketRecords := 0; // Don' t import records !
    CdsQuickRep.Active := true;

    for r := 0 to aDBGrid.CheckedList.Count-1 do
    begin
      aDBGrid.DataSource.DataSet.GotoBookmark( TBookmark(aDBGrid.CheckedList[r]) );
      CdsQuickRep.Append;
      CdsQuickRep.CopyFields(aDBGrid.DataSource.DataSet);
      CdsQuickRep.Post;
    end;
  end
  else
    if QRGridReport1.SelectedRecordsOnly then
    begin
      CdsQuickRep.PacketRecords := 0; // Don' t import records !
      CdsQuickRep.Active := true;

      for r := 0 to aDBGrid.SelectedRows.Count-1 do
      begin
        aDBGrid.DataSource.DataSet.GotoBookmark( TBookmark(aDBGrid.SelectedRows[r]) );
        CdsQuickRep.Append;
        CdsQuickRep.CopyFields(aDBGrid.DataSource.DataSet);
        CdsQuickRep.Post;
      end;

      QRGridReport1.SelectedRecordsOnly := false;  // Print all CdsQuickRep records ...
    end
    else begin
      aDBGrid.Datasource.Dataset.First;

(*  NOT WORKING FOR NOW ...
      // Can't use this method for TTable (Borland Database engine) component because it remove any range on table:
      if AnsiUppercase(aDBGrid.Datasource.Dataset.ClassName) = 'TTABLE' then
      begin
        CdsQuickRep.PacketRecords := 0; // Don' t import records !
        CdsQuickRep.Active := true;


        with aDBGrid.Datasource do
        begin
          while not Dataset.Eof do
          begin
            CdsQuickRep.Append;
            CdsQuickRep.CopyFields(aDBGrid.DataSource.DataSet);
            CdsQuickRep.Post;

            Dataset.Next;
          end;
        end;
      end
      else begin
        CdsQuickRep.PacketRecords := -1; // Import all records
        CdsQuickRep.Active := true;
      end;          *)

      CdsQuickRep.PacketRecords := -1; // Import all records -> Bad char importing WideString field values in DELPHI 2009 !!!
      CdsQuickRep.Active := true;
    end;

  // Display label / display format / display values :
  for f := 0 to CdsQuickRep.FieldCount-1 do
    try
      CdsQuickRep.Fields[f].DisplayLabel := aDBGrid.DataSource.Dataset.FindField(CdsQuickRep.Fields[f].FieldName).DisplayLabel;

      if CdsQuickRep.Fields[f] is TIntegerField then
        TIntegerField(CdsQuickRep.Fields[f]).DisplayFormat := TIntegerField( aDBGrid.DataSource.Dataset.FindField(CdsQuickRep.Fields[f].FieldName) ).DisplayFormat;

      if CdsQuickRep.Fields[f] is TFloatField then
        TFloatField(CdsQuickRep.Fields[f]).DisplayFormat := TFloatField( aDBGrid.DataSource.Dataset.FindField(CdsQuickRep.Fields[f].FieldName) ).DisplayFormat;

      if CdsQuickRep.Fields[f] is TBooleanField then
        TBooleanField(CdsQuickRep.Fields[f]).DisplayValues := TBooleanField( aDBGrid.DataSource.Dataset.FindField(CdsQuickRep.Fields[f].FieldName) ).DisplayValues;
    except
    end;

  // Add same columns definition to DBGridPrint as in aDBGrid :
  DBGridPrint.Columns.Assign(aDBGrid.Columns);
  DBGridPrint.CustomLayoutOptions.DataRowHeight := aDBGrid.CustomLayoutOptions.DataRowHeight;
  DBGridPrint.CustomLayoutOptions.TitleRowHeight := aDBGrid.CustomLayoutOptions.TitleRowHeight;

  // Remove columns that are not visible to avoid QRGridReport1 printing visual bug :
  for c := aDBGrid.Columns.Count-1 downto 0 do
    if not aDBGrid.Columns[c].Visible then
      DBGridPrint.Columns[c].Free;

  // Adjust columns :
  if AdjustDBGridColumnsToContent then
    DBGridPrint.AdjustColumnsToContent(0, IncColWidth); // Max rows = 0

  QuickRep1.ReportTitle := aTitle;
  qrlTitle.Caption := aTitle;
  QRGridReport1.SpanPagesHorizontally := SpanPagesHorizontally;

  // Add image/memo field handling :
  for c := 0 to DBGridPrint.Columns.Count-1 do
    if DBGridPrint.Columns[c].Visible then
      PrepareField(DBGridPrint.DataSource.DataSet.FindField(DBGridPrint.Columns[c].FieldName));
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_Prepare(aDBGrid: TcyDBAdvGrid; aTitle: String; CheckedRowsOnly: Boolean; SpanPagesHorizontally: Boolean);
begin
  if Assigned(aDBGrid.OnSetContentFieldRendering) then
    DBGridPrint.OnSetContentFieldRendering := aDBGrid.OnSetContentFieldRendering;

  QuickReport_Prepare(TcyDBGrid(aDBGrid), aTitle, CheckedRowsOnly, SpanPagesHorizontally);
end;










function TFrmQuickRep_PrintDBGrid.AddColumnSummary(ColumnFieldName: String; Mask: String; IncColumn: Integer): TQrExpr;
var
  c: Integer;
begin
  Result := TQrExpr.Create(SummaryBand1);
  Result.ParentReport := QuickRep1;
  Result.Parent := SummaryBand1;
  Result.ResetAfterPrint := true;
  Result.Expression := 'SUM(' + ColumnFieldName + ')';
  Result.Hint := ColumnFieldName;
  Result.Mask := Mask;
  Result.Transparent := true;
  Result.AutoSize := false;
  Result.Alignment := taRightJustify;
  Result.Top := 3;
  Result.Left := 0;
  Result.Font.Size := 8;

  // Inc column size :
  for c := 0 to QRGridReport1.DBGrid.Columns.Count-1 do
    if AnsiUpperCase(QRGridReport1.DBGrid.Columns[c].FieldName) = AnsiUpperCase(ColumnFieldName) then
      QRGridReport1.DBGrid.Columns[c].Width := QRGridReport1.DBGrid.Columns[c].Width + IncColumn;
end;

function TFrmQuickRep_PrintDBGrid.AddColumnImage(ColumnFieldName: String): TQrImage;
begin
  Result := TQrImage.Create(QRGraphicControlsBand);
  Result.ParentReport := QuickRep1;
  Result.Parent := QRGraphicControlsBand;        // Will be moved on BeforePrint ...
  Result.Hint := ColumnFieldName;
  Result.Stretch := true;
  Result.Center := true;
  Result.AutoSize := false;
  Result.Top := 1;
  Result.Left := 0;
end;

function TFrmQuickRep_PrintDBGrid.AddColumnRichText(ColumnFieldName: String): TQrRichText;
begin
  Result := TQrRichText.Create(QRRichTextControlsBand);
  Result.ParentReport := QuickRep1;
  Result.Parent := QRRichTextControlsBand;        // Will be moved on BeforePrint ...
  Result.Hint := ColumnFieldName;
  Result.Top := 1;
  Result.Left := 0;
end;

function TFrmQuickRep_PrintDBGrid.GraphicControlsBand_FindImage: TQRImage;
var
  i: Integer;
begin
  Result := Nil;

  for i := 0 to QRGraphicControlsBand.ControlCount-1 do
    if QRGraphicControlsBand.Controls[i] is TQRImage then
    begin
      Result := TQRImage( QRGraphicControlsBand.Controls[i] );
      Break;
    end;
end;

function TFrmQuickRep_PrintDBGrid.RichTextControlsBand_FindRichText: TQRRichText;
var
  i: Integer;
begin
  Result := Nil;

  for i := 0 to QRRichTextControlsBand.ControlCount-1 do
    if QRRichTextControlsBand.Controls[i] is TQRRichText then
    begin
      Result := TQRRichText( QRRichTextControlsBand.Controls[i] );
      Break;
    end;
end;

procedure TFrmQuickRep_PrintDBGrid.Get_Image_Parent_and_master(aQRImage: TQRImage; var RsltBand: TQRCustomBand; var RsltLeft, RsltWidth: Integer);
var
  i: Integer;
  QrDBText: TQrDBText;

        function Find_QRDBTextField(OnBand: TQRCustomBand; FieldName: String): TqrDBText;
        var
          i: Integer;
        begin
          Result := Nil;

          for i := 0 to OnBand.ControlCount-1 do
            if OnBand.Controls[i] is TQRDBText then
              with TQRDBText(OnBand.Controls[i]) do
                if TQRDBText(OnBand.Controls[i]).DataField = FieldName then
                begin
                  Result := TQRDBText(OnBand.Controls[i]);
                  Break;
                end;

        end;

        function FindColumnWidth(aFieldName: String): Integer;
        var
          i: Integer;
        begin
          Result := 100;

          for i := 0 to QRGridReport1.DBGrid.Columns.Count - 1 do
            if AnsiUppercase(aFieldName) = AnsiUppercase(QRGridReport1.DBGrid.Columns[i].FieldName) then
              Result := QRGridReport1.DBGrid.Columns[i].Width;
        end;

begin
  Rsltband := Nil;
  RsltLeft := 0;
  RsltWidth := 100;

  for i := 0 to QuickRep1.ControlCount-1 do
    if (QuickRep1.Controls[i] is TQRSubDetail) or (isDetailBand(QuickRep1.Controls[i])) then
    begin
      QrDBText := Find_QRDBTextField(TQRCustomBand(QuickRep1.Controls[i]), aQRImage.Hint);

      if QrDBText <> Nil then
      begin
        RsltBand   := TQRCustomBand(QuickRep1.Controls[i]);
        RsltLeft   := QrDBText.Left;
        RsltWidth  := QrDBText.Width;

        QrDBText.Free; // Remove component !
        Break;
      end;
    end;
end;

procedure TFrmQuickRep_PrintDBGrid.Get_RichText_Parent_and_master(aQRRichText: TQRRichText; var RsltBand: TQRCustomBand; var RsltLeft, RsltWidth: Integer);
var
  i: Integer;
  QrDBText: TQrDBText;

        function Find_QRDBTextField(OnBand: TQRCustomBand; FieldName: String): TqrDBText;
        var
          i: Integer;
        begin
          Result := Nil;

          for i := 0 to OnBand.ControlCount-1 do
            if OnBand.Controls[i] is TQRDBText then
              with TQRDBText(OnBand.Controls[i]) do
                if TQRDBText(OnBand.Controls[i]).DataField = FieldName then
                begin
                  Result := TQRDBText(OnBand.Controls[i]);
                  Break;
                end;

        end;

        function FindColumnWidth(aFieldName: String): Integer;
        var
          i: Integer;
        begin
          Result := 100;

          for i := 0 to QRGridReport1.DBGrid.Columns.Count - 1 do
            if AnsiUppercase(aFieldName) = AnsiUppercase(QRGridReport1.DBGrid.Columns[i].FieldName) then
              Result := QRGridReport1.DBGrid.Columns[i].Width;
        end;

begin
  Rsltband := Nil;
  RsltLeft := 0;
  RsltWidth := 100;

  for i := 0 to QuickRep1.ControlCount-1 do
    if (QuickRep1.Controls[i] is TQRSubDetail) or (isDetailBand(QuickRep1.Controls[i])) then
    begin
      QrDBText := Find_QRDBTextField(TQRCustomBand(QuickRep1.Controls[i]), aQRRichText.Hint);

      if QrDBText <> Nil then
      begin
        RsltBand   := TQRCustomBand(QuickRep1.Controls[i]);
        RsltLeft   := QrDBText.Left;
        RsltWidth  := QrDBText.Width;

        QrDBText.Free; // Remove component !
        Break;
      end;
    end;
end;

function TFrmQuickRep_PrintDBGrid.SummaryBand1_FindExpr: TQRExpr;
var
  i: Integer;
begin
  Result := Nil;

  for i := 0 to SummaryBand1.ControlCount-1 do
    if SummaryBand1.Controls[i] is TQRExpr then
      if TQRExpr(SummaryBand1.Controls[i]).Hint <> '' then
      begin
        Result := TQRExpr( SummaryBand1.Controls[i] );
        Break;
      end;
end;

procedure TFrmQuickRep_PrintDBGrid.Get_Expr_Parent_and_master(aQRExpr: TQRExpr; var RsltBand: TWinControl; var RsltMaster: TComponent; var RsltLeft, RsltWidth: Integer);
var
  i: Integer;
  QrDBText: TQrDBText;

        function Find_QRDBTextField(OnBand: TQRCustomBand; FieldName: String): TqrDBText;
        var
          i: Integer;
        begin
          Result := Nil;

          for i := 0 to OnBand.ControlCount-1 do
            if OnBand.Controls[i] is TQRDBText then
              with TQRDBText(OnBand.Controls[i]) do
                if TQRDBText(OnBand.Controls[i]).DataField = FieldName then
                begin
                  Result := TQRDBText(OnBand.Controls[i]);
                  Break;
                end;

        end;

begin
  Rsltband := Nil;
  RsltMaster := Nil;
  RsltLeft := 0;
  RsltWidth := 100;

  for i := 0 to QuickRep1.ControlCount-1 do
    if QuickRep1.Controls[i] is TQRSubDetail then   // Detailband handled below ...
    begin
      QrDBText := Find_QRDBTextField(TQRCustomBand(QuickRep1.Controls[i]), aQrExpr.Hint);

      if QrDBText <> Nil then
      begin
        RsltBand   := TQRSubDetail(QuickRep1.Controls[i]).Footerband;
        RsltMaster := TQRSubDetail(QuickRep1.Controls[i]);
        RsltLeft   := QrDBText.Left;
        RsltWidth  := QrDBText.Width;

        Break;
      end;
    end
    else
      if isDetailBand(QuickRep1.Controls[i]) then
      begin
        QrDBText := Find_QRDBTextField(TQRCustomBand(QuickRep1.Controls[i]), aQrExpr.Hint);

        if QrDBText <> Nil then
        begin
          RsltBand   := SummaryBand1;
          RsltMaster := QuickRep1; // Not working ... TQRCustomBand(QuickRep1.Controls[i]);
          RsltLeft   := QrDBText.Left;
          RsltWidth  := QrDBText.Width;

          Break;
        end;
      end;

end;

procedure TFrmQuickRep_PrintDBGrid.QuickRep1BeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
var
  i: Integer;
  WildImage: TQrImage;
  WildRichText: TQrRichText;
  SummaryExpr: TQRExpr;

  ColumnImage_Band: TQRCustomBand;
  ColumnImage_Left, ColumnImage_Width: Integer;

  ColumnRichText_Band: TQRCustomBand;
  ColumnRichText_Left, ColumnRichText_Width: Integer;

  ColumnSummary_Band: TWinControl;
  ColumnSummary_Master: TComponent;
  ColumnSummary_Left, ColumnSummary_Width: Integer;
begin
  // Move TQrImage components :
  while GraphicControlsBand_FindImage <> Nil do
  begin
    WildImage := GraphicControlsBand_FindImage;

    Get_Image_Parent_and_master(WildImage, ColumnImage_Band, ColumnImage_Left, ColumnImage_Width);

    if ColumnImage_Band <> Nil then
    begin
      ColumnImage_Band.BeforePrint := RecordBandBeforePrint;

      WildImage.Parent := ColumnImage_Band;
      WildImage.Left   := ColumnImage_Left;
      WildImage.Width  := ColumnImage_Width;
      WildImage.Height := ColumnImage_Band.Height - WildImage.Top * 2;

      // Already assigned ... if not Assigned(WildImage.Picture.Bitmap) then
      //  WildImage.Picture.Bitmap := TBitmap.Create;
      WildImage.Picture.Bitmap.Width  := WildImage.Width;
      WildImage.Picture.Bitmap.Height := WildImage.Height;
    end
    else
      Break;
  end;


  // Move TQrRichText components :
  while RichTextControlsBand_FindRichText <> Nil do
  begin
    WildRichText := RichTextControlsBand_FindRichText;

    Get_RichText_Parent_and_master(WildRichText, ColumnRichText_Band, ColumnRichText_Left, ColumnRichText_Width);

    if ColumnRichText_Band <> Nil then
    begin
      ColumnRichText_Band.BeforePrint := RecordBandBeforePrint;

      WildRichText.Parent := ColumnRichText_Band;
      WildRichText.Left   := ColumnRichText_Left;
      WildRichText.Width  := ColumnRichText_Width;
      WildRichText.Height := ColumnRichText_Band.Height - WildRichText.Top * 2;
    end
    else
      Break;
  end;


  if SummaryBand1.ControlCount <> 0 then
  begin
    // Important: if SpanPagesHorizontally = true, QuickRep1 could have several SubDetails !   if SpanPagesHorizontally = false, QuickRep1 has a Detail ...
    for i := 0 to QuickRep1.ControlCount-1 do
      if QuickRep1.Controls[i] is TQRSubDetail then   // SubDetail footer initialisation !
      begin
        with TQRSubDetail(QuickRep1.Controls[i]) do
        begin
          // Bands.HasHeader is false !
          BeforePrint := RecordBandBeforePrint;

          if not Bands.HasFooter then
          begin
            Bands.HasFooter := true;
            FooterBand.Height := SummaryBand1.Height;
          end;
        end;
      end;

    // Move TQRExpr components :
    while SummaryBand1_FindExpr <> Nil do
    begin
      SummaryExpr := SummaryBand1_FindExpr;

      Get_Expr_Parent_and_master(SummaryExpr, ColumnSummary_Band, ColumnSummary_Master, ColumnSummary_Left, ColumnSummary_Width);

      if ColumnSummary_Band <> Nil then
      begin
        SummaryExpr.Parent := ColumnSummary_Band;
        SummaryExpr.Master := ColumnSummary_Master;
        SummaryExpr.Left   := ColumnSummary_Left;
        SummaryExpr.Width  := ColumnSummary_Width;
        SummaryExpr.Hint   := ''; // Don' t handle anymore ...
      end
      else
        Break;
    end;
  end;

  SummaryBand1.Enabled := SummaryBand1.ControlCount <> 0;
end;

procedure TFrmQuickRep_PrintDBGrid.RecordBandBeforePrint(Sender: TQRCustomBand; var PrintBand: Boolean);
var
  i: Integer;
  aField: TField;
  aGraphic: TGraphic;
  QrImageRect: TRect;
  FieldContent: TFieldContentRendering;
begin
  for i := 0 to Sender.ControlCount-1 do
  begin
    if Sender.Controls[i] is TQRDBText then
      if TQRDBText(Sender.Controls[i]).DataField <> '' then
      begin
        aField := QRGridReport1.DBGrid.DataSource.DataSet.FindField(TQRDBText(Sender.Controls[i]).DataField);

        if Assigned(aField) then
        begin
          FieldContent := fcDefault;

          if Assigned(DBGridPrint.OnSetContentFieldRendering) then
            DBGridPrint.OnSetContentFieldRendering(DBGridPrint, 0, 0, Rect(0, 0, 0, 0), DBGridPrint.FindColumnByFieldName(aField.FieldName), [gdSelected], false, FieldContent);

          if FieldContent = fcWordwrap then
            with TQRDBText(Sender.Controls[i]) do
            begin
              WordWrap := true;
              Height := TQRCustomBand(Parent).Height - Top * 2;
              // AutoStretch := true;
            end;
        end;
      end;

    if Sender.Controls[i] is TQRImage then
    begin
      aField := QRGridReport1.DBGrid.DataSource.DataSet.FindField(TQRImage(Sender.Controls[i]).Hint);

      if Assigned(aField) then
        if aField is TBlobField then
        begin
          try
            with TQRImage(Sender.Controls[i]) do
            begin
              QrImageRect := Rect(0, 0, Picture.Bitmap.Width, Picture.Bitmap.Height);
              Picture.Bitmap.Canvas.Brush.Color := clWhite;
              Picture.Bitmap.Canvas.FillRect(QrImageRect);

              FieldContent := DefaultImageFieldContent;

              if Assigned(DBGridPrint.OnSetContentFieldRendering) then
                DBGridPrint.OnSetContentFieldRendering(DBGridPrint, 0, 0, Rect(0, 0, 0, 0), DBGridPrint.FindColumnByFieldName(aField.FieldName), [gdSelected], false, FieldContent);

              if FieldContent in [fcBitmap, fcIcon, fcJpeg, fcPng] then
              begin
                aGraphic := GetGraphicFromField(TBlobField(aField), GetGraphicClass(FieldContent));

                if Assigned(aGraphic) then
                begin
                  DrawGraphic(Picture.Bitmap.Canvas, QrImageRect, aGraphic, false, bgStretchProportional, bgCentered, 0, 0, 0, 0, 1, 1);
                  aGraphic.Free;
                end;
              end;
            end;
          except
          end;
        end;
    end;

    if Sender.Controls[i] is TQRRichText then
    begin
      aField := QRGridReport1.DBGrid.DataSource.DataSet.FindField(TQRRichText(Sender.Controls[i]).Hint);

      if Assigned(aField) then
        if aField is TMemoField then
        begin
          try
            TQRRichText(Sender.Controls[i]).Lines.Assign(TMemoField(aField));
          except
          end;
        end;
    end;
  end;

  if Assigned(fOnCustomRecordBandBeforePrint) then
    fOnCustomRecordBandBeforePrint(Sender, PrintBand);
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_Print;
begin
  QRGridReport1.PrintReport(QuickRep1);
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_PreviewModal;
begin
  QRGridReport1.PreviewReportModal(QuickRep1);
end;

procedure TFrmQuickRep_PrintDBGrid.QuickReport_ExportToPdf(Filename: String; CompressionOn: Boolean);
var
  aPdf: TQRPDFDocumentFilter;
begin
  QRGridReport1.CreateReportFromGrid;

  aPDF := TQRPDFDocumentFilter.Create(Filename);
  aPdf.CompressionOn := CompressionOn;
  aPdf.TextEncoding := QRPDFFilter1.TextEncoding;

  QuickRep1.ExportToFilter(aPdf);

  aPdf.Free;
end;

end.
