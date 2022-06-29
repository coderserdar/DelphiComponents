
{*****************************************}
{                                         }
{             FastReport v2.3             }
{         Print DBGrid component          }
{                                         }
{  FR_PGrid.pas:                          }
{  Copyright (c) 1999 by                  }
{  Butov Konstantin <kos@sp.iae.nsk.su>   }
{                                         }
{  FastReport:                            }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit FR_PGrid;

interface

{$I FR.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBGrids, Printers, FR_DSet, FR_DBSet, FR_Class;

type
  TfrPrintGrid = class(TComponent)
  private
    FDBGrid: TDBGrid;
    FReport: TfrReport;
    FReportDataSet: TfrDBDataSet;
    FColumnDataSet: TfrUserDataSet;
    FOrientation: TPrinterOrientation;
    FFont, FTitleFont: TFont;
    FCaption: String;
    FShowCaption: Boolean;
    FWidth: Integer;
    FDataSet: TDataset;
    procedure OnEnterRect(Memo: TStringList; View: TfrView);
    procedure OnPrintColumn(ColNo: Integer; var Width: Integer);
    function RealColumnIndex(Index: Integer; UseDD: Boolean): Integer;
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PreviewReport;
  published
    property DBGrid: TDBGrid read FDBGrid write FDBGrid;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property Font: TFont read FFont write FFont;
    property Caption: String read FCaption write FCaption;
    property ShowCaption: Boolean read FShowCaption write FShowCaption;
 end;


procedure Register;

implementation

type
  THackDBGrid = class(TDBGrid)
  end;

{ TfrPrintGrid }

constructor TfrPrintGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := TFont.Create;
  FFont.Name := 'Arial';
{$IFNDEF Delphi2}
  FFont.Charset := frCharset;
{$ENDIF}
  FFont.Size := 10;
  FTitleFont := TFont.Create;
  FTitleFont.Assign(FFont);
  FTitleFont.Style := [fsBold];
  FCaption := 'Grid';
  FShowCaption := True;
end;

destructor TfrPrintGrid.Destroy;
begin
  FFont.Free;
  FTitleFont.Free;
  inherited Destroy;
end;

procedure TfrPrintGrid.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = DBGrid) then
    DBGrid := nil;
end;

function TfrPrintGrid.RealColumnIndex(Index: Integer; UseDD: Boolean): Integer;
var
  Y, I, DD: Integer;
begin
  Result := 0;
  if (dgIndicator in DBGrid.Options) and UseDD then
    DD := 1 else
    DD := 0;
  Y := -1;
  for I := 0 to FDataSet.FieldCount - 1 do
    if FDataSet.Fields[I].Visible then
    begin
      Inc(Y);
      if Y = Index then
      begin
        Result := I + DD;
        break;
      end;
    end;
end;

procedure TfrPrintGrid.PreviewReport;
var
  v: TfrView;
  b: TfrBandView;
  Page: TfrPage;
begin
  if (FDBGrid = nil) or (DBGrid.Datasource = nil) or
     (DBGrid.Datasource.Dataset = nil) then Exit;

  FDataSet := DBGrid.Datasource.Dataset;

  FReport := TfrReport.Create(Self);
  FReport.OnEnterRect := OnEnterRect;
  FReport.OnPrintColumn := OnPrintColumn;

  FReportDataSet := TfrDBDataSet.Create(Self);
  FReportDataSet.Name := 'frGridDBDataSet1';
  FReportDataSet.DataSet := FDataSet;

  FColumnDataSet := TfrUserDataSet.Create(Self);
  FColumnDataSet.Name := 'frGridUserDataSet1';
  FColumnDataSet.RangeEnd := reCount;
  FColumnDataSet.RangeEndCount := FDataSet.FieldCount;

  try
    FReportDataSet.DataSource := DBGrid.DataSource;
    FReport.Pages.Add;
    Page := FReport.Pages[0];
    with Page do
      ChangePaper(pgSize, pgWidth, pgHeight, FOrientation);

    if FShowCaption then
    begin
      b := TfrBandView(frCreateObject(gtBand, ''));
      b.SetBounds(10, 20, 1000, 25);
      b.BandType := btReportTitle;
      Page.Objects.Add(b);
      v := frCreateObject(gtMemo, '');
      v.SetBounds(20, 20, Page.PrnInfo.PgW - 40, 25);
      TfrMemoView(v).Adjust:= frtaCenter;
      TfrMemoView(v).Font := FTitleFont;
      v.Memo.Add(FCaption);
      Page.Objects.Add(v);
    end;

    b := TfrBandView(frCreateObject(gtBand, ''));
    b.BandType := btMasterHeader;
    b.SetBounds(20, 60, 1000, 20);
    Page.Objects.Add(b);

    v := frCreateObject(gtMemo, '');
    v.SetBounds(20, 60, 20, 20);
    TfrMemoView(v).Adjust := frtaCenter;
    TfrMemoView(v).FillColor := clGray;
    TfrMemoView(v).Font := FTitleFont;
    TfrMemoView(v).FrameTyp := 15;
    v.Memo.Add('[Header]');
    Page.Objects.Add(v);

    b := TfrBandView(frCreateObject(gtBand, ''));
    b.BandType := btMasterData;
    b.Dataset := FReportDataSet.Name;
    b.SetBounds(0, 100, 1000, 18);
    Page.Objects.Add(b);

    b := TfrBandView(frCreateObject(gtBand, ''));
    b.BandType := btCrossData;
    b.Dataset := FColumnDataSet.Name;
    b.SetBounds(20, 0, 20, 1000);
    Page.Objects.Add(b);

    v := frCreateObject(gtMemo, '');
    v.SetBounds(20, 100, 20, 18);
    v.Memo.Add('[Cell]');
    TfrMemoView(v).FrameTyp := 15;
    Page.Objects.Add(v);

    FReport.ShowReport;
  finally
    FReport.Free;
    FReportDataSet.Free;
    FColumnDataSet.Free;
  end;
end;

procedure TfrPrintGrid.OnEnterRect(Memo: TStringList; View: TfrView);
begin
  if Memo[0] = '[Cell]' then
  begin
    Memo[0] := FDataSet.Fields[RealColumnIndex(FColumnDataSet.RecNo, False)].AsString;
    View.dx := FWidth;
    case FDataSet.Fields[RealColumnIndex(FColumnDataSet.RecNo, False)].Alignment of
      taLeftJustify : TfrMemoView(View).Adjust := frtaLeft;
      taRightJustify: TfrMemoView(View).Adjust := frtaRight;
      taCenter      : TfrMemoView(View).Adjust := frtaCenter;
    end;
  end;
  if Memo[0] = '[Header]' then
  begin
    Memo[0] := FDataSet.Fields[RealColumnIndex(FColumnDataSet.RecNo, False)].FieldName;
    View.dx := FWidth;
  end;
end;

procedure TfrPrintGrid.OnPrintColumn(ColNo: Integer; var Width: Integer);
begin
  Width := THackDBGrid(DBGrid).ColWidths[RealColumnIndex(ColNo - 1, True)];
  FWidth := Width;
end;


procedure Register;
begin
  RegisterComponents('ButSoft', [TfrPrintGrid]);
end;

end.
