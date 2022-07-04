//******************************************************************************
//  Print dataset using TVolgaDbGrid properties with FastReport
//******************************************************************************
//  Используются свойства TVolgaColumn:
//
//  procedure Execute; - сформировать отчет и вывести Preview
//  property Dataset - источник данных
//  property DetailFont - фонт для детальной части отчета
//  property SumFields - список полей, которые надо суммировать
//  property Summary - текстовые строки, выводимые в концовке отчета
//  property Title - текстовые строки, выводимые в заголовке отчета
//  property TitleFont - фонт заголовка отчета
//  property DisablePrint - сделать невидимыми кнопки вывода на печать и сохранения отчета
//    (для использования в демо-версии программы)
//
//  В строках заголовка и концовки возможно следующее форматирование:
//  1.строка начинается с пробела - она будет центрироваться
//  2.строка начинается с +2 или +4 и т.п. - она будет выведена шрифтом на 2-4 пункта
//     крупнее заданного (DetailFont или TitleFont)
//  3.строка состоит из символа "-" - будет выведена гориз.линия на всю ширину листа
//
//  Вывод таблицы производится по умолчанию на портретный лист, но если ширина
//  колонок превышает ширину листа - то автоматически берется альбомный лист.
//  Не происходит переноса по ширине на след.лист, все колонки автоматически масштабируются,
//  чтобы занять всю ширину листа.
//  Итоговые суммы (если они есть) печатаются жирным детальным шрифтом.
//  (c) 2000-2003, Olga Vlasova       info@volgadb.com
//******************************************************************************

unit FRVGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  DB, Printers, FR_DSet, FR_DBSet, FR_Class, FR_View, VolDBGrid;

type
  TFRVGrid = class(TComponent)
  private
    { Private declarations }
    FDataSet: TDataset;
    FReport: TfrReport;
    FReportDataSet: TfrDBDataSet;
    FDetailFont: TFont;
    FTitleFont: TFont;
    FSumFields: TStringList;
    FSummary: TStrings;
    FTitle: TStrings;
    v: TfrView;
    b: TfrBandView;
    Page: TfrPage;
    c: TCanvas;
    cd: Boolean;
    widfld: array[0..50] of integer;    {ширины колонок}
    t, FontH, dh, MaxWid, plus: integer;
    sim: string[1];
    koef: Double;
    FDisablePrint: Boolean;
    FGrid: TVolgaDBGrid;
    procedure SetDetailFont(const Value: TFont);
    procedure SetSumFields(const Value: TStringList);
    procedure SetSummary(const Value: TStrings);
    procedure SetTitle(const Value: TStrings);
    procedure SetTitleFont(const Value: TFont);
    procedure WhatFormat(v: TfrMemoView; F: TField);
    procedure TitlePrepare;
    procedure PageHeaderPrepare;
    procedure ColumnHeaderPrepare;
    procedure DetailPrepare;
    procedure SummaryPrepare;
    procedure SetGrid(const Value: TVolgaDBGrid);
    procedure frReportUserFunction(const Name: string; p1, p2,
      p3: Variant; var Val: Variant);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    { Published declarations }
    property Grid: TVolgaDBGrid read FGrid write SetGrid;
    property DetailFont: TFont read FDetailFont write SetDetailFont;
    property SumFields: TStringList read FSumFields write SetSumFields;
    property Summary: TStrings read FSummary write SetSummary;
    property Title: TStrings read FTitle write SetTitle;
    property TitleFont: TFont read FTitleFont write SetTitleFont;
    property DisablePrint: Boolean read FDisablePrint write FDisablePrint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Volga', [TFRVGrid]);
end;

{ TFRVGrid }

constructor TFRVGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSumFields := TStringList.Create;
  FSumFields.Sorted := true;
  FTitle := TStringList.Create;
  FSummary := TStringList.Create;
  FTitleFont := TFont.Create;
  FTitleFont.Name := 'Times New Roman';
  FTitleFont.Charset := RUSSIAN_CHARSET;
  FTitleFont.Size := 10;
  FTitleFont.Style := [fsBold];
  FDetailFont := TFont.Create;
  FDetailFont.Name := 'Arial';
  FDetailFont.Charset := RUSSIAN_CHARSET;
  FDetailFont.Size := 8;
  FReport := TfrReport.Create(Self);
end;

destructor TFRVGrid.Destroy;
begin
  FReport.Free;
  FReport := nil;
  FTitleFont.Free;
  FDetailFont.Free;
  FSumFields.Free;
  FTitle.Free;
  FSummary.Free;
  inherited Destroy;
end;

procedure TFRVGrid.Execute;
var i: integer;
begin                                   {вывод репорта}
  if FDataSet = nil then Exit;

  if not FDataSet.Active then FDataset.Open;
  FReportDataSet := TfrDBDataSet.Create(Self);
  FReportDataSet.Name := 'FRVGridDBDataSet1';
  FReportDataSet.DataSet := FDataSet;

  {размер одной цифры шрифта при FDetailFont}
  c := TCanvas.Create;
  c.Handle := GetDC(0);
  c.Font := FDetailFont;
  FontH := c.TextHeight('0123456789');

  {рассчитываем колонки по гриду}
  MaxWid := 0;
  for i := 0 to FGrid.Columns.Count - 1 do
  begin
    if FGrid.Columns[i].Visible then
      widfld[i] := FGrid.Columns[i].Width
    else
      widfld[i] := 0;
    Inc(MaxWid, widfld[i]);             {ширина полей}
  end;
  c.Free;

  cd := FDataSet.ControlsDisabled;
  try
    FDataSet.DisableControls;
    FReport.Pages.Clear;
    FReport.Pages.Add;
    Page := FReport.Pages[0];
    Page.pgMargins := Rect(36, 36, 36, 36); //отступы от края листа по 1 см
    Page.UseMargins := true;

    {развернуть лист, если широкий репорт}
    if (MaxWid > Page.PrnInfo.Pw - 2 * 36) and (Page.pgOr = poPortrait) then
      with Page do
        ChangePaper(pgSize, pgWidth, pgHeight, -1, poLandscape);
    koef := (Page.PrnInfo.Pw - 2 * 36) / MaxWid; {коеф.пересчета}
    {пропорционально задать ширину колонок, чтобы все уместились на Bande}
    for i := 0 to FGrid.Columns.Count - 1 do
      widfld[i] := Round(widfld[i] * koef);

    // Title
    if FTitle.Count > 0 then TitlePrepare;
    // PageHeader
    PageHeaderPrepare;
    // Column Header
    ColumnHeaderPrepare;
    // Detail Band
    DetailPrepare;
    //Итоговые суммы и строки
    if (Summary.Count > 0) or (SumFields.Count > 0) then SummaryPrepare;

    //печать репорта
    if DisablePrint then
      FReport.PreviewButtons := FReport.PreviewButtons - [pbPrint, pbSave]
    else
      FReport.PreviewButtons := FReport.PreviewButtons + [pbPrint, pbSave];
    FReport.OnUserFunction := frReportUserFunction;
    FReport.ShowReport;
  finally
    if not cd then FDataSet.EnableControls;
    FReportDataSet.Free;
  end;
end;

procedure TFRVGrid.frReportUserFunction(const Name: string; p1, p2,
  p3: Variant; var Val: Variant);
var Col: TVolgaColumn;
  fv: Variant;
  ind: integer;
begin
  if (Name <> 'LOOKUP') and (Name <> 'COMBO') then Exit;
  fv := frParser.Calc(p1);
  Col := FGrid.Columns[p2];
  if Name = 'LOOKUP' then
  begin
    if Col.LookupLinkField = p3 then
      Val := fv
    else
      Val := Col.LookupDataSet.Lookup(Col.LookupLinkField, fv, p3);
  end
  else
  begin
    ind := Col.PickValues.IndexOf(fv);
    if ind >= 0 then
      Val := Col.PickList[ind]
    else
      Val := '';
  end;
end;

procedure TFRVGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Grid) then
    Grid := nil;
end;

procedure TFRVGrid.TitlePrepare;
var i: integer;
begin
  b := TfrBandView(frCreateObject(gtBand, ''));
  c := TCanvas.Create;
  c.Handle := GetDC(0);
  c.Font := FTitleFont;
  dh := c.TextHeight('0123456789');     //высота фонта заголовка
  c.Free;
  b.SetBounds(0, 36, 1000, dh * FTitle.Count + 10);
  b.BandType := btReportTitle;
  Page.Objects.Add(b);
  t := 36;                              //Top страницы
  for i := 0 to FTitle.Count - 1 do
  begin
    sim := Copy(FTitle[i], 1, 1);
    plus := 0;
    if sim = '-' then
    begin                               //гориз.линия
      //рисуем линию
      v := frCreateObject(gtLine, '');
      v.SetBounds(36, t, Page.PrnInfo.Pw - 36 * 2, 1);
      Page.Objects.Add(v);
      Inc(t, 2);
      Continue;
    end
    else if sim = '+' then
    begin                               {размер увеличения шрифта заголовка}
      try
        plus := StrToInt(Copy(FTitle[i], 2, 1));
      except plus := 0;
      end;
      sim := Copy(FTitle[i], 3, 1);     //смещаемся за значение увеличения шрифта
      FTitle[i] := Copy(FTitle[i], 3, length(FTitle[i]) - 2);
    end;
    //выводим текст
    v := frCreateObject(gtMemo, '');
    v.SetBounds(36, t, Page.PrnInfo.Pw - 36 * 2, dh + plus);  //при увеличении фонта растет высота
    if sim = ' ' then                   //центруем текст
      TfrMemoView(v).Alignment := frtaCenter + frtaMiddle
    else
      TfrMemoView(v).Alignment := frtaLeft + frtaMiddle;
    TfrMemoView(v).Font := FTitleFont;
    TfrMemoView(v).Font.Size := TfrMemoView(v).Font.Size + plus; {увеличенный шрифт}
    v.Memo.Add(FTitle[i]);              //печатаемый текст
    Page.Objects.Add(v);
    Inc(t, dh + plus);
  end;
end;

procedure TFRVGrid.PageHeaderPrepare;
begin
  b := TfrBandView(frCreateObject(gtBand, ''));
  b.BandType := btPageHeader;           //банд для заголовков строк
  b.SetBounds(0, 200, 1000, FontH + 5);
  b.Flags := b.Flags or flBandOnFirstPage; //печатать на первой странице
  Page.Objects.Add(b);
  t := 36;                              //левый край банда/листа
  v := frCreateObject(gtMemo, '');
  v.SetBounds(t, 200, Page.PrnInfo.Pw - 36 * 2, FontH);
  TfrMemoView(v).Alignment := frtaLeft + frtaMiddle;
  TfrMemoView(v).Font := FDetailFont;
  v.FrameWidth := 1;
  v.Memo.Add('Дата составления [DATE] [TIME]     Лист [PAGE#]');
  Page.Objects.Add(v);
end;

procedure TFRVGrid.ColumnHeaderPrepare;
var i: integer;
begin
  b := TfrBandView(frCreateObject(gtBand, ''));
  b.BandType := btColumnHeader;         //банд для заголовков строк
  b.SetBounds(0, 250, 1000, FontH);
  b.Flags := b.Flags or flStretched;    //растягиваемый
  Page.Objects.Add(b);
  t := 36;                              //левый край банда/листа
  for i := 0 to FGrid.Columns.Count - 1 do
    if widfld[i] > 0 then
    begin
      v := frCreateObject(gtMemo, '');
      v.SetBounds(t, 250, widfld[i], FontH);
      TfrMemoView(v).Alignment := frtaCenter + frtaMiddle;
      TfrMemoView(v).Font := FDetailFont;
      v.FrameTyp := 8 + 4 + 2;          //1-правая линия, 2-нижняя, 4-левая, 8-верхняя
      v.FrameWidth := 1;
      v.Memo.Add(FGrid.Columns[i].Title.Caption); //название колонки
      v.Flags := v.Flags or flStretched or flWordWrap or flWordBreak;  //растягиваемый  и перенос
      Page.Objects.Add(v);
      Inc(t, widfld[i]);
    end;
  TfrMemoView(Page.Objects[Page.Objects.Count - 1]).FrameTyp := 8 + 4 + 2 + 1;  //все рамки у последнего
end;

procedure TFRVGrid.DetailPrepare;
var i: integer;

  function IsLinkActive(Col: TVolgaColumn): Boolean;
  begin
    try
      Result := (Col.LookupDataSet <> nil)
        and Col.LookupDataSet.Active and (Col.LookupLinkField > '') and
        (Col.LookupDropDownFields > '') and (Col.LookupKeyField > '');
    except Result := false;
    end;
  end;

  function ViewField(names: string): string;
  begin
    if Pos(';', names) > 0 then         //видимое поле всегда первое из списка!
      Result := Copy(names, 1, Pos(';', names) - 1)
    else
      Result := names;
  end;

begin
  b := TfrBandView(frCreateObject(gtBand, ''));
  b.BandType := btMasterData;
  b.Dataset := FReportDataSet.Name;
  b.SetBounds(0, 300, 1000, FontH);
  b.Flags := b.Flags or flStretched;    //растягиваемый
  Page.Objects.Add(b);
  t := 36;                              //левый край банда/листа
  for i := 0 to FGrid.Columns.Count - 1 do
    if widfld[i] > 0 then
    begin
      v := frCreateObject(gtMemo, '');
      v.SetBounds(t, 300, widfld[i], FontH);
      TfrMemoView(v).Font := FDetailFont;
      v.FrameTyp := 4 + 2;              //1-правая линия, 2-нижняя, 4-левая, 8-верхняя
      v.FrameWidth := 1;
      case FGrid.Columns[i].ButtonStyle of
        cbsLookup:
          if IsLinkActive(FGrid.Columns[i]) then
            v.Memo.Add('[LOOKUP(["' + FGrid.Columns[i].FieldName + '"],' + IntToStr(i)
              + ',' + ViewField(FGrid.Columns[i].LookupDropDownFields) + ')]')
          else
            v.Memo.Add('["' + FGrid.Columns[i].FieldName + '"]'); //название поля
        cbsCombo:
          if FGrid.Columns[i].PickList.Count = FGrid.Columns[i].PickValues.Count then
            v.Memo.Add('[COMBO(["' + FGrid.Columns[i].FieldName + '"],' + IntToStr(i) +
              ')]')
          else
            v.Memo.Add('["' + FGrid.Columns[i].FieldName + '"]'); //название поля
        else
          v.Memo.Add('["' + FGrid.Columns[i].FieldName + '"]'); //название поля
      end;
      if (FGrid.Columns[i].Field.DataType = ftString) or (FGrid.Columns[i].Field.DataType
        =
        ftMemo) then
        v.Flags := v.Flags or flStretched or flWordWrap {or flWordBreak};  //растягиваемый  и перенос
      case FGrid.Columns[i].Alignment of
        taLeftJustify: TfrMemoView(v).Alignment := frtaLeft;
        taRightJustify: TfrMemoView(v).Alignment := frtaRight;
        taCenter: TfrMemoView(v).Alignment := frtaCenter;
      end;
      WhatFormat(TfrMemoView(v), FGrid.Columns[i].Field); //формат поля
      Page.Objects.Add(v);
      Inc(t, widfld[i]);
    end;
  TfrMemoView(Page.Objects[Page.Objects.Count - 1]).FrameTyp := 4 + 2 + 1;  //все рамки у последнего
end;

procedure TFRVGrid.SummaryPrepare;
var i, j: integer;
begin
  b := TfrBandView(frCreateObject(gtBand, ''));
  b.BandType := btReportSummary;
  b.SetBounds(0, 400, 1000, FontH + FontH * Summary.Count);
  Page.Objects.Add(b);
  t := 36;                              //левый край банда/листа
  if SumFields.Count > 0 then
  begin
    for i := 0 to FGrid.Columns.Count - 1 do
      if widfld[i] > 0 then
      begin
        v := frCreateObject(gtMemo, '');
        v.SetBounds(t, 400, widfld[i], FontH);
        TfrMemoView(v).Alignment := frtaRight + frtaMiddle; //суммы справа
        TfrMemoView(v).Font := FDetailFont;
        TfrMemoView(v).Font.Style := [fsBold];
        v.FrameTyp := 4 + 2;            //2-нижняя, 4-левая
        v.FrameWidth := 1;
        if SumFields.Find(FGrid.Columns[i].FieldName, j) then
        begin
          v.Memo.Add('[SUM("' + FGrid.Columns[i].FieldName + '")]'); //название поля
          WhatFormat(TfrMemoView(v), FGrid.Columns[i].Field); //формат поля
        end;
        Page.Objects.Add(v);
        Inc(t, widfld[i]);
      end;
    TfrMemoView(Page.Objects[Page.Objects.Count - 1]).FrameTyp := 4 + 2 + 1;  //все рамки у последнего
    t := 400 + FontH;
  end
  else
    t := 400;
  if FSummary.Count > 0 then
  begin
    for i := 0 to FSummary.Count - 1 do
    begin
      sim := Copy(FSummary[i], 1, 1);
      plus := 0;
      if sim = '-' then
      begin                             //гориз.линия
        //рисуем линию
        v := frCreateObject(gtLine, '');
        v.SetBounds(36, t, Page.PrnInfo.Pw - 36 * 2, 1);
        Page.Objects.Add(v);
        Inc(t, 2);
        Continue;
      end
      else if sim = '+' then
      begin                             {размер увеличения шрифта заголовка}
        try
          plus := StrToInt(Copy(FSummary[i], 2, 1));
        except plus := 0;
        end;
        sim := Copy(FSummary[i], 3, 1); //смещаемся за значение увеличения шрифта
        FSummary[i] := Copy(FSummary[i], 3, length(FSummary[i]) - 2);
      end;
          //выводим текст
      v := frCreateObject(gtMemo, '');
      v.SetBounds(36, t, Page.PrnInfo.Pw - 36 * 2, FontH + plus);  //при увеличении фонта растет высота
      if sim = ' ' then                 //центруем текст
        TfrMemoView(v).Alignment := frtaCenter + frtaMiddle
      else
        TfrMemoView(v).Alignment := frtaLeft + frtaMiddle;
      TfrMemoView(v).Font := FDetailFont;
      TfrMemoView(v).Font.Size := TfrMemoView(v).Font.Size + plus; {увеличенный шрифт}
      v.Memo.Add(FSummary[i]);          //печатаемый текст
      Page.Objects.Add(v);
      Inc(t, FontH + plus);
    end;
  end;
end;

procedure TFRVGrid.SetDetailFont(const Value: TFont);
begin
  if Value <> nil then
    FDetailFont.Assign(Value)
  else
  begin                                 {фонт по умолчанию}
    FDetailFont.Name := 'Arial';
    FDetailFont.Charset := RUSSIAN_CHARSET;
    FDetailFont.Size := 8;
  end;
end;

procedure TFRVGrid.SetSumFields(const Value: TStringList);
begin
  if Value <> nil then
    FSumFields.Assign(Value)
  else
    FSumFields.Clear;
end;

procedure TFRVGrid.SetSummary(const Value: TStrings);
begin
  if Value <> nil then
    FSummary.Assign(Value)
  else
    FSummary.Clear;
end;

procedure TFRVGrid.SetTitle(const Value: TStrings);
begin
  if Value <> nil then
    FTitle.Assign(Value)
  else
    FTitle.Clear;
end;

procedure TFRVGrid.SetTitleFont(const Value: TFont);
begin
  if Value <> nil then
    FTitleFont.Assign(Value)
  else
  begin                                 {фонт по умолчанию}
    FTitleFont.Name := 'Times New Roman';
    FTitleFont.Charset := RUSSIAN_CHARSET;
    FTitleFont.Size := 10;
    FTitleFont.Style := [fsBold];
  end;
end;

procedure TFRVGrid.WhatFormat(v: TfrMemoView; F: TField);
begin
  if F is TNumericField then
  begin
    v.Format := 1 * $01000000 + 4 * $00010000 + 3 * $00000100; //число произв.формата
    v.Format := v.Format + Ord(',');    //разделитель дроби - запятая
    if TNumericField(F).DisplayFormat > '' then
      v.FormatStr := TNumericField(F).DisplayFormat //берем формат из поля
    else
      v.FormatStr := '#.##';            //иначе задаем сами
  end
  else if F is TDateField then
  begin
    v.Format := 2 * $01000000 + 1 * $00010000; //дата формата дд.мм.гггг
  end;
end;

procedure TFRVGrid.SetGrid(const Value: TVolgaDBGrid);
begin
  if Value <> nil then
  begin
    FGrid := Value;
    if (FGrid.DataSource <> nil) and (FGrid.DataSource.DataSet <> nil) then
      FDataSet := FGrid.DataSource.DataSet
    else
      FDataSet := nil;
  end
  else
  begin
    FGrid := nil;
    FDataSet := nil;
  end;
end;

end.

