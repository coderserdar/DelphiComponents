{======================================================================}
{ Based on DCubeHTMLProducer from Dmitry I. Kogtev, lmaster@inbox.ru   }
{======================================================================}

unit FxHtml;

interface

uses
  SysUtils, Classes, Graphics, Controls,
  FxDB, FxConsts;

type

  THTMLFontSize = 0..7;

  THTMLFont = class(TPersistent)
  protected
   FFace : string;
   FColor: TColor;
   FSize : THTMLFontSize;
  public
   constructor Create;
   function Content(const Text: string) : string;
   procedure Assign(Source:TPersistent);override;
  published
   property Face : string read FFace write FFace;
   property Color: TColor read FColor write FColor default clNone;
   property Size : THTMLFontSize read FSize write FSize default 0;
  end;

  THtmlHAlignment=(haDefault,haLeft,haCenter,haRight);
  THtmlVAlignment=(vaDefault,vaTop,vaMiddle,vaBottom,vaBaseline);

  THtmlCell=class(TPersistent)
  private
    FBgColor:TColor;
    FBorderColor:TColor;
    FFont:THtmlFont;
    FHAlignment:THtmlHAlignment;
    FVAlignment:THtmlVAlignment;
    FHeader:Boolean;
    FNoWrap:Boolean;
    FHeight:string;
    FWidth :string;
    procedure SetHeight(const Value: string);
    procedure SetWidth(const Value: string);
    procedure SetFont(const Value: THtmlFont);
  public
    constructor Create;
    destructor Destroy;override;
    function Content(const AText:string;const AParam:string=''):string;
    procedure Assign(Source:TPersistent);override;
  published
    property BgColor:TColor read FBgColor write FBgColor default clNone;
    property BorderColor:TColor read FBorderColor write FBorderColor default clNone;
    property Font:THtmlFont read FFont write SetFont;
    property Header:Boolean read FHeader write FHeader default False;
    property NoWrap:Boolean read FNoWrap write FNoWrap default False;
    property HAlignment:THtmlHAlignment read FHAlignment write FHAlignment default haDefault;
    property VAlignment:THtmlVAlignment read FVAlignment write FVAlignment default vaDefault;
    property Heigh:string read FHeight write SetHeight;
    property Width:string read FWidth write SetWidth;
  end;

  TFxHtml = class(TComponent)
  private
    FCellPadding: integer;
    FCellSpacing: integer;
    FBorderWidth: integer;
    FBorderColor: TColor;
    FDecisionSource : TFxSource;
    FTitle : string;
    FHeader : TStrings;
    FFooter : TStrings;
    FCellCaption: THtmlCell;
    FCellData: THtmlCell;
    FCellDataSum: THtmlCell;
    FCellEmpty: THtmlCell;
    FCellLabel: THtmlCell;
    FCellLabelSum: THtmlCell;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
    procedure SetCellCaption(const Value: THtmlCell);
    procedure SetCellData(const Value: THtmlCell);
    procedure SetCellLabel(const Value: THtmlCell);
    procedure SetCellDataSum(const Value: THtmlCell);
    procedure SetCellEmpty(const Value: THtmlCell);
    procedure SetCellLabelSum(const Value: THtmlCell);
    procedure SetDecisionSource(const Value: TFxSource);
  protected
    FTotals : boolean;
    function GetHeader : string;
    function GetRows : string;
    function HeaderTop:string;
    procedure Notification(AComponent:TComponent; Operation:TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function Content : string;
    function PageContent:string;
  published
    { Published declarations }
    property DecisionSource : TFxSource read FDecisionSource write SetDecisionSource;
    property BorderColor : TColor read FBorderColor write FBorderColor default clNone;
    property BorderWidth : integer read FBorderWidth write FBorderWidth default 1;
    property CellSpacing : integer read FCellSpacing write FCellSpacing default 0;
    property CellPadding : integer read FCellPadding write FCellPadding default 0;
    property CellCaption:THtmlCell read FCellCaption write SetCellCaption;
    property CellData:THtmlCell read FCellData write SetCellData;
    property CellEmpty:THtmlCell read FCellEmpty write SetCellEmpty;
    property CellLabel:THtmlCell read FCellLabel write SetCellLabel;
    property CellLabelSum:THtmlCell read FCellLabelSum write SetCellLabelSum;
    property CellDataSum:THtmlCell read FCellDataSum write SetCellDataSum;
    property Header : TStrings read FHeader write SetHeader;
    property Footer : TStrings read FFooter write SetFooter;
    property Title : string read FTitle write FTitle;
    property Totals : boolean read FTotals write FTotals default True;
  end;


implementation

const
  HAlignValue:array [THtmlHAlignment]of string=
    ('','"left"','"center"','"right"');
  VAlignValue:array [THtmlVAlignment]of string=
    ('','"Top"','"Middle"','"Bottom"','"Baseline"');

function Cat(const Str1,Str2:string):string;
begin
  if (Str1<>'') and (Str2<>'') then
    Result:=Str1+' '+Str2
  else
    Result:=Str1+Str2;
end;

function ColorValue(const AColor:TColor):string;
var
  Rd,Gr,Bl: byte;
  C:LongInt;
begin
  C:=ColorToRGB(AColor);
  Rd:=C and $000000FF;
  Gr:=(C and $0000FF00) shr 8;
  Bl:=(C and $00FF0000) shr 16;
  Result:=Format('"#%.2x%.2x%.2x"', [Rd, Gr, Bl]);
end;

function ColSpan(const I:Integer):string;
begin
  Result:='colspan='+IntToStr(I);
end;

function RowSpan(const I:Integer):string;
begin
  Result:='rowspan='+IntToStr(I);
end;

{ THTMLFont }

procedure THTMLFont.Assign(Source: TPersistent);
begin
  if Source is THtmlFont then begin
    FFace :=THtmlFont(Source).FFace;
    FColor:=THtmlFont(Source).FColor;
    FSize :=THtmlFont(Source).FSize;
  end;
  inherited;
end;

constructor THTMLFont.Create;
begin
  inherited Create;
  FColor:=clNone;
end;

function THTMLFont.Content(const Text:string): string;
begin
  Result:='<font ';
  Result:='';
  if FFace<>'' then
    Result:=Cat(Result,'face='+FFace);
  if FColor<>clNone then
    Result:=Cat(Result,'color='+ColorValue(FColor));
  if FSize>0 then
    Result:=Cat(Result,'size="'+IntToStr(FSize)+'"');
  if Result<>'' then
    Result:=Format('<font %s>%s</font>',[Result,Text])
  else
    Result:=Text;
end;

{TFxHTML}

constructor TFxHtml.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBorderColor:=clNone;
  FHeader:=TStringList.Create;
  FFooter:=TStringList.Create;
  FCellPadding:=0;
  FCellSpacing:=0;
  FBorderWidth:=1;
  FCellEmpty:=THtmlCell.Create;
  FCellCaption:=THtmlCell.Create;
  FCellData:=THtmlCell.Create;
  FCellDataSum:=THtmlCell.Create;
  FCellLabelSum:=THtmlCell.Create;
  FCellLabel:=THtmlCell.Create;
  FTotals:=True;
end;

destructor TFxHtml.Destroy;
begin
  FreeAndNil(FCellCaption);
  FreeAndNil(FCellDataSum);
  FreeAndNil(FCellData);
  FreeAndNil(FCellEmpty);
  FreeAndNil(FCellLabel);
  FreeAndNil(FCellLabelSum);
  FreeAndNil(FHeader);
  FreeAndNil(FFooter);
  inherited Destroy;
end;

function TFxHtml.Content : string;
begin
  Result:='';
  if Assigned(DecisionSource) then
  begin
    Result:=Cat('<table','border='+IntToStr(BorderWidth));
    Result:=Cat(Result,'cellspacing='+IntToStr(CellSpacing));
    Result:=Cat(Result,'cellpadding='+IntToStr(CellPadding));
    if FBorderColor<>clNone then
      Result:=Cat(Result,'bordercolor='+ColorValue(BorderColor));
    Result:=Result+'>';
    Result:=Result+GetHeader;
    Result:=Result+GetRows;
    Result:=Result+'</table>';
  end;
end;

function TFxHtml.PageContent: string;
begin
  Result:='';
  if Assigned(DecisionSource) then
  begin
    Result:=Result+'<html>';
    Result:=Result+'<head>';
    Result:=Result+'<meta http-equiv="Content-Type" content="text/html">';
    Result:=Result+'<meta name="GENERATOR" content="Decision Cube Filter by D. Kogtev">';
    Result:=Result+'<title>'+FTitle+'</title>';
    Result:=Result+'</head>';
    Result:=Result+'<body>';
    Result:=Result+Header.Text;
    Result:=Result+'<CENTER>'+Content+'</CENTER>';
    Result:=Result+Footer.Text;
    Result:=Result+'</body>';
    Result:=Result+'</html>';
  end;
end;

function TFxHtml.GetHeader : string;
var
  ColDimIndex, RowDimIndex: integer;
  DataColIndex : integer;
  isBreak, isSum : boolean;
  DataColInc : integer;
  iDim : integer;
  DimRange : TDimRange;
  ValIndex : integer;
  DimName : string;
  SumCount : integer;
  scDataColIndex : integer;
begin
  Result:=' ';
  with DecisionSource do begin
    if Pivot.ActiveColCount > 0 then
    begin
      Result:=Result+'<TR VALIGN="TOP">'+HeaderTop+'</TR>';
      for ColDimIndex:=0 to Pivot.ActiveColCount-1 do
      begin
        Result:=Result+'<TR VALIGN="TOP">';
        if Pivot.ActiveRowCount > 0 then
        begin
          if ColDimIndex=Pivot.ActiveColCount-1 then
            for RowDimIndex:=0 to Pivot.ActiveRowCount-1 do
            begin
              iDim:=Pivot.IndexInfo(dgRow, RowDimIndex, true).Dim;
              Result:=Result+CellCaption.Content(DecisionCube.Dimensions[iDim].Name);
            end
          else
            Result:=Result+CellEmpty.Content('',ColSpan(Pivot.ActiveRowCount));
        end;
        DataColIndex:=0;
        while  DataColIndex < nDataCols do
        begin
          DataColInc:=1;
          ValIndex:=GetValueIndex(dgCol,ColDimIndex,DataColIndex,isBreak,isSum);
          if isBreak and isSum then
          begin
            if Totals then
              Result:=Result+CellLabel.Content(STotalCaption);
          end
          else if isSum then
          begin
            if Totals then
              Result:=Result+CellLabelSum.Content('');
          end
          else if isBreak  then
          begin
            DimRange:=GetGroupExtent(dgCol, ColDimIndex, DataColIndex);
            DataColInc:=DimRange.Last+1-DimRange.First;
            iDim:=Pivot.IndexInfo(dgCol, ColDimIndex, true).Dim;
            SumCount:=0;
            if not Totals then begin
              for scDataColIndex:=DimRange.First to DimRange.Last do
              begin
                GetValueIndex(dgCol, Pivot.ActiveColCount-1,scDataColIndex,isBreak,isSum);
                if isSum then
                  inc(SumCount);
              end;
            end;
            Result:=Result+CellLabel.Content(GetMemberAsString(iDim, ValIndex),ColSpan(DataColInc-SumCount));
          end;
          inc(DataColIndex, DataColInc);
        end;
        Result:=Result+'</TR>';
      end;
    end;
    if (Pivot.ActiveRowCount>0) and (Pivot.ActiveColCount=0) then
    begin
      Result:=Result+'<TR VALIGN="TOP">';
      for RowDimIndex:=0 to Pivot.ActiveRowCount-1 do
      begin
        iDim:=Pivot.IndexInfo(dgRow, RowDimIndex, true).Dim;
        DimName:=GetDimensionName(iDim);
      end;
      Result:=Result+CellCaption.Content(DimName);
      Result:=Result+CellEmpty.Content('',ColSpan(NDataCols-Pivot.ActiveColCount));
      Result:=Result+'</TR>';
    end;
  end;
end;

function TFxHtml.GetRows : string;
var
  DataRowIdx, DataColIdx : integer;
  isBreak, isSum, isColSum : boolean;
  RowDimIndex : integer;
  ValIndex : integer;
  SubLevel : integer;
  iDim : integer;
  DimRange : TDimRange;
  scDataColIndex : integer;
  SumCount : integer;
  csIsSum : boolean;
begin
  Result:='';
  for DataRowIdx:=0 to DecisionSource.nDataRows-1 do
   begin
    if DecisionSource.Pivot.ActiveRowCount>0 then
      DecisionSource.GetValueIndex(dgRow, DecisionSource.Pivot.ActiveRowCount-1,DataRowIdx,isBreak,isSum)
    else
      isSum:=false;
    if Totals or not isSum then
    begin
      Result:=Result+'<TR VALIGN="TOP">';
      isSum:=false;
      if DecisionSource.Pivot.ActiveRowCount>0 then
      begin
        for RowDimIndex:=0 to DecisionSource.Pivot.ActiveRowCount-1 do
        begin
          ValIndex:=DecisionSource.GetValueIndex(dgRow, RowDimIndex,DataRowIdx,isBreak,isSum);
          if isBreak and isSum then
            Result:=Result+CellLabelSum.Content(STotalCaption)
          else if isSum then
            Result:=Result+CellLabelSum.Content('')
          else if isBreak then begin
            DimRange:=DecisionSource.GetGroupExtent(dgRow, RowDimIndex, DataRowIdx);
            iDim:=DecisionSource.Pivot.IndexInfo(dgRow, RowDimIndex, true).Dim;
            if Totals then
              Result:=Result+CellLabel.Content(DecisionSource.GetMemberAsString(iDim, ValIndex),RowSpan(DimRange.Last+1-DimRange.First))
            else begin
              SumCount:=0;
              for scDataColIndex:=DimRange.First to DimRange.Last do
              begin
                DecisionSource.GetValueIndex(dgRow, DecisionSource.Pivot.ActiveRowCount-1,scDataColIndex,isBreak,csIsSum);
                if csIsSum then
                  inc(SumCount);
              end;
              if (DimRange.Last+1-DimRange.First-SumCount)>0 then
                Result:=Result+CellLabel.Content(DecisionSource.GetMemberAsString(iDim, ValIndex),RowSpan(DimRange.Last+1-DimRange.First-SumCount));
            end;
          end;
        end;
      end;
      for DataColIdx:=0 to DecisionSource.nDataCols-1 do begin
        isColSum:=false;
        if DecisionSource.Pivot.ActiveColCount>0 then
          DecisionSource.GetValueIndex(dgCol, DecisionSource.Pivot.ActiveColCount-1, DataColIdx,isBreak,isColSum);
        if isSum or isColSum then begin
          if Totals then
            Result:=Result+CellDataSum.Content(DecisionSource.GetDataAsString(DataRowIdx, DataColIdx, subLevel));
        end else
          Result:=Result+CellData.Content(DecisionSource.GetDataAsString(DataRowIdx, DataColIdx, subLevel));
      end;
      Result:=Result+'</TR>';
    end;
   end;
end;

procedure TFxHtml.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TFxHtml.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

function TFxHtml.HeaderTop: string;
var
  I,iDim,SumCount:Integer;
  IsBreak,IsSum:Boolean;
begin
  with DecisionSource do begin
    if Pivot.ActiveRowCount > 0 then
      Result:=Result+CellEmpty.Content('',ColSpan(Pivot.ActiveRowCount));
    for I:=0 to Pivot.ActiveColCount-1 do
    begin
      iDim:=Pivot.IndexInfo(dgCol,I,True).Dim;
      Result:=Result+CellCaption.Content(DecisionCube.Dimensions[iDim].Name);
    end;
    if Totals then
      Result:=Result+CellEmpty.Content('',ColSpan(NDataCols-Pivot.ActiveColCount))
    else begin
      SumCount:=0;
      if Pivot.ActiveColCount>0 then begin
        for I:=0 to NDataCols-1 do begin
          GetValueIndex(dgCol, Pivot.ActiveColCount-1,I,isBreak,isSum);
          if isSum then
            Inc(SumCount);
        end;
      end;
      if (NDataCols-Pivot.ActiveColCount-SumCount)>0 then
        Result:=Result+CellEmpty.Content('',ColSpan(NDataCols-Pivot.ActiveColCount-SumCount));
    end;
  end;
end;

procedure TFxHtml.SetCellCaption(const Value: THtmlCell);
begin
  FCellCaption.Assign(Value);
end;

procedure TFxHtml.SetCellData(const Value: THtmlCell);
begin
  FCellData.Assign(Value);
end;

procedure TFxHtml.SetCellLabel(const Value: THtmlCell);
begin
  FCellLabel.Assign(Value);
end;

procedure TFxHtml.SetCellDataSum(const Value: THtmlCell);
begin
  FCellDataSum.Assign(Value);
end;

procedure TFxHtml.SetCellEmpty(const Value: THtmlCell);
begin
  FCellEmpty.Assign(Value);
end;

procedure TFxHtml.SetCellLabelSum(const Value: THtmlCell);
begin
  FCellLabelSum.Assign(Value);
end;

procedure TFxHtml.Notification(AComponent:TComponent; Operation:TOperation);
begin
  inherited Notification(AComponent,Operation);
  if (AComponent=FDecisionSource)and(Operation=opRemove) then
    SetDecisionSource(nil);
end;

procedure TFxHtml.SetDecisionSource(const Value: TFxSource);
begin
  if Value<>FDecisionSource then begin
    if Value<>nil then
      Value.FreeNotification(Self);
    if FDecisionSource<>nil then
      FDecisionSource.RemoveFreeNotification(Self);
    FDecisionSource := Value;
  end;
end;

{ THtmlCell }

procedure THtmlCell.Assign(Source: TPersistent);
begin
  if Source is THtmlCell then begin
    FBgColor:=THtmlCell(Source).FBgColor;
    FBorderColor:=THtmlCell(Source).FBorderColor;
    FFont.Assign(THtmlCell(Source).FFont);
    FHeader:=THtmlCell(Source).FHeader;
    FNoWrap:=THtmlCell(Source).FNoWrap;
    FHAlignment:=THtmlCell(Source).FHAlignment;
    FHeight:=THtmlCell(Source).FHeight;
    FVAlignment:=THtmlCell(Source).FVAlignment;
    FWidth:=THtmlCell(Source).FWidth;
  end;
  inherited;
end;

function THtmlCell.Content(const AText:string; const AParam:string=''): string;
var
  Cell:string;
begin
  Result:=Font.Content(AText);
  if Result='' then Result:='&nbsp';
  case Header of
  False: Cell:=Cat('<td',AParam);
  True : Cell:=Cat('<th',AParam);
  end;
  if FBgColor<>clNone then
    Cell:=Cat(Cell,'bgcolor='+ColorValue(FBgColor));
  if FBorderColor<>clNone then
    Cell:=Cat(Cell,'bordercolor='+ColorValue(FBorderColor));
  if FHAlignment<>haDefault then
    Cell:=Cat(Cell,'align='+HAlignValue[FHAlignment]);
  if FVAlignment<>vaDefault then
    Cell:=Cat(Cell,'valign='+VAlignValue[FVAlignment]);
  if FNoWrap then
    Cell:=Cat(Cell,'nowrap');
  if FHeight<>'' then
    Cell:=Cat(Cell,'height="'+FHeight+'"');
  if FWidth<>'' then
    Cell:=Cat(Cell,'width="'+FWidth+'"');
  case Header of
  False: Result:=Format('%s>%s</td>',[Cell,Result]);
  True : Result:=Format('%s>%s</th>',[Cell,Result]);
  end;
end;

constructor THtmlCell.Create;
begin
  inherited Create;
  FFont:=THtmlFont.Create;
  FBgColor:=clNone;
  FBorderColor:=clNone;
  FHAlignment:=haDefault;
  FVAlignment:=vaDefault;
end;

destructor THtmlCell.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure THtmlCell.SetFont(const Value: THtmlFont);
begin
  FFont.Assign(Value);
end;

procedure THtmlCell.SetHeight(const Value: string);
begin
  if Value<>'' then StrToInt(Value);
  FHeight:= Value;
end;

procedure THtmlCell.SetWidth(const Value: string);
begin
  if Value<>'' then StrToInt(Value);
  FWidth := Value;
end;

initialization
  StartClassGroup(TControl);
  GroupDescendentsWith(TFxHtml, TControl);
  RegisterClass(TFxHtml);
end.
