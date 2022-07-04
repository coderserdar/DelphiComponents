///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMWorksheet;

{$B-}

interface

uses
  Windows, SysUtils, Classes, Controls, Graphics, Grids, DMContainer;

type
  TGetCellTextEvent = function (Sender: TObject; Data: TData; Index: integer): string of object;
  TWorksheet = class (TDrawGrid)
  private
    FContainer: TContainer;
    FBlockColorF: TColor;
    FBlockColorB: TColor;
    FAlignRight: Boolean;
    FDrawHeaders: Boolean;
    FHeader: TStrings;
    FOnGetCellText: TGetCellTextEvent;
    procedure SetContainer(C: TContainer);
    procedure SetBlockColorF(C: TColor);
    procedure SetBlockColorB(C: TColor);
    procedure SetAlignRight(B: Boolean);
    procedure SetDrawHeaders(B: Boolean);
    procedure SetHeader(const Value: TStrings);
    procedure StringsChanged(Sender: TObject);
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    procedure MouseDown(Btn: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    function GetEditText(ACol, ARow: Longint): string; override;
    procedure SetEditText(ACol, ARow: Longint; const Value: string); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AlignTextOut(ARect: TRect; ss: string; HTML: boolean=false);
    procedure UpdateSize;
    procedure CopyToClipBoard(UseTabs: boolean=false);
    procedure PasteFromClipBoard(InsertLines,Overwrite: boolean);
    procedure Delete;
    procedure SelectAll;
    procedure DeleteCell;
    procedure DeleteLine;
    procedure InsertLine;
  published
    property Container: TContainer read FContainer write SetContainer;
    property BlockColorF: TColor read FBlockColorF write SetBlockColorF;
    property BlockColorB: TColor read FBlockColorB write SetBlockColorB;
    property AlignRight: Boolean read FAlignRight write SetAlignRight;
    property DrawHeaders: Boolean read FDrawHeaders write SetDrawHeaders;
    property Header: TStrings read FHeader write SetHeader;
    property OnGetCellText: TGetCellTextEvent read FOnGetCellText write FOnGetCellText;
  end;
  
procedure Register;

implementation

uses Forms, ClipBrd, Math, DMHTMLText;

procedure Register;
begin
  RegisterComponents('DM2003', [TWorksheet]);
end;

function WordStr(S: string; N: integer): string;
var I, J: integer; Ss: string;
begin
  I:=1;
  J:=0;
  SS:='';
  while I<=length(S) do
  begin
    SS:='';
    case S[I] of
    ' ', #9:
      Inc(I);
    '|' :
      begin
        Inc(I);
        if I>length(S) then Break;
        SS:='';
        while (I<=length(S)) and (S[I]<>'|') do
        begin Ss:=Ss+S[I]; Inc(I); end;
        Inc(J); Inc(I); if J=N then Break;
      end;
    else
      begin
        while (I<=length(S)) and (S[I]<>' ') and (S[I]<>#9) do
        begin Ss:=Ss+S[I]; Inc(I); end;
        Inc(J); if J=N then Break;
      end
    end;
  end;
  if J=N then WordStr:=SS else WordStr:='';
end;

function NumWords(S: string): integer;
var I, J: integer;
begin
  I:=1; J:=0;
  while I<=length(S) do
  begin
    case S[I] of
    ' ', #9:
      Inc(I);
    '|' :
      begin
        Inc(I);
        while (I<=length(S)) and (S[I]<>'|') do Inc(I);
        Inc(J);  Inc(I);
      end;
    else
      begin
        while (I<=length(S)) and (S[I]<>' ') and (S[I]<>#9) do
        Inc(I);
        Inc(J);
      end
    end;
  end;
  NumWords:=J;
end;

{---------- Class: TWorksheet ----------}
procedure TWorksheet.AlignTextOut(ARect: TRect; ss: string; HTML: boolean=false);
var
  X,Y,W,H: Integer;
begin
  if ss='' then Exit;
  X:=ARect.Left+GridLineWidth-Font.Height div 2;
  Y:=ARect.Top+(RowHeights[Row]+Font.Height) div 2;
  if AlignRight then
  begin
    if HTML then
    begin
      HTMLCalculateSize(Canvas, ss, W, H);
      Canvas.Font:=Font; // restore font after HTMLCalculateSize
      Y:=ARect.Top+(RowHeights[Row]-H) div 2; // correct Y in HTML mode
    end else
      W:=Canvas.TextWidth(ss);
    if ARect.Right-X-W>0
    then X:=ARect.Right-X-W+ARect.Left;
  end;
  if HTML
  then HTMLTextOut(Canvas, ss, X, Y, false)
  else Canvas.TextOut(X, Y, ss);
end;

procedure TWorksheet.CopyToClipBoard(UseTabs: boolean=false);
var
  D: TData;
  S,Ss: string;
  SL: TStringList;
  LC,CC,Btm,L,R,N,I: Integer;
  Data: Pointer;
  HData: THandle;
  MS: TMemoryStream;
  X: TReal;
begin
  if not Assigned(FContainer) or (FContainer.Items.Count<1) then Exit;
  Btm:=Selection.Bottom;
  if Btm=RowCount-1
  then Btm:=RowCount-2;
  SL:=TStringList.Create;
  MS:=TMemoryStream.Create;
  Screen.Cursor:=crHourGlass;
  try
    LC:=0;
    MS.Write(LC, SizeOf(LC));
    for LC:=Selection.Top to Btm do
    begin
      S:='';
      if Btm>Selection.Top then
      FContainer.ShowProgress(Round((LC-Selection.Top)*100
        /(Btm-Selection.Top)));
      D:=FContainer.Items[LC-FixedRows];
      if D is TRealData then
      begin
        L:=Selection.Left-FixedCols+1;
        R:=Selection.Right-FixedCols+1;
        if (D as TRealData).Size>R
        then N:=R
        else N:=(D as TRealData).Size;
        I:=N-L+1;
        MS.Write(I, SizeOf(I));
        for CC:=L to N do
        begin
          X:=(D as TRealData).RData[CC];
          MS.Write(X, SizeOf(X));
        end;
        integer(MS.Memory^):=integer(MS.Memory^)+1;
      end;
      for CC:=Selection.Left to Selection.Right do
      begin
        if D is TRealData then
          if Assigned(FOnGetCellText)
          then SS:=FOnGetCellText(Self, D, CC-FixedCols+1)
          else SS:=(D as TRealData).GetItemText(CC-FixedCols+1);
        if not (D is TRealData) then
        begin // Warning! selection columns and OnGetCellText not used!
          S:=D.Data;
          Break;
        end;
        if SS<>'' then if S<>'' then
          if UseTabs then S:=S+#9+Ss else S:=S+' '+Ss else S:=SS;
      end;
      SL.Add(S);
    end;
    ClipBoard.Open;
    try
      ClipBoard.AsText:=SL.Text;
      if integer(MS.Memory^)>0 then
      begin
        HData:=GlobalAlloc(GMEM_MOVEABLE+GMEM_DDESHARE, MS.Size);
        try
          Data:=GlobalLock(HData);
          try
            Move(MS.Memory^, Data^, MS.Size);
            SetClipboardData(TRealData.GetClipboardFormat, HData);
          finally
            GlobalUnlock(HData);
          end;
        except
          GlobalFree(HData); raise;
        end;
      end;
    finally
      ClipBoard.Close;
    end;
  finally
    MS.Free;
    SL.Free;
    Screen.Cursor:=crDefault;
  end;
end;

constructor TWorksheet.Create(AOwner: TComponent);
begin
  inherited;
  Options:=[goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goThumbTracking, goColSizing, goRangeSelect];
  FBlockColorF:=clYellow;
  FBlockColorB:=Color;
  DefaultDrawing:=true;
  FAlignRight:=false;
  DrawHeaders:=false;
  FHeader:=TStringList.Create;
  TStringList(FHeader).OnChange:=StringsChanged;
end;

procedure TWorksheet.Delete;
var
  BegItem,EndItem,Item,N,J,SL,SR,SR1: Integer;
  s1,s2,s3: string;
  D: TData;
begin
  if not Assigned(FContainer) or (FContainer.Items.Count<1)
  then Exit;
  Screen.Cursor:=crHourGlass;
  try
    BegItem:=Selection.Top-FixedRows;
    EndItem:=Selection.Bottom-FixedRows;
    SL:=Selection.Left-FixedCols+1;
    SR:=Selection.Right-FixedCols+1;
    Item:=BegItem;
    if EndItem>FContainer.Items.Count-1
    then EndItem:=FContainer.Items.Count-1;
    while Item<=EndItem do
    begin
      D:=Container.Items[Item];
      if D is TRealData
      then N:=(D as TRealData).Size
      else N:=NumWords(D.Data);
      if (SL=1) and (SR>=N) then // all item data selected => delete whole item
      begin
        Container.Items.Delete(Item);
        Dec(EndItem);
      end else
      begin
        if D is TRealData then
        begin {real data:}
          if SR>N then SR1:=N else SR1:=SR;
          for J:=1 to SR1-SL+1 do
          (D as TRealData).DelItem(SL);
        end else
        begin {string:}
          s1:=D.Data;
          s2:='';
          s3:='';
          for J:=1 to SL-1 do
          s2:=s2+' '+WordStr(s1, J);
          for J:=SR+1 to N do
          s3:=s3+' '+WordStr(s1, J);
          D.Data:=s2+' '+s3;
        end;
        Inc(Item);
      end;
    end;
    UpdateSize;
    LeftCol:=FixedCols;
  finally
    Screen.Cursor:=crDefault;
    FContainer.Modified:=true;
  end;
end;

destructor TWorksheet.Destroy;
begin
  if Assigned(FHeader) then FHeader.Free;
  inherited;
end;

procedure TWorksheet.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  S: string;
  D: TData;
begin
  if (ACol<FixedCols) or (ARow<FixedRows)
  then
    if FDrawHeaders then
    begin
      if (ARow=0) and (ACol>=FixedCols)
        and (ACol-FixedCols<FHeader.Count) then
      AlignTextOut(ARect, FHeader[ACol-FixedCols], true);
      if (Acol=0) and (Arow=0) then
      begin
        Canvas.Brush.Color:=FBlockColorB;
        ARect.Left:=ARect.Left+2;
        ARect.Right:=ARect.Right-2;
        ARect.Top:=ARect.Top+2;
        ARect.Bottom:=ARect.Bottom-2;
        Canvas.FillRect(ARect);
        Exit;
      end;
      if (ACol=0) and (ARow>=FixedRows) then
      AlignTextOut(ARect,IntToStr(ARow-FixedRows));
      if (ACol>0) and (ARow>0) then
      inherited DrawCell(ACol, ARow, ARect, AState);
    end
    else inherited DrawCell(ACol, ARow, ARect, AState)
  else
  begin
    if (not Assigned(FContainer)) or (ARow>=FixedRows+FContainer.Items.Count) then
    begin
      inherited DrawCell(ACol, ARow, ARect, AState);
      Exit;
    end;
    D:=FContainer.Items[ARow-FixedRows];
    ACol:=ACol-FixedCols;
    if gdSelected in AState then
    begin
      Canvas.Brush.Color:=FBlockColorB; Canvas.FillRect(ARect);
      Canvas.Font.Color:=FBlockColorF;
    end;
    if Assigned(FOnGetCellText)
    then
      S:=FOnGetCellText(Self, D, ACol+1)
    else
    begin
      if (D is TRealData) and (ACol<MaxCols) then
      S:=(D as TRealData).GetItemText(ACol+1);
      if not (D is TRealData)
      then S:=WordStr(D.Data, ACol+1);
    end;
    ACol:=ACol+FixedCols;
    inherited DrawCell(ACol, ARow, ARect, AState); // call OnDrawCell event
    AlignTextOut(ARect, S);
    if gdSelected in AState // restore color for focus frame
    then Canvas.Font.Color:=FBlockColorF;
  end;
end;

function TWorksheet.GetEditText(ACol, ARow: Longint): string;
var
  D: TData;
begin
  if not Assigned(FContainer) or (ARow-FixedRows>FContainer.Items.Count-1)
  then Result:=inherited GetEditText(ACol, ARow) else
  begin
    D:=FContainer.Items[ARow-FixedRows];
    if not (D is TRealData)
    then Result:=inherited GetEditText(ACol, ARow);
    ACol:=ACol-FixedCols;
    if D is TRealData
    then Result:=(D as TRealData).GetItemText(ACol+1);
  end;
end;

procedure TWorksheet.KeyDown(var Key: Word; Shift: TShiftState);
begin
  {$ifdef InPlaceHotKeys}
  if (Key=vk_Insert) and (ssShift in Shift)
  then InsertLine;
  if (Key=vk_Delete) and (ssShift in Shift)
  then DeleteLine;
  if (Key=vk_Delete) and (ssAlt in Shift)
  then DeleteCell;
  {$endif}
  inherited KeyDown(Key, Shift);
end;

procedure TWorksheet.MouseDown(Btn: TMouseButton; Shift: TShiftState; X,Y: Integer);
var
  Col,Row: LongInt;
begin
  inherited;
  if (not FDrawHeaders) or (not Assigned(FContainer)) or (FixedCols=0)
     or (FixedRows=0)
  then Exit;
  MouseToCell(X, Y, Col, Row);
  if (Col=0) and (Row=0)
  then SelectAll;
end;

procedure TWorksheet.PasteFromClipBoard(InsertLines,Overwrite: boolean);
var
  H: THandle;
  Buf: PChar;
  S: TStringList;
  I,J,N,N1,SL,NN,Mn,Row,Col,RowC: Integer;
  D: TData;
  s1,s2,s3: string;
  R, R1: TRealArray;
  MS: TMemoryStream;
begin
  {$ifndef PasteRowCol}
  Row:=Selection.Top;
  Col:=Selection.Left;
  {$else}
  Row:=Self.Row;
  Col:=Self.Col;
  {$endif}
  if Clipboard.HasFormat(TRealData.GetClipboardFormat)
    and (Container.DataType=dtRealData) then
  begin
    Screen.Cursor:=crHourGlass;
    Clipboard.Open;
    try
      H:=GetClipboardData(TRealData.GetClipboardFormat);
      if H=0 then Exit;
      Buf:=GlobalLock(H);
      if Buf=nil then Exit;
      try
        MS:=TMemoryStream.Create;
        try
          MS.WriteBuffer(Buf^, GlobalSize(H));
          MS.Position:=0;
          MS.Read(NN, SizeOf(NN));
          RowC:=RowCount; // use buffer to avoid repaint
          if NN>0 then
          try
            for I:=0 to NN-1 do
            begin
              if NN>1 then Container.ShowProgress(Round(I/(NN-1)*100));
              MS.Read(N, SizeOf(N));
              for J:=1 to N do
              MS.Read(R[J], SizeOf(R[J]));
              if InsertLines then
              begin
                D:=Container.InitItem;
                (D as TRealData).SetRData(N,R);
                if Row-FixedRows<Container.Items.Count
                then Container.Items.Insert(Row+I-FixedRows, D)
                else Container.Items.Add(D);
                Inc(RowC);
              end else
              if (Row-FixedRows<Container.Items.Count) and
                 (I<Container.Items.Count-Row+FixedRows) then
              begin
                D:=FContainer.Items[Row-FixedRows+I];
                SL:=Col-FixedCols;
                N1:=(D as TRealData).GetRData(R1);
                if Overwrite then
                begin
                  Mn:=Min(N, MaxCols-SL);
                  for J:=1 to Mn do
                  R1[J+SL]:=R[J];
                  (D as TRealData).SetRData(Max(Mn+SL, N1), R1);
                end else
                begin
                  for J:=Min(N1, MaxCols-N-SL) downto SL+1 do
                  R1[J+N]:=R1[J];
                  for J:=1 to Min(N, MaxCols-SL) do
                  R1[J+SL]:=R[J];
                  (D as TRealData).SetRData(Min(N+N1, MaxCols), R1);
                end
              end else
              begin
                D:=FContainer.InitItem;
                (D as TRealData).SetRData(N, R);
                FContainer.Items.Add(D);
                Inc(RowC);
              end;
            end;
          finally
            if RowC<>RowCount
            then RowCount:=RowC; // actual update
            ReFresh;
            Container.Modified:=true;
          end;
        finally
          MS.Free;
        end;
      finally
        GlobalUnlock(H);
      end;
    finally
      Clipboard.Close;
      Screen.Cursor:=crDefault;
    end;
    Exit;
  end;
  if not Clipboard.HasFormat(CF_TEXT) then Exit;
  Clipboard.Open;
  Screen.Cursor:=crHourGlass;
  H:=GetClipboardData(CF_TEXT);
  Buf:=GlobalLock(H);
  S:=TStringList.Create;
  RowC:=RowCount;
  try
    S.SetText(Buf);
    for I:=0 to S.Count-1 do
    begin
      if S.Count>1
      then FContainer.ShowProgress(Round(I/(S.Count-1)*100));
      if InsertLines then
      begin
        D:=FContainer.InitItem;
        D.Data:=S[I];
        if Row-FixedRows<FContainer.Items.Count
        then FContainer.Items.Insert(Row+I-FixedRows, D)
        else FContainer.Items.Add(D);
        Inc(RowC);
      end else
      if (Row-FixedRows<FContainer.Items.Count) and
         (I<FContainer.Items.Count-Row+FixedRows) then
      begin
        D:=FContainer.Items[Row-FixedRows+I];
        SL:=Col-FixedCols;
        if D is TRealData then
        begin
          N:=Str2Real(S[I], R1);
          if ((not Overwrite) and ((D as TRealData).Size+N>MaxCols)) or
             (Overwrite and (SL+N>MaxCols))
          then raise ERealDataError.Create(strInsertItem);
          N1:=(D as TRealData).GetRData(R);
          for J:=1 to N do
          R[SL+J]:=R1[J];
          if not Overwrite then
          for J:=SL+1 to N1 do
          R[J+N]:=(D as TRealData).RData[J];
          if Overwrite
          then (D as TRealData).SetRData(Max(SL+N, N1), R)
          else (D as TRealData).SetRData((D as TRealData).Size+N, R);
        end else
        begin
          s1:=D.Data;
          s2:='';
          s3:='';
          for J:=1 to SL do
          s2:=s2+' '+WordStr(s1, J);
          for J:=SL+1 to NumWords(s1) do
          s3:=s3+' '+WordStr(s1, J);
          if Overwrite then
          begin
            s2:=s2+' '+S[I];
            for J:=NumWords(s2)+1 to NumWords(s1) do
            s2:=s2+' '+WordStr(s1,J);
            D.Data:=s2;
          end
          else D.Data:=s2+' '+S[I]+' '+s3;
        end;
      end else
      begin
        D:=FContainer.InitItem;
        D.Data:=S[I];
        FContainer.Items.Add(D);
        Inc(RowC);
      end;
    end;
  finally
    GlobalUnlock(H);
    S.Free;
    Clipboard.Close;
    Screen.Cursor:=crDefault;
    if RowC<>RowCount
    then RowCount:=RowC;
    ReFresh;
    FContainer.Modified:=true;
  end;
end;

procedure TWorksheet.SelectAll;
var
  R: TGridRect;
begin
  R.Left:=FixedCols;
  R.Right:=ColCount-1;
  R.Top:=FixedRows;
  R.Bottom:=FContainer.Items.Count+FixedRows-1;
  if Assigned(FContainer) and (FContainer.Items.Count>0)
  then Selection:=R;
end;

function TWorksheet.SelectCell(ACol, ARow: Longint): Boolean;
var
  D: TData;
begin
  Result:=true;
  if not Assigned(FContainer) then
  begin
    Result:=inherited SelectCell(ACol, ARow);
    Exit;
  end;
  ACol:=ACol-FixedCols;
  ARow:=ARow-FixedRows;
  if ARow>FContainer.Items.Count-1
  then Result:=ACol<1 else
  begin
    D:=FContainer.Items[ARow];
    if D is TRealData then Result:=ACol<=(D as TRealData).Size;
  end;
end;

procedure TWorksheet.SetAlignRight(B: Boolean);
begin
  FAlignRight:=B;
  Invalidate;
end;

procedure TWorksheet.SetBlockColorB(C: TColor);
begin
  FBlockColorB:=C;
  Invalidate;
end;

procedure TWorksheet.SetBlockColorF(C: TColor);
begin
  FBlockColorF:=C;
  Invalidate;
end;

procedure TWorksheet.SetContainer(C: TContainer);
begin
  FContainer:=C;
  UpdateSize;
end;

procedure TWorksheet.SetDrawHeaders(B: Boolean);
begin
  if B<>FDrawHeaders then
  begin
    FDrawHeaders:=B;
    Invalidate;
  end;
end;

procedure TWorksheet.SetEditText(ACol, ARow: Longint; const Value: string);
var
  R: TReal;
  Flag: Integer;
  D: TData;
begin
  if not Assigned(FContainer) or (ARow-FixedRows>FContainer.Items.Count-1)
  then inherited SetEditText(ACol, ARow, Value) else
  begin
    Flag:=0;
    try
      R:=StrToFloat(Value); // uses decimal separator
    except
      Val(Value, R, Flag);
    end;
    // Val(Value, R, Flag);
    if Flag<>0 then Exit;
    D:=FContainer.Items[ARow-FixedRows];
    if not (D is TRealData) then
    inherited SetEditText(ACol, ARow, Value);
    ACol:=ACol-FixedCols;
    try
      if D is TRealData then
      with (D as TRealData) do
      if ACol<Size
      then SetItem(ACol+1, R)
      else InsItem(R);
    finally
      FContainer.Modified:=true;
    end;
  end;
end;

procedure TWorksheet.SetHeader(const Value: TStrings);
begin
  if Assigned(FHeader) then FHeader.Assign(Value);
end;

procedure TWorksheet.StringsChanged(Sender: TObject);
begin
  if Sender=FHeader then Invalidate;
end;

procedure TWorksheet.UpdateSize;
begin
  if Assigned(FContainer) then
  begin
    if FContainer.DataType=dtRealData
    then ColCount:=FixedCols+MaxCols;
    RowCount:=FContainer.Items.Count+1+FixedRows;
  end;
  Invalidate;
end;

procedure TWorksheet.DeleteCell;
var
  D: TRealData;
begin
  if not (Assigned(FContainer) and (goEditing in Options))
  then Exit;
  if (Row>=FContainer.Items.Count+FixedRows)
  then Exit;
  D:=FContainer.Items[Row-FixedRows];
  if (not (D is TRealData)) or (Col+1-FixedCols>D.Size)
  then Exit;
  D.DelItem(Col+1-FixedCols);
  Invalidate;
  FContainer.Modified:=true;
end;

procedure TWorksheet.DeleteLine;
var
  D: TRealData;
begin
  if not (Assigned(FContainer) and (goEditing in Options))
  then Exit;
  if (Row>=FContainer.Items.Count+FixedRows) or (FContainer.Items.Count=0)
  then Exit;
  D:=FContainer.Items[Row-FixedRows];
  D.Free;
  FContainer.Items.Remove(D);
  RowCount:=RowCount-1;
  Invalidate;
  FContainer.Modified:=true;
end;

procedure TWorksheet.InsertLine;
begin
  if not (Assigned(FContainer) and (goEditing in Options))
  then Exit;
  if Row-FixedRows<FContainer.Items.Count then
  FContainer.Items.Insert(Row-FixedRows, FContainer.InitItem)
  else FContainer.Items.Add(FContainer.InitItem);
  RowCount:=RowCount+1;
  Invalidate;
  FContainer.Modified:=true;
end;

end.
