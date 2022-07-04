{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software

  E-Mail:  mshkolnik@scalabium.com
  WEB: http://www.scalabium.com

  This is a grid a-la MS Access grid for text file import
}
unit SMBreakGrid;

interface

uses Classes, Controls, StdCtrls, ExtCtrls;

type
  TSMBreakColumn = class(TCollectionItem)
  private
    { Private declarations }
    FPosition: Integer;

    procedure SetPosition(Value: Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property Position: Integer read FPosition write SetPosition;
  end;

  TSMBreakColumns = class(TCollection)
  private
    FBreakGrid: TComponent;

    function GetColumn(Index: Integer): TSMBreakColumn;
    procedure SetColumn(Index: Integer; Value: TSMBreakColumn);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(ABreakGrid: TComponent);
    function Add: TSMBreakColumn;
    function Insert(Index: Integer): TSMBreakColumn;

    property BreakGrid: TComponent read FBreakGrid;
    property Items[Index: Integer]: TSMBreakColumn read GetColumn write SetColumn; default;
  end;

  TSMScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer) of object;

  TSMBreakGrid = class(TPanel)
  private
    FColumns: TSMBreakColumns;
    FRows: TStrings;
    FReadonly: Boolean;

    pbBreaks: TPaintBox;
    sbBreakHorz: TScrollbar;
    sbBreakVert: TScrollbar;

    intBreakShiftX, intBreakShiftY: Integer;
    InDownMouse: Integer;
    MousePressed: Boolean;

    FOnChangeColumns: TNotifyEvent;

    FOnScrollHorizontal: TSMScrollEvent;
    FOnScrollVertical: TSMScrollEvent;

    function GetColumns: TSMBreakColumns;
    procedure SetColumns(AValue: TSMBreakColumns);

    function GetRows: TStrings;
    procedure SetRows(AValue: TStrings);
  protected
    procedure CreateInternalControls;
    function GetBreak(X: Integer; IsAdd: Boolean): Integer;

    procedure pbBreaksMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbBreaksMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pbBreaksMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure pbBreaksPaint(Sender: TObject);

    procedure sbBreakHorzScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    procedure sbBreakVertScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Columns: TSMBreakColumns read GetColumns write SetColumns;
    property Rows: TStrings read GetRows write SetRows;
    property Readonly: Boolean read FReadonly write FReadonly default False;

    property OnChangeColumns: TNotifyEvent read FOnChangeColumns write FOnChangeColumns;
    property OnScrollHorizontal: TSMScrollEvent read FOnScrollHorizontal write FOnScrollHorizontal;
    property OnScrollVertical: TSMScrollEvent read FOnScrollVertical write FOnScrollVertical;
  end;

procedure Register;

implementation

uses Windows, Graphics, SysUtils, Forms;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMBreakGrid]);
end;

{ TSMBreakColumn }
procedure TSMBreakColumn.SetPosition(Value: Integer);
begin
  FPosition := Value;
  if (Collection is TSMBreakColumns) then
    (TSMBreakColumns(Collection).BreakGrid as TSMBreakGrid).Invalidate 
end;

procedure TSMBreakColumn.Assign(Source: TPersistent);
begin
  if Source is TSMBreakColumn then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      Position := TSMBreakColumn(Source).Position;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

{ TSMBreakColumns }
constructor TSMBreakColumns.Create(ABreakGrid: TComponent);
begin
  inherited Create(TSMBreakColumn);

  FBreakGrid := ABreakGrid;
end;

function TSMBreakColumns.Add: TSMBreakColumn;
begin
  Result := TSMBreakColumn(inherited Add);
end;

function TSMBreakColumns.Insert(Index: Integer): TSMBreakColumn;
begin
  Result := TSMBreakColumn(inherited Add);

  while (Result.Index <= Index) and (Result.Index > 0) do
  begin
    Result.Assign(Items[Result.Index-1]);
    Result := Items[Result.Index-1];
  end;
end;

function TSMBreakColumns.GetOwner: TPersistent;
begin
  Result := FBreakGrid;
end;

function TSMBreakColumns.GetColumn(Index: Integer): TSMBreakColumn;
begin
  Result := TSMBreakColumn(inherited Items[Index]);
end;

procedure TSMBreakColumns.SetColumn(Index: Integer; Value: TSMBreakColumn);
begin
  Items[Index].Assign(Value);
end;


{ TSMBreakGrid }
constructor TSMBreakGrid.Create(AOwner: TComponent);
begin
  inherited;

  FReadonly := False;
  FColumns := TSMBreakColumns.Create(Self);
  FRows := TStringList.Create;
  Font.Name := 'Courier';

  CreateInternalControls;
end;

destructor TSMBreakGrid.Destroy;
begin
  FColumns.Free;
  FRows.Free;

  inherited
end;

procedure TSMBreakGrid.CreateInternalControls;
begin
  {1. paintbox}
  pbBreaks := TPaintBox.Create(Self);
  pbBreaks.Parent := Self;
  pbBreaks.Top := 2;
  pbBreaks.Left := 2;
  pbBreaks.OnMouseDown := pbBreaksMouseDown;
  pbBreaks.OnMouseMove := pbBreaksMouseMove;
  pbBreaks.OnMouseUp := pbBreaksMouseUp;
  pbBreaks.OnPaint := pbBreaksPaint;


  {2. hor.scrollbar}
  sbBreakHorz := TScrollbar.Create(Self);
  sbBreakHorz.Parent := Self;
  sbBreakHorz.OnScroll := sbBreakHorzScroll;

  {3. vert.scrollbar}
  sbBreakVert := TScrollbar.Create(Self);
  sbBreakVert.Parent := Self;
  sbBreakVert.OnScroll := sbBreakVertScroll;
  sbBreakVert.Kind := sbVertical;

  intBreakShiftX := 0;
  intBreakShiftY := 0;
  sbBreakHorz.Position := 0;
  sbBreakVert.Position := 0;
end;

function TSMBreakGrid.GetColumns: TSMBreakColumns;
begin
  Result := FColumns
end;

procedure TSMBreakGrid.SetColumns(AValue: TSMBreakColumns);
begin
  FColumns.Assign(AValue);
  pbBreaks.Invalidate
end;

function TSMBreakGrid.GetRows: TStrings;
begin
  Result := FRows
end;

procedure TSMBreakGrid.SetRows(AValue: TStrings);
var
  i, intMax: Integer;
begin
  FRows.Assign(AValue);

  intMax := 10;
  for i := 0 to FRows.Count-1 do
    if intMax < Length(FRows[i]) then
      intMax := Length(FRows[i]);
  sbBreakHorz.Max := intMax;
  pbBreaks.Invalidate
end;

{mouse processing}
procedure TSMBreakGrid.pbBreaksMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Readonly then exit;

  InDownMouse := GetBreak(X, False);
  MousePressed := True;

  if (ssDouble in Shift) then
  begin
    if (InDownMouse > -1) then
    begin
      Columns[InDownMouse].Free;
      pbBreaks.Repaint;
      InDownMouse := -1;

      if Assigned(OnChangeColumns) then
        OnChangeColumns(Self);
    end
  end
  else
    if (InDownMouse < 0) then
      GetBreak(X, True);
end;

procedure TSMBreakGrid.pbBreaksMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MousePressed := False;
end;

procedure TSMBreakGrid.pbBreaksMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  intNew, intPos, intCharWidth: Integer;
begin
  if ReadOnly then exit;

  if MousePressed and (InDownMouse > -1) then
  begin
    intCharWidth := pbBreaks.Canvas.TextWidth('W');
    intPos := X div intCharWidth;
    if (intPos = 0) then exit;

    intNew := intBreakShiftX + intPos;
    if (Columns[InDownMouse].Position <> intNew) then
    begin
      Columns[InDownMouse].Position := intNew;

      if Assigned(OnChangeColumns) then
        OnChangeColumns(Self);
    end;
  end
end;

{draw the grid}
procedure TSMBreakGrid.pbBreaksPaint(Sender: TObject);
var
  R: TRect;
  i, j, k,
  intCharWidth, intCharHeight,
  intSampleWidth, intSampleHeight: Integer;
  intBreak: LongInt;
  s: string;
begin
  with pbBreaks, Canvas do
  begin
    intCharWidth := TextWidth('W');
    intCharHeight := TextHeight('Wg');

    R := ClientRect;
    R.Top := R.Top + intCharHeight + 15;
    Brush.Color := clBlack;
    FrameRect(R);

    InflateRect(R, -1, -1);

    intSampleWidth := (R.Right-R.Left) div intCharWidth;
    intSampleHeight := 5;

    Brush.Color := clWhite;
    Pen.Color := clBlack;
    FillRect(R);

    {draw a text sample}
    for i := 0 to intSampleHeight-1 do
    begin
      if i + intBreakShiftY < Rows.Count then
        s := Rows[i + intBreakShiftY]
      else
        s := '';
      s := Copy(s, intBreakShiftX+1, intSampleWidth);
      TextOut(R.Left, R.Top + i*intCharHeight, s);
    end;

    {draw the ruler}
    Brush.Color := Color;
    MoveTo(R.Left, R.Top - 8);
    LineTo(R.Right, R.Top - 8);
    for i := 0 to intSampleWidth-1 do
    begin
      j := i*intCharWidth;
      k := i + intBreakShiftX;
      if k mod 5 = 0 then
      begin
        MoveTo(j, R.Top - 13);
        LineTo(j, R.Top - 8);

        if (k <> 0) and (k mod 10 = 0) then
        begin
          s := IntToStr(k);
          k := TextWidth(s);
          TextOut(j - k div 2, R.Top - intCharHeight - 15, s);
        end
      end
      else
      begin
        MoveTo(j, R.Top - 10);
        LineTo(j, R.Top - 8);
      end
    end;

    {draw the arrows for breaks}
    for i := 0 to Columns.Count-1 do
    begin
      intBreak := Columns[i].Position;
      if (intBreakShiftX <= intBreak) and
         (intBreak <= intBreakShiftX + intSampleWidth) then
      begin
        j := ((intBreak - intBreakShiftX) mod intSampleWidth)*intCharWidth;
        if MousePressed and (i = InDownMouse) then
          Pen.Style := psDot
        else
          Pen.Style := psSolid;

        {arrow above}
        Brush.Color := clBlack;
        Polygon([Point(j, R.Top - 7),
                 Point(j - 3, R.Top - 3),
                 Point(j + 3, R.Top - 3)]);
        {line}
        MoveTo(j, R.Top - 3);
        LineTo(j, R.Bottom);

        Pen.Style := psSolid;
      end;
    end;
  end;
end;

{ scrollbar processing }
procedure TSMBreakGrid.sbBreakHorzScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  i, intCharWidth, intSampleWidth: Integer;
  R: TRect;
begin
  if Assigned(OnScrollHorizontal) then
    OnScrollHorizontal(Sender, ScrollCode, ScrollPos);

  with pbBreaks do
  begin
    R := ClientRect;
    R.Top := R.Top + 20;
    intCharWidth := Canvas.TextWidth('W');
  end;
  intSampleWidth := (R.Right-R.Left) div intCharWidth;
//  sbBreakHorz.Max := 1000;
  case ScrollCode of
    scLineUp: i := intBreakShiftX - 1;
    scLineDown: i := intBreakShiftX + 1;
    scPageUp: i := intBreakShiftX - intSampleWidth;
    scPageDown: i := intBreakShiftX + intSampleWidth;
    scTop: i := 0;
    scBottom: i := sbBreakHorz.Max;
    scPosition,
    scEndScroll,
    scTrack: with sbBreakHorz do
             begin
               i := Min + ScrollPos;//(ScrollPos * (Max - Min)) div MaxShortInt;
//               caption := Format('ScrollPos=%d, Min=%d, Max=%d, Cur=%d', [ScrollPos, Min, Max, Position])
             end;
  else
    i := intBreakShiftX;
  end;

  if (i < 0) then
    i := 0;
  if (i > sbBreakHorz.Max) then
    i := sbBreakHorz.Max;
  if (i <> intBreakShiftX) then
  begin
    sbBreakHorz.Position := i;
    intBreakShiftX := i;

    pbBreaks.Invalidate
  end;
end;

procedure TSMBreakGrid.sbBreakVertScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
var
  i, intSampleHeight: Integer;
begin
  if Assigned(OnScrollvertical) then
    OnScrollVertical(Sender, ScrollCode, ScrollPos);

  intSampleHeight := 5;
  sbBreakVert.Max := Rows.Count;
  case ScrollCode of
    scLineUp: i := intBreakShiftY - 1;
    scLineDown: i := intBreakShiftY + 1;
    scPageUp: i := intBreakShiftY - intSampleHeight;
    scPageDown: i := intBreakShiftY + intSampleHeight;
    scTop: i := 0;
    scBottom: i := sbBreakVert.Max;
    scPosition,
    scEndScroll,
    scTrack: with sbBreakVert do
             begin
               i := Min + ScrollPos;//(ScrollPos * (Max - Min)) div MaxShortInt;
//               caption := Format('ScrollPos=%d, Min=%d, Max=%d, Cur=%d', [ScrollPos, Min, Max, Position])
             end;
  else
    i := intBreakShiftY;
  end;

  if (i < 0) then
    i := 0;
  if (i > sbBreakVert.Max) then
    i := sbBreakVert.Max;
  if (i <> intBreakShiftY) then
  begin
    sbBreakVert.Position := i;
    intBreakShiftY := i;

    pbBreaks.Invalidate
  end;
end;

function TSMBreakGrid.GetBreak(X: Integer; IsAdd: Boolean): Integer;
var
  i, j, intPos, intCharWidth: Integer;
  IsFound: Boolean;
begin
  Result := -1;

  intCharWidth := pbBreaks.Canvas.TextWidth('W');
  intPos := X div intCharWidth;
  if IsAdd and (intPos = 0) then exit;

  IsFound := False;
  for i := 0 to Columns.Count-1 do
  begin
    j := Columns[i].Position - intBreakShiftX;
    if (j = intPos) then
    begin
      IsFound := True;
      Result := i;
      break
    end;
  end;
  if IsAdd and not IsFound then
  begin
    if (Columns.Count = 0) then
      i := 0;
    with Columns.Insert(i) do
      Position := intBreakShiftX + intPos;

    if Assigned(OnChangeColumns) then
      OnChangeColumns(Self);
  end;
end;

procedure TSMBreakGrid.Resize;
begin
  inherited;

  {change position for scrollbars and paintbox}
  sbBreakHorz.Top := ClientHeight - sbBreakHorz.Height - 2;
  sbBreakVert.Height := sbBreakHorz.Top - sbBreakVert.Top;
  pbBreaks.Height := ClientHeight - pbBreaks.Top - 2;
  sbBreakVert.Left := ClientWidth - sbBreakVert.Width - 2;
  sbBreakHorz.Width := sbBreakVert.Left-sbBreakHorz.Left;
  pbBreaks.Width := sbBreakHorz.Width;
end;

end.
