{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmEditGrid
Purpose  : Standard Grids (String/Draw) with enhanced inplace editing capabilities.
Date     : 10-05-2000
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmEditGrid;

interface

{$I CompilerDefines.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, grids, buttons,
  StdCtrls, ExtCtrls;

type
  TrmEditGridEditEvent = procedure(Sender: TObject; aCol, aRow, popx, popy: integer) of Object;
  TrmEditGridBeforeSelectCellEvent = procedure(Sender: TObject; aCol, aRow : integer; var ShowButton, ButtonEnabled: boolean) of object;

  TrmEditDrawGrid = class(TDrawGrid)
  private
    FButton: TSpeedButton;
    fOnDoCellEdit: TrmEditGridEditEvent;
    fOnBeforeCellSelect: TrmEditGridBeforeSelectCellEvent;
    procedure ClickCell(X, Y: integer; LeftButton: Boolean);
    procedure ButtonClick(Sender: TObject);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function SelectCell(ACol, ARow: integer): boolean; override;
    procedure TopLeftChanged; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; Override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnDoCellEdit: TrmEditGridEditEvent read fOnDoCellEdit write fOnDoCellEdit;
    property OnBeforeCellSelect: TrmEditGridBeforeSelectCellEvent read fOnBeforeCellSelect write fOnBeforeCellSelect;
  end;

  TrmEditGrid = class(TStringGrid)
  private
    FButton: TSpeedButton;
    fOnDoCellEdit: TrmEditGridEditEvent;
    fOnBeforeCellSelect: TrmEditGridBeforeSelectCellEvent;
    procedure ClickCell(X, Y: integer; LeftButton: Boolean);
    procedure ButtonClick(Sender: TObject);
  protected
    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function SelectCell(ACol, ARow: integer): boolean; override;
    procedure TopLeftChanged; override;
    procedure ColumnMoved(FromIndex, ToIndex: Longint); override;
    procedure ColWidthsChanged; Override;
    procedure DoExit; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnDoCellEdit: TrmEditGridEditEvent read fOnDoCellEdit write fOnDoCellEdit;
    property OnBeforeCellSelect: TrmEditGridBeforeSelectCellEvent read fOnBeforeCellSelect write fOnBeforeCellSelect;
  end;

implementation

{ TrmEditGrid }

procedure TrmEditGrid.ButtonClick(Sender: TObject);
begin
  FButton.Down := True;
  try
     ClickCell(TSpeedButton(Sender).Left, TSpeedButton(Sender).Top, True);
  finally
     FButton.Down := False;
  end;
end;

procedure TrmEditGrid.ClickCell(X, Y: integer; LeftButton: Boolean);
var
  P: TPoint;
  mCol,
    mRow: integer;
begin
  if not assigned(fOnDoCellEdit) then
    exit;

  MouseToCell(X, Y, mCol, mRow);
  if (mCol < 0) or (mRow < 0) then
    Exit;

  if LeftButton then
  begin
    P.X := X;
    P.Y := Y + 16;
  end
  else
  begin
    P.X := X;
    P.Y := Y;
  end;
  P := ClientToScreen(P);
  if ((mCol > 0) and (mCol < ColCount)) and ((mRow >= 0) and (mRow < RowCount)) and LeftButton then
     fOnDoCellEdit(self, mcol, mrow, p.x, p.y);
end;

procedure TrmEditGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  // Now let the control do the move ....
  inherited ColumnMoved(FromIndex, ToIndex);
  if FButton.Visible = True then
  begin
    FButton.Visible := False;
    FButton.OnClick := nil;
    SelectCell(Col, Row);
  end;
end;

procedure TrmEditGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  if FButton.Visible = True then
  begin
    FButton.Visible := False;
    FButton.OnClick := nil;
    SelectCell(Col, Row);
  end;
end;

constructor TrmEditGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //creating the magic portion of our editor...
  FButton := TSpeedButton.Create(nil);
  FButton.GroupIndex := 1;
  FButton.AllowAllUp := True;
  FButton.Parent := Self;
  FButton.Visible := False;
  FButton.Caption := '...';
end;

destructor TrmEditGrid.Destroy;
begin
  FButton.Free;
  inherited;
end;

procedure TrmEditGrid.DoExit;
begin
  FButton.Visible := False;
  FButton.OnClick := nil;
  inherited;
end;

procedure TrmEditGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //Keyboard triggers for the editing stuff....
  if (((key = VK_RETURN) and (Shift = [ssCtrl])) or ((Key = VK_SPACE) or (key = vk_f2))) and
    fButton.visible then
  begin
    ButtonClick(FButton);
  end;
  inherited KeyDown(Key, Shift);
end;

function TrmEditGrid.SelectCell(ACol, ARow: integer): boolean;
var
  Rect: TRect;
  wCalc : integer;
  fCanShow, fIsEnabled : boolean;
begin
  result := inherited SelectCell(ACol, ARow);

  FButton.Visible := False;
  fButton.OnClick := nil;

  Rect := CellRect(ACol, ARow);
  wCalc := Rect.Bottom - Rect.top;

  FButton.Top := Rect.Top;
  FButton.Left := Rect.Right - wCalc;
  FButton.Width := wCalc;
  FButton.Height := wCalc;

  fCanShow := false;
  fIsEnabled := false;

  if assigned(fOnBeforeCellSelect) then
     fOnBeforeCellSelect(Self, acol, arow, fCanShow, fIsEnabled);

  fButton.onclick := ButtonClick;
  FButton.Enabled := fIsEnabled;
  FButton.Visible := fCanShow;
end;

procedure TrmEditGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if FButton.Visible = True then
  begin
    FButton.Visible := False;
    FButton.OnClick := nil;
    SelectCell(Col, Row);
  end;
end;

procedure TrmEditGrid.WndProc(var Message: TMessage);
begin
  //There is a better way to do this but....
  if Message.Msg = WM_RBUTTONDOWN then
    ClickCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, False);
  inherited WndProc(Message);
end;

{ TrmEditDrawGrid }

procedure TrmEditDrawGrid.ButtonClick(Sender: TObject);
begin
  FButton.Down := True;
  try
     ClickCell(TSpeedButton(Sender).Left, TSpeedButton(Sender).Top, True);
  finally
     FButton.Down := False;
  end;
end;

procedure TrmEditDrawGrid.ClickCell(X, Y: integer; LeftButton: Boolean);
var
  P: TPoint;
  mCol,
    mRow: integer;
begin
  if not assigned(fOnDoCellEdit) then
    exit;

  MouseToCell(X, Y, mCol, mRow);
  if (mCol < 0) or (mRow < 0) then
    Exit;

  if LeftButton then
  begin
    P.X := X;
    P.Y := Y + 16;
  end
  else
  begin
    P.X := X;
    P.Y := Y;
  end;
  P := ClientToScreen(P);
  if ((mCol > 0) and (mCol < ColCount)) and ((mRow >= 0) and (mRow < RowCount)) and LeftButton then
     fOnDoCellEdit(self, mcol, mrow, p.x, p.y);
end;

procedure TrmEditDrawGrid.ColumnMoved(FromIndex, ToIndex: Integer);
begin
  // Now let the control do the move ....
  inherited ColumnMoved(FromIndex, ToIndex);
  if FButton.Visible = True then
  begin
    FButton.Visible := False;
    FButton.OnClick := nil;
    SelectCell(Col, Row);
  end;
end;

procedure TrmEditDrawGrid.ColWidthsChanged;
begin
  inherited ColWidthsChanged;
  if FButton.Visible = True then
  begin
    FButton.Visible := False;
    FButton.OnClick := nil;
    SelectCell(Col, Row);
  end;
end;

constructor TrmEditDrawGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //creating the magic portion of our editor...
  FButton := TSpeedButton.Create(nil);
  FButton.GroupIndex := 1;
  FButton.AllowAllUp := True;
  FButton.Parent := Self;
  FButton.Visible := False;
  FButton.Caption := '...';
end;

destructor TrmEditDrawGrid.Destroy;
begin
  FButton.Free;
  inherited;
end;

procedure TrmEditDrawGrid.DoExit;
begin
  FButton.Visible := False;
  FButton.OnClick := nil;
  inherited;
end;

procedure TrmEditDrawGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  //Keyboard triggers for the editing stuff....
  if (((key = VK_RETURN) and (Shift = [ssCtrl])) or ((Key = VK_SPACE) or (key = vk_f2))) and
    fButton.visible then
  begin
    ButtonClick(FButton);
  end;
  inherited KeyDown(Key, Shift);
end;

function TrmEditDrawGrid.SelectCell(ACol, ARow: integer): boolean;
var
  Rect: TRect;
  wCalc : integer;
  fCanShow, fIsEnabled : boolean;
begin
  result := inherited SelectCell(ACol, ARow);

  FButton.Visible := False;
  fButton.OnClick := nil;

  Rect := CellRect(ACol, ARow);
  wCalc := Rect.Bottom - Rect.top;

  FButton.Top := Rect.Top;
  FButton.Left := Rect.Right - wCalc;
  FButton.Width := wCalc;
  FButton.Height := wCalc;

  fCanShow := false;
  fIsEnabled := false;

  if assigned(fOnBeforeCellSelect) then
     fOnBeforeCellSelect(Self, acol, arow, fCanShow, fIsEnabled);

  fButton.onclick := ButtonClick;
  FButton.Enabled := fIsEnabled;
  FButton.Visible := fCanShow;
end;

procedure TrmEditDrawGrid.TopLeftChanged;
begin
  inherited TopLeftChanged;
  if FButton.Visible = True then
  begin
    FButton.Visible := False;
    FButton.OnClick := nil;
    SelectCell(Col, Row);
  end;
end;

procedure TrmEditDrawGrid.WndProc(var Message: TMessage);
begin
  //There is a better way to do this but....
  if Message.Msg = WM_RBUTTONDOWN then
    ClickCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, False);
  inherited WndProc(Message);
end;

end.
