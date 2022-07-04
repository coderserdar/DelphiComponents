{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TvgDBLookupCombo, TvgDBGrid,                  }
{         TDBHistoryComboBox                            }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgRxDBCt;

interface

uses
  Windows, Messages, SysUtils, Classes, Menus, Graphics, Controls, DB, DBCtrls, RXLookup,
  Placemnt, RXDBCtrl;

const
  scButtonClick = scAlt + VK_RIGHT;
  clRowColor1 = clInfoBk;
  clRowColor2 = TColor($02FFFFFF);

type
  TvgDBLookupCombo = class(TRxDBLookupCombo)
  private
    { Private declarations }
    FEnableDropDown: Boolean;
    FEnableRightClick: Boolean;
    FButtonPressed, FMouseInButton: Boolean;
    FClickKey: TShortCut;
    FOnButtonClick: TNotifyEvent;
    function ButtonRect: TRect;
{$IFNDEF RX240}
    function GetField: TField;
{$ENDIF}
    function GetLookupField_: TField;
    procedure SetEnableRightClick(Value: Boolean);
    procedure UpdateButton;
  protected
    { Protected declarations }
    procedure DoButtonClick; dynamic;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure DropDown; override;
{$IFNDEF RX240}
    property Field: TField read GetField;
{$ENDIF}
    property LookupField_: TField read GetLookupField_;
  published
    { Published declarations }
    property ClickKey: TShortCut read FClickKey write FClickKey default scButtonClick;
    property EnableDropDown: Boolean read FEnableDropDown write FEnableDropDown default True;
    property EnableRightClick: Boolean read FEnableRightClick write SetEnableRightClick default True;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

{ TvgDBGrid }
  TvgDBGrid = class(TRxDBGrid)
  private
    FRowColorsUse: Boolean;
    FRowColors: array[0..1] of TColor;
    procedure SetRowColorsUse(Value: Boolean);
    procedure SetRowColor(Index: Integer; Value: TColor);
  protected
    procedure GetCellProps(Field: TField; AFont: TFont; var Background: TColor;
      Highlight: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Scroll(Distance: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property RowColorsUse: Boolean read FRowColorsUse write SetRowColorsUse default False;
    property RowColor1: TColor index 0 read FRowColors[0] write SetRowColor default clRowColor1;
    property RowColor2: TColor index 1 read FRowColors[1] write SetRowColor default clRowColor2;
  end;

{ TDBHistoryComboBox }
  TDBHistoryComboBox = class (TDBComboBox)
  private
    FHistoryClearItems: Boolean;
    FHistoryID: string;
    FHistoryReadOnly: Boolean;
    FHistorySize: Integer;
    FIniLink: TIniLink;
    procedure IniLoad(Sender: TObject);
    procedure IniSave(Sender: TObject);
    function GetPlacement: TFormPlacement;
    procedure SetPlacement(Value: TFormPlacement);
  protected
    procedure DoExit; override;
    procedure InsertString(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function HistoryIdent(Index: Integer): string;
    function HistorySection: string;
  published
    property HistoryClearItems: Boolean read FHistoryClearItems write FHistoryClearItems default False;
    property HistoryID: string read FHistoryID write FHistoryID;
    property HistoryReadOnly: Boolean read FHistoryReadOnly write FHistoryReadOnly default False;
    property HistorySize: Integer read FHistorySize write FHistorySize default 0;
    property Placement: TFormPlacement read GetPlacement write SetPlacement;
  end;

implementation
uses AppUtils, Buttons;

const
  SmallButtonSize = 6;

{ TvgDBLookupCombo }
function TvgDBLookupCombo.ButtonRect: TRect;
begin
  Result := ClientRect;
  Result.Left := Result.Right - SmallButtonSize;
  Result.Bottom := Result.Top + SmallButtonSize;
end;

constructor TvgDBLookupCombo.Create(AOwner: Tcomponent);
begin
  inherited;
  FClickKey := scButtonClick;
  FEnableDropDown := True;
  EnableRightClick := True;
end;

procedure TvgDBLookupCombo.DoButtonClick;
begin
  if Assigned(FOnButtonClick) then FOnButtonClick(Self);
end;

procedure TvgDBLookupCombo.DropDown;
begin
  if FEnableDropDown and not FButtonPressed then inherited;
end;

{$IFNDEF RX240}
function TvgDBLookupCombo.GetField: TField;
begin
  Result := nil;
  if Assigned(DataSource) and (DataSource.DataSet <> nil) then
    Result := DataSource.DataSet.FindField(DataField);
end;
{$ENDIF}

function TvgDBLookupCombo.GetLookupField_: TField;
begin
  Result := nil;
  if Assigned(LookupSource) and (LookupSource.DataSet <> nil) then
    Result := LookupSource.DataSet.FindField(LookupField);
end;

procedure TvgDBLookupCombo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if ShortCut(Key, Shift) = FClickKey then
  begin
    DoButtonClick;
    Key := 0;
  end;
  inherited;
end;

procedure TvgDBLookupCombo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled and FEnableRightClick and (Button = mbLeft) and
    PtInRect(ButtonRect, Point(X, Y)) then
  begin
    SetFocus;
    MouseCapture := True;
    FButtonPressed := True;
    FMouseInButton := True;
    UpdateButton;
    Exit;
  end;
  inherited;
end;

procedure TvgDBLookupCombo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  InButton: Boolean;
begin
  if FButtonPressed then
  begin
    InButton := PtInRect(ButtonRect, Point(X, Y));
    if FMouseInButton <> InButton then
    begin
      FMouseInButton := InButton;
      UpdateButton;
    end;
    Exit;
  end;
  inherited;
end;

procedure TvgDBLookupCombo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FButtonPressed then
  begin
    MouseCapture := False;
    FButtonPressed := False;
    UpdateButton;
    if PtInRect(ButtonRect, Point(X, Y)) then
    begin
      DoButtonClick;
      Exit;
    end;
  end;
  inherited;
end;

procedure TvgDBLookupCombo.Paint;
begin
  inherited;
  if FEnableRightClick then
  with Canvas do
    DrawButtonFace(Canvas, ButtonRect, 1, bsNew, False, FButtonPressed and FMouseInButton, False);
end;

procedure TvgDBLookupCombo.SetEnableRightClick(Value: Boolean);
begin
  if (FEnableRightClick <> Value) then
  begin
    FEnableRightClick := Value;
    Invalidate;
  end;
end;

procedure TvgDBLookupCombo.UpdateButton;
var
  R: TRect;
begin
   R := ButtonRect;
   InvalidateRect(Handle, @R, False);
   Update;
end;


{ TvgDBGrid }
constructor TvgDBGrid.Create(AOwner: TComponent);
begin
  inherited;
  FRowColors[0] := clRowColor1;
  FRowColors[1] := clRowColor2;
end;

procedure TvgDBGrid.GetCellProps(Field: TField; AFont: TFont; var Background: TColor;
  Highlight: Boolean);
begin
  if FRowColorsUse and not Highlight then
    Background := FRowColors[DataLink.ActiveRecord mod 2];
  inherited;
end;

procedure TvgDBGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if ((Key = VK_NEXT) and not DataLink.DataSet.EOF)
    or ((Key = VK_PRIOR) and not DataLink.DataSet.BOF) then
    Invalidate;
end;

procedure TvgDBGrid.Scroll(Distance: Integer);
begin
  Invalidate;
  inherited;
end;

procedure TvgDBGrid.SetRowColorsUse(Value: Boolean);
begin
  if FRowColorsUse <> Value then
  begin
    FRowColorsUse := Value;
    Invalidate;
  end;
end;

procedure TvgDBGrid.SetRowColor(Index: Integer; Value: TColor);
begin
  if FRowColors[Index] <> Value then
  begin
    FRowColors[Index] := Value;
    if FRowColorsUse then Invalidate;
  end;
end;

{ TDBHistoryComboBox }
constructor TDBHistoryComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FIniLink := TIniLink.Create;
  FIniLink.OnLoad := IniLoad;
  FIniLink.OnSave := IniSave;
end;

destructor TDBHistoryComboBox.Destroy;
begin
  FIniLink.Free;
  inherited;
end;

procedure TDBHistoryComboBox.DoExit;
begin
  inherited;
  InsertString(Text);
end;

function TDBHistoryComboBox.HistoryIdent(Index: Integer): string;
begin
  Result := Format('Item%d', [Index]);
end;

function TDBHistoryComboBox.HistorySection: string;
begin
  if FHistoryID = '' then
    Result := GetDefaultSection(Self) else Result := FHistoryID;
end;

procedure TDBHistoryComboBox.IniLoad(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if FHistorySize > 0 then
  begin
    Items.BeginUpdate;
    try
      if FHistoryClearItems then Items.Clear;
      for I := FHistorySize - 1 downto 0 do
      begin
        S := IniReadString(FIniLink.IniObject, HistorySection, HistoryIdent(I), Items[I]);
        InsertString(S);
      end;
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TDBHistoryComboBox.IniSave(Sender: TObject);
var
  I: Integer;
begin
  if (FHistorySize > 0) and not FHistoryReadOnly then
  begin
    IniEraseSection(FIniLink.IniObject, HistorySection);
    for I := 0 to Items.Count - 1 do
      IniWriteString(FIniLink.IniObject, HistorySection, HistoryIdent(I), Items[I]);
  end;
end;

procedure TDBHistoryComboBox.InsertString(const Value: string);
var
  I: Integer;
begin
  if FHistorySize > 0 then
  begin
    I := Items.IndexOf(Value);
    if I = 0 then Exit;

    Items.BeginUpdate;
    try
      while I >= 0 do
      begin
        Items.Delete(I);
        I := Items.IndexOf(Value);
      end;

      while Items.Count > FHistorySize - 1 do
        Items.Delete(Items.Count - 1);

      Items.Insert(0, Value);
    finally
      Items.EndUpdate;
    end;
  end;
end;

function TDBHistoryComboBox.GetPlacement: TFormPlacement;
begin
  Result := FIniLink.Storage;
end;

procedure TDBHistoryComboBox.SetPlacement(Value: TFormPlacement);
begin
  FIniLink.Storage := Value;
end;

end.
