{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  Этот компонент предоставляет возможность сворачивать
  (открывать и закрывать) панель (вернее ScrollBox),
  включая все ее содержимое
  (like component panel in the Microsoft Outlook)

  Первоначальная идея и частичная реализация:
    Valera Svetlov 2:461/42.5 (компоненты FCPanel и FCPanelPage)
}
unit SMBox;

interface

{$R SMBOX}

uses
  Classes, Graphics, Controls, StdCtrls, ExtCtrls, Forms;

type
  TSMPanel = class;
  TButtonStyle = (bsNone, bsPlusMinus, bsPicture, bsPlusMinusPicture);

  TSMBox = class(TScrollBox) //WinControl)
  private
    { Private declarations }
    FSMPanel: TSMPanel;

    FPageOpened: Boolean;

    FPicturePlus: TPicture;
    FPictureMinus: TPicture;
    FPictureOpen: TPicture;
    FPictureClosed: TPicture;

    FpnlButton: TPanel;
    FimgPlusMinus: TImage;
    FimgNode: TImage;
    FlblText: TLabel;
    FAlignment: TAlignment;
    FCaption: TCaption;

    FButtonColor: TColor;
    FButtonStyle: TButtonStyle;
    FButtonDown: Boolean;
    FButtonHeight: Integer;

    FHeight: Integer;
    FHorzScroll, FVertScroll: Boolean;

    procedure ButtonClick(Sender: TObject);
    procedure SetSMPanel(InsertInPanel: TSMPanel);
    procedure SetButtonColor(Value: TColor);
    procedure SetButtonStyle(Value: TButtonStyle);
    procedure SetButtonHeight(Value: Integer);
    procedure SetCaption(Value: TCaption);
    procedure SetAlignment(Value: TAlignment);
    procedure SetPageOpened(Value: Boolean);
    procedure SetButtonDown(Value: Boolean);
    function GetPicture(Index: Integer): TPicture;
    procedure SetPicture(Index: Integer; Value: TPicture);
  protected
    { Protected declarations }
    procedure ReadState(Reader: TReader); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property SMPanel: TSMPanel read FSMPanel write SetSMPanel;
    procedure MovePageUp;
    procedure MovePageDown;
  published
    { Published declarations }
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
    property ButtonStyle: TButtonStyle read FButtonStyle write SetButtonStyle;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight;
    property ButtonDown: Boolean read FButtonDown write SetButtonDown;
    property Caption: TCaption read FCaption write SetCaption;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property PageOpened: Boolean read FPageOpened write SetPageOpened;

    property PicturePlus: TPicture index 0 read GetPicture write SetPicture;
    property PictureMinus: TPicture index 1 read GetPicture write SetPicture;
    property PictureOpen: TPicture index 2 read GetPicture write SetPicture;
    property PictureClosed: TPicture index 3 read GetPicture write SetPicture;
  end;

  TSMPanel = class(TScrollBox) //TCustomPanel)
  private
    { Private declarations }
    FPages: TList; {Список страниц}
    FClosePagesBeforeOpen: Boolean;
    FStartTop: integer;
    function GetPage(index: Integer): TSMBox;
    function GetPagesCount: Integer;
    procedure SetClosePagesBeforeOpen(Value: Boolean);
    procedure SetStartTop(InStartTop: Integer);
    procedure AlignPages;
    procedure OnPanelResize(Sender: TObject);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClosePages;

    procedure InsertPage(AddPage: TSMBox);
    procedure RemovePage(AddPage: TSMBox);

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    property PagesCount: integer read GetPagesCount;
    property Pages[index: integer]: TSMBox read GetPage;
  published
    { Published declarations }
//    property Align;
    property ClosePagesBeforeOpen: Boolean read FClosePagesBeforeOpen write SetClosePagesBeforeOpen;
    property StartTop: integer read FStartTop write SetStartTop;
  end;


implementation
uses Windows;

constructor TSMBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPicturePlus := TPicture.Create;
  FPicturePlus.Bitmap.Handle := LoadBitmap(hInstance, 'PLUS');

  FPictureMinus := TPicture.Create;
  FPictureMinus.Bitmap.Handle := LoadBitmap(hInstance, 'MINUS');

  FPictureOpen := TPicture.Create;
  FPictureOpen.Bitmap.Handle := LoadBitmap(hInstance, 'OPEN');

  FPictureClosed := TPicture.Create;
  FPictureClosed.Bitmap.Handle := LoadBitmap(hInstance, 'CLOSED');

  FButtonStyle := bsPlusMinusPicture;
  FButtonColor := clGray;
  FButtonHeight := 25;

  FpnlButton := TPanel.Create(Self);
  with FpnlButton do
  begin
    Parent := Self;
    Height := FButtonHeight;
    Align := alTop;
    Caption := '';
    Color := FButtonColor;
  end;

  FimgPlusMinus := TImage.Create(Self);
  with FimgPlusMinus do
  begin
    Parent := FpnlButton;
    Height := FButtonHeight;
    Width := 14;
    Center := True;
    AutoSize := True;
    Align := alLeft;
    Transparent := True;

    OnClick := ButtonClick;
  end;

  FimgNode := TImage.Create(Self);
  with FimgNode do
  begin
    Parent := FpnlButton;
    Height := FButtonHeight;
    Width := 25;
    Center := True;
    AutoSize := True;
    Align := alLeft;
    Transparent := True;

    OnClick := ButtonClick;
  end;

  FAlignment := taCenter;
  FlblText := TLabel.Create(Self);
  with FlblText do
  begin
    Parent := FpnlButton;
    Align := alClient;
    Alignment := FAlignment;
    Layout := tlCenter;
    Transparent := True;
    WordWrap := True;

    OnClick := ButtonClick;
  end;

  FHorzScroll := HorzScrollBar.Visible;
  FVertScroll := VertScrollBar.Visible;
  FHeight := Height;

  Caption := 'New Page';
  PageOpened := True;
end;

destructor TSMBox.Destroy;
begin
  FPicturePlus.Free;
  FPictureMinus.Free;
  FPictureOpen.Free;
  FPictureClosed.Free;

  FlblText.Free;
  FimgPlusMinus.Free;
  FimgNode.Free;
  FpnlButton.Free;

  inherited Destroy;
end;

procedure TSMBox.ButtonClick(Sender: TObject);
begin
  PageOpened := not PageOpened;
end;

procedure TSMBox.SetSMPanel(InsertInPanel: TSMPanel);
begin
  if (FSMPanel <> nil) then
    FSMPanel.RemovePage(Self);

  Parent := InsertInPanel;

  if (InsertInPanel <> nil) then
    InsertInPanel.InsertPage(Self);
end;

{При чтении из потока}
procedure TSMBox.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);

  if Reader.Parent is TSMPanel then
    SMPanel := TSMPanel(Reader.Parent);
end;

{Передвигает страницу вверх}
procedure TSMBox.MovePageUp;
var p: Pointer;
    n: Integer;
begin
  if (SMPanel <> nil) then
  begin
    n := SMPanel.FPages.IndexOf(Self);
    {Если нет страниц или эта страница первая, то выходим}
    if (n = -1) or (n = 0) then Exit;

    with SMPanel do
    begin
      p := FPages[n-1];
      FPages[n-1] := FPages[n];
      FPages[n] := p;
      AlignPages;
    end;
  end;
end;

{Передвигает страницу вниз}
procedure TSMBox.MovePageDown;
var p: Pointer;
    n: Integer;
begin
  if (SMPanel <> nil) then
  begin
    n := SMPanel.FPages.IndexOf(Self);
    {Если нет страниц или эта страница последняя, то выходим}
    if (n = -1) or (n = SMPanel.FPages.Count-1) then Exit;

    with SMPanel do
    begin
      p := FPages[n+1];
      FPages[n+1] := FPages[n];
      FPages[n] := p;
      AlignPages;
    end;
  end;
end;

procedure TSMBox.SetButtonStyle(Value: TButtonStyle);
begin
  if (Value <> FButtonStyle) then
  begin
    FButtonStyle := Value;
    FimgPlusMinus.Visible := (FButtonStyle in [bsPlusMinus, bsPlusMinusPicture]);
    FimgNode.Visible := (FButtonStyle in [bsPicture, bsPlusMinusPicture]);
    Refresh
  end;
end;

procedure TSMBox.SetButtonColor(Value: TColor);
begin
  if (Value <> FButtonColor) then
  begin
    FButtonColor := Value;
    FpnlButton.Color := Value;
  end;
end;

procedure TSMBox.SetButtonHeight(Value: Integer);
begin
  if (Value <> FButtonHeight) then
  begin
    FButtonHeight := Value;
    FpnlButton.Height := Value;
  end;
end;

procedure TSMBox.SetCaption(Value: TCaption);
begin
  if (Value <> FCaption) then
  begin
    FCaption := Value;
    FlblText.Caption := Value;
  end;
end;

procedure TSMBox.SetAlignment(Value: TAlignment);
begin
  if (Value <> FAlignment) then
  begin
    FAlignment := Value;
    FlblText.Alignment := Value;
  end;
end;

procedure TSMBox.SetPageOpened(Value: Boolean);
begin
  if (Value <> FPageOpened) then
  begin
    FPageOpened := Value;
    if FPageOpened then
    begin
      FimgPlusMinus.Picture.Assign(FPictureMinus);
      FimgNode.Picture.Assign(FPictureOpen);

      HorzScrollBar.Visible := FHorzScroll;
      VertScrollBar.Visible := FVertScroll;
      Height := FHeight;
    end
    else
    begin
      FimgPlusMinus.Picture.Assign(FPicturePlus);
      FimgNode.Picture.Assign(FPictureClosed);

      FHeight := Height;
      FHorzScroll := HorzScrollBar.Visible;
      HorzScrollBar.Visible := False;

      FVertScroll := VertScrollBar.Visible;
      VertScrollBar.Visible := False;
      Height := FButtonHeight+4;
    end;

    {перерасполагаем страницы}
    if (SMPanel <> nil) then
      SMPanel.AlignPages;
  end;
end;

procedure TSMBox.SetButtonDown(Value: Boolean);
begin
  if (Value <> FButtonDown) then
  begin
    FButtonDown := Value;
    if FButtonDown then
    begin
      FpnlButton.BevelInner := bvLowered;
      FpnlButton.BevelOuter := bvNone;
    end
    else
    begin
      FpnlButton.BevelInner := bvNone;
      FpnlButton.BevelOuter := bvRaised;
    end;
  end;
end;

function TSMBox.GetPicture(Index: Integer): TPicture;
begin
  case Index of
    0: Result := FPicturePlus;
    1: Result := FPictureMinus;
    2: Result := FPictureOpen;
    3: Result := FPictureClosed;
  else
    Result := nil
  end;
end;

procedure TSMBox.SetPicture(Index: Integer; Value: TPicture);
begin
  case Index of
    0: FPicturePlus.Assign(Value);
    1: FPictureMinus.Assign(Value);
    2: FPictureOpen.Assign(Value);
    3: FPictureClosed.Assign(Value);
  end;
end;

{ TSMPanel }
constructor TSMPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStartTop := 0;
  Caption := ' ';
  Height := 200;
  FPages := TList.Create;

  HorzScrollBar.Visible := False;
  OnResize := OnPanelResize;
end;

destructor TSMPanel.Destroy;
begin
  OnResize := nil;
  FPages.Free;

  inherited Destroy;
end;

{Ф-ция возвращает заданную страницу по номеру}
function TSMPanel.GetPage(index: Integer): TSMBox;
begin
  if (FPages.Count > 0) then
    Result := FPages[index]
  else
    Result := nil;
end;

{Возвращает кол-во страниц}
function TSMPanel.GetPagesCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TSMPanel.SetClosePagesBeforeOpen(Value: Boolean);
begin
  if (FClosePagesBeforeOpen <> Value) then
  begin
    FClosePagesBeforeOpen := Value;
    AlignPages;
  end;
end;

{Устанавливает сдвиг сверху}
procedure TSMPanel.SetStartTop(InStartTop: Integer);
begin
  if (FStartTop <> InStartTop) then
  begin
    FStartTop := -InStartTop;
    AlignPages;
  end;
end;

{Процедура возвращает все дочерние объекты}
procedure TSMPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var i: Integer;
begin
  for i := 0 to FPages.Count-1 do
    Proc(TComponent(FPages[i]));
end;

{Закрывает все страницы}
procedure TSMPanel.ClosePages;
var i: Integer;
begin
  for i := 0 to GetPagesCount-1 do
    Pages[i].PageOpened := False;
end;

{Вставляет заданную страницу}
procedure TSMPanel.InsertPage(AddPage: TSMBox);
begin
  FPages.Add(AddPage);
  AddPage.FSMPanel := Self;
  AlignPages;
end;

{Удаляет страницу}
procedure TSMPanel.RemovePage(AddPage: TSMBox);
var i: Integer;
begin
  i := FPages.IndexOf(AddPage);
  if (i <> -1) then
    FPages.Delete(i);
  AlignPages;
end;

{Располагает страницы по порядку}
procedure TSMPanel.AlignPages;
var i, t: Integer;
begin
  if FClosePagesBeforeOpen then
    ClosePages;

  {Проверим, не задвинули ли мы слишком вниз}
  if (FStartTop > 0) then
    FStartTop := 0;
  {Проверим, не задвинули ли слишком вверх}
  t := 0;
  if (GetPagesCount > 0) then
    for i := 0 to GetPagesCount-1 do
      t := t + Pages[i].Height;
  {Если все влазит, то уберем сдвиг}
  if (t <= Height) then
    FStartTop := 0
  else {иначе установим границу движения вверх}
    if (-FStartTop > t-Height) then
      FStartTop := -(t-Height);
  t := FStartTop;
  if (GetPagesCount > 0) then
    for i := 0 to GetPagesCount-1 do
    begin
      Pages[i].Left := 0;
      Pages[i].Top := t;
      Pages[i].Width := Width;
      t := t + Pages[i].Height;
    end;
end;

procedure TSMPanel.OnPanelResize(Sender: TObject);
begin
  AlignPages;
end;


end.
