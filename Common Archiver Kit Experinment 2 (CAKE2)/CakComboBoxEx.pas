unit CakComboBoxEx;


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, CakTreeView2;

type
  TCakComboBoxEx = class;
  TDropDownFormEx = class(TForm)
  private
    { Private declarations }
    Combo : TCakComboBoxEx;
    procedure WMACTIVATE(var Msg: TMessage); message WM_ACTIVATE;
  public
    { Public declarations }
  end;

  TCakComboBoxEx = class(TPanel)
  private
    { Private declarations }
    FDroppedDown : TNotifyEvent;
    FOnChanged : TNotifyEvent;
    aPanel : TPanel;




    Canvas :  TControlCanvas;
  protected
    { Protected declarations }
  public
    { Public declarations }
    IconBitmap : TBitmap;
    DropDownHeight : integer;
    DropDownForm   : TDropDownFormEx;
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure DropDownClick(Sender : TObject);
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CancelMode;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure PaintWindow(DC: HDC); override;
    procedure Paint; dynamic;
  published
    { Published declarations }
    property OnDroppedDown : TNotifyEvent read FDroppedDown write FDroppedDown;
    property OnChanged : TNotifyEvent read FOnChanged write FOnChanged;
  end;

  procedure Register;

var
  DropDownForm: TDropDownFormEx;

implementation

{$R *.dfm}

procedure Register;
begin
  RegisterComponents('CAKE', [TCakComboBoxEx]);
end;

constructor TCakComboBoxEx.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  Height := 22;
  Canvas := TControlCanvas.Create;
  Canvas.Control := Self;
  DropDownHeight := 150;

  aPanel := TPanel.Create(Self);
  aPanel.Parent := Self;
  aPanel.Align := alRight;
  aPanel.Width := 14;
  aPanel.BorderWidth := 0;
  aPanel.Font.Name := 'Wingdings 3';
  aPanel.Font.Size := 6;
  aPanel.caption := chr(113);
  aPanel.BevelInner := bvNone;
  aPanel.BevelOuter := bvSpace;
  aPanel.BevelWidth := 1;
  aPanel.OnClick := DropDownClick;

  Self.BevelInner  := bvNone;
  Self.BevelOuter  := bvSpace;
  Self.BorderStyle := bsSingle;
  Self.BevelWidth  := 1;
  Self.Color       := clWindow;
  Self.Caption     := '';
  DropDownForm     := TDropDownFormEx.Create(Self);


  IconBitmap := TBitmap.Create;
end;

destructor TCakComboBoxEx.Destroy;
begin
  DropDownForm.Free;
  Canvas.Free;
  IconBitmap.Free;
  inherited Destroy;
end;

procedure TCakComboBoxEx.DropDownClick(Sender : TObject);
begin
  DropDownForm.Top   := Self.ClientOrigin.Y + Self.Height - 4;
  DropDownForm.Left  := Self.ClientOrigin.X - 2;
  DropDownForm.Width := Self.Width;
  DropDownForm.Height := DropDownHeight;
  DropDownForm.Combo := Self;
  if Assigned(FDroppedDown) then
        FDroppedDown(Sender);
  DropDownForm.Show;
end;

procedure TCakComboBoxEx.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> DropDownForm) and
     (DropDownForm.Visible) then
  DropDownForm.Close;
end;

procedure TCakComboBoxEx.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TDropDownFormEx.WMACTIVATE(var Msg: TMessage);
begin
  if LOWORD(Msg.wParam) = WA_INACTIVE then
    Close;
end;

procedure TCakComboBoxEx.PaintWindow(DC: HDC);
begin
  inherited paintWindow(DC);
  Paint;
end;


procedure TCakComboBoxEx.paint;
begin
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Rect(0,0,Width,Height));
  Canvas.Draw(2,0,IconBitmap);
  Canvas.TextOut(20,2,Caption)
end;

end.
