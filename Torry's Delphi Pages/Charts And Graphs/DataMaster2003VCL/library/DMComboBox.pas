///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Buttons, StdCtrls;

type
  TExpressionComboBox = class(TComboBox)
  private
    { Private declarations }
    FButton: TSpeedButton;
    FEditLeft, FEditTop, FEditWidth, FEditHeight: integer;
    FOldEditProc, FNewEditProc: TFarProc;
    procedure EditWndProc(var M: TMessage);
    procedure WMSIZE(var M: TWMSIZE); message WM_SIZE;
    procedure ButtonClick(Sender: TObject);
  protected
    { Protected declarations }
   procedure CreateWnd; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses Themes, TextDlg;

const
  ButtonWidth=18;

procedure Register;
begin
  RegisterComponents('DM2003', [TExpressionComboBox]);
end;

{ TExpressionComboBox }

procedure TExpressionComboBox.ButtonClick(Sender: TObject);
var 
  S: string;
begin
  S:=Text;
  if InputText(S, Self) then
  begin
    Text:=S;
    Change; // manually call OnChange if assigned
  end;
end;

constructor TExpressionComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FButton:=TSpeedButton.Create(Self);
  FButton.ParentFont:=false;
  FButton.Caption:='...';
  FButton.Margin:=2;
  FButton.Visible:=true;
  FButton.OnClick:=ButtonClick;
  FButton.Parent:=Self;
end;

procedure TExpressionComboBox.CreateWnd;
var
  R: TRect;
  P: TPoint;
begin
  inherited;
  // calculate original edit rectangle
  Assert(GetWindowRect(EditHandle, R), '{3A5500AD-0297-4940-B894-9546303BA310}');
  P:=ClientToScreen(Point(0,0));
  OffsetRect(R, -P.X, -P.Y);
  // shrink edit window by ButtonWidth
  FEditLeft:=R.Left;
  FEditTop:=R.Top;
  FEditWidth:=R.Right-R.Left-ButtonWidth;
  FEditHeight:=R.Bottom-R.Top;
  MoveWindow(EditHandle, FEditLeft, FEditTop, FEditWidth, FEditHeight, true);
  // set Button position
  if ThemeServices.ThemesEnabled
  then FButton.SetBounds(R.Right-R.Left-ButtonWidth+4, 1, ButtonWidth, Height-2)
  else FButton.SetBounds(R.Right-R.Left-ButtonWidth+4, 2, ButtonWidth, Height-4);
  FButton.Anchors:=[akTop, akRight, akBottom];
  // subclass edit
  FNewEditProc:=MakeObjectInstance(EditWndProc);
  FOldEditProc:=pointer(GetWindowLong(EditHandle, GWL_WNDPROC));
  SetWindowLong(EditHandle, GWL_WNDPROC, longint(FNewEditProc));
end;

{fix edit resize when user clicks dropdown button}
procedure TExpressionComboBox.EditWndProc(var M: TMessage);
begin
  if M.Msg=WM_SIZE
  then MoveWindow(EditHandle, FEditLeft, FEditTop, FEditWidth, FEditHeight, true);
  M.Result:=CallWindowProc(FOldEditProc, EditHandle, M.Msg, M.WParam, M.LParam);
end;

{correct edit size when combobox resized}
procedure TExpressionComboBox.WMSIZE(var M: TWMSIZE);
begin
  inherited;
  FEditWidth:=FButton.Left-4;
  FEditHeight:=Height-FEditTop*2; // correct when font size changed
  MoveWindow(EditHandle, FEditLeft, FEditTop, FEditWidth, FEditHeight, true);
end;

end.

