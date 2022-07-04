
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit BalloonHintFrm;

interface

{$I STD.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, TypInfo, Balloon, SuplCtrls;

{ TBalloonHintForm }

type
  TBalloonHintForm = class(TForm)
    GroupBox: TGroupBox;
    CaptionEdit: TEdit;
    OKButtopn: TButton;
    CancelButton: TButton;
    Background: TBackground;
    TextMemo: TMemo;
    CaptionLabel: TLabel;
    TextLabel: TLabel;
    KindBox: TComboBox;
    PositionBox: TComboBox;
    KindLabel: TLabel;
    PositionLabel: TLabel;
    ParamsEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ParamsChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
  	FHint: TBalloonHint;
    procedure SetHint(Value: TBalloonHint);
  	procedure BackgroundPaint(Sender: TObject);
  public
  	property Hint: TBalloonHint read FHint write SetHint;
  end;

function EditBalloonHint(Hint: TBalloonHint = nil): string;

implementation

{$R *.DFM}

{ TBalloonHintForm }

procedure TBalloonHintForm.FormCreate(Sender: TObject);
begin
  ClientHeight := CancelButton.Top + 32;
  ClientWidth := Background.Left + CancelButton.Left + CancelButton.Width;
  Background.Color := clAppWorkspace;
  Background.OnPaint := BackgroundPaint;
  KindBox.ItemIndex := 0;
  PositionBox.ItemIndex := 0;
end;

procedure TBalloonHintForm.BackgroundPaint(Sender: TObject);
var
	Icon: HICON;
begin
	if (FHint <> nil) and (KindBox.ItemIndex = Ord(bkCustom)) then
  	Icon := FHint.Icon.Handle
	else
  	Icon := 0;
	PreviewBalloonHint(Background.Canvas.Handle, Background.ClientRect,
  	CaptionEdit.Text, TextMemo.Text, Icon, TBalloonKind(Ord(KindBox.ItemIndex)),
    TBalloonPosition(Ord(PositionBox.ItemIndex)));
end;

procedure TBalloonHintForm.ParamsChange(Sender: TObject);
var
	C, T, K, P, X, Y, D: string;
begin
	C := StringReplace(CaptionEdit.Text, '''', '''''', [rfReplaceAll]);
	T := StringReplace(TextMemo.Text, '''', '''''', [rfReplaceAll]);
	T := StringReplace(T, #13#10, '''#13#10''', [rfReplaceAll]);
  if KindBox.ItemIndex = Ord(bkCustom) then
  	K := 'Icon'
	else
  	K := GetEnumName(TypeInfo(TBalloonKind), KindBox.ItemIndex);
  P := GetEnumName(TypeInfo(TBalloonPosition), PositionBox.ItemIndex);
  if FHint <> nil then
  begin
    X := IntToStr(FHint.X);
    Y := IntToStr(FHint.Y);
    D := IntToStr(FHint.Duration);
  end
  else
  begin
    X := 'X';
    Y := 'Y';
    D := '0';
  end;
	ParamsEdit.Text := Format('BalloonHint(''%s'', ''%s'', %s, %s, %s, %s, %s);',
  	[C, T, K, P, X, Y, D]);
  Background.Draw;
end;

procedure TBalloonHintForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  	ModalResult := mrCancel;
end;

procedure TBalloonHintForm.SetHint(Value: TBalloonHint);
begin
	FHint := Value;
  if FHint <> nil then
  begin
		CaptionEdit.Text := FHint.Caption;
		TextMemo.Text := FHint.Text;
    KindBox.ItemIndex := Ord(FHint.Kind);
    PositionBox.ItemIndex := Ord(FHint.Position);
  end
  else
  	CaptionEdit.Text := 'Balloon Hint';
end;

function EditBalloonHint(Hint: TBalloonHint = nil): string;
var
	H: TBalloonHint absolute Hint;
begin
	with TBalloonHintForm.Create(Application) do
  try
		Hint := H;
    if ShowModal = mrOK then
    begin
    	Result := ParamsEdit.Text;
			if FHint <> nil then
    	begin
	      H.Caption := CaptionEdit.Text;
  	    H.Text := TextMemo.Text;
    	  H.Kind := TBalloonKind(Ord(KindBox.ItemIndex));
      	H.Position := TBalloonPosition(Ord(PositionBox.ItemIndex));
   	  end;
		end
    else
    	Result := '';
  finally
  	Free;
  end;
end;

end.
