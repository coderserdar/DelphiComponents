unit TSStrPropDlg;


interface


uses
	Forms, TSForms, ImgList, Controls, Classes, ActnList, Dialogs, StdCtrls, Buttons,
	ExtCtrls;

type
	TFrmStrPropDlg = class(TTSForm)
		MeText: TMemo;
		pnlButton: TPanel;
		DlgOpen: TOpenDialog;
		DlgSave: TSaveDialog;
		BvlFrame: TBevel;
		cbWordWrap: TCheckBox;
		ActionList1: TActionList;
		ActLoad: TAction;
		ImageList1: TImageList;
		ActSave: TAction;
		ActOK: TAction;
		ActCancel: TAction;
		ActTglWW: TAction;
		BtnLoad: TBitBtn;
		BtnOK: TBitBtn;
		BtnCancel: TBitBtn;
		BtnSave: TBitBtn;
		ActSelAll: TAction;
		BtnSelAll: TBitBtn;
		LbCaret: TLabel;
		procedure ActLoadExecute(Sender: TObject);
		procedure ActSaveExecute(Sender: TObject);
		procedure ActTglWWExecute(Sender: TObject);
		procedure ActOKExecute(Sender: TObject);
		procedure ActCancelExecute(Sender: TObject);
		procedure ActSelAllExecute(Sender: TObject);
		procedure MeTextKeyPress(Sender: TObject; var Key: Char);
		procedure MeTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure MeTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure MeTextClick(Sender: TObject);
		procedure TSFormShow(Sender: TObject);
    procedure TSFormClose(Sender: TObject; var Action: TCloseAction);
	private
		procedure UpdateWordWrap(Flag: Boolean);
		procedure UpdateTextPos;
	end;


implementation


uses
	SysUtils,
	Windows,
	Registry;


{$R *.DFM}

const
	sepChars = [#9, #$20..#$2f, #$3a..#$40, #$5b..#$60, #$7b..#$7f];
	sepCharsBack = sepChars + [#10];
	sepCharsFwd = sepChars + [#13, #10];
var
	FormPos: TRect = (Left:0; Top:0; Right:0; Bottom:0);
	FormMax: Boolean = False;

procedure TFrmStrPropDlg.ActOKExecute(Sender: TObject);
begin
	ModalResult := mrOK;
end;

procedure TFrmStrPropDlg.ActCancelExecute(Sender: TObject);
begin
	ModalResult := mrCancel;
end;

procedure TFrmStrPropDlg.ActSelAllExecute(Sender: TObject);
begin
	MeText.SetFocus;
	MeText.SelectAll;
end;

procedure TFrmStrPropDlg.ActLoadExecute(Sender: TObject);
begin
	if dlgOpen.Execute then
		MeText.Lines.LoadFromFile(dlgOpen.FileName);
end;

procedure TFrmStrPropDlg.ActSaveExecute(Sender: TObject);
begin
	if DlgSave.Execute then
		MeText.Lines.SaveToFile(dlgSave.FileName);
end;

procedure TFrmStrPropDlg.ActTglWWExecute(Sender: TObject);
begin
	UpdateWordWrap(cbWordWrap.Checked);
	MeText.SetFocus;
end;

procedure TFrmStrPropDlg.TSFormShow(Sender: TObject);
begin
	UpdateWordWrap(cbWordWrap.Checked);
	WindowState := wsNormal;
	if (FormPos.Right>FormPos.Left) and (FormPos.Bottom>FormPos.Top) then
		Self.BoundsRect := FormPos;
	if FormMax then
		WindowState := wsMaximized;
end;

procedure TFrmStrPropDlg.TSFormClose(Sender: TObject; var Action: TCloseAction);
begin
	FormMax := WindowState = wsMaximized;
	WindowState := wsNormal;
	Application.ProcessMessages;
	FormPos := Self.BoundsRect;
end;

procedure TFrmStrPropDlg.UpdateTextPos;
var
	CP: TPoint;
	I: Integer;
begin
	CP := MeText.CaretPos;
	LbCaret.Caption := IntToStr(CP.Y+1) + ' / ' + IntToStr(CP.X+1);
end;

procedure TFrmStrPropDlg.UpdateWordWrap(Flag: Boolean);
const
	cScrollBars: array[Boolean] of TScrollStyle = (ssBoth, ssVertical);
begin
	cbWordWrap.Checked := Flag;
	MeText.WordWrap := Flag;
	MeText.ScrollBars := cScrollBars[Flag];
end;


procedure TFrmStrPropDlg.MeTextClick(Sender: TObject);
begin
	UpdateTextPos;
end;

procedure TFrmStrPropDlg.MeTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
	L: Integer;
begin
	if Shift = [ssCtrl] then
	begin
		case Key of
			VK_LEFT: begin

				while (MeText.SelStart>0) and (MeText.Lines.Text[MeText.SelStart] in sepChars) do
					MeText.SelStart := MeText.SelStart -1;
				if (MeText.SelStart>1) and (MeText.Lines.Text[MeText.SelStart]=#10) then
					MeText.SelStart := MeText.SelStart -2
				else
					while (MeText.SelStart>0) and not (MeText.Lines.Text[MeText.SelStart] in sepCharsBack) do
					begin
						if (MeText.SelStart>1) and (MeText.Lines.Text[MeText.SelStart]=#10) then
							MeText.SelStart := MeText.SelStart -2
						else
							MeText.SelStart := MeText.SelStart -1;
					end;
				Key := 0;
			end;
			VK_RIGHT: begin
				L := Length(MeText.Text);
				while (MeText.SelStart<L) and not (MeText.Lines.Text[MeText.SelStart+1] in sepCharsFwd) do
						MeText.SelStart := MeText.SelStart +1;
				while (MeText.SelStart<L) and (MeText.Lines.Text[MeText.SelStart+1] in sepCharsFwd) do
						MeText.SelStart := MeText.SelStart +1;
				Key := 0;
			end;
		end;
	end;
end;

procedure TFrmStrPropDlg.MeTextKeyPress(Sender: TObject; var Key: Char);
var
	SSt: Integer;
	C: char;
	Indent: string;
begin
	case Key of
	#13: begin
		SSt := MeText.SelStart;
		Indent := '';
		if SSt>0 then
		begin
			while (SSt>1) and not (MeText.Lines.Text[SSt] in [#13, #10]) do
				dec(SSt);
			if MeText.Lines.Text[SSt] in [#13, #10] then
				inc(SSt);
			while (SSt<MeText.SelStart) and (MeText.Lines.Text[SSt] in [' ', #9]) do
			begin
				Indent := Indent + MeText.Lines.Text[SSt];
				inc(SSt);
			end;
		end;
		MeText.SelText := #13#10 + Indent;
		MeText.SelStart := MeText.SelStart + Length(Indent) + 2;
		Key := #0;
	end;
	#127: begin
		if MeText.SelLength > 0 then
			MeText.SelText := ''
		else begin
			SSt := MeText.SelStart;
			repeat
				C := MeText.Lines.Text[MeText.SelStart];
				MeText.SelStart := MeText.SelStart - 1;
			until (MeText.SelStart=0) or (C  in sepCharsBack);
			C := MeText.Lines.Text[MeText.SelStart];
			while (C  in sepCharsBack) and (MeText.SelStart>0) do begin
				MeText.SelStart := MeText.SelStart - 1;
				C := MeText.Lines.Text[MeText.SelStart];
			end;
			MeText.SelLength := SSt - MeText.SelStart;
			MeText.SelText := '';
		end;
		Key := #0;
	end;
	^T: begin
		if MeText.SelLength > 0 then
			MeText.SelText := ''
		else begin
			SSt := Length(MeText.Lines.Text);
			repeat
				MeText.SelLength := MeText.SelLength + 1;
				C := MeText.Lines.Text[MeText.SelStart+MeText.SelLength+1];
			until (MeText.SelStart+MeText.SelLength>=SSt) or (C in sepCharsFwd);
			while (C in sepCharsFwd) and (MeText.SelStart+MeText.SelLength<SSt) do begin
				MeText.SelLength := MeText.SelLength + 1;
				C := MeText.Lines.Text[MeText.SelStart+MeText.SelLength+1];
			end;
			MeText.SelText := '';
		end;
		Key := #0;
	end;
	^Y: begin
		MeText.Lines.Delete(MeText.CaretPos.Y);
		Key := #0;
	end;
	end;
end;

procedure TFrmStrPropDlg.MeTextKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	UpdateTextPos;
end;

end.
