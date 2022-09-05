unit TSPropEdit;

interface

uses
	Classes,
	Graphics,
	Types,
	DesignIntf,
	DesignEditors,
	VCLEditors;


type
	TTSColorProperty = class(TColorProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
	public
		function GetValue: string; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure SetValue(const Value: string); override;
		procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
									const ARect: TRect; ASelected: Boolean);

		{ ICustomPropertyListDrawing }
		procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
			var AWidth: Integer);
		procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
			var AHeight: Integer);

		{ CustomPropertyDrawing }
		procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
			ASelected: Boolean);
		procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
			ASelected: Boolean);

	end;

	TTSStringProperty = class(TStringProperty)
		function GetAttributes: TPropertyAttributes; override;
		procedure Edit; override;
	end;

	TTSStringListProperty = class(TClassProperty)
		procedure Edit; override;
		function GetAttributes: TPropertyAttributes; override;
	end;

implementation

uses
	Controls,
	Forms,
	SysUtils,
	TypInfo,
	TSColors,
	TSStrPropDlg;


{ TTSColorProperty }

procedure TTSColorProperty.GetValues(Proc: TGetStrProc);
begin
	GetColorValues(Proc);
	GetTSColorValues(Proc);
end;

function TTSColorProperty.GetValue: string;
var
	Color: TColor;
begin
	Color := TColor(GetOrdValue);
	if not TSColorToIdent(Color, Result) then
		FmtStr(Result, '%s%.8x', [HexDisplayPrefix, Color]);
end;

procedure TTSColorProperty.SetValue(const Value: string);
var
	NewColor: LongInt;
begin
	if not TSIdentToColor(Value, NewColor) then
		NewColor := StrToInt(Value);
	SetOrdValue(NewColor);
end;


//Die Prozedur wurde 1:1 übernommen aus der Unit VCL-Editors. Nur eine Zeile wurde ersetzt:
//	Der Aufruf von "StringToColor(Value)" wurde ersetzt durch "TSStringToColor(Value)
procedure TTSColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
	function ColorToBorderColor(AColor: TColor): TColor;
	type
		TColorQuad = record
			Red,
			Green,
			Blue,
			Alpha: Byte;
		end;
	begin
		if (TColorQuad(AColor).Red > 192) or
			 (TColorQuad(AColor).Green > 192) or
			 (TColorQuad(AColor).Blue > 192) then
			Result := clBlack
		else if ASelected then
			Result := clWhite
		else
			Result := AColor;
	end;
var
	Right: Integer;
	OldPenColor, OldBrushColor: TColor;
begin
	Right := (ARect.Bottom - ARect.Top) {* 2} + ARect.Left;
	with ACanvas do
	begin
		// save off things
		OldPenColor := Pen.Color;
		OldBrushColor := Brush.Color;

		// frame things
		Pen.Color := Brush.Color;
		Rectangle(ARect.Left, ARect.Top, Right, ARect.Bottom);

		// set things up and do the work

		Brush.Color := TSStringToColor(Value); //Aufruf geändert (original: StringToColor(Value) )

		Pen.Color := ColorToBorderColor(ColorToRGB(Brush.Color));
		Rectangle(ARect.Left + 1, ARect.Top + 1, Right - 1, ARect.Bottom - 1);

		// restore the things we TSiddled with
		Brush.Color := OldBrushColor;
		Pen.Color := OldPenColor;
		DefaultPropertyListDrawValue(Value, ACanvas, Rect(Right, ARect.Top, ARect.Right,
			ARect.Bottom), ASelected);
	end;
end;

procedure TTSColorProperty.ListMeasureWidth(const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin
	AWidth := AWidth + ACanvas.TextHeight('M');
end;

procedure TTSColorProperty.ListMeasureHeight(const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
	//keine Implementierung (wie auch in TColorProperty)
end;

procedure TTSColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
	DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TTSColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
	if GetVisualValue <> '' then
		ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
	else
		DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;



{ TTSStringListProperty }

procedure TTSStringListProperty.Edit;
begin
	with TFrmStrPropDlg.Create(Application) do
		try
			MeText.Lines.Assign(TStringList(Pointer(GetOrdValue)));
			if ShowModal = mrOK then
				SetOrdValue(Longint(Pointer(MeText.Lines)));
		finally
			Free;
		end;
end;

function TTSStringListProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paDialog];
end;

{ TTSStringProperty }

procedure TTSStringProperty.Edit;
begin
	with TFrmStrPropDlg.Create(Application) do
		try
			MeText.Lines.Text := GetStrValue;
			if ShowModal = mrOK then
				SetStrValue(MeText.Lines.Text);
		finally
			Free;
		end;
end;

function TTSStringProperty.GetAttributes: TPropertyAttributes;
begin
	Result := inherited GetAttributes + [paDialog];
end;

end.
