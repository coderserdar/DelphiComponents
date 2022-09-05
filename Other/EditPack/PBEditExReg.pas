unit PBEditExReg;

// This propertyeditor for TImageIndex property has initially
// been written (or at least posted) by Serge Gubenko - Thanks!

{$UNDEF DEL34}
{$UNDEF DEL345}
{$IFDEF VER100}
	{$DEFINE DEL34}
	{$DEFINE DEL345}
{$ENDIF}
{$IFDEF VER120}
	{$DEFINE DEL34}
	{$DEFINE DEL345}
{$ENDIF}
{$IFDEF VER130} {$DEFINE DEL345} {$ENDIF}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, TypInfo, PBEditEx,
{$IFDEF DEL345} DsgnIntf
{$ELSE}
	DesignIntf, DesignEditors, VCLEditors, RTLConsts
{$ENDIF};

type
{$IFDEF DEL34}
	TImageIndexEditor = class(TIntegerProperty);
{$ELSE}
	TImageIndexEditor = class(TIntegerProperty, ICustomPropertyDrawing,
		ICustomPropertyListDrawing)
	protected
		function GetImageList: TImageList; virtual;
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: string; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure SetValue(const Value: string); override;
		procedure ListMeasureWidth(const Value: string;
			ACanvas: TCanvas; var AWidth: Integer); virtual;
		procedure ListMeasureHeight(const Value: string;
			ACanvas: TCanvas; var AHeight: Integer); virtual;
		procedure ListDrawValue(const Value: string;
			ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); virtual;
		procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
			ASelected: Boolean);
		procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
			ASelected: Boolean);
	end;
{$ENDIF}

procedure Register;

implementation

{$IFNDEF DEL34}

function TImageIndexEditor.GetImageList: TImageList;
begin
	Result := TImageList(TypInfo.GetObjectProp(GetComponent(0), 'Images'));
end;

function TImageIndexEditor.GetAttributes: TPropertyAttributes;
begin
	Result := [paValueList, paRevertable];
end;

function TImageIndexEditor.GetValue: string;
begin
	Result := IntToStr(GetOrdValue);
end;

procedure TImageIndexEditor.SetValue(const Value: string);
var
	XValue: integer;
begin
	try
		XValue := StrToInt(Value);
		SetOrdValue(XValue);
	except
		inherited SetValue(Value);
	end;
end;

procedure TImageIndexEditor.GetValues(Proc: TGetStrProc);
var
	XImageList: TImageList;
	i: integer;
begin
	XImageList:=GetImageList;
	if Assigned(XImageList)
		then for i := 0 to XImageList.Count - 1 do Proc(IntToStr(i));
end;

procedure TImageIndexEditor.ListMeasureWidth(const Value: string;
	ACanvas : TCanvas; var AWidth: Integer);
begin
	AWidth := AWidth + ACanvas.TextHeight('M');
	if AWidth < 17 then AWidth := 17;
end;

procedure TImageIndexEditor.ListMeasureHeight(const Value: string;
	ACanvas : TCanvas; var AHeight: Integer);
var
	XImageList: TImageList;
begin
	XImageList:=GetImageList;
	if Assigned(XImageList)
		then AHeight := XImageList.Height + 2
	else AHeight := 20;
	if AHeight < 17 then AHeight := 17;
end;

procedure TImageIndexEditor.ListDrawValue(const Value: string;
	ACanvas : TCanvas; const ARect : TRect; ASelected : Boolean);
var
	XImageList: TImageList;
	XRight: Integer;
	XOldPenColor, XOldBrushColor: TColor;
begin
	XImageList := GetImageList;
	XRight := 0;
	try
		if Assigned(XImageList) then
		begin
			XRight := (ARect.Bottom-ARect.Top) + ARect.Left;
			XOldPenColor := ACanvas.Pen.Color;
			XOldBrushColor := ACanvas.Brush.Color;
			ACanvas.Pen.Color := ACanvas.Brush.Color;
			ACanvas.Rectangle(ARect.Left, ARect.Top, XRight, ARect.Bottom);
			XImageList.DrawOverlay(ACanvas, ARect.Left + 1, ARect.Top,
				StrToInt(Value), 0);
			ACanvas.Brush.Color := XOldBrushColor;
			ACanvas.Pen.Color := XOldPenColor;
		end;
	finally
		DefaultPropertyListDrawValue(Value, ACanvas, Rect(XRight, ARect.Top,
			ARect.Right, ARect.Bottom), ASelected);
	end;
end;

procedure TImageIndexEditor.PropDrawName(ACanvas: TCanvas;
	const ARect : TRect; ASelected: Boolean);
begin
	DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TImageIndexEditor.PropDrawValue(ACanvas: TCanvas;
	const ARect : TRect; ASelected: Boolean);
var
	XImageList: TImageList;
begin
	XImageList := GetImageList;
	if (GetVisualValue <> '') and Assigned(XImageList) and (XImageList.Height < 17)
		then ListDrawValue(GetVisualValue, ACanvas, ARect, True)
	else DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

{$ENDIF}

//-------------------------- Register -----------------------
procedure Register;
begin
	RegisterComponents('PBEdit', [TPBEditEx]);
	RegisterPropertyEditor(TypeInfo(TImageIndex), TPBEditEx, '',
		TImageIndexEditor);
end;

end.
