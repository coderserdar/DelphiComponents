unit TSColors;
{
	How to add your own colors to the IDE:

		1) Add the color constant to the "const" section below
		2) Add the color value and name to the TSColorMap array
		3) Count the number of Items in TSColorMap and set TSColorCount accordingly.

	Remember to add a reference to this unit to every form unit that uses the
	newly defined color codes.
}

interface

uses
	Classes,
	Graphics,
	Types;

const
	clTSRed = TColor($005652EE); //5657327
	clTSYellow = TColor($0080FFFE); //8454143
	clTSGreen = TColor($004BF562); //4978017
	clTSGray = TColor($00BCBCBD); //12369084
	clTSBlue = TColor($00FB8B6B); //16485226
	clTSOcean = TColor($00C6A800);
	clTSOrange = TColor($0017BAFF);
	clTSBrown = TColor($000053A6);
	clTSWhite = TColor($00FEFEFE); //16776959
	clTSBackground = TColor($00DCDCDC); //13226196
	clTSReadOnly = TColor($00C9C9CA); //15132390
	clTSLightGray = TColor($00E0E0E0);
	clTSPaleGray = TColor($00F0F0F0);
	clTSPaleYellow = TColor($00C8FFFF);
	clTSSplitter = TColor($00A33829);

function TSColorToIdent(Color: Longint; var Ident: string): Boolean;
function TSIdentToColor(const Ident: string; var Color: Longint): Boolean;
function TSStringToColor(const S: string): TColor;
procedure GetTSColorValues(Proc: TGetStrProc);

implementation

uses
	SysUtils;

const
	TSColorCount = 15;
	TSColorMap: array[0..TSColorCount - 1] of TIdentMapEntry = (
		(Value: clTSRed; Name: 'clTSRed'),
		(Value: clTSYellow; Name: 'clTSYellow'),
		(Value: clTSGreen; Name: 'clTSGreen'),
		(Value: clTSGray; Name: 'clTSGray'),
		(Value: clTSBlue; Name: 'clTSBlue'),
		(Value: clTSOrange; Name: 'clTSOrange'),
		(Value: clTSOcean; Name: 'clTSOcean'),
		(Value: clTSBrown; Name: 'clTSBrown'),
		(Value: clTSWhite; Name: 'clTSWhite'),
		(Value: clTSBackground; Name: 'clTSBackground'),
		(Value: clTSReadOnly; Name: 'clTSReadOnly'),
		(Value: clTSLightGray; Name: 'clTSLightGray'),
		(Value: clTSSplitter; Name: 'clTSSplitter'),
		(Value: clTSPaleYellow; Name: 'clTSPaleYellow'),
		(Value: clTSPaleGray; Name: 'clTSPaleGray')
	);

function TSColorToIdent(Color: Longint; var Ident: string): Boolean;
var
	I: Integer;
begin
	Result := ColorToIdent(Color, Ident);
	if not Result then begin
		for I := Low(TSColorMap) to High(TSColorMap) do begin
			if TSColorMap[I].Value = Color then
			begin
				Result := True;
				Ident := TSColorMap[I].Name;
				Exit;
			end;
		end;
	end;
end;

function TSIdentToColor(const Ident: string; var Color: Longint): Boolean;
var
	I: Integer;
begin
	Result := IdentToColor(Ident, Color);
	if not Result then begin
		for I := Low(TSColorMap) to High(TSColorMap) do begin
			if SameText(TSColorMap[I].Name, Ident) then
			begin
				Result := True;
				Color := TSColorMap[I].Value;
				Exit;
			end;
		end;
	end;
end;

function TSStringToColor(const S: string): TColor;
begin
	if not TSIdentToColor(S, Longint(Result)) then
		Result := TColor(StrToInt(S));
end;


procedure GetTSColorValues(Proc: TGetStrProc);
var
	I: Integer;
begin
	for I := Low(TSColorMap) to High(TSColorMap) do
		Proc(TSColorMap[I].Name);
end;

initialization
	UnRegisterIntegerConsts(TypeInfo(TColor), IdentToColor, ColorToIdent);
	RegisterIntegerConsts(TypeInfo(TColor), TSIdentToColor, TSColorToIdent);


finalization
	UnRegisterIntegerConsts(TypeInfo(TColor), TSIdentToColor, TSColorToIdent);
	RegisterIntegerConsts(TypeInfo(TColor), IdentToColor, ColorToIdent);



end.


