unit FormDispCaps;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Grids, StdCtrls, mcConst;

type
	TfrDispCaps = class(TForm)
		sgDispCaps: TStringGrid;
		lbCaps: TListBox;
		procedure FormShow(Sender: TObject);
		procedure sgDispCapsKeyPress(Sender: TObject; var Key: Char);
		procedure sgDispCapsClick(Sender: TObject);
	private
		function DevCapsStr(Idx: integer): string;
		procedure GetDeviceType(DeviceIndex: TDeviceIndex);
		procedure GetDetailInfo(DeviceIndex: TDeviceIndex);
	public
		{ Public declarations }
	end;

var
	frDispCaps: TfrDispCaps;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrDispCaps.GetDeviceType(DeviceIndex: TDeviceIndex);
const
	ClickMsg= 'Click here for more info';
var
	i, j: integer;
begin
	i:= 0;
	sgDispCaps.RowCount:= 2;
	sgDispCaps.Cells[0,0]:= 'Idx';
	sgDispCaps.Cells[1,0]:= 'Parameter';
	sgDispCaps.Cells[2,0]:= 'Value';
	// For more information, read GetDeviceCaps function from
	// Win32 Programmers Ref. Help File
	while i <= BLTALIGNMENT do
	begin
		if (DevCapsStr(i) <> '') then
		with frMain.APITools1 do
		begin
			j:= sgDispCaps.RowCount-1;
			sgDispCaps.Cells[0,j]:= IntToStr(i);
			sgDispCaps.Cells[1,j]:= DevCapsStr(i);
			sgDispCaps.Cells[2,j]:= IntToStr(GetDevCaps(DeviceIndex,i));
			if i = PDEVICESIZE then
			sgDispCaps.Cells[2,j]:= 'Reserved';
			if i = TECHNOLOGY then
			case GetDevCaps(DeviceIndex,TECHNOLOGY) of
				DT_PLOTTER: sgDispCaps.Cells[2,j]:= 		'Vector Plotter';					// 	0
				DT_RASDISPLAY: sgDispCaps.Cells[2,j]:= 	'Display Device';					// 	1
				DT_RASPRINTER: sgDispCaps.Cells[2,j]:= 	'Printer Device';       	//	2
				DT_RASCAMERA: sgDispCaps.Cells[2,j]:= 	'Camera Device';        	//	3
				DT_CHARSTREAM: sgDispCaps.Cells[2,j]:= 	'Character Stream Device';//	4
				DT_METAFILE: sgDispCaps.Cells[2,j]:= 		'Metafile Device';        //	5
				DT_DISPFILE: sgDispCaps.Cells[2,j]:= 		'Display File';           //	6
			end else
			case i of
				CURVECAPS,
				LINECAPS,
				RASTERCAPS,
				POLYGONALCAPS,
				TEXTCAPS: sgDispCaps.Cells[2,j]:= ClickMsg;
			end;
			sgDispCaps.RowCount:= sgDispCaps.RowCount+1;
		end;
		Inc(i);
	end;
end;

procedure TfrDispCaps.FormShow(Sender: TObject);
begin
	with frMain do
	begin
		if idoDisplay.Checked then
		GetDeviceType(diDisplay) else
		GetDeviceType(diPrinter);
	end;
end;

function TfrDispCaps.DevCapsStr(Idx: integer): string;
begin
	result:= '';
	case Idx of
		DRIVERVERSION: result:= 'DRIVERVERSION';
		TECHNOLOGY: result:= 'TECHNOLOGY';
		HORZSIZE: result:= 'HORZSIZE';
		VERTSIZE: result:= 'VERTSIZE';
		HORZRES: result:= 'HORZRES';
		VERTRES: result:= 'VERTRES';
		BITSPIXEL: result:= 'BITSPIXEL';
		PLANES: result:= 'PLANES';
		NUMBRUSHES: result:= 'NUMBRUSHES';
		NUMPENS: result:= 'NUMPENS';
		NUMMARKERS: result:= 'NUMMARKERS';
		NUMFONTS: result:= 'NUMFONTS';
		NUMCOLORS: result:= 'NUMCOLORS';
		PDEVICESIZE: result:= 'PDEVICESIZE';
		CURVECAPS: result:= 'CURVECAPS';
		LINECAPS: result:= 'LINECAPS';
		POLYGONALCAPS: result:= 'POLYGONALCAPS';
		TEXTCAPS: result:= 'TEXTCAPS';
		CLIPCAPS: result:= 'CLIPCAPS';
		RASTERCAPS: result:= 'RASTERCAPS';
		ASPECTX: result:= 'ASPECTX';
		ASPECTY: result:= 'ASPECTY';
		ASPECTXY: result:= 'ASPECTXY';
		LOGPIXELSX: result:= 'LOGPIXELSX';
		LOGPIXELSY: result:= 'LOGPIXELSY';
		SIZEPALETTE: result:= 'SIZEPALETTE';
		NUMRESERVED: result:= 'NUMRESERVED';
		COLORRES: result:= 'COLORRES';
		{ Printing related DeviceCaps. These replace the appropriate Escapes }
		PHYSICALWIDTH: result:= 'PHYSICALWIDTH';
		PHYSICALHEIGHT: result:= 'PHYSICALHEIGHT';
		PHYSICALOFFSETX: result:= 'PHYSICALOFFSETX';
		PHYSICALOFFSETY: result:= 'PHYSICALOFFSETY';
		SCALINGFACTORX: result:= 'SCALINGFACTORX';
		SCALINGFACTORY: result:= 'SCALINGFACTORY';
		{ Display driver specific}
		VREFRESH: result:= 'VREFRESH';
		DESKTOPVERTRES: result:= 'DESKTOPVERTRES';
		DESKTOPHORZRES: result:= 'DESKTOPHORZRES';
		BLTALIGNMENT: result:= 'BLTALIGNMENT';
	end;
end;

procedure TfrDispCaps.sgDispCapsKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

procedure TfrDispCaps.GetDetailInfo(DeviceIndex: TDeviceIndex);
var
	DCC: DWORD;
begin
	with sgDispCaps,frMain.APITools1,lbCaps,Items do
	begin
		Clear;
		Visible:= False;
		if Cells[Col,Row] = 'RASTERCAPS' then
		begin
			Visible:= True;
			DCC:= GetDevCaps(DeviceIndex,RASTERCAPS);
			if (DCC and RC_BANDING) = RC_BANDING then
			Add('Device requires banding support') else
			Add('Device does not require banding support');
			if (DCC and RC_BITBLT) = RC_BITBLT then
			Add('Device transfers bitmaps') else
			Add('Device cannot transfer bitmaps');
			if (DCC and RC_BITMAP64) = RC_BITMAP64 then
			Add('Device supports bitmaps larger than 64K') else
			Add('Device cannot support bitmaps larger than 64K');
			if (DCC and RC_DI_BITMAP) = RC_DI_BITMAP then
			Add('Device supports SetDIBits and GetDIBits') else
			Add('Device cannot support SetDIBits and GetDIBits');
			if (DCC and RC_DIBTODEV) = RC_DIBTODEV then
			Add('Device supports SetDIBitsToDevice') else
			Add('Device cannot support SetDIBitsToDevice');
			if (DCC and RC_FLOODFILL) = RC_FLOODFILL then
			Add('Device performs flood fills') else
			Add('Device cannot perform flood fills');
			if (DCC and RC_GDI20_OUTPUT) = RC_GDI20_OUTPUT then
			Add('Device supports features of Windows 2.0') else
			Add('Device cannot support features of Windows 2.0');
			if (DCC and RC_PALETTE) = RC_PALETTE then
			Add('Device specifies a palette-based device') else
			Add('Device does not specify a palette-based device');
			if (DCC and RC_SCALING) = RC_SCALING then
			Add('Device capable of scaling') else
			Add('Device is not capable of scaling');
			if (DCC and RC_STRETCHBLT) = RC_STRETCHBLT then
			Add('Device performs StretchBlt function') else
			Add('Device cannot perform StretchBlt function');
			if (DCC and RC_STRETCHDIB) = RC_STRETCHDIB then
			Add('Device performs StretchDIBits function') else
			Add('Device cannot perform StretchDIBits function');
		end;
		if Cells[Col,Row] = 'CURVECAPS' then
		begin
			Visible:= True;
			DCC:= GetDevCaps(DeviceIndex,CURVECAPS);
			if (DCC and CC_NONE) = CC_NONE then
			Add('Device supports curves') else
			Add('Device does not support curves');
			if (DCC and CC_CIRCLES) = CC_CIRCLES then
			Add('Device can draw circles') else
			Add('Device cannot draw circles');
			if (DCC and CC_PIE) = CC_PIE then
			Add('Device can draw pie wedges') else
			Add('Device cannot draw pie wedges');
			if (DCC and CC_CHORD) = CC_CHORD then
			Add('Device can draw chord arcs') else
			Add('Device cannot draw chord arcs');
			if (DCC and CC_ELLIPSES) = CC_ELLIPSES then
			Add('Device can draw ellipses') else
			Add('Device cannot draw ellipses');
			if (DCC and CC_WIDE) = CC_WIDE then
			Add('Device can draw wide borders') else
			Add('Device cannot draw wide borders');
			if (DCC and CC_STYLED) = CC_STYLED then
			Add('Device can draw styled borders') else
			Add('Device cannot draw styled borders');
			if (DCC and CC_WIDESTYLED) = CC_WIDESTYLED then
			Add('Device can draw wide and styled borders') else
			Add('Device cannot draw wide and styled borders');
			if (DCC and CC_INTERIORS) = CC_INTERIORS then
			Add('Device can draw interiors') else
			Add('Device cannot draw interiors');
			if (DCC and CC_ROUNDRECT) = CC_ROUNDRECT then
			Add('Device can draw rounded rectangles') else
			Add('Device cannot draw rounded rectangles');
		end;
		if Cells[Col,Row] = 'LINECAPS' then
		begin
			Visible:= True;
			DCC:= GetDevCaps(DeviceIndex,LINECAPS);
			if (DCC and LC_NONE) = LC_NONE then
			Add('Device supports lines') else
			Add('Device does not support lines');
			if (DCC and LC_POLYLINE) = LC_POLYLINE then
			Add('Device can draw a polyline') else
			Add('Device cannot draw a polyline');
			if (DCC and LC_MARKER) = LC_MARKER then
			Add('Device can draw a marker') else
			Add('Device cannot draw a marker');
			if (DCC and LC_POLYMARKER) = LC_POLYMARKER then
			Add('Device can draw multiple markers') else
			Add('Device cannot draw multiple markers');
			if (DCC and LC_WIDE) = LC_WIDE then
			Add('Device can draw wide lines') else
			Add('Device cannot draw wide lines');
			if (DCC and LC_STYLED) = LC_STYLED then
			Add('Device can draw styled lines') else
			Add('Device cannot draw styled lines');
			if (DCC and LC_WIDESTYLED) = LC_WIDESTYLED then
			Add('Device can draw wide and styled lines') else
			Add('Device cannot draw wide and styled lines');
			if (DCC and LC_INTERIORS) = LC_INTERIORS then
			Add('Device can draw interiors') else
			Add('Device cannot draw interiors');
		end;
		if Cells[Col,Row] = 'POLYGONALCAPS' then
		begin
			Visible:= True;
			DCC:= GetDevCaps(DeviceIndex,POLYGONALCAPS);
			if (DCC and PC_NONE) = PC_NONE then
			Add('Device supports polygons') else
			Add('Device does not support polygons');
			if (DCC and PC_POLYGON) = PC_POLYGON then
			Add('Device can draw alternate-fill polygons') else
			Add('Device cannot draw alternate-fill polygons');
			if (DCC and PC_RECTANGLE) = PC_RECTANGLE then
			Add('Device can draw rectangles') else
			Add('Device cannot draw rectangles');
			if (DCC and PC_WINDPOLYGON) = PC_WINDPOLYGON then
			Add('Device can draw winding-fill polygons') else
			Add('Device cannot draw winding-fill polygons');
			if (DCC and PC_SCANLINE) = PC_SCANLINE then
			Add('Device can draw a single scanline') else
			Add('Device cannot draw a single scanline');
			if (DCC and PC_WIDE) = PC_WIDE then
			Add('Device can draw wide borders') else
			Add('Device cannot draw wide borders');
			if (DCC and PC_STYLED) = PC_STYLED then
			Add('Device can draw styled borders') else
			Add('Device cannot draw styled borders');
			if (DCC and PC_WIDESTYLED) = PC_WIDESTYLED then
			Add('Device can draw borders that are wide and styled') else
			Add('Device cannot draw borders that are wide and styled');
			if (DCC and PC_INTERIORS) = PC_INTERIORS then
			Add('Device can draw interiors') else
			Add('Device cannot draw interiors');
		end;
		if Cells[Col,Row] = 'TEXTCAPS' then
		begin
			Visible:= True;
			DCC:= GetDevCaps(DeviceIndex,TEXTCAPS);
			if (DCC and TC_OP_CHARACTER) = TC_OP_CHARACTER then
			Add('Device is capable of character output precision') else
			Add('Device is not capable of character output precision');
			if (DCC and TC_OP_STROKE) = TC_OP_STROKE then
			Add('Device is capable of stroke output precision') else
			Add('Device is not capable of stroke output precision');
			if (DCC and TC_CP_STROKE) = TC_CP_STROKE then
			Add('Device is capable of stroke clip precision') else
			Add('Device is not capable of stroke clip precision');
			if (DCC and TC_CR_90) = TC_CR_90 then
			Add('Device is capable of 90-degree character rotation') else
			Add('Device is not capable of 90-degree character rotation');
			if (DCC and TC_CR_ANY) = TC_CR_ANY then
			Add('Device is capable of any character rotation') else
			Add('Device is not capable of any character rotation');
			if (DCC and TC_SF_X_YINDEP) = TC_SF_X_YINDEP then
			Add('Device can scale independently in the x- and y-directions') else
			Add('Device cannot scale independently in the x- and y-directions');
			if (DCC and TC_SA_DOUBLE) = TC_SA_DOUBLE then
			Add('Device is capable of doubled character for scaling') else
			Add('Device is not capable of doubled character for scaling');
			if (DCC and TC_SA_INTEGER) = TC_SA_INTEGER then
			Add('Device uses integer multiples only for character scaling') else
			Add('Device does not use integer multiples only for character scaling');
			if (DCC and TC_SA_CONTIN) = TC_SA_CONTIN then
			Add('Device uses any multiples for exact character scaling') else
			Add('Device does not use multiples for exact character scaling');
			if (DCC and TC_EA_DOUBLE) = TC_EA_DOUBLE then
			Add('Device can draw double-weight characters') else
			Add('Device cannot draw double-weight characters');
			if (DCC and TC_IA_ABLE) = TC_IA_ABLE then
			Add('Device can italicize') else
			Add('Device cannot italicize');
			if (DCC and TC_UA_ABLE) = TC_UA_ABLE then
			Add('Device can underline') else
			Add('Device cannot underline');
			if (DCC and TC_SO_ABLE) = TC_SO_ABLE then
			Add('Device can draw strikeouts') else
			Add('Device cannot draw strikeouts');
			if (DCC and TC_RA_ABLE) = TC_RA_ABLE then
			Add('Device can draw raster fonts') else
			Add('Device cannot draw raster fonts');
			if (DCC and TC_VA_ABLE) = TC_VA_ABLE then
			Add('Device can draw vector fonts') else
			Add('Device cannot draw vector fonts');
			if (DCC and TC_SCROLLBLT) = TC_SCROLLBLT then
			Add('Device cannot scroll using a bit-block transfer') else
			Add('Device can scroll using a bit-block transfer');
		end;
	end;
end;

procedure TfrDispCaps.sgDispCapsClick(Sender: TObject);
begin
	with frMain do
	begin
		if idoDisplay.Checked then
		GetDetailInfo(diDisplay) else
		GetDetailInfo(diPrinter);
	end;
end;

end.
