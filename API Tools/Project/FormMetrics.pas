unit FormMetrics;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Grids;

type
	TfrMetrics = class(TForm)
		sgMetrics: TStringGrid;
		procedure FormShow(Sender: TObject);
		procedure sgMetricsKeyPress(Sender: TObject; var Key: Char);
	private
		function MetricsStr(Idx: integer): string;
	public
		{ Public declarations }
	end;

var
  frMetrics: TfrMetrics;

implementation

uses FormMain, APITool;

{$R *.DFM}

procedure TfrMetrics.FormShow(Sender: TObject);
var
	i,j: integer;
begin
	i:= 0;
	sgMetrics.RowCount:= 2;
	sgMetrics.Cells[0,0]:= 'Idx';
	sgMetrics.Cells[1,0]:= 'Parameter';
	sgMetrics.Cells[2,0]:= 'Value';
	// For more information, read GetSystemMetrics function from
	// Win32 Programmers Ref. Help File
	while i <= SM_MOUSEWHEELPRESENT do
	begin
		if (MetricsStr(i) <> '') then
		begin
			j:= sgMetrics.RowCount-1;
			sgMetrics.Cells[0,j]:= IntToStr(i);
			sgMetrics.Cells[1,j]:= MetricsStr(i);
			sgMetrics.Cells[2,j]:= IntToStr(GetSystemMetrics(i));
			if i = SM_CLEANBOOT then
			case GetSystemMetrics(i) of
				0: sgMetrics.Cells[2,j]:= 'Normal boot';
				1: sgMetrics.Cells[2,j]:= 'Fail-safe boot';
				2: sgMetrics.Cells[2,j]:= 'Fail-safe with network boot';
			end;
			sgMetrics.RowCount:= sgMetrics.RowCount+1;
		end;
		Inc(i);
	end;
end;

function TfrMetrics.MetricsStr(Idx: integer): string;
begin
	result:= '';
	case Idx of
{0000}SM_CXSCREEN: result:= 'SM_CXSCREEN';
//{0000}ARW_LEFT: result:= 'ARW_LEFT';
//{0000}ARW_RIGHT: result:= 'ARW_RIGHT';
//{0000}ARW_BOTTOMLEFT: result:= 'ARW_BOTTOMLEFT';
{0001}SM_CYSCREEN: result:= 'SM_CYSCREEN';
//{0001}ARW_BOTTOMRIGHT: result:= 'ARW_BOTTOMRIGHT';
{0002}SM_CXVSCROLL: result:= 'SM_CXVSCROLL';
//{0002}ARW_TOPLEFT: result:= 'ARW_TOPLEFT';
{0003}SM_CYHSCROLL: result:= 'SM_CYHSCROLL';
//{0003}ARW_TOPRIGHT: result:= 'ARW_TOPRIGHT';
{0004}SM_CYCAPTION: result:= 'SM_CYCAPTION';
//{0004}ARW_DOWN: result:= 'ARW_DOWN';
//{0004}ARW_UP: result:= 'ARW_UP';
{0005}SM_CXBORDER: result:= 'SM_CXBORDER';
{0006}SM_CYBORDER: result:= 'SM_CYBORDER';
{0007}SM_CXDLGFRAME: result:= 'SM_CXDLGFRAME';//{0007}SM_CXFIXEDFRAME: result:= 'SM_CXFIXEDFRAME';
{0008}ARW_HIDE: result:= 'ARW_HIDE';
//{0008}SM_CYFIXEDFRAME: result:= 'SM_CYFIXEDFRAME';
//{0008}SM_CYDLGFRAME: result:= 'SM_CYDLGFRAME';
{0009}SM_CYVTHUMB: result:= 'SM_CYVTHUMB';
{0010}SM_CXHTHUMB: result:= 'SM_CXHTHUMB';
{0011}SM_CXICON: result:= 'SM_CXICON';{0012}SM_CYICON: result:= 'SM_CYICON';
{0013}SM_CXCURSOR: result:= 'SM_CXCURSOR';
{0014}SM_CYCURSOR: result:= 'SM_CYCURSOR';
{0016}SM_CXFULLSCREEN: result:= 'SM_CXFULLSCREEN';
{0015}SM_CYMENU: result:= 'SM_CYMENU';
{0017}SM_CYFULLSCREEN: result:= 'SM_CYFULLSCREEN';
{0018}SM_CYKANJIWINDOW: result:= 'SM_CYKANJIWINDOW';
{0019}SM_MOUSEPRESENT: result:= 'SM_MOUSEPRESENT';
{0020}SM_CYVSCROLL: result:= 'SM_CYVSCROLL';
{0021}SM_CXHSCROLL: result:= 'SM_CXHSCROLL';
{0022}SM_DEBUG: result:= 'SM_DEBUG';
{0023}SM_SWAPBUTTON: result:= 'SM_SWAPBUTTON';
{0028}SM_CXMIN: result:= 'SM_CXMIN';
{0029}SM_CYMIN: result:= 'SM_CYMIN';
{0030}SM_CXSIZE: result:= 'SM_CXSIZE';
{0031}SM_CYSIZE: result:= 'SM_CYSIZE';
{0032}SM_CXFRAME: result:= 'SM_CXFRAME';//{0032}SM_CXSIZEFRAME: result:= 'SM_CXSIZEFRAME';
{0033}SM_CYFRAME: result:= 'SM_CYFRAME';
//{0033}SM_CYSIZEFRAME: result:= 'SM_CYSIZEFRAME';{0034}SM_CXMINTRACK: result:= 'SM_CXMINTRACK';
{0035}SM_CYMINTRACK: result:= 'SM_CYMINTRACK';
{0036}SM_CXDOUBLECLK: result:= 'SM_CXDOUBLECLK';
{0037}SM_CYDOUBLECLK: result:= 'SM_CYDOUBLECLK';
{0038}SM_CXICONSPACING: result:= 'SM_CXICONSPACING';{0039}SM_CYICONSPACING: result:= 'SM_CYICONSPACING';
{0040}SM_MENUDROPALIGNMENT: result:= 'SM_MENUDROPALIGNMENT';
{0041}SM_PENWINDOWS: result:= 'SM_PENWINDOWS';
{0042}SM_DBCSENABLED: result:= 'SM_DBCSENABLED';
{0043}SM_CMOUSEBUTTONS: result:= 'SM_CMOUSEBUTTONS';
{0044}SM_SECURE: result:= 'SM_SECURE';
{0045}SM_CXEDGE: result:= 'SM_CXEDGE';
{0046}SM_CYEDGE: result:= 'SM_CYEDGE';
{0047}SM_CXMINSPACING: result:= 'SM_CXMINSPACING';
{0048}SM_CYMINSPACING: result:= 'SM_CYMINSPACING';
{0049}SM_CXSMICON: result:= 'SM_CXSMICON';
{0050}SM_CYSMICON: result:= 'SM_CYSMICON';
{0051}SM_CYSMCAPTION: result:= 'SM_CYSMCAPTION';{0052}SM_CXSMSIZE: result:= 'SM_CXSMSIZE';{0053}SM_CYSMSIZE: result:= 'SM_CYSMSIZE';
{0054}SM_CXMENUSIZE: result:= 'SM_CXMENUSIZE';
{0055}SM_CYMENUSIZE: result:= 'SM_CYMENUSIZE';
{0057}SM_CXMINIMIZED: result:= 'SM_CXMINIMIZED';{0056}SM_ARRANGE: result:= 'SM_ARRANGE';
{0058}SM_CYMINIMIZED: result:= 'SM_CYMINIMIZED';
{0059}SM_CXMAXTRACK: result:= 'SM_CXMAXTRACK';
{0060}SM_CYMAXTRACK: result:= 'SM_CYMAXTRACK';
{0061}SM_CXMAXIMIZED: result:= 'SM_CXMAXIMIZED';
{0062}SM_CYMAXIMIZED: result:= 'SM_CYMAXIMIZED';
{0063}SM_NETWORK: result:= 'SM_NETWORK';
{0067}SM_CLEANBOOT: result:= 'SM_CLEANBOOT';
{0068}SM_CXDRAG: result:= 'SM_CXDRAG';
{0069}SM_CYDRAG: result:= 'SM_CYDRAG';
{0070}SM_SHOWSOUNDS: result:= 'SM_SHOWSOUNDS';{0071}SM_CXMENUCHECK: result:= 'SM_CXMENUCHECK';{0072}SM_CYMENUCHECK: result:= 'SM_CYMENUCHECK';
{0073}SM_SLOWMACHINE: result:= 'SM_SLOWMACHINE';{0074}SM_MIDEASTENABLED: result:= 'SM_MIDEASTENABLED';{0075}SM_MOUSEWHEELPRESENT: result:= 'SM_MOUSEWHEELPRESENT'; // not defined in Ver 2.0	end;
end;

procedure TfrMetrics.sgMetricsKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
