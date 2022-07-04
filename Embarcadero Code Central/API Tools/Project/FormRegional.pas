unit FormRegional;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids;

type
  TfrRegional = class(TForm)
    sgLocale: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure sgLocaleKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
	public
    { Public declarations }
  end;

var
  frRegional: TfrRegional;

implementation

uses FormMain;

{$R *.DFM}

procedure TfrRegional.FormShow(Sender: TObject);
var
{$ifndef VER90}
	i: LongWord;
{$else}
	i: integer;
{$endif}
	j: integer;
begin
	i:= 0;
	sgLocale.RowCount:= 2;
	sgLocale.Cells[0,0]:= 'Idx';
	sgLocale.Cells[1,0]:= 'LCTYPE Constants';
	sgLocale.Cells[2,0]:= 'Default Settings';
	sgLocale.Cells[3,0]:= 'User Settings';
	with frMain.APITools1 do
	// More info read LCTYPE Constants from Win32 Programmers Ref. Help File
	while i <= LOCALE_STIMEFORMAT do
	begin
		if LocaleStr(i) <> '' then
		begin
			j:= sgLocale.RowCount-1;
			sgLocale.Cells[0,j]:= IntToStr(i);	// LCTYPE Constant Index
			sgLocale.Cells[1,j]:= LocaleStr(i);	// LCTYPE Constant
			sgLocale.Cells[2,j]:= LocaleInfo(i or LOCALE_NOUSEROVERRIDE);	// Default settings
			sgLocale.Cells[3,j]:= LocaleInfo(i);	// User settings
			sgLocale.RowCount:= sgLocale.RowCount+1;
		end;
		Inc(i);
	end;
end;

procedure TfrRegional.sgLocaleKeyPress(Sender: TObject; var Key: Char);
begin
	if Key = #27 then
	Close;
end;

end.
