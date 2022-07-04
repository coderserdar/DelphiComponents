unit FormLocale;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids;

type
  TfrLocales = class(TForm)
    sgLocale: TStringGrid;
    sgSet: TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure sgLocaleClick(Sender: TObject);
	private
    { Private declarations }
  public
		{ Public declarations }
  end;

var
  frLocales: TfrLocales;

implementation

uses FormMain;

{$R *.DFM}

var
	StrLst: TStringList;

function GetLocaleData(ID: LCID; Flag: DWORD): string;
var
	BufSize: integer;
begin
	BufSize:= GetLocaleInfo(ID, Flag, nil, 0);
	SetLength(result, BufSize);
	GetLocaleInfo(ID, Flag, PChar(result), BufSize);
	SetLength(result, BufSize-1);
end;

function LocalesCallback(Name: PChar): BOOL; stdcall;
var
	LCID: integer;
begin
	LCID:= StrToInt('$' + Copy(Name, 5, 4));
	//if LCID > 0 then
	StrLst.Add(GetLocaleData(LCID, LOCALE_SLANGUAGE)+'='+IntToStr(LCID));
	Result:= BOOL(1);
end;

procedure TfrLocales.FormShow(Sender: TObject);
var
	i, j: integer;
	stLocale,
	stLCID: string;
begin
	sgLocale.RowCount:= 2;
	sgLocale.Cells[0,0]:= 'Locale Name';
	sgLocale.Cells[1,0]:= 'Locale Value';

	sgSet.Cells[0,0]:= 'Locale Flags';
	sgSet.Cells[1,0]:= 'Settings';

	StrLst:= TStringList.Create;
	try
		StrLst.Sorted:= True;
		EnumSystemLocales(@LocalesCallback, LCID_SUPPORTED);
		for i:= 0 to StrLst.Count - 1 do
		begin
			j:= sgLocale.RowCount-1;
			stLocale:= Copy(StrLst.Strings[i], 1, AnsiPos('=', StrLst.Strings[i])-1);
			stLCID:= Copy(StrLst.Strings[i], AnsiPos('=', StrLst.Strings[i])+1, Length(StrLst.Strings[i]));
			sgLocale.Cells[0, j]:= stLocale;
			sgLocale.Cells[1, j]:= stLCID;
			sgLocale.RowCount:= sgLocale.RowCount+1;
		end;
		sgLocaleClick(Sender);
	finally
		StrLst.Free;
		StrLst:= nil;
	end;
end;

procedure TfrLocales.sgLocaleClick(Sender: TObject);
var
	i: DWORD;
	j: integer;
	cStr: string;
begin
	i:= 0;
	with sgLocale do
	cStr:= Cells[Col+1, Row];

	with frMain.APITools1 do
	with sgSet do
	begin
		RowCount:= 2;
		while i <= LOCALE_STIMEFORMAT do
		begin
			if (LocaleStr(i) <> '') and (LocaleInfoStr(StrToInt(cStr), i) <> '') then
			begin
				j:= RowCount-1;
				Cells[0, j]:= LocaleStr(i);
				Cells[1, j]:= LocaleInfoStr(StrToInt(cStr), i);
				RowCount:= RowCount+1;
			end;
			Inc(i);
		end;
	end;
end;

end.
