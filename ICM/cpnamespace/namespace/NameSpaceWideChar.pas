//
// The original Delphi code is : NameSpaceWideChar.pas released 16.03.2003
// Last version: 0.88 released 28.09.2003
// The initial developer is Cedomir Plavljanic (cedomirp@yahoo.com)
// Copyright (C) 2003-2004 Cedomir Plavljanic
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// This unit contains Wide Upper and Lower Function
// warning - I don't use overload directive (not support for Delphi 4 for WideString and String)
//
// version: 0.88
// fix bug in NSWideUpperCaseW, NSWideUpperCaseA, NSWideLowerCaseW, NSWideLowerCaseA
//
// version: 0.86
// fix bug in NSWideFirstUpperW and NSWideFirstUpperA
//
// version: 0.83
//

unit NameSpaceWideChar;

{$ifdef  Ver140}     // For Delphi6 suggestion Anders Lee
	{$J+}
{$endif}

interface

function NSWideUpperCaseW(const S: WideString): WideString;
function NSWideUpperCaseA(const S: String): String;
function NSWideLowerCaseW(const S: WideString): WideString;
function NSWideLowerCaseA(const S: String): String;
function NSWideFirstUpperW(const S: WideString): WideString;
function NSWideFirstUpperA(const S: String): String;

implementation

uses
	Windows, SysUtils;

const
	user32    = 'user32.dll';

//return zero if not exist on OS
function CharUpperBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD; stdcall; external user32 name 'CharUpperBuffW';
//return zero if not exist on OS
function CharLowerBuffW(lpsz: PWideChar; cchLength: DWORD): DWORD; stdcall; external user32 name 'CharLowerBuffW';

function NSWideUpperCaseW(const S:WideString):WideString;

var
	Len:Integer;

begin
	//add in v 0.88
	if Length(S)=0 then begin Result:='';Exit end;

	Len:=Length(S);
	SetString(Result,PWideChar(S),Len);
	if Len>0 then CharUpperBuffW(Pointer(Result),Len);
end;

function NSWideUpperCaseA(const S:String):String;

var
	Len:Integer;
	w:WideString;

begin
	//add in v 0.88
	if Length(S)=0 then begin Result:='';Exit end;

	Len:=Length(S)*Sizeof(WideChar);
	SetLength(w,Len);
	StringToWideChar(s,PWideChar(w),Len);
	if Len>0 then
		if CharUpperBuffW(Pointer(w),Len)=0 then
			Result:=AnsiUpperCase(S)
		else
			Result:=WideCharToString(PWideChar(w))
	else
		Result:='';
end;

function NSWideLowerCaseW(const S:WideString):WideString;

var
	Len:Integer;

begin
	Len:=Length(S);
	//add in v 0.88
	if Len=0 then begin Result:='';Exit;end;

	SetString(Result,PWideChar(S),Len);
	if Len>0 then CharLowerBuffW(Pointer(Result),Len);
end;

function NSWideLowerCaseA(const S:String):String;

var
	Len:Integer;
	w:WideString;

begin
	//add in v 0.88
	if Length(S)=0 then begin Result:='';Exit end;

	Len:=Length(S)*Sizeof(WideChar);
	SetLength(w,Len);
	StringToWideChar(s,PWideChar(w),Len);
	if Len>0 then
		if CharLowerBuffW(Pointer(w),Len)=0 then
			Result:=AnsiLowerCase(S)
		else
			Result:=WideCharToString(PWideChar(w))
	else
		Result:='';
end;

function NSWideFirstUpperW(const S: WideString): WideString;

begin
	//add in v 0.86
	if Length(S)=0 then begin Result:='';Exit end;
	//add in v 0.86
	if Length(S)=1 then
		Result:=NSWideUpperCaseW(S[1])
	else
		//old
		Result:=NSWideUpperCaseW(S[1])+NSWideLowerCaseW(Copy(S, 2, Maxint));
end;

function NSWideFirstUpperA(const S: String): String;

begin
	//add in v 0.86
	if Length(S)=0 then begin Result:='';Exit end;
	//add in v 0.86
	if Length(S)=1 then
		Result:=NSWideUpperCaseA(S[1])
	else
		//old
		Result:=NSWideUpperCaseA(S[1])+NSWideLowerCaseA(Copy(S, 2, Maxint));
end;


end.
