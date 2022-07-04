{
=======================================================================

    KLIB v100
    Serious Software Made in Brazil


    home-page: www.knowhow-online.com.br (sorry, just portuguese)
    authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

    Released under the Netscape Public License Version 1.0 
   (see license.txt)

    Unless otherwise noted, all materials provided in this release
    are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit shellutils;

interface

function ShellAddToRecentDocs( const FileName: string ): Boolean;
function ShellBindExtension( const FExt, FIcon, FDesc, FApp, FCmd: string ): Boolean;
function ShellUnbindExtension( const FExt: string ): Boolean;

implementation

uses SysUtils, Windows, Registry, ShlObj;

function GetOSVer: TOSVersionInfo;
begin
	ZeroMemory( @Result, SizeOf( TOSVersionInfo ) );
	Result.dwOSVersionInfoSize := SizeOf( TOSVersionInfo );
	GetVersionEx( Result );
end;

function CheckWinNT: Boolean;
begin
	Result := ( GetOSVer.dwPlatformId = VER_PLATFORM_WIN32_NT );
end;

function ShellAddToRecentDocs( const FileName: string ): Boolean;
begin
	Result := false;
	if ( not CheckWinNT ) then
	begin
		Result := true;
		SHAddToRecentDocs( SHARD_PATH, PChar( FileName ) );
	end;
end;

function ShellBindExtension( const FExt, FIcon, FDesc, FApp, FCmd: string ): Boolean;
var
	reg: TRegistry;
	sExt,
	sApp,
	sCmd,
	sKey,
	sIco,
	sDesc: string;
begin
	Result := false;
	sExt := Trim( AnsiLowerCase( FExt ) );
	if ( sExt = '' ) or ( Pos( '.', sExt ) > 0 ) then
		Exit;
	sApp := Trim( AnsiLowerCase( FApp ) );
	if ( sApp = '' ) then
		Exit;
	sCmd := Trim( FCmd );
	if ( sCmd = '' ) then
		sCmd := ' %1';
	sIco := Trim( AnsiLowerCase( FIcon ) );
	if ( sIco = '' ) then
		sIco := sApp;
	sKey := AnsiUpperCase( sExt ) + 'File';
	sDesc := Trim( FDesc );
	reg := TRegistry.Create;
	try
		sExt := '.' + sExt;
		reg.RootKey := HKEY_CLASSES_ROOT;
		Result := reg.OpenKey( sExt, true );
		if Result then
		begin
			reg.WriteString( '', sKey );
			Result := reg.OpenKey( '\' + sKey, true );
			if Result then
			begin
				reg.WriteString( '', sDesc );
				Result := reg.OpenKey( '\' + sKey + '\DefaultIcon', true );
				if Result then
				begin
					reg.WriteString( '', sIco );
					Result := reg.OpenKey( '\' + sKey + '\Shell\Open\Command', true );
					if Result then
						reg.WriteString( '', sApp + ' ' + sCmd );
				end;
			end;
		end;
	finally
		reg.Free;
	end;
end;

function ShellUnbindExtension( const FExt: string ): Boolean;
var
	reg: TRegistry;
	sExt,
	sKey: string;
begin
	Result := false;
	sExt := Trim( AnsiLowerCase( FExt ) );
	if ( sExt = '' ) or ( Pos( '.', sExt ) > 0 ) then
		Exit;
	reg := TRegistry.Create;
	try
		sExt := '.' + sExt;
		reg.RootKey := HKEY_CLASSES_ROOT;
		Result := reg.OpenKey( sExt, false );
		if Result then
		begin
			sKey := reg.ReadString( '' );
			Result := true;
			Result := Result and reg.DeleteKey( '\' + sKey + '\DefaultIcon' );
			Result := Result and reg.DeleteKey( '\' + sKey + '\Shell\Open\Command' );
			Result := Result and reg.DeleteKey( '\' + sKey + '\Shell\Open' );
			Result := Result and reg.DeleteKey( '\' + sKey + '\Shell' );
			Result := Result and reg.DeleteKey( '\' + sKey );
			Result := Result and reg.DeleteKey( '\' + sExt );
		end;
	finally
		reg.Free;
	end;
end;

end.
