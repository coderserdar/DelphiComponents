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

unit ukrdUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, EditIntf, DsgnIntf, uksydUtils, ukrdClasses, ukrClasses;

type

	EKRDUtils = class( EKDKernel );

{
--------------------------------------------------------------------------------
--------------------------- Generic RTTI Routines ------------------------------
--------------------------------------------------------------------------------
}

function SetCompListPubPropsEx( CompList: TComponentList; PropValueList: TKPropList;
  ProcessChildren: Boolean ): Boolean;

{
--------------------------------------------------------------------------------
------------------------- Generic Resource Routines ----------------------------
--------------------------------------------------------------------------------
}

function MakeResName( const Str: string ): PChar;
function FormatName( Name: PChar ): string;
procedure CreateIconResourceFromFile( const FileName, ResName: string; ResNames: TStrings );
function CreateStringResource( const ResData, ResName: string ): TIResourceEntry;

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestKernelDsgnShareWareVersion;

{##NI##}

implementation

uses
	Windows, SysUtils, ExptIntf, uksyResStr, uksyConsts, uksyTypes, uksyUtils,
	uksyPackReg, uksydClasses, ukrUtils; 

{
--------------------------------------------------------------------------------
--------------------------- Generic RTTI Routines ------------------------------
--------------------------------------------------------------------------------
}

function SetCompListPubPropsEx( CompList: TComponentList;
	PropValueList: TKPropList; ProcessChildren: Boolean ): Boolean;
var
	i: Integer;
begin
	ForceObjects( [CompList, PropValueList] );
	Result := True;
  for i := 0 to PropValueList.Count-1 do
		Result := Result and SetCompListPubProp( CompList, PropValueList.PropNames[i],
			PropValueList.Items[i]^, ProcessChildren );
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Resource Routines ----------------------------
--------------------------------------------------------------------------------
}

{ Return a resource pointer, given a string. If the string
	starts with #, it must be a number, so convert the rest
	of the string to an integer. Otherwise it's a name, so
	return a pointer to the string as a PChar. }
function MakeResName( const Str: string ): PChar;
begin
	if ( not CheckStr( Str ) ) then
		Result := nil
	else if ( Str[1] = '#' ) then
		Result := PChar( StrToInt( Copy( Str, 2, Length( Str ) ) ) )
	else
		Result := PChar( Str );
end;

{ Format a resource name as a readable string. }
function FormatName( Name: PChar ): string;
begin
	if LongRec( Name ).Hi = 0 then
		Result := Format( '#%d', [Integer( Name )] )
	else
		Result := StrPas( Name )
end;

procedure CreateIconResourceFromFile( const FileName, ResName: string;
	ResNames: TStrings );
var
  PrjMod: TIModuleInterface;
  ResFile: TIResourceFile;
  ResEntry: TIResourceEntry;
	i,
  MaxID,
  ResID,
  ResHdrSize: Integer;
  ms: TMemoryStream;
  fs: TFileStream;
  rs: TKIResEntryStream;
	FileHdr: PKFileGroupHeader;
	ResHdr: PKGroupHeader;
begin
	ForceObject( ResNames );
  with ToolServices do
		PrjMod := GetModuleInterface( GetProjectName );
  try
    ResFile := PrjMod.GetProjectResource;
		try
      MaxId := 0;
			for I := 0 to ResFile.GetEntryCount-1 do
			begin
        ResEntry := ResFile.GetEntry(I);
        try
          if ( ResEntry.GetResourceType = RT_ICON ) then
          begin
            ResId := Integer( ResEntry.GetResourceName );
            if ( LongRec( ResID ).Hi = 0) and ( ResID > MaxId ) then
              MaxId := ResID;
          end;
        finally
          ResEntry.Free;
        end;
      end;

      Inc( MaxID );

			ms := TMemoryStream.Create;
      try
				fs := TFileStream.Create( Filename, fmOpenRead or fmShareDenyWrite);
        try
          ms.CopyFrom(fs, 0);
        finally
					fs.Free;
        end;

				FileHdr := PKFileGroupHeader( ms.Memory );
				ResHdrSize := ( 3 * SizeOf( Word ) +
					( FileHdr^.wCount * SizeOf( TKGroupItem ) ) );
				GetMem( ResHdr, ResHdrSize );
				try
					FillChar( ResHdr^, SizeOf( TKGroupHeader ), 0 );

				 {ResHdr^.wReserved := 0;}
					ResHdr^.wType  := FileHdr^.wType;
					ResHdr^.wCount := FileHdr.wCount;

					for i := 0 to FileHdr^.wCount-1 do
					begin
						Move( FileHdr^.Items[I], ResHdr^.Items[I], SizeOf( TKGroupItem ) );
						ResHdr^.Items[i].wNameOrdinal := MaxId;

						ResEntry := ResFile.CreateEntry( RT_ICON, PChar( MaxId ),
							0, GetUserDefaultLangID, 0, 0, 0 );

						ResEntry.SetDataSize( ResHdr^.Items[i].dwBytesInRes );

						ResNames.AddObject( PChar( MaxID ) + CH_EQUAL_TOKEN + PChar( RT_ICON ), ResEntry );

						rs := TKIResEntryStream.Create( ResEntry );
						try
							ms.Position := FileHdr^.Items[i].dwOffSet;
							rs.CopyFrom( ms, FileHdr^.Items[i].dwSize );
						finally
							rs.Free;
						end;

						Inc( MaxId );

					end;

					ResEntry := ResFile.CreateEntry( RT_GROUP_ICON,
						PChar( ResName ), 0, GetUserDefaultLangID, 0, 0, 0 );

					ResEntry.SetDataSize( ResHdrSize );

					rs := TKIResEntryStream.Create( ResEntry );
					try
						rs.WriteBuffer( ResHdr^, ResHdrSize );
					finally
						rs.Free;
					end;

					ResNames.AddObject( PChar( ResName ) + CH_EQUAL_TOKEN + PChar( RT_GROUP_ICON ), ResEntry );

				finally
					FreeMem( ResHdr, ResHdrSize );
				end;

			finally
				ms.Free;
			end;

		finally
			ResFile.Free;
		end;

  finally
    PrjMod.Free;
  end;
end;

function CreateStringResource( const ResData, ResName: string ): TIResourceEntry;
var
  PrjMod: TIModuleInterface;
  ResFile: TIResourceFile;
  rs: TKIResEntryStream;
  ss: TStringStream;
begin
  Result := nil;
  if ( Length( ResData ) = 0 ) then
    Exit;
  with ToolServices do
		PrjMod := GetModuleInterface( GetProjectName );
  try
    ResFile := PrjMod.GetProjectResource;
    try      
      Result := ResFile.CreateEntry( RT_STRING,
				PChar( ResName ), 0, GetUserDefaultLangID, 0, 0, 0 );

      Result.SetDataSize( Length( ResData ) );

      rs := TKIResEntryStream.Create( Result );
      try

        ss := TStringStream.Create( ResData );
        try
          rs.CopyFrom( ss, 0 );
        finally
          ss.Free;
        end;

      finally
        rs.Free;
      end;

    finally
			ResFile.Free;
		end;
	finally
		PrjMod.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestKernelDsgnShareWareVersion;
begin
	if ( IsKernel_Shareware and CurrentDelphi32Running ) then
		RaiseExceptionFmt( EKRDUtils, sErrShareWare, [GetPackageName( pedKernel )] );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
