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

unit uksyRegCheck;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}
uses
	Classes, uksyClasses, uksyPackReg;

{
--------------------------------------------------------------------------------
-------------------------- Thread ShareWare Checker ----------------------------
--------------------------------------------------------------------------------
}

type

	TKRegistryCheckThread = class( TThread )
	private
		FFatalReached: Boolean;
		FPackEnum: TKPackageEnum;

		procedure CheckMethod;

	{$IFDEF DELPHI4}
	protected  // In Delphi 4 you cannot alter to a lower visibility!
	{$ENDIF}
		procedure Execute; override;

	public
		constructor Create( pe: TKPackageEnum ); virtual;

		property PackageEnum: TKPackageEnum
						 read FPackEnum;

	end;

procedure TerminateResgitryCheckThread( Source: TKRegistryCheckThread );

{##NI##}

implementation

uses
	Windows, uksyResStr, uksyConsts, uksyUtils;

type

	EKRegCheck = class( EKSystem );

const
	REGISTRY_CHECK_INTERVAL: LongInt = SECOND_TO_MSECOND;

{
--------------------------------------------------------------------------------
-------------------------- Thread ShareWare Checker ----------------------------
--------------------------------------------------------------------------------
}

{ TKRegistryCheckThread }

constructor TKRegistryCheckThread.Create( pe: TKPackageEnum );
begin
	FPackEnum := pe;
	FFatalReached := False;
	FreeOnTerminate := True;
	inherited Create( False );
	Priority := tpIdle;
end;

procedure TKRegistryCheckThread.CheckMethod;
var
	s: string;
begin
	if ( not ( CurrentDelphi32Running or FFatalReached or Terminated ) ) then
	begin
		FFatalReached := True;
		try
		{ If the package is not in a system wide know directory an exception will be raised }
			s := GetPackageDescription( GetPackageName( FPackEnum ) );
		except
			s := GetPackageName( FPackEnum );
		end;
		FatalErrorFmt( sErrShareware, [s] );
		Halt; { In the case of a JMP Hacker! }
	end;
end;

procedure TKRegistryCheckThread.Execute;
begin
	while ( not Terminated ) do
	begin
		Synchronize( CheckMethod );
		SleepEx( REGISTRY_CHECK_INTERVAL, False );
	end;
end;

procedure TerminateResgitryCheckThread( Source: TKRegistryCheckThread );
begin
	ForceObject( Source );
	Source.Terminate;
	Source.WaitFor;
end;

end.
