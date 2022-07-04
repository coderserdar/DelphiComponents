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

unit ukexdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  ExptIntf, ToolIntf;

{
--------------------------------------------------------------------------------
--------------------- Package Registration Information -------------------------
--------------------------------------------------------------------------------
}

procedure Register;

{
--------------------------------------------------------------------------------
----------------------- DLL Registration Information ---------------------------
--------------------------------------------------------------------------------
}

function InitExpert( AToolServices: TIToolServices; RegisterProc: TExpertRegisterProc;
	var Terminate: TExpertTerminateProc ): Boolean; stdcall;

implementation

uses
	Forms, uksyShortCuts, uksyUtils, uksydUtils, ukexConsts, ukexUtils, ukexClasses;

{
--------------------------------------------------------------------------------
--------------------- Package Registration Information -------------------------
--------------------------------------------------------------------------------
}

procedure Register;
begin

{ Experts }
	RegisterLibraryExpert( CreateAddInExpert( TKCommentExpert, iaAfter,
    sCommentTargetMenuPoint, sCommentMenuText, sCommentMenuName, '', SC_CTRL_SHIFT_M,
    0, 0, [mfEnabled,mfVisible] ) );
	RegisterLibraryExpert( CreateAddInExpert( TKAdjustComponentExpert, iaBefore,
    sAdjustCompTargetMenuPoint, sAdjustCompMenuText, sAdjustCompMenuName, '',
    SC_CTRL_SHIFT_A, 0, 0, [mfEnabled,mfVisible] ) );
	RegisterLibraryExpert( TKCollectionExpert.Create( TKICollectionModCreator ) );

{ Especial Registration Mechanism - Only for Packages }
	RegisterSpecialExperts;

end;

{
--------------------------------------------------------------------------------
----------------------- DLL Registration Information ---------------------------
--------------------------------------------------------------------------------
}

procedure Cleanup; export;
begin
	UnRegisterSpecialExperts;
end;

function InitExpert( AToolServices: TIToolServices; RegisterProc: TExpertRegisterProc;
	var Terminate: TExpertTerminateProc ): Boolean; stdcall;
begin
	Result := CheckObject( AToolServices );
	if Result then
	begin
		ExptIntf.ToolServices := AToolServices;
		if IsLibrary then
			Application.Handle := AToolServices.GetParentHandle;
		Terminate := CleanUp;	
		RegisterProc( CreateAddInExpert( TKCommentExpert, iaAfter, sCommentTargetMenuPoint,
			sCommentMenuText, sCommentMenuName, '', SC_CTRL_SHIFT_M, 0, 0, [mfEnabled,mfVisible] ) );
    RegisterProc( CreateAddInExpert( TKAdjustComponentExpert, iaBefore,
      sAdjustCompTargetMenuPoint, sAdjustCompMenuText, sAdjustCompMenuName, '',
      SC_CTRL_SHIFT_A, 0, 0, [mfEnabled,mfVisible] ) ); 
		RegisterProc( TKCollectionExpert.Create( TKICollectionModCreator ) );
		{ Cannot register Special Experts via DLL initialization... }
	end;	
end;

end.
