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

unit ukfdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	DsgnIntf, ExptIntf, Classes, Menus, Forms, uksyUtils, ukrDBCtrls, ukfClasses,
	ukfCtrls, ukfConsts, ukfdClasses, ukfdConsts;

procedure Register;
begin
	RegisterNoIcon( [TKMainMenu, TKFormDBNavigator] );
	RegisterClasses( [TKMainMenu, TKFormDBNavigator] );

	RegisterPropertyEditor( TypeInfo( String ), TKPersistenceItem, 'PropertyName', TKPropertyNameProperty );
	RegisterPropertyEditor( TypeInfo( String ), TKPersistenceItem, 'ComponentName', TKComponentNameProperty );
	RegisterPropertyEditor( TypeInfo( String ), TKPersistenceItem, 'DefaultValue', TKDefaultValueProperty );
	RegisterPropertyEditor( TypeInfo( TMainMenu ), TKMDIForm, 'Menu', TKMainMenuProperty );
	RegisterPropertyEditor( TypeInfo( TFormStyle ), TKSimpleMDI, 'FormStyle', TKMDIFormStyleProperty );
	RegisterPropertyEditor( TypeInfo( TFormStyle ), TKChild, 'FormStyle', TKChildFormStyleProperty );
	RegisterPropertyEditor( TypeInfo( TPosition ), TKChild, 'Position', TKPositionProperty );

	RegisterCustomModule( TKForm, TKFormCustomModule );
	RegisterCustomModule( TKSimpleMDI, TKFormCustomModule );
	RegisterCustomModule( TKMDIForm, TKFormCustomModule );
	RegisterCustomModule( TKChild, TKFormCustomModule );
	RegisterCustomModule( TKNavMDIForm, TKFormCustomModule );
	RegisterCustomModule( TKDBChild, TKFormCustomModule );

	RegisterLibraryExpert( TKFormExpert.Create );
	RegisterLibraryExpert( TKSimpleMDIExpert.Create );
	RegisterLibraryExpert( TKMDIFormExpert.Create );
	RegisterLibraryExpert( TKChildExpert.Create );
	RegisterLibraryExpert( TKNavMDIFormExpert.Create );
	RegisterLibraryExpert( TKDBChildExpert.Create );

	GenerateKnowHowMenu;
end;

end.
