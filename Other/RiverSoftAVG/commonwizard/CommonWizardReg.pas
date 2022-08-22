unit CommonWizardReg;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//      07/2002 T. Grubb
//		Initial version.
//
//      File Contents:
//           Registration unit for Common Wizards
//
//--- Warning ----------------------------------------------------------------
//	This software is property of RiverSoftAVG. Unauthorized use or
//      duplication of this software is strictly prohibited. Authorized users
//      are subject to the following restrictions:
//	*	RiverSoftAVG is not responsible for
//		any consequence of the use of this software.
//	*	The origin of this software must not be misrepresented either by
//		explicit claim or by omission.
//	*	Altered versions of this software must be plainly marked as such.
//	*	This notice may not be removed or altered.
//
//      © 2002, Thomas G. Grubb
//
//=== End File Prolog ========================================================

interface

{$ifndef VER130} {$ifndef VER140} {$ifndef VER150} {$ObjExportAll on} {$endif} {$endif} {$endif}

procedure Register;

implementation

uses
    CollectionWizard, ExptIntf;

procedure Register;
begin
  RegisterLibraryExpert(TNewCollectionWizard.Create);

end;

end.
