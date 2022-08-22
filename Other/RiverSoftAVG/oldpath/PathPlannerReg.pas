unit PathPlannerReg;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//	10/2000	T. Grubb
//
//		Initial version.
//                   
//      File Contents:
//           Registration unit for Path Planner
//
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
//      This software is freeware.  You are authorized to duplicate and modify
//      this software subject to the restrictions above.
//
//=== End File Prolog ========================================================

interface

procedure Register;

implementation

uses
    Classes, PathPlanner;

procedure Register;
begin
  RegisterComponents('AI', [TAStarPathPlanner]);
  RegisterComponents('AI', [TSimplePathPlanner]);
  RegisterComponents('AI', [TSearchableMap]);
  RegisterComponents('AI', [TStateFactory]);
end;

end.
