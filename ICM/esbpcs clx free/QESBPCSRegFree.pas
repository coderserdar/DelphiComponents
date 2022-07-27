{: Contains the Registrations for the components in ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCSRegFree;

{$I esbpcs.inc}

interface

procedure Register;

implementation

uses
     Classes,
     {$IFDEF BelowD6}
     DsgnIntf,
     {$ELSE}
     DesignIntf, DesignEditors,
     {$ENDIF}
     { Globals}
     QESBPCSGlobals, QESBPCSGlobals2,
     { Edits }
     QESBPCSEdit, QESBPCSNumEdit;

procedure Register;
begin
     RegisterComponents ('ESBPCS', [TESBPCSEdit, TESBPosEdit, TESBIntEdit,
          TESBPosFloatEdit, TESBFloatEdit, TESBSciFloatEdit, TESBPercentEdit,
               TESBIPEdit, TESBHexEdit]);

end;

end.
