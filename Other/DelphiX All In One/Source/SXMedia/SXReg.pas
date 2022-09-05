{
 SXMedia  Components - Beta 1
 --------------------------------
 Copyright 1999 Dean Ellis
 http://www.sillex.freeserve.co.uk

 This unit is part of the SXMedia Component Set. This code is
 supplied as is with no guarantees and must be used at your own
 risk.

 No modifications to this code must be made without the express
 permission of the author. Please report any problems to
 support@sillex.freeserve.co.uk

 You may use these components to create any freeware/shareware
 applications that you wish. If the components are to be used in
 a commercail product then credit for developement of these components
 should be given.

 Credits :

 Developer : Dean Ellis
 Testers   : Dominique Louis
             Ivan Blecic

}
unit SXReg;

{$INCLUDE DelphiXcfg.inc}

interface

uses Windows, Classes, SysUtils, SXEditor, SXMovie, SXEngine, SXModPlayer,
{$IfNDef VER6UP} DsgnIntf {$Else} Designintf, DesignEditors {$EndIf};

procedure Register;

implementation

{$R SXReg.dcr}


procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TFilename), nil, 'Filename', TFilenameProperty);
  RegisterComponentEditor(TSXMovie,TSXComponentEditor);
  RegisterComponentEditor(TSXEngine,TSXComponentEditor);
  RegisterComponentEditor(TSXModPlayer,TSXComponentEditor);
  RegisterComponents('SX Media',[TSXMovie, TSXEngine, TSXModPlayer]);
  RegisterClass(TScreenRect);
end;

end.
