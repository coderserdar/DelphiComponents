{================================================================================
Copyright (C) 1997-2002 Mills Enterprise

Unit     : rmRegisterEx
Purpose  : This is the registration unit for all of the "rm" EX Controls.
Date     : 06-02-1999
Author   : Ryan J. Mills
Version  : 1.90
================================================================================}

unit rmRegisterEx;

interface

{$I CompilerDefines.INC}

uses rmZLIBDataStorage, rmDataStorage, rmDataStoragePropEdit;

procedure Register;

implementation

uses
{$ifdef d6_or_higher}
   Classes, DesignIntf, DesignEditors, ExptIntf;
{$else}
   classes, DsgnIntf, ExptIntf;
{$endif}

const
     PalettePage = 'rmControlsEx';

procedure Register;
begin
   RegisterComponents(PalettePage,[TrmZLIBDataStorage]);
   RegisterPropertyEditor(TypeInfo(TrmDataStorageLongint), TrmZLIBDataStorage, 'OriginalSize', TrmDataLongintProperty);
end;

end.
