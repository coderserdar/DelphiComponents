// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  15924: uTwnDlgHandle.pas 
//
//    Rev 1.2    2014-01-15 13:42:06  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.1    2013-12-04 23:16:18  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.0    04-12-2001 16:49:14  mcm    Version: DT 2.0

unit uTwnDlgHandle;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows;
     {$ELSE}
     Windows;
     {$ENDIF}

var   hDlg            : array[0..64] of THandle;
      DlgCount        : integer;

procedure AddDlgHandle(DlgHandle : THandle);

procedure DeleteDlgHandle(DlgHandle : THandle);


implementation

procedure AddDlgHandle(DlgHandle : THandle);
begin
  hDlg[DlgCount] := DlgHandle;
  Inc(DlgCount);
end; { End AddDlgHandle.                                                       }


procedure DeleteDlgHandle(DlgHandle : THandle);
var i : integer;
begin
  i := 0;
  while (i < DlgCount) and (hDlg[i] <> DlgHandle)
  do inc(i);

  if (hDlg[i] <> DlgHandle)
  then begin
       hDlg[i] := 0;
       while (i < DlgCount)
       do begin
          hDlg[i] := hDlg[i+1];
          hDlg[i+1] := 0;
          inc(i);
       end;
       dec(DlgCount);
  end;
end; { End DeleteDlgHandle.                                                    }


end.
