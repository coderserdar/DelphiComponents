unit EanReg;

interface

{$I ean.inc}

uses EanKod,{$ifdef PSOFT_BARCODE_DB} EanDB, {$endif}
        {$ifdef MSWINDOWS}
            {$ifdef PSOFT_QREPORT}
                EanQR,
                {$ifdef PSOFT_BARCODE_DB}EanDBQr,{$endif}
            {$endif}
            EanRead,
        {$endif}
        Classes;

procedure Register;


implementation


const PageEan = 'Barcode components';
procedure Register;
begin
     RegisterComponents(PageEan ,[TEan {$ifdef PSOFT_BARCODE_DB},TDBEan {$endif}]);
     {$ifdef MSWINDOWS}
        RegisterComponents(PageEan ,[TBarcodeReader]);
        {$ifdef PSOFT_QREPORT}
                RegisterComponents(PageEan ,[TQREan{$ifdef PSOFT_BARCODE_DB}, TQRDBEan{$endif}]);
        {$endif}
     {$endif}
end;

end.
