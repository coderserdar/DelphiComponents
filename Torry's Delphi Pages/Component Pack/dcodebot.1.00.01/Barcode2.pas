unit Barcode2;
{
Printer scaling functions

Copyright 1998-2000 Andreas Schmidt


use this functions only between
printer.BeginDoc and printer.EndDoc

}


interface

uses Printers;



function ConvertMmToPixelsX(const Value:Double):Integer;
function ConvertMmToPixelsY(const Value:Double):Integer;
function ConvertInchToPixelsX(const Value:Double):Integer;
function ConvertInchToPixelsY(const Value:Double):Integer;



implementation

uses Windows;

const mmPerInch = 25.4;


function GetPrinterRes(const pobj: TPrinter; Horz: Boolean): integer;
var
   Index: Integer;
begin
   if Horz then
      Index:=LOGPIXELSX
   else
      Index:=LOGPIXELSY;
   Result:=GetDeviceCaps(pobj.Handle, Index);
end;


function ConvertMMtoPixelsX(const Value:Double):Integer;
begin
   Result := Round(Value*GetPrinterRes(Printer, True) / mmPerInch);
end;

function ConvertMMtoPixelsY(const Value:Double):Integer;
begin
   Result := Round(Value*GetPrinterRes(Printer, False) / mmPerInch);
end;

function ConvertInchtoPixelsX(const Value:Double):Integer;
begin
   Result := Round(Value*GetPrinterRes(Printer, True));
end;

function ConvertInchtoPixelsY(const Value:Double):Integer;
begin
   Result := Round(Value*GetPrinterRes(Printer, False));
end;


end.
