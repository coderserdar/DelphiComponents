unit API_printer;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, Graphics, Printers;

type
  TAPI_printer = class(TComponent)
  private
    fversion: string;

    // report
    fprinter: string;
    fdriver: string;

    fprinterindex: integer;
    fprinterlist: tstringlist;

    // page
    ftitle: string;
    fheight: integer;
    fwidth: integer;
    fcopies: integer;
    forientation: tprinterorientation;

    procedure setcopies(i: integer);
    procedure setorientation(o: tprinterorientation);
    procedure dummys(s: string);
    procedure dummyi(i: integer);
    procedure setprinterindex(i: integer);

  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;

    function Printing: boolean;
    procedure Refresh;
    function PrintPage (canvas: tcanvas): boolean;

  published
    property Version: string read fversion write dummys stored false;

    // printers
    property PrinterIndex: integer read fprinterindex write setprinterindex;
    property Printers: tstringlist read fprinterlist;

    // report
    property PageTitle: string read ftitle write ftitle;
    property PageHeight: integer read fheight write dummyi stored false;
    property PageWidth: integer read fwidth write dummyi stored false;

    property Copies: integer read fcopies write setcopies;
    property Orientation: tprinterorientation read forientation write setorientation;

  end;

procedure Register;

implementation

{$r *.res}

const
  versioninfostring: string = 'r1.00/ari.pikivirta@kolumbus.fi';

procedure TAPI_printer.dummys(s: string); begin end;
procedure TAPI_printer.dummyi(i: integer); begin end;

//------------------------------------------------------------------------------
procedure TAPI_printer.Refresh;
begin
  printer.Refresh;

  fprinterindex:=printer.PrinterIndex;
  fprinterlist.clear;
  fprinterlist.addstrings(printer.Printers);

  if printerindex>-1 then
  begin
    fprinter:=fprinterlist[fprinterindex];
    fdriver:='???';
    fheight:=printer.PageHeight;
    fwidth:=printer.PageWidth;
    fcopies:=printer.Copies;
    forientation:=printer.Orientation;
  end else
  begin
    fprinter:='';
    fdriver:='';
    fheight:=0;
    fwidth:=0;
    fcopies:=0;
  end;
end;

//------------------------------------------------------------------------------
constructor TAPI_printer.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfostring;
  ftitle:='API_printer';
  fprinterlist:=tstringlist.create;
  Refresh;
end;

//------------------------------------------------------------------------------
destructor TAPI_printer.destroy;
begin
  fprinterlist.Free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_printer.setprinterindex(i: integer);
begin
  if i<printer.Printers.Count then
  begin
    fprinterindex:= i;
    printer.PrinterIndex:= fprinterindex;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_printer.PrintPage (canvas: tcanvas): boolean;
begin
  result:=false;
  if fPrinterIndex<0 then exit;

  printer.PrinterIndex:= fprinterindex;
  printer.Title:=ftitle;
  printer.BeginDoc;
  printer.Canvas.CopyRect(printer.Canvas.ClipRect, canvas, canvas.ClipRect);
  printer.EndDoc;

  result:=not printer.Aborted;
end;

//------------------------------------------------------------------------------
procedure TAPI_printer.setcopies(i: integer);
begin
  if i>0 then
  begin
    fcopies:=i;
    printer.Copies:=i;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_printer.printing: boolean;
begin
  result:=printer.Printing;
end;

//------------------------------------------------------------------------------
procedure TAPI_printer.setorientation(o: tprinterorientation);
begin
  forientation:=o;
  printer.Orientation:=forientation;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_printer]);
end;

end.
