unit API_paintbox;

//------------------------------------------------------------------------------
// API_PAINTBOX that just brings custom paintbox into VCL components listing
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Messages, API_base;

type
  TAPI_Paintbox = class(TAPI_Custom_PaintBox)
  private
    { Private declarations }
    fEraseBackground: boolean;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  published
    { Published declarations }
    property EraseBackground: boolean read fEraseBackground write fEraseBackground default TRUE;
  end;

procedure Register;

implementation

//------------------------------------------------------------------------------
procedure TAPI_paintbox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if fEraseBackground then inherited;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_Paintbox]);
end;

end.
