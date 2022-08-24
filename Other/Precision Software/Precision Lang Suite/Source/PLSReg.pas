{$IFNDEF PLSFMX}
unit PLSReg;
{$ENDIF}

interface

uses  
  Classes
  {$IFDEF FPC}
    , lresources, plsController
  {$ELSE}
    {$IFDEF PLSFMX}
      {$IF CompilerVersion >= 25.0 }
      , FMX.Controls,
      {$ELSE}
      , FMX.Types,
      {$IFEND}
      FMX.plsController
    {$ELSE}
      {$IF CompilerVersion >= 23.0 }
        , Vcl.Controls
      {$IFEND}
      , plsController
    {$ENDIF}
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
const
  pal = 'Precision Language Suite';
begin
  {$IFNDEF FPC}
    {$IFDEF PLSFMX}
      {$IF CompilerVersion >= 25.0 }
        GroupDescendentsWith(TplsController, FMX.Controls.TControl);
      {$ELSE}
        GroupDescendentsWith(TplsController, FMX.Types.TControl);
      {$IFEND}
    {$ELSE}
      {$IF CompilerVersion >= 23.0 }
        GroupDescendentsWith(TplsController, Vcl.Controls.TControl);
      {$IFEND}
    {$ENDIF}
  {$ENDIF}
  RegisterComponents(pal, [TplsController]);
end;

{$IFDEF FPC}
initialization
  {$I plsController.lrs}
{$ENDIF}

end.

