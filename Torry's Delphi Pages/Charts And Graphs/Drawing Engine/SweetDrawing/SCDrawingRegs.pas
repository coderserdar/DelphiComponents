{*******************************************************}
{                                                       }
{         CA SweetDrawing Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingRegs;

{$I SweetDrawing.inc}

interface

uses
  {$IFDEF SCDE_DELPHI6_UP} Types, DesignEditors, DesignIntf {$ELSE}
  TypInfo, DsgnIntf {$ENDIF};

procedure Register;

implementation

{$R *.dcr}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  SCDrawingSurface, SCDrawingEditor, SCDeLayerManager;

procedure Register;
begin
  RegisterComponents('Sweet Drawing Engine', [TSCDeViewer, TSCDeEditor,
    TSCDeLayerManager]);
end;

end.
