
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Registration unit            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}


unit FR_reg;

interface

{$I FR.inc}

procedure Register;

implementation

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFDEF Delphi6}
    DesignIntf, DesignEditors, DesignWindows, DsnConst,
  {$ELSE}
    DsgnIntf,
  {$ENDIF}
  Dialogs, FR_Class, FR_DSet, FR_DBSet,
  FR_OLE, FR_Rich, FR_ChBox, FR_Shape, FR_BarC, FR_RRect,
  FR_Desgn, FR_View, FR_Dock, FR_Ctrls, FR_DBOp,
  FR_E_TXT, FR_E_RTF, FR_E_CSV, FR_E_HTM, FR_Const
  {$IFDEF TeeChart}, FR_Chart {$ENDIF}
  {$IFDEF RX}, FR_RxRTF {$ENDIF};

{-----------------------------------------------------------------------}
type
  TfrRepEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
    procedure DoDesign;
  end;

procedure TfrRepEditor.ExecuteVerb(Index: Integer);
begin
  DoDesign;
end;

function TfrRepEditor.GetVerb(Index: Integer): String;
begin
  Result := LoadStr(SDesignReport);
end;

function TfrRepEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TfrRepEditor.DoDesign;
begin
  TfrReport(Component).DesignReport;
  if frDesigner <> nil then
    if TfrReportDesigner(frDesigner).Modified then
      Designer.Modified;
end;

{-----------------------------------------------------------------------}
procedure Register;
begin
  RegisterComponents('FreeReport',
    [TfrReport, TfrCompositeReport, TfrDBDataSet, TfrUserDataset,
     TfrOLEObject, TfrRichObject, TfrCheckBoxObject,
     TfrShapeObject, TfrBarCodeObject,
     {$IFDEF TeeChart} TfrChartObject, {$ENDIF}
     {$IFDEF RX} TfrRxRichObject, {$ENDIF}
     TfrRoundRectObject,
     TfrTextExport, TfrRTFExport, TfrCSVExport, TfrHTMExport,
     TfrDesigner,
     TfrPreview]);
  RegisterComponents('FR Tools',
    [TfrSpeedButton, TfrDock, TfrToolBar,
     TfrTBButton, TfrTBSeparator, TfrTBPanel, TfrOpenDBDialog]);
  RegisterComponentEditor(TfrReport, TfrRepEditor);
end;

end.
