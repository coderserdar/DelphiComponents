{--------------------------------------------------------------------------------
* Description : Property & Component Editors of TscExcelExport
* Dates : February 2000 - June 2007
* Version : 3.6 (Delphi 5, 6, 7, 2005, 2006, Turbo Delphi and 2007)

* Author : Stefan Cruysberghs
* Email : stefancr@scip.be
* Website : http://www.scip.be
  Visit SCIP.be for other components, tools and articles
--------------------------------------------------------------------------------
* This component is free of charge.
* The author doesn't give a warranty for error free running
  of this component and he doesn't give any support.
* Suggestions and bugs can be send by email.
--------------------------------------------------------------------------------
Installation
* Open the run-time package (X=Delphi version) ExcelExportPackX.dpr and build it
* Open the design-time package (X=Delphi version) dclExcelExportPackX.dpr and compile and install it
* The TscExcelExport component can be found in the tabsheet 'SC' of the component palette.
* When you like to add the component to an existing package, add the unit
  scExcelExport to a run-time package. Make sure the DCP file dclOffice is added as required.
  This file can be found in the Borland\Delphi\Lib folder.
  The unit scExcelExportReg.pas contains the registration and property and component editor.
  This unit should be included in a design-time package.
--------------------------------------------------------------------------------}

unit scExcelExportReg;

interface

uses Classes, {$IFDEF VER130} Dsgnintf; {$ELSE} DesignEditors, DesignIntf; {$ENDIF}

type
  TscExcelExportEditor = class(TComponentEditor)
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

  TscFileProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

procedure Register;

implementation

uses scExcelExport, TypInfo, Dialogs;

{$R scExcelExport.dcr}

procedure Register;
begin
  RegisterComponents('SC', [TscExcelExport]);
  RegisterComponentEditor(TscExcelExport,TscExcelExportEditor);
  RegisterPropertyEditor(TypeInfo(string),TscExcelExport,'Filename',TscFileProperty);
end;

//==============================================================================
// TscExcelExportEditor
//==============================================================================
procedure TscExcelExportEditor.Edit;
begin
  inherited;
  ExecuteVerb(1);
end;

//------------------------------------------------------------------------------
procedure TscExcelExportEditor.ExecuteVerb(Index: Integer);
var
  ExcelExport : TscExcelExport;
begin
  ExcelExport := (Component as TscExcelExport);

  case Index of
    0 :
      begin
        MessageDlg('TscExcelExport 3.6'+#13+#10+'Stefan Cruysberghs'+#13+#10+
          'Freeware, June 2007'+#13+#10+
          'http://www.scip.be', mtInformation, [mbOK], 0);
      end;
    1 :
      begin
        try
          ExcelExport.ExportDataset;
        finally
          ExcelExport.Disconnect;
        end
      end;
    2 :
      begin
        ExcelExport.LoadDefaultProperties;
      end;
  end;
end;

//------------------------------------------------------------------------------
function TscExcelExportEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'About';
    1 : Result := 'Export to Excel';
    2 : Result := 'Load default properties';
  end;
end;

//------------------------------------------------------------------------------
function TscExcelExportEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

//==============================================================================
// TscFileProperty
//==============================================================================
procedure TscFileProperty.Edit;
var
  OpenDialog : TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Excel files|*.XLS;*.XLSX';
    OpenDialog.DefaultExt := 'XLS';
    OpenDialog.Title := 'Select Excel file';
    if OpenDialog.Execute then
      SetStrValue(OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;

//------------------------------------------------------------------------------
function TscFileProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
