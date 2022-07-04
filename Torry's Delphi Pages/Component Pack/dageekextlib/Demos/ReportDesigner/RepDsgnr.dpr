{*******************************************************}
{                                                       }
{       Report Designer                                 }
{       Extension Library example of                    }
{       TELDesigner, TELDesignPanel                     }
{                                                       }
{       (c) 2001, Balabuyev Yevgeny                     }
{       E-mail: stalcer@rambler.ru                      }
{                                                       }
{*******************************************************}

program RepDsgnr;

uses
  madExcept,
  Forms,
  frmMainUnit in 'frmMainUnit.pas' {frmMain},
  dlgDataUnit in 'dlgDataUnit.pas' {dlgData},
  frmDocUnit in 'frmDocUnit.pas' {frmDoc},
  frmPropsUnit in 'frmPropsUnit.pas' {frmProps},
  dlgLinesEditorUnit in 'dlgLinesEditorUnit.pas' {dlgLinesEditor},
  dlgFieldsUnit in 'dlgFieldsUnit.pas' {dlgFields},
  dlgReportPropsUnit in 'dlgReportPropsUnit.pas' {dlgReportProps};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Report designer';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TdlgData, dlgData);
  Application.CreateForm(TfrmProps, frmProps);
  Application.CreateForm(TdlgLinesEditor, dlgLinesEditor);
  Application.CreateForm(TdlgFields, dlgFields);
  Application.CreateForm(TdlgReportProps, dlgReportProps);
  Application.Run;
end.
