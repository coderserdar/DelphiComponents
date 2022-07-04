program DbExplr;

uses
  Forms,
  DBExcpt,
  VCLUtils,
  Main in 'MAIN.PAS' {DBExplorerMainForm},
  ChildWin in 'CHILDWIN.PAS' {MDIChild},
  OpenDlg in 'OPENDLG.PAS' {OpenDatabaseDlg},
  DestTab in 'DESTTAB.PAS' {DestTableDlg},
  About in 'ABOUT.PAS' {AboutDlg},
  EditPict in 'EDITPICT.PAS' {PictEditDlg},
  EditStr in 'EDITSTR.PAS' {StrEditDlg},
  FiltDlg in 'FILTDLG.PAS' {FilterDialog},
  OptDlg in 'OPTDLG.PAS' {OptionsDialog},
  SrcTab in 'SRCTAB.PAS' {SrcTableDlg},
  BdeInfo in 'BDEINFO.PAS',
  BdeProp in 'BDEPROP.PAS' {BdePropertyDlg},
  ViewBlob in 'VIEWBLOB.PAS' {BlobViewDlg},
  Options in 'OPTIONS.PAS';

{$IFDEF WIN32}
{$R *.R32}
{$ELSE}
{$R *.RES}
{$ENDIF}

begin
{$IFDEF WIN32}
  Application.Initialize;
{$ENDIF}
  { Uncomment next line to allow start only one instance of DBEXPLR.EXE   }
  { if ActivatePrevInstance(TDBExplorerMainForm.ClassName, '') then Exit; }
  Application.Title := 'Database Explorer';
  DBErrorIntercept;
  Application.CreateForm(TDBExplorerMainForm, DBExplorerMainForm);
  Application.Run;
end.