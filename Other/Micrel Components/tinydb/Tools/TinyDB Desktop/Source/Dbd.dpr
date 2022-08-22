program Dbd;

uses
  Forms,
  Misc in 'Misc.pas',
  OptionFrm in 'OptionFrm.pas' {OptionForm},
  AboutFrm in 'AboutFrm.pas' {AboutForm},
  NewDBFrm in 'NewDBFrm.pas' {NewDBForm},
  MainFrm in 'MainFrm.pas' {MainForm},
  DBInfoFrm in 'DBInfoFrm.pas' {DBInfoForm},
  InputFrm in 'InputFrm.pas' {InputForm},
  NewTableFrm in 'NewTableFrm.pas' {NewTableForm},
  AddFieldFrm in 'AddFieldFrm.pas' {AddFieldForm},
  AddIndexFrm in 'AddIndexFrm.pas' {AddIndexForm},
  TableListFrm in 'TableListFrm.pas' {TableListForm},
  DBCommentsFrm in 'DBCommentsFrm.pas' {DBCommentsForm},
  MRUMgr in 'MRUMgr.pas',
  DBPropertyFrm in 'DBPropertyFrm.pas' {DBPropertyForm},
  ChgPwdFrm in 'ChgPwdFrm.pas' {ChgPwdForm},
  FieldPropFrm in 'FieldPropFrm.pas' {FieldPropForm},
  IndexPropFrm in 'IndexPropFrm.pas' {IndexPropForm},
  DBTableFrm in 'DBTableFrm.pas' {DBTableForm},
  DBGridFrm in 'DBGridFrm.pas' {DBGridForm},
  DBCardFrm in 'DBCardFrm.pas' {DBCardForm},
  BlobDataFrm in 'BlobDataFrm.pas' {BlobDataForm},
  SearchFrm in 'SearchFrm.pas' {SearchForm},
  FilterFrm in 'FilterFrm.pas' {FilterForm},
  ChgEncFrm in 'ChgEncFrm.pas' {ChgEncForm},
  LangMgr in 'LangMgr.pas',
  BaseFrm in 'BaseFrm.pas' {BaseForm};

{$R *.RES}

begin
  Application.Initialize;
  {$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion > 18}
  Application.MainFormOnTaskbar := True;
  {$IFEND}
  {$ENDIF}
  Application.Title := 'TinyDB Desktop';
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.Run;
end.
