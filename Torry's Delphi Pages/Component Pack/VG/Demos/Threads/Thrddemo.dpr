program ThrdDemo;

uses
  Forms,
  ThSort in 'ThSort.pas' {ThreadSortForm};

{$R *.RES}

begin
  Application.CreateForm(TThreadSortForm, ThreadSortForm);
  Application.Run;
end.
