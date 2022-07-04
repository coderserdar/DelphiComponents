{*******************************************************}
{File:      NCO8Console.dpr                             }
{Revision:  0.02.01 / 27.10.1999                        }
{Comment:   NC OCI8 Demo: NC O8 Console, project file   }
{Copyright: (c) 1999, Nica-Com Ltd.                     }
{Author:    Dmitry Arefiev, diman@ncom.ru               }
{*******************************************************}

program NCO8Console;

uses
  Forms,
  O8ConsoleFrm in 'O8ConsoleFrm.pas' {NCO8ConChildFrm},
  O8AboutFrm in 'O8AboutFrm.pas' {NCO8AboutFrm},
  O8MainFrm in 'O8MainFrm.pas' {NCO8ConMainFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'NC O8 Console';
  Application.CreateForm(TNCO8ConMainFrm, NCO8ConMainFrm);
  Application.Run;
end.
