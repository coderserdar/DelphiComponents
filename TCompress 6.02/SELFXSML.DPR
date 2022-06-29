program selfXsml;

{ TCompress V3.0 "Tiny" self-extracting EXE example -- only comments changed from V2.5
  This program uses the same approach (and doInstal unit) as
  does the SELFEXTR.DPR unit. Hence, refer to the instructions
  at the top of SELFEXTF.PAS for setup instructions.

  Note:
  Because no form is involved, this program will be
  far *smaller* than SELFEXTR.EXE. The downside is that
  it has no user interaction, although you could use Windows
  MessageBox calls to at least display a confirmation message.
}

{$R *.RES}
{$R COMP_RES.RES}

uses doInstal;


const PROG_NAME =   'notepad.exe';
      README_NAME = 'readme.txt';

begin
   DoInstall('',PROG_NAME, README_NAME); { into the CURRENT directory! }
end.
