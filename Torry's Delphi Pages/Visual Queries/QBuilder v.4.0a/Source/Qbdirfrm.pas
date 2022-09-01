{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       QBuilder dialog component                       }
{                                                       }
{       Copyright (c) 1996-99 Sergey Orlik              }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       Internet:  sorlik@inprise.ru                    }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

unit QBDirFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl;

type
  TOQBDirForm = class(TForm)
    BtnOk: TButton;
    BtnCancel: TButton;
    Bevel: TBevel;
    ComboDrive: TDriveComboBox;
    DirLbx: TDirectoryListBox;
    FileLbx: TFileListBox;
    procedure ComboDriveChange(Sender: TObject);
    procedure DirLbxChange(Sender: TObject);
  private
    procedure SetDir(aDir:string);
    function GetDir:string;
  public
    property Directory:string read GetDir write SetDir;
  end;

implementation

{$R *.DFM}

procedure TOQBDirForm.SetDir(aDir:string);
begin
  DirLbx.Directory:=aDir;
end;

function TOQBDirForm.GetDir:string;
begin
  Result:=DirLbx.Directory;
end;

procedure TOQBDirForm.ComboDriveChange(Sender: TObject);
begin
  DirLbx.Drive:=ComboDrive.Drive;
  FileLbx.Drive:=ComboDrive.Drive;
  FileLbx.Directory:=DirLbx.Directory;
end;

procedure TOQBDirForm.DirLbxChange(Sender: TObject);
begin
  ComboDrive.Drive:=DirLbx.Drive;
  FileLbx.Drive:=ComboDrive.Drive;
  FileLbx.Directory:=DirLbx.Directory;
end;

end.
