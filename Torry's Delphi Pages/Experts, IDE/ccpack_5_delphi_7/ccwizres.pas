{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Custom Containers Pack (CCPack)                 }
{                                                       }
{       Copyright (c) 1997-99, Sergey Orlik             }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       e-mail:  sorlik@inprise.ru                      }
{       WWW: http://www.inprise.ru                      }
{                                                       }
{       Personal Home Page:                             }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}
{$I CCPDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}
unit ccwizres;

interface

resourcestring
  sCCSourceContainerReg =
    'unit %UnitIdent%;'+#13#10+
    #13#10+
    'interface'+#13#10+
    #13#10+
    'uses'+#13#10+
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls,'+#13#10+
    '  Forms, Dialogs%AddUnit%;'+#13#10+
    #13#10+
    'type'+#13#10+
    '  %ClassName% = class(T%AncestorName%)'+#13#10+
    '  private'+#13#10+
    '    { Private declarations }'+#13#10+
    '  protected'+#13#10+
    '    { Protected declarations }'+#13#10+
    '  public'+#13#10+
    '    { Public declarations }'+#13#10+
    '  published'+#13#10+
    '    { Published declarations }'+#13#10+
    '  end;'+#13#10+
    #13#10+
    'procedure Register;'+#13#10+
    #13#10+
    'implementation'+#13#10+
    #13#10+
    'uses'+#13#10+
    '  CCReg;'+#13#10+
    #13#10+
    'procedure Register;'+#13#10+
    'begin'+#13#10+
    '  RegisterCustomContainer(%ClassName%);'+#13#10+
    'end;'+#13#10+
    #13#10+
    'end.'+#13#10;

  sCCSourceForm =
    'unit %UnitIdent%;'+#13#10+
    #13#10+
    'interface'+#13#10+
    #13#10+
    'uses'+#13#10+
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls,'+#13#10+
    '  Forms, Dialogs%AddUnit%;'+#13#10+
    #13#10+
    'type'+#13#10+
    '  %ClassName% = class(T%AncestorName%)'+#13#10+
    '  private'+#13#10+
    '    { Private declarations }'+#13#10+
    '  protected'+#13#10+
    '    { Protected declarations }'+#13#10+
    '  public'+#13#10+
    '    { Public declarations }'+#13#10+
    '  published'+#13#10+
    '    { Published declarations }'+#13#10+
    '  end;'+#13#10+
    #13#10+
    'var'+#13#10+
    '  %FormName%: %ClassName%;'+#13#10+
    #13#10+
    'implementation'+#13#10+
    #13#10+
    '{$R *.DFM}'+#13#10+
    #13#10+
    'end.'+#13#10;

  sCCSourceContainer =
    'unit %UnitIdent%;'+#13#10+
    #13#10+
    'interface'+#13#10+
    #13#10+
    'uses'+#13#10+
    '  Windows, Messages, SysUtils, Classes, Graphics, Controls,'+#13#10+
    '  Forms, Dialogs%AddUnit%;'+#13#10+
    #13#10+
    'type'+#13#10+
    '  %ClassName% = class(T%AncestorName%)'+#13#10+
    '  private'+#13#10+
    '    { Private declarations }'+#13#10+
    '  protected'+#13#10+
    '    { Protected declarations }'+#13#10+
    '  public'+#13#10+
    '    { Public declarations }'+#13#10+
    '  published'+#13#10+
    '    { Published declarations }'+#13#10+
    '  end;'+#13#10+
    #13#10+
    'procedure Register;'+#13#10+
    #13#10+
    'implementation'+#13#10+
    #13#10+
    '{$R *.DFM}'+#13#10+
    #13#10+
    'procedure Register;'+#13#10+
    'begin'+#13#10+
    '  RegisterComponents(''Composites'', [%ClassName%]);'+#13#10+
    'end;'+#13#10+
    #13#10+
    'end.'+#13#10;

implementation

end.
