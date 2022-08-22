unit TB97Reg;

{
  Toolbar97
  Copyright (C) 1998-2004 by Jordan Russell
  http://www.jrsoftware.org/

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.


  Design-time component registration

  $jrsoftware: tb97/Source/TB97Reg.pas,v 1.5 2004/02/23 22:53:00 jr Exp $
}

interface

{$I TB97Ver.inc}

procedure Register;

implementation

uses
  SysUtils, Classes, Dialogs, 
  {$IFDEF TB97D6} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  TB97Vers, TB97, TB97Tlbr, TB97Tlwn, TB97Ctls;

type
  TToolbar97VersionProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TToolbar97VersionProperty.Edit;
const
  AboutText =
    '%s'#13#10 +
    'Copyright (C) 1998-2004 by Jordan Russell'#13#10 +
    'For conditions of distribution and use, see LICENSE.TXT.'#13#10 +
    #13#10 +
    'Visit my web site for the latest versions of Toolbar97:'#13#10 +
    'http://www.jrsoftware.org/';
begin
  MessageDlg (Format(AboutText, [GetStrValue]), mtInformation, [mbOK], 0);
end;

function TToolbar97VersionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;


procedure Register;
begin
  RegisterComponents ('Toolbar97', [TDock97, TToolbar97, TToolWindow97,
    TToolbarButton97, TToolbarSep97, TEdit97]);
  RegisterPropertyEditor (TypeInfo(TToolbar97Version), nil, '',
    TToolbar97VersionProperty);
end;

end.
