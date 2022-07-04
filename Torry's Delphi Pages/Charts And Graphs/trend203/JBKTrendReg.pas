{-----------------------------------------------------------------------------
 Unit Name: JBKTrendReg  for Trend v 2.03
 Author:    jkehrel
 Purpose:   Part of Trend package.
 History:   v. 1.0
            v. 1.1 conditionals

 Copyright (c) 1997 by Mark Dodson (version 1.0)
 Copyright (c) 2002 by Dr. Juergen Kehrel (version 2.0)
-----------------------------------------------------------------------------}
{$I jedi.inc}

unit JBKTrendReg;

interface

uses
  Windows, Classes, Graphics, Dialogs,
{$IFNDEF COMPILER6_UP}
  DsgnIntf,
{$ELSE}
  DesignIntf,
  DesignEditors,
  VCLEditors,
{$ENDIF}
  JBKTrend;

type
  TJBKTrendAboutProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

procedure TJBKTrendAboutProperty.Edit;
begin
  with CreateMessageDialog('Trend version 2.03' + #13 +
    'Copyright ' + #169 + ' 1997 by Mark Dodson (version 1.0)' + #13 +
    'Copyright ' + #169 + ' 2002-5 by Dr. Juergen Kehrel (version 2.x)'+ #13 +
    'some improvements by Bart Michel March 2005',
    mtInformation, [mbOk]) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

function TJBKTrendAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly]
end;

function TJBKTrendAboutProperty.GetValue: string;
begin
  Result := 'Trend v2.03';
end;

procedure Register;
begin
  RegisterComponents('Beispiele', [TJBKTrend]);
  RegisterPropertyEditor(TypeInfo(TJBKTrendAboutBox), nil, '', TJBKTrendAboutProperty);
end;

end.

