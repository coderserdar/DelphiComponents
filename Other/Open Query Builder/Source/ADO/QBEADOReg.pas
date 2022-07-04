{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Open QBuilder Engine                            }
{                                                       }
{       Copyright (c) 2003 Fast Reports, Inc.           }
{                                                       }
{*******************************************************}

{$I QBDEF.INC}

unit QBEADOReg;

interface

uses
  Classes, QBEADO;

procedure Register;

implementation

uses
{$IFNDEF Delphi6}
  DsgnIntf
{$ELSE}
  DesignIntf, DesignEditors
{$ENDIF};

procedure Register;
begin
  RegisterComponents('OQBuilder', [TOQBEngineADO]);
end;

end.
