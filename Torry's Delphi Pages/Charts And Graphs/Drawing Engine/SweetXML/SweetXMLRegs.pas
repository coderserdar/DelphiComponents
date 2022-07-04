{*******************************************************}
{                                                       }
{         CA SweetXML Component Library             }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SweetXMLRegs;

{$I SweetXML.inc}

interface

uses
  {$IFDEF SCXML_DELPHI6_UP} Types, DesignEditors, DesignIntf {$ELSE}
  TypInfo, DsgnIntf {$ENDIF};

procedure Register;

implementation

{$R *.dcr}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  SCXML;

procedure Register;
begin
  RegisterComponents('Sweet XML', [TSCSaxParser, TSCDomParser]);
end;

end.
