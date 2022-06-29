{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: TranslatorReg.pas,v 1.4 2002/10/22 07:17:27 lge Exp $ }

unit TranslatorReg;

interface
{$i common.inc}

  procedure Register;

implementation

uses
  Classes, Translator, TranslatorEdt;

{========================}
{== Register Procedure ==}
{========================}


procedure Register;
begin
  if PerformVersionNotificationAtInstall then
  begin
    {== Register Components ==}
    RegisterComponents( 'Polycon',
                        [ TTranslator, TTranslatorClient ] );


    {== Register Property and Component Editors ==}
    TranslatorEdt.Register;
  end;
end;

end.

