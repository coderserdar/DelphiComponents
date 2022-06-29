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

{ $Id: DataInterfaces.pas,v 1.5 2001/08/02 15:03:29 mvj Exp $}

unit DataInterfaces;

interface

uses
{$ifndef LINUX}
  Graphics;
{$else}
  QGraphics;
{$endif LINUX}

type
  TTextAlignment = (rcaLeft, rcaRight, rcaCenter);

  {/** Interface for abstracting qr details from the rest of reporting. */}
  ILabel = interface
    ['{2BFA7F44-7AFC-45A8-B636-86569D668660}']
    function GetFontSize : integer;
    function GetTextAlignment : TTextAlignment;

    procedure SetFontSize(ASize : integer);
    procedure SetBGColor(AColor : TColor);
    procedure SetFGColor(AColor : TColor);
    procedure SetFontStyles(FontStyles : TFontStyles);
    procedure Clear;
    procedure SetTextAlignment(AnAlignment : TTextAlignment);
    property FontSize : integer read GetFontSize write SetFontSize;
    property TextAlignment : TTextAlignment read GetTextAlignment write SetTextAlignment;
  end;

implementation

end.

