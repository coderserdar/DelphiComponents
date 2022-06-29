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

{ $Id: EditorInterfaces.pas,v 1.8 2003/01/02 14:12:54 laa Exp $ }

unit EditorInterfaces;

interface
{$i common.inc}

uses
  DataElements;
  
type

  ISubTotalHandlerInterface = interface
    function CanEditValue(ARow : TAbstractRow; ADataField : TDataField; var ReadOnlyReason : String) : Boolean;
  end;
  
  IRowStorageHandlerInterface = interface
    procedure ShowSubTotalLevel(AField : TDataField);
    procedure HideSubTotalLevel(AField : TDataField);
    procedure ShowSubTotalLevelOnly(AField : TDataField);
  end;

  ISlave = interface
    ['{B873BB8A-A983-4440-B705-9D1874C16E89}']
    procedure MasterDestroying;
    procedure RowChange(ARow : TAbstractRow);
    procedure StorageChange(AStorage : TAbstractRowStorage);
    procedure ValueChanged(AField : TDataField);
  end;

  IMaster = interface
    ['{1871F831-07DA-4321-97E5-AE60EDFE7DFC}']
    procedure RegisterSlave(ASlave : ISlave);
    procedure UnregisterSlave(ASlave : ISlave);
    procedure ValueChanged(AField : TDataField);
  end;

implementation

end.

