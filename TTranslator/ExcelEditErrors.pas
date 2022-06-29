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

{ $Id: ExcelEditErrors.pas,v 1.3 2002/10/01 09:08:01 mjs Exp $ }

unit ExcelEditErrors;

interface

uses
  Classes, Controls, Forms, Dialogs, StdCtrls, Buttons;

type
  TfrmExcelErrors = class(TForm)
    btnOK: TBitBtn;
    MemoErrors: TMemo;
    lblCaption: TLabel;
  public
    procedure ShowMe(ErrorList:TStrings);
  end;

implementation

{$R *.DFM}

procedure TfrmExcelErrors.ShowMe(ErrorList:TStrings);
begin
  MemoErrors.Lines.Assign( ErrorList );

  ShowModal;
end;

end.

