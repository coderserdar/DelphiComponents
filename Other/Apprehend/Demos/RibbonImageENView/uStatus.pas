//------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012 Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : Status Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uStatus;

interface

uses
   Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   ComCtrls, StdCtrls, ExtCtrls;

type
   TFormStatus = class( TForm )
      lblStatus: TLabel;
   private
    { Private declarations }
   public
    { Public declarations }
   end;

var
   FormStatus: TFormStatus;

implementation

{$R *.DFM}

end.

