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

unit DBUInterfaces;

interface

uses
{$ifndef LINUX}
  Windows, Graphics, Grids,
{$else LINUX}
  QGrids, QGraphics, Types,
{$endif LINUX}
  DataType, DBUTypes, DBUFormatter;


type

  IInfoGrid = interface
    procedure InvalidateCell( ACol, ARow : Integer );
    function CellRect(ACol, ARow: Longint): TRect;
  end;

  IAntGrid = interface
    function BoxRect(ALeft, ATop, ARight, ABottom: Longint): TRect;
  end;

  IInfoEditor = interface
    procedure SetMouseCapture( AValue : Boolean );
    procedure Invalidate;
    function SelfEdit : TInplaceEdit;
    function GetWidth : Integer;
    function GetHeight : Integer;
    function EditorRect :TRect;
    function ClientRect :TRect;
    procedure Flush;
  end;

  IInfoCell = interface
    function GetCol: Integer;
    function GetRow: Integer;

    function GetMouseStateDown: TMouseState;
    function GetMouseStateMove: TMouseMoveState;
    function GetMouseStateUp: TMouseState;
    function GetMouseBtnStates: TMouseBtnStates;
    function GetFormatter( DrawState: TGridDrawState ) : TDBUFormatter;
    function GetValue : TValue;
    function GetDrawValue : TValue;
    function GetEnabled : Boolean;
    function CanEditModify : Boolean;
    function GetCustomizer : TDBUCustomizer;
    function SelfObject : TObject;
    function GetGrid: IInfoGrid;

    property Col: Integer read GetCol;
    property Row: Integer read GetRow;
  end;

  IGridCell = interface(IInfoCell)
  end;

  IEditorCell = interface(IInfoCell)
    function GetEditor: IInfoEditor;
    procedure SetValue(const AValue : TValue);
    function GetEditorValue : TValue;
    procedure SetEditorValue( const AValue : TValue );
    function GetEditorKeyValue : TValue;
    function GetEditorMode : Boolean;
    procedure SetEditorMode( AValue : Boolean );
    property EditorMode : Boolean read GetEditorMode write SetEditorMode;
    function GetInplaceEditText : String;
    procedure SetInplaceEditText( const Value : String );
    function SetCell : Boolean;
  end;

  IGridIterator = interface
    procedure ChangeCell( ACol, ARow : Integer );
    function GetFormatter( DrawState: TGridDrawState ) : TDBUFormatter;
    function GetGridValue : TValue;
    function SelfObject : TObject;
  end;

  IEditorIterator = interface
    procedure ChangeCell( ACol, ARow : Integer );
    function GetFormatter( DrawState: TGridDrawState ) : TDBUFormatter;
    function GetGridValue : TValue;
    procedure SetGridValue( const Value : TValue );
  end;

  IIconSupplier = interface
    procedure GetIcon(ACell : IInfoCell; var APicture : TPicture;
      var Align : TDBUAlign; var Margin : TDBUMargin );
    function HasIcon( ACell : IInfoCell ) : Boolean;
  end;

  ICheckInfoSupplier = interface
    procedure GetDescription( ACell : IInfoCell; var Descr : TValue );
  end;

implementation

end.

