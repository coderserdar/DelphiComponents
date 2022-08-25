//------------------------------------------------------------------------------
//  Apprehend Version  : 4.3
//  Copyright (c) 2010 : Adirondack Software & Graphics
//  Created            : 1-09-1992
//  Last Modification  : 01-06-2010
//  Description        : frmSelectionDimensions
//------------------------------------------------------------------------------

unit uSelectionDimensions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, ExtCtrls;

type
  TSelectionDimensionForm = class( TForm )
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edWidth: TEdit;
    UpDown1: TUpDown;
    OKBtn: TButton;
    CancelBtn: TButton;
    UpDown2: TUpDown;
    EdHeight: TEdit;
    Label2: TLabel;
    PopupMenu1: TPopupMenu;
    Increment1: TMenuItem;
    N11: TMenuItem;
    N51: TMenuItem;
    N101: TMenuItem;
    N201: TMenuItem;
    N401: TMenuItem;
    N501: TMenuItem;
    N1001: TMenuItem;
    Panel1: TPanel;
    procedure N11Click( Sender: TObject );
    procedure N51Click( Sender: TObject );
    procedure N101Click( Sender: TObject );
    procedure N201Click( Sender: TObject );
    procedure N401Click( Sender: TObject );
    procedure N501Click( Sender: TObject );
    procedure N1001Click( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SelectionDimensionForm: TSelectionDimensionForm;

implementation

{$R *.dfm}

procedure TSelectionDimensionForm.N11Click( Sender: TObject );
begin
  UpDown1.Increment := 1;
  UpDown2.Increment := 1;
end;

procedure TSelectionDimensionForm.N51Click( Sender: TObject );
begin
  UpDown1.Increment := 5;
  UpDown2.Increment := 5;
end;

procedure TSelectionDimensionForm.N101Click( Sender: TObject );
begin
  UpDown1.Increment := 10;
  UpDown2.Increment := 10;
end;

procedure TSelectionDimensionForm.N201Click( Sender: TObject );
begin
  UpDown1.Increment := 20;
  UpDown2.Increment := 20;
end;

procedure TSelectionDimensionForm.N401Click( Sender: TObject );
begin
  UpDown1.Increment := 40;
  UpDown2.Increment := 40;
end;

procedure TSelectionDimensionForm.N501Click( Sender: TObject );
begin
  UpDown1.Increment := 50;
  UpDown2.Increment := 50;
end;

procedure TSelectionDimensionForm.N1001Click( Sender: TObject );
begin
  UpDown1.Increment := 100;
  UpDown2.Increment := 100;
end;

end.

