//------------------------------------------------------------------------------
// Apprehend Version       : 6.0
// Copyright © 1986-2012   : Adirondack Software & Graphics
// Last Modification       : 04-01-2012
// Compiler                : Delphi 2010
// Description             : Selection Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uSelection;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, ieview, imageenview;

type
  TFormSelection = class( TForm )
    OKBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    UpDownSize: TUpDown;
    Label2: TLabel;
    ColorBoxColor1: TColorBox;
    ColorBoxColor2: TColorBox;
    Label3: TLabel;
    Label4: TLabel;
    CheckBoxExtendedSelectionDrawing: TCheckBox;
    Label9: TLabel;
    cbxBrushName1: TComboBox;
    GroupBox3: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    GripShape1: TRadioGroup;
    Panel1: TPanel;
    procedure CheckBox1Click( Sender: TObject );
    procedure CheckBox2Click( Sender: TObject );
    procedure CheckBox3Click( Sender: TObject );
    procedure CheckBox4Click( Sender: TObject );
    procedure CheckBox5Click( Sender: TObject );
    procedure CheckBox6Click( Sender: TObject );
    procedure CheckBox7Click( Sender: TObject );
    procedure Edit1Change( Sender: TObject );
    procedure cbxBrushName1Change( Sender: TObject );
    procedure ColorBoxColor1Change( Sender: TObject );
    procedure ColorBoxColor2Change( Sender: TObject );
    procedure FormActivate( Sender: TObject );
    procedure FormDeactivate( Sender: TObject );
    procedure GripShape1Click( Sender: TObject );
    procedure CheckBoxExtendedSelectionDrawingClick( Sender: TObject );
  private
    { Private declarations }
    ImageENView: TImageENView;
  public
    { Public declarations }
  end;

var
  FormSelection: TFormSelection;

implementation

uses uMain;

{$R *.dfm}

procedure TFormSelection.FormActivate( Sender: TObject );
const
  BS1: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
  BS2: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
var
  GripColor1: TColor;
  GripColor2: TColor;
  GripBrushStyle: TBrushStyle;
  GripSize: integer;
  ExtendedGrips: boolean;
  GripShape: TIEGripShape;
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      ColorBoxColor1.Selected := ImageENView.SelColor1;
      ColorBoxColor2.Selected := ImageENView.SelColor2;
      // get grips
      ImageENView.GetSelectionGripStyle( GripColor1, GripColor2, GripBrushStyle, GripSize,
        ExtendedGrips, GripShape );
      cbxBrushName1.ItemIndex := Ord( GripBrushStyle );
      FormSelection.Edit1.Text := IntToStr( GripSize );
      CheckBoxExtendedSelectionDrawing.Checked := ExtendedGrips;
      GripShape1.ItemIndex := ord( GripShape );
      CheckBox1.Checked := iesoAnimated in ImageENView.SelectionOptions;
      CheckBox2.Checked := iesoSizeable in ImageENView.SelectionOptions;
      CheckBox3.Checked := iesoMoveable in ImageENView.SelectionOptions;
      CheckBox4.Checked := iesoFilled in ImageENView.SelectionOptions;
      CheckBox5.Checked := iesoCutBorders in ImageENView.SelectionOptions;
      CheckBox6.Checked := iesoMarkOuter in ImageENView.SelectionOptions;
      CheckBox7.Checked := iesoCanScroll in ImageENView.SelectionOptions;
      end;
    end;
end;

procedure TFormSelection.FormDeactivate( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      // set grips
      ImageENView.SelColor1 := ColorBoxColor1.Selected;
      ImageENView.SelColor2 := ColorBoxColor2.Selected;
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );

      if CheckBox1.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoAnimated ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoAnimated ];

      if CheckBox2.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoSizeable ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoSizeable ];

      if CheckBox3.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoMoveable ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoMoveable ];

      if CheckBox4.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoFilled ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoFilled ];

      if CheckBox5.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoCutBorders ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoCutBorders ];

      if CheckBox6.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoMarkOuter ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoMarkOuter ];

      if CheckBox7.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoCanScroll ]
      else
        ImageENView.SelectionOptions := TImageENView(
          PageControl1.ActivePage.Tag ).SelectionOptions - [ iesoCanScroll ];
    end;
  end;

end;

procedure TFormSelection.GripShape1Click( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );
    end;
  end;
end;

procedure TFormSelection.cbxBrushName1Change( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );
    end;
  end;
end;

procedure TFormSelection.CheckBox1Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox1.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoAnimated ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoAnimated ];
    end;
  end;
end;

procedure TFormSelection.CheckBox2Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox2.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoSizeable ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoSizeable ];
    end;
  end;
end;

procedure TFormSelection.CheckBox3Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox3.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoMoveable ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoMoveable ];
    end;
  end;
end;

procedure TFormSelection.CheckBox4Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox4.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoFilled ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoFilled ];
    end;
  end;
end;

procedure TFormSelection.CheckBox5Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox5.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoCutBorders ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoCutBorders ];
    end;
  end;
end;

procedure TFormSelection.CheckBox6Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox6.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoMarkOuter ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoMarkOuter ];
    end;
  end;
end;

procedure TFormSelection.CheckBox7Click( Sender: TObject );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
      if CheckBox7.Checked then
        ImageENView.SelectionOptions := ImageENView.SelectionOptions + [ iesoCanScroll ]
      else
        ImageENView.SelectionOptions := ImageENView.SelectionOptions - [ iesoCanScroll ];
    end;
  end;
end;

procedure TFormSelection.CheckBoxExtendedSelectionDrawingClick( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );
    end;
  end;
end;

procedure TFormSelection.ColorBoxColor1Change( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );
    end;
  end;
end;

procedure TFormSelection.ColorBoxColor2Change( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );
    end;
  end;
end;

procedure TFormSelection.Edit1Change( Sender: TObject );
const
  BS: array[ 0..7 ] of TBrushStyle = ( bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross );
begin
  with FormMain do
  begin
    if Assigned( PageControl1.ActivePage ) then
    begin
      ImageENView := TImageENView( PageControl1.ActivePage.Controls[ 0 ] );
        ImageENView.SetSelectionGripStyle( ColorBoxColor1.Selected, ColorBoxColor2.Selected, BS[
          cbxBrushName1.ItemIndex ], StrToInt( FormSelection.Edit1.Text ),
            CheckBoxExtendedSelectionDrawing.Checked, TIEGripShape( GripShape1.ItemIndex ) );
    end;
  end;
end;

end.

