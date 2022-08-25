//------------------------------------------------------------------------------
//  Apprehend Version       : 6.0
//  Copyright © 1986-2012 : Adirondack Software & Graphics
//  Last Modification       : 04-01-2012
//  Compiler                : Delphi 2010
//  Description             : Import Unit
// This file is copyright © W W Miller, 1986-2012.
// It may be used without restriction. This code distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
//------------------------------------------------------------------------------

unit uImport;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls, Spin, ImageENIO;

type
  TFormImport = class ( TForm )
    RadioGroup1: TRadioGroup;
    GroupBox1: TGroupBox;
    RadioButton16: TRadioButton;
    RadioButton32: TRadioButton;
    RadioButton48: TRadioButton;
    RadioButton64: TRadioButton;
    RadioButton72: TRadioButton;
    RadioButton128: TRadioButton;
    RadioButton7: TRadioButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    RzLabel1: TLabel;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    ComboBox1: TComboBox;
    RadioButton256: TRadioButton;
    RadioButton96: TRadioButton;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure RadioButton7Click ( Sender: TObject );
    procedure Button1Click(Sender: TObject);
    procedure RzSpinEdit1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure RzRadioButton7Click(Sender: TObject);
    procedure RadioButton196Click(Sender: TObject);
    procedure RadioButton72Click(Sender: TObject);
    procedure RadioButton64Click(Sender: TObject);
    procedure RadioButton48Click(Sender: TObject);
    procedure RadioButton32Click(Sender: TObject);
    procedure RadioButton16Click(Sender: TObject);
    procedure RadioButton128Click(Sender: TObject);
    procedure RadioButton256Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    fIconWidth: integer;
    fIconHeight: integer;
    fBitsPerSample: integer;
    fSamplesPerPixel: integer;
    fBitCount: integer;
    fICO_BitCount: TIOICOBitCount;
  end;

var
  FormImport: TFormImport;

implementation

uses umain;

{$R *.dfm}

procedure TFormImport.RadioButton7Click ( Sender: TObject );
begin
  SpinEdit1.Enabled := true;
  SpinEdit2.Enabled := true;
end;

procedure TFormImport.Button1Click(Sender: TObject);
begin
  if RadioButton16.Checked then
  begin
    fIconWidth := 16;
    fIconHeight := 16;
  end
  else
    if RadioButton32.Checked then
    begin
      fIconWidth := 32;
      fIconHeight := 32;
    end
    else
      if RadioButton48.Checked then
      begin
        fIconWidth := 48;
        fIconHeight := 48;
      end
      else
        if RadioButton64.Checked then
        begin
          fIconWidth := 64;
          fIconHeight := 64;
        end
        else
          if RadioButton72.Checked then
          begin
            fIconWidth := 72;
            fIconHeight := 72;
          end
          else
          if RadioButton96.Checked then
          begin
            fIconWidth := 96;
            fIconHeight := 96;
          end
          else
            if RadioButton128.Checked then
            begin
              fIconWidth := 128;
              fIconHeight := 128;
            end
            else
            if RadioButton256.Checked then
          begin
            fIconWidth := 256;
            fIconHeight := 256;
          end
          else
              if RadioButton7.Checked then
              begin
                fIconWidth := SpinEdit1.Value;
                fIconHeight := SpinEdit2.Value;
              end;

  case RadioGroup1.ItemIndex of
    0:
      begin // 32 bit
        fBitCount := 32;
        fBitsPerSample := 8;
        fSamplesPerPixel := 4;
        fICO_BitCount[0] := 32;
      end;
    1:
      begin // 24 bit True
        fBitCount := 24;
        fBitsPerSample := 8;
        fSamplesPerPixel := 3;
        fICO_BitCount[0] := 24;
      end;
    2:
      begin // 256 color
        fBitCount := 8;
        fBitsPerSample := 8;
        fSamplesPerPixel := 1;
        fICO_BitCount[0] := 8;
      end;
    3:
      begin // 16 color
        fBitCount := 4;
        fBitsPerSample := 4;
        fSamplesPerPixel := 1;
        fICO_BitCount[0] := 4;
      end;
    4:
      begin // Monochrome
        fBitCount := 2;
        fBitsPerSample := 1;
        fSamplesPerPixel := 1;
        fICO_BitCount[0] := 2
      end;
  end; // case
end;

procedure TFormImport.RzSpinEdit1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  SpinEdit2.Value := SpinEdit1.Value;
end;

procedure TFormImport.RzRadioButton7Click(Sender: TObject);
begin
  SpinEdit1.Enabled := true;
  SpinEdit2.Enabled := true;
end;

procedure TFormImport.RadioButton196Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

procedure TFormImport.RadioButton256Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

procedure TFormImport.RadioButton72Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

procedure TFormImport.RadioButton64Click(Sender: TObject);
begin
  SpinEdit1.Enabled := true;
  SpinEdit2.Enabled := true;
end;

procedure TFormImport.RadioButton48Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

procedure TFormImport.RadioButton32Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

procedure TFormImport.RadioButton128Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

procedure TFormImport.RadioButton16Click(Sender: TObject);
begin
  SpinEdit1.Enabled := false;
  SpinEdit2.Enabled := false;
end;

end.

