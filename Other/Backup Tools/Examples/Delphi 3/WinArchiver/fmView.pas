unit fmView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, unTranslation;

type
  TView = class(TForm)
    rgUsing: TRadioGroup;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Déclarations privées }
    FOldCaption : String;
    FOldItem : String;

    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
    FileName : String;
    Exe : String;
  end;

var
  View: TView;

implementation

{$R *.DFM}

procedure TView.FormShow(Sender: TObject);
begin
  FOldCaption := Caption;
  FOldItem := rgUsing.Items.Strings[0];
  Caption := Format( FOldCaption, [FileName] );
  rgUsing.Items.Strings[0] := Format( FOldItem, [Exe] );
end;

procedure TView.FormHide(Sender: TObject);
begin
  Caption := FOldCaption;
  rgUsing.Items.Strings[0] := FOldItem;
end;

procedure TView.WMTranslate( var Message : TMessage );
begin
  rgUsing.Items.Text := GetStr(2003);
end;

end.
