Unit AutoCombo ;

{ Adds AutoCompletion to the standard ComboBox in csDropDown mode
  (similar to that already in csDropdownList format) }
{ Author:  Howard Harvey
  Version: 1.00
  Date:    12/JULY/2000
  email:   hharvey@dove.net.au }

Interface

Uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls ;

Const
  BackSpace = 8 ;

Type
  ThhAutoComboBox = class(TComboBox)
  private
    FAutoComplete : boolean ;
    FPreserveCase : boolean ;
    procedure SetAutoComplete( const Value: boolean ) ;
    function GetAutoComplete : boolean ;
    procedure SetPreserveCase( const Value: boolean ) ;
    function GetPreserveCase : boolean ;
  protected
    procedure ComboWndProc( var Message: TMessage ;
                            ComboWnd: HWnd ;
                            ComboProc: pointer ) ; override ;
  public
    { Public declarations }
  published
    property AutoComplete: boolean read  GetAutoComplete
                                   write SetAutoComplete ;
    property PreserveCase : boolean read GetPreserveCase
                                   write SetPreserveCase ;
  end ;

Procedure Register ;

Implementation

{ -------------------------------------------------------------------- }

Procedure ThhAutoComboBox.SetAutoComplete( const Value: boolean ) ;
begin
  fAutoComplete := Value ;
end ;

{ -------------------------------------------------------------------- }

Function ThhAutoComboBox.GetAutoComplete: boolean ;
begin
  GetAutoComplete := fAutoComplete ;
end ;

{ -------------------------------------------------------------------- }

procedure ThhAutoComboBox.SetPreserveCase( const Value: boolean ) ;
begin
  fPreserveCase :=  Value ;
end ;

{ -------------------------------------------------------------------- }

function ThhAutoComboBox.GetPreserveCase : boolean ;
begin
  GetPreserveCase := fPreserveCase ;
end ;

{ -------------------------------------------------------------------- }

Procedure ThhAutoComboBox.ComboWndProc( var Message: TMessage ;
                                  ComboWnd: HWnd ; ComboProc: pointer ) ;

{ Override the WndProc }

var
  RowNum  : integer ;
  StrLen  : integer ;
  ResWord : longint ;
  NewStr  : string ;
  NewLen  : integer ;

begin
  inherited ;

{ Process the Windows Message }
{ Proceed only if it was "WM_KEYUP" }

  if (Message.Msg = WM_KEYUP)
  then begin

{ Key has been released - continue only if AutoComplete is enabled }

    if (FAutoComplete = True)
    then begin

{ Autocompletion enabled - Validate the key response }

      ResWord := Message.WParam ;
      if  (ResWord <> VK_DELETE)
      AND (ResWord <> VK_SHIFT)
      AND (ResWord <> BackSpace)
      then begin

{ This must be a valid key - use CB_FINDSTRING to find matching string }

        RowNum := Perform( CB_FINDSTRING, -1, integer(PChar(Text)) ) ;

{ Update the ComboBox if a match was found }

        if (RowNum <> CB_ERR)
        then begin

{ Save the length of the current edit box portion current string }

          StrLen := Length(Text) ;
          if PreserveCase
          then begin

{ Create the new string }

            NewLen := length(Items[RowNum])- StrLen ;
            NewStr := COPY( Text , 1 , StrLen )
                    + COPY( Items[RowNum] , StrLen+1 , NewLen ) ;

{ Set the new string's ItemIndex and replace the text }

            ItemIndex := RowNum ;
            Text := NewStr ;
          end
          else begin

{ Only set the new string's ItemIndex }

            ItemIndex := RowNum ;
          end ;

{ Select remainder of new string }

          SelStart := StrLen ;

{ Select remaining length }

          SelLength := Length(Text) - StrLen ;

{ Update message result }

          Message.Result := 0 ;
        end ;
      end ;
    end ;
  end ;
end ;

{ -------------------------------------------------------------------- }

procedure Register ;
begin
  RegisterComponents('Howie', [ThhAutoComboBox]) ;
end ;

end.



