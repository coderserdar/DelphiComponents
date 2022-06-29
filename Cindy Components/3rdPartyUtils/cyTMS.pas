{   Unit cyImageEn3

    Description:
    Unit with functions to use with TMS components.

    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    $  €€€ Accept any PAYPAL DONATION $$$  €
    $      to: mauricio_box@yahoo.com      €
    €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€

    * ***** BEGIN LICENSE BLOCK *****
    *
    * Version: MPL 1.1
    *
    * The contents of this file are subject to the Mozilla Public License Version
    * 1.1 (the "License"); you may not use this file except in compliance with the
    * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
    *
    * Software distributed under the License is distributed on an "AS IS" basis,
    * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
    * the specific language governing rights and limitations under the License.
    *
    * The Initial Developer of the Original Code is Mauricio
    * (https://sourceforge.net/projects/tcycomponents/).
    *
    * Donations: see Donation section on Description.txt
    *
    * Alternatively, the contents of this file may be used under the terms of
    * either the GNU General Public License Version 2 or later (the "GPL"), or the
    * GNU Lesser General Public License Version 2.1 or later (the "LGPL"), in which
    * case the provisions of the GPL or the LGPL are applicable instead of those
    * above. If you wish to allow use of your version of this file only under the
    * terms of either the GPL or the LGPL, and not to allow others to use your
    * version of this file under the terms of the MPL, indicate your decision by
    * deleting the provisions above and replace them with the notice and other
    * provisions required by the LGPL or the GPL. If you do not delete the
    * provisions above, a recipient may use your version of this file under the
    * terms of any one of the MPL, the GPL or the LGPL.
    *
    * ***** END LICENSE BLOCK *****}

unit cyTMS;

interface

uses SysUtils, Db, Inifiles, AdvGrid, cyStrUtils;

    function Advstring_Locate(Grid: TAdvStringGrid; Column: Integer; StartRow, EndRow: Integer; Value: String; Options: TLocateOptions): Integer; overload;
    function Advstring_Locate(Grid: TAdvStringGrid; Column: Integer; StartRow, EndRow: Integer; Value: Double): Integer; overload;
    procedure Advstring_Columns_Def(Grelha: TAdvStringGrid; CustomFile: TFilename; Update_Num: ShortString; Save: Boolean);
    procedure Advstring_ClientColumn(Grid: TAdvStringGrid; Column: Integer; MinWidth: Integer);

implementation

function Advstring_Locate(Grid: TAdvStringGrid; Column: Integer; StartRow, EndRow: Integer; Value: String; Options: TLocateOptions): Integer;
var
  i : Integer;
  StrItem: String;
  FindPartialKey, FindCaseInsensitive: Boolean;
begin
  RESULT := -1;
  FindPartialKey := loPartialKey in Options;
  FindCaseInsensitive := loCaseInsensitive in Options;

  if FindCaseInsensitive
  then Value := AnsiUpperCase(Value);

  if EndRow = -1
  then EndRow := Grid.RowCount - 1;

  for i := StartRow to EndRow do
  begin
    if FindCaseInsensitive
    then StrItem := AnsiUpperCase(Grid.Cells[Column, i])
    else StrItem := Grid.Cells[Column, i];

    if FindPartialKey
    then begin
      if Pos(Value, StrItem) = 1  // Must start as equal
      then begin
        RESULT := i;
        Break;
      end;
    end
    else begin
      if Value = StrItem
      then begin
        RESULT := i;
        Break;
      end;
    end;
  end;
end;

function Advstring_Locate(Grid: TAdvStringGrid; Column: Integer; StartRow, EndRow: Integer; Value: Double): Integer;
var i : Integer;
begin
  RESULT := -1;
  if EndRow = -1
  then EndRow := Grid.RowCount - 1;

  for i := StartRow to EndRow do
    if Value = Grid.Floats[Column, i]
    then begin
      RESULT := i;
      Break;
    end;
end;

procedure Advstring_Columns_Def(Grelha: TAdvStringGrid; CustomFile: TFilename; Update_Num: ShortString; Save: Boolean);
var
  Ficheiro: TFilename;
  Seccao, Str: String;
  ColsWidth: String;
  IniF: TInifile;
  Separ: Char;
  i, intValue: Integer;
begin
  Separ  := '¥'; // = Chr(165);
  Seccao := Grelha.Owner.Name + '.' + Grelha.Name;

  if CustomFile = ''
  then Ficheiro := Copy(ParamStr(0), 1, Length(ParamStr(0)) - 4) + '.ini'
  else Ficheiro := CustomFile;

  if Save
  then begin
    IniF := TInifile.Create(Ficheiro);
    IniF.WriteString(Seccao, 'UPDATE_NUM', Update_Num);
    ColsWidth     := '';

    for i := 0 to Grelha.ColCount - 1 do
    begin
      ColsWidth := ColsWidth + IntToStr(Grelha.ColWidths[i]) + Separ;
    end;

    IniF.WriteString(Seccao, 'COLS_WIDTH', ColsWidth);
    IniF.Free;
  end
  else
    if FileExists(Ficheiro)
    then begin
      IniF := TInifile.Create(Ficheiro);

      if Update_Num = IniF.ReadString(Seccao, 'UPDATE_NUM', '')
      then begin
        ColsWidth := IniF.ReadString(Seccao, 'COLS_WIDTH', '');

        for i := 0 to Grelha.ColCount - 1 do
        begin
          if ColsWidth <> ''
          then begin
            Str := SUBSTRING_GET(ColsWidth, Separ, i + 1);

            if TryStrToInt(Str, intValue) then
              Grelha.ColWidths[i] := intValue;
          end;
        end;
      end
      else
        IniF.WriteString(Seccao, 'COLS_WIDTH', '');

      IniF.Free;
    end;
end;

procedure Advstring_ClientColumn(Grid: TAdvStringGrid; Column: Integer; MinWidth: Integer);
var
  ColInc, c, ColsWidth: Integer;
begin
  if Column = -1
  then Column := Grid.ColCount - 1;

  ColInc := 0;  // que tem ou não linhas verticais não muda a largura das colunas ...

  // Largura das colunas :
  ColsWidth := 0;  // Margem de erro ...

  for c := 0 to Grid.ColCount - 1 do
    inc(ColsWidth, Grid.ColWidths[c] + ColInc);

  // Ver se o espaço disponível é suficiente para não aparecer o ScrollBar horizontal ...
  // Usamos Grelha.ClientWidth-2 para não encostar totalmente ...
  if Grid.ClientWidth - 2 > ColsWidth - Grid.ColWidths[Column] + MinWidth
  then Grid.ColWidths[Column] := (Grid.ClientWidth - 2) - (ColsWidth - Grid.ColWidths[Column]);
end;

end.
