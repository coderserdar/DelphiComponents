unit cyDBXJedi;

{   Unit cyDBXJedi

    Description:
    Unit with ClientDataset/DB Express functions using Jedi components

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

{$I ..\Core\cyCompilerDefines.inc}

interface

uses
  {$IFDEF MsWindows}
  Dialogs,
  {$ELSE}
  fmx.Dialogs, System.UITypes,
  {$ENDIF}

  Classes, Variants, Graphics, SimpleDS, DB, DBClient, {$IFDEF DELPHI2007_OR_ABOVE} DBXCommon, {$ENDIF} Provider, SysUtils, SqlExpr, cyStrUtils, cyDb, JvJVCLUtils, cyJedi;

function SQLReturnFieldValueAsGraphic(aConnection: TSQLConnection; SQL: TStrings; OnPicture: TPicture; const ReplaceTMetafileBy: TGraphicClass = Nil; const ClearIfFieldIsNull: Boolean = true): Boolean; overload;

function SQLReturnFieldValueAsGraphic(aConnection: TSQLConnection; OnPicture: TPicture; const ReplaceTMetafileBy: TGraphicClass = Nil; const ClearIfFieldIsNull: Boolean = true;
          const SqlLine1: String = ''; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                                       const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Boolean; overload;


implementation

function SQLReturnFieldValueAsGraphic(aConnection: TSQLConnection; SQL: TStrings; OnPicture: TPicture; const ReplaceTMetafileBy: TGraphicClass = Nil; const ClearIfFieldIsNull: Boolean = true): Boolean;
var
  FieldIsNull: Boolean;
  EmptyBmp: TBitmap;
  aQuery: TSQLQuery;
  // Graphic: TGraphic;
  // GraphicClass: TGraphicClass;
  // Stream: TMemoryStream;
begin
  Result := false;
  aQuery := TSQLQuery.Create(Nil);

  try
    aQuery.SQLConnection := aConnection;
    aQuery.SQL.AddStrings(SQL);
    aQuery.Active := true;

    FieldIsNull := true;

    if not aQuery.Eof then
      if aQuery.Fields[0] is TBlobField then
        FieldIsNull := TBlobField(aQuery.Fields[0]).IsNull;

    if FieldIsNull then
    begin
      // Clear actual graphic :
      if Assigned(OnPicture.Graphic) and ClearIfFieldIsNull then
      begin
        EmptyBmp := TBitmap.Create;

        try
          EmptyBmp.Width := 1;
          EmptyBmp.Height := 1;
          OnPicture.Graphic.Assign(EmptyBmp);
          OnPicture.Bitmap.Modified := true;      // Refresh when image on panel visible by with ManualDock ...
        finally
          EmptyBmp.Free;
        end;
      end;
    end
    else
      Result := cyJedi.AssignBlobFieldToPicture(TBlobField(aQuery.Fields[0]), OnPicture, ReplaceTMetafileBy, ClearIfFieldIsNull);
  finally
    aQuery.Active := false;
    aQuery.Free;
  end;
end;

function SQLReturnFieldValueAsGraphic(aConnection: TSQLConnection; OnPicture: TPicture; const ReplaceTMetafileBy: TGraphicClass = Nil; const ClearIfFieldIsNull: Boolean = true;
          const SqlLine1: String = ''; const SqlLine2: String = ''; const SqlLine3: String = ''; const SqlLine4: String = '';
                                       const SqlLine5: String = ''; const SqlLine6: String = ''; const SqlLine7: String = ''; const SqlLine8: String = ''): Boolean;
var
  SQL: TStrings;
begin
  SQL := TStringList.Create;

  if SqlLine1 <> '' then SQL.Add(SqlLine1);
  if SqlLine2 <> '' then SQL.Add(SqlLine2);
  if SqlLine3 <> '' then SQL.Add(SqlLine3);
  if SqlLine4 <> '' then SQL.Add(SqlLine4);
  if SqlLine5 <> '' then SQL.Add(SqlLine5);
  if SqlLine6 <> '' then SQL.Add(SqlLine6);
  if SqlLine7 <> '' then SQL.Add(SqlLine7);
  if SqlLine8 <> '' then SQL.Add(SqlLine8);

  try
    Result := SQLReturnFieldValueAsGraphic(aConnection, SQL, OnPicture, ReplaceTMetafileBy, ClearIfFieldIsNull);
  finally
    SQL.Free;
  end;
end;

end.
