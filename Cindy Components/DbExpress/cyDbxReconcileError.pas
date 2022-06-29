unit cyDbxReconcileError;

{   Component(s):
    tcyDbxReconcileError

    Description:
    tcyDbxReconcileError allows reconcile error definitions/handling.
    You can attach a tcyDbxReconcileError to one or more tcyDbxTables.



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

interface

uses Classes, DBClient, DB, cyDBX, Dialogs;

type
  TcyDbxReconcileErrorEvent = procedure (Sender: TObject; FromDataset, DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction) of object;

  TcyDbxReconcileError = class(TComponent)
  private
    FInsertMessage: String;
    FDeleteMessage: String;
    FModifyMessage: String;
    FInsertAction: TReconcileAction;
    FModifyAction: TReconcileAction;
    FDeleteAction: TReconcileAction;
    FOnReconcileError: TcyDbxReconcileErrorEvent;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(fromDataset, DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
  published
    property InsertAction: TReconcileAction read FInsertAction write FInsertAction default raCancel;
    property InsertMessage: String read FInsertMessage write FInsertMessage;
    property ModifyAction: TReconcileAction read FModifyAction write FModifyAction default raCancel;
    property ModifyMessage: String read FModifyMessage write FModifyMessage;
    property DeleteAction: TReconcileAction read FDeleteAction write FDeleteAction default raCancel;
    property DeleteMessage: String read FDeleteMessage write FDeleteMessage;
    property OnReconcileError: TcyDbxReconcileErrorEvent read FOnReconcileError write FOnReconcileError;
  end;

implementation

{ TcyDbxReconcileError }

constructor TcyDbxReconcileError.Create(AOwner: TComponent);
begin
  inherited;
  FInsertAction  := raCancel;
  FModifyAction  := raCancel;
  FDeleteAction  := raCancel;

  // Determine at design time if
  // the form is loading or if we have just added the component at design time :
  if csDesigning in ComponentState then
    if Owner <> nil then
      if not (csLoading in Owner.ComponentState) then  // we have just added the component at design time
      begin
        FInsertMessage := cyDBX.cDbxErrorServerInsert;
        FModifyMessage := cyDBX.cDbxErrorServerModify;
        FDeleteMessage := cyDBX.cDbxErrorServerDelete;
      end;
end;

destructor TcyDbxReconcileError.Destroy;
begin

  inherited;
end;

procedure TcyDbxReconcileError.Execute(fromDataset, DataSet: TCustomClientDataSet; E: EReconcileError; UpdateKind: TUpdateKind; var Action: TReconcileAction);
begin
  // Action default value = raSkip
  // Delphi self handling : HandleReconcileError(DataSet, UpdateKind, E);

  case UpdateKind of
    ukInsert:
      begin
        if FInsertMessage <> '' then
          MessageDlg(FInsertMessage, mtError, [mbOk], 0);
        Action := FInsertAction;
      end;

    ukModify:
      begin
        if FModifyMessage <> '' then
          MessageDlg(FModifyMessage, mtError, [mbOk], 0);
        Action := FModifyAction;
      end;

    ukDelete:
      begin
        if FDeleteMessage <> '' then
          MessageDlg(FDeleteMessage, mtError, [mbOk], 0);
        Action := FDeleteAction;
      end;

    else
      MessageDlg('Not handled UpadeKind!', mtWarning, [mbOk], 0);
  end;

  if Assigned(FOnReconcileError) then
    FOnReconcileError(Self, fromDataset, DataSet, E, UpdateKind, Action);

  // Erro ao alterar registo quando este está desactualizado:
  // Action := raSkip;     // Anula para o registo actual - necessita Rollback!
  // Action := raAbort;    // Anula para o registo actual e seguintes - necessita Rollback!
  // Action := raMerge;    // Altera sómente campos não alterados por autras conexões - Chama constantemente SimpleDataSet1ReconcileError!
  // Action := raCorrect;  // Altera o registo com dados no clientDataset - necessita Rollback!
  // Action := raCancel;   // Anula para o registo actual e retira alteração do delta - Mostra valores antes da alteração!
  // Action := raRefresh;  // Chama constantemente SimpleDataSet1ReconcileError!
end;

end.
