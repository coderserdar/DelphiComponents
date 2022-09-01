unit bvFindDialogUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  DBGrids,Grids, StdCtrls, Buttons,ExtCtrls,  Menus, ComCtrls,
  bvwaitGlobeUnit,
{$else}
  QForms,
  QStdCtrls,
  QButtons,
  QExtCtrls,
  QDBGrids,
  QControls,
{$endif}
  Classes,
  SysUtils,
  bvFormSaver,
  bvLocalization;

{$ifndef LINUX}


type
  TbvFindDialogForm = class(TForm)
    LabTextCap: TLabel;
    EditText: TEdit;
    BitBtnFind: TBitBtn;
    BitBtnClose: TBitBtn;
    CheckWholeWordsOnly: TCheckBox;
    CheckMatchCase: TCheckBox;
    EditDirection: TRadioGroup;
    Bevel1: TBevel;
    CheckOneColumn: TCheckBox;
    AnimateGlobe: TAnimate;
    procedure BitBtnFindClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    {ExecProc:TSearchProc;}


    FInExecProc:boolean;
    procedure Execute;

  protected
  public
    { Public declarations }
    //IsDataActive:boolean;

    NeedDestroy:boolean;
    NeedStop:boolean;

    Finder:TComponent;

    property InExecProc:boolean read FInExecProc;

    procedure ShowNotFound;
    function CheckDataActive:boolean;

    constructor  create(Aowner:TComponent); override;
    destructor destroy; override;


  end;

{$else}

type
  TbvFindDialogForm = class(TForm)
    LabTextCap: TLabel;
    EditText: TEdit;
    BitBtnFind: TBitBtn;
    BitBtnClose: TBitBtn;
    CheckWholeWordsOnly: TCheckBox;
    CheckMatchCase: TCheckBox;
    EditDirection: TRadioGroup;
    Bevel1: TBevel;
    CheckOneColumn: TCheckBox;
    procedure BitBtnFindClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtnCloseClick(Sender: TObject);
    procedure EditTextChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    {ExecProc:TSearchProc;}


    FInExecProc:boolean;
    procedure Execute;

  protected
  public
    { Public declarations }
    //IsDataActive:boolean;

    NeedDestroy:boolean;
    NeedStop:boolean;

    Finder:TComponent;

    property InExecProc:boolean read FInExecProc;

    procedure ShowNotFound;
    function CheckDataActive:boolean;

    constructor  create(Aowner:TComponent); override;
    destructor destroy; override;


  end;


{$endif}

var
  bvFindDialogForm: TbvFindDialogForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

uses bvDBGrid,bvdbGridSaver,
     //bvdbTableSaver,bvsaveTableUnit,
     DB,bvFindUnit,bvMessageUnit;


destructor TbvFindDialogForm.Destroy;
begin
  if assigned(Finder) and (Finder is bv_Find) and ((Finder as bv_find).fdialog=self)
  then (Finder as bv_find).fdialog:=nil;
  inherited;
end;

procedure TbvFindDialogForm.BitBtnFindClick(Sender: TObject);
begin
  {$ifndef LINUX}
  animateGlobe.Visible:=true;
  AnimateGlobe.active:=true;
  {$endif}

  try
    Execute;
  except
  end;
end;


procedure TbvFindDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree;
end;

procedure TbvFindDialogForm.BitBtnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TbvFindDialogForm.EditTextChange(Sender: TObject);
begin
  BitBtnFind.Enabled:=not (trim(EditText.text)='');
end;

procedure TbvFindDialogForm.FormCreate(Sender: TObject);
begin
  BitBtnFind.Enabled:=not (trim(EditText.text)='');

  self.caption:=StrFinderCaption;
  LabTextCap.caption:=StrTextCaption;
  CheckOneColumn.caption:=StrOneColumnCaption;
  CheckWholeWordsOnly.caption:=StrWholeWordsOnly;
  CheckMatchCase.caption:=StrmatchCase;
  EditDirection.caption:=StrDirection;
  EditDirection.Items[0]:=StrUp;
  EditDirection.Items[1]:=StrDown;
  BitBtnFind.caption:=StrFind;
  BitBtnClose.caption:=StrCancel;

  NeedDestroy:=false;
  FInExecProc:=false;
  NeedStop:=false;
end;

procedure TbvFindDialogForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and (BitBtnFind.enabled) then begin
    Key:=#0;
    BitBtnFind.Click;
  end;
end;

procedure TbvFindDialogForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FInExecProc then begin
    NeedStop:=true;
    CanClose:=false;
  end
  else CanClose:=true;
end;

procedure TbvFindDialogForm.Execute;
var
     Is_Ok :boolean;
        iFieldNum,iFieldCount :Integer;
        DATA:TDataSource;
        DATAField:TField;
begin
 NeedDestroy:=false;
 FInExecProc:=true;
 NeedStop:=false;

 try
   EditText.Enabled:=false;
   CheckWholeWordsOnly.Enabled:=false;
   CheckMatchCase.Enabled:=false;
   EditDirection.Enabled:=false;
   BitBtnFind.Enabled:=false;
   CheckOneColumn.Enabled:=false;

   try


      if (Finder as bv_Find).ThisGrid is TDBGrid then DATA:=((Finder as bv_Find).ThisGrid as TDBGrid).DataSource
      else Data:=nil;

      with Data.DataSet do
      try
         if eof then last
         else if bof then first;

         DisableControls;
         if (Finder as bv_Find).ThisGrid is TDBGrid then
                 ((Finder as bv_Find).ThisGrid as TDBGrid).Options:=((Finder as bv_Find).ThisGrid as TDBGrid).Options-[dgAlwaysShowSelection];
  //       end;
         if (Finder as bv_Find).ThisGrid is TDBGrid then begin
           iFieldNum:=((Finder as bv_Find).ThisGrid as TDBGrid).SelectedIndex;
           iFieldCount:=((Finder as bv_Find).ThisGrid as TDBGrid).Fieldcount;
         end
         else begin
           iFieldNum:=0;
           iFieldCount:=0;
         end;


         if CheckOneColumn.Checked then begin
           iFieldNum:=iFieldCount;
         end;

         repeat
            Application.ProcessMessages;

            if NeedDestroy
               or NeedStop
               or (csdestroying in self.Componentstate)
               or not Active
               or not checkDataActive
            then begin
               break
            end;



            if (iFieldNum)<(iFieldCount-1)
            then begin

              inc(iFieldNum);

              if CheckOneColumn.Checked then begin
                iFieldNum:=iFieldCount;
              end;

              while not NeedDestroy and not NeedStop and CheckDataActive do
              begin

                Application.ProcessMessages;

                if NeedDestroy or NeedStop
                   or (csdestroying in self.Componentstate)
                   or not Active or not CheckDataActive
                then begin
                   break
                end;

                if (Finder as bv_Find).ThisGrid is TDBGrid then begin
                   if CheckOneColumn.Checked
                   then
                      DataField:=((Finder as bv_Find).ThisGrid as TDBGrid).SelectedField
                   else
                      DataField:=((Finder as bv_Find).ThisGrid as TDBGrid).Fields[ifieldnum]
                end
                else DataField:=nil;

                if assigned(datafield)
                then begin
                  if not CheckWholeWordsOnly.checked then
                     if  CheckMatchCase.Checked then
                        is_ok:=(StrPos(pchar(DataField.Displaytext),
                                pchar(EditText.Text))<>nil)
                     else
                        is_ok:=(StrPos(pchar(AnsiUpperCase(DataField.DisplayText)),
                                             pchar(AnsiUpperCase(EditText.Text)))<>nil)
                  else
                     if CheckMatchCase.Checked then
                         is_ok:=dataField.AsString=EditText.Text
                     else
                         is_ok:=AnsiUppercase(dataField.DisplayText)=
                                AnsiUppercase(EditText.Text);
                end
                else Is_Ok:=false;

                if  is_ok then begin
                   EnableControls;
                   if (Finder as bv_Find).ThisGrid is TDBGrid then begin
                     ((Finder as bv_Find).ThisGrid as TDBGrid).Options:=((Finder as bv_Find).ThisGrid as TDBGrid).Options+[dgAlwaysShowSelection];
                     if NOt CheckOneColumn.Checked then
                        ((Finder as bv_Find).ThisGrid as TDBGrid).SelectedIndex:=iFieldNum;
                     if (Finder as bv_Find).ThisGrid is tbvDbGrid then
                     ((Finder as bv_Find).ThisGrid as TbvDBGrid).layoutchanged;
                   end;
                   exit;
                end
                else  if iFieldNum<iFieldcount-1
                  then inc(iFieldNum)
                  else break;
              end;
            end;

            if not NeedDestroy  and not NeedStop and CheckDataActive then begin
               if EditDirection.ItemIndex=1 then next
               else prior;
               iFieldNum:=-1;
            end;

            if NeedDestroy or NeedStop or (csdestroying in {Thform.}self.Componentstate)
               or not Active or not CheckDataActive
            then begin
               break
            end
         until NeedDestroy or needstop or not active or not CheckDataActive or  eof or  BOF ;

         if not NeedDestroy and not needStop and  not (csdestroying in {Thform.}self.Componentstate)
         then begin
            if active and CheckDataActive then EnableControls;
            if assigned(Finder)
               and Assigned((Finder as bv_Find).ThisGrid)
               and ((Finder as bv_Find).ThisGrid is TDBGrid)
            then begin
                ((Finder as bv_Find).ThisGrid as TDBGrid).Options:=((Finder as bv_Find).ThisGrid as TDBGrid).Options+[dgAlwaysShowSelection];
                if Not CheckOneColumn.Checked then begin
                  if ifieldNum=-1 then  ((Finder as bv_Find).ThisGrid as TDBGrid).SelectedIndex:=0
                  else ((Finder as bv_Find).ThisGrid as TDBGrid).SelectedIndex:=ifieldNum ;
                end;
                if (Finder as bv_Find).ThisGrid is tbvDbGrid then
                ((Finder as bv_Find).ThisGrid as TbvDBGrid).layoutchanged;
            end;
            if Active and (eof or bof) then begin
               {$ifndef LINUX}
               AnimateGlobe.Active:=false;
               {$endif}
               {Synchronize(}ShowNotFound{)};
            end
         end
      finally
         enablecontrols;
      end;
   finally
      if Not NeedDestroy and not (csDestroying in {Thform.}self.ComponentState)
      then
      begin
        EditText.Enabled:=true;
        CheckWholeWordsOnly.Enabled:=true;
        CheckMatchCase.Enabled:=true;
        EditDirection.Enabled:=true;
        BitBtnFind.Enabled:=true;
        {$ifndef LINUX}
        AnimateGlobe.active:=false;
        animateGlobe.visible:=false;
        {$endif}
        CheckOneColumn.Enabled:=true;
      end;
   end;
 finally
   FInExecProc:=false;
   if NeedDestroy then Close;
 end;
end;


procedure TbvFindDialogForm.ShowNotFound;
begin
  bvMessage(StrNotFound);
end;

function TbvFindDialogForm.CheckDataActive;
begin
  REsult:=
     not NeedDestroy
     and Assigned(Finder)
     and (Finder is bv_Find)
     and assigned((Finder as bv_find).ThisGrid)
     and assigned((Finder as bv_find).ThisGrid.datasource)
     and assigned((Finder as bv_find).ThisGrid.datasource.dataset)
     and (Finder as bv_find).ThisGrid.DataSource.DataSet.active;
end;

procedure TbvFindDialogForm.FormDestroy(Sender: TObject);
begin
  {$ifndef LINUX}
  AnimateGlobe.Active:=false;
  {$endif}
  saveform(self);
end;

constructor  TbvFindDialogForm.create(Aowner:TComponent);
begin
  inherited;
  REstoreForm(self);
  {$ifndef LINUX}
  AnimateGlobe.ResName:=bvwaitglobeunit.FINDFILERES;
  {$endif}

end;

procedure TbvFindDialogForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=vk_F3 then begin
     if BitBtnFind.enabled then bitBtnFind.Click;
  end
end;

end.
