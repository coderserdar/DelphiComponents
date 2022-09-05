{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

{*******************************************************}
{                                                       }
{       ����ͣ����Ϣ                                    }
{       CnDockInfo ��Ԫ                                 }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDockInfo;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�����ͣ����Ϣ 
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.13 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Controls, Inifiles, Registry, Classes, Sysutils, Forms, Messages,
  CnDockFormControl, CnDockSupportClass, CnDockSupportProc;

type
  TCnDockInfoTree = class;

  TDockFormStyle = (dsNormal, dsConjoin, dsTab, dsDockPanel);

  TCnDockInfoZone = class(TCnBaseZone)
  private
    FDockFormName: string;      //ͣ�����������
    FParentName: string;        //�����������
    FDockRect: TRect;           //ͣ������Ĵ�С
    FLastDockSiteName: string;  //��һ��ͣ���Ĵ��������
    FUnDockLeft: Integer;       //��һ��ͣ�������
    FUnDockTop: Integer;        //��һ��ͣ�����ϱ�
    FLRDockWidth: Integer;      //ͣ��ʱ���ڵĿ��
    FTBDockHeight: Integer;     //ͣ��ʱ���ڵĸ߶�
    FUnDockWidth: Integer;      //����ʱ���ڵĿ��
    FUnDockHeight: Integer;     //����ʱ���ڵĸ߶�
    FVSPaneWidth: Integer;      //���Ŀ��
    FVisible: Boolean;          //�Ƿ�ɼ�
    FBorderStyle: TBorderStyle; //�����BorderStyle����
    FFormStyle: TFormStyle;     //�����FormStyle����
    FWindowState: TWindowState; //�����WindowState����
    FCanDocked: Boolean;        //�Ƿ��ͣ��
    FEachOtherDocked: Boolean;  //�Ƿ���໥ͣ��
    FLeftDocked: Boolean;       //�Ƿ��ͣ�������
    FTopDocked: Boolean;        //�Ƿ��ͣ�����ϱ�
    FRightDocked: Boolean;      //�Ƿ��ͣ�����ұ�
    FBottomDocked: Boolean;     //�Ƿ��ͣ�����±�
    FDockFormStyle: TDockFormStyle;//ͣ������
    FDockClientData: string;    //ͣ����Ϣ
    FDockControl: TWinControl;  //����ָ��
    function GetChildControlCount: Integer; //����Ӵ���ĸ���
  public
    procedure SetDockInfoFromControlToNode(Control: TControl); virtual;
    procedure SetDockInfoFromNodeToControl(Control: TControl); virtual;
    procedure SetDockInfoFromDockControlToNode(DockControl: TCnDockBaseControl); virtual;
    procedure SetDockInfoFromNodeToDockControl(DockControl: TCnDockBaseControl); virtual;
    ////////////////////////////////////////////////////////////////////
    property DockFormName: string read FDockFormName write FDockFormName;
    property ParentName: string read FParentName write FParentName;
    property DockRect: TRect read FDockRect write FDockRect;
    property LastDockSiteName: string read FLastDockSiteName write FLastDockSiteName;
    property UnDockLeft: Integer read FUnDockLeft write FUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write FUnDockTop;
    property LRDockWidth: Integer read FLRDockWidth write FLRDockWidth;
    property TBDockHeight: Integer read FTBDockHeight write FTBDockHeight;
    property UnDockWidth: Integer read FUnDockWidth write FUnDockWidth;
    property UnDockHeight: Integer read FUnDockHeight write FUnDockHeight;
    property VSPaneWidth: Integer read FVSPaneWidth write FVSPaneWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    property WindowState: TWindowState read FWindowState write FWindowState;
    property Visible: Boolean read FVisible write FVisible;
    property CanDocked: Boolean read FCanDocked write FCanDocked;
    property EachOtherDocked: Boolean read FEachOtherDocked write FEachOtherDocked;
    property LeftDocked: Boolean read FLeftDocked write FLeftDocked;
    property TopDocked: Boolean read FTopDocked write FTopDocked;
    property RightDocked: Boolean read FRightDocked write FRightDocked;
    property BottomDocked: Boolean read FBottomDocked write FBottomDocked;
    property DockFormStyle: TDockFormStyle read FDockFormStyle write FDockFormStyle;
    property DockClientData: string read FDockClientData write FDockClientData;
    property DockControl: TWinControl read FDockControl write FDockControl;
  end;

  TCnDockInfoStyle =
    ( isNone,         //û���κ�����
      isReadFileInfo, //���ڶ��ļ�
      isWriteFileInfo,//����д�ļ�
      isReadRegInfo,  //���ڶ�ע���
      isWriteRegInfo);//����дע���

  {�洢ͣ����Ϣ����}
  TCnDockInfoTree = class(TCnBaseTree)
  private
    FDockInfoIni: TIniFile;  //INI�ļ�
    FDockInfoReg: TRegistry; //ע�����
    FRegName: string;        //ע����·��
    FCnDockInfoStyle: TCnDockInfoStyle;//���ڴ����״̬
    FDataStream: TMemoryStream; //���������洢�ͻ�ԭͣ����Ϣ
    function FindDockForm(FormName: string): TCustomForm;
    function CreateHostControl(ATreeZone: TCnDockInfoZone): TWinControl;
//    function FindDockHost(ControlName: string): TWinControl;
  protected
    procedure ScanTreeZone(TreeZone: TCnBaseZone); override;    //��ɨ�赽һ���ڵ�ʱ�������������
    procedure CreateZoneAndAddInfoFromIni; virtual;//��INI�ļ�����ȡ��ͣ����Ϣ
    procedure CreateZoneAndAddInfoFromReg; virtual;//��ע�������ȡ��ͣ����Ϣ
    procedure SetDockControlInfo(ATreeZone: TCnDockInfoZone); virtual;
  public
    constructor Create(TreeZone: TCnTreeZoneClass); override;
    destructor Destroy; override;
    procedure CreateZoneAndAddInfoFromApp(Control: TControl); virtual;//��Ӧ�ó�������ȡ��ͣ����Ϣ
//    procedure DoFloatAllForm; virtual;            //ʹ���еĴ��嶼��Ϊ������
    procedure ReadInfoFromIni;                    //��INI�ļ��ж����ݵ�����
    procedure ReadInfoFromReg(RegName: string);   //��ע����ж����ݵ�����
    procedure WriteInfoToIni;                     //�����е�����д��INI�ļ���
    procedure WriteInfoToReg(RegName: string);    //�����е�����д��ע�����
    property DockInfoIni: TIniFile read FDockInfoIni write FDockInfoIni;
    property DockInfoReg: TRegistry read FDockInfoReg write FDockInfoReg;
  end;

implementation

uses CnDockGlobal, CnVSNETDockStyle;

{��Ӧ�ó����в�������ΪFormName�Ĵ���}
function FindDockForm(FormName: string): TCustomForm;
begin
  if Pos(gs_CnDockInfoSplitter, FormName) > 0 then Result := nil
  else
  begin
    Result := Cn_FindDockFormWithName(FormName);
  end;
end;

function FindDockPanel(ControlName: string): TWinControl;
var Index: Word;
  DockServer: TCnDockServer;
begin
  Result := nil;
  Index := Pos(gs_CnDockInfoSplitter, ControlName);
  if Index = 0 then Exit;
  Result := FindDockForm(Copy(ControlName, 1, Index - 1));
  if Result <> nil then
  begin
    DockServer := FindDockServer(Result);
    if DockServer <> nil then
      with DockServer do
      begin
        if Pos('TopDockPanel', ControlName) > Index then
          Result := TopDockPanel
        else if Pos('LeftDockPanel', ControlName) > Index then
          Result := LeftDockPanel
        else if Pos('BottomDockPanel', ControlName) > Index then
          Result := BottomDockPanel
        else if Pos('RightDockPanel', ControlName) > Index then
          Result := RightDockPanel;
        if (Result <> nil) and (Pos('PopupPanel', ControlName) > 20) then
          Result := TCnVSNETDockPanel(Result).VSChannel.VSPopupPanel;
      end;
  end;
end;

{��������ΪControlName��WinControl}
function FindDockHost(ControlName: string): TWinControl;
begin
  Result := FindDockForm(ControlName);
  if Result = nil then
    Result := FindDockPanel(ControlName);
end;

{ TCnDockInfoZone }

function TCnDockInfoZone.GetChildControlCount: Integer;
var AZone: TCnBaseZone;
begin
  Result := 0;
  if ChildZone <> nil then
  begin
    Inc(Result);
    AZone := ChildZone;
    while AZone.NextSibling <> nil do
    begin
      AZone := AZone.NextSibling;
      if TCnDockInfoZone(AZone).DockControl <> nil then
        Inc(Result);
    end;
  end;
end;

procedure TCnDockInfoZone.SetDockInfoFromControlToNode(Control: TControl);
begin
  DockRect      := Control.BoundsRect;
  UnDockWidth   := Control.UndockWidth;
  UnDockHeight  := Control.UndockHeight;
  if Control is TCnVSPopupPanel then
    Control.Visible := False
  else
    Visible := Control.Visible;
    
  if Control is TForm then
  begin
    BorderStyle := TForm(Control).BorderStyle;
    FormStyle := TForm(Control).FormStyle;
    WindowState := TForm(Control).WindowState;
    LRDockWidth := Control.LRDockWidth;
    TBDockHeight := Control.TBDockHeight;
  end;
end;

procedure TCnDockInfoZone.SetDockInfoFromDockControlToNode(
  DockControl: TCnDockBaseControl);
  function GetLastDockSiteName(AControl: TControl): string;
  begin
    Result := gs_CannotFindWindow;
    if AControl <> nil then
    begin
      if AControl.Parent is TCnDockableForm then
        Result := AControl.Parent.Name
      else if AControl is TCnDockPanel then
        Result := AControl.Parent.Name + gs_CnDockInfoSplitter + AControl.Name;
    end;
  end;
begin
  CanDocked       := DockControl.EnableDock;
  EachOtherDocked := DockControl.EachOtherDock;
  LeftDocked      := DockControl.LeftDock;
  TopDocked       := DockControl.TopDock;
  RightDocked     := DockControl.RightDock;
  BottomDocked    := DockControl.BottomDock;
  if DockControl is TCnDockClient then
  begin
    VSPaneWidth   := TCnDockClient(DockControl).VSPaneWidth;
    UnDockLeft := TCnDockClient(DockControl).UnDockLeft;
    UnDockTop := TCnDockClient(DockControl).UnDockTop;
    LastDockSiteName := GetLastDockSiteName(TCnDockClient(DockControl).LastDockSite);
  end
  else
    VSPaneWidth   := 0;
end;

procedure TCnDockInfoZone.SetDockInfoFromNodeToControl(Control: TControl);
var CnDockServer: TCnDockServer;

  procedure SetPopupPanelSize(PopupPanel: TCnVSPopupPanel);
  begin
{    case PopupPanel.VSChannel.Align of
      alLeft,
      alRight: Control.Width := DockRect.Right - DockRect.Left;
      alTop,
      alBottom:Control.Width := DockRect.Bottom - DockRect.Top;
    end;}
  end;

  procedure SetDockSiteSize(DockSite: TCnDockPanel);
  begin
    if DockSite.Align in [alTop, alBottom] then
      DockSite.CnDockManager.DockSiteSize := DockRect.Bottom - DockRect.Top
    else
      DockSite.CnDockManager.DockSiteSize := DockRect.Right - DockRect.Left;
  end;

{  procedure ResetVSChannelVisible;
  var i: Integer;
  begin
    for i := 0 to 3 do
    begin
      if CnDockServer.DockPanel[i] is TCnVSNETDockPanel then
      begin
        with TCnVSNETDockPanel(CnDockServer.DockPanel[i]) do
        begin
          VSChannel.Visible := VSChannel.BlockCount > 0;
        end;
      end;
    end;
  end;}
//var DockClient: TCnDockClient;
begin
{  DockClient := FindDockClient(Control);
  if DockClient <> nil then
    Control.Visible := False;}
  if (ParentName = '') or ((Control is TCnDockPanel) and
    (TCnDockPanel(Control).VisibleDockClientCount > 0)) then
  begin
    TWinControl(Control).DisableAlign;
    try
      if Control is TForm then
      begin
        TForm(Control).BorderStyle := BorderStyle;
        TForm(Control).FormStyle := FormStyle;
        if WindowState = wsNormal then
          Control.BoundsRect := DockRect;
        TForm(Control).WindowState := WindowState;
      end else
      begin
        if Control is TCnVSPopupPanel then
          SetPopupPanelSize(Control as  TCnVSPopupPanel)
        else
          SetDockSiteSize(Control as TCnDockPanel);
      end;
      CnDockServer := FindDockServer(Control);
      if CnDockServer <> nil then
      begin
        CnDockServer.GetClientAlignControl(alTop);
        CnDockServer.GetClientAlignControl(alBottom);
        CnDockServer.GetClientAlignControl(alLeft);
        CnDockServer.GetClientAlignControl(alRight);
//        ResetVSChannelVisible;
      end;
    finally
      TWinControl(Control).EnableAlign;
    end;
  end;
  Control.Visible := Visible;
  Control.LRDockWidth := LRDockWidth;
  Control.TBDockHeight := TBDockHeight;
  Control.UndockHeight := UndockHeight;
  Control.UndockWidth := UndockWidth;
end;

procedure TCnDockInfoZone.SetDockInfoFromNodeToDockControl(
  DockControl: TCnDockBaseControl);
  function GetLastDockSite(AName: string): TWinControl;
  begin
    Result := FindDockPanel(AName);
    if Result = nil then
    begin
      Result := FindDockForm(AName);
      if Result is TCnDockableForm then
        Result := TCnDockableForm(Result).DockableControl;
    end;
  end;
begin
  if (DockControl is TCnDockClient) then
  begin
    TCnDockClient(DockControl).UnDockLeft := UnDockLeft;
    TCnDockClient(DockControl).UnDockTop := UnDockTop;
    TCnDockClient(DockControl).LastDockSite := GetLastDockSite(LastDockSiteName);
    if Visible then
    begin
      TCnDockClient(DockControl).ParentVisible := False;
      TCnDockClient(DockControl).MakeShowEvent;
    end else TCnDockClient(DockControl).MakeHideEvent;
    TCnDockClient(DockControl).VSPaneWidth := VSPaneWidth;
  end;
  DockControl.EnableDock := CanDocked;
  DockControl.LeftDock := LeftDocked;
  DockControl.TopDock := TopDocked;
  DockControl.BottomDock := BottomDocked;
  DockControl.RightDock := RightDocked;
end;

{ TCnDockInfoTree }

constructor TCnDockInfoTree.Create(TreeZone: TCnTreeZoneClass);
begin
  inherited Create(TreeZone);
  FDockInfoIni := nil;
  FCnDockInfoStyle := isNone;
  FDataStream := TMemoryStream.Create;
end;

procedure TCnDockInfoTree.CreateZoneAndAddInfoFromApp(Control: TControl);
var i: Integer;
  TreeZone: TCnDockInfoZone;
  ADockBaseControl: TCnDockBaseControl;
  TmpDockPanel: TCnDockPanel;
begin
  TreeZone := TCnDockInfoZone(AddChildZone(CurrTreeZone, nil));
  with TreeZone do
  begin
    ParentName := TCnDockInfoZone(CurrTreeZone).DockFormName;
    SetDockInfoFromControlToNode(Control);
    if Control is TCnDockPanel then
      DockFormName := TCnDockInfoZone(CurrTreeZone).DockFormName +
        gs_CnDockInfoSplitter + Control.Name
    else DockFormName := Control.Name;
    FDataStream.Clear;
    if Control is TCnTabDockHostForm then
      TCnTabDockHostForm(Control).PageControl.SaveToStream(FDataStream)
    else if Control is TCnConjoinDockHostForm then
      TCnConjoinDockHostForm(Control).Panel.DockManager.SaveToStream(FDataStream)
    else if Control is TCnDockPanel then
      TCnDockPanel(Control).DockManager.SaveToStream(FDataStream);
    DockClientData := Cn_StreamDataToString(FDataStream);
    ADockBaseControl := FindDockBaseControl(Control);
    if ADockBaseControl <> nil then
    begin
      SetDockInfoFromDockControlToNode(ADockBaseControl);
      if Control is TCnTabDockHostForm then
        DockFormStyle := dsTab
      else if Control is TCnConjoinDockHostForm then
        DockFormStyle := dsConjoin
      else DockFormStyle := dsNormal;
      if ADockBaseControl is TCnDockClient then
      begin
        if Control is TCnDockableForm then
        begin
          with TCnDockableForm(Control).DockableControl do
          begin
            for i := 0 to DockClientCount -1 do
            begin
              CurrTreeZone := TreeZone;
              CreateZoneAndAddInfoFromApp(DockClients[i]);
              CurrTreeZone := TreeZone.GetParentZone;
            end;
          end;
        end;
      end else
      begin
        for i := 0 to 3 do
        begin
          CurrTreeZone := TreeZone;
          TmpDockPanel := TCnDockServer(ADockBaseControl).DockPanel[i];
          CreateZoneAndAddInfoFromApp(TmpDockPanel);
          if TmpDockPanel is TCnVSNETDockPanel then
            CreateZoneAndAddInfoFromApp(TCnVSNETDockPanel(TmpDockPanel).VSChannel.VSPopupPanel);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;

    if Control is TCnDockPanel then
    begin
      DockFormStyle := dsDockPanel;
      if Control is TCnVSPopupPanel then
      begin
        with TCnVSPopupPanel(Control) do
        begin
          for i := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[i]));
            CurrTreeZone := TreeZone.GetParentZone;
          end;
        end;
      end else
      with TCnDockPanel(Control) do
      begin
        for i := 0 to DockClientCount - 1 do
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfoFromApp(TWinControl(DockClients[i]));
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;
end;

procedure TCnDockInfoTree.CreateZoneAndAddInfoFromIni;
var Sections: TStringList;
  i: Integer;
  procedure CreateZoneAndAddInfo(Index: Integer);
  var i: Integer;
    TreeZone: TCnDockInfoZone;
  begin
    TreeZone := TCnDockInfoZone(AddChildZone(CurrTreeZone, nil));
    with TreeZone, FDockInfoIni do
    begin
      {��ȡINI�ļ��е����ݵ�һ�����ڵ�}
      DockFormName  := Sections[Index];
      ParentName    := ReadString(DockFormName, 'ParentName', 'ERROR');
      DockRect := Rect(ReadInteger(DockFormName, 'DockLeft', 0),
                       ReadInteger(DockFormName, 'DockTop', 0),
                       ReadInteger(DockFormName, 'DockRight', 100),
                       ReadInteger(DockFormName, 'DockBottom', 100));
      LastDockSiteName:= ReadString(DockFormName, 'LastDockSiteName', 'ERROR');
      UnDockLeft    := ReadInteger(DockFormName, 'UnDockLeft', 100);
      UnDockTop     := ReadInteger(DockFormName, 'UnDockTop', 100);
      LRDockWidth   := ReadInteger(DockFormName, 'LRDockWidth', 100);
      TBDockHeight  := ReadInteger(DockFormName, 'TBDockHeight', 100);
      UnDockWidth   := ReadInteger(DockFormName, 'UndockWidth', 100);
      UnDockHeight  := ReadInteger(DockFormName, 'UndockHeight', 100);
      VSPaneWidth   := ReadInteger(DockFormName, 'VSPaneWidth', 100);
      Visible       := ReadBool(DockFormName, 'Visible', True);
      BorderStyle   := TBorderStyle(ReadInteger(DockFormName, 'BorderStyle', 0));
      FormStyle     := TFormStyle(ReadInteger(DockFormName, 'FormStyle', 0));
      WindowState   := TWindowState(ReadInteger(DockFormName, 'WindowState', 0));
      DockFormStyle := TDockFormStyle(ReadInteger(DockFormName, 'DockFormStyle', 0));
      CanDocked     := ReadBool(DockFormName, 'CanDocked', True);
      EachOtherDocked := ReadBool(DockFormName, 'EachOtherDocked', True);
      LeftDocked    := ReadBool(DockFormName, 'LeftDocked', LeftDocked);
      TopDocked     := ReadBool(DockFormName, 'TopDocked', True);
      RightDocked   := ReadBool(DockFormName, 'RightDocked', True);
      BottomDocked  := ReadBool(DockFormName, 'BottomDocked', True);
      DockClientData:= ReadString(DockFormName, 'DockClientData', '');
    end;
    for i := Index - 1 downto 0 do
    begin
      if FDockInfoIni.ReadString(Sections[i], 'ParentName', 'ERROR') = Sections[Index] then
      begin
        CurrTreeZone := TreeZone;
        CreateZoneAndAddInfo(i);
        CurrTreeZone := TreeZone.GetParentZone;
      end;
    end;
  end;

begin
  Sections := TStringList.Create;
  try
    FDockInfoIni.ReadSections(Sections);
    {����INI�ļ������������Ϣȡ������������}
    FCnDockInfoStyle := isReadFileInfo;
    for i := Sections.Count - 1 downto 0 do
    begin
      if FDockInfoIni.ReadString(Sections[i], 'ParentName', 'ERROR') = '' then
        CreateZoneAndAddInfo(i);
    end;
    FCnDockInfoStyle := isNone;
  finally
    Sections.Free;
  end;
end;

procedure TCnDockInfoTree.CreateZoneAndAddInfoFromReg;
var FormList: TStringList;
  procedure CreateZoneAndAddInfo(Index: Integer);
  var i: Integer;
    TreeZone: TCnDockInfoZone;
  begin
    FDockInfoReg.OpenKey(FRegName, False);
    if FDockInfoReg.KeyExists(FormList[Index]) then
    begin
      FDockInfoReg.OpenKey(FRegName + '\' + FormList[Index], False);
      TreeZone := TCnDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, FDockInfoReg do
      begin
        {��ȡע����е����ݵ�һ�����ڵ�}
        DockFormName  := FormList[Index];
        ParentName    := ReadString('ParentName');
        DockRect := Rect(ReadInteger('DockLeft'),
                         ReadInteger('DockTop'),
                         ReadInteger('DockRight'),
                         ReadInteger('DockBottom'));
        LRDockWidth   := ReadInteger('LRDockWidth');
        LastDockSiteName:= ReadString('LastDockSiteName');
        UnDockLeft    := ReadInteger('UnDockLeft');
        UnDockTop     := ReadInteger('UnDockTop');
        TBDockHeight  := ReadInteger('TBDockHeight');
        UnDockWidth   := ReadInteger('UnDockWidth');
        UnDockHeight  := ReadInteger('UnDockHeight');
        VSPaneWidth   := ReadInteger('VSPaneWidth');
        Visible       := ReadBool('Visible');
        BorderStyle   := TBorderStyle(ReadInteger('BorderStyle'));
        FormStyle     := TFormStyle(ReadInteger('FormStyle'));
        WindowState   := TWindowState(ReadInteger('WindowState'));
        DockFormStyle := TDockFormStyle(ReadInteger('DockFormStyle'));
        CanDocked     := ReadBool('CanDocked');
        EachOtherDocked := ReadBool('EachOtherDocked');
        LeftDocked    := ReadBool('LeftDocked');
        TopDocked     := ReadBool('TopDocked');
        RightDocked   := ReadBool('RightDocked');
        BottomDocked  := ReadBool('BottomDocked');
        DockClientData:= ReadString('DockClientData');
      end;
      for i := Index - 1 downto 0 do
      begin
        FDockInfoReg.OpenKey(FRegName + '\' + FormList[i], False);
        if FDockInfoReg.ReadString('ParentName') = FormList[Index] then
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfo(i);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;
var
  cp, cp1: PChar;
  i: Integer;
begin
  FormList := TStringList.Create;
  try
    if FDockInfoReg.OpenKey(FRegName, False) then
    begin
      cp := PChar(FDockInfoReg.ReadString('FormNames'));
      cp1 := StrPos(cp, '\');
      while cp1 <> nil do
      begin
        cp1^ := #0;
        FormList.Add(string(cp));
        cp := cp1 + 1;
        cp1 := StrPos(cp, '\');
      end;
      FCnDockInfoStyle := isReadFileInfo;
      for i := FormList.Count - 1 downto 0 do
      begin
        FDockInfoReg.OpenKey(FRegName + '\' + FormList[i], False);
        if FDockInfoReg.ReadString('ParentName') = '' then
          CreateZoneAndAddInfo(i);
      end;
      FCnDockInfoStyle := isNone;
    end;
  finally
    FDockInfoReg.CloseKey;
    FormList.Free;
  end;
end;

destructor TCnDockInfoTree.Destroy;
begin
  FDataStream.Free;
  inherited Destroy;
end;

procedure TCnDockInfoTree.ReadInfoFromIni;
begin
  CreateZoneAndAddInfoFromIni;
  {ʹ���е�ͣ�������Ϊ������}
  DoFloatAllForm;
  {���������е���Ϣ}
  Application.ProcessMessages;
  {���������}
  FCnDockInfoStyle := isReadFileInfo;
  MiddleScanTree(TopTreeZone);
  FCnDockInfoStyle := isNone;
end;

procedure TCnDockInfoTree.ReadInfoFromReg(RegName: string);
begin
  FRegName := RegName;
  CreateZoneAndAddInfoFromReg;
  {ʹ���е�ͣ�������Ϊ������}
  DoFloatAllForm;
  {���������е���Ϣ}
  Application.ProcessMessages;
  {���������}
  FCnDockInfoStyle := isReadRegInfo;
  MiddleScanTree(TopTreeZone);
  FCnDockInfoStyle := isNone;
end;

procedure TCnDockInfoTree.ScanTreeZone(TreeZone: TCnBaseZone);
var i: Integer;
begin
  if (FCnDockInfoStyle = isReadFileInfo) or (FCnDockInfoStyle = isReadRegInfo) then
  begin
    {���ļ��ж�ͣ����Ϣ}
    for i := 0 to TreeZone.GetChildCount - 1 do
    begin
      with TCnDockInfoZone(TreeZone.GetChildZone(i)) do
        DockControl := FindDockForm(DockFormName);
    end;
    SetDockControlInfo(TCnDockInfoZone(TreeZone));
  end else
  if FCnDockInfoStyle = isWriteFileInfo then
  begin
    {��ͣ����Ϣд���ļ���}
    if TreeZone <> TopTreeZone then
    begin
      with TCnDockInfoZone(TreeZone), FDockInfoIni do
      begin
        WriteString(DockFormName,
          'ParentName', ParentName);
        WriteInteger(DockFormName,
          'DockLeft', DockRect.Left);
        WriteInteger(DockFormName,
          'DockTop', DockRect.Top);
        WriteInteger(DockFormName,
          'DockRight', DockRect.Right);
        WriteInteger(DockFormName,
          'DockBottom', DockRect.Bottom);
        WriteString(DockFormName,
          'LastDockSiteName', LastDockSiteName);
        WriteInteger(DockFormName,
          'UnDockLeft', UnDockLeft);
        WriteInteger(DockFormName,
          'UnDockTop', UnDockTop);
        WriteInteger(DockFormName,
          'LRDockWidth', LRDockWidth);
        WriteInteger(DockFormName,
          'TBDockHeight', TBDockHeight);
        WriteInteger(DockFormName,
          'UnDockWidth', UnDockWidth);
        WriteInteger(DockFormName,
          'UnDockHeight', UnDockHeight);
        WriteInteger(DockFormName,
          'VSPaneWidth', VSPaneWidth);
        WriteBool(DockFormName,
          'Visible', Visible);
        WriteInteger(DockFormName,
          'BorderStyle', Integer(BorderStyle));
        WriteInteger(DockFormName,
          'WindowState', Integer(WindowState));
        WriteInteger(DockFormName,
          'FormStyle', Integer(FormStyle));
        WriteInteger(DockFormName,
          'DockFormStyle', Integer(DockFormStyle));
        WriteBool(DockFormName,
          'CanDocked', CanDocked);
        WriteBool(DockFormName,
          'EachOtherDocked', EachOtherDocked);
        WriteBool(DockFormName,
          'LeftDocked', LeftDocked);
        WriteBool(DockFormName,
          'TopDocked', TopDocked);
        WriteBool(DockFormName,
          'RightDocked', RightDocked);
        WriteBool(DockFormName,
          'BottomDocked', BottomDocked);
        WriteString(DockFormName,
          'DockClientData', DockClientData);
      end;
    end;
  end else if FCnDockInfoStyle = isWriteRegInfo then
  begin
    if TreeZone <> TopTreeZone then
    begin
      with TCnDockInfoZone(TreeZone), FDockInfoReg do
      begin
        OpenKey(FRegName, True);
        WriteString('FormNames', ReadString('FormNames') + DockFormName + '\');
        OpenKey(FRegName + '\' + DockFormName, True);
        WriteString('ParentName', ParentName);
        WriteInteger('DockLeft', DockRect.Left);
        WriteInteger('DockTop', DockRect.Top);
        WriteInteger('DockRight', DockRect.Right);
        WriteInteger('DockBottom', DockRect.Bottom);
        WriteString('LastDockSiteName', LastDockSiteName);
        WriteInteger('UnDockLeft', UnDockLeft);
        WriteInteger('UnDockTop', UnDockTop);
        WriteInteger('LRDockWidth', LRDockWidth);
        WriteInteger('TBDockHeight', TBDockHeight);
        WriteInteger('UnDockWidth', UnDockWidth);
        WriteInteger('UnDockHeight', UnDockHeight);
        WriteInteger('VSPaneWidth', VSPaneWidth);
        WriteBool('Visible', Visible);
        WriteInteger('BorderStyle', Integer(BorderStyle));
        WriteInteger('FormStyle', Integer(FormStyle));
        WriteInteger('WindowState', Integer(WindowState));
        WriteInteger('DockFormStyle', Integer(DockFormStyle));
        WriteBool('CanDocked', CanDocked);
        WriteBool('EachOtherDocked', EachOtherDocked);
        WriteBool('LeftDocked', LeftDocked);
        WriteBool('TopDocked', TopDocked);
        WriteBool('RightDocked', RightDocked);
        WriteBool('BottomDocked', BottomDocked);
        WriteString('DockClientData', DockClientData);
        CloseKey;
      end;
    end;
  end;
  inherited ScanTreeZone(TreeZone);
end;

{��Ӧ�ó����в�������ΪFormName�Ĵ���}
function TCnDockInfoTree.FindDockForm(FormName: string): TCustomForm;
begin
  if Pos(gs_CnDockInfoSplitter, FormName) > 0 then Result := nil
  else
  begin
    Result := Cn_FindDockFormWithName(FormName);
  end;
end;

{��������ΪControlName��WinControl}
(*function TCnDockInfoTree.FindDockHost(ControlName: string): TWinControl;
var Index: Word;
  DockServer: TCnDockServer;
begin
  Result := FindDockForm(ControlName);
  if Result = nil then
  begin
    Index := Pos(gs_CnDockInfoSplitter, ControlName);
    if Index = 0 then Exit;
    Result := FindDockForm(Copy(ControlName, 1, Index - 1));
    if Result <> nil then
    begin
      DockServer := FindDockServer(Result);
      if DockServer <> nil then
        with DockServer do
        begin
          if Pos('TopDockPanel', ControlName) > Index then
            Result := TopDockPanel
          else if Pos('LeftDockPanel', ControlName) > Index then
            Result := LeftDockPanel
          else if Pos('BottomDockPanel', ControlName) > Index then
            Result := BottomDockPanel
          else if Pos('RightDockPanel', ControlName) > Index then
            Result := RightDockPanel;
          if (Result <> nil) and (Pos('PopupPanel', ControlName) > 20) then
            Result := TCnVSNETDockPanel(Result).VSChannel.VSPopupPanel;
        end;
    end;
  end;
end;*)

{����ƽ�̻��ҳ����}
function TCnDockInfoTree.CreateHostControl(ATreeZone: TCnDockInfoZone): TWinControl;
var Form: TForm;
  DockClient: TCnDockClient;
begin
  Result := nil;
  case ATreeZone.DockFormStyle of
  dsConjoin:
    begin
      Form := TCnConjoinDockHostForm.Create(Application);
      DockClient := FindDockClient(TCnDockInfoZone(ATreeZone.ChildZone).DockControl);
      Result := DockClient.CreateConjoinPanelClass(Form).Parent;
    end;
  dsTab:
    begin
      Form := TCnTabDockHostForm.Create(Application);
      DockClient := FindDockClient(TCnDockInfoZone(ATreeZone.ChildZone).DockControl);
      Result := DockClient.CreateTabDockClass(Form).Parent;
    end;
  end;
  if Result <> nil then
  begin
    Result.Name := ATreeZone.DockFormName;
  end;
end;

procedure TCnDockInfoTree.SetDockControlInfo(ATreeZone: TCnDockInfoZone);
var
  ADockBaseControl: TCnDockBaseControl;
  Host: TWinControl;
begin
  with ATreeZone do
  begin
    if DockFormName = '' then Exit;
    Host := FindDockHost(DockFormName);
    if (Host = nil) and (ATreeZone.GetChildControlCount > 1)  then
      Host := CreateHostControl(ATreeZone);
    if (Host <> nil) and (DockClientData <> '') then
    begin
      {���TMemoryStream}
      FDataStream.Clear;
      {���ַ���ת��������ʽ}
      Cn_StringToStreamData(FDataStream, DockClientData);
      {��ʼ��}
      FDataStream.Position := 0;
      if Host is TCnTabDockHostForm then
      begin
        {����Ƿ�ҳ���壬�͵�������LoadFromStream����}
        with TCnTabDockHostForm(Host).PageControl do
        begin
          DisableAlign;
          try LoadFromStream(FDataStream);
          finally EnableAlign; end;
        end;
      end else if Host is TCnConjoinDockHostForm then
      begin
        {�����ƽ�̴��壬�͵�������DockManageer��LoadFromStream����}
        with TCnConjoinDockHostForm(Host).Panel do
        begin
          DisableAlign;
          try DockManager.LoadFromStream(FDataStream);
          finally EnableAlign; end;
        end;
      end else if Host is TCnDockPanel then
      begin
        {�����ͣ�����������͵�������DockManageer��LoadFromStream����}
        with TCnDockPanel(Host) do
        begin
          DisableAlign;
          try DockManager.LoadFromStream(FDataStream);
          finally EnableAlign; end;
        end;
      end;
    end;
    if Host <> nil then
    begin
      SetDockInfoFromNodeToControl(Host);
      ADockBaseControl := FindDockBaseControl(Host);
      if ADockBaseControl <> nil then
        SetDockInfoFromNodeToDockControl(ADockBaseControl);
    end;
  end;
end;

procedure TCnDockInfoTree.WriteInfoToIni;
var Sections: TStringList;
  i: Integer;
begin
  Sections := TStringList.Create;
  try
    FDockInfoIni.ReadSections(Sections);
    {���Ȱ�INI�ļ����}
    for i := 0 to Sections.Count - 1 do
      FDockInfoIni.EraseSection(Sections[i]);
  finally
    Sections.Free;
  end;
  {���������}
  FCnDockInfoStyle := isWriteFileInfo;
  MiddleScanTree(TopTreeZone);
  FCnDockInfoStyle := isNone;
end;

procedure TCnDockInfoTree.WriteInfoToReg(RegName: string);
begin
  try
    {ɾ��·����ΪRegName��ע�����}
    if FDockInfoReg.OpenKey(RegName, False) then
      FDockInfoReg.DeleteKey(RegName);
    {�ٴδ�����}
    FDockInfoReg.CreateKey(RegName);
    FDockInfoReg.CloseKey;
    FRegName := RegName;
    {���������}
    FCnDockInfoStyle := isWriteRegInfo;
    MiddleScanTree(TopTreeZone);
    FCnDockInfoStyle := isNone;
  finally
    FDockInfoReg.CloseKey;
  end;
end;

end.
