unit CnTreeTestUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Controls, FMX.Dialogs, FMX.Edit, FMX.Forms, FMX.Graphics, FMX.StdCtrls, FMX.TreeView, FMX.Types,
  CnTree, FMX.Controls.Presentation, FMX.Layouts;

type
  TCnTreeTestForm = class(TForm)
    tvData: TTreeView;
    TTreeViewItem0: TTreeViewItem;
    TTreeViewItem1: TTreeViewItem;
    TTreeViewItem2: TTreeViewItem;
    TTreeViewItem3: TTreeViewItem;
    TTreeViewItem4: TTreeViewItem;
    TTreeViewItem5: TTreeViewItem;
    TTreeViewItem6: TTreeViewItem;
    TTreeViewItem7: TTreeViewItem;
    TTreeViewItem8: TTreeViewItem;
    TTreeViewItem9: TTreeViewItem;
    TTreeViewItem10: TTreeViewItem;
    grpTree: TGroupBox;
    btnLoadFromTreeView: TButton;
    btnSaveToTreeView: TButton;
    btnDepthFirstTravel: TButton;
    btnWidthFirstTravel: TButton;
    btnTreeHeight: TButton;
    grpBTree: TGroupBox;
    btnBLoad: TButton;
    btnBSave: TButton;
    btnPreOrderTravel: TButton;
    btnInOrderTravel: TButton;
    btnPostOrderTravel: TButton;
    btnIsFull: TButton;
    btnIsComplete: TButton;
    btnIsBalance: TButton;
    btnBTreeHeight: TButton;
    grpTrieTree: TGroupBox;
    btnSaveTrie: TButton;
    btnGenerateTrie: TButton;
    btnShowTrieHeight: TButton;
    btnSearch: TButton;
    edtSearch: TEdit;
    chkAnsi: TCheckBox;
    chkCase: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadFromTreeViewClick(Sender: TObject);
    procedure btnSaveToTreeViewClick(Sender: TObject);
    procedure btnDepthFirstTravelClick(Sender: TObject);
    procedure btnWidthFirstTravelClick(Sender: TObject);
    procedure btnTreeHeightClick(Sender: TObject);
    procedure btnBLoadClick(Sender: TObject);
    procedure btnBSaveClick(Sender: TObject);
    procedure btnPreOrderTravelClick(Sender: TObject);
    procedure btnInOrderTravelClick(Sender: TObject);
    procedure btnPostOrderTravelClick(Sender: TObject);
    procedure btnIsFullClick(Sender: TObject);
    procedure btnIsCompleteClick(Sender: TObject);
    procedure btnIsBalanceClick(Sender: TObject);
    procedure btnBTreeHeightClick(Sender: TObject);
    procedure btnSaveTrieClick(Sender: TObject);
    procedure btnGenerateTrieClick(Sender: TObject);
    procedure btnShowTrieHeightClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    FTree: TCnTree;
    FBinaryTree: TCnBinaryTree;
    FTrieTree: TCnTrieTree;
    FTravalResult: string;
    procedure TreeWidthFirstTrav(Sender: TObject);
    procedure TreeDepthFirstTrav(Sender: TObject);
    procedure TreePreOrderTrav(Sender: TObject);
    procedure TreeInOrderTrav(Sender: TObject);
    procedure TreePostOrderTrav(Sender: TObject);
  public

  end;

var
  CnTreeTestForm: TCnTreeTestForm;

implementation

{$R *.fmx}

const
  // ����ƴ������������ȫ��������ƴ���ο�����������ʾ�ַ�������
  PinYins: array[0..340] of string = ( // ���� v ����
    'a',
    'o',
    'e',
    'i',
    'u',
    'v',
    'ai',
    'ei',
    'ui',
    'ao',
    'ou',
    'iu',
    'ie',
    've',
    'er',
    'an',
    'en',
    'in',
    'un',
    'ang',
    'eng',
    'ing',
    'ong',
    'b',
    'ba',
    'bo',
    'bai',
    'bei',
    'bao',
    'ban',
    'ben',
    'bang',
    'beng',
    'bi',
    'bie',
    'biao',
    'bian',
    'bin',
    'bing',
    'p',
    'pa',
    'po',
    'pai',
    'pao',
    'pou',
    'pan',
    'pen',
    'pang',
    'peng',
    'pi',
    'pie',
    'piao',
    'pian',
    'pin',
    'ping',
    'm',
    'ma',
    'mo',
    'me',
    'mai',
    'mao',
    'mou',
    'man',
    'men',
    'mang',
    'meng',
    'mi',
    'mie',
    'miao',
    'miu',
    'mian',
    'min',
    'ming',
    'f',
    'fa',
    'fo',
    'fei',
    'fou',
    'fan',
    'fen',
    'fang',
    'feng',
    'd',
    'da',
    'de',
    'dai',
    'dei',
    'dao',
    'dou',
    'dan',
    'dang',
    'deng',
    'di',
    'die',
    'diao',
    'diu',
    'dian',
    'ding',
    'duan',
    'duang',
    't',
    'ta',
    'te',
    'tai',
    'tao',
    'tou',
    'tan',
    'tang',
    'teng',
    'ti',
    'tie',
    'tiao',
    'tian',
    'ting',
    'tuan',
    'tuang',
    'n',
    'na',
    'nai',
    'nei',
    'nao',
    'no',
    'nen',
    'nang',
    'neng',
    'ni',
    'nie',
    'niao',
    'niu',
    'nian',
    'nin',
    'niang',
    'ning',
    'nuan',
    'nuang',
    'l',
    'la',
    'le',
    'lai',
    'lei',
    'lao',
    'lou',
    'lan',
    'lang',
    'leng',
    'li',
    'lia',
    'lie',
    'liao',
    'liu',
    'lian',
    'lin',
    'liang',
    'ling',
    'luan',
    'luang',
    'g',
    'ga',
    'ge',
    'gai',
    'gei',
    'gao',
    'gou',
    'gan',
    'gen',
    'gang',
    'geng',
    'guan',
    'guang',
    'k',
    'ka',
    'ke',
    'kai',
    'kou',
    'kan',
    'ken',
    'kang',
    'keng',
    'kuan',
    'kuang',
    'h',
    'ha',
    'he',
    'hai',
    'hei',
    'hao',
    'hou',
    'hen',
    'hang',
    'heng',
    'huan',
    'huang',
    'j',
    'ji',
    'jia',
    'jie',
    'jiao',
    'jiu',
    'jian',
    'jin',
    'jiang',
    'jing',
    'jiong',
    'juan',
    'juang',
    'q',
    'qi',
    'qia',
    'qie',
    'qiao',
    'qiu',
    'qian',
    'qin',
    'qiang',
    'qing',
    'qiong',
    'quan',
    'quang',
    'x',
    'xi',
    'xia',
    'xie',
    'xiao',
    'xiu',
    'xian',
    'xin',
    'xiang',
    'xing',
    'xiong',
    'xuan',
    'xuang',
    'zh',
    'zha',
    'zhe',
    'zhi',
    'zhai',
    'zhao',
    'zhou',
    'zhan',
    'zhen',
    'zhang',
    'zheng',
    'zhuan',
    'zhuang',
    'ch',
    'cha',
    'che',
    'chi',
    'chai',
    'chou',
    'chan',
    'chen',
    'chang',
    'cheng',
    'chuan',
    'chuang',
    'sh',
    'sha',
    'she',
    'shi',
    'shai',
    'shao',
    'shou',
    'shan',
    'shen',
    'shang',
    'sheng',
    'shuan',
    'shuang',
    'r',
    're',
    'ri',
    'rao',
    'rou',
    'ran',
    'ren',
    'rang',
    'reng',
    'ruan',
    'ruang',
    'z',
    'za',
    'ze',
    'zi',
    'zai',
    'zao',
    'zou',
    'zang',
    'zeng',
    'zuan',
    'zuang',
    'c',
    'ca',
    'ce',
    'ci',
    'cai',
    'cao',
    'cou',
    'can',
    'cen',
    'cang',
    'ceng',
    'cuan',
    'cuang',
    's',
    'sa',
    'se',
    'si',
    'sai',
    'sao',
    'sou',
    'san',
    'sen',
    'sang',
    'seng',
    'suan',
    'suang',
    'y',
    'ya',
    'yao',
    'you',
    'yan',
    'yang',
    'yu',
    'ye',
    'yue',
    'yuan',
    'yi',
    'yin',
    'yun',
    'ying',
    'w',
    'wa',
    'wo',
    'wai',
    'wei',
    'wan',
    'wen',
    'wang',
    'weng',
    'wu'
  );

procedure TCnTreeTestForm.FormCreate(Sender: TObject);
begin
  tvData.ExpandAll;
  FTree := TCnTree.Create;
  FTree.OnDepthFirstTravelLeaf := TreeDepthFirstTrav;
  FTree.OnWidthFirstTravelLeaf := TreeWidthFirstTrav;

  FBinaryTree := TCnBinaryTree.Create;
  FBinaryTree.OnPreOrderTravelLeaf := TreePreOrderTrav;
  FBinaryTree.OnInOrderTravelLeaf := TreeInOrderTrav;
  FBinaryTree.OnPostOrderTravelLeaf := TreePostOrderTrav;
end;

procedure TCnTreeTestForm.FormDestroy(Sender: TObject);
begin
  FTrieTree.Free;
  FBinaryTree.Free;
  FTree.Free;
end;

procedure TCnTreeTestForm.TreeDepthFirstTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreeInOrderTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnBinaryLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreePostOrderTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnBinaryLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreePreOrderTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnBinaryLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.TreeWidthFirstTrav(Sender: TObject);
begin
  FTravalResult := FTravalResult + TCnLeaf(Sender).Text + ' ';
end;

procedure TCnTreeTestForm.btnLoadFromTreeViewClick(Sender: TObject);
begin
  FTree.LoadFromTreeView(tvData);
  ShowMessage('Load OK. Count(Include Root) ' + IntToStr(FTree.Count));
end;

procedure TCnTreeTestForm.btnSaveToTreeViewClick(Sender: TObject);
begin
  if FTree.Count = 1 then
  begin
    ShowMessage('No Content. Do not Save.');
    Exit;
  end;
  FTree.SaveToTreeView(tvData);
  tvData.ExpandAll;
  ShowMessage('Save OK. Count ' + IntToStr(tvData.GlobalCount));
end;

procedure TCnTreeTestForm.btnDepthFirstTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FTree.DepthFirstTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnWidthFirstTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FTree.WidthFirstTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnTreeHeightClick(Sender: TObject);
begin
  ShowMessage('Tree Height: ' + IntToStr(FTree.Height));
end;

procedure TCnTreeTestForm.btnBLoadClick(Sender: TObject);
begin
  FBinaryTree.LoadFromTreeView(tvData);
  FBinaryTree.Root.Text := 'Root';
  ShowMessage('Load OK. Count(Include Root) ' + IntToStr(FBinaryTree.Count));
end;

procedure TCnTreeTestForm.btnBSaveClick(Sender: TObject);
begin
  if FBinaryTree.Count = 1 then
  begin
    ShowMessage('No Content. Do not Save.');
    Exit;
  end;
  FBinaryTree.SaveToTreeView(tvData);
  tvData.ExpandAll;
  ShowMessage('Save OK. Count ' + IntToStr(tvData.GlobalCount));
end;

procedure TCnTreeTestForm.btnPreOrderTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FBinaryTree.PreOrderTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnInOrderTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FBinaryTree.InOrderTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnPostOrderTravelClick(Sender: TObject);
begin
  FTravalResult := '';
  FBinaryTree.PostOrderTravel;
  ShowMessage(FTravalResult);
end;

procedure TCnTreeTestForm.btnIsFullClick(Sender: TObject);
begin
  if FBinaryTree.IsFull then
    ShowMessage('Full!')
  else
    ShowMessage('NOT Full!');
end;

procedure TCnTreeTestForm.btnIsCompleteClick(Sender: TObject);
begin
  if FBinaryTree.IsComplete then
    ShowMessage('Complete!')
  else
    ShowMessage('NOT Complete!');
end;

procedure TCnTreeTestForm.btnIsBalanceClick(Sender: TObject);
begin
  if FBinaryTree.IsBalance then
    ShowMessage('Balance!')
  else
    ShowMessage('NOT Balance!');
end;

procedure TCnTreeTestForm.btnBTreeHeightClick(Sender: TObject);
begin
  ShowMessage('Tree Height: ' + IntToStr(FBinaryTree.Height));
end;

procedure TCnTreeTestForm.btnSaveTrieClick(Sender: TObject);
begin
  if FTrieTree = nil then
    Exit;

  if FTrieTree.Count = 1 then
  begin
    ShowMessage('No Content. Do not Save.');
    Exit;
  end;
  FTrieTree.SaveToTreeView(tvData);
  tvData.ExpandAll;
  ShowMessage('Save OK. Count ' + IntToStr(tvData.GlobalCount));
end;

procedure TCnTreeTestForm.btnGenerateTrieClick(Sender: TObject);
var
  I, C, T: Integer;
  Leaf: TCnTrieLeaf;
begin
  FreeAndNil(FTrieTree);
  FTrieTree := TCnTrieTree.Create(chkCase.IsChecked, False, chkAnsi.IsChecked);

  C := 0;
  for I := Low(PinYins) to High(PinYins) do
  begin
    Leaf := FTrieTree.InsertString(PinYins[I]);
    if Leaf <> nil then
    begin
      Inc(C);
      Leaf.Data := 1;
    end;
  end;

  T := 0;
  for I := 0 to FTrieTree.Count - 1 do
    if (FTrieTree.Items[I] <> nil) and (FTrieTree.Items[I].Data = 1) then
      Inc(T);

  ShowMessage('Generate OK: ' + IntToStr(C) + ' Check OK: ' + IntToStr(T) + #13#10 +
    'TrieTree Leaf Count(Include Root): ' + IntToStr(FTrieTree.Count));
end;

procedure TCnTreeTestForm.btnShowTrieHeightClick(Sender: TObject);
begin
  if FTrieTree <> nil then
    ShowMessage('TrieTree Height: ' + IntToStr(FTrieTree.Height));
end;

procedure TCnTreeTestForm.btnSearchClick(Sender: TObject);
var
  Leaf: TCnTrieLeaf;
begin
  if FTrieTree = nil then
    Exit;

  Leaf := FTrieTree.SearchString(edtSearch.Text);
  if Leaf <> nil then
    ShowMessage('Found: ' + Leaf.Text)
  else
    ShowMessage('NOT Found.');
end;

end.
