unit langPortuguese;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Escolha "Novo" para criar ou "Abrir" para abrir um arquivo' );
  AddStr(   2, 'OK' );
  AddStr(   3, 'Cancelar' );
  AddStr(   4, '&Help' );
  // unit fmAboutBox
  AddStr( 500, 'Sobre' );
  AddStr( 501, 'por Morgan Martinet (C)1998' );
  AddStr( 502, 'Estes componentes são Freeware.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'Biblioteca pasZLib:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'Implementação BlowFish fornecida por Greg Carter, CRYPTOCard' );
  AddStr( 507, 'Código SFX por Oliver Buschjost' );
  AddStr( 508, 'Web site:' );
  // unit fmTiming
  AddStr( 600, 'Tempo decorrido :' );
  AddStr( 601, 'Tempo restante :' );
  // unit fmMain
  AddStr( 700, 'Novo...' );
  AddStr( 701, 'Abrir...' );
  AddStr( 702, '&Adicionar...' );
  AddStr( 703, '&Extrair...' );
  AddStr( 704, 'E&xcluir...' );
  AddStr( 705, '&Interromper' );
  AddStr( 706, 'Novo Arquivo' );
  AddStr( 707, 'Abrir Arquivo' );
  AddStr( 708, 'Adicionar arquivos...' );
  AddStr( 709, 'Extrair arquivos...' );
  AddStr( 710, 'Excluir arquivos...' );
  AddStr( 711, 'Arquivos compactados (*.mmm)|*.mmm|Arquivos SFX (*.exe)|*.exe|Todos (*.*)|*.*' );
  AddStr( 712, 'Arquivos compactados (*.mmm)|*.mmm|Todos (*.*)|*.*' );
  AddStr( 713, 'Abrir um arquivo existente' );
  AddStr( 714, 'Criar um novo arquivo' );
  AddStr( 715, 'Abrir um segmento de um arquivo' );
  AddStr( 718, '%d arquivos(s), %s' );
  AddStr( 720, 'O arquivo "%s", já existe' );
  AddStr( 721, 'Deseja reiniciar este arquivo ?' );
  AddStr( 722, 'Deseja excluir este arquivo ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, '%d arquivo(s) selecionado(s), %s' );
  AddStr( 729, 'Indisponível' );
  AddStr( 730, 'Renomear o arquivo corrente para:' );
  AddStr( 731, 'Impossível renomear o arquivo para "%s" !' );
  AddStr( 732, 'Configuração do SFX' );
  AddStr( 733, 'Criar arquivo auto-extrator (SFX)' );
  AddStr( 734, 'Criar' );
  AddStr( 735, 'Impossível criar arquivo auto-extrator !' );
  AddStr( 736, 'Defina comentário para o arquivo' );
  AddStr( 737, 'Comentário do arquivo' );
  AddStr( 738, 'Operação em progresso. Aguarde o término ou clique em Interromper.' );
  AddStr( 739, 'Você executou um arquivo. Tente outra vez quando encerrar a execução.' );
  AddStr( 740, 'Primeiro abra ou crie um arquivo.' );
  AddStr( 741, 'Impossível encontrar associação para %s' );
  AddStr( 742, 'Nome' );
  AddStr( 743, 'Data' );
  AddStr( 744, 'Hora' );
  AddStr( 745, 'Tamanho' );
  AddStr( 746, 'Proporção' );
  AddStr( 747, 'Compactado' );
  AddStr( 748, 'Seg#' );
  AddStr( 749, 'Caminho' );
  AddStr( 750, '&Arquivo' );
  AddStr( 751, '&Ações' );
  AddStr( 752, '&Opções' );
  AddStr( 753, '&Ajuda' );
  AddStr( 754, '&Novo arquivo...' );
  AddStr( 755, '&Abrir arquivo...' );
  AddStr( 756, 'Abrir &segmento...' );
  AddStr( 757, '&Fechar arquivo' );
  AddStr( 758, '&Informação...' );
  AddStr( 759, 'Re&nomear Arquivo' );
  AddStr( 760, '&Reinicializar arquivo' );
  AddStr( 761, '&Excluir arquivo' );
  AddStr( 762, '&Encerrar' );
  AddStr( 763, '&Visualizar...' );
  AddStr( 764, '&Selecione todos' );
  AddStr( 765, '&Criar arquivo .EXE' );
  AddStr( 766, 'Definir Comentário do Arquivo...' );
  AddStr( 767, 'Configuração do &SFX...' );
  AddStr( 769, '&Sobre...' );
  AddStr( 770, 'Criar novo arquivo' );
  AddStr( 771, 'Abrir um arquivo existente' );
  AddStr( 772, 'Abrir um segmento de um Arquivo' );
  AddStr( 773, 'Fecha este arquivo' );
  AddStr( 774, 'Mostra informações sobre este arquivo' );
  AddStr( 775, 'Renomeia o arquivo corrente...' );
  AddStr( 776, 'Reinicializa conteúdo do arquivo' );
  AddStr( 777, 'Exclui arquivo' );
  AddStr( 778, 'Encerra aplicação' );
  AddStr( 781, 'Adiciona arquivos' );
  AddStr( 782, 'Extrai arquivos' );
  AddStr( 783, 'Exclui arquivos' );
  AddStr( 784, 'Visualiza arquivos' );
  AddStr( 785, 'Seleciona todos os arquivos' );
  AddStr( 786, 'Cria arquivo auto extrator' );
  AddStr( 787, 'Define um comentário para o arquivo corrente' );
  AddStr( 788, 'Muda configuração' );
  AddStr( 789, 'Muda configuração de criação de SFX' );
  AddStr( 790, 'Sobre esta aplicação' );
  AddStr( 798, '&Configuração...' );
  AddStr( 799, '%s Arquivo' );
  AddStr( 800, 'Fechando arquivo compactado...' );
  AddStr( 801, 'Seleciona &nenhum' );
  AddStr( 802, '&Inverte seleção' );
  AddStr( 803, 'Raiz' );
  AddStr( 804, 'Visão em árvore' );
  AddStr( 805, 'Icones grandes' );
  AddStr( 806, 'Icones pequenos' );
  AddStr( 807, 'Lista' );
  AddStr( 808, 'Detalhes' );
  AddStr( 809, 'Expansão completa' );
  AddStr( 810, 'Fechar tudo' );
  AddStr( 811, 'Limpando Lista de Arquivos' );
  AddStr( 812, 'Criando Lista de Arquivos' );
  AddStr( 813, 'Classificando Lista de Arquivos' );
  AddStr( 814, 'O arquivo %s não existe!' );
  AddStr( 815, 'Chec&ar integridade' );
  AddStr( 816, 'Checar integridade do arquivo corrente' );
  AddStr( 817, '&Visualizar última saída...' );
  AddStr( 818, 'Visualizar saída da última operação' );
  AddStr( 819, 'Instalar' );
  AddStr( 820, 'Extrair conteúdo e executar programa de instalação' );
  AddStr( 821, '&Fonte...' );
  AddStr( 822, '&Classificar' );
  AddStr( 823, '&Ordem Original' );
  AddStr( 824, 'Mudar fonte corrente' );
  AddStr( 825, 'Selecionar ordem de classificação' );
{*}  AddStr( 826, 'F&ilters...' );
{*}  AddStr( 827, 'Lets you define filters to select files to be added' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Adicionar de' );
  AddStr( 901, 'Nome :' );
  AddStr( 902, 'Pasta' );
  AddStr( 903, 'Examinar subpastas' );
  AddStr( 904, 'Inclir caminho atual' );
  AddStr( 905, 'Incluir pastas vazias' );
  AddStr( 906, 'Armazenamento de caminho :' );
  AddStr( 907, 'Encriptar arquivos ?' );
  AddStr( 908, 'Nível de Compressão:' );
  AddStr( 909, 'Adicionar' );
  AddStr( 910, 'Nenhum'+#13+
               'Inteiro'+#13+
               'Relativo' );
  AddStr( 911, 'Maximo (mais lento)'+#13+
               'Normal'+#13+
               'Rápido'+#13+
               'Super Rápido'+#13+
               'Nenhum' );
  AddStr( 912, 'Adicionar arquivos esquecidos' );
  AddStr( 913, 'Adicionar itens' );
  AddStr( 914, 'Filtro :' );
  AddStr( 915, 'Adicionar à pasta corrente ?' );
{*}AddStr( 916, 'Filter files ?' );
  // unit fmConfiguration
  AddStr( 1000, 'Configuração' );
  AddStr( 1001, 'Divisão por disquetes' );
  AddStr( 1002, 'Criação do Arquivo' );
  AddStr( 1003, 'Opções' );
  AddStr( 1004, 'Dividir arquivo' );
  AddStr( 1005, 'Tamanho máximo do segmento:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                'Outro (Kb):' );
  AddStr( 1007, 'Compactar arquivo' );
  AddStr( 1008, 'Encriptar arquivo' );
  AddStr( 1009, 'Arquivo Sólido' );
  AddStr( 1010, 'Somente para leitura' );
  AddStr( 1011, 'Criar arquivo SFX' );
  AddStr( 1014, 'Tamanho do Bloco' );
  AddStr( 1015, 'Espaço de Reserva' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, 'Linguagem:' );
  AddStr( 1018, 'Automatico'+#13+
                'Inglês'+#13+
                'Françês'+#13+
                'Chinês'+#13+
                'Português'+#13+
                'Alemão'+#13+
                'Italiano'+#13+
                'Russo'+#13+
                'Espanhol'+#13+
{*}             'Danish'+#13+
                'Dutch'+#13+
                'Czech'
                );
  AddStr( 1019, 'Mostrar pastas vazias' );
  AddStr( 1020, 'Mostrar visão em árvore' );
  // unit fmCreateFolder
  AddStr( 1100, 'Pasta corrente:' );
  AddStr( 1101, 'Nome:' );
  // unit fmDelete
  AddStr( 1200, 'Excluir' );
  AddStr( 1201, 'Arquivos' );
  AddStr( 1202, '&Arquivo inteiro'+#13+
                '&Arquivos selecionados'+#13+
                '&Arquivos:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'Mensagem do sistema' );
  AddStr( 1301, 'Esconder senha ?' );
  // unit fmExtract
  AddStr( 1400, 'Extrair' );
  AddStr( 1401, 'Extrair em:' );
  AddStr( 1402, 'Arquivos' );
  AddStr( 1403, 'Arquivos &Selecionados'+#13+
                '&Todos os arquivos'+#13+
                'A&rquivos:' );
  AddStr( 1404, '&Use nomes das pastas' );
  AddStr( 1405, 'Substituir arquivos existentes'+#13+
                'Ignororar arquivos existentes'+#13+
                'Atualizar arquivos recentes'+#13+
                'Pedir confirmação'+#13+
                'Restaurar apenas arquivos existentes'+#13+
                'Atualizar arquivos existentes' );
  AddStr( 1406, 'Pastas / Drives' );
  AddStr( 1407, 'Nova pasta...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'Os seguintes comandos devem ser usados nos campos "Linha de Comando" e'+#13+
                '"Caminho Default de extração" :' );
  AddStr( 1501, 'será substituído pelo diretório \Temp'+#13+
                '(geralmete ''c:\windows\temp'' ou ''c:\win95\temp'' ou ''c:\temp'')' );
  AddStr( 1502, 'será substituído pelo diretório \Windows'+#13+
                '(geralmente ''c:\windows'' ou ''c:\win95'')' );
  AddStr( 1503, 'será substituído pelo diretório \System'+#13+
                '(geralmete ''c:\windows\system'' ou ''c:\win95\system'')' );
  AddStr( 1504, 'será substituído pelo diretório ProgramFiles'+#13+
                '(geralmete ''c:\Program Files'' ou ''c:\Arquivos de Programas'''+#13+
                ' [dependendo da linguagem do Windows instalado])' );
  AddStr( 1505, 'será substituído pelo diretório para onde os arquivos'+#13+
                'foram extraídos (apenas para os campos "Linha de Comando" ou'+#13+
                '"Argumentos")' );
  AddStr( 1506, 'Exemplo:' );
  AddStr( 1507, '<PF>Compania\MyStuff' );
  // unit fmInformation
  AddStr( 1600, 'Caminho:' );
  AddStr( 1601, 'Nome:' );
  AddStr( 1602, 'File Size:' );
  AddStr( 1603, 'Arquivos:' );
  AddStr( 1604, 'Compressão:' );
  AddStr( 1605, 'Data/Hora:' );
  AddStr( 1606, 'Segmento:' );
  AddStr( 1607, 'Atributos' );
  AddStr( 1608, 'Encriptado' );
  AddStr( 1609, 'Comprimido' );
  AddStr( 1610, 'Solido' );
  AddStr( 1611, 'Somente para leitura' );
  AddStr( 1612, 'Segmento Final' );
  AddStr( 1613, 'Informação' );
  // unit fmSFXComments
  AddStr( 1700, 'Comentários' );
  AddStr( 1701, 'Comentário mostrado ao abrir o arquivo SFX' );
  AddStr( 1702, 'Comentário mostrado após a extraçào dos arquivos armazenados no arquivo SFX' );
  AddStr( 1703, 'Limpar comentários' );
  // unit fmSFXConfig
  AddStr( 1800, 'Configuração de SFX' );
  AddStr( 1801, 'Executar arquivos após extrair ?' );
  AddStr( 1802, 'Usuário escolhe arquivos a extrair ?' );
  AddStr( 1803, 'Usuário escolhe modo Substituir ?');
  AddStr( 1804, 'Título:' );
  AddStr( 1805, 'Linha de Comando:' );
  AddStr( 1806, 'Argumentos:' );
  AddStr( 1807, 'Caminho Default de extração:' );
  AddStr( 1808, 'Modo Substituir:' );
  AddStr( 1809, 'Comentários...' );
  AddStr( 1810, 'Pedir confirmação'+#13+
                'Substituir arquivos existentes'+#13+
                'Ignorar arquivos existentes'+#13+
                'Substituir apenas se mais recente'+#13+
                'Restaurar apenas arquivos existentes'+#13+
                'Extrair arquivo apenas se ja existe e é mais recente' );
  AddStr( 1811, 'Usuário tem permissão para não executar arquivo ?' );
  // unit fmTextViewer
  AddStr( 1900, 'Visualizar: %s' );
  AddStr( 1901, '&Copiar para Clipboard' );
  AddStr( 1902, '&Fonte' );
  // unit fmView
  AddStr( 2000, 'Visualizar : %s' );
  AddStr( 2001, 'Usando' );
  AddStr( 2002, 'Visualizar' );
  AddStr( 2003, 'Programa &Associado (%s)'+#13+
                'Visualizador &Interno de Texto ASCII' );
  // unit fmLastOutput
  AddStr( 2100, 'Visualizar Último Output' );
  // unit fmFilters
{*}  AddStr( 2200, 'Filters' );
{*}  AddStr( 2202, 'Add' );
{*}  AddStr( 2203, 'Edit' );
{*}  AddStr( 2204, 'Delete' );
{*}  AddStr( 2205, 'Clear all' );
{*}  AddStr( 2206, 'Kind of filter' );
{*}  AddStr( 2207, 'Require'+#13+
{*}                'Exclude' );
{*}  AddStr( 2208, 'Edit filter:' );
{*}  AddStr( 2209, 'New filter:' );
end;

end.
 