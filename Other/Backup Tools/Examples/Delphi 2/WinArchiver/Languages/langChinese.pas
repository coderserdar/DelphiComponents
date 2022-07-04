unit langChinese;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, '選擇 "開新檔案" 或 "開啟舊檔" 來建立及開啟現有的壓縮檔' );
  AddStr(   2, '確定' );
  AddStr(   3, '取消' );
  AddStr(   4, '說明(&H)' );
  // unit fmAboutBox
  AddStr( 500, '關於' );
  AddStr( 501, 'by Morgan Martinet (C)1998' );
  AddStr( 502, '這些元件都是免費的.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'pasZLib 函式庫:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'BlowFish 演算法的實作由 Greg Carter, CRYPTOCard 提供' );
  AddStr( 507, '自我解壓縮程式碼由 Oliver Buschjost 提供' );
  AddStr( 508, '網站:' );
  // unit fmTiming
  AddStr( 600, '已花時間 :' );
  AddStr( 601, '剩餘時間 :' );
  // unit fmMain
  AddStr( 700, '開新檔案...' );
  AddStr( 701, '開啟舊檔...' );
  AddStr( 702, '加入(&A)...' );
  AddStr( 703, '解壓縮(&E)...' );
  AddStr( 704, '刪除(&D)...' );
  AddStr( 705, '放棄(&A)' );
  AddStr( 706, '開新檔案' );
  AddStr( 707, '開啟舊檔' );
  AddStr( 708, '加入檔案...' );
  AddStr( 709, '解開檔案...' );
  AddStr( 710, '刪除檔案...' );
  AddStr( 711, '壓縮檔 (*.mmm)|*.mmm|SFX Archives (*.exe)|*.exe|所有的檔案 (*.*)|*.*' );
  AddStr( 712, '壓縮檔 (*.mmm)|*.mmm|所有的檔案 (*.*)|*.*' );
  AddStr( 713, '開啟現有的壓縮檔' );
  AddStr( 714, '建立新的壓縮檔' );
  AddStr( 715, '開啟壓縮檔的一個分割區段' );
  AddStr( 718, '%d 個檔案, %s' );
  AddStr( 720, '檔案 "%s" 已經存在' );
  AddStr( 721, '您要重設這個壓縮檔嗎 ?' );
  AddStr( 722, '您要刪除這個壓縮檔嗎 ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, '選擇了 %d 個檔案, %s' );
  AddStr( 729, '無效' );
  AddStr( 730, '將目前的壓縮檔更名為:' );
  AddStr( 731, '無法將壓縮檔更名為 "%s" !' );
  AddStr( 732, '自解檔(SFX) 組態設定' );
  AddStr( 733, '建立一個自我解壓縮檔案' );
  AddStr( 734, '建立' );
  AddStr( 735, '無法建立自解檔 !' );
  AddStr( 736, '設定壓縮檔註解' );
  AddStr( 737, '壓縮檔註解' );
  AddStr( 738, '程序進行中, 請等待直到工作完成, 或者按[放棄]來中斷。' );
  AddStr( 739, '您已經執行了某個檔案, 請將其結束然後在試一次。' );
  AddStr( 740, '你必須先開啟一個壓縮檔。' );
  AddStr( 741, '找不到與檔案類型 %s 關聯的程式' );
  AddStr( 742, '檔名' );
  AddStr( 743, '日期' );
  AddStr( 744, '時間' );
  AddStr( 745, '長度' );
  AddStr( 746, '比例' );
  AddStr( 747, '壓縮後的長度' );
  AddStr( 748, '分割#' );
  AddStr( 749, '路徑' );
  AddStr( 750, '檔案(&F)' );
  AddStr( 751, '動作(&A)' );
  AddStr( 752, '選項(&O)' );
  AddStr( 753, '說明(&H)' );
  AddStr( 754, '開新檔案(&N)...' );
  AddStr( 755, '開啟舊檔(&O)...' );
  AddStr( 756, '開啟一個分割檔(&S)...' );
  AddStr( 757, '關閉壓縮檔(&C)' );
  AddStr( 758, '&資訊...' );
  AddStr( 759, '更改壓縮檔名稱(&N)' );
  AddStr( 760, '重設壓縮檔(&R)' );
  AddStr( 761, '刪除壓縮檔(&D)' );
  AddStr( 762, '結束(&Q)' );
  AddStr( 763, '檢視(&V)...' );
  AddStr( 764, '全選(&S)' );
  AddStr( 765, '建立可執行檔(&M)' );
  AddStr( 766, '設定壓縮檔註解...' );
  AddStr( 767, '自解檔(&SFX) 組態設定...' );
  AddStr( 769, '關於(&A)...' );
  AddStr( 770, '建立新的壓縮檔' );
  AddStr( 771, '開啟現存的壓縮檔' );
  AddStr( 772, '開啟壓縮檔的一個分割區段' );
  AddStr( 773, '關閉壓縮檔' );
  AddStr( 774, '顯示壓縮檔相關資訊' );
  AddStr( 775, '更改目前壓縮檔的名稱...' );
  AddStr( 776, '重設壓縮檔內容' );
  AddStr( 777, '刪除壓縮檔' );
  AddStr( 778, '結束程式' );
  AddStr( 781, '加入檔案' );
  AddStr( 782, '解壓縮檔案' );
  AddStr( 783, '刪除檔案' );
  AddStr( 784, '檢視檔案' );
  AddStr( 785, '選擇所有檔案' );
  AddStr( 786, '建立自解檔' );
  AddStr( 787, '為目前的壓縮檔定義註解' );
  AddStr( 788, '變更設定' );
  AddStr( 789, '變更自解檔設定' );
  AddStr( 790, '關於本程式' );
  AddStr( 798, '設定(&C)...' );
  AddStr( 799, '%s 檔案' );
  AddStr( 800, '關閉檔案...' );
  AddStr( 801, '取消選擇(&N)' );
  AddStr( 802, '反向選擇(&I)' );
  AddStr( 803, '根' );
  AddStr( 804, '樹狀檢視' );
  AddStr( 805, '大圖示' );
  AddStr( 806, '小圖示' );
  AddStr( 807, '清單' );
  AddStr( 808, '詳細資料' );
  AddStr( 809, '全部展開' );
  AddStr( 810, '全部閉合' );
  AddStr( 809, '完全展開' );
  AddStr( 810, '完全閉合' );
  AddStr( 811, '清除檔案清單' );
  AddStr( 812, '建立檔案清單' );
  AddStr( 813, '排序檔案清單' );
  AddStr( 814, '%s'#10#13'壓縮檔不存在!' );
  AddStr( 815, '檢查完整性' );
  AddStr( 816, '檢查壓縮檔的完整性' );
  AddStr( 817, '檢視上一次的輸出(&V)...' );
  AddStr( 818, '檢視上一次操作的輸出結果' );
  AddStr( 819, '安裝' );
  AddStr( 820, '解壓縮並執行安裝程式' );
  AddStr( 821, '字型(&F)...' );
  AddStr( 822, '排序(&S)' );
  AddStr( 823, '原始順序(&O)' );
  AddStr( 824, '變更目前的字型' );
  AddStr( 825, '選擇排序方式' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, '加入哪些檔案' );
  AddStr( 901, '名稱 :' );
  AddStr( 902, '資料夾' );
  AddStr( 903, '搜尋子資料夾' );
  AddStr( 904, '包含目前路徑' );
  AddStr( 905, '儲存空資料夾' );
  AddStr( 906, '路徑儲存方式 :' );
  AddStr( 907, '是否將檔案加密 ?' );
  AddStr( 908, '壓縮等級:' );
  AddStr( 909, '加入' );
  AddStr( 910, '無'+#13+
               '完整'+#13+
               '相對' );
  AddStr( 911, '最大 (最慢)'+#13+
               '一般'+#13+
               '快速'+#13+
               '超快速'+#13+
               '無' );
  AddStr( 912, '加入拖曳的檔案' );
  AddStr( 913, '加入項目' );
  AddStr( 914, '篩選 :' );
  AddStr( 915, '加到目前的資料夾 ?' );
  // unit fmConfiguration
  AddStr( 1000, '設定' );
  AddStr( 1001, '檔案分割' );
  AddStr( 1002, '壓縮檔的建立' );
  AddStr( 1003, '選項' );
  AddStr( 1004, '切割壓縮檔' );
  AddStr( 1005, '每個分割檔案最大容量:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                '其他 (Kb):' );
  AddStr( 1007, '使用壓縮' );
  AddStr( 1008, '使用加密' );
  AddStr( 1009, '強固壓縮(Solid)' );
  AddStr( 1010, '唯讀' );
  AddStr( 1011, '建立自解檔' );
  AddStr( 1014, '區塊大小' );
  AddStr( 1015, '保留空間' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, '語係:' );
  AddStr( 1018, '自動'+#13+
                '英文'+#13+
                '法文'+#13+
                '中文'+#13+
                '葡萄牙文'+#13+
                '德文'+#13+
                '義大利文'+#13+
                '蘇聯文'+#13+
                '西班牙文' );
  AddStr( 1019, '顯示空的資料夾' );
  AddStr( 1020, '顯示樹狀結構' );
  // unit fmCreateFolder
  AddStr( 1100, '目前資料夾:' );
  AddStr( 1101, '名稱:' );
  // unit fmDelete
  AddStr( 1200, '刪除' );
  AddStr( 1201, '檔案' );
  AddStr( 1202, '整個壓縮檔(&E)'+#13+
                '選擇的檔案(&S)'+#13+
                '檔案(&F):' );
  // unit fmEnterCryptKey
  AddStr( 1300, '系統訊息e' );
  AddStr( 1301, '隱藏密碼 ?' );
  // unit fmExtract
  AddStr( 1400, '解壓縮' );
  AddStr( 1401, '加壓縮至:' );
  AddStr( 1402, '檔案' );
  AddStr( 1403, '選擇的檔案(&S)'+#13+
                '所有檔案(&A)'+#13+
                '檔案(&I):' );
  AddStr( 1404, '使用資料夾名稱(&U)' );
  AddStr( 1405, '覆蓋已存在的檔案'+#13+
                '略過已存在的檔案'+#13+
                '更新較新的檔案'+#13+
                '要求確認'+#13+
                '只回存已存在的檔案'+#13+
                '更新已存在的檔案' );
  AddStr( 1406, '資料夾 / 磁碟機' );
  AddStr( 1407, '新資料夾...' );
  // unit fmHelpOnSFX
  AddStr( 1500, '以下關鍵字可能會使用於"命令列"及'+#13+
                '"內定解壓縮路徑" 欄位 :' );
  AddStr( 1501, '將會被暫存目錄所取代'+#13+
                '(通常是 ''c:\windows\temp'' 或 ''c:\win95\temp'' 或 ''c:\temp'')' );
  AddStr( 1502, '將會被 Windows 目錄所取代'+#13+
                '(通常是 ''c:\windows'' 或 ''c:\win95'')' );
  AddStr( 1503, '將會被 System 目錄所取代'+#13+
                '(通常是 ''c:\windows\system'' 或 ''c:\win95\system'')' );
  AddStr( 1504, '將會被 Program Files 目錄所取代'+#13+
                '(通常是 ''c:\Program Files'' [視 Windows 安裝的語言而定])' );
  AddStr( 1505, '將會被檔案欲解壓至的目錄所取代'+#13+
                '(只適用於 "命令列" 或 "引數" 欄位)' );
  AddStr( 1506, '範例:' );
  AddStr( 1507, '<PF>MyCompany\MyStuff' );
  // unit fmInformation
  AddStr( 1600, '路徑:' );
  AddStr( 1601, '名稱:' );
  AddStr( 1602, '檔案大小:' );
  AddStr( 1603, '檔案:' );
  AddStr( 1604, '壓縮:' );
  AddStr( 1605, '日期/時間:' );
  AddStr( 1606, '分割區段:' );
  AddStr( 1607, '屬性' );
  AddStr( 1608, '已加密' );
  AddStr( 1609, '已壓縮' );
  AddStr( 1610, '強固壓縮(Solid)' );
  AddStr( 1611, '唯讀' );
  AddStr( 1612, '最後的分割區段' );
  AddStr( 1613, '資訊' );
  // unit fmSFXcomments
  AddStr( 1700, '註解' );
  AddStr( 1701, '開啟自解檔時顯示註解' );
  AddStr( 1702, '解開自解檔之後顯示註解' );
  AddStr( 1703, '清除註解' );
  // unit fmSFXConfig
  AddStr( 1800, '自解檔設定' );
  AddStr( 1801, '解壓縮後執行檔案 ?' );
  AddStr( 1802, '使用者選擇要解壓縮的檔案 ?' );
  AddStr( 1803, '使用者選擇覆蓋模式 ?');
  AddStr( 1804, '標題:' );
  AddStr( 1805, '命令列:' );
  AddStr( 1806, '引數:' );
  AddStr( 1807, '內定解壓縮路徑:' );
  AddStr( 1808, '檔案覆蓋模式:' );
  AddStr( 1809, '註解...' );
  AddStr( 1810, '要求確認'+#13+
                '覆蓋已存在的檔案'+#13+
                '略過已存在的檔案'+#13+
                '只覆蓋較新的檔案'+#13+
                '只還原已存在的檔案'+#13+
                '只解壓縮已存在而且比較新的檔案' );
  AddStr( 1811, '允許使用者不執行這個檔案 ?' );
  // unit fmTextViewer
  AddStr( 1900, '檢視: %s' );
  AddStr( 1901, '複製剪貼簿(&C)' );
  AddStr( 1902, '字型(&F)' );
  // unit fmView
  AddStr( 2000, '檢視 : %s' );
  AddStr( 2001, '使用' );
  AddStr( 2002, '檢視' );
  AddStr( 2003, '登記關聯的程式(&A) (%s)'+#13+
                '內建的 ASCII 文字瀏覽器' );
  // unit fmLastOutput
  AddStr( 2100, '檢視上次輸出' );
end;

end.
