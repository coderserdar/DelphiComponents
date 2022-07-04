Как создать пакет компонентов для Delphi/C++Builder:
 1. Создайте новый пакет (File -> New -> Package).
 2. Сохраните его под именем SXSkin_XX_R.bpk (например, SXSkin_D5_R или SXSkin_CB5_R).
 3. Перейдите на страницу свойст проекта и установите Usage options в Designtime and Runtime.
 4. Добавьте все pas-файлы в этот пакет, не включая следующие:
     SXSkinIcons.dcr
     SXSkinNotebookReg.pas
     SXSkinReg_D.pas
     SXStrLEdit.pas
     SXSkinEditors.pas
 5. Скомпилируйте этот пакет.
 6. Создайте новый пакет.
 7. Сохраните его под именем SXSkin_XX_D.bpk.
 8. Перейдите на страницу свойств проекта и установите Usage options в Designtime only.
 9. Добавьте следующие файлы в этот пакет:
     SXSkinIcons.dcr
     SXSkinNotebookReg.pas
     SXSkinReg_D.pas
     SXStrLEdit.pas
     SXSkinEditors.pas
10. Выберите Project -> View Source и добавьте SXSkin_XX_R в список requires.
11. Установите данный пакет.

P.S. При невозможности добавления файла SXSkinIcons.dcr добавьте следующую
     строку в файл SXSkin_XX_R.bpk:
     {$R '..\SXSkinIcons.dcr'}
     (Если пакет сохранен в папке SXSkinComponents\Packages).