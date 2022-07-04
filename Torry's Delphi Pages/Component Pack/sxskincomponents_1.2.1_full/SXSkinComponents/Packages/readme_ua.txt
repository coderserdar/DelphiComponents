Як створити пакет компонентів для Delphi/C++Builder:
 1. Створіть новий пакет (File -> New -> Package).
 2. Збережіть його з ім'ям SXSkin_XX_R.bpk (наприклад, SXSkin_D5_R або SXSkin_CB5_R).
 3. Перейдіть на сторінку властивостей проекту й встановіть Usage options в Designtime and Runtime.
 4. Додайте всі pas-файли в цей пакет окрім наступних:
     SXSkinIcons.dcr
     SXSkinNotebookReg.pas
     SXSkinReg_D.pas
     SXStrLEdit.pas
     SXSkinEditors.pas
 5. Скомпілюйте цей пакет.
 6. Створіть новий пакет.
 7. Збережіть його з ім'ям SXSkin_XX_D.bpk.
 8. Перейдіть на сторінку властивостей проекту й встановіть Usage options в Designtime only.
 9. Додайте наступні файли в цей пакет:
     SXSkinIcons.dcr
     SXSkinNotebookReg.pas
     SXSkinReg_D.pas
     SXStrLEdit.pas
     SXSkinEditors.pas
10. Выберіть Project -> View Source і додайте SXSkin_XX_R до списку requires.
11. Встановіть цей пакет.

P.S. Якщо неможливо додати файл SXSkinIcons.dcr, додайте наступний рядок до файлу SXSkin_XX_R.bpk:
     {$R '..\SXSkinIcons.dcr'}
     (Якщо пакет збережений в каталозі SXSkinComponents\Packages).