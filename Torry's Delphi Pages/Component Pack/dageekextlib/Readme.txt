Library 2 (REFINED BY DAGEEK)
___________________


Обновления и дополнения
_______________________

Версия 2

  * Добавлены следующие компоненты ( описания см. ниже):
      TELPropertyInspector,
      TELDiagram,
      TELDBDiagram;

  * Исправлены найденные ошибки.

Версия 2.1

  * Устранено мерцание в TELPropertyInspector, в TELDesignPanel (мерцание
      DesignControl во время изменения размеров TELDesignPanel), в TELDiagram,
      TELDBDiagram (мерцание при пересещении item'ов и изменении их размеров);
  * В TELPropertyInspector исправлена прорисовка (свойства теперь отделяются
      пунктирной линией, как в Delphi);
  * В TELDesigner исправлен баг, связанный с тем, что при изменении размеров
      выделенного компонента выделение оставалось на том же месте);
  * В TELDesigner добавлена поддержка "Drag and drop" (события OnDragOver,
      OnDragDrop, см. пример TELDesigner в подкаталоге \Demos);
  * В TELDesigner оптимизирована прорисовка сетки
  * В TELDiagram, TELDBDiagram исправлена поддержка "Drag and drop". Теперь
      перетаскивать можно Item'ы как в TListView
  * В TELDiagram, TELDBDiagram добавлены методы и свойства:
      property Data: Pointer; (TELDiagramLink)
      property Data: Pointer; (TELDiagramItem)
      function ItemRect(AIndex: Integer): TRect; (TELDiagramItems)
      property AutoItemPos; (TELCustomDiagram)
      property OnInsertItem; (TELCustomDiagram)
      property OnDeleteItem; (TELCustomDiagram)
      property OnInsertLink; (TELCustomDiagram)
      property OnDeleteLink; (TELCustomDiagram)
      function LineAtPos(APos: TPoint; AExisting: Boolean): Integer; (TELDBDiagramItem)
  * В TELDBDiagram добавлена поддержка OwnerDraw для Item'ов (свойство ItemStyle,
      события OnMeasureItemLine, OnDrawItemLine)
                      
Версия 2.2
  * REFINED BY DAGEEK

Инсталяция
__________

Для установки:
  в меню Delphi выберите File|Open;
  откройте файл ExtLib_D6.dpk (для Delphi 6) или ExtLib_D5.dpk (для Delphi 5);
  в появившемся окне нажмите кнопку Install;
  в меню Delphi выберите Tools|Invironment options;
  в появившемся окне выберите квладку Library;
  добавьте в Library path каталог библиотеки.

Описание компонентов и функций
______________________________


Модуль ELControls.

TELEvent, TELEventSender

  Компоненты позволяют обмениваться сообщениями в режиме "Один ко многим".
  К компоненту TELEventSender может быть подключено несколько компонентов
  TELevent. При вызове метода TELEventsender.SendEvent все они получают
  сообщение и генерируют событие OnEvent. Компоненты могут использоваться 
  для организации рассылки сообщений, причем код модуля, посылающего сообщения
  не зависит от количества модулей, их принимающих. См. пример TELEvent в
  подкаталоге \Demos.

TELInstanceChecker

  Объект позволяет организовать невозможность запуска второй копии приложения
  в системе, а также передать уже запущенной копии приложения какие-либо данные.
  Напимер, он может использоваться для MDI текстового редактора, который при 
  запуске (при щелчке по текстовому файлу) не запускает вторую копию редактора,
  а передает параметры запуска (имя файла) уже работающей. См. примеры CDPlayer
  и TELInstanceChecker в подкаталоге \Demos.

TELStringList

  Компонент обертка для класса TStringList, который может использоваться в 
  "design-time".

TELTrayIcon

  Компонент позволяет:
    Отображать иконку в system tray;
    Реагировать на события мыши этой иконки;
    Отображать контекстное меню;
    Скрывать кнопку приложения на панели задач;
    Использовать анимацию иконки.


Модуль ELDsgnr.

TELDesigner
  
  Компонент позволяет добавить в приложение возможности визуального
  дизайнера. По своим характеристикам он максимально приближен к 
  визульному дизайнеру Delphi. Дизайнить можно как формы, так и 
  отдельные компоненты (например TPanel). Также данный компонент можно 
  применить для разработки редактора отчетов.

  Некоторые возможности:
    отображение сетки (сетка полностью настраиваема);
    настройка возможности отображения окна Hint при изменении размеров 
      компонентов, при вставке нового компонента, при наведении курсора
      мыши на компонент;
    режим SnapToGrid;
    отображение контекстного меню;
    запирание компонентов (режимы запирания: lmNoMove, lmNoResize, lmNoDelete,
      lmNoInsertIn, lmNoCopy, lmCustom1, lmCustom2, lmCustom3, lmCustom4,
      lmCustom5, lmCustom6, lmCustom7, lmCustom8);
    операции с буфером обмена (Возможность копирования в буфер обмена всех выделенных
      компнентов, возможность вставки их с сохранением взаимозависимостей);
    выполнение следующих операций над выделенными компонентами: BringToFront, SendToBack,
      AlignToGrid, Align (несколько типов);
    готовый кнопочный интерфейс (можно переопределить, используя события).
  
  См. примеры TELDesigner и ReportDesigner из подкаталога \Demos.

TELDesignPanel

  Компонент используется совместно с компонентом TELDesigner для дизайна
  компонентов не являющихся наследниками TCustomForm. См. пример ReportDesigner 
  из подкаталога \Demos.


Модуль ELUtils.

Функции ELPackStrings и ELUnpackStrings
  
  Функции позволяют преобразовать TStrings в строку и
  обратно. См. пример StreamingStrings из подкаталога \Demos.

Функции ELSaveStringToStream и ELLoadStringFromStream

  Функции позволяют записывать и читать строки из TStream.
  См. пример StreamingStrings из подкаталога \Demos.

Функции ELPrepareMask и ELMaskCompare

  Функции позволяют организовать сравнение строки с маской.
  Маска может использовать WinCard символы "*" и "?".
  По наблюдения функции работают гораздо быстрее, чем
  стандартный класс Delphi - TMask (В некоторых случаях
  разница во времени выполнения сравнения превосходила 100 раз).

Функция ELQuickSort

  Реализация алгоритма Quick sort для произвольного типа данных.

Функции ELSearchFiles

  Поиск файлов (с большим количеством настроек).

Класс TELThreadFilesSearch

  Тоже поиск файлов, но в отдельном потоке.

Модуль ELPropInsp.

TELPropertyInspector

  Компонент является аналогом Delphi ObjectInspector

  Некоторые возможности:
    позволяет показывать и редактировать свойства подключенных к 
      нему объектов (набора объектов);
    для свойств - ссылок на компоненты (Component reference)
      имеет события, позволяющие сформировать список доступных 
      компонентов,получить ссылку по имени или имя по ссылке;
    позволяет раскрывать свойства ссылок на компоненты 
      (как в Delphi 6);
    для ограничения показываемых свойств имеется свойство
      PropKinds, а также событие OnFilterProp
    имеет встроенные редакторы свойств для стандартных типов
      Objects Pascal, а также для основных типов VCL:
        * TCaption
        * TCursor
        * TFontCharset
        * TFontName
        * TImeName
        * TFont
        * TModalResult
        * TPenStyle
        * TBrushStyle
        * TTabOrder
        * TShortCat
    в модуле так же объявлен класс TELPropEditor, являющийся
      базовым классом для создания других редакторов свойств,
      которые далее могут быть зарегистрированы в компоненте
  

Модуль ELDgrm.

TELDiagram
  
  Позволяет создавать диаграммы, имеющие Item'ы (панели) и
  связи между ними. Имеет события для прорисовки Item'а
  и связей

TELDBDiagram
  
  Аналог диаграммы базы данных MS Access

__________________________________

(c) 1999 - 2002, Balabuyev Yevgeny
E-mail: stalcer@rambler.ru











