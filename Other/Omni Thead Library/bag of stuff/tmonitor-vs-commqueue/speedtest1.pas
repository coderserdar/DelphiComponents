unit speedtest1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  StdCtrls,
  SyncObjs,
  DSiWin32,
  GpStuff,
  OtlCommon,
  OtlSync,
  OtlTask,
  OtlContainers,
  OtlCollections,
  OtlParallel,
  SimpleThreadedQueueSem,
  SimpleThreadedQueueLL,
  SimpleThreadedQueueNoWait,
  Cromis.AnyValue,
  Cromis.Threading;

const
  // configurable
  CNumReaders = 8;
  CNumWriters = 8;
  CNumTestRepeats = 10; // must be > 1
  CNumMessagesPerThread = 100000 ;

  // non-configurable
  CNumTests = 6;
  CNumSubtests = 3;

type
  TForm28 = class(TForm)
    btnTest: TButton;
    lbLog: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  strict private
    FMinThrough: array [1..CNumTests, 1..CNumSubTests, 1..CNumReaders, 1..CNumWriters] of integer;
    FThroughput: array [1..CNumTests, 1..CNumSubTests, 1..CNumReaders, 1..CNumWriters] of integer;
  strict protected
    procedure RunBench(numWriters, numReaders: integer; var msgs_per_sec: integer;
      readerCode, writerCode, allWritersDoneCode: TProc);
    procedure TestBlockingCollection(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer);
    procedure TestOmniQueue(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer);
    procedure TestSimpleThreadedQueueLL(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer);
    procedure TestSimpleThreadedQueueNoWait(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer);
    procedure TestSimpleThreadedQueueSem(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer);
    procedure TestThreadSafeQueue(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer);
  end;

var
  Form28: TForm28;

implementation

uses
  ActiveX;

{$R *.dfm}

type
  TTestRunnerProc = procedure(numWriters, numReaders, subtest: integer; var msgs_per_sec: integer) of object;

const
  CTestName: array [1..CNumTests] of string = ('TOmniQueue', 'TSimpleThreadedQueueSem',
    'TSimpleThreadedQueueLL', 'TSimpleThreadedQueueNoWait', 'TThreadSafeQueue',
    'TBlockingCollection');
  CTestTag: array [1..CNumTests] of string = ('TOQ', 'TSTQS', 'TSTQLL', 'TSTQNW', 'TTSQ', 'TBC');
  CSubTestName: array [1..CNumSubTests] of string = ('integer', 'string-1', 'string-2');

procedure TForm28.FormCreate(Sender: TObject);
begin
  GParallelPool.MaxExecuting := CNumReaders + CNumWriters;
end;

procedure TForm28.btnTestClick(Sender: TObject);
var
  f           : textfile;
  iRepeat     : integer;
  msgs_per_sec: integer;
  readers     : integer;
  subtest     : integer;
  test        : integer;
  writers     : integer;
begin
  btnTest.Enabled := false;
  btnTest.Update;
  lbLog.ItemIndex := lbLog.Items.Add(Format('Started @ %s', [FormatDateTime('ttt', Now)]));
  lbLog.Update;
  try
    for test := 1 to 1 {CNumTests} do
      for subtest := 1 to CNumSubTests do begin
        lbLog.ItemIndex := lbLog.Items.Add(CTestName[test] + ' - ' + CSubTestName[subtest]);
        Caption := lbLog.Items[lbLog.ItemIndex];
        for readers := 1 to CNumReaders do
          for writers := 1 to CNumWriters do begin
            for iRepeat := 1 to CNumTestRepeats do begin
              case test of
                1: TestOmniQueue(writers, readers, subtest, msgs_per_sec);
                2: TestSimpleThreadedQueueSem(writers, readers, subtest, msgs_per_sec);
                3: TestSimpleThreadedQueueLL(writers, readers, subtest, msgs_per_sec);
                4: TestSimpleThreadedQueueNoWait(writers, readers, subtest, msgs_per_sec);
                5: TestThreadSafeQueue(writers, readers, subtest, msgs_per_sec);
                6: TestBlockingCollection(writers, readers, subtest, msgs_per_sec);
              end;
              FThroughput[test, subtest, readers, writers] := FThroughput[test, subtest, readers, writers] + msgs_per_sec;
              if (iRepeat = 1) or
                 (msgs_per_sec < FMinThrough[test, subtest, readers, writers])
              then
                FMinThrough[test, subtest, readers, writers] := msgs_per_sec;
              Application.ProcessMessages;
            end;
            FThroughput[test, subtest, readers, writers] :=
              Round((FThroughput[test, subtest, readers, writers] - FMinThrough[test, subtest, readers, writers]) / (CNumTestRepeats - 1));
          end;
      end;
    lbLog.ItemIndex :=
      lbLog.Items.Add(Format('All done @ %s', [FormatDateTime('ttt', Now)]));
    Caption := 'All done';
    AssignFile(f, 'speedtest.csv');
    Rewrite(f);
    for test := 1 to CNumTests do
      for subtest := 1 to CNumSubTests do begin
        Writeln(f, CTestName[test], ' - ', CSubTestName[subtest]);
        for writers := 1 to CNumWriters do
          Write(f, ',', writers);
        Writeln(f);
        for readers := 1 to CNumReaders do begin
          Write(f, readers, ' ', CTestTag[test]);
          for writers := 1 to CNumWriters do
            Write(f, ',', FThroughput[test, subtest, readers, writers]);
          Writeln(f);
        end;
      end;
    CloseFile(f);
    lbLog.Items.SaveToFile('speedtest.log');
  finally btnTest.Enabled := true; end;
end;

procedure TForm28.RunBench(numWriters, numReaders: integer; var msgs_per_sec: integer;
  readerCode, writerCode, allWritersDoneCode: TProc);
var
  countDoneReaders   : IOmniResourceCount;
  countDoneWriters   : IOmniResourceCount;
  countStartedWorkers: IOmniResourceCount;
  firstWriter        : TGp8AlignedInt64;
  i                  : integer;
  lastReader         : TGp8AlignedInt64;
  startReading       : TDSiEventHandle;
  startWriting       : TDSiEventHandle;
  testCompleted      : TDSiEventHandle;
begin
  startWriting := 0;
  startReading := 0;
  testCompleted := 0;
  countDoneReaders := nil;
  countStartedWorkers := nil;
  try
    startWriting := CreateEvent(nil, true, false, nil);
    startReading := CreateEvent(nil, true, false, nil);
    testCompleted := CreateEvent(nil, true, false, nil);
    countDoneReaders := CreateResourceCount(numReaders);
    countStartedWorkers := CreateResourceCount(numReaders + numWriters);

    for i := 1 to numReaders do
      Parallel.Async(
        procedure
        begin
          countStartedWorkers.Allocate;
          WaitForSingleObject(startReading, INFINITE);
          readerCode();
          lastReader.Value := DSiTimeGetTime64;
          countDoneReaders.Allocate;
          WaitForSingleObject(testCompleted, INFINITE);
        end);

    countDoneWriters := CreateResourceCount(numWriters);
    firstWriter.Value := 0;
    for i := 1 to numWriters do
      Parallel.Async(
        procedure
        begin
          countStartedWorkers.Allocate;
          WaitForSingleObject(startWriting, INFINITE);
          firstWriter.CAS(0, DSiTimeGetTime64);
          writerCode();
          countDoneWriters.Allocate;
          WaitForSingleObject(testCompleted, INFINITE);
        end);

    WaitForSingleObject(countStartedWorkers.Handle, INFINITE);
    SetEvent(startReading);
    SetEvent(startWriting);
    WaitForSingleObject(countDoneWriters.Handle, INFINITE);
    allWritersDoneCode();
    WaitForSingleObject(countDoneReaders.Handle, INFINITE);
    SetEvent(testCompleted);
  finally
    CloseHandle(startWriting);
    CloseHandle(testCompleted);
  end;

  msgs_per_sec := Trunc(CNumMessagesPerThread * numWriters / (lastReader.Value - firstWriter.Value));
  lbLog.ItemIndex :=
    lbLog.Items.Add(Format('%d writer(s), %d reader(s), %d messages in %d ms, %d messages/ms',
      [numWriters, numReaders, CNumMessagesPerThread * numWriters,
       lastReader.Value - firstWriter.Value, msgs_per_sec]));
  lbLog.Update;
end;

procedure TForm28.TestBlockingCollection(numWriters, numReaders, subtest: integer; var
  msgs_per_sec: integer);
var
  queue: TOmniBlockingCollection;
begin
  queue := TOmniBlockingCollection.Create;
  try
    RunBench(numWriters, numReaders, msgs_per_sec,
      procedure
      var
        value: TOmniValue;
      begin
        while queue.Take(value) do
            ;
      end,

      procedure
      var
        i   : integer;
        guid: TGUID;
        s   : string;
      begin
        CoCreateGuid(guid);
        s := GUIDToString(guid);
        for i := 1 to CNumMessagesPerThread do
          if subtest = 1 then
            queue.Add(i)
          else begin
            if subtest = 3 then begin
              CoCreateGuid(guid);
              s := GUIDToString(guid);
            end;
            queue.Add(s);
          end;
      end,

      procedure
      begin
        queue.CompleteAdding;
      end);

  finally FreeAndNil(queue); end;
end;

procedure TForm28.TestOmniQueue(numWriters, numReaders, subtest: integer; var
  msgs_per_sec: integer);
var
  allWritersDone: boolean;
  queue         : TOmniQueue;
begin
  queue := TOmniQueue.Create;
  try
    allWritersDone := false;

    RunBench(numWriters, numReaders, msgs_per_sec,
      procedure
      var
        value: TOmniValue;
      begin
        repeat
          while queue.TryDequeue(value) do
            ;
        until allWritersDone;
      end,

      procedure
      var
        i   : integer;
        guid: TGUID;
        s   : string;
      begin
        CoCreateGuid(guid);
        s := GUIDToString(guid);
        for i := 1 to CNumMessagesPerThread do
          if subtest = 1 then
            queue.Enqueue(i)
          else begin
            if subtest = 3 then begin
              CoCreateGuid(guid);
              s := GUIDToString(guid);
            end;
            queue.Enqueue(s);
          end;
       end,

       procedure
       begin
         allWritersDone := true;
       end);

  finally FreeAndNil(queue); end;
end;

procedure TForm28.TestSimpleThreadedQueueLL(numWriters, numReaders, subtest: integer; var
  msgs_per_sec: integer);
var
  allWritersDone: boolean;
  countReaders  : IOmniResourceCount;
  countWriters  : IOmniResourceCount;
  queue1        : SimpleThreadedQueueLL.TSimpleThreadedQueue<integer>;
  queue2        : SimpleThreadedQueueLL.TSimpleThreadedQueue<string>;
begin
  countReaders := CreateResourceCount(numReaders);
  countWriters := CreateResourceCount(numWriters);
  queue1 := SimpleThreadedQueueLL.TSimpleThreadedQueue<integer>.Create;
  try
    queue2 := SimpleThreadedQueueLL.TSimpleThreadedQueue<string>.Create;
    try
      allWritersDone := false;

      RunBench(numWriters, numReaders, msgs_per_sec,
        procedure
        var
          svalue: string;
          value : integer;
        begin
          repeat
            if subtest = 1 then begin
              while queue1.Dequeue(value, 10) = wrSignaled do
                ;
            end
            else begin
              while queue2.Dequeue(svalue, 10) = wrSignaled do
                ;
            end;
          until allWritersDone;
        end,

        procedure
        var
          i   : integer;
          guid: TGUID;
          s   : string;
        begin
          CoCreateGuid(guid);
          s := GUIDToString(guid);
          for i := 1 to CNumMessagesPerThread do
            if subtest = 1 then
              queue1.Enqueue(i)
            else begin
              if subtest = 3 then begin
                CoCreateGuid(guid);
                s := GUIDToString(guid);
              end;
              queue2.Enqueue(s);
            end;
        end,

        procedure
        begin
          allWritersDone := true;
        end);

    finally FreeAndNil(queue2); end;
  finally FreeAndNil(queue1); end;
end;

procedure TForm28.TestSimpleThreadedQueueNoWait(numWriters, numReaders, subtest: integer;
  var msgs_per_sec: integer);
var
  allWritersDone: boolean;
  countReaders  : IOmniResourceCount;
  countWriters  : IOmniResourceCount;
  queue1        : SimpleThreadedQueueNoWait.TSimpleThreadedQueue<integer>;
  queue2        : SimpleThreadedQueueNoWait.TSimpleThreadedQueue<string>;
begin
  countReaders := CreateResourceCount(numReaders);
  countWriters := CreateResourceCount(numWriters);
  queue1 := SimpleThreadedQueueNoWait.TSimpleThreadedQueue<integer>.Create;
  try
    queue2 := SimpleThreadedQueueNoWait.TSimpleThreadedQueue<string>.Create;
    try
      allWritersDone := false;

      RunBench(numWriters, numReaders, msgs_per_sec,
        procedure
        var
          svalue: string;
          value : integer;
        begin
          repeat
            if subtest = 1 then begin
              while queue1.Dequeue(value) do
                ;
            end
            else begin
              while queue2.Dequeue(svalue) do
                ;
            end;
          until allWritersDone;
        end,

        procedure
        var
          i   : integer;
          guid: TGUID;
          s   : string;
        begin
          CoCreateGuid(guid);
          s := GUIDToString(guid);
          for i := 1 to CNumMessagesPerThread do
            if subtest = 1 then
              queue1.Enqueue(i)
            else begin
              if subtest = 3 then begin
                CoCreateGuid(guid);
                s := GUIDToString(guid);
              end;
              queue2.Enqueue(s);
            end;
        end,

        procedure
        begin
          allWritersDone := true;
        end);

    finally FreeAndNil(queue2); end;
  finally FreeAndNil(queue1); end;
end;

procedure TForm28.TestSimpleThreadedQueueSem(numWriters, numReaders, subtest: integer;
  var msgs_per_sec: integer);
var
  allWritersDone: boolean;
  countReaders  : IOmniResourceCount;
  countWriters  : IOmniResourceCount;
  queue1        : SimpleThreadedQueueSem.TSimpleThreadedQueue<integer>;
  queue2        : SimpleThreadedQueueSem.TSimpleThreadedQueue<string>;
begin
  countReaders := CreateResourceCount(numReaders);
  countWriters := CreateResourceCount(numWriters);
  queue1 := SimpleThreadedQueueSem.TSimpleThreadedQueue<integer>.Create;
  try
    queue2 := SimpleThreadedQueueSem.TSimpleThreadedQueue<string>.Create;
    try
      allWritersDone := false;

      RunBench(numWriters, numReaders, msgs_per_sec,
        procedure
        var
          svalue: string;
          value : integer;
        begin
          repeat
            if subtest = 1 then begin
              while queue1.Dequeue(value, 10) = wrSignaled do
                ;
            end
            else begin
              while queue2.Dequeue(svalue, 10) = wrSignaled do
                ;
            end;
          until allWritersDone;
        end,

        procedure
        var
          i   : integer;
          guid: TGUID;
          s   : string;
        begin
          CoCreateGuid(guid);
          s := GUIDToString(guid);
          for i := 1 to CNumMessagesPerThread do
            if subtest = 1 then
              queue1.Enqueue(i)
            else begin
              if subtest = 3 then begin
                CoCreateGuid(guid);
                s := GUIDToString(guid);
              end;
              queue2.Enqueue(s);
            end;
        end,

        procedure
        begin
          allWritersDone := true;
        end);

    finally FreeAndNil(queue2); end;
  finally FreeAndNil(queue1); end;
end;

procedure TForm28.TestThreadSafeQueue(numWriters, numReaders, subtest: integer; var
  msgs_per_sec: integer);
var
  allWritersDone: boolean;
  countReaders  : IOmniResourceCount;
  countWriters  : IOmniResourceCount;
  queue         : TThreadSafeQueue;
begin
  countReaders := CreateResourceCount(numReaders);
  countWriters := CreateResourceCount(numWriters);
  queue := TThreadSafeQueue.Create(50000);
  try
    allWritersDone := false;

    RunBench(numWriters, numReaders, msgs_per_sec,
      procedure
      var
        value: TOmniValue;
      begin
        repeat
          while queue.Dequeue(value) do
            ;
        until allWritersDone;
      end,

      procedure
      var
        i   : integer;
        guid: TGUID;
        s   : string;
      begin
        CoCreateGuid(guid);
        s := GUIDToString(guid);
        for i := 1 to CNumMessagesPerThread do
          if subtest = 1 then
            queue.Enqueue(i)
          else begin
            if subtest = 3 then begin
              CoCreateGuid(guid);
              s := GUIDToString(guid);
            end;
            queue.Enqueue(s);
          end;
      end,

      procedure
      begin
        allWritersDone := true;
      end);

  finally FreeAndNil(queue); end;
end;

end.
