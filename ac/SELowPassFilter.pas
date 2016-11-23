unit SELowPassFilter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math;

const
 // Два пи
  TwoPi: Double = 2 * Pi;
type
  TMode = (mdNoneFiltering, mdLowPassFilter, mdSubtractionNoise);
type
  TEnumDegree = (edSmall, edMedium, edLarge);
  TDigits = array of Double;
  TInteger = array of integer;
  TNotifyEventStep = procedure (Sender: TObject; Percent: double) of object;
  TLowPassFilter = class(TComponent)
  private
   // Режим работы фильтра
    FMode: TMode;
   // Длина входной выборки
    FSampleCount: Integer;
   // Количество спектральных составляющих
    FSpectrumCount: Integer;
   // Количество элементов гистограммы
    FHistogramCount: Integer;
   // Количество основных гармоник
    FHostHarmonicCount: Integer;
   // Полоса фильтра
    FBandWidth: double;
   // Признак устранения выбросов
    FOvershoot: boolean;
   // Масштаб разрешения в частотной области
    FFrequencyResolution: Integer;
   // Степень подавления выбросов
    FSuppressionDegree: TEnumDegree;
   // Степень вычитания шумов
    FSubstractionNoiseDegree: TEnumDegree;
   // Событие - начало обработки
    FBeforeExecute: TNotifyEvent;
   // Событие - обработка завершена
    FAfterExecute: TNotifyEvent;
   // Событие - устранение выбросов завершено
    FOnDeleteOvershoot: TNotifyEvent;
   // Событие - очередные 5% выполнены
    FOnStepExecute: TNotifyEventStep;
   // Флаг - требование прекратить обработку
    FBreakExecute: boolean;
   // Сумма спектральных составляющих без постоянной составляющей
    SumSpectrum: Double;
   // Сумма квадратов спектральных составляющих без постоянной составляющей
    SquareSumSpectrum: Double;
   // Уровень шумовых составляющих
    dblThreshold: double;
   // Показатель в которую надо возвести 2, чтобы получить число спектральных составляющих
    NExp: Integer;
   // Массивы для хранения входных данных, выходных данных, входного и выходного спектров
    SQRe, SQIm, SpRe,SpIm,SpMod: TDigits;
    SQReO, SQImO, SpReO,SpImO,SpModO: TDigits;
   // Массив индексов отсортированных по убыванию спектральных составляющих
    intIndex: TInteger;
   // Гистограмма
    intHistogram: TInteger;
   // Массив основных гармоник
    dblHostHarmonic: TDigits;
   // Процедура установки свойства "Полоса фильтра"
    procedure SetBandWidth(Value: double);
   // Процедура установки свойчтва "Масштаб разрешения  по частоте"
    procedure SetFrequencyResolution(Value: integer);
   // Функция индексированного свойства "Элемент входных данных"
    function GetInputDataItem(Index: Integer) : double;
   // Функция индексированного свойства "Элемент выходных данных"
    function GetOutputDataItem(Index: Integer) : double;
   // Функция индексированного свойства "Элемент входного спектра"
    function GetInputSpectrumItem(Index: Integer) : double;
   // Функция индексированного свойства "Элемент выходного спектра"
    function GetOutputSpectrumItem(Index: Integer) : double;
   // Функция индексированного свойства "Элемент гистограммы"
    function GetHistogramItem(Index: Integer) : integer;
   // Функция индексированного свойства "Элемент массива основных гармоник"
    function GetHostHarmonicItem(Index: Integer): double;
   // Функция вычисляет номер спектральной составляющей, соответствующей полосе фильтра
    function BandNumber: integer;
   // Функция вычисляет количество спектральных составляющих
    function CalcSpectrumCount: Integer;
   // Процедура удаления выбросов во входной последовательности
    procedure DeleteOverchoos;
   // Прямое быстрое преобразование Фурье
    procedure BPF;
   // Обратное быстрое преобразование Фурье
    procedure BackBPF;
   // Преобразование входного спектра в выходной
    procedure LPFiltr;
   // Процедура построения индекса для массива (сортировка по убыванию)
    procedure BuildIndex(dblArray: TDigits; intIndex: TInteger; SizeArray: Integer; SendEvent: Boolean);
   // Функция вычисляет порог отбора спектральных составляющих
    function CalcThreshold(dblArray: TDigits; intIndex: TInteger) : double;
   //  Построить список основных гармоник
    procedure BuildHostHarmonicList;
   // Вычисляет период гармоники по ее номеру
    function CalcPeriodHarmonic(NumberHarmonic: Integer): double;
  public
    { Public declarations }
   // Конструктор
    constructor Create(AOwner: TComponent); override;
   // Деструктор
    destructor Destroy; override;
   // Метод - обнулить данные, хранящиеся в фильтре
    procedure ClearArray;
   // Метод - добавить элемент входных данных
    procedure AddInputDataItem(Value: double);
   // Метод - стартовать обработку
    procedure Execute;
   // Метод - прервать обработку
    procedure BreakExecute;
   // Индексированное свойство "Элемент входных данных"
    property InputDataItem[index: Integer]: double read GetInputDataItem;
   // Индексированное свойство "Элемент выходных данных"
    property OutputDataItem[index: Integer]: double read GetOutputDataItem;
   // Индексированное свойство "Элемент входного спектра"
    property InputSpectrumItem[index: Integer]: double read GetInputSpectrumItem;
   // Индексированное свойство "Элемент выходного спектра"
    property OutputSpectrumItem[index: Integer]: double read GetOutputSpectrumItem;
    // Индексированное свойство "Элемент гистограммы"
    property HistogramItem[index: Integer]: Integer read GetHistogramItem;
    // Основные гармоники
    property HostHarmonicItem[index: Integer]: double read GetHostHarmonicItem;
  published
    { Published declarations }
   // Свойство "Режим работы"
    property Mode: TMode read FMode write FMode;
   // Свойство "Количество основных гармоник"
    property HostHarmonicCount: Integer read FHostHarmonicCount;
   // Свойство "Полоса фильтра"
    property BandWidth: double read FBandWidth write SetBandWidth;
   // Свойство " Масштаб разрешения по частоте"
    property FrequencyResolution: Integer read FFrequencyResolution write SetfrequencyResolution;
   // Свойство "Степень подавления выбросов"
    property SuppressionDegree: TEnumDegree read FSuppressionDegree write FSuppressionDegree;
   // Свойство "Степень вычитания шумов"
    property SubstractionNoiseDegree: TEnumDegree read FSubstractionNoiseDegree write FSubstractionNoiseDegree;
   // Свойство "Устранит выбросы"
    property Overshoot: boolean read FOvershoot write FOvershoot;
   // Свойство "Длина входной выборки"
    property SampleCount: integer read FSampleCount;
   // Свойство "Количество спектральных составляющих"
    property SpectrumCount: integer read FSpectrumCount;
   // Свойство "Количество точек в гистограмме"
    property HistogramCount: integer read FHistogramCount;
   // Свойство "Начало обработки"
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
   // Свойство "Обработка завершена"
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
   // Свойство "Устранение выбросов завершено"
    property OnDeleteOvershoot: TNotifyEvent read FOnDeleteOvershoot write FOnDeleteOvershoot;
   // Свойство "Выполнены очередные 5% обработки"
    property OnStepExecute: TNotifyEventStep read FOnStepExecute write FOnStepExecute;
  end;


implementation

// Конструктор
constructor TLowPassFilter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMode := mdSubtractionNoise;
  FOvershoot := true;
  FFrequencyResolution := 2;
  FBandWidth := 0.1;
  FSuppressionDegree := edMedium;
  FSubstractionNoiseDegree := edMedium;
end;
// Деструктор
destructor TLowPassFilter.Destroy;
begin
  SQRe := nil;
  inherited Destroy;
end;
// Метод - обнулить все данные фильтра
procedure TLowPassFilter.ClearArray;
begin
   SQRe := nil;
   SQIm := nil;
   SpRe := nil;
   SpIm := nil;
   SpMod := nil;
   SQReO := nil;
   SQImO := nil;
   SpReO := nil;
   SpImO := nil;
   SpModO := nil;
   FSampleCount := 0;
   FSpectrumCount :=0;
end;

/////////////////////////////////////////////
// Метод - добавить элемент входных данных //
/////////////////////////////////////////////
procedure TLowPassFilter.AddInputDataItem(Value: double);
begin
 FSampleCount := FSampleCount + 1;
 SetLength(SQRe, FSampleCount);
 SQRe[FSampleCount - 1] := Value;
end;

///////////////////////////////////////////////////////////////////
// Индексированное свойство - возвратить элемент выходных данных //
///////////////////////////////////////////////////////////////////
function TLowPassFilter.GetOutputDataItem(Index: integer): double;
begin
 if (Index < FSampleCount) and (Index >= 0) then
   Result := SQReO[Index]
 else
   Result := 0;
end;

//////////////////////////////////////////////////////////////////
// Индексированное свойство - возвратить элемент входных данных //
//////////////////////////////////////////////////////////////////
function TLowPassFilter.GetInputDataItem(Index: integer): double;
begin
 if (Index < FSampleCount) and (Index >=0) then
   Result := SQRe[Index]
 else
   Result := 0;
end;

////////////////////////////////////////////////////////////////////
// Индексированное свойство - возвратить элемент входного спектра //
////////////////////////////////////////////////////////////////////
function TLowPassFilter.GetInputSpectrumItem(Index: integer): double;
begin
  if (Index < FSpectrumCount) and (Index>=0) then
    Result := SpMod[Index]
  else
    Result := 0;
end;

/////////////////////////////////////////////////////////////////////
// Индексированное свойство - возвратить элемент выходного спектра //
/////////////////////////////////////////////////////////////////////
function TLowPassFilter.GetOutputSpectrumItem(Index: Integer): double;
begin
  if (Index < FSpectrumCount) and (Index>=0) then
    Result := SpModO[Index]
  else
    Result := 0;
end;

///////////////////////////////////////////////////////////////
// Индексированное свойство - возвратить элемент гистограммы //
///////////////////////////////////////////////////////////////
function TLowPassFilter.GetHistogramItem(Index: Integer): Integer;
begin
  if (Index < FHistogramCount) and (Index >= 0) then
    Result := intHistogram[Index]
  else
    Result := 0;
end;

///////////////////////////////////////////////////////////////
// Индексированное свойство - возвратить элемент гистограммы //
///////////////////////////////////////////////////////////////
function TLowPassFilter.GetHostHarmonicItem(Index: Integer): double;
begin
  if (Index < FHostHarmonicCount) and (Index >=0) then
    Result := dblHostHarmonic[Index]
  else
    Result:= 0;
end;

//////////////////////////////////
// Метод - выполнить обработку  //
//////////////////////////////////
procedure TLowPassFilter.Execute;
var
  i: Integer;
  dblSteadyComponent: double;
begin
  FBreakExecute := false;
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self);
  FSpectrumCount := CalcSpectrumCount;
  if FSampleCount > 0 then
  begin
    // Убрать аномальные выбросы
    if Overshoot then
      DeleteOverchoos;
    // Убрать постоянную составляющую
    dblSteadyComponent := 0;
    for i := 0 to FSampleCount - 1 do
      dblSteadyComponent := dblSteadyComponent + SQRe[i];
    dblSteadyComponent := dblSteadyComponent / FSampleCount;
    for i := 0 to FSampleCount -1 do
      SQRe[i] := SQRe[i] - dblSteadyComponent;

    // Прямое преобразование Фурье
    if not FBreakExecute  then BPF;
    // Преобразование спектра
    if not FBreakExecute then LPFiltr;
    // Построить список основных гармоник
    if not FBreakExecute then  BuildHostHarmonicList;
    // Обратное преобразование Фурье
    if not FBreakExecute then BackBPF;
    // Восстановить постоянную составляющую
    for i := 0 to FSampleCount -1 do
    begin
        SQReO[i] := SQReO[i] + dblSteadyComponent;
        SQRe[i] := SQRe[i] + dblSteadyComponent;
    end;
  end;
  if Assigned(FAfterExecute) then
    FAfterExecute(Self);
end;

//////////////////////////////////
// Метод - прервать обработку  //
//////////////////////////////////
procedure TLowPassFilter.BreakExecute;
begin
  FBreakExecute := true;
end;

//////////////////////////////////////////////////////
// Свойство - установить полосу пропускания (0 - 1) //
//////////////////////////////////////////////////////
procedure TLowPassFilter.SetBandWidth(Value: double);
begin
  if Value > 1 then
    FBandWidth := 1
  else
    if Value < 0 then
      FBandWidth := 0
    else
      FBandWidth := Value;
end;

//////////////////////////////////////////////////////////
// Свойство - установить масштаб разрешения по частоте. //
//////////////////////////////////////////////////////////
procedure TLowPassFilter.SetFrequencyResolution(Value: integer);
begin
  if (Value >= 1) and (Value <=10) then
    FFrequencyResolution := Value;
end;

////////////////////////////////////////////////////////////////////////////////
// По заданной полосе определяет максимальный номер пропускаемой составляющей //
////////////////////////////////////////////////////////////////////////////////
function TLowPassFilter.BandNumber:Integer;
begin
  BandNumber := Round(FSpectrumCount / 2 * BandWidth);
end;

/////////////////////////////////
// Получение выходного спектра //
/////////////////////////////////
procedure TLowPassFilter.LPFiltr;
var
  n, i: integer;
begin
  // Построить индекс по убыванию для составляющих входного спектра
  intIndex := nil;
  SetLength(intIndex,FSpectrumCount);
  BuildIndex(SpMod, intIndex, FSpectrumCount, true);
  if  FBreakExecute then exit;
  // Режим фильтра нижних частот
  if (Mode = mdLowPassFilter) or (Mode = mdNoneFiltering) then
  begin
    if Mode = mdLowPassFilter then
      n := BandNumber()
    else n := FSpectrumCount div 2;
    SpReO[0] := SpRe[0];
    SpImO[0] := SpIm[0];
    SpModO[0] := SpMod[0];
    for i := 1 to FSpectrumCount div 2 do
      if i > n then
      begin
        SpReO[i] := 0;
        SpImO[i] := 0;
        SpModO[i] := 0;
        SpReO[FSpectrumCount - i] := 0;
        SpImO[FSpectrumCount - i] := 0;
        SpModO[FSpectrumCount - i] := 0;
      end
      else
      begin
        SpReO[i] := SpRe[i];
        SpImO[i] := SpIm[i];
        SpModO[i] := SpMod[i];
        SpReO[FSpectrumCount - i] := SpRe[FSpectrumCount - i];
        SpImO[FSpectrumCount - i] := SpIm[FSpectrumCount-i];
        SpModO[FSpectrumCount-i] := SpMod[FSpectrumCount-i];
      end;
  end
  // Режим вычитания шума
  else if Mode = mdSubtractionNoise then
       begin
         dblThreshold := CalcThreshold(SpMod, intIndex);
         // Отобрать составляющие, превышающие порог
         for i := 0 to FSpectrumCount -1 do
           if SpMod[i] > dblThreshold then
             begin
               SpReO[i] := SpRe[i];
               SpImO[i] := SpIm[i];
               SpModO[i] := SpMod[i];
             end;
       end;
end;

////////////////////////////////////
// Устранение аномальных выбросов //
////////////////////////////////////
procedure TLowPassFilter.DeleteOverchoos;
var
  i:Integer;
  xPred, xS, xD, xKrit:Double;
  xSupportArray: TDigits;
  xIndex: TInteger;
begin
  xSupportArray := nil;
  SetLength(xSupportArray, FSampleCount - 1);
  // Продифференцировать входной процесс
  for i := 0 to FSampleCount - 2 do
    xSupportArray[i] := abs(SQRe[i+1] - SQRe[i]);
  // Построить индекс по убыванию для скоростей
  xIndex := nil;
  SetLength(xIndex,FSampleCount - 1 );
  BuildIndex(xSupportArray, xIndex, FSampleCount - 1, false);
  // Медиана скоростей
  xS := xSupportArray[xIndex[FSampleCount div 2]];
  // Массив отклонений скоростей от медиаы
  for i := 0 to FSampleCount - 2 do
    xSupportArray[i] := abs(xSupportArray[i] - xS);
  // Построить индекс по убыванию для отклонений
  BuildIndex(xSupportArray, xIndex, FSampleCount - 2, false);
  // Медиана отклонений скоростей
  xD := xSupportArray[xIndex[FSampleCount div 2]];
  // Степень подавления выбросов
  if FSuppressionDegree = edMedium then
    xKrit :=xS + xD * 3
  else if FSuppressionDegree = edLarge then
         xKrit :=xS + xD * 2
       else
         xKrit :=xS + xD * 4;
  xPred := SQRe[0];
  for i := 0 to FSampleCount - 1 do
  begin
    if abs(SQRe[i] - xPred) >= xKrit then
      if (SQRe[i] - xPred)>0 then
        SQRe[i] := xPred + xKrit
      else
        SQRe[i] := xPred - xKrit;
    xPred:=SQRe[i];
  end;
  if Assigned(FOnDeleteOvershoot) then
    FOnDeleteOvershoot(Self);
end;

///////////////////////////////////////////////
// Вычисляет количество составляющих спектра //
///////////////////////////////////////////////
function TLowPassFilter.CalcSpectrumCount: Integer;
var
  i, n :Integer;
begin
  n := 1;
  i := 0;
  while n < FSampleCount do
  begin
    n := n * 2;
    i := i + 1;
  end;
  Nexp := i + FFrequencyResolution - 1;
  for i := 1 to FFrequencyResolution - 1 do
    n := n * 2;

  FSpectrumCount := n;
  SetLength(SQRe,n);

  SQIm := nil;
  SQReO := nil;
  SQImO := nil;
  SpRe := nil;
  SpIm := nil;
  SpMod := nil;
  SpReO := nil;
  SpImO := nil;
  SpModO := nil;

  SetLength(SQIm,n);
  SetLength(SQReO,n);
  SetLength(SQImO,n);
  SetLength(SpRe,n);
  SetLength(SpIm,n);
  SetLength(SpMod,n);
  SetLength(SpReO,n);
  SetLength(SpImO,n);
  SetLength(SpModO,n);
  Result := n;
end;

//////////////////////////////////////////////////////////////
// Реализует алгоритм быстрого прямого преобразования Фурье //
//////////////////////////////////////////////////////////////
procedure TLowPassFilter.Bpf;
var
  xC1re,xC1im,xC2re,xC2im,xVre,xVim : double;
  i,j,k : integer;
  xMm,xLl,xJj,xKk,xNn,xNv2,xNm1: integer;
  xCounter, xNecessary: Integer;
begin
// copy
  for i := 0 to FSpectrumCount - 1 do
  begin
    SpIm[i] := 0;
    SpMod[i] := 0;
    if i < FSampleCount then
      SpRe[i]:=SQRe[i]
    else
      SpRe[i]:=0;
  end;

  //Begin
  xCounter := 0;
  xNecessary := Round(FSpectrumCount * log2(FSpectrumCount));
  xMm:=1;
  xLl:=FSpectrumCount;

  // Внешний цикл для слоев Nexp
  for k := 1 to Nexp do
  begin
    xNn:=xLl div 2;
    xJj:=xMm+1;
   // Переупорядочивание и предварительные вычисления
    i:=1;
    while i <= FSpectrumCount do
    begin
      xKk := i + xNn;
      xC1re := SpRe[i-1] + SpRe[xKk-1];
      xC1im := SpIm[i-1] + SpIm[xKk-1];
      SpRe[xKk-1] := SpRe[i-1] - SpRe[xKk-1];
      SpIm[xKk-1] := SpIm[i-1] - SpIm[xKk-1];
      SpRe[i-1] := xC1re;
      SpIm[i-1] := xC1im;
      i := i + xLl;
    end;

    if xNn <> 1 then
    begin
     // Двухточечное преобразование Фурье
      for j := 2 to xNn do
      begin
        xC2re := Cos(TwoPi * (xJj-1) / FSpectrumCount);
        xC2im := -Sin(TwoPi * (xJj-1)/ FSpectrumCount);
        xCounter := xCounter + 2;
        i := j;
        while i <= FSpectrumCount do
        begin
          xKk := i + xNn;
          xC1re := SpRe[i-1] + SpRe[xKk-1];
          xC1im := SpIm[i-1] + SpIm[xKk-1];
          xVre := (SpRe[i-1] - SpRe[xKk-1]) * xC2re - (SpIm[i-1] - SpIm[xKk-1]) * xC2im;
          xVim := (SpRe[i-1] - SpRe[xKk-1]) * xC2im + (SpIm[i-1] - SpIm[xKk-1]) * xC2re;
          SpRe[xKk-1] := xVre;
          SpIm[xKk-1] := xVim;
          SpRe[i-1] := xC1re;
          SpIm[i-1] := xC1im;
          i := i+xLl;

          xCounter := xCounter + 2;
          if (xCounter mod (xNecessary div 20) = 0) then
            begin
              Application.ProcessMessages;
              if FBreakExecute then exit;
              if Assigned(FOnStepExecute) then
                FOnStepExecute(Self, xCounter / xNecessary * 33);
            end;
        end;

        xJj := xJj + xMm;
      end;

      xLl := xNn;
      xMm := xMm * 2;
    end;

  end;

  // Восстановление нужной последовательности
  xNv2 := FSpectrumCount div 2;
  xNm1 := FSpectrumCount - 1;
  j := 1;

  for i := 1 to xNm1 do
  begin
    if i < j then
    begin
      xC1re := SpRe[j-1];
      xC1im := SpIm[j-1];
      SpRe[j-1] := SpRe[i-1];
      SpIm[j-1] := SpIm[i-1];
      SpRe[i-1] := xC1re;
      SpIm[i-1] := xC1im;
    end;
    k := xNv2;
    while k < j do
    begin
      j := j - k;
      k := k div 2;
    end;
    j := j + k;

  end;

  // Вычисление модуля
  SumSpectrum := 0;
  SquareSumSpectrum :=0;
  for i := 0 to FSpectrumCount - 1 do
  begin
    SpMod[i] := Sqrt(SpRe[i] * SpRe[i] + SpIm[i] * SpIm[i]);
    if i<>0 then
    begin
      SumSpectrum := SumSpectrum + SpMod[i];
      SquareSumSpectrum := SquareSumSpectrum + SpMod[i] * SpMod[i];
    end;
  end;
end;

////////////////////////////////////////////////////////////////
// Реализует алгоритм быстрого обратного преобразования Фурье //
////////////////////////////////////////////////////////////////
procedure TLowPassFilter.BackBpf;
var
  xC1re,xC1im,xC2re,xC2im,xVre,xVim : double;
  i,j,k : integer;
  xMm,xLl,xJj,xKk,xNn,xNv2,xNm1: integer;
  xCounter, xNecessary: Integer;
begin
// Копировать выходной спектр в выходной буфер
  for i:=0 to FSpectrumCount-1 do
  begin
    SQReO[i] := SpReO[i];
    SQImO[i] := SpImO[i];
  end ;

  //Begin
  xCounter := 0;
  xNecessary := Round(FSpectrumCount * log2(FSpectrumCount));
  xMm := 1;
  xLl := FSpectrumCount;

  // Внешний цикл для слоев Nexp
  for k := 1 to Nexp do
  begin
    xNn := xLl div 2;
    xJj := xMm + 1;
    // Переупорядочивание и предварительные вычисления
    i := 1;
    while i <= FSpectrumCount do
    begin
      xKk := i + xNn;
      xC1re := SQReO[i-1] + SQReO[xKk-1];
      xC1im := SQImO[i-1] + SQImO[xKk-1];
      SQReO[xKk-1] := SQReO[i-1] - SQReO[xKk-1];
      SQImO[xKk-1] := SQImO[i-1] - SQImO[xKk-1];
      SQReO[i-1] := xC1re;
      SQImO[i-1] := xC1im;
      i := i + xLl;
    end;

    if xNn <> 1 then
    begin
     // Двухточечное преобразование Фурье
      for j := 2 to xNn do
      begin
        xC2re := Cos(TwoPi * (xJj - 1) / FSpectrumCount);
        xC2im := Sin(TwoPi * (xJj - 1) / FSpectrumCount);
        xCounter := xCounter + 2;
        i := j;
        while i <= FSpectrumCount do
        begin
          xKk := i + xNn;
          xC1re := SQReO[i - 1] + SQReO[xKk - 1];
          xC1im := SQImO[i - 1] + SQImO[xKk - 1];
          xVre := (SQReO[i - 1] - SQReO[xKk - 1]) * xC2re - (SQImO[i - 1] - SQImO[xKk - 1]) * xC2im;
          xVim := (SQReO[i - 1] - SQReO[xKk - 1]) * xC2im + (SQImO[i - 1] - SQImO[xKk - 1]) * xC2re;
          SQReO[xKk - 1] := xVre;
          SQImO[xKk - 1] := xVim;
          SQReO[i - 1] := xC1re;
          SQImO[i - 1] := xC1im;
          i := i + xLl;

          xCounter := xCounter + 2;
          if (xCounter mod (xNecessary div 20) = 0) then
            Begin
              Application.ProcessMessages;
              if FBreakExecute then exit;
              if Assigned(FOnStepExecute) then
                FOnStepExecute(Self,66 + xCounter / xNecessary * 33);
            end;
        end;

        xJj := xJj + xMm;
      end;

      xLl := xNn;
      xMm := xMm * 2;
    end;
  end;

  // Восстановление нужной последовательности
  xNv2 := FSpectrumCount div 2;
  xNm1 := FSpectrumCount - 1;
  j := 1;

  for i := 1 to xNm1  do
  begin
    if i < j then
    begin
      xC1re := SQReO[j - 1];
      xC1im := SQImO[j - 1];
      SQReO[j - 1] := SQReO[i - 1];
      SQImO[j - 1] := SQImO[i - 1];
      SQReO[i - 1] := xC1re;
      SQImO[i - 1] := xC1im;
    end;
    k := xNv2;
    while k < j do
    begin
     j := j - k;
     k := k div 2;
    end;
    j := j + k;
  end;
  
  // Нормировать временной ряд
  SumSpectrum := 0;
  for i := 0 to FSpectrumCount-1 do
  begin
    SQReO[i] := SQReO[i] / FSpectrumCount;
    SQImO[i] := SQImO[i] / FSpectrumCount;
  end;
end;

////////////////////////////////////////////////////////////////
// Построение индекса для массива (сортировка по убыванию) //
////////////////////////////////////////////////////////////////
procedure TLowPassFilter.BuildIndex(dblArray: TDigits; intIndex: TInteger; SizeArray: Integer; SendEvent: Boolean);
var
  i, n: Integer;
  xTotalCount, xCurrentCount: Integer;
  xN1, xN2, xN1z, xN2z, xZ, xOrd: Integer; // Индексы слияния и их границы
  xTemporary: TInteger; // Массив слияния индексов
begin
  if SizeArray <=0 then exit;

//  xCurrentCount := 0;
  xTotalCount := 0;

  if SendEvent  then
  begin
    xCurrentCount :=0;
    xTotalCount := Round(SizeArray * log2(SizeArray));
  end;

  SetLength(xTemporary, SizeArray);
  for i := 0 to SizeArray - 1 do
    intIndex[i] := i;
  xZ := 1; // Начальный размер зоны слияния
  While (xZ < SizeArray) do
  begin
    xN1 := 0;
    While (xN1 < SizeArray) do
    begin
      xN1z :=xN1 + xZ;
      if xN1z > SizeArray then xN1z := SizeArray;
      xN2 := xN1z;
      xN2z := xN2 +xZ;
      if xN2z > SizeArray then xN2z := SizeArray;

      n := xN1;
      While ((xN1 < xN1z) or (xN2 < xN2z)) do  // Выбор из зон
      begin

        
        if SendEvent then
        begin
          Inc(xCurrentCount);
          if (xCurrentCount mod (xTotalCount div 20) = 0) then
            begin
              Application.ProcessMessages;
              if FBreakExecute then exit;
              if Assigned(FOnStepExecute) then
                FOnStepExecute(Self,33 + xCurrentCount / xTotalCount * 33);
            end;
        end;

        if xN2 >= xN2z then
          xOrd := 1
        else if xN1 >= xN1z then
               xOrd := 2
             else if dblArray[intIndex[xN1]] > dblArray[intIndex[xN2]] then
                    xOrd := 1
                  else
                    xOrd := 2;
        if xOrd = 1 then
        begin
          xTemporary[n] := intIndex[xN1];
          Inc(xN1);
        end
        else
        begin
          xTemporary[n] := intIndex[xN2];
          Inc(xN2);
        end;
        Inc(n);
      end;
      xN1 := xN2;
    end;
    for i := 0 to SizeArray - 1 do
      intIndex[i] := xTemporary[i];
    xZ := xZ * 2;
  end;
  xTemporary := nil;
end;

////////////////////////////////////////////
// Вычисление уровня шумовых составляющих //
////////////////////////////////////////////
function TLowPassFilter.CalcThreshold(dblArray: TDigits; intIndex: TInteger) : double;
var
  i, n, k : integer;
  xWeightingFactor: double;
  xHistogramInterval, xMaxValue, xMinValue : double;
begin
  // Определить число точек в гистограмме по оценочной формуле
  n := Round(1 + 3.2 * log10(FSpectrumCount));
  if n < 20 then n := 20;
  FHistogramCount := n;
  // Определить весовой коэффициент
  if FSubstractionNoiseDegree = edMedium then
    xWeightingFactor := 0.06
  else if FSubstractionNoiseDegree = edSmall then
         xWeightingFactor := 0.1
       else
         xWeightingFactor := 0.03;

  intHistogram := nil;
  SetLength(intHistogram, n);
  xMaxValue := dblArray[intIndex[0]];
  xMinValue := dblArray[intIndex[High(intIndex)]];
  xHistogramInterval := (xMaxValue - xMinValue) / n;
  // Построить гистограмму
  for i := 0 to FSpectrumCount - 1 do
  begin
    k := Trunc((dblArray[i] -xMinValue) / xHistogramInterval);
    if k >=n then
      k := n-1;
    intHistogram[k] := intHistogram[k]+1 ;
  end;
  // Найти максимум распределения
  k := 0;
  for i := 1 to n - 1 do
    if intHistogram[i] > intHistogram[k] then k := i;
  // Найти порог
  i := k;
  repeat
    i := i + 1
  until (intHistogram[i] < xWeightingFactor * intHistogram[k]) or (i = n - 1);
  // Вернуть результат
  if i < n - 1 then
    Result := xMinValue + xHistogramInterval * (i + 1)
  else
    Result := xMaxValue;
end;

////////////////////////////////////////
// Построить список основных гармоник //
////////////////////////////////////////
procedure TLowPassFilter.BuildHostHarmonicList;
var
  i, n: integer;
  dblTempArray: TDigits;
  intTempNumberHarmonic: TInteger;
  intTempIndex: TInteger;
begin
  dblHostHarmonic := nil;
  dblTempArray := nil;
  intTempIndex := nil;
  intTempNumberHarmonic := nil;
  n := 0;
  // Проверить первую гармонику
  if (SpModO[1] > SpModO[2]) and (SpModO[1] >= SpModO[0]) and((SpModO[1] > dblThreshold) or (Mode = mdLowPassFilter)) then
  begin
    Inc(n);
    SetLength(dblTempArray, n);
    SetLength(intTempNumberHarmonic,n);
    dblTempArray[n - 1] := SpModO[1];
    intTempNumberHarmonic[n - 1] := 1;
  end;
 // Остальные
  for i := 2 to FSpectrumCount div 2 do
    begin
      if (SpModO[i] > SpModO[i + 1]) and (SpModO[i] >= SpModO[i - 1]) and ((SpModO[i] > dblThreshold)  or (Mode = mdLowPassFilter) or (Mode =mdNoneFiltering)) then
      begin
        Inc(n);
        SetLength(dblTempArray, n );
        SetLength(intTempNumberHarmonic,n);
        dblTempArray[n - 1] := SpModO[i];
        intTempNumberHarmonic[n - 1] := i;
      end;
    end;
  // Сортировать
  intTempIndex := nil;
  SetLength(intTempIndex,n);
  BuildIndex(dblTempArray, intTempIndex, n, false);
  // Построить список основных гармоник
  SetLength(dblHostHarmonic,n);
  for i := 0 to n - 1 do
    dblHostHarmonic[i] := CalcPeriodHarmonic(intTempNumberHarmonic[intTempIndex[i]]);
  FHostHarmonicCount := n;
end;

/////////////////////////////////////////////////////////////////////////////
// Вычислить период гармоники в количестве временных отсчетов по ее номеру //
/////////////////////////////////////////////////////////////////////////////
function TLowPassFilter.CalcPeriodHarmonic(NumberHarmonic: Integer): double;
begin
  Result :=  (FSampleCount / NumberHarmonic) * (FSpectrumCount / FSampleCount);
end;
end.
