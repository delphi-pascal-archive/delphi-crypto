{******************************************************************************}
{*  DiscreteMath.int.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
type
{ массив действительных чисел }
    parray_of_double = ^array_of_double;
    array_of_double  = array [0..0] of double;
{ функция }
    pdouble_function = ^double_function;
    double_function  = function (const anArray: parray_of_double;
                                 const X: WORD) : double;
{ параметры вызова функции }
    pdouble_function_params = ^double_function_params;
    double_function_params  = packed record
        X1 : WORD;
        X2 : WORD;
        dX : WORD;
    end;

function Frequency (const aValue: Double;
                    const anArray: parray_of_double;
                    const aLength: WORD;
                    const aPrecision: Double = 0.001) : Double; overload;

function Entropy (const anArray: parray_of_double;
                  const aLength: WORD;
                  const aPrecision: Double = 0.001) : Double; overload;

function EntropyPercent (const anArray: parray_of_double;
                         const aLength: WORD;
                         const aPrecision: Double = 0.001) : Double; overload

procedure FFT (var anArray: parray_of_double;
               const aLength: WORD;
               const doInverse: Boolean = FALSE); overload;

function buffer_pool (const anArray: parray_of_double;
                      const X: WORD) : Double;

function fft_pool (const anArray: parray_of_double;
                   const X: WORD) : Double;

function entropy_pool (const anArray: parray_of_double;
                       const X: WORD) : Double;

function DrawDiscreteFunction (const aCanvas: TCanvas;
                               const aRect: TRect;
                               const aFunction: double_function;
                               const anArray: parray_of_double;
                               const aParams: double_function_params;
                               const aBackGroundColor: TColor;
                               const aBackColor: TColor;
                               const aFrontColor: TColor;
                               const drawAxes: Boolean = TRUE;
                               const drawLines: Boolean = TRUE;
                               const drawSubGraph: Boolean = FALSE;
                               const aText: String = '') : Boolean;
