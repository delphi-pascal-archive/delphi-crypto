{******************************************************************************}
{*  DiscreteMath.imp.inc                                                      *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
function Frequency (const aValue: Double;
                    const anArray: parray_of_double;
                    const aLength: WORD;
                    const aPrecision: Double = 0.001) : Double;
var
    I     : WORD;
    Count : WORD;
begin
    Result := -1;
    try
        Count := 0;
        { array [2n] + i*array [2n+1] }
        I := 0;
        while ( I < 2*aLength ) do
        begin
            if ( abs (aValue - anArray^ [I]) <= aPrecision ) then
                Inc (Count);
            Inc (I,2); // real only
        end;
        Result := Count / aLength;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_DISCRETE_MATH_GET_FREQUENCY,E.Message]);
    end;
end;

function Entropy (const anArray: parray_of_double;
                  const aLength: WORD;
                  const aPrecision: Double = 0.001) : Double;
var
    I   : WORD;
    Frq : Double;
begin
    Result := 0;
    try
        { array [2n] + i*array [2n+1] }
        I := 0;
        while ( I < 2*aLength ) do
        begin
            Frq := Frequency (anArray^ [I],
                              anArray,
                              aLength,
                              aPrecision);
            if ( Frq > 0 ) then
                Result := Result + log2 (1/Frq);
            Inc (I,2); // real only
        end;
        Result := Result / aLength;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_DISCRETE_MATH_GET_ENTROPY,E.Message]);
    end;
end;

function EntropyPercent (const anArray: parray_of_double;
                         const aLength: WORD;
                         const aPrecision: Double = 0.001) : Double;
var
    I   : WORD;
    Frq : Double;
begin
    Result := 0;
    try
        { array [2n] + i*array [2n+1] }
        I := 0;
        while ( I < 2*aLength ) do
        begin
            Frq := Frequency (anArray^ [I],
                              anArray,
                              aLength,
                              aPrecision);
            if ( Frq > 0 ) then
                Result := Result + 100 * log2 (1/Frq);
            Inc (I,2); // real only
        end;
        Result := Result / ( aLength * log2 (aLength) );
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_DISCRETE_MATH_GET_ENTROPY,E.Message]);
    end;
end;

procedure FFT (var anArray: parray_of_double;
               const aLength: WORD;
               const doInverse: Boolean = FALSE);
var
    nn    : WORD;
    ii    : Integer;
    jj    : Integer;
    n     : Integer;
    m     : Integer;
    mMax  : Integer;
    i     : Integer;
    j     : Integer;
    iSign : Integer;
    iStep : Integer;
    wTemp : Double;
    wR    : Double;
    wI    : Double;
    wpR   : Double;
    wpI   : Double;
    tempR : Double;
    tempI : Double;
    theta : Double;
begin
    try
        case doInverse of
            TRUE  : iSign := 1;
            FALSE : iSign := -1;
        end;
        nn := aLength;
        n := 2*nn;
        j := 1;
        ii := 1;
        while ( ii <= nn ) do
        begin
            i := 2*ii - 1;
            if ( j > i ) then
            begin
                tempR := anArray^ [j-1];
                tempI := anArray^ [j];
                anArray^ [j-1] := anArray^ [i-1];
                anArray^ [j]   := anArray^ [i];
                anArray^ [i-1] := tempR;
                anArray^ [i]   := tempI;
            end;
            m := n div 2;
            while (  ( m >= 2) and ( j > m )  ) do
            begin
                j := j - m;
                m := m div 2;
            end;
            j := j + m;
            Inc (ii);
        end;
        mMax := 2;
        while ( n > mMax ) do
        begin
            iStep := 2*mMax;
            theta := 2 * Pi / (iSign*mMax);
            wpR := -2.0 * sqr ( sin (0.5*theta) );
            wpI := sin (theta);
            wR := 1.0;
            wI := 0.0;
            ii := 1;
            while ( ii <= mMax div 2 ) do
            begin
                m  := 2*ii - 1;
                jj := 0;
                while ( jj <= (n-m)  div iStep ) do
                begin
                    i := m + jj*iStep;
                    j := i + mMax;
                    tempR := wR * anArray^ [j-1] - wI * anArray^ [j];
                    tempI := wR * anArray^ [j]   + wI * anArray^ [j-1];
                    anArray^ [j-1] := anArray^ [i-1] - tempR;
                    anArray^ [j]   := anArray^ [i]   - tempI;
                    anArray^ [i-1] := anArray^ [i-1] + tempR;
                    anArray^ [i]   := anArray^ [i]   + tempI;
                    Inc (jj);
                end;
                wTemp := wR;
                wR := wR*wpR - wI*wpI    + wR;
                wI := wI*wpR + wTemp*wpI + wI;
                Inc (ii);
            end;
            mMax := iStep;
        end;
        if doInverse then
        begin
            i := 1;
            while ( i <= 2*nn ) do
            begin
                anArray^ [i-1] := anArray^ [i-1] / nn;
                Inc (i);
            end;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_DISCRETE_MATH_GET_FFT,E.Message]);
    end;
end;

function buffer_pool (const anArray: parray_of_double;
                      const X: WORD) : Double;
begin
    Result := anArray^ [2*x];
end;

function fft_pool (const anArray: parray_of_double;
                   const X: WORD) : Double;
begin
    Result := sqr ( anArray^ [x] );
end;

function entropy_pool (const anArray: parray_of_double;
                       const X: WORD) : Double;
begin
    Result := anArray^ [x];
end;

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
var
    x1, x2 : WORD;    { область определения функции }
    y1, y2 : Double;  { область значения функции }
    x      : WORD;    { локальное значение аргумента }
    y      : Double;  { локальное значение функции }
    dx     : WORD;    { приращение аргумента }
    x0, y0 : Integer; { начало координат }
    Left   : Integer; 
    Bottom : Integer;
    Width  : Integer;
    Height : Integer;
    ScaleX : Double;  { масштаб по оси x }
    ScaleY : Double;  { масштаб по оси y }
begin
    Result := FALSE;
    try
        x1 := aParams.X1;
        x2 := aParams.X2;
        dx := aParams.dX;
        with aCanvas do
        begin
            Brush.Color := aBackGroundColor;
            Pen.Color := aFrontColor;
            Rectangle (aRect);
            if (  ( x2 < 0 ) or ( x1 < 0 )  ) then
                Left := (aRect.Right - aRect.Left) div 2
            else
                Left := aRect.Left + 8;
            Width := (aRect.Right - aRect.Left) - 18;
            Bottom := (aRect.Bottom - aRect.Top) - 5;
            Height := (aRect.Bottom - aRect.Top) - 22;
            y1 := aFunction (anArray,x1);
            y2 := aFunction (anArray,x1);
            x := x1;
            repeat
                y := aFunction (anArray,x);                             
                if ( y < y1 ) then
                    y1 := y;
                if ( y > y2 ) then
                    y2 := y;
                x := x + dx;
            until ( x >= x2 );
            if ( abs (y2-y1) < dx ) then Exit;
            ScaleX := Width / abs (x2-x1);
            ScaleY := Height / abs (y2-y1);
            x0 := Left;
            y0 := Bottom - abs ( round (y1*ScaleY) );
            if drawAxes then
            begin
                MoveTo (Left,Bottom);
                LineTo ( Left, Bottom-Height );
                MoveTo (Left,y0);
                LineTo ( x0+Width, y0 );
            end;
            Pen.Color := aBackColor;
            x := x1;
            repeat
                y := aFunction (anArray,x);
                if ( y > dx ) then
                    Pixels [ x0 + round (x*ScaleX), y0 - round (y*ScaleY)-1 ] := aFrontColor
                else
                    Pixels [ x0 + round (x*ScaleX), y0 - round (y*ScaleY){+1} ] := aFrontColor;
                MoveTo ( x0 + round (x*ScaleX), y0 - round (y*ScaleY) );
                if not drawLines then
                    x := x + dx
                else
                begin
                    if drawSubGraph then
                    begin
                        LineTo ( x0 + round (x*ScaleX), y0 );
                        x := x + dx;
                    end
                    else
                    begin
                        x := x + dx;
                        y := aFunction (anArray,x);
                        LineTo ( x0 + round (x*ScaleX), y0 - round (y*ScaleY) );
                    end;
                end;
            until ( x >= x2 );
            if notEmpty ( Trim (aText) ) then
            begin
                Brush.Color := aBackColor;
                Pen.Color := clBlack;
                TextOut (  Left + 14 + ( Width - Font.Size * Length (aText) ) div 2,
                           (Height + Font.Size) div 2,
                           Format (' %s ',[aText])  );
            end;
            Result := TRUE;
        end;
    except on E: Exception do
        raise Exception.CreateFmt ('%s : %s',[ERR_DISCRETE_MATH_DRAW_FUNCTION,E.Message]);
    end;
end;