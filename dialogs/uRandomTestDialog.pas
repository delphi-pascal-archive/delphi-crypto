unit uRandomTestDialog;
{******************************************************************************}
{*  Random Test Dialog Unit                                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, ImgList, Math,
    acPNG,
    sSkinProvider, sSkinManager,
    sPanel, sButton, sBitBtn, acAlphaImageList, sGroupBox,
    Kernel, DialogClasses,
    uProtoDialog,
    Utils, Strings, Versions, VarRecs,
    EClasses;

{$I 'DiscreteMath.int.inc'}

type
{$M+}
    TRandomTestDialog = class;
    TRandomTestContent = class;
{$M-}

{ диалоговое окно тестирования генератора случайных чисел }
{$M+}
    CRandomTestDialog = class of TRandomTestDialog;
    PRandomTestDialog = ^TRandomTestDialog;
    TRandomTestDialog = class (TProtoDialog)
    private
        f_RandomType: String;
        f_DataLength: WORD;
        f_Data: parray_of_double;
        f_EntropyData: parray_of_double;
        f_EntropyDataIndex: WORD;
    protected
        procedure OnTimer (Sender: TObject); virtual;
    protected
        procedure SetRandomType (const aValue: String); virtual;
        procedure SetDataLength (const aValue: WORD); virtual;
        function GetRandomData (anIndex: WORD) : Double; virtual;
        procedure SetRandomData (anIndex: WORD; anItem: Double); virtual;
        procedure SetEntropyDataIndex (const aValue: WORD); virtual;
        function GetEntropyData (anIndex: WORD) : Double; virtual;
        procedure SetEntropyData (anIndex: WORD; anItem: Double); virtual;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
    public
        constructor Create (anArgs: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const) : Integer; overload; virtual;
        class function Execute (anArgs: array of const) : Boolean; overload; virtual;
    public
        property RandomType: String read f_RandomType write SetRandomType;
        property DataLength: WORD read f_DataLength write SetDataLength;
        property Data [anIndex: WORD]: Double read GetRandomData write SetRandomData;
        property EntropyDataIndex: WORD read f_EntropyDataIndex write SetEntropyDataIndex;
        property EntropyData [anIndex: WORD]: Double read GetEntropyData write SetEntropyData;
    end;
{$M-}

{ содержимое }
{$M+}
    CRandomTestContent = class of TRandomTestContent;
    PRandomTestContent = ^TRandomTestContent;
    TRandomTestContent = class (TFrame)
        pnlBackground: TsPanel;
        bxBuffer: TsGroupBox;
        bxFFT: TsGroupBox;
        bxEntropy: TsGroupBox;
        tmRandom: TTimer;
        ptBuffer: TPaintBox;
        ptFFT: TPaintBox;
        ptEntropy: TPaintBox;
        procedure FrameResize(Sender: TObject);
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    end;
{$M-}

{ кнопки }
const
    btOk = 0;

resourcestring
    ERR_TRANDOMTESTDIALOG_ON_TIMER               = 'Ошибка обработки события OnTimer!';
    ERR_TRANDOMTESTDIALOG_SET_RANDOM_TYPE        = 'Ошибка записи типа генератора случайных чисел!';
    ERR_TRANDOMTESTDIALOG_SET_DATA_LENGTH        = 'Ошибка записи размера пула данных!';
    ERR_TRANDOMTESTDIALOG_GET_RANDOM_DATA        = 'Ошибка чтения элемента буфера случайных чисел!';
    ERR_TRANDOMTESTDIALOG_SET_RANDOM_DATA        = 'Ошибка записи элемента буфера случайных чисел!';
    ERR_TRANDOMTESTDIALOG_SET_ENTROPY_DATA_INDEX = 'Ошибка записи временного индекса энтропии!';
    ERR_TRANDOMTESTDIALOG_GET_ENTROPY_DATA       = 'Ошибка чтения элемента буфера энтропии!';
    ERR_TRANDOMTESTDIALOG_SET_ENTROPY_DATA       = 'Ошибка записи элемента буфера энтропии!';

{$I 'DiscreteMath.err.inc'}

implementation

{$R *.dfm}

uses
    Crypto;

{$I 'DiscreteMath.imp.inc'}

constructor TRandomTestDialog.Create (anArgs: array of const);
begin
    try
        inherited Create (anArgs,[ _(['OK',btnOk,mrOk]) ],TRandomTestContent);
        minWidth := 480;
        minHeight := 360;
        Width := 512;
        Height := 384;
        //BorderIcons := [biSystemMenu,biMaximize];
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{F454BA87-7DE2-4F2B-821C-EEFDA4F75278}']);
    end;
end;

destructor TRandomTestDialog.Destroy;
begin
    try
        Dispose (f_EntropyData);
        f_EntropyData := NIL;
        Dispose (f_Data);
        f_Data := NIL;
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{A9559F78-8D0D-43B7-9BF1-3B12DBD5702B}']);
    end;
end;

procedure TRandomTestDialog.SetRandomType (const aValue: String);
begin
    try
        f_RandomType := aValue;
    except on E: Exception do
        _raise (['SetRandomType',ERR_TRANDOMTESTDIALOG_SET_RANDOM_TYPE,E],
                ['{1AA42175-C04B-4BBD-8B51-F526A681D932}']);
    end;
end;

procedure TRandomTestDialog.SetDataLength (const aValue: WORD);
begin
    try
        if ( aValue > 0 ) then
        begin
            f_DataLength := aValue;
            Dispose (f_Data);
            f_Data := NIL;
            { array [2n] + i*array [2n+1] }
            f_Data := AllocMem ( 2 * f_DataLength * SizeOf (Double) );
            { буфер энтропии }
            Dispose (f_EntropyData);
            f_EntropyData := NIL;
            f_EntropyData := AllocMem ( f_DataLength * SizeOf (Double) );
            f_EntropyDataIndex := 0;
        end
        else
            SetDataLength (1024);
    except on E: Exception do
        _raise (['SetDataLength',ERR_TRANDOMTESTDIALOG_SET_DATA_LENGTH,E],
                ['{1C675FDD-49BD-49A5-9A57-CE585B617105}']);
    end;
end;

function TRandomTestDialog.GetRandomData (anIndex: WORD) : Double;
begin
    Result := 0.0;
    try
        if ( anIndex >= 0 ) and ( anIndex < 2 * f_DataLength ) then
            Result := f_Data^ [anIndex];
    except on E: Exception do
        _raise (['GetRandomData',ERR_TRANDOMTESTDIALOG_GET_RANDOM_DATA,E],
                ['{BCB9FE06-DEAC-4339-9E09-D8B8457EBA37}']);
    end;
end;

procedure TRandomTestDialog.SetRandomData (anIndex: WORD; anItem: Double);
begin
    try
        if ( anIndex >= 0 ) and ( anIndex < 2 * f_DataLength ) then
            f_Data^ [anIndex] := anItem;
    except on E: Exception do
        _raise (['SetRandomData',ERR_TRANDOMTESTDIALOG_SET_RANDOM_DATA,E],
                ['{A7081991-6B70-4A5F-8497-15CB5284EAC1}']);
    end;
end;

procedure TRandomTestDialog.SetEntropyDataIndex (const aValue: WORD);
begin
    try
        if ( aValue < 0 ) or ( aValue >= f_DataLength - 1 ) then
            f_EntropyDataIndex := 0
        else
            f_EntropyDataIndex := aValue;
    except on E: Exception do
        _raise (['SetEntropyDataIndex',ERR_TRANDOMTESTDIALOG_SET_ENTROPY_DATA_INDEX,E],
                ['{B5C47697-A204-49F4-A2DE-C8AF985EFB82}']);
    end;
end;

function TRandomTestDialog.GetEntropyData (anIndex: WORD) : Double;
begin
    Result := 0.0;
    try
        if ( anIndex >= 0 ) and ( anIndex < f_DataLength ) then
            Result := f_EntropyData^ [anIndex];
    except on E: Exception do
        _raise (['GetEntropyData',ERR_TRANDOMTESTDIALOG_GET_ENTROPY_DATA,E],
                ['{367A1C90-B6BA-4DD9-ABDB-7CAFC41A3863}']);
    end;
end;

procedure TRandomTestDialog.SetEntropyData (anIndex: WORD; anItem: Double);
begin
    try
        if ( anIndex >= 0 ) and ( anIndex < f_DataLength ) then
            f_EntropyData^ [anIndex] := anItem;
    except on E: Exception do
        _raise (['SetEntropyData',ERR_TRANDOMTESTDIALOG_SET_ENTROPY_DATA,E],
                ['{1C024117-63D9-4E47-B267-FCEFFA669DB8}']);
    end;
end;

procedure TRandomTestDialog.OnTimer (Sender: TObject);
var
    I      : WORD;
    Rect   : TRect;
    Params : double_function_params;
begin
    try
        with TRandomTestContent (Content) do
        begin
            { array [2n] + i*array [2n+1] }
            for I := 0 to ( 2 * DataLength - 1 ) do
            begin
                 Data [I]   := TCryptoKernel.Random ([0,DataLength,Randomtype]);
                 Data [I+1] := 0;
            end;
            Rect.Left := 4;
            Rect.Right := ptBuffer.Width - 4;
            Rect.Top := 4;
            Rect.Bottom := ptBuffer.Height - 4;
            Params.X1 := 0;
            Params.X2 := DataLength-2;
            Params.dX := 2;
            DrawDiscreteFunction (ptBuffer.Canvas,
                                  Rect,
                                  buffer_pool,
                                  f_Data,
                                  Params,
                                  clBlack,
                                  clMaroon,
                                  clRed,
                                  FALSE,
                                  FALSE,
                                  FALSE);

            EntropyDataIndex := EntropyDataIndex + 1;
            EntropyData [EntropyDataIndex] := EntropyPercent ( f_Data, DataLength );
            EntropyData [0] := 100;
            Rect.Left := 4;
            Rect.Right := ptEntropy.Width - 4;
            Rect.Top := 4;
            Rect.Bottom := ptEntropy.Height - 4;
            Params.X1 := 0;
            Params.X2 := DataLength;
            Params.dX := 1;
            DrawDiscreteFunction ( ptEntropy.Canvas,
                                   Rect,
                                   entropy_pool,
                                   f_EntropyData,
                                   Params,
                                   clBlack,
                                   clMaroon,
                                   clRed,
                                   TRUE,
                                   TRUE,
                                   TRUE,
                                   Format ('%2f %%',[ EntropyData [EntropyDataIndex] ]) );

            FFT (f_Data,DataLength);
            Rect.Left := 4;
            Rect.Right := ptFFT.Width - 4;
            Rect.Top := 4;
            Rect.Bottom := ptFFT.Height - 4;
            Params.X1 := 2;
            Params.X2 := DataLength;
            Params.dX := 1;
            DrawDiscreteFunction (ptFFT.Canvas,
                                  Rect,
                                  fft_pool,
                                  f_Data,
                                  Params,
                                  clBlack,
                                  clMaroon,
                                  clRed,
                                  TRUE,
                                  TRUE,
                                  TRUE);
        end;
    except on E: Exception do begin
        TTimer (Sender).Enabled := FALSE;
        ModalResult := mrCancel;
        _raise (['OnTimer',ERR_TRANDOMTESTDIALOG_ON_TIMER,E],
                ['{3287A853-DE83-4562-893A-2C33CDF37C76}']);
    end; end;
end;

procedure TRandomTestDialog.GetData (anArgs: array of const);
begin
    try
        inherited GetData (['тестирование генератора случайных чисел',dlgRandom],
                           [ _(['OK',btnOk,mrOk]) ]);
        { первый параметр - тип гпсч }
        if notEmpty (0,anArgs) then
            RandomType := toString (anArgs [0]);
        Caption := Format ('%s : %s',[Caption,RandomType]);
        { второй параметр - размер пула данных }
        if notEmpty (1,anArgs) then
            DataLength := toInteger (anArgs [1])
        else
            DataLength := 1024;
        with TRandomTestContent (Content) do
            tmRandom.OnTimer := OnTimer;
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{9EA630BE-3C7F-4A17-A855-F0FFE5FC27B6}']);
    end;
end;

class function TRandomTestDialog.Open (anArgs: array of const) : Integer;
begin
    try
        with Create (anArgs) do
        try
            GetData (anArgs);
            Result := ShowModal;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TPROTODIALOG_OPEN,E],
                ['{E2281D16-502F-414B-BABF-B6A4F6B27864}']);
    end;
end;

class function TRandomTestDialog.Execute (anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs) <> mrNone );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{CFB3C042-7F06-4D87-9028-E29E42723146}']);
    end;
end;

class procedure TRandomTestContent._raise (anArgs: array of const;
                                           const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TRandomTestContent._raise (anArgs: array of const;
                                           anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

procedure TRandomTestContent.FrameResize(Sender: TObject);
begin
    try
        bxBuffer.Height  := Height div 3 - 6;
        bxFFT.Height     := Height div 3 - 6;
        bxEntropy.Height := Height div 3 - 6;
    except on E: Exception do
        _raise (['FrameResize',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{949B72D4-E45E-4952-A164-09E2E4F0FF1D}']);
    end;
end;


end.
