unit uHashTestDialog;
{******************************************************************************}
{*  Hash Test Dialog Unit                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Clipbrd, StdCtrls, ExtCtrls, ComCtrls, Buttons, Menus, ImgList,
    acPNG, acAlphaImageList,
    sSkinProvider, sSkinManager,
    sPanel, sPageControl, sLabel, sButton, sBitBtn, sSpeedButton,
    Mask, sMaskEdit, sRichEdit,
    Crypto, Kernel, DialogClasses,
    uProtoDialog, uMetaDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses;

{ кнопки }
const
    btAutoTest = 0;
    btHash     = 1;

{ действия }
const
    mrAutoTest = 14;
    mrHash     = 15;

type
{ диалоговое окно тестирования асимметричного шифра }
{$M+}
    CHashTestDialog = class of THashTestDialog;
    PHashTestDialog = ^THashTestDialog;
    THashTestDialog = class (TMetaDialog)
    private
        f_HashAlgoritm: String;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        procedure doAutoTest; virtual;
        procedure doHash; virtual;
    public
        constructor Create (anArgs: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const) : Integer; overload; virtual;
        class function Execute (anArgs: array of const) : Boolean; overload; virtual;
    public
        property HashAlgoritm : String read f_HashAlgoritm write f_HashAlgoritm;
    end;
{$M-}

resourcestring
    ERR_THASHTESTDIALOG_AUTO_TEST = 'Ошибка тестирования хэш-функции!';
    ERR_THASHTESTDIALOG_HASH      = 'Ошибка вычисления значения хэш-функции!';
    ERR_THASHTESTDIALOG_TEST      = 'Ошибка тестирования хэш-функции ''%s''!';

implementation

constructor THashTestDialog.Create (anArgs: array of const);
begin
    try
        inherited Create ([ 'тестирование хэш-функции', dlgBinary ],
                          [ _(['OK',btnOk,mrAutoTest]),
                            _(['Вычислить',btnCustom,mrHash,'alLeft']) ],
                          [ _(['message','сообщение',dtText,'']),
                            _(['hash','хэш',dtString,'']) ]);
        maxHeight := minHeight;
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{C3624A36-6CDC-48D5-AB55-B077125FBA22}']);
    end;
end;

destructor THashTestDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{AA3004DB-5EB8-480A-BCD7-57D5E48C5AD1}']);
    end;
end;

procedure THashTestDialog.GetData (anArgs: array of const);
var
    I : Integer;
begin
    try
        inherited GetData ([ 'тестирование хэш-функции', dlgBinary ],
                           [ _(['OK',btnOk,mrAutoTest]),
                             _(['Вычислить',btnCustom,mrHash,'alLeft']) ],
                           [ _(['message','сообщение',dtText,'']),
                             _(['hash','хэш',dtString,'']) ]);
        with TMetaContent (Content) do
        begin
            with TMemo (Items.ItemOf ['message'].Control) do
            begin
                AutoSize := FALSE;
                Font.Name := 'Courier New';
                Font.Style := [];
                Font.Size := 8;
            end;
            with TEdit (Items.ItemOf ['hash'].Control) do
            begin
                AutoSize := FALSE;
                Font.Name := 'Courier New';
                Font.Style := [];
                Font.Size := 8;
            end;
        end;
        { первый параметр - алгоритм хэширования }
        if notEmpty (0,anArgs) then
            HashAlgoritm := toString (anArgs [0]);
        Caption := Format ('%s : %s',[Caption,HashAlgoritm]);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{8BC6AAED-6975-496C-ABB1-C56DBB7CD8BC}']);
    end;
end;

procedure THashTestDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrAutoTest : doAutoTest;
            mrHash     : doHash;
            else         inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{CC08ED4A-0E90-4284-93F8-F667DE9A4CD3}']);
    end;
end;

procedure THashTestDialog.doAutoTest;
begin
    try
        if not TCryptoKernel.TestHash ([HashAlgoritm]) then
            raise Exception.CreateFmt (ERR_THASHTESTDIALOG_TEST,
                                       [HashAlgoritm]);
        inherited SetData (mrOk);
    except on E: Exception do
        _raise (['doAutoTest',ERR_THASHTESTDIALOG_AUTO_TEST,E],
                ['{DC091C0B-A6DC-4DD8-8A2F-A53C005A352B}']);
    end;
end;

procedure THashTestDialog.doHash;
var
    alg : THashType;
begin
    try
        alg := StrToHashType (HashAlgoritm);
        ValueOf ['hash'] := TCryptoKernel.Hash ( ValueOf ['message'], alg );
    except on E: Exception do
        _raise (['doHash',ERR_THASHTESTDIALOG_HASH,E],
                ['{F2CB89DC-B5B5-459C-9DDF-285E592A84B0}']);
    end;
end;

class function THashTestDialog.Open (anArgs: array of const) : Integer;
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
                ['{6CDC0231-62FB-4AA0-AAC3-94775448F9E6}']);
    end;
end;

class function THashTestDialog.Execute (anArgs: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{5909EC72-9993-4E39-B004-52EDF37C7F57}']);
    end;
end;


end.
