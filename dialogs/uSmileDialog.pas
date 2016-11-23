unit uSmileDialog;
{******************************************************************************}
{*  Smile Dialog Unit                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Clipbrd, StdCtrls, ExtCtrls, Buttons, Menus, ImgList,
    acPNG, acAlphaImageList,
    sSkinProvider, sSkinManager,
    sPanel, sSpeedButton,
    Kernel, DialogClasses,
    uIconDialog,
    Utils, Strings, Versions, VarRecs,
    EClasses;

const
    SMILES : array [0..9] of String = (
        ':)', ':D', ':(', ';)',
        '.)', ':C', ':o', '%)',
        '8)', '<3'
    );

    SMILES_ADVANCED : array [0..6] of String = (
        ':D.', 'xD', ':P', ':/',
        'O_O', ':|', 'O~*'
    );

type
    CSmileDialog = class of TSmileDialog;
    PSmileDialog = ^TSmileDialog;
    TSmileDialog = class (TIconDialog)
    private
        f_Smile: String;
    protected
        procedure OnIconClick (Sender: TObject); override;
    public
        constructor Create (anArgs: array of const); override;
        destructor Destroy; override;
        class function Open (anArgs: array of const; out aSmile: String) : Integer; virtual;
        class function Execute (anArgs: array of const; out aSmile: String) : Boolean; virtual;
    public
        property Smile: String read f_Smile write f_Smile;
    end;

resourcestring
    ERR_TSMILEDIALOG_CREATE             = 'Ошибка создания!';
    ERR_TSMILEDIALOG_DESTROY            = 'Ошибка уничтожения!';
    ERR_TSMILEDIALOG_INVALID_ICONS      = 'Некорректный набор смайлов!';
    ERR_TSMILEDIALOG_INVALID_ICON_INDEX = 'Некорректный индекс изображения!';
    ERR_TSMILEDIALOG_GET_DATA           = 'Ошибка чтения данных!';
    ERR_TSMILEDIALOG_SET_DATA           = 'Ошибка записи данных!';
    ERR_TSMILEDIALOG_OPEN               = 'Ошибка открытия!';
    ERR_TSMILEDIALOG_EXECUTE            = 'Ошибка выполнения!';

implementation

constructor TSmileDialog.Create (anArgs: array of const);
var
    I   : Integer;
    btn : TsSpeedButton;
begin
    try
        inherited Create (anArgs);
        f_Smile := '';
    except on E: Exception do
        _raise (['Create',ERR_TSMILEDIALOG_CREATE,E],
                ['{3EFDFE4F-7F3E-4FF0-B84B-DE3172CA9D86}']);
    end;
end;

destructor TSmileDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TSMILEDIALOG_DESTROY,E],
                ['{0A0A1CB6-1D0C-4096-9E88-AE2C458AE9F5}']);
    end;
end;

procedure TSmileDialog.OnIconClick (Sender: TObject);
begin
    inherited OnIconClick (Sender);
    if ( IconIndex >= Low (SMILES) ) and ( IconIndex <= High (SMILES) ) then
        Smile := SMILES [IconIndex]
    else
        raise Exception.CreateFmt ('%s'#13#10'%d out of bounds [%d,%d]',
                                   [ ERR_TSMILEDIALOG_INVALID_ICON_INDEX,
                                     IconIndex, Low (SMILES), High (SMILES) ]);
end;

class function TSmileDialog.Open (anArgs: array of const; out aSmile: String) : Integer;
begin
    try
        with Create (anArgs) do
        try
            GetData (anArgs);
            Result := ShowModal;
            if ( Result = mrOk ) then
                aSmile := Smile;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TSMILEDIALOG_OPEN,E],
                ['{2234E9BE-DCC5-4870-9AB1-F9D54777A55A}']);
    end;
end;

class function TSmileDialog.Execute (anArgs: array of const; out aSmile: String) : Boolean;
begin
    try
        Result := ( Open (anArgs,aSmile) = mrIcon );
    except on E: Exception do
        _raise (['Execute',ERR_TSMILEDIALOG_EXECUTE,E],
                ['{40C47668-BD73-4887-8360-235DA1A2EE38}']);
    end;
end;


end.
