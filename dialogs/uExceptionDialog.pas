unit uExceptionDialog;
{******************************************************************************}
{*  Exception Dialog Unit                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, SysUtils, Classes, Controls, Forms,
    Kernel, DialogClasses,
    uProtoDialog, uTextDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses;

type
{ диалоговое окно вывода исключений }
{$M+}
    CExceptionDialog = class of TExceptionDialog;
    PExceptionDialog = ^TExceptionDialog;
    TExceptionDialog = class (TTextDialog)
    public
        class procedure OpenExceptionDialog (Sender: TObject; Error: Exception);
    end;
{$M-}

implementation

class procedure TExceptionDialog.OpenExceptionDialog (Sender: TObject; Error: Exception);
begin
    try
        Error.Message := StrReplace (Error.Message,'{','[COLOR:#FF0000]');
        Error.Message := StrReplace (Error.Message,'}','[/COLOR]');
        Error.Message := Format ('[B]%s[/B]',[Error.Message]);
        Open ([ 'ошибка', dlgError, Error.Message ],
              [ _(['OK',btnOK,mrOk]) ]);
    except on E: Exception do
        _raise (['OpenExceptionDialog',ERR_TPROTODIALOG_OPEN,E],
                ['{70951960-7ADC-4360-8940-3A54EBD354A4}']);
    end;
end;

initialization
    Application.OnException := TExceptionDialog.OpenExceptionDialog;


end.