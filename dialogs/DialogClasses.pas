unit DialogClasses;
{******************************************************************************}
{*  Dialog Classes Unit                                                       *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs,
    {$IF not Defined (MINI_CRYPTO) }
    imgList, acPNG, acAlphaImageList,
    {$IFEND}
    Kernel, ProtoClasses,
    {$IF not Defined (MINI_CRYPTO) }
    CryptoClasses,
    Engine,
    {$IFEND}
    Utils, Strings, Versions, VarRecs,
    EClasses;

{ dialog properties types }
const
    dtPassword  = 21;
    dtFile      = 22;
    dtDirectory = 23;

{ иконки }
const
    dlgCustom       = -1;
    dlgWarning      = 0;
    dlgError        = 1;
    dlgInformation  = 2;
    dlgConfirmation = 3;
    dlgHint         = 4;
    dlgHelp         = 5;
    dlgLogin        = 6;
    dlgPassword     = 7;
    dlgRandom       = 8;
    dlgSettings     = 9;
    dlgBinary       = 10;
    dlgAdd          = 11;
    dlgEdit         = 12;
    dlgDelete       = 13;
    dlgTime         = 14;

{ кнопки }
const
    btnCustom   = -1;
    btnOK       = 0;
    btnCancel   = 1;
    btnPrevious = 2;
    btnNext     = 3;
    btnEncrypt  = 4;
    btnDecrypt  = 5;

const
{ пункты меню }
    mnCopy      = 0;
    mnCut       = 1;
    mnPaste     = 2;
    mnDelete    = 3;
    mnSelectAll = 5;

type
{$M+}
    TDialogField = class;
    TDialogFields = class;
{$M-}

{ описание диалогового поля }
{$I 'TDialogField.int.inc'}
{$I 'TDialogFields.int.inc'}

{ ошибки диалогового поля }
{$I 'TDialogField.err.inc'}
{$I 'TDialogFields.err.inc'}

procedure ShowMessage (const Msg: string);
procedure ShowMessageFmt (const Msg: string; Params: array of const);
procedure ShowError (const Msg: string);
procedure ShowErrorFmt (const Msg: string; Params: array of const);
function MessageDlg (const Msg: string; Icon: Integer; Buttons: array of const) : Integer;

{$IF not Defined (MINI_CRYPTO) }
function GetColor (const aColor: TColor) : TColor;
function GetSmile (const aSmiles: TsAlphaImageList) : String;

function SignIn (const aDBFileName: String;
                 aLogin: String = '') : Boolean;

function AddCategorie (const aDBFileName: String;
                       const aParentID: TID) : Boolean;
function EditCategorie (const aDBFileName: String;
                        const anID: TID) : Boolean;
function DeleteCategorie (const aDBFileName: String;
                          const anID: TID) : Boolean;

function AddMessage (const aDBFileName: String;
                     const aCategorieID: TID) : Boolean;
function EditMessage (const aDBFileName: String;
                      const anID: TID) : Boolean;
function DeleteMessage (const aDBFileName: String;
                        const anID: TID) : Boolean;

function AddUser (const aDBFileName: String;
                  const aLogin: String) : Boolean;
function EditUser (const aDBFileName: String;
                   const anID: TID) : Boolean;
function DeleteUser (const aDBFileName: String;
                     const anID: TID) : Boolean;
{$IFEND}

function RandomTest (const genRandom: String) : Boolean;
function AsymmetricTest (const algAsymmetric: String) : Boolean;
function SymmetricTest (const algSymmetric: String;
                        const modeSymmetric: String) : Boolean;
function HashTest (const algHash: String) : Boolean;

implementation

uses
    uProtoDialog, uMetaDialog, uTextDialog,
    uExceptionDialog,
    {$IF not Defined (MINI_CRYPTO) }
    uColorDialog,
    uIconDialog,
    uSmileDialog,
    uLoginDialog,
    uRegisterDialog,
    uCategorieDialog,
    uMessageDialog,
    uUserDialog,
    {$IFEND}
    uRandomTestDialog,
    uAsymmetricTestDialog,
    uSymmetricTestDialog,
    uHashTestDialog,
    Crypto;

{ реализация диалогового поля }
{$I 'TDialogField.imp.inc'}
{$I 'TDialogFields.imp.inc'}

procedure ShowMessage (const Msg: string);
begin
    TTextDialog.Open ([ 'сообщение', dlgHint, Format ('[B]%s[/B]',[Msg]) ],
                      [ _(['OK',btnOK,mrOk]) ]);
end;

procedure ShowMessageFmt (const Msg: string; Params: array of const);
begin
    ShowMessage ( Format (Msg, Params) );
end;

procedure ShowError (const Msg: string);
begin
    TTextDialog.Open ([ 'ошибка', dlgError, Format ('[B]%s[/B]',[Msg]) ],
                      [ _(['OK',btnOK,mrOk]) ]);
end;

procedure ShowErrorFmt (const Msg: string; Params: array of const);
begin
    ShowError ( Format (Msg, Params) );
end;

function MessageDlg (const Msg: string; Icon: Integer; Buttons: array of const) : Integer;
begin
    Result := TTextDialog.Open ([ '', Icon, Format ('[B]%s[/B]',[Msg]) ],Buttons);
end;

{$IF not Defined (MINI_CRYPTO) }
function GetColor (const aColor: TColor) : TColor;
begin
    Result := aColor;
    TColorDialog.Execute ([aColor],Result);
end;

function GetSmile (const aSmiles: TsAlphaImageList) : String;
begin
    Result := '';
    TSmileDialog.Execute ([aSmiles],Result);
end;

function SignIn (const aDBFileName: String;
                 aLogin: String = '') : Boolean;
begin
    Result := TLoginDialog.Execute (aDBFileName,[aLogin])
end;

function AddCategorie (const aDBFileName: String;
                       const aParentID: TID) : Boolean;
begin
    Result := TCategorieDialog.Execute (aDBFileName,[0,aParentID]);
end;

function EditCategorie (const aDBFileName: String;
                        const anID: TID) : Boolean;
begin
    Result := TCategorieDialog.Execute (aDBFileName,[anID]);
end;

function DeleteCategorie (const aDBFileName: String;
                          const anID: TID) : Boolean;
begin
    Result := TCategorieDeleteDialog.Execute (aDBFileName,[anID]);
end;

function AddMessage (const aDBFileName: String;
                     const aCategorieID: TID) : Boolean;
begin
    //Result := TMessageDialog.Execute (aDBFileName,[0,aCategorieID]);
end;

function EditMessage (const aDBFileName: String;
                      const anID: TID) : Boolean;
begin
    //Result := TMessageDialog.Execute (aDBFileName,[anID]);
end;

function DeleteMessage (const aDBFileName: String;
                        const anID: TID) : Boolean;
begin
    Result := TMessageDeleteDialog.Execute (aDBFileName,[anID]);
end;

function AddUser (const aDBFileName: String;
                  const aLogin: String) : Boolean;
begin
    Result := TUserDialog.Execute (aDBFileName,[0,aLogin]);
end;

function EditUser (const aDBFileName: String;
                   const anID: TID) : Boolean;
begin
    Result := TUserDialog.Execute (aDBFileName,[anID]);
end;

function DeleteUser (const aDBFileName: String;
                     const anID: TID) : Boolean;
begin
    Result := TUserDeleteDialog.Execute (aDBFileName,[anID]);
end;
{$IFEND}

function RandomTest (const genRandom: String) : Boolean;
begin
    Result := TRandomTestDialog.Execute ([genRandom]);
end;

function AsymmetricTest (const algAsymmetric: String) : Boolean;
begin
    Result := TAsymmetricTestDialog.Execute ([algAsymmetric]);
end;

function SymmetricTest (const algSymmetric: String;
                        const modeSymmetric: String) : Boolean;
var
    alg : TCryptoAlgoritm;
    I   : Integer;
begin
    Result := TRUE;
    try
        alg := StrToCryptoAlgoritm (algSymmetric);
        for I := 0 to High (alg) do
            if not TSymmetricTestDialog.Execute ([ CryptoTypeToStr (alg [I]),
                                                   modeSymmetric ]) then
            begin
                Result := FALSE;
                Break;
            end;
    except
        Result := FALSE;
        raise;
    end;
end;

function HashTest (const algHash: String) : Boolean;
var
    alg : THashAlgoritm;
    I   : Integer;
begin
    Result := TRUE;
    try
        alg := StrToHashAlgoritm (algHash);
        for I := 0 to High (alg) do
            if not THashTestDialog.Execute ([ HashTypeToStr (alg [I]) ]) then
            begin
                Result := FALSE;
                Break;
            end;
    except
        Result := FALSE;
        raise;
    end;
end;


end.
