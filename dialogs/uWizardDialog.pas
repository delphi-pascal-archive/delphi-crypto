unit uWizardDialog;
{******************************************************************************}
{*  Register Dialog Unit                                                      *}
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
    sPanel, sButton, sBitBtn, sPageControl, sLabel, sEdit, sRichEdit, sMemo,
    Mask, sMaskEdit, sSpinEdit, sTooledit, sCustomComboEdit,
    sCurrEdit, sCurrencyEdit, sCheckBox,
    Kernel, DialogClasses,
    uProtoDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses;

{ кнопки }
const
    btPrev     = 0;
    btNext     = 1;
    btCancel   = 2;

{ действия }
const
    mrPrevious = 14;
    mrNext     = 15;
    mrFinish   = 16;

type
{$M+}
    TWizardContent = class;
    TWizardDialog = class;
{$M-}

{ мастер-диалог }
{$M+}
    CWizardDialog = class of TWizardDialog;
    PWizardDialog = ^TWizardDialog;
    TWizardDialog = class (TProtoDialog)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); override;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); override;
    public
        procedure GetData (anArgs: array of const;
                           aTabs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); override;
    protected
        function Validate (aPageIndex: Integer) : Boolean; virtual;
    public
        constructor Create (anArgs: array of const;
                            aTabs: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const;
                             aTabs: array of const) : Integer; overload; virtual;
        class function Execute (anArgs: array of const;
                                aTabs: array of const) : Boolean; overload; virtual;
    end;
{$M-}

{ содержимое }
{$M+}
    CWizardContent = class of TWizardContent;
    PWizardContent = ^TWizardContent;
    TWizardContent = class (TFrame)
        tabs: TsPageControl;
    private
        f_List: TList;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    protected
        procedure OnTabChange (Sender: TObject); virtual;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anOwner: TProtoDialog;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
        procedure Previous;
        procedure Next;
    end;
{$M-}

resourcestring
    ERR_TWIZARDDIALOG_VALIDATE       = 'Ошибка валидации данных!';

resourcestring
    ERR_TWIZARDCONTENT_CREATE        = 'Ошибка создания!';
    ERR_TWIZARDCONTENT_DESTROY       = 'Ошибка уничтожения!';
    ERR_TWIZARDCONTENT_GET_VALUE     = 'Ошибка чтения поля!';
    ERR_TWIZARDCONTENT_SET_VALUE     = 'Ошибка записи поля!';
    ERR_TWIZARDCONTENT_GET_DATA      = 'Ошибка чтения данных!';
    ERR_TWIZARDCONTENT_SET_DATA      = 'Ошибка записи данных!';
    ERR_TWIZARDCONTENT_ON_TAB_CHANGE = 'Ошибка выбора закладки!';

implementation

{$R *.dfm}

class procedure TWizardDialog._raise (anArgs: array of const;
                                      const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TWizardDialog._raise (anArgs: array of const;
                                      anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TWizardDialog.Create (anArgs: array of const;
                                  aTabs: array of const);
begin
    try
        inherited Create (anArgs,
                          [ _(['Назад',btnPrevious,mrPrevious]),
                            _(['Далее',btnNext,mrNext,'alRight','blGlyphRight']),
                            _(['Отмена',btnCancel,mrCancel]) ],
                          NIL);
        Content := TWizardContent.Create (self,aTabs);
        minHeight := 64 + TWizardContent (Content).Height + 8 + pnlButtons.Height;
        Height := minHeight;
        with TWizardContent (Content) do
        begin
            Parent := pnlBack;
            Align := alClient;
            AlignWithMargins := TRUE;
            Margins.Left := 10;
            Margins.Top := 0;
            Margins.Right := 10;
            Margins.Bottom := 8;
        end;
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{494D680B-F0A4-487A-878E-8507B7239C92}']);
    end;
end;

destructor TWizardDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{37C8DD88-0247-43F4-800F-5139595B0E95}']);
    end;
end;

function TWizardDialog.Validate (aPageIndex: Integer) : Boolean;
begin
    Result := TRUE;
    try
        with TWizardContent (Content) do
            Result := Result and
                      ( aPageIndex >= 0 ) and
                      ( aPageIndex <= tabs.PageCount - 1 );
    except on E: Exception do
        _raise (['Validate',ERR_TWIZARDDIALOG_VALIDATE,E],
                ['{EACE62BC-0A39-46C5-B4AB-4B934D9EB5DC}']);
    end;
end;

procedure TWizardDialog.GetData (anArgs: array of const;
                                 aTabs: array of const);
begin
    try
        inherited GetData (anArgs,
                           [ _(['Назад',btnPrevious,mrPrevious]),
                             _(['Далее',btnNext,mrNext,'alRight','blGlyphRight']),
                             _(['Отмена',btnCancel,mrCancel]) ]);
        with TWizardContent (Content) do
            GetData (aTabs);
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{EE7F3E3E-EFF5-4C79-82A0-99DD19FFBF43}']);
    end;
end;

procedure TWizardDialog.SetData (anIndex: Integer);
begin
    try
        with TWizardContent (Content) do
            SetData (anIndex);
        case anIndex of
            mrPrevious : with TWizardContent (Content) do
                             Previous;
            mrNext     : with TWizardContent (Content) do
                             Next;
            else         inherited SetData (anIndex);
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{7A6A34F7-8274-44F9-8EC0-AB262DDA0352}']);
    end;
end;

class function TWizardDialog.Open (anArgs: array of const;
                                   aTabs: array of const) : Integer;
begin
    try
        with Create (anArgs,aTabs) do
        try
            GetData (anArgs,aTabs);
            Result := ShowModal;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TPROTODIALOG_OPEN,E],
                ['{28D5020B-B8EC-4D49-A97C-5B6BDCF599DE}']);
    end;
end;

class function TWizardDialog.Execute (anArgs: array of const;
                                      aTabs: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs,aTabs) = mrOk );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{02EBEC78-B2F5-4CF8-95BF-C564D09874FA}']);
    end;
end;

class procedure TWizardContent._raise (anArgs: array of const;
                                       const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TWizardContent._raise (anArgs: array of const;
                                       anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TWizardContent.Create (anOwner: TProtoDialog;
                                   anArgs: array of const);
var
    I   : Integer;
    tab : TsTabSheet;
begin
    try
        inherited Create (anOwner);
        Height := 200;
        tabs.OnChange := OnTabChange;
        { параметры - вкладки }
        for I := Low (anArgs) to High (anArgs) do
        begin
            tab := TsTabSheet.Create (tabs);
            tab.PageControl := tabs;
            tab.Caption := toString (anArgs [I]);
        end;
    except on E: Exception do
        _raise (['Create',ERR_TWIZARDCONTENT_CREATE,E],
                ['{25D04B54-1F92-4B61-BBA9-91A7ACA8E268}']);
    end;
end;

destructor TWizardContent.Destroy;
begin
    try
        while ( tabs.PageCount > 0 ) do
            tabs.Pages [tabs.PageCount - 1].Free;
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TWIZARDCONTENT_DESTROY,E],
                ['{5F73C783-49B7-4430-9C8A-AA28CC9EE597}']);
    end;
end;

procedure TWizardContent.OnTabChange (Sender: TObject);
begin
    try
        with TProtoDialog (Owner) do
        begin
            Buttons.ItemAt [btPrev].Enabled := ( tabs.TabIndex > 0 );
            Buttons.ItemAt [btNext].Enabled := ( tabs.TabIndex <= tabs.PageCount - 1 );
            if ( tabs.TabIndex = tabs.PageCount - 1 ) then
            begin
                Buttons.ItemAt [btNext].Caption := 'Готово';
                Buttons.ItemAt [btNext].ImageIndex := btnCustom;
                Buttons.ItemAt [btNext].ModalResult := mrFinish;
                Buttons.ItemAt [btNext].Resize;
            end
            else
            begin
                Buttons.ItemAt [btNext].Caption := 'Далее';
                Buttons.ItemAt [btNext].ImageIndex := btnNext;
                Buttons.ItemAt [btNext].ModalResult := mrNext;
                Buttons.ItemAt [btNext].Resize;
            end;
        end;
    except on E: Exception do
        _raise (['OnTabChange',ERR_TWIZARDCONTENT_ON_TAB_CHANGE,E],
                ['{A80991C8-FE85-4367-A9DC-71D42D8859C0}']);
    end;
end;

procedure TWizardContent.Previous;
begin
    try
        tabs.TabIndex := tabs.TabIndex - 1;
        tabs.OnChange (tabs);
    except on E: Exception do
        _raise (['Previous',ERR_TWIZARDCONTENT_ON_TAB_CHANGE,E],
                ['{D239EB15-B456-4D27-B587-B987B4CD4319}']);
    end;
end;

procedure TWizardContent.Next;
begin
    try
        if TWizardDialog (Owner).Validate (tabs.TabIndex) then
        begin
            tabs.TabIndex := tabs.TabIndex + 1;
            tabs.OnChange (tabs);
        end;
    except on E: Exception do
        _raise (['Next',ERR_TWIZARDCONTENT_ON_TAB_CHANGE,E],
                ['{1CAC3662-9D77-4B87-904B-B12163D04A46}']);
    end;
end;

procedure TWizardContent.GetData (anArgs: array of const);
begin
    try
        OnTabChange (tabs);
    except on E: Exception do
        _raise (['GetData',ERR_TWIZARDCONTENT_GET_DATA,E],
                ['{2AD1DACD-C392-446C-ACFC-F709E2AC1321}']);
    end;
end;

procedure TWizardContent.SetData (anIndex: Integer);
begin
    try
        {do nothing}
    except on E: Exception do
        _raise (['SetData',ERR_TWIZARDCONTENT_SET_DATA,E],
                ['{1DF6AAE4-0C62-4FD4-A592-09226E9AB566}']);
    end;
end;


end.
