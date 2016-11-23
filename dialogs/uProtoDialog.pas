unit uProtoDialog;
{******************************************************************************}
{*  Proto Dialog Unit                                                         *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
interface

{$I '../std.inc'}

uses
    Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
    Dialogs, Clipbrd, ComCtrls, StdCtrls, ExtCtrls, Menus,
    sSkinProvider, sSkinManager,
    ImgList, acAlphaImageList, jpeg, pngimage, acPNG,
    sPanel, Buttons, sButton, sBitBtn, Mask, sMaskEdit, RichEdit,
    Kernel, DialogClasses,
    Utils, Strings, Versions, VarRecs, EClasses;

type
{$M+}
    TDialogButton = class;
    TDialogButtons = class;
    TProtoDialog = class;
{$M-}

{$I 'TDialogButton.int.inc'}
{$I 'TDialogButtons.int.inc'}

{ класс контента диалогового окна }
    CFrame = class of TFrame;

{ диалоговое окно }
{$M+}
    CProtoDialog = class of TProtoDialog;
    PProtoDialog = ^TProtoDialog;
    TProtoDialog = class (TForm)
        SkinBlack: TsSkinManager;
        SkinProvider: TsSkinProvider;
        pnlBack: TsPanel;
        imgIcon: TImage;
        imgIcons: TsAlphaImageList;
        pnlButtons: TsPanel;
        imgButtons: TsAlphaImageList;
        imgMenu: TsAlphaImageList;
        mnTextPopup: TPopupMenu;
        mnItemCopy: TMenuItem;
        mnItemCut: TMenuItem;
        mnItemPaste: TMenuItem;
        mnItemDelete: TMenuItem;
        mnItemBreak: TMenuItem;
        mnItemSelectAll: TMenuItem;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_MinWidth: Integer;
        f_MaxWidth: Integer;
        f_MinHeight: Integer;
        f_MaxHeight: Integer;
        f_IconIndex: Integer;
        f_Buttons: TDialogButtons;
        f_Content: TFrame;
    protected
        procedure WMGetMinMaxInfo (var M: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    protected
        procedure OnFormResize (Sender: TObject); virtual;
        procedure OnFormShow (Sender: TObject); virtual;
        procedure OnFormKeyPress (Sender: TObject;
                                  var Key: Char); virtual;
        procedure OnButtonClick (Sender: TObject); virtual;
        procedure OnMenuPopup (Sender: TObject); virtual;
        procedure OnMenuClick (Sender: TObject); virtual;
    protected
        procedure SetIconIndex (const aValue: Integer); virtual;
    public
        procedure GetData (anArgs: array of const;
                           aButtons: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anArgs: array of const;
                            aButtons: array of const;
                            aContentClass: CFrame); overload; virtual;
        constructor Create (anArgs: array of const;
                            aButtons: array of const); overload; virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const;
                             aButtons: array of const) : Integer; overload; virtual;
        class function Execute (anArgs: array of const;
                                aButtons: array of const) : Boolean; overload; virtual;
    public
        property minWidth: Integer read f_MinWidth write f_MinWidth;
        property maxWidth: Integer read f_MaxWidth write f_MaxWidth;
        property minHeight: Integer read f_MinHeight write f_MinHeight;
        property maxHeight: Integer read f_MaxHeight write f_MaxHeight;
        property IconIndex: Integer read f_IconIndex write SetIconIndex;
        property Buttons: TDialogButtons read f_Buttons write f_Buttons;
        property Content: TFrame read f_Content write f_Content;
    end;
{$M-}

{$I 'TDialogButton.err.inc'}
{$I 'TDialogButtons.err.inc'}

resourcestring
    ERR_TPROTODIALOG_CREATE          = 'Ошибка создания!';
    ERR_TPROTODIALOG_DESTROY         = 'Ошибка уничтожения!';
    ERR_TPROTODIALOG_NOTIFY          = 'Ошибка обработки события!';
    ERR_TPROTODIALOG_ON_RESIZE       = 'Ошибка изменения размера!';
    ERR_TPROTODIALOG_ON_SHOW         = 'Ошибка получения фокуса!';
    ERR_TPROTODIALOG_ON_BUTTON_CLICK = 'Ошибка обработки нажатия на кнопку!';
    ERR_TPROTODIALOG_ON_KEY_PRESS    = 'Ошибка обработки нажатия на кнопку!';
    ERR_TPROTODIALOG_ON_MENU_POPUP   = 'Ошибка вызова контекстного меню!';
    ERR_TPROTODIALOG_ON_MENU_CLICK   = 'Ошибка действия контекстного меню!';
    ERR_TPROTODIALOG_GET_ICON_INDEX  = 'Ошибка чтения индекса иконки!';
    ERR_TPROTODIALOG_SET_ICON_INDEX  = 'Ошибка записи индекса иконки!';
    ERR_TPROTODIALOG_GET_DATA        = 'Ошибка чтения данных!';
    ERR_TPROTODIALOG_SET_DATA        = 'Ошибка записи данных!';
    ERR_TPROTODIALOG_OPEN            = 'Ошибка открытия!';
    ERR_TPROTODIALOG_EXECUTE         = 'Ошибка выполнения!';

implementation

{$R *.dfm}

{$I 'TDialogButton.imp.inc'}
{$I 'TDialogButtons.imp.inc'}

class procedure TProtoDialog._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TProtoDialog._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TProtoDialog.Create (anArgs: array of const;
                                 aButtons: array of const;
                                 aContentClass: CFrame);
var
    I : Integer;
begin
    try
        inherited Create (NIL);
        OnResize := OnFormResize;
        OnShow := OnFormShow;
        OnKeyPress := OnFormKeyPress;
        minWidth := 400;
        maxWidth := Screen.Width;
        minHeight := 200;
        maxHeight := Screen.Height;
        Left := (Screen.Width - Width) div 2;
        Top := (Screen.Height - Height) div 2;
        Caption := '';
        f_IconIndex := dlgCustom;
        f_Buttons := TDialogButtons.Create (pnlButtons,
                                            imgButtons,
                                            OnButtonClick);
        mnTextPopup.OnPopup := OnMenuPopup;
        for I := 0 to mnTextPopup.Items.Count - 1 do
            mnTextPopup.Items.Items [I].OnClick := OnMenuClick;
        if Assigned (aContentClass) then
            f_Content := aContentClass.Create (pnlBack);
        if Assigned (f_Content) then
            with f_Content do
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
                ['{17AE0EBC-7DA6-45E1-B0A4-37462556EBAF}']);
    end;
end;

constructor TProtoDialog.Create (anArgs: array of const;
                                 aButtons: array of const);
begin
    try
        Create (anArgs,aButtons,TFrame);
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{DA3DB6FC-B692-45ED-BDDA-F9F633499BB6}']);
    end;
end;

destructor TProtoDialog.Destroy;
begin
    try
        if Assigned (Buttons) then
            Buttons.FreeAll;
        FreeAndNil (f_Buttons);
        FreeAndNil (f_Content);
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{1AE3B7BF-E68A-4D0F-9FB3-024FDA4B6741}']);
    end;
end;

procedure TProtoDialog.WMGetMinMaxInfo (var M: TWMGetMinMaxInfo);
begin
    try
        with M.MinMaxInfo^ do
        begin
            PTMaxSize.X := maxWidth;
            PTMaxPosition.X := 0;
            PTMaxTrackSize.X := maxWidth;
            PTMinTrackSize.X := minWidth;
            PTMaxSize.Y := maxHeight;
            PTMaxPosition.Y := 0;
            PTMaxTrackSize.Y := maxHeight;
            PTMinTrackSize.Y := minHeight;
        end;
    except on E: Exception do
        _raise (['WMGetMinMaxInfo',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{A2DEF069-797D-472F-9F6B-05E35EAED098}']);
    end;
end;

procedure TProtoDialog.OnFormResize (Sender: TObject);
begin
    try
        if Assigned (Content) and Assigned (Content.OnResize) then
            Content.OnResize (Content);
    except on E: Exception do
        _raise (['OnFormResize',ERR_TPROTODIALOG_ON_RESIZE,E],
                ['{A3D01981-4215-4CF5-9D09-63B6948162F0}']);
    end;
end;

procedure TProtoDialog.OnFormShow (Sender: TObject);
begin
    try
        {do nothing}
    except on E: Exception do
        _raise (['OnFormShow',ERR_TPROTODIALOG_ON_SHOW,E],
                ['{63BB8E3A-A982-43E9-8D71-CF66352304BE}']);
    end;
end;

procedure TProtoDialog.OnButtonClick (Sender: TObject);
begin
    try
        SetData ( TDialogButton (Sender).ModalResult );
    except on E: Exception do
        _raise (['OnButtonClick',ERR_TPROTODIALOG_ON_BUTTON_CLICK,E],
                ['{C0EC7356-493B-40EA-B5B3-28BEB743402C}']);
    end;
end;

procedure TProtoDialog.OnFormKeyPress (Sender: TObject;
                                       var Key: Char);
var
    I : Integer;
begin
    case Ord (Key) of
        VK_RETURN : begin
            { ищем кнопку OK }
            for I := 0 to Buttons.Count - 1 do
                if ( Buttons.ItemAt [I].ModalResult = mrOK ) then
                begin
                    Buttons.ItemAt [I].Click;
                    Exit;
                end;
        end;
        VK_ESCAPE : Close;
    end;
end;

procedure TProtoDialog.OnMenuPopup (Sender: TObject);
begin
    try
        if TPopUpMenu (Sender).PopupComponent is TCustomEdit then
            with TCustomEdit ( TPopUpMenu (Sender).PopupComponent ),
                 TPopUpMenu (Sender).Items do
            begin
                Items [mnCopy].Enabled := ( SelLength > 0 ) and
                    not ( TPopUpMenu (Sender).PopupComponent.ClassType = TsMaskEdit );
                Items [mnCut].Enabled := not ReadOnly and ( SelLength > 0 ) and
                    not ( TPopUpMenu (Sender).PopupComponent.ClassType = TsMaskEdit );
                Items [mnPaste].Enabled := not ReadOnly and ClipBoard.HasFormat (CF_TEXT);
                Items [mnDelete].Enabled := not ReadOnly and ( SelLength > 0 );
                Items [mnSelectAll].Enabled := ( Length (Text) > 0 );
            end;
        if TPopUpMenu (Sender).PopupComponent is TCustomComboBox then
            with TComboBox ( TPopUpMenu (Sender).PopupComponent ),
                 TPopUpMenu (Sender).Items do
            begin
                Items [mnCopy].Enabled := ( SelLength > 0 ) and
                    not ( TPopUpMenu (Sender).PopupComponent.ClassType = TsMaskEdit );
                Items [mnCut].Enabled := ( SelLength > 0 ) and
                    not ( TPopUpMenu (Sender).PopupComponent.ClassType = TsMaskEdit );
                Items [mnPaste].Enabled := ClipBoard.HasFormat (CF_TEXT);
                Items [mnDelete].Enabled := ( SelLength > 0 );
                Items [mnSelectAll].Enabled := ( Length (Text) > 0 );
            end;
    except on E: Exception do
        _raise (['OnMenuPopup',ERR_TPROTODIALOG_ON_MENU_POPUP,E],
                ['{FE9B7709-4467-4155-8816-29552C1F1964}']);
    end;
end;

procedure TProtoDialog.OnMenuClick (Sender: TObject);
begin
    try
        if TPopUpMenu ( TMenuItem (Sender).GetParentMenu ).PopupComponent is TCustomEdit then
            with TCustomEdit (  TPopUpMenu ( TMenuItem (Sender).GetParentMenu ).PopupComponent  ) do
                case TMenuItem (Sender).MenuIndex of
                    mnCopy      : ClipBoard.AsText := SelText;
                    mnCut       : begin
                                      ClipBoard.AsText := SelText;
                                      SelText := '';
                                  end;
                    mnPaste     : SelText := ClipBoard.AsText;
                    mnDelete    : SelText := '';
                    mnSelectAll : SelectAll;
                end;
        if TPopUpMenu ( TMenuItem (Sender).GetParentMenu ).PopupComponent is TCustomComboBox then
            with TComboBox (  TPopUpMenu ( TMenuItem (Sender).GetParentMenu ).PopupComponent  ) do
                case TMenuItem (Sender).MenuIndex of
                    mnCopy      : ClipBoard.AsText := SelText;
                    mnCut       : begin
                                      ClipBoard.AsText := SelText;
                                      SelText := '';
                                  end;
                    mnPaste     : SelText := ClipBoard.AsText;
                    mnDelete    : SelText := '';
                    mnSelectAll : SelectAll;
                end;
    except on E: Exception do
        _raise (['OnMenuClick',ERR_TPROTODIALOG_ON_MENU_CLICK,E],
                ['{44D5E529-F095-49C0-9EEC-3BBEBF2BAB97}']);
    end;
end;

procedure TProtoDialog.SetIconIndex (const aValue: Integer);
begin
    try
        if ( (aValue >= dlgCustom) and (aValue <= imgIcons.Count-1) ) then
        begin
            f_IconIndex := aValue;
            if ( IconIndex > dlgCustom ) then
                imgIcons.GetBitmap32 (IconIndex, imgIcon.Picture.Bitmap);
            imgIcon.Visible := ( IconIndex > dlgCustom );
        end
        else
            SetIconIndex (dlgCustom);
    except on E: Exception do
        _raise (['SetIconIndex',ERR_TPROTODIALOG_SET_ICON_INDEX,E],
                ['{72CB1E17-A84B-451E-ADBD-8D7F48551132}']);
    end;
end;

procedure TProtoDialog.GetData (anArgs: array of const;
                                aButtons: array of const);
var
    I      : Integer;
    Button : TDialogButton;
begin
    try
        { первый параметр - заголовок }
        if notEmpty (0,anArgs) then
            Caption := toString (anArgs [0]);
        { второй параметр - иконка }
        if notEmpty (1,anArgs) then
            IconIndex := toInteger (anArgs [1]);
        { остальные параметры - кнопки }
        Buttons.Clear (TRUE); 
        for I := Low (aButtons) to High (aButtons) do
            if notEmpty (I,aButtons) then
            begin
                Button := TDialogButton.Create ( pnlButtons,
                                                 imgButtons,
                                                 OnButtonClick,
                                                 toArrayOfConst (aButtons [I]) );
                Buttons.Add (Button);
            end;
        { устанавливаем положение контента }
        if Assigned (Content) then
            with Content do
            begin
                if imgIcon.Visible then
                    Margins.Left := imgIcon.Left + imgIcon.Width + 10
                else
                    Margins.Left := 10;
            end;
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{879F7E53-19B5-4EC1-B9B0-B1BA90BAAFA1}']);
    end;
end;

procedure TProtoDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrNone : { do nothing };
            else     ModalResult := anIndex;
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TPROTODIALOG_SET_DATA,E],
                ['{03635422-DA19-4900-8573-3180F3C5560D}']);
    end;
end;

class function TProtoDialog.Open (anArgs: array of const;
                                  aButtons: array of const) : Integer;
begin
    try
        with Create (anArgs,aButtons) do
        try
            GetData (anArgs,aButtons);
            Result := ShowModal;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TPROTODIALOG_OPEN,E],
                ['{8D70FF87-ADEE-4F43-AE90-7FC05D32626A}']);
    end;
end;

class function TProtoDialog.Execute (anArgs: array of const;
                                     aButtons: array of const) : Boolean;
begin
    try
        Result := ( Open (anArgs,aButtons) <> mrNone );
    except on E: Exception do
        _raise (['Execute',ERR_TPROTODIALOG_EXECUTE,E],
                ['{DB8C68AE-3CB0-489C-998A-317BA127722F}']);
    end;
end;


end.
