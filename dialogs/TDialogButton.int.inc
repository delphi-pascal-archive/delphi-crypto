{******************************************************************************}
{*  TDialogButton.int.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{ кнопка диалогового окна }
{$M+}
    CDialogButton = class of TDialogButton;
    PDialogButton = ^TDialogButton;
    TDialogButton = class (TsBitBtn)
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_ModalResult: Integer;
    protected
        class function toButtonAlign (const aValue: TVarRec) : TAlign;
        class function toButtonLayout (const aValue: TVarRec) : TButtonLayout;
    public
        constructor Create (anOwner: TWinControl;
                            aCaption: String = '';
                            anImageList: TCustomImageList = NIL;
                            anImageIndex: Integer = dlgCustom;
                            aModalResult: Integer = mrNone;
                            anOnClick: TNotifyEvent = NIL;
                            anAlign: TAlign = alRight;
                            aLayout: TButtonLayout = blGlyphLeft); overload; virtual;
        constructor Create (anOwner: TWinControl;
                            anImageList: TCustomImageList;
                            anOnClick: TNotifyEvent;
                            anArgs: array of const); overload; virtual;
        destructor Destroy; override;
        procedure Resize; virtual;
    public
        property ModalResult: Integer read f_ModalResult write f_ModalResult;
    end;
{$M-}
