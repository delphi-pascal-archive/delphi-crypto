unit uTextDialog;
{******************************************************************************}
{*  Text Dialog Unit                                                          *}
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
    sPanel, sButton, sBitBtn, Mask, sMaskEdit, sRichEdit,
    Kernel, DialogClasses,
    uProtoDialog,
    Utils, Strings, BBCode, Versions, VarRecs,
    EClasses;

type
{$M+}
    TTextDialog = class;
    TTextContent = class;
{$M-}

{ диалоговое окно с текстовым полем }
{$M+}
    CTextDialog = class of TTextDialog;
    PTextDialog = ^TTextDialog;
    TTextDialog = class (TProtoDialog)
    private
        f_Message: String;
    public
        procedure GetData (anArgs: array of const;
                           aButtons: array of const); override;
    public
        constructor Create (anArgs: array of const;
                            aButtons: array of const); override;
        destructor Destroy; override;
    public
        property Message: String read f_Message write f_Message;
    end;
{$M-}

{ содержимое }
{$M+}
    CTextContent = class of TTextContent;
    PTextContent = ^TTextContent;
    TTextContent = class (TFrame)
        pnlBackGround: TsPanel;
        edMessage: TsRichEdit;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    end;
{$M-}

implementation

{$R *.dfm}

constructor TTextDialog.Create (anArgs: array of const;
                                aButtons: array of const);
begin
    try
        inherited Create (anArgs,aButtons,TTextContent);
        f_Message := '';
        with TTextContent (Content) do
            edMessage.PopupMenu := mnTextPopup; 
    except on E: Exception do
        _raise (['Create',ERR_TPROTODIALOG_CREATE,E],
                ['{95FA9B56-B598-4E37-BFE0-862D5A0F6D31}']);
    end;
end;

destructor TTextDialog.Destroy;
begin
    try
        _FillChar ( f_Message, Length (f_Message), $00 );
        if Assigned (Content) then
            with TTextContent (Content) do
                edMessage.Clear;
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TPROTODIALOG_DESTROY,E],
                ['{A7DB3256-6355-4B7E-A533-52B559A14B93}']);
    end;
end;

procedure TTextDialog.GetData (anArgs: array of const;
                               aButtons: array of const);
begin
    try
        inherited GetData (anArgs,aButtons);
        { третий параметр - текст }
        if notEmpty (2,anArgs) then
            Message := toString (anArgs [2]);
        with TTextContent (Content) do
        begin
            edMessage.Clear;
            //edMessage.Text := Message;
            InsertBBCode (edMessage, Message);
        end;
    except on E: Exception do
        _raise (['GetData',ERR_TPROTODIALOG_GET_DATA,E],
                ['{7300235F-4869-42AA-9360-303AC0FF79C0}']);
    end;
end;

class procedure TTextContent._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TTextContent._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;


end.
