unit uIconDialog;
{******************************************************************************}
{*  Icon Dialog Unit                                                          *}
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
    Utils, Strings, Versions, VarRecs,
    EClasses;

const
    mrIcon = 13;

type
    CIconDialog = class of TIconDialog;
    PIconDialog = ^TIconDialog;
    TIconDialog = class (TForm)
        pnlBackGround: TsPanel;
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Icons: TsAlphaImageList;
        f_IconIndex: Integer;
        f_ColCount: WORD;
    protected
        procedure OnIconClick (Sender: TObject); virtual;
        procedure OnBackgroundClick (Sender: TObject); virtual;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anArgs: array of const); virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const; out anIconIndex: Integer) : Integer; virtual;
        class function Execute (anArgs: array of const; out anIconIndex: Integer) : Boolean; virtual;
    public
        property Icons: TsAlphaImageList read f_Icons;
        property IconIndex: Integer read f_IconIndex write f_IconIndex;
        property ColCount: WORD read f_ColCount write f_ColCount;
    end;

resourcestring
    ERR_TICONDIALOG_CREATE        = 'Ошибка создания!';
    ERR_TICONDIALOG_DESTROY       = 'Ошибка уничтожения!';
    ERR_TICONDIALOG_INVALID_ICONS = 'Некорректный набор изображений!';
    ERR_TICONDIALOG_GET_DATA      = 'Ошибка чтения данных!';
    ERR_TICONDIALOG_SET_DATA      = 'Ошибка записи данных!';
    ERR_TICONDIALOG_OPEN          = 'Ошибка открытия!';
    ERR_TICONDIALOG_EXECUTE       = 'Ошибка выполнения!';

implementation

{$R *.dfm}

class procedure TIconDialog._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TIconDialog._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TIconDialog.Create (anArgs: array of const);
begin
    try
        inherited Create (NIL);
        f_IconIndex := 0;
        f_ColCount := 4;
        Left := Mouse.CursorPos.X + 8;
        Top := Mouse.CursorPos.Y - 8;
    except on E: Exception do
        _raise (['Create',ERR_TICONDIALOG_CREATE,E],
                ['{FBC162AA-B7C6-41E5-8875-115060003585}']);
    end;
end;

destructor TIconDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TICONDIALOG_DESTROY,E],
                ['{F05F0960-1A44-4ED2-A47D-31C3AFA5E981}']);
    end;
end;

procedure TIconDialog.GetData (anArgs: array of const);
var
    I   : Integer;
    btn : TsSpeedButton;
begin
    try
        { первый параметр - набор иконок }
        if notEmpty (0,anArgs) then
            f_Icons := toObject (anArgs [0]) as TsAlphaImageList;
        if not Assigned (Icons) or ( Icons.Count = 0 ) then
            raise Exception.Create (ERR_TICONDIALOG_INVALID_ICONS);
        { второй параметр - количество столбцов }
        if notEmpty (1,anArgs) then
            ColCount := toInteger (anArgs [1]);
        if ( ColCount = 0 ) or ( ColCount > Icons.Count ) then
        begin
            if ( Icons.Count <= 4 ) then
                ColCount := Icons.Count
            else
                ColCount := 4;
        end;
        { очищаем }
        while ( pnlBackGround.ComponentCount > 0 ) do
            pnlBackGround.Components [0].Free;
        { строим список }
        Width := ColCount * ( Icons.Width + 4 ) + 12;
        Height := ( Icons.Count div ColCount + 1 ) * ( Icons.Height + 4 ) + 4;
        for I := 0 to Icons.Count - 1 do
        begin
            btn := TsSpeedButton.Create (pnlBackGround);
            btn.Parent := pnlBackGround;
            btn.Width := Icons.Width;
            btn.Height := Icons.Height;
            if ( I mod ColCount = 0 ) then
                btn.Left := 6
            else
                btn.Left := TControl (pnlBackGround.Components [I-1]).Left +
                            TControl (pnlBackGround.Components [I-1]).Width + 4;
            btn.Top := ( I div ColCount ) * ( btn.Height + 4 ) + 4;
            btn.Images := Icons;
            btn.ImageIndex := I;
            btn.Tag := I;
            btn.Grayed := TRUE;
            btn.SkinData.SkinSection := 'WEBBUTTON';
            btn.OnClick := OnIconClick;
        end;
        pnlBackground.OnClick := OnBackgroundClick;
    except on E: Exception do
        _raise (['GetData',ERR_TICONDIALOG_GET_DATA,E],
                ['{EDF8CB18-3D0B-429F-A4AC-3E60C431C3E0}']);
    end;
end;

procedure TIconDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrIcon : ModalResult := mrOk;
            else     ModalResult := anIndex;
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TICONDIALOG_SET_DATA,E],
                ['{60E77739-E343-47BD-81FE-C4C1587B61D5}']);
    end;
end;

procedure TIconDialog.OnIconClick (Sender: TObject);
begin
    IconIndex := TControl (Sender).Tag;
    SetData (mrIcon);
end;

procedure TIconDialog.OnBackgroundClick (Sender: TObject);
begin
    SetData (mrCancel);
end;

class function TIconDialog.Open (anArgs: array of const; out anIconIndex: Integer) : Integer;
begin
    try
        with Create (anArgs) do
        try
            GetData (anArgs);
            Result := ShowModal;
            if ( Result = mrOk ) then
                anIconIndex := IconIndex;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TICONDIALOG_OPEN,E],
                ['{34C2FCF7-810A-4E98-8F42-CFC8745DE07A}']);
    end;
end;

class function TIconDialog.Execute (anArgs: array of const; out anIconIndex: Integer) : Boolean;
begin
    try
        Result := ( Open (anArgs,anIconIndex) = mrIcon );
    except on E: Exception do
        _raise (['Execute',ERR_TICONDIALOG_EXECUTE,E],
                ['{92D74D39-C606-473D-B392-DABDDB7815D1}']);
    end;
end;


end.
