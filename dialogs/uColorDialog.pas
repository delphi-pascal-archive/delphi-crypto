unit uColorDialog;
{******************************************************************************}
{*  Color Dialog Unit                                                         *}
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
    sPanel, 
    Kernel, DialogClasses,
    Utils, Strings, Versions, VarRecs,
    EClasses;

const
    mrColor = 13;

type
    CColorDialog = class of TColorDialog;
    PColorDialog = ^TColorDialog;
    TColorDialog = class (TForm)
        pnlBackGround: TsPanel;
        imgColor: TImage;
        Colors: TsColorsPanel;
        procedure FormKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure ColorsChange (Sender: TObject);
        procedure imgColorMouseUp (Sender: TObject; Button: TMouseButton;
                                   Shift: TShiftState; X, Y: Integer);
    public
        class procedure _raise (anArgs: array of const;
                                const anEGUID: String = ''); overload; virtual;
        class procedure _raise (anArgs: array of const;
                                anEGUID: array of const); overload; virtual;
    private
        f_Color: TColor;
    public
        procedure GetData (anArgs: array of const); overload; virtual;
        procedure SetData (anIndex: Integer); overload; virtual;
    public
        constructor Create (anArgs: array of const); virtual;
        destructor Destroy; override;
        class function Open (anArgs: array of const; out aColor: TColor) : Integer; virtual;
        class function Execute (anArgs: array of const; out aColor: TColor) : Boolean; virtual;
    public
        property Color: TColor read f_Color write f_Color;
    end;

resourcestring
    ERR_TCOLORDIALOG_CREATE   = 'Ошибка создания!';
    ERR_TCOLORDIALOG_DESTROY  = 'Ошибка уничтожения!';
    ERR_TCOLORDIALOG_GET_DATA = 'Ошибка чтения данных!';
    ERR_TCOLORDIALOG_SET_DATA = 'Ошибка записи данных!';
    ERR_TCOLORDIALOG_OPEN     = 'Ошибка открытия!';
    ERR_TCOLORDIALOG_EXECUTE  = 'Ошибка выполнения!';

implementation

{$R *.dfm}

class procedure TColorDialog._raise (anArgs: array of const;
                                     const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TColorDialog._raise (anArgs: array of const;
                                     anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TColorDialog.Create (anArgs: array of const);
begin
    try
        inherited Create (NIL);
        f_Color := clBlack;
        Left := Mouse.CursorPos.X + 8;
        Top := Mouse.CursorPos.Y - 8;
    except on E: Exception do
        _raise (['Create',ERR_TCOLORDIALOG_CREATE,E],
                ['{951ECCE4-4BBB-4040-95FF-E2F885A191FF}']);
    end;
end;

destructor TColorDialog.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TCOLORDIALOG_DESTROY,E],
                ['{BE8C2E8A-A2C9-4F28-A7FA-408829C33267}']);
    end;
end;

procedure TColorDialog.FormKeyDown (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if ( Key = VK_ESCAPE ) then
        Close;
end;

procedure TColorDialog.GetData (anArgs: array of const);
begin
    try
        { первый параметр - цвет }
        if notEmpty (0,anArgs) then
            Color := TColor ( toInt64 (anArgs [0]) );
    except on E: Exception do
        _raise (['GetData',ERR_TCOLORDIALOG_GET_DATA,E],
                ['{43244081-CB22-453E-BCCD-58A5BE1F8890}']);
    end;
end;

procedure TColorDialog.imgColorMouseUp (Sender: TObject; Button: TMouseButton;
                                        Shift: TShiftState; X, Y: Integer);
begin
    if ( Button = mbLeft ) then
    begin
        Color := imgColor.Canvas.Pixels [X,Y];
        SetData (mrColor);
    end
    else
        Close;
end;

procedure TColorDialog.ColorsChange (Sender: TObject);
begin
    Color := Colors.ColorValue;
    SetData (mrColor);
end;

procedure TColorDialog.SetData (anIndex: Integer);
begin
    try
        case anIndex of
            mrColor : ModalResult := mrOk;
            else      ModalResult := anIndex;
        end;
    except on E: Exception do
        _raise (['SetData',ERR_TCOLORDIALOG_SET_DATA,E],
                ['{076B912B-38CC-403E-8C60-7B859CCD2300}']);
    end;
end;

class function TColorDialog.Open (anArgs: array of const; out aColor: TColor) : Integer;
begin
    try
        with Create (anArgs) do
        try
            GetData (anArgs);
            Result := ShowModal;
            if ( Result = mrOk ) then
                aColor := Color;
        finally
            Free;
        end;
    except on E: Exception do
        _raise (['Open',ERR_TCOLORDIALOG_OPEN,E],
                ['{E777504F-6FA5-4869-A90C-2D9ECE718232}']);
    end;
end;

class function TColorDialog.Execute (anArgs: array of const; out aColor: TColor) : Boolean;
begin
    try
        Result := ( Open (anArgs,aColor) = mrColor );
    except on E: Exception do
        _raise (['Execute',ERR_TCOLORDIALOG_EXECUTE,E],
                ['{6C54FB3B-BE23-45D2-97C9-9AE69F27788B}']);
    end;
end;

end.
