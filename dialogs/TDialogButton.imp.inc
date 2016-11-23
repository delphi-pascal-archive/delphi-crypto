{******************************************************************************}
{*  TDialogButton.imp.inc                                                     *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
class procedure TDialogButton._raise (anArgs: array of const;
                                      const anEGUID: String = '');
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

class procedure TDialogButton._raise (anArgs: array of const;
                                      anEGUID: array of const);
begin
    raise EClass.Create ( _([self],anArgs), anEGUID );
end;

constructor TDialogButton.Create (anOwner: TWinControl;
                                  aCaption: String = '';
                                  anImageList: TCustomImageList = NIL;
                                  anImageIndex: Integer = dlgCustom;
                                  aModalResult: Integer = mrNone;
                                  anOnClick: TNotifyEvent = NIL;
                                  anAlign: TAlign = alRight;
                                  aLayout: TButtonLayout = blGlyphLeft);
begin
    try
        inherited Create (anOwner);
        Parent := anOwner;
        Caption := aCaption;
        Align := alRight;
        AlignWithMargins := TRUE;
        Margins.Bottom := 5;
        Margins.Left := 5;
        Margins.Right := 5;
        Margins.Top := 5;
        Images := anImageList;
        ImageIndex := ImageIndex;
        ModalResult := aModalResult;
        OnClick := anOnClick;
        Align := anAlign;
        Layout := aLayout;
        Resize;
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGBUTTON_CREATE,E],
                ['{366A0B63-4F7A-46D3-8E91-0A0714376342}']);
    end;
end;

constructor TDialogButton.Create (anOwner: TWinControl;
                                  anImageList: TCustomImageList;
                                  anOnClick: TNotifyEvent;
                                  anArgs: array of const);
begin
    try
        Create (anOwner,'',anImageList,dlgCustom,mrNone,anOnClick);
        { первый параметр - заголовок }
        if notEmpty (0,anArgs) then
            Caption := toString (anArgs [0]);
        { второй параметр - иконка }
        if notEmpty (1,anArgs) then
            ImageIndex := toInteger (anArgs [1]);
        { третий параметр - результат }
        ModalResult := mrNone;
        if notEmpty (2,anArgs) then
            ModalResult := toInteger (anArgs [2]);
        { четвертый параметр - выравнивание }
        if notEmpty (3,anArgs) then
            Align := toButtonAlign (anArgs [3]);
        { пятый параметр - выравнивание иконки }
        if notEmpty (4,anArgs) then
            Layout := toButtonLayout (anArgs [4]);
        Resize;
    except on E: Exception do
        _raise (['Create',ERR_TDIALOGBUTTON_CREATE,E],
                ['{F28E1C8D-DA29-4BA3-9004-473FA018C3B8}']);
    end;
end;

destructor TDialogButton.Destroy;
begin
    try
        inherited Destroy;
    except on E: Exception do
        _raise (['Destroy',ERR_TDIALOGBUTTON_DESTROY,E],
                ['{BC9F7330-7CA8-41F6-BA1E-541B1F440F9C}']);
    end;
end;

procedure TDialogButton.Resize;
begin
    try
        Width := Round ( Length (Caption) * Font.Size + 24 );
        if Assigned (Images) and ( ImageIndex > btnCustom ) then
            Width := Width + Images.Width;
        Repaint;
    except on E: Exception do
        _raise (['Resize',ERR_TDIALOGBUTTON_RESIZE,E],
                ['{EB51C9FE-6CA7-45D0-BABB-B3682835A612}']);
    end;
end;

class function TDialogButton.toButtonAlign (const aValue: TVarRec) : TAlign;
const
    ALIGN_STRING : array [alNone..alCustom] of String = (
        'alNone',
        'alTop',
        'alBottom',
        'alLeft',
        'alRight',
        'alClient',
        'alCustom'
    );
var
    I : TAlign;
begin
    Result := alNone;
    try
        for I := Low (ALIGN_STRING) to High (ALIGN_STRING) do
            if ( toString (aValue) = ALIGN_STRING [I] ) then
            begin
                Result := I;
                Break;
            end;
    except
        Result := alNone;
    end;
end;

class function TDialogButton.toButtonLayout (const aValue: TVarRec) : TButtonLayout;
const
    BUTTON_LAYOUT_STRING : array [blGlyphLeft..blGlyphBottom] of String = (
        'blGlyphLeft',
        'blGlyphRight',
        'blGlyphTop',
        'blGlyphBottom'
    );
var
    I : TButtonLayout;
begin
    Result := blGlyphLeft;
    try
        for I := Low (BUTTON_LAYOUT_STRING) to High (BUTTON_LAYOUT_STRING) do
            if ( toString (aValue) = BUTTON_LAYOUT_STRING [I] ) then
            begin
                Result := I;
                Break;
            end;
    except
        Result := blGlyphLeft;
    end;
end;
