unit acThumbForm;
{$I sDefs.inc}
{.$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ExtCtrls{$IFDEF LOGGED}, sDebugMsgs{$ENDIF};

type
  TThumbForm = class(TForm)
    procedure FormHide(Sender: TObject);
  private
    FScaledWnd: HWND;
    Thumbnail: THandle;
    procedure SetScaledWnd(const Value: HWND);
    property ScaledWnd: HWND read FScaledWnd write SetScaledWnd;
    function ScaledRect: TRect;
    function CenterPoint: TPoint;
    procedure OnColorChanged(var Message: TMessage); message $0320; // WM_DWMCOLORIZATIONCOLORCHANGED;
  public
    ScaleFactor : integer;
    constructor Create(AOwner:TComponent); override;
    procedure UpdateScaledWnd;
    procedure UpdateThumbnail;
    procedure WndProc(var Message : TMessage); override;
  end;

  TDWMThumbnailProperties = packed record
    dwFlags: DWORD;
    rcDestination: TRect;
    rcSource: TRect;
    opacity: Byte;
    fVisible: BOOL;
    fSourceClientAreaOnly: BOOL;
  end;
  
function DwmUnregisterThumbnail(hThumbnailId: THandle): HResult;
function DwmGetColorizationColor(out pcrColorization: DWORD; out pfOpaqueBlend: BOOL): HResult;
function DwmRegisterThumbnail(hwndDestination: HWND; hwndSource: HWND; out phThumbnailId: THandle): HResult;
function DwmUpdateThumbnailProperties(hThumbnailId: THandle; const ptnProperties: TDWMThumbnailProperties): HResult;

implementation

uses sConst, sSKinProvider;

{$R *.dfm}

var
  _DwmUnregisterThumbnail: function(hThumbnailId: THandle): HResult; stdcall;
  _DwmGetColorizationColor: function(out pcrColorization: DWORD; out pfOpaqueBlend: BOOL): HResult; stdcall;
  _DwmRegisterThumbnail: function(hwndDestination: HWND; hwndSource: HWND; out phThumbnailId: THandle): HResult; stdcall;
  _DwmUpdateThumbnailProperties: function(hThumbnailId: THandle; const ptnProperties: TDWMThumbnailProperties): HResult; stdcall;

function ARGBToColor(c: Cardinal): TColor;
begin
  Result := (c and $ff) shl 16;
  Result := Result or (c and $ff00);
  Result := Result or ((c and $ff0000) shr 16);
end;

function TThumbForm.CenterPoint: TPoint;
begin
  Result.X := Left + (Width div 2);
  Result.Y := Top + (Height div 2);
end;

constructor TThumbForm.Create(AOwner: TComponent);
begin
  Thumbnail := 0;
  inherited;
end;

procedure TThumbForm.FormHide(Sender: TObject);
begin
  DwmUnregisterThumbnail(Thumbnail);
end;

procedure TThumbForm.OnColorChanged(var Message: TMessage);
var
  col: Cardinal;
  opaque: LongBool;
begin
  DwmGetColorizationColor(col, opaque);
  Color := ARGBToColor(col);
end;

function TThumbForm.ScaledRect: TRect;
var
  r: TRect;
begin
  Result.Left := Left + ((Width - (Width div ScaleFactor)) div 2);
  Result.Top := Top + ((Height - (Height div ScaleFactor)) div 2);
  Result.Right := Result.Left + (Width div ScaleFactor);
  Result.Bottom := Result.Top + (Height div ScaleFactor);

  GetWindowRect(ScaledWnd, r);
  Dec(Result.Left, r.Left);
  Dec(Result.Top, r.Top);
  Dec(Result.Right, r.Left);
  Dec(Result.Bottom, r.Top);
end;

procedure TThumbForm.SetScaledWnd(const Value: HWND);
begin
  if FScaledWnd <> Value then begin
    if Thumbnail <> 0 then DwmUnregisterThumbnail(Thumbnail);
    DwmRegisterThumbnail(Handle, Value, Thumbnail);
  end;
  FScaledWnd := Value;
end;

procedure TThumbForm.UpdateScaledWnd;
var
  h: HWND;
  pnt: TPoint;
  r: TRect;
begin
  if ScaleFactor = 0 then Exit;
  h := Handle;
  pnt := CenterPoint;
  repeat
    h := GetWindow(h, GW_HWNDNEXT);
{$IFDEF D2007}    
    if Application.MainFormOnTaskBar and (h = Application.MainForm.Handle) then h := GetWindow(h, GW_HWNDNEXT);
{$ENDIF}    

    GetWindowRect(h, r);
  until (h = 0) or (IsWindowVisible(h) and PtInRect(r, pnt));
  ScaledWnd := h;
end;

procedure TThumbForm.UpdateThumbnail;
var
  prop: TDWMThumbnailProperties;
  col: Cardinal;
  opaque: LongBool;
begin
  if ScaleFactor = 0 then Exit;
  DwmGetColorizationColor(col, opaque);
  Color := ARGBToColor(col);
  UpdateScaledWnd;

  prop.dwFlags := 1 {DWM_TNP_RECTDESTINATION} or 2 {DWM_TNP_RECTSOURCE};
  prop.rcDestination := ClientRect;
  prop.rcSource := ScaledRect;
  DwmUpdateThumbnailProperties(Thumbnail, prop);
end;

procedure TThumbForm.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  inherited;
end;

function DwmUnregisterThumbnail(hThumbnailId: THandle): HResult;
begin
  if Assigned(_DwmUnregisterThumbnail) then
    Result := _DwmUnregisterThumbnail(hThumbnailId)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmUnregisterThumbnail := GetProcAddress(hDWMAPI, 'DwmUnregisterThumbnail'); // Do not localize
      if Assigned(_DwmUnregisterThumbnail) then
        Result := _DwmUnregisterThumbnail(hThumbnailId);
    end;
  end;
end;

function DwmGetColorizationColor(out pcrColorization: DWORD; out pfOpaqueBlend: BOOL): HResult;
begin
  if Assigned(_DwmGetColorizationColor) then
    Result := _DwmGetColorizationColor(pcrColorization, pfOpaqueBlend)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmGetColorizationColor := GetProcAddress(hDWMAPI, 'DwmGetColorizationColor'); // Do not localize
      if Assigned(_DwmGetColorizationColor) then
        Result := _DwmGetColorizationColor(pcrColorization, pfOpaqueBlend);
    end;
  end;
end;

function DwmRegisterThumbnail(hwndDestination: HWND; hwndSource: HWND; out phThumbnailId: THandle): HResult;
begin
  if Assigned(_DwmRegisterThumbnail) then
    Result := _DwmRegisterThumbnail(hwndDestination, hwndSource, phThumbnailId)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then begin
      _DwmRegisterThumbnail := GetProcAddress(hDWMAPI, 'DwmRegisterThumbnail'); // Do not localize
      if Assigned(_DwmRegisterThumbnail) then Result := _DwmRegisterThumbnail(hwndDestination, hwndSource, phThumbnailId);
    end;
  end;
end;

function DwmUpdateThumbnailProperties(hThumbnailId: THandle; const ptnProperties: TDWMThumbnailProperties): HResult;
begin
  if Assigned(_DwmUpdateThumbnailProperties) then
    Result := _DwmUpdateThumbnailProperties(hThumbnailId, ptnProperties)
  else
  begin
    InitDwmApi;
    Result := E_NOTIMPL;
    if hDWMAPI > 0 then
    begin
      _DwmUpdateThumbnailProperties := GetProcAddress(hDWMAPI, 'DwmUpdateThumbnailProperties'); // Do not localize
      if Assigned(_DwmUpdateThumbnailProperties) then
        Result := _DwmUpdateThumbnailProperties(hThumbnailId, ptnProperties);
    end;
  end;
end;

end.
