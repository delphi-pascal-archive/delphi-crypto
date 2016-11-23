unit ac3dNewClass;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF DELPHI6}Variants, {$ENDIF}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, sBitBtn, sComboBox, sEdit, sSkinProvider;

type
  TFormNewThirdClass = class(TForm)
    sEdit1: TsEdit;
    sComboBox1: TsComboBox;
    sBitBtn1: TsBitBtn;
    sBitBtn2: TsBitBtn;
    sSkinProvider1: TsSkinProvider;
    procedure FormCreate(Sender: TObject);
  end;

var
  FormNewThirdClass: TFormNewThirdClass;

implementation

uses sDefaults;

{$R *.dfm}

procedure TFormNewThirdClass.FormCreate(Sender: TObject);
var
  i : integer;
begin
  sEdit1.Text := '';
  for i := 0 to Length(acThirdCaptions) - 1 do sComboBox1.Items.Add(acThirdCaptions[i]);
  sComboBox1.ItemIndex := 0;
end;

end.
