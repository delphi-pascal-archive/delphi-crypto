unit SELpfEditDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TSELpfEditDlg = class(TForm)
    Apply: TButton;
    Cancel: TButton;
    RadioGroup2: TRadioGroup;
    NoneFiltering: TRadioButton;
    LowPassFiltering: TRadioButton;
    SubstractNoise: TRadioButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    BandWidth: TEdit;
    Label2: TLabel;
    FrequencyResolution: TComboBox;
    Label3: TLabel;
    SubstractionNoiseDegree: TComboBox;
    CheckOvershoot: TCheckBox;
    Label4: TLabel;
    SuppressionDegree: TComboBox;
    Help: TButton;
    procedure ApplyClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure NoneFilteringClick(Sender: TObject);
    procedure LowPassFilteringClick(Sender: TObject);
    procedure SubstractNoiseClick(Sender: TObject);
    procedure CheckOvershootClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TSELpfEditDlg;

implementation

{$R *.DFM}




procedure TSELpfEditDlg.ApplyClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSELpfEditDlg.CancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSELpfEditDlg.FormActivate(Sender: TObject);
begin
  if NoneFiltering.Checked then  // Режим - нет фильтрация
  begin
    BandWidth.Enabled := false;
    FrequencyResolution.Enabled := false;
    SubstractionNoiseDegree.Enabled := false;
  end
  else if LowPassFiltering.Checked then  // Режим - фильтрация нижних частот
       begin
         BandWidth.Enabled := true;
         SubstractionNoiseDegree.Enabled := false;
       end
       else
       begin                             // Режим - вычитание шума
         BandWidth.Enabled := false;
         SubstractionNoiseDegree.Enabled := true;
       end;
  if CheckOvershoot.Checked then        // Есть ли признак подавлени выбросов
    SuppressionDegree.Enabled := true
  else
    SuppressionDegree.Enabled := false;
end;

procedure TSELpfEditDlg.NoneFilteringClick(Sender: TObject);
begin
  BandWidth.Enabled := false;
  FrequencyResolution.Enabled := false;
  SubstractionNoiseDegree.Enabled := false;
end;

procedure TSELpfEditDlg.LowPassFilteringClick(Sender: TObject);
begin
  BandWidth.Enabled := true;
  SubstractionNoiseDegree.Enabled := false;
  if not FrequencyResolution.Enabled then
    FrequencyResolution.Enabled := true;
end;

procedure TSELpfEditDlg.SubstractNoiseClick(Sender: TObject);
begin
  BandWidth.Enabled := false;
  SubstractionNoiseDegree.Enabled := true;
  if not FrequencyResolution.Enabled then
    FrequencyResolution.Enabled := true;
end;

procedure TSELpfEditDlg.CheckOvershootClick(Sender: TObject);
begin
  if CheckOvershoot.Checked then
    SuppressionDegree.Enabled := true
  else
    SuppressionDegree.Enabled := false;  
end;

procedure TSELpfEditDlg.HelpClick(Sender: TObject);
begin
  Application.HelpFile := 'LowPassFilterHelp.hlp';
  Application.HelpContext(40);
end;

end.
 