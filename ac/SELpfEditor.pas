unit SELpfEditor;

interface
uses
  DesignIntf, DesignEditors,
  SELowPassFilter, SELpfEditDlg, Controls, SysUtils;

type
  TSELpfEditor =class(TComponentEditor)
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): String; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
procedure Register;

implementation

// Регистрировать редактор компонента
procedure Register;
begin
  RegisterComponentEditor(TLowPassFilter, TSELpfEditor);
end;

function TSELpfEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TSELpfEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit LowPassFilter';
    else
      Result := 'Undefined Menu';
  end;
end;

//
procedure TSELpfEditor.ExecuteVerb(Index: Integer);
var
  i:Integer;
  strTemp: string;
  Dialog: TSELpfEditDlg;

begin
  case Index of
  0:
    begin
      Dialog := TSELpfEditDlg.Create(nil);
      Dialog.Caption := Component.Owner.Name + '.' + Component.Name +' - ' + Dialog.Caption;
     // Режим работы
      if (Component as TLowPassFilter).Mode = mdNoneFiltering then
        Dialog.NoneFiltering.Checked := true
      else if (Component as TLowPassFilter).Mode = mdLowPassFilter then
             Dialog.LowPassFiltering.Checked := true
           else
             Dialog.SubstractNoise.Checked := true;
     // Полоса фильтра
      Dialog.BandWidth.Text :=FloatToStr( (Component as TLowPassFilter).BandWidth);
     // Масштаб разрешения по частоте
      for i := 1 to 10 do
      begin
        Dialog.FrequencyResolution.Items[i - 1] := IntToStr(i);
        if i = (Component as TLowPassFilter).FrequencyResolution then
          Dialog.FrequencyResolution.ItemIndex := i - 1;
      end;
     // Признак подавления выбросов
      Dialog.CheckOvershoot.Checked := (Component as TLowPassFilter).OverShoot;
     // Степень подавления выбросов и степень вычитания шума
      for i := 0 to 2 do
      begin
        case i of
          0: strTemp := 'Small';
          1: strTemp := 'Medium';
          2: strTemp := 'Large';
        end;
        Dialog.SuppressionDegree.Items[i] := strTemp;
        Dialog.SubstractionNoiseDegree.Items[i] := strTemp;
        if Ord((Component as TLowPassFilter).SuppressionDegree) = i then
          Dialog.SuppressionDegree.ItemIndex := i;
        if Ord((Component as TLowPassFilter).SubstractionNoiseDegree) = i then
          Dialog.SubstractionNoiseDegree.ItemIndex := i;
      end;

     // Отобразить диалог
      if Dialog.ShowModal = mrOK then
      begin
        // Полоса
        (Component as TLowPassFilter).BandWidth := StrToFloat(Dialog.BandWidth.Text);
        // Степень подавления выбросов
        case  Dialog.SuppressionDegree.ItemIndex of
          0: (Component as TLowPassFilter).SuppressionDegree := edSmall;
          1: (Component as TLowPassFilter).SuppressionDegree := edMedium;
          2: (Component as TLowPassFilter).SuppressionDegree := edLarge;
        end;
        // Разрешение
        (Component as TLowPassFilter).FrequencyResolution := Dialog.FrequencyResolution.ItemIndex + 1;
        // Признак устранения выбросов
        (Component as TLowPassFilter).Overshoot := Dialog.CheckOvershoot.Checked;
        // Степень вычитания шума
        case Dialog.SubstractionNoiseDegree.ItemIndex of
          0: (Component as TLowPassFilter).SubstractionNoiseDegree := edSmall;
          1: (Component as TLowPassFilter).SubstractionNoiseDegree := edMedium;
          2: (Component as TLowPassFilter).SubstractionNoiseDegree := edLarge;
        end;
        // Режим работы фильтра
        if Dialog.NoneFiltering.Checked then
          (Component as TLowPassFilter).Mode := mdNoneFiltering
        else if Dialog.LowPassFiltering.Checked then
               (Component as TLowPassFilter).Mode := mdLowPassFilter
             else
               (Component as TLowPassFilter).Mode := mdSubtractionNoise;

        Designer.Modified;
      end;
      Dialog.Free;

    end;
  end;
end;

end.
