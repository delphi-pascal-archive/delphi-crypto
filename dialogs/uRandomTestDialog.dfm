object RandomTestContent: TRandomTestContent
  Left = 0
  Top = 0
  Width = 400
  Height = 200
  TabOrder = 0
  TabStop = True
  OnResize = FrameResize
  object pnlBackground: TsPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 200
    Align = alClient
    TabOrder = 0
    SkinData.SkinSection = 'PANEL_LOW'
    object bxBuffer: TsGroupBox
      AlignWithMargins = True
      Left = 9
      Top = 4
      Width = 382
      Height = 60
      Margins.Left = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = ' '#1041#1091#1092#1077#1088' : '
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      SkinData.SkinSection = 'GROUPBOX'
      object ptBuffer: TPaintBox
        Left = 2
        Top = 17
        Width = 378
        Height = 41
        Align = alClient
        Color = clBlack
        ParentColor = False
        ExplicitLeft = 48
        ExplicitTop = 24
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object bxFFT: TsGroupBox
      AlignWithMargins = True
      Left = 9
      Top = 67
      Width = 382
      Height = 60
      Margins.Left = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = ' '#1060#1091#1088#1100#1077' - '#1072#1085#1072#1083#1080#1079' : '
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      SkinData.SkinSection = 'GROUPBOX'
      object ptFFT: TPaintBox
        Left = 2
        Top = 17
        Width = 378
        Height = 41
        Align = alClient
        Color = clBlack
        ParentColor = False
        ExplicitLeft = 48
        ExplicitTop = 24
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
    object bxEntropy: TsGroupBox
      AlignWithMargins = True
      Left = 9
      Top = 130
      Width = 382
      Height = 60
      Margins.Left = 8
      Margins.Right = 8
      Margins.Bottom = 0
      Align = alTop
      Caption = ' '#1069#1085#1090#1088#1086#1087#1080#1103' : '
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      SkinData.SkinSection = 'GROUPBOX'
      object ptEntropy: TPaintBox
        Left = 2
        Top = 17
        Width = 378
        Height = 41
        Align = alClient
        Color = clBlack
        ParentColor = False
        ExplicitLeft = 48
        ExplicitTop = 24
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
    end
  end
  object tmRandom: TTimer
    Interval = 100
    Left = 368
    Top = 8
  end
end
