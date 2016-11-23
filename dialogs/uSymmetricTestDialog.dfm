object SymmetricTestContent: TSymmetricTestContent
  Left = 0
  Top = 0
  Width = 350
  Height = 239
  Color = 2169368
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = 14603725
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = [fsBold]
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  TabStop = True
  OnResize = FrameResize
  object pnlBackGround: TsPanel
    Left = 0
    Top = 0
    Width = 350
    Height = 239
    Align = alClient
    ParentColor = True
    TabOrder = 0
    SkinData.SkinSection = 'PANEL_LOW'
    object pnlLabels: TsPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 101
      Height = 231
      Margins.Right = 0
      Align = alLeft
      Padding.Left = 8
      Padding.Right = 8
      ParentColor = True
      TabOrder = 0
      SkinData.SkinSection = 'BARPANEL'
      object lbKey: TsLabel
        Left = 17
        Top = 12
        Width = 73
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1082#1083#1102#1095
      end
      object lbSeed: TsLabel
        Left = 17
        Top = 89
        Width = 73
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1089#1084#1077#1097#1077#1085#1080#1077
      end
      object lbPlain: TsLabel
        Left = 16
        Top = 129
        Width = 74
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1089#1086#1086#1073#1097#1077#1085#1080#1077
      end
      object lbCipher: TsLabel
        Left = 8
        Top = 169
        Width = 82
        Height = 16
        Alignment = taRightJustify
        AutoSize = False
        Caption = #1096#1080#1092#1088#1086#1090#1077#1082#1089#1090
      end
    end
    object pnlControls: TsPanel
      AlignWithMargins = True
      Left = 105
      Top = 4
      Width = 241
      Height = 231
      Margins.Left = 0
      Align = alClient
      ParentColor = True
      TabOrder = 1
      SkinData.SkinSection = 'BARPANEL'
      object btGenerateKey: TsSpeedButton
        Left = 208
        Top = 9
        Width = 24
        Height = 24
        Glyph.Data = {
          C6010000424DC60100000000000036000000280000000A0000000A0000000100
          2000000000009001000000000000000000000000000000000000001EC8FF001E
          C8FF001EC8FF001EC8FF001EC8FF001EC8FF001EC8FFFFFFFF00FFFFFF00FFFF
          FF00001EC8FF3A49EDFF3846EBFF3541EAFF323DE9FF001EC8FFFFFFFF00FFFF
          FF00FFFFFF00FFFFFF00001EC8FF3B4BF0FF3A49EDFF3744ECFF3440EAFF001E
          C8FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00001EC8FF3D4FF0FF3B4CEEFF3947
          ECFF3642EBFF142BD7FF001EC8FFFFFFFF00FFFFFF00FFFFFF00001EC8FF4053
          F0FF3C4EF1FF3A4AEFFF3847EDFF3643EBFF142BD7FF001EC8FFFFFFFF00FFFF
          FF00001EC8FF001EC8FF001EC8FF1731DAFF3A49EDFF3846EBFF3541EAFF132A
          D7FF001EC8FFFFFFFF00001EC8FFFFFFFF00FFFFFF00001EC8FF1731DAFF3A49
          EDFF3744ECFF3440EAFF132AD7FF001EC8FFFFFFFF00FFFFFF00FFFFFF00FFFF
          FF00001EC8FF1730D8FF3947ECFF3642EBFF142BD7FF001EC8FFFFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00001EC8FF162ED9FF162ED9FF001EC8FFFFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00001EC8FF001E
          C8FFFFFFFF00FFFFFF00}
        OnClick = btGenerateKeyClick
        SkinData.SkinSection = 'WEBBUTTON'
        Grayed = True
      end
      object key: TsRichEdit
        AlignWithMargins = True
        Left = 9
        Top = 9
        Width = 199
        Height = 64
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 32
        Margins.Bottom = 0
        Align = alTop
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '1e88fd650a4f3847100eded50'
          'd8'
          '5'
          '92a0918571c6dcd497b5e30ae'
          '9c'
          'f'
          'c8e63d8f222521eafd50f9f7f'
          'ab8'
          '80'
          '7ecc24033c2eed7a048a8b98f'
          '72'
          'b'
          '29ea9340472cd287e39a249ec'
          'b7'
          '7'
          '34c8708599b251ea3489e2508'
          'fd'
          '1'
          'eae2505d52726ced7dc051b6a'
          '4'
          '05'
          'ebcf44fcfaaa8e09ef8d8dadf'
          'c9b'
          '7'
          '8b97ee815456f8fe3a95e6222'
          '8a'
          '13')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        Text = 
          '1e88fd650a4f3847100eded50'#13#10'd8'#13#10'5'#13#10'92a0918571c6dcd497b5e30ae'#13#10'9c'#13 +
          #10'f'#13#10'c8e63d8f222521eafd50f9f7f'#13#10'ab8'#13#10'80'#13#10'7ecc24033c2eed7a048a8b98' +
          'f'#13#10'72'#13#10'b'#13#10'29ea9340472cd287e39a249ec'#13#10'b7'#13#10'7'#13#10'34c8708599b251ea3489' +
          'e2508'#13#10'fd'#13#10'1'#13#10'eae2505d52726ced7dc051b6a'#13#10'4'#13#10'05'#13#10'ebcf44fcfaaa8e09' +
          'ef8d8dadf'#13#10'c9b'#13#10'7'#13#10'8b97ee815456f8fe3a95e6222'#13#10'8a'#13#10'13'#13#10
        BoundLabel.Indent = 0
        BoundLabel.Font.Charset = DEFAULT_CHARSET
        BoundLabel.Font.Color = clWindowText
        BoundLabel.Font.Height = -11
        BoundLabel.Font.Name = 'Tahoma'
        BoundLabel.Font.Style = []
        BoundLabel.Layout = sclLeft
        BoundLabel.MaxWidth = 0
        BoundLabel.UseSkinColor = True
        SkinData.SkinSection = 'EDIT'
      end
      object seed: TsRichEdit
        AlignWithMargins = True
        Left = 9
        Top = 81
        Width = 223
        Height = 32
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 0
        Align = alTop
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '4ceea1a89464a5d2f89e078953ca'
          'f77'
          '63'
          '65898a5bd5e8e448c65da26ff989'
          '00c'
          'd'
          '08061ef446c69b48dc4609ed8654'
          'a64'
          '6'
          'd708262cb8409ac27c4a49adfed4'
          '7a8'
          '5'
          'ad429ed75077578e4c273c61e2c3'
          'b4'
          '6b'
          'eb472f0a345a05d61a7eaaad8a63'
          'e0b'
          '3'
          'd4952f27c4081329e')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
        Text = 
          '4ceea1a89464a5d2f89e078953ca'#13#10'f77'#13#10'63'#13#10'65898a5bd5e8e448c65da26ff' +
          '989'#13#10'00c'#13#10'd'#13#10'08061ef446c69b48dc4609ed8654'#13#10'a64'#13#10'6'#13#10'd708262cb8409' +
          'ac27c4a49adfed4'#13#10'7a8'#13#10'5'#13#10'ad429ed75077578e4c273c61e2c3'#13#10'b4'#13#10'6b'#13#10'e' +
          'b472f0a345a05d61a7eaaad8a63'#13#10'e0b'#13#10'3'#13#10'd4952f27c4081329e'#13#10
        BoundLabel.Indent = 0
        BoundLabel.Font.Charset = DEFAULT_CHARSET
        BoundLabel.Font.Color = clWindowText
        BoundLabel.Font.Height = -11
        BoundLabel.Font.Name = 'Tahoma'
        BoundLabel.Font.Style = []
        BoundLabel.Layout = sclLeft
        BoundLabel.MaxWidth = 0
        BoundLabel.UseSkinColor = True
        SkinData.SkinSection = 'EDIT'
      end
      object plain: TsRichEdit
        AlignWithMargins = True
        Left = 9
        Top = 121
        Width = 223
        Height = 32
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 0
        Align = alTop
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          '7672cfc27a41d501aa4c41baabf4'
          '525a'
          '7'
          'c455fc8000000000000000000000'
          '000')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 2
        Text = 
          '7672cfc27a41d501aa4c41baabf4'#13#10'525a'#13#10'7'#13#10'c455fc8000000000000000000' +
          '000'#13#10'000'#13#10
        BoundLabel.Indent = 0
        BoundLabel.Font.Charset = DEFAULT_CHARSET
        BoundLabel.Font.Color = clWindowText
        BoundLabel.Font.Height = -11
        BoundLabel.Font.Name = 'Tahoma'
        BoundLabel.Font.Style = []
        BoundLabel.Layout = sclLeft
        BoundLabel.MaxWidth = 0
        BoundLabel.UseSkinColor = True
        SkinData.SkinSection = 'EDIT'
      end
      object Cipher: TsRichEdit
        AlignWithMargins = True
        Left = 9
        Top = 161
        Width = 223
        Height = 61
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'cb0b9b2a6ad520c2faf8b8ba6be2'
          '66a'
          'f'
          'ceeceff83cd52f879f968aa73025'
          'f4d7')
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 3
        Text = 
          'cb0b9b2a6ad520c2faf8b8ba6be2'#13#10'66a'#13#10'f'#13#10'ceeceff83cd52f879f968aa730' +
          '25'#13#10'f4d7'#13#10
        BoundLabel.Indent = 0
        BoundLabel.Font.Charset = DEFAULT_CHARSET
        BoundLabel.Font.Color = clWindowText
        BoundLabel.Font.Height = -11
        BoundLabel.Font.Name = 'Tahoma'
        BoundLabel.Font.Style = []
        BoundLabel.Layout = sclLeft
        BoundLabel.MaxWidth = 0
        BoundLabel.UseSkinColor = True
        SkinData.SkinSection = 'EDIT'
      end
    end
  end
end
