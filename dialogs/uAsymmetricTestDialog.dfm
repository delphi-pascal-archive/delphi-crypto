object AsymmetricTestContent: TAsymmetricTestContent
  Left = 0
  Top = 0
  Width = 350
  Height = 370
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
  object tabs: TsPageControl
    Left = 0
    Top = 0
    Width = 350
    Height = 370
    ActivePage = tbRSA
    Align = alClient
    TabOrder = 0
    SkinData.SkinSection = 'PAGECONTROL'
    object tbRSA: TsTabSheet
      Caption = 'RSA'
      SkinData.CustomColor = False
      SkinData.CustomFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlRSAControls: TsPanel
        AlignWithMargins = True
        Left = 104
        Top = 3
        Width = 235
        Height = 333
        Margins.Left = 0
        Align = alClient
        ParentColor = True
        TabOrder = 0
        SkinData.SkinSection = 'BARPANEL'
        object btGenerateKeysRSA: TsSpeedButton
          Left = 208
          Top = 67
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
          OnClick = btGenerateKeysRSAClick
          SkinData.SkinSection = 'WEBBUTTON'
          Grayed = True
        end
        object KeyE: TsRichEdit
          AlignWithMargins = True
          Left = 9
          Top = 65
          Width = 193
          Height = 32
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            'ad22ee94ab9f5d2ae56d7078'
            'fb5'
            '3'
            'fab57c3548413ed592d9216c'
            '1fa'
            '2c'
            'a45edab')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 1
          Text = 
            'ad22ee94ab9f5d2ae56d7078'#13#10'fb5'#13#10'3'#13#10'fab57c3548413ed592d9216c'#13#10'1fa'#13 +
            #10'2c'#13#10'a45edab'#13#10
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
        object KeyN: TsRichEdit
          AlignWithMargins = True
          Left = 9
          Top = 9
          Width = 193
          Height = 48
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '9caaddf1fd250b22e33c4764'
            '4c5f'
            '7'
            'fa255f0301d6e14c27fa2f89'
            'ceb5'
            'e'
            'a1b7489908ef5b89f5a6c3fe'
            '7b04'
            '0'
            '06c4065f5d8eb5cde956a938'
            'a2c'
            'fc'
            '1706bf37884e22b9f8a4c71a'
            'eb4'
            '5a'
            'b48f5c161f9d35f6b1c9aef3'
            'aa8a'
            '5'
            'b404ba2475064f2920e08209'
            '25c'
            '7'
            'ef571c30bfbf75415d2042e2'
            'c29'
            'ef'
            '5ff4d623d0539ac3cea74841'
            'd')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 0
          Text = 
            '9caaddf1fd250b22e33c4764'#13#10'4c5f'#13#10'7'#13#10'fa255f0301d6e14c27fa2f89'#13#10'ceb' +
            '5'#13#10'e'#13#10'a1b7489908ef5b89f5a6c3fe'#13#10'7b04'#13#10'0'#13#10'06c4065f5d8eb5cde956a93' +
            '8'#13#10'a2c'#13#10'fc'#13#10'1706bf37884e22b9f8a4c71a'#13#10'eb4'#13#10'5a'#13#10'b48f5c161f9d35f6b' +
            '1c9aef3'#13#10'aa8a'#13#10'5'#13#10'b404ba2475064f2920e08209'#13#10'25c'#13#10'7'#13#10'ef571c30bfbf' +
            '75415d2042e2'#13#10'c29'#13#10'ef'#13#10'5ff4d623d0539ac3cea74841'#13#10'd'#13#10
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
        object KeyD: TsRichEdit
          AlignWithMargins = True
          Left = 9
          Top = 105
          Width = 193
          Height = 48
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '1e88fd650a4f3847100eded5'
            '0d8'
            '5'
            '92a0918571c6dcd497b5e30a'
            'e9c'
            'f'
            'c8e63d8f222521eafd50f9f7'
            'fab8'
            '80'
            '7ecc24033c2eed7a048a8b98'
            'f72'
            'b'
            '29ea9340472cd287e39a249e'
            'cb7'
            '7'
            '34c8708599b251ea3489e250'
            '8fd'
            '1'
            'eae2505d52726ced7dc051b6'
            'a4'
            '05'
            'ebcf44fcfaaa8e09ef8d8dad'
            'fc9b'
            '7'
            '8b97ee815456f8fe3a95e622'
            '28a'
            '13')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 2
          Text = 
            '1e88fd650a4f3847100eded5'#13#10'0d8'#13#10'5'#13#10'92a0918571c6dcd497b5e30a'#13#10'e9c'#13 +
            #10'f'#13#10'c8e63d8f222521eafd50f9f7'#13#10'fab8'#13#10'80'#13#10'7ecc24033c2eed7a048a8b98' +
            #13#10'f72'#13#10'b'#13#10'29ea9340472cd287e39a249e'#13#10'cb7'#13#10'7'#13#10'34c8708599b251ea3489' +
            'e250'#13#10'8fd'#13#10'1'#13#10'eae2505d52726ced7dc051b6'#13#10'a4'#13#10'05'#13#10'ebcf44fcfaaa8e09' +
            'ef8d8dad'#13#10'fc9b'#13#10'7'#13#10'8b97ee815456f8fe3a95e622'#13#10'28a'#13#10'13'#13#10
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
          Top = 161
          Width = 217
          Height = 48
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '4ceea1a89464a5d2f89e078953c'
            'af77'
            '63'
            '65898a5bd5e8e448c65da26ff98'
            '900c'
            'd'
            '08061ef446c69b48dc4609ed865'
            '4a64'
            '6'
            'd708262cb8409ac27c4a49adfed'
            '47a8'
            '5'
            'ad429ed75077578e4c273c61e2c'
            '3b4'
            '6b'
            'eb472f0a345a05d61a7eaaad8a6'
            '3e0b'
            '3'
            'd4952f27c4081329e')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 3
          Text = 
            '4ceea1a89464a5d2f89e078953c'#13#10'af77'#13#10'63'#13#10'65898a5bd5e8e448c65da26ff' +
            '98'#13#10'900c'#13#10'd'#13#10'08061ef446c69b48dc4609ed865'#13#10'4a64'#13#10'6'#13#10'd708262cb8409' +
            'ac27c4a49adfed'#13#10'47a8'#13#10'5'#13#10'ad429ed75077578e4c273c61e2c'#13#10'3b4'#13#10'6b'#13#10'e' +
            'b472f0a345a05d61a7eaaad8a6'#13#10'3e0b'#13#10'3'#13#10'd4952f27c4081329e'#13#10
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
          Top = 217
          Width = 217
          Height = 32
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          Lines.Strings = (
            '7672cfc27a41d501aa4c41baabf'
            '4525a'
            '7'
            'c455fc800000000000000000000'
            '0000')
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 4
          Text = 
            '7672cfc27a41d501aa4c41baabf'#13#10'4525a'#13#10'7'#13#10'c455fc8000000000000000000' +
            '00'#13#10'0000'#13#10
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
          Top = 257
          Width = 217
          Height = 67
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Align = alClient
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ScrollBars = ssVertical
          TabOrder = 5
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
      object pnlRSALabels: TsPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 101
        Height = 333
        Margins.Right = 0
        Align = alLeft
        Padding.Left = 8
        Padding.Right = 8
        ParentColor = True
        TabOrder = 1
        SkinData.SkinSection = 'BARPANEL'
        object lbE: TsLabel
          Left = 17
          Top = 72
          Width = 73
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'e'
        end
        object lbN: TsLabel
          Left = 17
          Top = 24
          Width = 73
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'n'
        end
        object lbD: TsLabel
          Left = 17
          Top = 120
          Width = 73
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'd'
        end
        object lbSeed: TsLabel
          Left = 17
          Top = 176
          Width = 73
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1089#1084#1077#1097#1077#1085#1080#1077
        end
        object lbPlain: TsLabel
          Left = 16
          Top = 224
          Width = 74
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1089#1086#1086#1073#1097#1077#1085#1080#1077
        end
        object lbCipher: TsLabel
          Left = 8
          Top = 258
          Width = 82
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1096#1080#1092#1088#1086#1090#1077#1082#1089#1090
        end
      end
    end
  end
end
