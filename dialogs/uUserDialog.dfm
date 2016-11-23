object UserContent: TUserContent
  Left = 0
  Top = 0
  Width = 400
  Height = 299
  Color = 2169368
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = 14603725
  Font.Height = -13
  Font.Name = 'Arial'
  Font.Style = []
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  TabStop = True
  OnResize = FrameResize
  object tabs: TsPageControl
    Left = 0
    Top = 0
    Width = 400
    Height = 299
    ActivePage = tbMain
    Align = alClient
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = 14603725
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    SkinData.SkinSection = 'PAGECONTROL'
    object tbMain: TsTabSheet
      Caption = ' '#1087#1086#1083#1100#1079#1086#1074#1072#1090#1077#1083#1100' '
      SkinData.CustomColor = False
      SkinData.CustomFont = False
      object pnlMainLabels: TsPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 105
        Height = 262
        Margins.Right = 0
        Align = alLeft
        Padding.Left = 8
        Padding.Right = 8
        ParentColor = True
        TabOrder = 0
        SkinData.SkinSection = 'BARPANEL'
        object lbLogin: TsLabel
          Left = 59
          Top = 12
          Width = 37
          Height = 16
          Alignment = taRightJustify
          Caption = #1083#1086#1075#1080#1085
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
        object lbMail: TsLabel
          Left = 9
          Top = 44
          Width = 87
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1101#1083'. '#1087#1086#1095#1090#1072
        end
        object lbIP: TsLabel
          Left = 15
          Top = 76
          Width = 81
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'ip - '#1072#1076#1088#1077#1089
        end
        object lbDescription: TsLabel
          Left = 15
          Top = 195
          Width = 81
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1086' '#1089#1077#1073#1077
        end
        object lbPic: TsLabel
          Left = 15
          Top = 108
          Width = 81
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1072#1074#1072#1090#1072#1088
        end
      end
      object pnlMainControls: TsPanel
        AlignWithMargins = True
        Left = 108
        Top = 3
        Width = 281
        Height = 262
        Margins.Left = 0
        Align = alClient
        ParentColor = True
        TabOrder = 1
        SkinData.SkinSection = 'BARPANEL'
        DesignSize = (
          281
          262)
        object lbPort: TsLabel
          Left = 200
          Top = 75
          Width = 6
          Height = 19
          Margins.Left = 0
          Margins.Right = 0
          Alignment = taRightJustify
          Anchors = [akTop, akRight]
          Caption = ':'
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = 14603725
          Font.Height = -16
          Font.Name = 'Arial'
          Font.Style = [fsBold]
        end
        object login: TsEdit
          AlignWithMargins = True
          Left = 9
          Top = 9
          Width = 263
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          MaxLength = 32
          ParentFont = False
          TabOrder = 0
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'Tahoma'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
        end
        object mail: TsEdit
          AlignWithMargins = True
          Left = 9
          Top = 41
          Width = 263
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          MaxLength = 32
          ParentFont = False
          TabOrder = 1
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'Tahoma'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
        end
        object ip: TsEdit
          AlignWithMargins = True
          Left = 9
          Top = 73
          Width = 181
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 90
          Margins.Bottom = 0
          Align = alTop
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          MaxLength = 15
          ParentFont = False
          TabOrder = 2
          Text = '127.0.0.1'
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'Tahoma'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
        end
        object description: TsRichEdit
          AlignWithMargins = True
          Left = 9
          Top = 192
          Width = 135
          Height = 61
          Margins.Left = 8
          Margins.Top = 4
          Margins.Right = 136
          Margins.Bottom = 8
          Align = alClient
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          MaxLength = 512
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
        object pic: TsFilenameEdit
          AlignWithMargins = True
          Left = 96
          Top = 105
          Width = 176
          Height = 24
          Margins.Left = 95
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 59
          Align = alTop
          AutoSize = False
          MaxLength = 255
          TabOrder = 4
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
          GlyphMode.Blend = 0
          GlyphMode.Grayed = False
        end
        object pnlPic: TsPanel
          Left = 9
          Top = 105
          Width = 80
          Height = 80
          TabOrder = 8
          SkinData.SkinSection = 'PANEL_LOW'
          object imgPic: TImage
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 74
            Height = 74
            Margins.Left = 2
            Margins.Top = 2
            Margins.Right = 2
            Margins.Bottom = 2
            Align = alClient
            Center = True
            Picture.Data = {
              0A544A504547496D616765200E0000FFD8FFE000104A46494600010200006400
              640000FFEC00114475636B79000100040000003C0000FFEE000E41646F626500
              64C000000001FFDB0084000604040405040605050609060506090B080606080B
              0C0A0A0B0A0A0C100C0C0C0C0C0C100C0E0F100F0E0C1313141413131C1B1B1B
              1C1F1F1F1F1F1F1F1F1F1F010707070D0C0D181010181A1511151A1F1F1F1F1F
              1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F1F
              1F1F1F1F1F1F1F1F1F1F1F1FFFC00011080064006403011100021101031101FF
              C4007A0000020301010101000000000000000000000705060804020301010100
              0000000000000000000000000000001000010303030203050506050500000000
              0102030411050600120721133141085161223214714252621581A17292231691
              B182A233C1D1432425110100000000000000000000000000000000FFDA000C03
              010002110311003F00D53A0AFE7B9ADA70AC4EE1925D0931A0B754B29202DD75
              476B6D22BF796A207BBC74180F90B98F3CCEAE4F49BB5C9D6A0A944C7B5475A9
              B8AD23C921008DC7F32AA4E83970ECB6359A5A1E172BDDA5D041FABB5C94120F
              BD950642C7B8B9A0D8BC25C9374C9191186430B2D8CD81DD7FB46DB778E0F4AC
              986A2B69D479771B5FF3681C3A0341C17C9F320DB1E9311969F7DB4920487931
              D8481E2B75D215B509F124249F76831CF2C72FB97390F46979D4CBA78836CC65
              8FA0B620FE132DE5A9E91FC5DB23D9A048FEB32E3CD12ED9265C37927725EFA8
              51741F6F7101B35D06A7F4CDEA22ED7BB9B78566124CA9AF2546CF75729DC714
              81B8C7795F794520942CF53E06A69A0D39A0340682B39D71D6359CC6850B236D
              D936E84FFD48828754D34EBA125292EF6F6ACED0A340143C7410C780F86CB1D8
              FED381B294AEC56FFE7DDBFF007E81759DFA38C22E8C38FE2525DB15C3A96E3B
              AA54888A3EC3BCA9D47DA1469F8741972FF8E67DC619636DCC4BF67BCC457761
              4D654425C4834EE32E8E8B41F3FF00050F2D06EDE1ACB32FC9B0A62765B6872D
              578695DA5A969D88948084A9125A4F925615F6541A74D05EB418ABD53F22F225
              C322771E9F0E4D8B1665D7110632EA8FAF0CAB6992B50FF9104F540F947F1683
              9B88FD2AE4F984766F1913ABB0D89DA2994145664841150A4215D1B41F252FF6
              248EBA0D1D61F4CFC3367612D8B022E0E8F9A44F71C7D6AF794921B1FE940D07
              54AF4F9C4EB97167C1B2A6D37284F37221CEB7AD6C38DBAD282D2A0904B67A8F
              BC93A063680D01A0F2871B59504A81283B563D8680D0FEC3A0A2E79CA6D63F71
              4D8ECF005E7212CFD549614FB7122428B5A7D44E94E550CA09F9452AAF2F2A82
              F13EA662C154D45DEF98D392435FFA11ADC6E8FB7DFDE9E8F4A1194DECD9BBE2
              424F5A796839EFDC83C67CAF6018F6651516953EBD969C8633CDCD84C4B58A22
              92901B5B0A579B721B6F70E9A063F0AA2F96BC2E3E2D9174BC63CEBB6CEE75DA
              FC7636AE3BCD13E282C3A803ECA1EA0E82FCEBADB2D29D75410DA0554A3E000D
              0678C9ACF8CC8E4EB8724722952ECF6F902D18663E1B53EFCE7627471D447482
              A711DF2B2814A1F157C34A8493DEA96CADDC1D667081648C1B743425487674BE
              FED3D9EEB16F69F69B4EFA6F05FDC0795741F5B1FA81BCBEDAA6162CD945B190
              5C9DFDB929F4DC23B29EAB78DBA736D3CE2123A9D87A0D039ACF78B65EAD716E
              B6B908976F9ADA5E8D21B354AD0A1507FEE0F51A0EA438DB809428282494923D
              A93423F61D07AD01A0576713F93655F569E35663A5F6D2A8F759371EB0D4B49F
              80A69E0EB553F29513E0A4F44E810D96F17E7716CF3226790EF2FC99D3177195
              91E3E1174624BAA484213362131DD486024F68821290A202740BFB770EB3746E
              E0F40BD381BB5C7FAB968936C9ACBDDA0E21AA21094BA16BDCE0F842BDA7CB41
              3F8B7A7ACAEF68719B222E89324069D9F3A29B5404B44D54545E5ADF91EE421A
              F1EB5D06AC4B8AB265F81E2A652A53AD5AA725E7D7F3B8223519A0E2FDEB5127
              4127CB1725DB38DF21B82090A8B0D6E8A78FC343A0597A83E22C8F37B8DA325B
              0C87A4C48D0D71655BA2BA86A42997545CEEC7EE94B4B2A0AA2D0A527700003A
              0CEEC7054D933DF82264DB7C86187E52DBBADA66453DA8CD975CA29BFA84295B
              527684A8EE3D0683D623C7901BBBC6930A5647729EC2D2E464586D4E32E07126
              A9225495A035FC5DB34D03B713C43D496348B9BD638D6FB5D8AED21C968C7DD7
              D32E441EF12A5FD39A21A0BF68DC535FBBE5A07D6253A34BB2301865718C7FE8
              BD1DD254E21C48F8C385412BDD535254904F891A098D01A0F2D32D32DA5B6901
              B6D3F2A122807EC1A0F5A0340683396239FB19AFAAC94E40703B69B25AA4C084
              E24D52BEDAD1DD747B94EAC807CD20681ADCE08DFC43970F65AE42BF9504FF00
              D34101E9AB9098CC38CA034E3A1576B12116EB8364FC54693461DFB1C680EBF8
              82BD9A06B680D01A0F2969A4B8B712801C72816B03AAB6F854F9D2BA0F5A0341
              CEC5C214893262B2FA1C930CA53299046F6CB890B46E4F88DC9351EDD0746821
              B2ACC719C4ED6BBA6437166DF0D00D14EABE2591F75B40AAD6AFCA904E828976
              CAAE9C9161B45A311FAAB3C3C9587A4DD2EB25BED498B6C6DDEC9ED22AAFEB4B
              350D2AB408AABD9A082C1F0BB0583D42CAB7D8612215B2C58AB11825006E5BB2
              256FEE3AAF15AD4949AA95D4E819FC8F6D55CF8FB25B7A45572AD731A40FCCA6
              1613FBF409DC1311916BC0313E4CC2D84B5788D6861BC8AD0D8086AED1194ED7
              6A0500949D856DB9F78F4578E8198DF32E09FDDC715973156FB8B8C31260BD31
              1D88F2DB92D8711F4EF28ED511BA841A7C5502B43A0BC680D073C0B842B84612
              A13E893194A5A52F364290A2DA8A15450E868A491A0E8D01A0CE5EA659BB62B9
              8E31C876D9B36DF01D522D3903B6E73B4E9652B2EB7E216851285394DE922A06
              82BCFE799FDAB94D186E6F9CDC22633734A1CB1DFE035058EF3120D633AE3A58
              3B52A1F02949F955F97AE81DD69E0CE3E87714DD6E319FC8EEE9A1171BE4872E
              0E820D4512E9ED27AFB11A0BA3D012242A6C6094CD4C75476B757B6457720280
              EB44A8797B4E81233B24CAB1BCD957EBB2F1EB765D3E1A614DC7245CBB2CCF89
              196571E5C492E22AC39B9C5B7DB7934501506BA0EDBFF31E5522D7222C98363C
              49990D290E5E2E77C893436958DA54CC587B9C79743F0A6A057C7416AE2BB75F
              98C71365723C2670E890DB898FCD8F23EA654C67694FD5BA5003280F27E3094D
              48269E5A0B364382E1F91DB1AB65F2D11AE109848430DBCD825B005076D628B4
              741F748D027794ED163E22C49CBAD8F2DBF5A5D5D59B358512D12E3B8F53A252
              D4C6DF286D1E2B505741EF2340AECA32AE61B8E078DC4BC6493DCC9B3894116B
              B4470CC540B72A8DA5C7FB0DA1C5190E38368DDB7603E3E41AF315B046C7B1AB
              5D8A2F562D91598A85529BBB48092AFB54454E825340682B9C87855BF35C36E7
              8DCDA25139A2197A952D3E9F899747F02C03EF1D34197ADD8E3FC8DC75338DEE
              C94C7E4FE3C53A8B421C202A4C341A2980A34DC05024797FC67C09D07D78D7D5
              A4EC4F1E4639985A655CA6DAC98EC4A4AD2DBE1B6FE10D3E970577B74DBBBC69
              E3D45484D5DBD70C7EC2D368C5565F23FA6E4B940201F69436D927ECDC3408BB
              45B736E65E4AEDB8F19377BB39DD992D40F6A34745029741F2B6D22894A7EC1E
              2740D9E76F4BB0316C4D9C870E3224B76D6C0BE47795DC5A903C6522805369F9
              D23A01D452874152E1EF53391F1EDA7F42970537BB2214571585BA59763EE355
              A5B736B83613D7694F8F81D035DAF5BF8C9455DC626A17F852FB4A15FB484FF9
              682878F26EBCE7C8F2F32CC14206058E24BD290B5111D98EDFC698A95F4DCB72
              9B9D578D3D9F08D031B86E0BBC99CAB75E569CC16B1FB3FF00F33128AB4ED003
              6929DE13E036216547F3AFA7CBA0D19A0340680D02339E38ED50EFF6CE57B232
              F19D64527FB81A86AEDC8721246D321923FF002B0827E604293F30294D084F27
              1AC1AFC63DDF29C5EDF7C6E7B4DBD172C89112EB729A5A6ADAE43680A7595EDA
              56BB91F987CA02524705F0FCE80E2236356E67BEDA92D4B61868AD05428168DE
              95A2A3C45524682BD63F4FD72C65F92EE27993F65328252F16AD76C2A5250494
              8528328240AE8254F1A728B89521EE5098E34B052B41B55BA8527A106A823AE8
              23B17F4CB80DA9F5BF766DABF1524A4332214161A49241DC12C3285D7A7E2A7B
              B412B73E2BE16B77C1FD9B0A5CA23FA70E2C30F3AAFD83E140FCCB5253EFD02F
              B9371BBA641FA3716D9E247B27EA6A4CB76C96FDA23DB6D8D2EAE4B9AA68252E
              BCE2FE16D03E006BD54ADAA00F6C5F1AB4E338FC1B0DA19EC5BEDED259611E66
              9D4A947CD4B5554A3E64E825340680D01A0FC5252A494A802922841EA083A0A4
              635687307BC2EC6C02710B9BAA76CA3CADF29C254EC23EC65D512B63F0AB723C
              D1A0BC682A1CB0F6591304BADCF15B8A6DD76B5B0E4E4A9C69B79B75B61056E3
              4A0E2554DC907691E74F2D020F80797F98F9173C45AA7DF596AD509954EB8844
              48C1C71A42D280D20ECE9BD4E004F90AF9E8355E823EFD774DA6DAE4B0CAE53F
              D1B890DAA771F7D7D1B6915E80A8F99E891527A03A086C1B0F72CA99B75BAB88
              9794DF1C122F53920EC05228D4662BD4311D1F0201F1EAA3D4E82D3A0340680D
              01A0341E1E65A7DA534F202DB57CC95751EDD07BD02EBD43DC64DBF85F2A7E31
              29717152C123F048790CB9FEC70E8329FA4CB8C989CD36C61924373E34B8F200
              F340614F0FF7B493A0DE3A0F0A6195BA875480A71BAF6D47AEDDDD0D3EDD07BD
              01A0340680D01A0340680D05279B2D2ABB712E57092372CDB9E7909F6AA3A7BE
              91FE2DE8329FA3BB4AA672E7D653E0B65BE4BE55EC53850C0FDCE9D06E2D01A0
              340680D01A0340680D01A0341CD74FA2FD325FD7D3E87B2E7D56EF0ED6C3BEBF
              E9AE83327A32FED2FD632FFD37EA3EBAACFD2FD4ECAFD06F5EDF93EFEEDBDCF2
              F969A0D49A0340680D01A0341FFFD9}
            ExplicitLeft = 2
            ExplicitTop = 2
            ExplicitWidth = 75
            ExplicitHeight = 75
          end
        end
        object port: TsSpinEdit
          Left = 214
          Top = 73
          Width = 58
          Height = 24
          Margins.Left = 0
          Margins.Top = 8
          Margins.Right = 0
          Margins.Bottom = 0
          Anchors = [akTop, akRight]
          MaxLength = 6
          TabOrder = 3
          Text = '80'
          SkinData.SkinSection = 'EDIT'
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'Tahoma'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
          MaxValue = 65535
          MinValue = 0
          Value = 80
        end
        object sex: TsComboBoxEx
          Left = 151
          Top = 192
          Width = 120
          Height = 30
          BoundLabel.Indent = 0
          BoundLabel.Font.Charset = DEFAULT_CHARSET
          BoundLabel.Font.Color = clWindowText
          BoundLabel.Font.Height = -11
          BoundLabel.Font.Name = 'Tahoma'
          BoundLabel.Font.Style = []
          BoundLabel.Layout = sclLeft
          BoundLabel.MaxWidth = 0
          BoundLabel.UseSkinColor = True
          Anchors = [akTop, akRight]
          Color = 2169368
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = 14603725
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ItemHeight = 24
          ParentFont = False
          TabOrder = 6
          Images = imgSex
          ItemsEx = <
            item
              Caption = #1085#1077#1074#1072#1078#1085#1086
              ImageIndex = 0
            end
            item
              Caption = #1084#1091#1078#1095#1080#1085#1072
              ImageIndex = 1
            end
            item
              Caption = #1078#1077#1085#1097#1080#1085#1072
              ImageIndex = 2
            end>
          SkinData.SkinSection = 'EDIT'
        end
        object birthday: TsDateEdit
          Left = 151
          Top = 229
          Width = 117
          Height = 24
          Anchors = [akRight, akBottom]
          AutoSize = False
          Color = 2169368
          EditMask = '!99/99/9999;1; '
          MaxLength = 10
          TabOrder = 7
          Text = '  .  .    '
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
          GlyphMode.Blend = 0
          GlyphMode.Grayed = False
          DialogTitle = #1076#1072#1090#1072' '#1088#1086#1078#1076#1077#1085#1080#1103
        end
      end
    end
    object tbCrypto: TsTabSheet
      Caption = #1082#1088#1080#1087#1090#1086'-'#1089#1080#1089#1090#1077#1084#1072
      SkinData.CustomColor = False
      SkinData.CustomFont = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object pnlCryptoControls: TsPanel
        AlignWithMargins = True
        Left = 108
        Top = 3
        Width = 281
        Height = 262
        Margins.Left = 0
        Align = alClient
        ParentColor = True
        TabOrder = 0
        SkinData.SkinSection = 'BARPANEL'
        object btRandomTest: TsSpeedButton
          Left = 246
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
          OnClick = btRandomTestClick
          SkinData.SkinSection = 'WEBBUTTON'
          Grayed = True
        end
        object btAsymmetricTest: TsSpeedButton
          Left = 246
          Top = 41
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
          OnClick = btAsymmetricTestClick
          SkinData.SkinSection = 'WEBBUTTON'
          Grayed = True
        end
        object btSymmetricTest: TsSpeedButton
          Left = 246
          Top = 73
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
          OnClick = btSymmetricTestClick
          SkinData.SkinSection = 'WEBBUTTON'
          Grayed = True
        end
        object btHashTest: TsSpeedButton
          Left = 246
          Top = 137
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
          OnClick = btHashTestClick
          SkinData.SkinSection = 'WEBBUTTON'
          Grayed = True
        end
        object random: TsComboBox
          AlignWithMargins = True
          Left = 9
          Top = 9
          Width = 239
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Alignment = taLeftJustify
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
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ItemHeight = 0
          ItemIndex = -1
          ParentFont = False
          TabOrder = 0
        end
        object PublicKey: TsRichEdit
          AlignWithMargins = True
          Left = 9
          Top = 169
          Width = 263
          Height = 84
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 8
          Margins.Bottom = 8
          Align = alClient
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
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
        object asymmetric: TsComboBox
          AlignWithMargins = True
          Left = 9
          Top = 41
          Width = 239
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Alignment = taLeftJustify
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
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ItemHeight = 0
          ItemIndex = -1
          ParentFont = False
          TabOrder = 1
        end
        object symmetric: TsComboBox
          AlignWithMargins = True
          Left = 9
          Top = 73
          Width = 239
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Alignment = taLeftJustify
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
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ItemHeight = 0
          ItemIndex = -1
          ParentFont = False
          TabOrder = 2
        end
        object mode: TsComboBox
          AlignWithMargins = True
          Left = 9
          Top = 105
          Width = 239
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Alignment = taLeftJustify
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
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ItemHeight = 0
          ItemIndex = -1
          ParentFont = False
          TabOrder = 3
        end
        object hash: TsComboBox
          AlignWithMargins = True
          Left = 9
          Top = 137
          Width = 239
          Height = 24
          Margins.Left = 8
          Margins.Top = 8
          Margins.Right = 32
          Margins.Bottom = 0
          Align = alTop
          Alignment = taLeftJustify
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
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          ItemHeight = 0
          ItemIndex = -1
          ParentFont = False
          TabOrder = 4
        end
      end
      object pnlCryptoLabels: TsPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 105
        Height = 262
        Margins.Right = 0
        Align = alLeft
        Padding.Left = 8
        Padding.Right = 8
        ParentColor = True
        TabOrder = 1
        SkinData.SkinSection = 'BARPANEL'
        object lbRandom: TsLabel
          Left = 9
          Top = 4
          Width = 87
          Height = 32
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1075#1077#1085#1077#1088#1072#1090#1086#1088#13#10#1089#1083#1091#1095'. '#1095#1080#1089#1077#1083
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
        object lbAsymmetric: TsLabel
          Left = 9
          Top = 44
          Width = 87
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1072#1089#1080#1084#1084'. '#1096#1080#1092#1088
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
        object lbSymmetric: TsLabel
          Left = 9
          Top = 76
          Width = 87
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1089#1080#1084#1084'. '#1096#1080#1092#1088
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
        object lbMode: TsLabel
          Left = 9
          Top = 108
          Width = 87
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1088#1077#1078#1080#1084
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
        object lbHash: TsLabel
          Left = 9
          Top = 140
          Width = 87
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1093#1101#1096'-'#1092#1091#1085#1082#1094#1080#1103
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
        object lbPublicKey: TsLabel
          Left = 9
          Top = 172
          Width = 87
          Height = 16
          Alignment = taRightJustify
          AutoSize = False
          Caption = #1087#1091#1073#1083'. '#1082#1083#1102#1095
          ParentFont = False
          Font.Charset = RUSSIAN_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = [fsBold]
          UseSkinColor = False
        end
      end
    end
  end
  object imgSex: TsAlphaImageList
    DrawingStyle = dsTransparent
    Height = 24
    Width = 24
    Items = <
      item
        ImageFormat = ifPNG
        ImgData = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000B1300000B1301009A9C18000000206348524D0000
          7A25000080830000F9FF000080E9000075300000EA6000003A980000176F925F
          C546000005F54944415478DA9C956B6C93551CC69FF7EDE56DD7AE6C9DEDBA76
          6377D8A5205664182D4EB6A1824894808062043122910F1A4DF016898ACA1713
          62BC2208E24425DC12234A04E53A281D34DD266C6DD7B1756DD7755DD7766DDF
          EBF183A24344D127391FCFFF77FEE739E7FF50B88ED6AC5983D9B31BD0709B0D
          959515904491968854CA73D96691E7EC3C9F2D2704C90263C9062627D74D24E9
          AAFD342D0300C8F10FA20048927093C0B30B059E5BC7A8B4D394DA7C86A26950
          140D8167C166C7AB657265834C2E8FFD5D8DEB0272D42A5D6D4DE55A7391E959
          46A531E568F5A068190891402411A2C04112598070553C9B7A5B92D44FD134F5
          67074AF91F87BC464FAE7E6CE9ABAF6C78C75C5C5E0E4A068AA2402880881224
          4980C065303C1440676717BCBE5ED8ED73F8DDDFEC9FBD77DFC1F31A4D0E00A0
          A3A3F3EF3BD8F2EE5B9BD6AF7FE6255E000441002DC36F00422049228824A0BF
          AF073B777D855FBAFDB87FC13D188E84155A8D7AA9DFEF3FFF4F578E87972C5A
          3E36EC25C3211F1919EA2399F151220A2C11058E705C86F05C86C422FDE485E7
          9E21F35A9A88B7DB4D32C908F174B59123870FFA72737373AFD4228480107255
          07F2B98D7356C7E309C8E5328092C3D3D30D5E24C8CBD3C3525C8CFC7C3D2EB8
          DC3873D689B75EDF805E4F17F9F4C3F7251988EC89A79FAE58F5F8CA959F6EDB
          F18146A3B9EAA15C51C12B2F3EF7DDC2F92DB3940C83FD07BE45343606A59241
          281482B9C888975EDC8044721CEDE74E0084E0D0E16389629345FEE8F2E53953
          EAEA118E84025EFFE0B4DABAFA784181FE1A0F524EA7B3D75A5336EBF8E97698
          2D93F1CEE697018943AFA70B3D3D3DF07A3D98D960C760C00F4110C89B6F6CD2
          9ACDC5344924C08D8E40AFD3166FDBBED57AE2C4E9935EAFF71A003B188E741C
          3FD9B6CC6028C4632B1E82F3CC311CFDF938974AA585F9F7B5A820F1742C1A41
          6DFD2D488E8D50C6C2224ACAB210B269402147381C4091B1A0D9E7F39DBCE61F
          D014458507827ED96C9B3479B299FEFEFB4338D5762169B3CDC492C576D5945A
          2B1D0CF81018E8456DBD0D02CF231A0D83CBA6C1A6531818E887CBED42457949
          A342A1F8D358638EBA6EBE6DD61693A9B03472D953C0C553629665E9B2D20ABC
          F6F2024DB9C5421391401078302A0D0A8B7410451E264B2962C34174BADB914C
          A6E0767720140AE3C1C5B71A954AA51C800000724E92D4B168D81E8984996C7A
          8C040783C3969A72C3EDB7DBA952B385CE0C8521110931810372B4C8CDD5C0D3
          DD098552852F76EDC46D336762F1B2D5C8703B100886A056A919B95CAEF80310
          CFB2EE33FEBE8FF218E64EBD526590A5D306577B075555550963BE1E3AB50A89
          C4282E793DC80A020E1CFC16D1E8081CED1D0028149ACCD8F5F936ECDB7B000F
          3ED082642AC5A5331971A2077C84655F88B0AC0E48E4012816BD7DEBFCBEDEA5
          B191388A4C26844221D45BAD30190A7197FD0E9C38ED8046EB87D55A8B1DBBF6
          A0CE5C8875AB96A1CC5A0757474F82E738EEAF26F300467E5F3E9F7F40BC3C10
          6AAED3E6EA7BFBFC38D5E64456A451552DC21F1C81ABD383B2620B56CCB16352
          8B0AE6CA324425162A550E828321EFBF4E539EE75D478EB5B5DE6A9BBE7EC1DC
          B9B8BBB111070E7E87B37D7D9089225635CF4189D98249FA3C38BD5E0C2A096C
          374F874EA783E39CF3FC8D8CEB94AFB7FFBD4FB67F393593C9CE6B6A6AC6DAA7
          56213E3C8C4C2A8570780863D90C76EEDB8BA33FB5614FEBC7A89A6A457C340E
          97BBB3EB86F20080A7FD42D7F3E3E399378686228B9A9A9A602C3440A1D3E126
          468D297A03AAA7D46270308A1F7E3C85A67B97C0E93C3FE2F7F7F5E03FAA3C3F
          7FD2E6471E5E186BFD6C0B3972E86BE2BBD44E0821643436446A6BA6921933A6
          9380BF8BAC5BBB7A0F00D9C4692ABB01403C9B651D5D17BDDDFD03812215A328
          0906FA299E4FA3BA661A1C8EB370381C282D3162EBF6D6CD8944CA05001B376E
          BC7EA25D473200558C52B1C8768B757175C5E469F35AE6AACF9C73A175F75E98
          4C862F2F5EF23D0B2072A583FF0AB82206402945510DFAFC49D3958CF2E6F1F1
          4C2491486E02707162E0FC5FC0C42C5101C89FF08FA4BF027E1D00DDB9E2AA74
          63BEAB0000000049454E44AE426082}
      end
      item
        ImageFormat = ifPNG
        ImgData = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F8000000097048597300000B1300000B1301009A9C18000000206348524D0000
          7A25000080830000F9FF000080E9000075300000EA6000003A980000176F925F
          C546000004884944415478DA9C955B88955514C77F6BEFEF9C3367669CD471D4
          191B754ACB14CC1CB348AD2C898A240A13A120A227ED823D043E85BD84506691
          3DF95220141A3574231233111FCA3B6534792935E7E265AEE7F69D6FEFBD7A38
          958E9E916C3D7D7B7FECF55F7BADFFFFBF45551111AA45FB82458F23BA5DA290
          D2A008B684970E15DFAA4AE4D5AF3F7260DF37D5CEAA2A00321AC082F645CB40
          7704020A0455505051D4FBCA378A8AAE3F7AE4D0EBD70D306FFEC25821AD3E10
          54D100DE07D08031828AA0C1A391A3F3A75F64340053ED7A73E62EB82929BBB4
          8B131297E0BDA3582AD23A710C931AEBE91F8A1918CA91240E5F56DA6E9DF91C
          A344546DD3954B8D88A04151C079CF502EE6DBEF9F81E118322936BCB69B0D5B
          F67243431A31A1EFBA00BCD7E0BCA3EC02E524C127C21B6BEE81B317D1A24344
          58F7EA7CCAF9221B3FDA4F7D9D84D100AACE60CAD419ED355166FF8AA5D3696D
          CAF2E443D3696E1B0BC51248A5BF220626D7D132670BD8727BF7993307ABCDA0
          EA0DCEF69774F7072F70EF1D5D100B386070183501CAC3A82AEA4B183B81D913
          6BD879F8C4C1EB6A51F7923FA3A6E60BD057004DA1C58BF84217948BA8318022
          584CE1144F647AF6EF64F4A80AD0FC353FE6D719B22E8FF61E2104C01A5088C4
          56E644096C96F19978F81AF9ABD31420F75D071227F85C018D1D5A88A1989014
          8AB842018A0ED793E7E061FFD5FF0218FC5AB751D786941228E4917C82E6634C
          A98494626C59D9B3ED246F0D15375E0BA02A8B563F3BA5E6D6CED285876E6CAD
          9BF5EC12C2B10324B99854C3052432880803C7CB6CFDC592BD41F33F6775F2BB
          1F9EC9FD27AB58B37CF2A6793DD9B577DEFD18435395DCA7BF316D6A1F63262C
          259DFE84739D11BF1E1F43FD4C434B4F9E9ED3039C6A0E9C9861DF7BF3D3AE97
          AF09F0FC234D9BDA079AD62E5FBA1A3D7D8E5CC797E46FCE3314B530A62943BA
          7698523E8D767AC6BB41E2629AC8A5E83783EC6BBEC0B119A977DEFFFCEC2B55
          01563E3A69C5C2BEFAED4FDDF7127AF424033B7620D90C462C068F91186B2CBE
          CEE28AA089410191081B02C326CFAE297D1C9D11CDDFFA59D7A1AB84D6FA47BC
          FDFE079E467ACF737ED797485D0D22099E04045083055CDE1311F04640C09210
          8B524B865BBAA148F92095139758F4F8FD8D9B9BA7CD646A432BBD1D1F636A0D
          92AA58B1DA043509440E6713B009CEFEF3CFE14C19B5092E2A719B9D840658F9
          70CBE61142B303F1AAA5B72FA6FF873D9029E1ADBD4433AFE8E56B40FF5DFDED
          715211611C62E6E653FC3E90AC025EFC17A029641BC78D9D485FE767A8052E37
          C7C8A0B84BF4ABFA3E061070569828969A5CDC38D22AB2696A067378CD433603
          2A88802AA85524C8DFC91554C0566AD02B642A5E690A42D12723BD289BAE2349
          8649D7A608C6A056702E20A692388822462A851AA9BCC7A6D22E0901630C4620
          32867EA33444E9914A7EF08E86A36D5233BB550C6DA4A877308B888054A8A801
          9591E55A815E02C316BA709C0F9E93AE48AEEC2835A4BABFD8D7DF324207772D
          1E37365DD6B7D3495866123FBEB6ECEBAE6AF515131063B59036175D647A4224
          A75DCA6ED9BBB7AFE372A1FD350009DA64D5B04E856E0000000049454E44AE42
          6082}
      end
      item
        ImageFormat = ifPNG
        ImgData = {
          89504E470D0A1A0A0000000D4948445200000018000000180806000000E0773D
          F80000042B49444154488995945B6C55451486BFD97B9FD372A017DA726BA018
          8810A4106B4452254604896825D12818D1174DD450C54463A2262624C618637C
          F0C517129ED088977A57BC3C143484844451A05C6C504A5BE8959ED3B36FB3F7
          9EE5C3E92954CF29B092B5339959F3FD336BED598A696CE3A607D601FB80C689
          A9B3C071600B004AA160CF4F3F7CF55439869A06DE049C9B0C54855049049420
          461011123120F1FB073A7F6E2FC5B1CA2A2BDEC15080240961A8F1FC8020F408
          438F40FB04DA47EB8050473BD6B6AECB5C974018866D61E413041E63D91C9515
          C2AAE575CC9A95C2F543B4D668AD098B1EC6AF96E238E504B40E33C618C65DCD
          D6FB97B3EBAD6D40C2A1FD47D8B6E34BEA6A2A100382A04830C216E0F56BBF81
          D6E4F2011B6F9BCFAE37EE8381011818E6F63B57B0F3B195F40D647183006362
          A490D2F8BA5214EB00D7757976FB1A181903ED177C748C975FDAC8D0E1E7697F
          A41965D924226559650504484CC24D4B9BC0CB431440A441C730D2870A2EF062
          FB8DBCB26D31616800BA4A71CAD6A0600A625D703D8E04FD100D4F9C4BA17216
          1B56C4BC66003871DD02B66371E2F87956D61FC5F81E281B94020A449528BEF9
          258B635B00ADA5182553D4BCBAE5184045CAE1C3EF4E41E423818F041EE2BB93
          8E76393F1053E0D3D6BCBA65E8AA02CDAB5BDA80668074CAE2A30367A0BE1595
          CF82EB81E783E763053EE4033E381A9272261B42C3B5DCA0412814591B838E3C
          4E7EF11BD6A2F558A187F2F3D8B18F0A02F6BDDD4D431093B714AE52E42CEBE2
          7F61257BD1AA95377FD21F470FB7CD9DC7EEED0F128DBF4977570DF577B45257
          3DC2E98EC39C3A15B2E1D139CC39D0C0D7DD97E81F0FF5524966DF33D0EB4D2B
          10A6AB1A958EFA523B9F86B63524870EA2969E86F9307A3ACBC83F2EB54DD5CC
          BBA51E460DE16709E92A9BE8DB6EFCBEB1DE5A9D5D34AD408F53D537FBB9271B
          339B5B883EDD8BE4C7B11666483FB304FCA8B043804A87E8F30B247FE6C0B150
          331CDC1FCF12F6E73A1BFD4BEB4BD6E02F27F3F88CCDEB1B676E5881DEBB1B33
          328A4409715796E4988F2C5E8699BF04B96119C9DF42D479110934E206981197
          CCAD734990BBCE656657169953DE8117474F2CB87B2DD1AF9D98C14B904A0109
          2004EFFD8ED3E3A216D5610E9E25F96308957190C05C06A46C9CC62AF2BDB977
          81F6FF0998EADA4D99C65AFC8E2388ED80BE623342FC71176204E558602BC4BB
          721D508ACA9A3483BDA6ADA4406059485F0F49DE47555452CE24321095985750
          39CB4227D2543245D4CC4486C7C1351095ECBE57375BA16D45B1794F11B03233
          B1B58DE526884E10E4AA3C35F1B5D4C45809967D797D8AC09993C73ADCC1C187
          16F801D51586B9E9F4A448F17517A1C5FF3B122117C5E4E384A1386238342825
          1D530F7085EDC16E4EE005818506B9B7189446516F3958C0B089274B60A37236
          1CB2503DB6E27B076BFF56098322EF5FF7CB0F4EB0A9BCB10000000049454E44
          AE426082}
      end>
    Left = 352
    Top = 192
    Bitmap = {}
  end
end
