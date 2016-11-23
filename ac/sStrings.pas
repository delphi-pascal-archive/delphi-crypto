unit sStrings;

interface

const
  MaxAlphaStrID = 59000;
  s_MsgDlgWarning = MaxAlphaStrID - 2;
  s_MsgDlgError = MaxAlphaStrID - 3;
  s_MsgDlgInformation = MaxAlphaStrID - 4;
  s_MsgDlgConfirm = MaxAlphaStrID - 5;
  s_MsgDlgYes = MaxAlphaStrID - 6;
  s_MsgDlgNo = MaxAlphaStrID - 7;
  s_MsgDlgOK = MaxAlphaStrID - 8;
  s_MsgDlgCancel = MaxAlphaStrID - 9;
  s_MsgDlgHelp = MaxAlphaStrID - 10;
  s_MsgDlgHelpNone = MaxAlphaStrID - 11;
  s_MsgDlgHelpHelp = MaxAlphaStrID - 12;
  s_MsgDlgAbort = MaxAlphaStrID - 13;
  s_MsgDlgRetry = MaxAlphaStrID - 14;
  s_MsgDlgIgnore = MaxAlphaStrID - 15;
  s_MsgDlgAll = MaxAlphaStrID - 16;
  s_MsgDlgNoToAll = MaxAlphaStrID - 17;
  s_MsgDlgYesToAll = MaxAlphaStrID - 18;

  s_RestoreStr = MaxAlphaStrID - 19;
  s_MoveStr = MaxAlphaStrID - 20;
  s_SizeStr = MaxAlphaStrID - 21;
  s_MinimizeStr = MaxAlphaStrID - 22;
  s_MaximizeStr = MaxAlphaStrID - 23;
  s_CloseStr = MaxAlphaStrID - 24;

  s_HintClose = MaxAlphaStrID - 25;
  s_HintMaximize = MaxAlphaStrID - 26;
  s_HintMinimize = MaxAlphaStrID - 27;
  s_HintRestore = MaxAlphaStrID - 28;
  s_HintHelp = MaxAlphaStrID - 29;

  s_FileOpen = MaxAlphaStrID - 30;

  s_Calculator = MaxAlphaStrID - 31;
  s_GradBuilder = MaxAlphaStrID - 32;
  s_HotGradBuilder = MaxAlphaStrID - 33;
  s_Panels = MaxAlphaStrID - 34;

  s_AvailSkins = MaxAlphaStrID - 36;
  s_InternalSkin = MaxAlphaStrID - 37;

  s_ErrorSettingCount = MaxAlphaStrID - 38;
  s_ListBoxMustBeVirtual = MaxAlphaStrID - 39;

  // Color dialog
  s_ColorDlgAdd       = MaxAlphaStrID - 40;
  s_ColorDlgDefine    = MaxAlphaStrID - 41;
  s_ColorDlgMainPal   = MaxAlphaStrID - 42;
  s_ColorDlgAddPal    = MaxAlphaStrID - 43;

  s_ColorDlgTitle     = MaxAlphaStrID - 44;
  s_ColorDlgRed       = MaxAlphaStrID - 45;
  s_ColorDlgGreen     = MaxAlphaStrID - 46;
  s_ColorDlgBlue      = MaxAlphaStrID - 47;
  s_ColorDlgDecimal   = MaxAlphaStrID - 48;
  s_ColorDlgHex       = MaxAlphaStrID - 49;

  // Frame adapter
  s_FrameAdapterError1 = MaxAlphaStrID - 50;

  // Hint designer
  s_HintDsgnTitle        = MaxAlphaStrID - 51;
  s_HintDsgnPreserved    = MaxAlphaStrID - 52;
  s_HintDsgnStyle        = MaxAlphaStrID - 53;
  s_HintDsgnBevelWidth   = MaxAlphaStrID - 54;
  s_Blur                 = MaxAlphaStrID - 55;
  s_HintDsgnArrowLength  = MaxAlphaStrID - 56;
  s_HintDsgnHorizMargin  = MaxAlphaStrID - 57;
  s_HintDsgnVertMargin   = MaxAlphaStrID - 58;
  s_HintDsgnRadius       = MaxAlphaStrID - 59;
  s_HintDsgnMaxWidth     = MaxAlphaStrID - 60;
  s_HintDsgnPauseHide    = MaxAlphaStrID - 61;
  s_Percent              = MaxAlphaStrID - 62;
  s_HintDsgnOffset       = MaxAlphaStrID - 63;
  s_HintDsgnTransparency = MaxAlphaStrID - 64;
  s_HintDsgnNoPicture    = MaxAlphaStrID - 65;
  s_Font                 = MaxAlphaStrID - 66;
  s_Texture              = MaxAlphaStrID - 67;
  s_HintDsgnLoad         = MaxAlphaStrID - 68;
  s_HintDsgnSave         = MaxAlphaStrID - 69;
  s_HintDsgnColor        = MaxAlphaStrID - 70;
  s_HintDsgnBorderTop    = MaxAlphaStrID - 71;
  s_HintDsgnBorderBottom = MaxAlphaStrID - 72;
  s_Shadow               = MaxAlphaStrID - 73;
  s_Background           = MaxAlphaStrID - 74;
  s_Gradient             = MaxAlphaStrID - 75;
  s_PreviewHint          = MaxAlphaStrID - 76;

  // File dialogs
  s_Folder               = MaxAlphaStrID - 77;
  s_FileName             = MaxAlphaStrID - 78;
  s_FileType             = MaxAlphaStrID - 79;
  s_ReadOnly             = MaxAlphaStrID - 80;
  s_ViewStyleList        = MaxAlphaStrID - 81;
  s_ViewStyleReport      = MaxAlphaStrID - 82;
  s_ViewStyleIcons       = MaxAlphaStrID - 83;
  s_ViewStyleSmallIcons  = MaxAlphaStrID - 84;
  s_OpenFile             = MaxAlphaStrID - 85;
  s_SaveFile             = MaxAlphaStrID - 86;
  s_BtnOpen              = MaxAlphaStrID - 87;
  s_BtnSave              = MaxAlphaStrID - 88;
  s_SelectDir            = MaxAlphaStrID - 89;
  s_Create               = MaxAlphaStrID - 90;

implementation

{$R sStrings.res}

end.
