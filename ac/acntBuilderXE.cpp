//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("sImgListEditor.pas", Simglisteditor, FormImgListEditor);
USEFORMNS("sInternalSkins.pas", Sinternalskins, FormInternalSkins);
USEFORMNS("sGradBuilder.pas", Sgradbuilder, GradBuilder);
USEFORMNS("sHintDesigner.pas", Shintdesigner, HintDesigner);
USEFORMNS("sStrEdit.pas", Sstredit, StrEditDlg);
USEFORMNS("acHintPage.pas", Achintpage, FrameHintPage); /* TFrame: File Type */
USEFORMNS("ac3dNewClass.pas", Ac3dnewclass, FormNewThirdClass);
USEFORMNS("ac3rdPartyEditor.pas", Ac3rdpartyeditor, Form3rdPartyEditor);
USEFORMNS("acAlphaHintsEdit.pas", Acalphahintsedit, AlphaHintsEdit);
USEFORMNS("acRootEdit.pas", Acrootedit, acRootPathEditDlg);
USEFORMNS("acSelectSkin.pas", Acselectskin, FormSkinSelect);
USEFORMNS("acSkinInfo.pas", Acskininfo, SkinInfoForm);
USEFORMNS("acSkinPreview.pas", Acskinpreview, FormSkinPreview);
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------


#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
	return 1;
}
//---------------------------------------------------------------------------
