//---------------------------------------------------------------------------

#include <basepch.h>
#pragma hdrstop
USEFORMNS("sCalcUnit.pas", Scalcunit, sCalcForm);
USEFORMNS("sPopupClndr.pas", Spopupclndr, sPopupCalendar);
USEFORMNS("sColorDialog.pas", Scolordialog, sColorDialogForm);
USEFORMNS("acPathDialog.pas", Acpathdialog, PathDialogForm);
USEFORMNS("acMagn.pas", Acmagn, acMagnForm);
USEFORMNS("acThumbForm.pas", Acthumbform, MagnifierOwner);
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
