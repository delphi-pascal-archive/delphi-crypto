{******************************************************************************}
{*  TwoFish.err.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
resourcestring
    ERR_TWOFISH_INCORRECT_KEY   = 'Некорректный ключ "%s"!';
    ERR_TWOFISH_INCORRECT_BLOCK = 'Некорректный блок "%s"!';
    ERR_TWOFISH_INCORRECT_IV    = 'Некорректный вектор инициализации "%s"!';
{$IFDEF ECB}
    ERR_TWOFISH_ECB_ENCRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме электронной кодовой книги'#13#10'Ошибка шифрования данных';
    ERR_TWOFISH_ECB_DECRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме электронной кодовой книги'#13#10'Ошибка дешифровки данных';
{$ENDIF ECB}
{$IFDEF CBC}
    ERR_TWOFISH_CBC_ENCRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме сцепления блоков по шифротексту'#13#10'Ошибка шифрования данных';
    ERR_TWOFISH_CBC_DECRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме сцепления блоков по шифротексту'#13#10'Ошибка дешифровки данных';
{$ENDIF CBC}
{$IFDEF CFB}
    ERR_TWOFISH_CFB_ENCRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме обратной связи по шифротексту'#13#10'Ошибка шифрования данных';
    ERR_TWOFISH_CFB_DECRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме обратной связи по шифротексту'#13#10'Ошибка дешифровки данных';
{$ENDIF CFB}
{$IFDEF OFB}
    ERR_TWOFISH_OFB_ENCRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме обратной загрузки выходных данных'#13#10'Ошибка шифрования данных';
    ERR_TWOFISH_OFB_DECRYPT     = 'Ошибка криптосистемы TwoFish::%d %s в режиме обратной загрузки выходных данных'#13#10'Ошибка дешифровки данных';
{$ENDIF OFB}
