{******************************************************************************}
{*  BlowFish.err.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
resourcestring
    ERR_BLOWFISH_INCORRECT_KEY   = 'Некорректный ключ "%s"!';
    ERR_BLOWFISH_INCORRECT_BLOCK = 'Некорректный блок "%s"!';
    ERR_BLOWFISH_INCORRECT_IV    = 'Некорректный вектор инициализации "%s"!';
{$IFDEF ECB}
    ERR_BLOWFISH_ECB_ENCRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме электронной кодовой книги'#13#10'Ошибка шифрования данных';
    ERR_BLOWFISH_ECB_DECRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме электронной кодовой книги'#13#10'Ошибка дешифровки данных';
{$ENDIF ECB}
{$IFDEF CBC}
    ERR_BLOWFISH_CBC_ENCRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме сцепления блоков по шифротексту'#13#10'Ошибка шифрования данных';
    ERR_BLOWFISH_CBC_DECRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме сцепления блоков по шифротексту'#13#10'Ошибка дешифровки данных';
{$ENDIF CBC}
{$IFDEF CFB}
    ERR_BLOWFISH_CFB_ENCRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме обратной связи по шифротексту'#13#10'Ошибка шифрования данных';
    ERR_BLOWFISH_CFB_DECRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме обратной связи по шифротексту'#13#10'Ошибка дешифровки данных';
{$ENDIF CFB}
{$IFDEF OFB}
    ERR_BLOWFISH_OFB_ENCRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме обратной загрузки выходных данных'#13#10'Ошибка шифрования данных';
    ERR_BLOWFISH_OFB_DECRYPT     = 'Ошибка криптосистемы BlowFish::%d %s в режиме обратной загрузки выходных данных'#13#10'Ошибка дешифровки данных';
{$ENDIF OFB}