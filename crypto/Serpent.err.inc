{******************************************************************************}
{*  Serpent.err.inc                                                           *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
resourcestring
    ERR_SERPENT_INCORRECT_KEY   = 'Некорректный ключ "%s"!';
    ERR_SERPENT_INCORRECT_BLOCK = 'Некорректный блок "%s"!';
    ERR_SERPENT_INCORRECT_IV    = 'Некорректный вектор инициализации "%s"!';
{$IFDEF ECB}
    ERR_SERPENT_ECB_ENCRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме электронной кодовой книги'#13#10'Ошибка шифрования данных';
    ERR_SERPENT_ECB_DECRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме электронной кодовой книги'#13#10'Ошибка дешифровки данных';
{$ENDIF ECB}
{$IFDEF CBC}
    ERR_SERPENT_CBC_ENCRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме сцепления блоков по шифротексту'#13#10'Ошибка шифрования данных';
    ERR_SERPENT_CBC_DECRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме сцепления блоков по шифротексту'#13#10'Ошибка дешифровки данных';
{$ENDIF CBC}
{$IFDEF CFB}
    ERR_SERPENT_CFB_ENCRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме обратной связи по шифротексту'#13#10'Ошибка шифрования данных';
    ERR_SERPENT_CFB_DECRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме обратной связи по шифротексту'#13#10'Ошибка дешифровки данных';
{$ENDIF OFB}
{$IFDEF OFB}
    ERR_SERPENT_OFB_ENCRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме обратной загрузки выходных данных'#13#10'Ошибка шифрования данных';
    ERR_SERPENT_OFB_DECRYPT     = 'Ошибка криптосистемы Serpent::%d %s в режиме обратной загрузки выходных данных'#13#10'Ошибка дешифровки данных';
{$ENDIF OFB}
