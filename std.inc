{******************************************************************************}
{*  std.inc                                                                   *}
{*  Revolutionary Confederation of Anarcho Syndicalists                       *}
{*  Written by: black.rabbit 2011                                             *}
{******************************************************************************}
{$LONGSTRINGS ON}         // длинные строки
{$ALIGN ON}               // выравнивание полей классов
{$DEFINE ASM}             // компиляция в ассемблерном варианте
{.$DEFINE HEX_UPPER_CASE} // верхний регистр в 16-ричной записи чисел
{.$DEFINE EXTERNAL_LINK}  // использовать крипто-функции из dll
{$DEFINE ECB}             // доступность режима электронной кодовой книги
{$DEFINE CBC}             // доступность режима сцепления блоков по шифротексту
{$DEFINE CFB}             // доступность режима обратной связи по шифротексту
{$DEFINE OFB}             // доступность режима обратной загрузки выходных данных
{$DEFINE GZIP}            // использовать gzlib вместо zlib
{.$DEFINE DELETE_TRIGGER} // удалять записи в свзязанных таблицах с помощью триггеров
//                          ( к сожалению SQLit3 не поддерживает рекурсию в триггерах )
{$DEFINE CHECK_PASSWORDS} // проверять пароли на минимальную сложность при регистрации
{$DEFINE SMPL_PASSWORDS} // допустимы простые пароли
{.$DEFINE NORM_PASSWORDS}  // допустимы пароли средней сложности
{.$DEFINE DIFF_PASSWORDS} // допустимы лишь сложные пароли

{.$DEFINE HTTP}            // использовать для связи http
{$DEFINE SMTP_POP3}       // использовать для связи smtp/pop3

{ -- настройки SQLite}
{.$DEFINE SQLITE_SYNCHRONOUS_FULL}  // использовать полную синхронизацию
{$DEFINE SQLITE_SYNCHRONOUS_NORMAL} // использовать частичную синхронизацию
{.$DEFINE SQLITE_SYNCHRONOUS_OFF}   // не использовать синхронизацию (крайне не рекомендуется)
{$DEFINE SQLITE_AUTO_VACUUM_NONE}         // автоматическое сжатие БД отключено
{.$DEFINE SQLITE_AUTO_VACUUM_FULL}        // полное автоматическое сжатие БД
{.$DEFINE SQLITE_AUTO_VACUUM_INCREMENTAL} // пошаговое автоматическое сжатие БД

