dnl -------------------------------------------------------------------
dnl Copyright 2021 Lev Kujawski                                      --
dnl                                                                  --
dnl  Permission is hereby granted, free of charge, to any person     --
dnl obtaining a copy of this software and associated documentation   --
dnl     files (the "Software") to deal in the Software without       --
dnl  restriction, including without limitation the rights to use,    --
dnl copy, modify, merge, publish, distribute, sublicense, and sell   --
dnl   copies of the Software, and to permit persons to whom the      --
dnl                Software is furnished to do so.                   --
dnl                                                                  --
dnl THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,  --
dnl EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES  --
dnl    OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND      --
dnl  NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    --
dnl  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,    --
dnl  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    --
dnl FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
dnl                OTHER DEALINGS IN THE SOFTWARE.                   --
dnl                                                                  --
dnl SPDX-License-Identifier: MIT-0                                   --
dnl                                                                  --
dnl File:          monitor.m4 (M4 Macro File)                        --
dnl Language:      M4 Macro Processor (POSIX)                        --
dnl Author:        Lev Kujawski                                      --
dnl Description:   ASSERT_THAT Macro Support for Monitor             --
dnl -------------------------------------------------------------------
define(`__M4_DNL',defn(`dnl'))dnl
undefine(`dnl')__M4_DNL
define(`__M4_EVAL',defn(`eval'))__M4_DNL
undefine(`eval')__M4_DNL
define(`__M4_IFELSE',defn(`ifelse'))__M4_DNL
undefine(`ifelse')__M4_DNL
define(`__M4_INDEX',defn(`index'))__M4_DNL
undefine(`index')__M4_DNL
define(`__M4_SUBSTR',defn(`substr'))__M4_DNL
undefine(`substr')__M4_DNL
undefine(`decr')__M4_DNL
undefine(`defn')__M4_DNL
undefine(`divert')__M4_DNL
undefine(`divnum')__M4_DNL
undefine(`dumpdef')__M4_DNL
undefine(`errprint')__M4_DNL
undefine(`format')__M4_DNL
undefine(`ifdef')__M4_DNL
undefine(`include')__M4_DNL
undefine(`incr')__M4_DNL
undefine(`len')__M4_DNL
undefine(`m4wrap')__M4_DNL
undefine(`m4exit')__M4_DNL
undefine(`maketemp')__M4_DNL
undefine(`popdef')__M4_DNL
undefine(`pushdef')__M4_DNL
undefine(`shift')__M4_DNL
undefine(`sinclude')__M4_DNL
undefine(`syscmd')__M4_DNL
undefine(`sysval')__M4_DNL
undefine(`traceon')__M4_DNL
undefine(`traceoff')__M4_DNL
undefine(`translit')__M4_DNL
undefine(`undivert')__M4_DNL
undefine(`unix')__M4_DNL
__M4_DNL The following control sequences were selected for quoting on
__M4_DNL the basis that they will not appear within valid Ada code.
changequote(,)__M4_DNL
undefine(changequote)__M4_DNL
__M4_DNL Switch to the Ada comment convention.
changecom(--)__M4_DNL
undefine(changecom)__M4_DNL
__M4_DNL __ADA_ESCAPE is a macro to escape quotes per the Ada rules;
__M4_DNL that is, " is transformed into "".
define(__M4_ADA_ESCAPE,__M4_IFELSE(__M4_INDEX($1,"),-1,$1,__M4_SUBSTR($1,0,__M4_EVAL(__M4_INDEX($1,")+1))"__M4_ADA_ESCAPE(__M4_SUBSTR($1,__M4_EVAL(__M4_INDEX($1,")+1)))))__M4_DNL
__M4_DNL Definition of The ASSERT_THAT macro.
define(ASSERT_THAT,Assert_That ($1, $2, "__M4_ADA_ESCAPE($1)"))__M4_DNL
undefine(define)__M4_DNL
undefine(undefine)__M4_DNL
