Здесь мы пытаемся разобраться в отладчике SBCL
===============================================

;;; There is one per compiled file and one per function compiled at
;;; toplevel or loaded from source.
(defstruct debug-source)

Не можем понять со стороны кода. Попробуем со стороны компилятора:

defun sb!xc:compile-file - тут вход в компиляцию 

source-info - структура, которая участвует в этом процессе. 

ir1tran.lisp содержит то, что нам нужно:

;;; *SOURCE-PATHS* is a hashtable from source code forms to the path
;;; taken through the source to reach the form. This provides a way to
;;; keep track of the location of original source forms, even when
;;; macroexpansions and other arbitary permutations of the code
;;; happen. This table is initialized by calling FIND-SOURCE-PATHS on
;;; the original source.
;;;
;;; It is fairly useless to store symbols, characters, or fixnums in
;;; this table, as 42 is EQ to 42 no matter where in the source it
;;; appears. SB-C::GET-SOURCE-PATH and NOTE-SOURCE-PATH functions should be
;;; always used to access this table.
(declaim (hash-table *source-paths*))
(defvar *source-paths*)


;;; создаёт контекст для работы find-source-paths
(defmacro sb-c::with-source-paths (&body forms))


;;; This function is called on freshly read forms to record the
;;; initial location of each form (and subform.) Form is the form to
;;; find the paths in, and TLF-NUM is the top level form number of the
;;; truly top level form.
(defun sb-c::find-source-paths (form tlf-num)
 <-- sub-sub-compile-file 

Общая идея. Система координат в формах - это номер формы в файле и путь внутри формы. SB-C::SUB-FIND-SOURCE-PATHS`отвечает за собственно сопоставление подформам путей. Она идёт по форме, заходит в car и cdr и меняет текущий путь. Текущий путь для каждой подформы записывается в хеш-таблицу *source-paths* .

Почему же записывается не строка/колонка? Видимо, потому что так мы более-менее устойчивы к редактированию файла. Хотя вставка новой формы всё же должна сломать нашу работу, если мы действительно считаем именно порядковый номер в файле. 

Как нам приделать это к нашему интерпретатору? 
load-as-source - тоже что-то пытается сделать на тему запоминания исходников. Но пока неясно, как оно может помочь нам. 

Видимо, вот как:
defun выполняется с помощью eval-tlf. eval-tlf запоминает 
*eval-source-info* - оно непусто во время load. 

Теперь задача - проассоциировать *eval-source-info* с функцией и у нас появится шанс!
eval-tlf --> bind SB-IMPL::*EVAL-SOURCE-INFO* --> sb-impl::eval-in-lexenv --> 
SB-EVAL:EVAL-IN-NATIVE-ENVIRONMENT --> ... --> %defun получает в качестве одного из параметров 
definition-source-location, но далее его след теряется. 

Т.е., если мы делаем load, то исходник показывается в top-level формах, например, если 
мы напишем в файле (break) и будем его загружать, то из отладчика мы попадём в это место. 
А вот если мы напишем (defun kaka () (break)) и загрузим load-ом эту функцию, то мы не увидим это 
место в отладчике, вызвав функцию kaka. По той причине, что на стеке находится не сама интерпретируемая
функция, а элементы интерпретатора. Однако 

(sb-eval::INTERPRETED-FUNCTION-SOURCE-LOCATION #'kaka) при этом вернёт что-то. Значит, у нас всё не так уж и плохо. 

План подключения нахождения исходника для интерп.ф-ии:
- проверить, не сделано ли оно уже в SWANK? - нет. 
- запоминать в %%eval, какую именно ф-ю мы выполняем. 
- когда надо найти исходник, строим хеш-таблицу и пользуемся ей в мирных целях. 

(и заодно - надо бы улучшить хождение по скомпилированному коду, но это другая работа). 

а также надо подумать о лучшей поддержке макросов. 






