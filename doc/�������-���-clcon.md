мы не в состоянии отлаживаться в среде, потому что она всё время виснет. 
Пытаемся отлаживаться с помощью (консольной) консоли SBCL, минуя swank. Для этого добавлена соответствующая колонка в список тредов. Если для вас это работает, то этот документ можно не читать. Если не работает - то тут описан процесс достижения отлаживаемости в консоли SBCL.

Вроде сработал вариант с переменной (setf clco:*globally-disable-sldb* t), но вывод идёт в swank. Как убрать?

Добавил соответствующий код в патченную swank::debug-in-emacs.

Для отладки repl-thread нужно действовать так:
clcon> (/ 0)
в чёрной консоли появляется запрос на предоставление доступа:
debugger invoked on a DIVISION-BY-ZERO in thread
#<THREAD "repl-thread" RUNNING {243D3C91}>:
и т.п.

В консоли нужно выполнить (sb-thread::release-foreground) и мы уступим консоль этому треду. По завершению отладки он её вернёт главному треду (проверить текущий тред можно,
вычислив sb-thread:*current-thread* )

(То, что надо нажать Enter - это фича )

Пытаемся теперь прервать поток:
запишем в глоб.перем aaa - repl-thread

(setf clco::*globally-disable-sldb* t)
в чёрной консоли:
(sb-thread:release-foreground aaa)
в REPL:
(sb-thread:interrupt-thread aaa #'break)
он показывается. Когда он нам надоел и отладчик закончил работу, то в REPL пишем
(sb-thread:release-foreground) и главный тред снова может работать в консоли. 

А что если это не repl-thread? Тогда нас может спасти то, что REPL не заблокирован и мы можем из него вызвать: 
(sb-thread:interrupt-thread aaa #'sb-thread:release-foreground)

Другой вариант:
(setf clco::*globally-disable-sldb* t)
в чёрной консоли:
(sb-thread:release-foreground aaa)
в REPL:
(sb-thread:interrupt-thread aaa (clco::make-caller-releasing-foreground 'break))
Тогда после выхода из отладчика *repl-thread* восстановится автоматически.

Ну и наконец, собираем всё вместе:
(clco::break-thread-in-black-console интересующий тред)
хотя тут странно, что нужно много раз нажимать Enter, пока он соизволит что-то показать. 
Или это с каждым новым condition-ом скапливаются нужные нажатия. 



