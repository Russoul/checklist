# checklist

## Правила

* checklist может содержать следующие выражения: строки, дефиниции функций, параметров; ввод, вывод из потока;
  вызовы функций, обращения(вставка) к параметрам, интерполяторы, условия, подзаголовки
* подзаголовки(entry) могут содержать все что checklist, кроме дефиниций функций
* все внутренние по отнощению к подзаголовкам, условиям(телу if и else), дефинициям функций выражения должны быть
  табулированы(используя только ПРОБЕЛЫ !), учитывается табуляция только по отношению к родителю, между собой(между выражениями внутри родителя)
  табуляция может быть любой
* пустые строки не учитываются
* дефиниции функций могут содержать все что подзаголовки, кроме самих подзаголовков
* все выражения читаются построчно
* тела условий могут содеражать все что дефиниции функций


Примеры варажений:

```
##это название чеклиста//самая первая строка в чеклисте
строка и символы () <<<===>>> //это комментарий(начинается с `//`), все что слева от `//` это одна строка, она будет распечатана как есть
//в строках запрещены символы $ # { } \n и наборы символов <- ->

любые
такие
строки
будут
распечатаны
как
есть


$$моя_функция(аргумент1, аргумент2)
    //тело табулируется
    любые
    такие
    строки
    будут
    распечатаны
    как
    есть
    $аргумент1 //
    $аргумент2 //обращение(вставка аргументов `аргументX`)
это уже не тело функции
$моя_функция("мой аргумент", "еще один")//при вызове функций и еще в нескольких случаях строки нужно записывать в `" "`
//также в этих же случаях можно записывать числа и булевы значения без `" "`
//например
$моя_функция(2.56, true)

//условия:

$if{100 % 2 == 0}
    100 это четное число
$else
    100 это нечетное число
    
    
//внутри { } у условий запись такая же как при вызове функций, тоесть строки нужно записывать в `" "` можно использовать числа,
//булевы значения и как видно и примера - встроенные операторы
//полный список того что можно в таких случаях:
//строки,числа,булевы значения, операторы, вызов функций(уже без `$` впереди), вставка значений(тоже без доллара)
//сложное выражение внутри { } должно упрощатся к булевому значению, иначе это ошибка интерпретации
//символы `if` и `else` долны быть одинаковой табуляции чтобы быть связаными (else не может существовать без if) 
//встроенные бинарные операторы - это просто функции и их можно вызывать как функции:
$<=(1,2)
//это равносильно
${1 <= 2} //это интеполятор
//сразу примеры его использования:
моя обычная строка ${2 + 3 * 5 == 17 && (2+3) * 5 == 25} << это было вставлено прямо сюда
//встроенные унарные операторы тоже могут быть вызваны как функции:
${unary_-(1) == -1}
${unary_!(true) == !true}
``` 