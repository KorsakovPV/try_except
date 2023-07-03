В Python 2.5 впервые появился новый оператор связанный с исключениями with. Он задумывался как альтернатива try ... finally

Если посмотреть что докуменация пишет про него то все магия улетучивается в один момент.
Этот код
```python
with EXPRESSION as TARGET:
    SUITE
```

Соответствует такому
```python
manager = (EXPRESSION)
enter = type(manager).__enter__
exit = type(manager).__exit__
value = enter(manager)
hit_except = False

try:
    TARGET = value
    SUITE
except:
    hit_except = True
    if not exit(manager, *sys.exc_info()):
        raise
finally:
    if not hit_except:
        exit(manager, None, None, None)
```

Я бы даже его упростил чоб было проще понять магию.

```python
manager = (EXPRESSION)
enter = type(manager).__enter__
exit = type(manager).__exit__
value = enter(manager)

try:
    TARGET = value
    SUITE
finally:
    exit(manager, *sys.exc_info())
```

У экземпляра класса вызываем метод __enter__. В конструкции try выполняем код, который находится под with. Как бы не 
отработал код всегда вызываем метод __exit__ из finally. И райзим ошибку если генерируется исключение. 
Все, ни какой магии. Но кода с with в 5 раз меньше.  

Чтоб with мог работать с нашим классом нам нужно реализовать 2 метода __enter__ и __exit__, или для асинхронной версии 
async with нужно реализовать __aenter__ и __aexit__. Данная концепция прижилась, и уже 17 лет является стандартом.
