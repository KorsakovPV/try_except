https://docs.python.org/release/2.5/lib/module-contextlib.html

Функция contextmanager из библиотеки contextlib впервые появилась, как и with в Python 2.5. В 2006 году в ней было 
всего три метода. С тех пор она значительно разрослась и все ее методы в том или ином виде помогают работать с 
менеджером контекста.

Сontextmanager это еще один синтаксический сахар для работы с контекстом. У нее есть еще асинхронная версия она 
практически ни чем не отличается.

Если посмотреть ее код там есть краткий и понятный докстринг.

```python
def contextmanager(func):
    """@contextmanager decorator.

    Typical usage:

        @contextmanager
        def some_generator(<arguments>):
            <setup>
            try:
                yield <value>
            finally:
                <cleanup>

    This makes this:

        with some_generator(<arguments>) as <variable>:
            <body>

    equivalent to this:

        <setup>
        try:
            <variable> = <value>
            <body>
        finally:
            <cleanup>
    """
    @wraps(func)
    def helper(*args, **kwds):
        return _GeneratorContextManager(func, args, kwds)
    return helper
```

То есть contextmanager это функция декоратор который параметром получает генератор (для возврата значения используется 
yield). Этот генератор возвращает только одно значение. Функция contextmanager возвращает объект класса 
_GeneratorContextManager. У этого объекта реализованны методы __enter__ и __exit__. И его можно использовать в 
менеджере контекста with.

Рекомендую посмотреть код класса _GeneratorContextManager и классов от которых он наследован. Код сложноват для джуна, 
но содержит много комментариев и я думаю крепкий джун сможет его отдебажить и разобраться в нем.

От себя добавлю что собака в декораторе это тоже питоновский синтаксический сахар. И иногда удобнее декорировать 
функцию не там где мы ее определяем, а там где мы ее вызываем и тогда докстринг можно немного поправить

```python
def contextmanager(func):
    """@contextmanager decorator.

    Typical usage:

        def some_generator(<arguments>):
            <setup>
            try:
                yield <value>
            finally:
                <cleanup>

    This makes this:

        with contextmanager(some_generator)(<arguments>) as <variable>:
            <body>

    equivalent to this:

        <setup>
        try:
            <variable> = <value>
            <body>
        finally:
            <cleanup>
    """
```
