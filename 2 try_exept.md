https://docs.python.org/release/1.4/ref/ref7.html#REF13811

Забегая вперед под капотом конструкции with и contextmanager обычный try ... except.

Конструкция try ... except появилась в Python очень давно. Я еще не программировал, а конструкция уже была. Она 
досталась ему от тех языков которые оказали на него свое влияние (Ada, C++, Java) имеют свои механизмы обработки 
исключений очень похожие на Python.

ADA https://pro-prof.com/forums/topic/ada_lesson_14
```ada
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
 
procedure main is
    x, y, z : Integer;
begin
    Put("X = ");
    Get(x);
    Put("Y = ");
    Get(y);
 
    z := x / y;
    Put_Line(Integer'Image(x) & " / " & Integer'Image(y) & " = " & Integer'Image(z));
 
    exception
        when Data_Error => Put_Line("Цифирь давай, цифирь!");
        when Constraint_Error => Put_Line("Я такое не ем!");
        when others => Put_Line("Что-то пошло не так!");
end main;
```

C++ https://metanit.com/cpp/tutorial/6.1.php
```cpp
double divide(int a, int b)
{
    if (b)
        return a / b;
    throw "Division by zero!";
}
  
int main()
{
    int x{500};
    int y{};
     
    try
    {
        double z {divide(x, y)};
        std::cout << z << std::endl;
    }
    catch (const char* error_message)
    {
        std::cout << error_message << std::endl;
    }
    std::cout << "The End..." << std::endl;
}
```

JAVA https://javarush.com/groups/posts/1943-iskljuchenija-perekhvat-i-obrabotka
```java
public static void main(String[] args) throws IOException {
   try {
       BufferedReader reader = new BufferedReader(new FileReader("C:\\Users\\Username\\Desktop\\test.txt"));
       String firstString = reader.readLine();
       System.out.println(firstString);
   } catch (ArithmeticException e) {

       System.out.println("Ошибка! Файл не найден!");
   }
}
```

На сайте https://docs.python.org самую старую документацию которую удалось найти это 1.4 от 25 октября 1996 года.
Дока говорит что существует две формы выражения try: 1 try...except [else] и 2 try...finally. Эти формы нельзя 
смешивать, но они могут быть вложены друг в друга. Первый вариант это обработчик исключений, а второй нужен для 
гарантированного выполнения раздела finally.

Так жили 10 лет пока 19 сентября 2006 не появился Python 2.5. С этого момента стало возможным совместное использование 
else, except и finally. А строковые исключения стали Deprecation.

С тех пор ничего не поменялась. Разве что ошибки теперь обязательно наследуются от класса Exception. Актуальная дока 
предлагает такой код.

https://docs.python.org/3/tutorial/errors.html#errors-and-exceptions

```python
def divide(x, y):
    try:
        result = x / y
    except ZeroDivisionError:
        print("division by zero!")
    else:
        print("result is", result)
    finally:
        print("executing finally clause")
```

От себя добавлю что если несколько ошибок обрабатываются одинокого их можно завернуть в кортеж 
```python
except (ZeroDivisionError, ValueError):
```
и в одном try может быть несколько except
```python
    try:
        result = x / y
    except ZeroDivisionError:
        print("division by zero!")
    except Exception:
        print("unexpected error!")
```

В Python использование исключения не приводит к значительным накладным расходам (а зачастую даже позволяет ускорить 
исполнение программ) и по этому очень широко используется.
