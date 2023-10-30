from pytermgui import Container, Prompt, alert, run_app

def main():
    # Создание основного контейнера
    container = Container()

    # Функция-обработчик кнопки
    def on_button_click(value):
        alert(f"Вы выбрали: {value}")

    # Диалоговое окно с выбором
    prompt = Prompt(
        "Выберите ваш любимый фрукт:",
        ["Яблоко", "Банан", "Виноград"],
        callback=on_button_click
    )

    container.add(prompt)

    # Запуск приложения
    run_app(container)

if __name__ == "__main__":
    main()

