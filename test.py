from textual import App, Button, TextInput, events
import configparser


class MyConfigApp(App):

    async def on_mount(self) -> None:
        # Создание кнопки и текстового поля
        button = Button("Load INI Data", name="load_button")
        text_input = TextInput(name="text_input")

        # Добавление виджетов на экран
        await self.view.dock(button, edge="top")
        await self.view.dock(text_input, edge="top")

    async def on_event(self, event: events.Event) -> None:
        if isinstance(event, events.ButtonClick):
            if event.sender.name == "load_button":
                self.load_ini_data()

    def load_ini_data(self):
        # Загрузка данных из INI-файла
        config = configparser.ConfigParser()
        config.read('config.ini')

        # Получение данных из файла и установка в текстовом поле
        data = config['section']['key']
        text_input = self.view.find_by_name("text_input")
        text_input.value = data


if __name__ == "__main__":
    MyConfigApp.run()
