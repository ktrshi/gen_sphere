from rich.text import Text
from rich.segment import Segment

import os

from textual.app import App, ComposeResult, RenderResult
from textual import events, on
from textual.strip import Strip
from textual.containers import Vertical, Horizontal
from textual.containers import Center
from textual.widget import Widget
from textual.widgets import Button, Footer, Header, Static, Label, Input, ContentSwitcher, Log, DataTable

ROWS = [
    ("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
    (0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
    (10, 11, 12, 13, 14, 15, 16, 17, 18, 19),
    (20, 21, 22, 23, 24, 25, 26, 27, 28, 29),
    (30, 31, 32, 33, 34, 35, 36, 37, 38, 39),
    (40, 41, 42, 43, 44, 45, 46, 47, 48, 49),
    (50, 51, 52, 53, 54, 55, 56, 57, 58, 59),
    (60, 61, 62, 63, 64, 65, 66, 67, 68, 69),
    (70, 71, 72, 73, 74, 75, 76, 77, 78, 79),
    (80, 81, 82, 83, 84, 85, 86, 87, 88, 89),
    (90, 91, 92, 93, 94, 95, 96, 97, 98, 99)
]


class Configuration:

    def __init__(self, config_path='input.ini'):
        self.config_path = config_path
        self.config = None

        self.params = {'particle': '', 'e': '', 'th': '', 'atm': '', 'q': '', 'start': '', 'end': ''}
        self.part = self.params['particle']
        self.e = self.params['e']
        self.th = self.params['th']
        self.atm = self.params['atm']
        self.q = self.params['q']
        self.start = self.params['start']
        self.end = self.params['end']

        self.file_names = {'clout': '', 'dir': '', 'phels': '', 'moshits': ''}
        self.clout = self.file_names['clout']
        self.dir = self.file_names['dir']
        self.phels = self.file_names['phels']
        self.moshits = self.file_names['moshits']

        self.read_config()
        self.get_parameters()
        self.get_filenames()

    def read_config(self):
        import configparser
        self.config = configparser.ConfigParser()
        self.config.read(self.config_path)

    def get_parameter(self, parameter_name, default_section='DEFAULT'):
        return self.config[default_section][parameter_name]

    def get_parameters(self):
        self.part = self.get_parameter('PRMPAR')
        self.e = self.get_parameter('E')
        self.th = self.get_parameter('THETAP')
        self.atm = self.get_parameter('ATMOD')
        self.q = self.get_parameter('QMOD')
        self.start = self.get_parameter('START')
        self.end = self.get_parameter('END')

    def get_filenames(self):
        self.clout = f'CLOUT4w_Q{self.q}_atm{self.atm:02}_{self.part:04}_{self.e}PeV_{self.th:02}_'
        self.dir = f'Q{self.q}_atm{self.atm:02}_{self.part:04}_{self.e}PeV_{self.th:02}_'
        self.phels = f'phels_to_trace_Q{self.q}_atm{self.atm:02}_{self.part:04}_{self.e}PeV_{self.th:02}_'
        self.moshits = f'moshits_Q{self.q}_atm{self.atm:02}_{self.part:04}_{self.e}PeV_{self.th:02}_'


class ConfigText(Widget):

    BORDER_TITLE = "Параметры запуска"

    def compose(self) -> ComposeResult:
        with Vertical():
            with Vertical():
                yield Label("Частица", classes="config_label")
                yield Input(value="", classes="config_input", id="part")
            with Vertical():
                yield Label("Энергия", classes="config_label")
                yield Input(value="", classes="config_input", id="e")
            with Vertical():
                yield Label("Угол", classes="config_label")
                yield Input(value="", classes="config_input", id="th")
            with Vertical():
                yield Label("Модель атмосферы", classes="config_label")
                yield Input(value="", classes="config_input", id="atm")
            with Vertical():
                yield Label("Модель взаимодействия", classes="config_label")
                yield Input(value="", classes="config_input", id="q")
            with Vertical():
                yield Label("Номер начального файла", classes="config_label")
                yield Input(value="", classes="config_input", id="start")
            with Vertical():
                yield Label("Номер конечного файла", classes="config_label")
                yield Input(value="", classes="config_input", id="end")
            yield Button("Чтение конфигурации", id="input_read")

    def on_mount(self) -> None:
        config = Configuration()
        self.query_one("#part").value = config.part
        self.query_one("#e").value = config.e
        self.query_one("#th").value = config.th
        self.query_one("#atm").value = config.atm
        self.query_one("#q").value = config.q
        self.query_one("#start").value = config.start
        self.query_one("#end").value = config.end

    @on(Button.Pressed, "#input_read")
    def read_ini_file(self):
        config = Configuration()
        self.query_one("#part").value = config.part
        self.query_one("#e").value = config.e
        self.query_one("#th").value = config.th
        self.query_one("#atm").value = config.atm
        self.query_one("#q").value = config.q
        self.query_one("#start").value = config.start
        self.query_one("#end").value = config.end


class MainFields(Widget):

    CSS = """
        Screen {
            align: center middle;
        }
        """

    def compose(self) -> ComposeResult:
        with Horizontal(id="tabs"):
            yield Button("Clout and Phels", id="input_stats")
            yield Button("Mosaic", id="mosaic_stats")
            yield Button("Logs", id="logs")
        with ContentSwitcher(initial="clout_stats", classes="box"):
            with Vertical(id="input_stats"):
                yield Label("Наличие CLOUT-файлов")
                yield Label("Можно выбрать номер")
                table = DataTable(id="clout_stats")
                table.zebra_stripes = True
                table.show_header = False
                table.add_columns(*ROWS[0])
                for row in ROWS[1:]:
                    table.add_row(*row)
                table.styles.width = "auto"
                yield table

                yield Label("Наличие phels-файлов")
                table = DataTable(id="phels_stats")
                table.zebra_stripes = True
                table.show_header = False
                table.add_columns(*ROWS[0])
                for row in ROWS[1:]:
                    table.add_row(*row)
                table.styles.width = "auto"
                yield table
            with Vertical(id="mosaic_stats"):
                yield Label("Наличие CLOUT-файлов")
                yield Label("Можно выбрать номер")
                table = DataTable(id="clout_stats")
                table.zebra_stripes = True
                table.show_header = False
                table.add_columns(*ROWS[0])
                for row in ROWS[1:]:
                    table.add_row(*row)
                table.styles.width = "auto"
                yield table

                yield Label("Наличие mosaic-файлов")
                table = DataTable(id="mosaic_stats")
                table.zebra_stripes = True
                table.show_header = False
                table.add_columns(*ROWS[0])
                for row in ROWS[1:]:
                    table.add_row(*row)
                table.styles.width = "auto"
                yield table

            with Vertical(id="logs"):
                yield Label("Fortran")
                yield Log(id="fortran_logs")
                yield Label("Geant 4")
                yield Log(id="geant_logs")

        with Horizontal(id="buttons_main"):
            yield Center(Button("Старт", id="start"))
            yield Center(Button("Стоп", id="stop"))
            yield Center(Button("Проверка файлов", id="check"))
            yield Center(Button("Удалить ненужные", id="delete"))

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id in ["input_stats", "mosaic_stats", "logs"]:
            self.query_one(ContentSwitcher).current = event.button.id

    # def on_mount(self, event: events.Mount) -> None:



class GenerationApp(App):

    CSS_PATH = "layout.tcss"
    TITLE = "Генерации образов Мозаики"
    BINDINGS = [
        ("ctrl+c", "quit", "Выйти из приложения")
    ]

    def compose(self) -> ComposeResult:
        yield Header()
        yield ConfigText(id="config")
        yield MainFields()
        yield Footer()

    # @on(Button.Pressed, "#input_read")

    def action_quit(self) -> None:
        self.exit()


if __name__ == "__main__":
    app = GenerationApp()
    app.run()
