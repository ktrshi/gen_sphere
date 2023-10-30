from rich.text import Text
from rich.segment import Segment

import shutil
import os
import glob
import pandas as pd
import gzip
import subprocess

from textual.app import App, ComposeResult, RenderResult
from textual import events, on, work
from textual.coordinate import Coordinate
from textual.message_pump import MessagePump
from textual.reactive import reactive
from textual.validation import Validator, ValidationResult
from textual.containers import Vertical, Horizontal, VerticalScroll, Center, Grid
from textual.widget import Widget
from textual.widgets import Button, Footer, Header, Static, Label, Input, ContentSwitcher, Log, DataTable, Pretty
from app_utility import Configuration
from textual.worker import Worker, get_current_worker

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

LAST_PHELS = 0
CLOUT_RANGE: list[tuple[int, int]] = []
START = 0
END = 100
PARTICLES = {"P": "14", "He": "402", "N": "1407", "AL": "2613", "S": "3216", "Fe": "5626"}
ENERGY = {"5 PeV": "5", "10 PeV": "10", "30 PeV": "30", "100 PeV": "100"}
ATM = {"1": "1", "3": "3", "4": "4", "11": "11"}
Q = {"QGSJET01": "1", "QGSJETII04": "2"}
THETA = {"5\u00b0": "5", "10\u00b0": "10", "15\u00b0": "15", "20\u00b0": "20", "25\u00b0": "25", "30\u00b0": "30"}
config = Configuration()


def find_position(value):
    position = None
    for row_idx, row in enumerate(ROWS[1:]):
        for col_idx, val in enumerate(row):
            if val == value:
                position = (row_idx, col_idx)
                break
        if position is not None:
            break
    return position


def create_table(table):
    table.show_header = False
    table.add_columns(*ROWS[0])
    for row in ROWS[1:]:
        styled_row = [
            Text(str(cell), style="Bold #AC0303", justify="left") for cell in row
        ]
        table.add_row(*styled_row)
    table.styles.width = "auto"
    table.styles.height = "auto"


def reset_table(table):
    table.clear(True)
    create_table(table)


def find_consecutive_trues(column):
    s = column.cumsum()
    groups = (column != column.shift()).cumsum()
    consecutive_ranges = s.groupby(groups).transform('max') - s.groupby(groups).transform('min') + column
    ranges = consecutive_ranges[consecutive_ranges > 0].index.tolist()
    if not ranges:
        return []
    starts = ranges[:-1]
    ends = ranges[1:]
    if column.iloc[0]:
        starts = [ranges[0]] + starts
    if column.iloc[-1]:
        ends = ends + [ranges[-1]]
    return list(zip(starts, ends))


class CustomInput(Input):
    validity = reactive(False)


class Particle(Validator):
    def validate(self, value: str) -> ValidationResult:
        if value in ["14", "402", "1407", "2613", "3216", "5626"]:
            return self.success()
        else:
            return self.failure("Неверный номер ядра")


class Energy(Validator):
    def validate(self, value: str) -> ValidationResult:
        if value in ["5", "10", "30", "100"]:
            return self.success()
        else:
            return self.failure("Неверная энергия")


class Theta(Validator):
    def validate(self, value: str) -> ValidationResult:
        if value in ["5", "10", "15", "20", "25", "30"]:
            return self.success()
        else:
            return self.failure("Неверный угол")


class Atm(Validator):
    def validate(self, value: str) -> ValidationResult:
        if value in ["1", "3", "4", "11"]:
            return self.success()
        else:
            return self.failure("Неверная модель атмосферы")


class QValidation(Validator):
    def validate(self, value: str) -> ValidationResult:
        if value in ["1", "2"]:
            return self.success()
        else:
            return self.failure("Неверная модель взаимодействия")


class Start(Validator):
    def validate(self, value: str) -> ValidationResult:
        global END
        try:
            int(value)
        except ValueError:
            return self.failure("Начальный номер не может быть строкой")
        if value in [f"{i}" for i in range(0, 100)] and int(value) < int(END):
            return self.success()
        elif int(value) > int(END):
            return self.failure("Начальный номер не может быть больше конечного")
        else:
            return self.failure("Неверный номер начального файла")


class End(Validator):
    def validate(self, value: str) -> ValidationResult:
        global START
        try:
            int(value)
        except ValueError:
            return self.failure("Начальный номер не может быть строкой")
        if value in [f"{i}" for i in range(0, 100)] and int(value) > int(START):
            return self.success()
        elif int(value) < int(START):
            return self.failure("Конечный номер не может быть меньше начального")
        else:
            return self.failure("Неверный номер конечного файла")


class ConfigText(Widget):
    BORDER_TITLE = "Параметры запуска"

    def compose(self) -> ComposeResult:
        with VerticalScroll():
            yield Label("Частица", classes="config_label")
            yield CustomInput(placeholder="14, 402...",
                              validators=[
                                  Particle(),
                              ],
                              classes="config_input", id="part")
            yield Pretty([], id="part_prt")
            yield Label("Энергия", classes="config_label")
            yield CustomInput(placeholder="5, 10...",
                              validators=[
                                  Energy(),
                              ],
                              classes="config_input", id="e")
            yield Pretty([], id="e_prt")
            yield Label("Угол", classes="config_label")
            yield CustomInput(placeholder="5, 10...",
                              validators=[
                                  Theta(),
                              ],
                              classes="config_input", id="th")
            yield Pretty([], id="th_prt")
            yield Label("Модель атмосферы", classes="config_label")
            yield CustomInput(placeholder="1, 3, 4, 11",
                              validators=[
                                  Atm(),
                              ],
                              classes="config_input", id="atm")
            yield Pretty([], id="atm_prt")
            yield Label("Модель взаимодействия", classes="config_label")
            yield CustomInput(placeholder="1, 2",
                              validators=[
                                  QValidation(),
                              ],
                              classes="config_input", id="q")
            yield Pretty([], id="q_prt")
            yield Label("Номер начального файла", classes="config_label")
            yield CustomInput(placeholder="1, 2, 3...",
                              validators=[
                                  Start(),
                              ],
                              classes="config_input", id="start")
            yield Pretty([], id="start_prt")
            yield Label("Номер конечного файла", classes="config_label")
            yield CustomInput(placeholder="1, 2, 3...",
                              validators=[
                                  End(),
                              ],
                              classes="config_input", id="end")
            yield Pretty([], id="end_prt")
        yield Center(Pretty([], id="buttons_status"))
        with Horizontal(id="buttons_config"):
            yield Button("Чтение конфигурации", id="input_read")
            yield Button("Запись конфигурации", id="input_write")

    def on_mount(self) -> None:
        global START, END
        self.query_one("#part").value = config.part
        self.query_one("#e").value = config.e
        self.query_one("#th").value = config.th
        self.query_one("#atm").value = config.atm
        self.query_one("#q").value = config.q
        self.query_one("#start").value = config.start
        START = self.query_one("#start").value
        self.query_one("#end").value = config.end
        END = self.query_one("#end").value
        self.query_one("#buttons_config").styles.height = "0.1fr"
        self.query_one("#input_read").styles.width = 10
        self.query_one("#input_write").styles.width = 10

    def on_input_changed(self, event: Input.Changed) -> None:
        if event.control == self.query_one("#start"):
            global START
            START = event.value
        if event.control == self.query_one("#end"):
            global END
            END = event.value

    @on(Input.Changed)
    def show_invalid_reasons(self, event: Input.Changed) -> None:
        global START, END, CLOUT_RANGE
        if event.control == self.query_one("#part"):
            if not event.validation_result.is_valid:
                self.query_one("#part_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#part").validity = False
            else:
                key = next((k for k, v in PARTICLES.items() if v == event.value), None)
                self.query_one("#part_prt").update(key)
                self.query_one("#part").validity = True
        if event.control == self.query_one("#e"):
            if not event.validation_result.is_valid:
                self.query_one("#e_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#e").validity = False
            else:
                key = next((k for k, v in ENERGY.items() if v == event.value), None)
                self.query_one("#e_prt").update(key)
                self.query_one("#e").validity = True
        if event.control == self.query_one("#th"):
            if not event.validation_result.is_valid:
                self.query_one("#th_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#th").validity = False
            else:
                key = next((k for k, v in THETA.items() if v == event.value), None)
                self.query_one("#th_prt").update(key)
                self.query_one("#th").validity = True
        if event.control == self.query_one("#atm"):
            if not event.validation_result.is_valid:
                self.query_one("#atm_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#atm").validity = False
            else:
                key = next((k for k, v in ATM.items() if v == event.value), None)
                self.query_one("#atm_prt").update(key)
                self.query_one("#atm").validity = True
        if event.control == self.query_one("#q"):
            if not event.validation_result.is_valid:
                self.query_one("#q_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#q").validity = False
            else:
                key = next((k for k, v in Q.items() if v == event.value), None)
                self.query_one("#q_prt").update(key)
                self.query_one("#q").validity = True
        if event.control == self.query_one("#start"):
            if not event.validation_result.is_valid:
                self.query_one("#start_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#start").validity = False
            else:
                self.query_one("#start_prt").update(event.value)
                self.query_one("#start").validity = True
                if CLOUT_RANGE:
                    if int(event.value) < CLOUT_RANGE[0][0]:
                        self.parent.query_one("#start_main").disabled = True
                        self.parent.query_one("#start_status_label").update(Text("Недостаточно CLOUT-файлов",
                                                                                 style="#d20f39"))
                    else:
                        self.parent.query_one("#start_main").disabled = False
                        self.parent.query_one("#start_status_label").update(Text("Можно запускать",
                                                                                 style="#40a02b"))
        if event.control == self.query_one("#end"):
            if not event.validation_result.is_valid:
                self.query_one("#end_prt").update(event.validation_result.failure_descriptions)
                self.query_one("#end").validity = False
            else:
                self.query_one("#end_prt").update(event.value)
                self.query_one("#end").validity = True
                if CLOUT_RANGE:
                    if int(event.value) > CLOUT_RANGE[-1][1]:
                        self.parent.query_one("#start_main").disabled = True
                        self.parent.query_one("#start_status_label").update(Text("Недостаточно CLOUT-файлов",
                                                                                 style="#d20f39"))
                    else:
                        self.parent.query_one("#start_main").disabled = False
                        self.parent.query_one("#start_status_label").update(Text("Можно запускать",
                                                                                 style="#40a02b"))
        condition = self.query_one("#part").validity and self.query_one("#e").validity and self.query_one(
            "#th").validity and \
                    self.query_one("#atm").validity and self.query_one("#q").validity and self.query_one(
            "#start").validity and \
                    self.query_one("#end").validity
        if condition:
            self.query_one("#input_write").disabled = False
            self.query_one("#buttons_status").update("Верные параметры")
        else:
            self.query_one("#input_write").disabled = True
            self.query_one("#buttons_status").update("Неверные параметры")

    @on(Button.Pressed, "#input_read")
    def read_ini_file(self):
        self.query_one("#part").value = config.part
        self.query_one("#e").value = config.e
        self.query_one("#th").value = config.th
        self.query_one("#atm").value = config.atm
        self.query_one("#q").value = config.q
        self.query_one("#start").value = config.start
        self.query_one("#end").value = config.end

    @on(Button.Pressed, "#input_write")
    def write_ini_file(self):
        self.query_one("#buttons_status").update("Верные параметры")
        config.part = self.query_one("#part").value
        config.e = self.query_one("#e").value
        config.th = self.query_one("#th").value
        config.atm = self.query_one("#atm").value
        config.q = self.query_one("#q").value
        config.start = self.query_one("#start").value
        config.end = self.query_one("#end").value
        config.write_config()


class MainFields(Widget):
    CSS = """
        Screen {
            align: center middle;
        }
        """

    def compose(self) -> ComposeResult:
        with Horizontal(id="tabs"):
            yield Button("Files", id="files_stats")
            yield Button("Logs", id="logs")
        with ContentSwitcher(initial="files_stats", classes="box"):
            with Grid(id="files_stats"):
                with VerticalScroll(id="files_stats_left"):
                    yield Label(Text("Наличие CLOUT-файлов", style="Bold #7287fd"))
                    table = DataTable(id="clout_stats_dt")
                    create_table(table)
                    yield table
                with VerticalScroll(id="phels_stats"):
                    yield Label(Text("Наличие phels-файлов", style="Bold #7287fd"))
                    table = DataTable(id="phels_stats_dt")
                    create_table(table)
                    yield table
                with VerticalScroll(id="mosaic_stats"):
                    yield Label(Text("Наличие mosaic-файлов", style="Bold #7287fd"))
                    table = DataTable(id="moshits_stats_dt")
                    create_table(table)
                    yield table
                with VerticalScroll(id="dir_names"):
                    global config
                    yield Label(Text("Названия файлов", style="Bold #7287fd"))
                    yield Label(Text("CLOUT-файлы: ", style="Bold #dc8a78"))
                    yield Pretty(config.clout, id="clout_file_name", classes="file_name")
                    yield Label(Text("Папка для результата: ", style="Bold #dc8a78"))
                    yield Pretty(config.dir, id="dir_file_name", classes="file_name")
                    yield Label(Text("Phels-файлы: ", style="Bold #dc8a78"))
                    yield Pretty(config.phels, id="phels_file_name", classes="file_name")
                    yield Label(Text("Moshits-файлы: ", style="Bold #dc8a78"))
                    yield Pretty(config.moshits, id="moshits_file_name", classes="file_name")

            with Vertical(id="logs"):
                yield Label("Fortran")
                yield Log(id="fortran_logs")
                yield Label("Geant 4")
                yield Log(id="geant_logs")

        with Horizontal(id="buttons_main"):
            with Vertical():
                yield Center(Label(Text("Сначала проверьте файлы", style="#d20f39"), id="start_status_label"))
                yield Center(Button("Старт", id="start_main"))
            with Vertical():
                yield Center(Label(Text("Не запущено", style="#d20f39"), id="stop_status_label"))
                yield Center(Button("Стоп", id="stop_main"))
            with Vertical():
                yield Center(Label(Text("Проверьте наличие файлов", style="#40a02b"), id="stop_status_label"))
                yield Center(Button("Проверка файлов", id="check_main"))
            with Vertical():
                yield Center(Label(Text("Не реализовано", style="#df8e1d"), id="stop_status_label"))
                yield Center(Button("Удалить ненужные", id="delete_main"))

    def on_mount(self, event: events.Mount) -> None:
        self.query_one(ContentSwitcher).current = "files_stats"
        self.query_one("#start_main").disabled = True
        self.query_one("#stop_main").disabled = True
        self.query_one("#delete_main").disabled = True
        self.query_one("#clout_file_name").update(config.clout)
        self.query_one("#dir_file_name").update(config.dir)
        self.query_one("#phels_file_name").update(config.phels)
        self.query_one("#moshits_file_name").update(config.moshits)

    def on_button_pressed(self, event: Button.Pressed) -> None:
        global CLOUT_RANGE, START, END
        if event.button.id in ["files_stats", "logs"]:
            self.query_one(ContentSwitcher).current = event.button.id
        if event.button.id == "check_main":
            path = os.getcwd()
            file_pattern = [f"{config.clout}[0-9][0-9][0-9].gz",
                            f"{config.clout}[0-9][0-9][0-9]"]
            columns = ["clout", "clout_process"]
            df = pd.DataFrame(False, index=range(100), columns=columns)
            for i, pattern in enumerate(file_pattern):
                match i:
                    case 0:
                        files = glob.glob(os.path.join(path + "/CLOUT/", pattern))
                        for file in files:
                            index = int(os.path.basename(file).rsplit('_')[-1][:3])
                            df.loc[index, columns[i]] = True
                    case 1:
                        files = glob.glob(os.path.join(path, pattern))
                        for file in files:
                            index = int(os.path.basename(file).rsplit('_')[-1][:3])
                            df.loc[index, columns[i]] = True
            CLOUT_RANGE = find_consecutive_trues(df["clout"])
            for item in CLOUT_RANGE:
                if int(START) <= item[0] and item[1] <= int(END):
                    self.query_one("#start_main").disabled = False
                    self.query_one("#start_status_label").update(Text("Можно запускать", style="#40a02b"))
            true_positions = df[df].stack().index.tolist()
            for item in true_positions:
                if item[1] == "clout":
                    pos = find_position(item[0])
                    cords = Coordinate(pos[0], pos[1])
                    self.query_one("#clout_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #40a02b"))
                if item[1] == "clout_process":
                    pos = find_position(item[0])
                    cords = Coordinate(pos[0], pos[1])
                    self.query_one("#clout_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #df8e1d"))
        if event.button.id == "start_main":
            self.query_one("#start_main").disabled = True
            self.query_one("#stop_main").disabled = True
            self.query_one("#check_main").disabled = True
            self.query_one("#start_status_label").update(Text("Запущено", style="#40a02b"))
            self.run_process()

    @on(DataTable.CellSelected, "#clout_stats_dt")
    def check_related_files(self, event: DataTable.CellSelected):
        reset_table(self.query_one("#phels_stats_dt"))
        reset_table(self.query_one("#moshits_stats_dt"))

        idx = str(self.query_one("#clout_stats_dt").get_cell_at(event.coordinate))
        path = os.getcwd()
        file_pattern = [f"{config.phels}{idx.zfill(3)}_?[0-9][0-9][0-9]",
                        f"{config.moshits}{idx.zfill(3)}_?[0-9][0-9][0-9]",
                        f"{config.moshits}{idx.zfill(3)}_?[0-9][0-9][0-9]*"]
        columns = ["phels", "moshits_process", "moshits"]
        df = pd.DataFrame(False, index=range(100), columns=columns)
        for i, pattern in enumerate(file_pattern):
            match i:
                case 0:
                    files = glob.glob(os.path.join(path, pattern))
                    for file in files:
                        index = int(os.path.basename(file).rsplit('_')[-1][1:4])
                        df.loc[index, columns[i]] = True
                case 1:
                    files = glob.glob(os.path.join(path, pattern))
                    for file in files:
                        index = int(os.path.basename(file).rsplit('_')[-1][1:4])
                        df.loc[index, columns[i]] = True
                case 2:
                    files = glob.glob(os.path.join(path + f"/mosaic/{config.dir}{''.zfill(3)}", pattern))
                    for file in files:
                        index = int(os.path.basename(file).rsplit('_')[-1][1:4])
                        df.loc[index, columns[i]] = True
        true_positions = df[df].stack().index.tolist()
        for item in true_positions:
            if item[1] == "phels":
                pos = find_position(item[0])
                cords = Coordinate(pos[0], pos[1])
                self.query_one("#phels_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #40a02b"))
            if item[1] == "moshits_process":
                pos = find_position(item[0])
                cords = Coordinate(pos[0], pos[1])
                self.query_one("#moshits_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #df8e1d"))
            if item[1] == "moshits":
                pos = find_position(item[0])
                cords = Coordinate(pos[0], pos[1])
                self.query_one("#moshits_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #40a02b"))

    def check_related_files_without_event(self, idx):
        global LAST_PHELS
        reset_table(self.query_one("#phels_stats_dt"))
        reset_table(self.query_one("#moshits_stats_dt"))
        path = os.getcwd()
        file_pattern = [f"{config.phels}{idx.zfill(3)}_?[0-9][0-9][0-9]",
                        f"{config.moshits}{idx.zfill(3)}_?[0-9][0-9][0-9]",
                        f"{config.moshits}{idx.zfill(3)}_?[0-9][0-9][0-9]*"]
        columns = ["phels", "moshits_process", "moshits"]
        df = pd.DataFrame(False, index=range(100), columns=columns)
        for i, pattern in enumerate(file_pattern):
            match i:
                case 0:
                    files = glob.glob(os.path.join(path, pattern))
                    for file in files:
                        index = int(os.path.basename(file).rsplit('_')[-1][1:4])
                        df.loc[index, columns[i]] = True
                case 1:
                    files = glob.glob(os.path.join(path, pattern))
                    for file in files:
                        index = int(os.path.basename(file).rsplit('_')[-1][1:4])
                        df.loc[index, columns[i]] = True
                case 2:
                    files = glob.glob(os.path.join(path + f"/mosaic/{config.dir}{''.zfill(3)}", pattern))
                    for file in files:
                        index = int(os.path.basename(file).rsplit('_')[-1][1:4])
                        df.loc[index, columns[i]] = True
            true_positions = df[df].stack().index.tolist()
            values = [value for value, n in true_positions if n == "phels"]
            LAST_PHELS = max(values) + 1 if values else 0
        for item in true_positions:
            if item[1] == "phels":
                pos = find_position(item[0])
                cords = Coordinate(pos[0], pos[1])
                self.query_one("#phels_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #40a02b"))
            if item[1] == "moshits_process":
                pos = find_position(item[0])
                cords = Coordinate(pos[0], pos[1])
                self.query_one("#moshits_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #df8e1d"))
            if item[1] == "moshits":
                pos = find_position(item[0])
                cords = Coordinate(pos[0], pos[1])
                self.query_one("#moshits_stats_dt").update_cell_at(cords, Text(str(item[0]), style="Bold #40a02b"))

    @work(exclusive=True, thread=True)
    def run_process(self) -> bool:
        global START, END, LAST_PHELS
        worker = get_current_worker()
        self.query_one("#start_status_label").update(Text("В процессе", style="#04a5e5"))
        for i in range(int(START), int(END)):
            pos = find_position(i)
            self.query_one("#clout_stats_dt").move_cursor(row=pos[0], column=pos[1])
            self.check_related_files_without_event(str(i))
            if os.path.isfile(f"CLOUT/{config.clout}{str(i).zfill(3)}.gz"):
                shutil.copy(f"CLOUT/{config.clout}{str(i).zfill(3)}.gz", ".")
            self.unzip_clout(i)
            with open('sphall_g4_sh_wbg.in', 'w') as f:
                f.write(f'  {config.atm}                         - catm = CorAtmMod\n'
                        f"{config.clout}{str(i).zfill(3)}    - fn_in\n"
                        '-1000.00                    - zz, m\n'
                        ' 2                          - key: 0-signal only, 1-BG only, >=2-both\n'
                        f' {str(LAST_PHELS)}                          - last_c')
            self.run_fortran()
            self.run_geant()

    def unzip_clout(self, idx) -> None:
        if os.path.isfile(f"{config.clout}{str(idx).zfill(3)}.gz"):
            with gzip.open(f"{config.clout}{str(idx).zfill(3)}.gz", 'rb') as f_in:
                with open(f"{config.clout}{str(idx).zfill(3)}", 'wb') as f_out:
                    shutil.copyfileobj(f_in, f_out)

    def run_fortran(self) -> None:
        worker = get_current_worker()
        process = subprocess.Popen(["./sphall"], stdout=subprocess.PIPE, text=True)
        for line in process.stdout:
            self.query_one("#fortran_logs").write(line)
        process.wait()

    def run_geant(self) -> None:
        process = subprocess.Popen(["./geant4", os.getcwd()], stdout=subprocess.PIPE, text=True)
        for line in process.stdout:
            self.query_one("#geant_logs").write(line)
        process.wait()


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

    def on_mount(self, event: events.Mount) -> None:
        config.read_config()

    def action_quit(self) -> None:
        self.exit()


if __name__ == "__main__":
    app = GenerationApp()
    app.run()
