import subprocess
import os
import cmd


def three_digs(number):
    result: str = '0'
    if number // 100 == 0:
        result = '0'
        if number // 10 == 0:
            result += '0' + str(number)
        else:
            result += str(number)
    else:
        result += str(number)
    return result


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


class ExternalProcessManager:
    def __init__(self):
        pass

    def run_process(self, command, *args):
        import subprocess
        process = subprocess.Popen([command, *args], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = process.communicate()
        return stdout.decode('utf-8'), stderr.decode('utf-8')

    def run_example_process(self, file_name):
        command = "echo"
        return self.run_process(command, file_name)


class FileSystemManager:
    def __init__(self, config: Configuration):
        self.config = config


class ConsoleApp(cmd.Cmd):
    intro = "Welcome to the Console App. Type 'help' or '?' to list commands."
    prompt = "(app) "

    def __init__(self):
        super().__init__()
        self.config_reader = Configuration()
        self.fs_manager = FileSystemManager(self.config_reader)
        self.proc_manager = ExternalProcessManager()

    # Command to get a parameter from the config
    def do_get_config(self):
        """Get a parameter value from the configuration."""
        try:
            print(self.config_reader.get_parameters)
        except KeyError:
            print(KeyError)

    # Command to run a sample external process
    def do_run_process(self, file_name):
        """Run a sample external process with the given file name."""
        stdout, stderr = self.proc_manager.run_example_process(file_name)
        print(stdout)

    # Command to exit the app
    def do_exit(self, arg):
        """Exit the application."""
        return True

if __name__ == '__main__':
    ConsoleApp().cmdloop()
# config = Configuration()
# lastCount = 1
# currentPath = os.getcwd()

# for i in range(int(config.start), int(config.end)):
#     firstCount = three_digs(i)
#     f = open('Names', 'w')
#     f.write(config.phels + firstCount + '_c\n' +
#             config.moshits + firstCount + '_c')
#     f.close()
#     if not os.path.isfile(f'{config.clout}{firstCount}'):
#         if os.path.isfile(f'CLOUT/{config.clout}{firstCount}.gz'):
#             os.system(f'gunzip CLOUT/{config.clout}{firstCount}.gz')
#             os.system(f'mv CLOUT/{config.clout}{firstCount} .')
#         else:
#             print(f'cant find {config.clout}{firstCount}')
#             continue
#     for j in range(0, 101):
#         if os.path.isfile(config.phels + three_digs(j)):
#             lastCount = j
#     os.system(f'mkdir {config.dir}{firstCount}')
#     f = open('sphall_g4_sh_wbg.in', 'w')
#     f.write('  1                         - catm = CorAtmMod\n' +
#             config.clout + firstCount + '    - fn_in\n'
#                                           '-1000.00                    - zz, m\n'
#                                           ' 2                          - key: 0-signal only, 1-BG only, >=2-both\n'
#                                           ' ' + str(lastCount) + '                          - last_c')
#     f.close()
#     if lastCount < 101:
#         os.system('./sphall')
#     # for k in range(1, 100):
#     #    secondCount = three_digs(k)
#     #    phels_name = startPhelsName + firstCount + '_c' + secondCount
#     os.system(f'/home/common/SPHERE-3_geant4/build/SPHERE-3 {currentPath}')
#     for k in range(0, 101):
#         secondCount = three_digs(k)
#         phels_name = f'{config.phels}{firstCount}_c{secondCount}'
#         moshits_name = f'{config.moshits}{firstCount}_c{secondCount}'
#         os.system(f'rm {phels_name}')
#         os.system(f'gzip {moshits_name}')
#         os.system(f'mv {moshits_name}.gz {config.dir}{firstCount}')
