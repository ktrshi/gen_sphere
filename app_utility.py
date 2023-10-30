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
        self.set_filenames()

    def read_config(self):
        import configparser
        self.config = configparser.ConfigParser()
        self.config.read(self.config_path)

    def update_config(self):
        self.set_parameters()
        self.set_filenames()

    def write_config(self):
        self.update_config()
        with open(self.config_path, 'w') as configfile:
            self.config.write(configfile)
    def get_parameter(self, parameter_name, default_section='DEFAULT'):
        return self.config[default_section][parameter_name]

    def set_parameter(self, parameter_name, parameter_value, default_section='DEFAULT'):
        self.config[default_section][parameter_name] = parameter_value

    def get_parameters(self):
        self.part = self.get_parameter('PRMPAR')
        self.e = self.get_parameter('E')
        self.th = self.get_parameter('THETAP')
        self.atm = self.get_parameter('ATMOD')
        self.q = self.get_parameter('QMOD')
        self.start = self.get_parameter('START')
        self.end = self.get_parameter('END')

    def set_parameters(self):
        self.set_parameter('PRMPAR', self.part)
        self.set_parameter('E', self.e)
        self.set_parameter('THETAP', self.th)
        self.set_parameter('ATMOD', self.atm)
        self.set_parameter('QMOD', self.q)
        self.set_parameter('START', self.start)
        self.set_parameter('END', self.end)

    def set_filenames(self):
        self.clout = f'CLOUT4w_Q{self.q}_atm{self.atm.zfill(2)}_{self.part.zfill(4)}_{self.e}PeV_{self.th.zfill(2)}_'
        self.dir = f'Q{self.q}_atm{self.atm.zfill(2)}_{self.part.zfill(4)}_{self.e}PeV_{self.th.zfill(2)}_'
        self.phels = f'phels_to_trace_Q{self.q}_atm{self.atm.zfill(2)}_{self.part.zfill(4)}_{self.e}PeV_{self.th.zfill(2)}_'
        self.moshits = f'moshits_Q{self.q}_atm{self.atm.zfill(2)}_{self.part.zfill(4)}_{self.e}PeV_{self.th.zfill(2)}_'