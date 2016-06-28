import os

class Settings:
    def __init__(self):
        self.current_path = os.path.dirname(os.path.realpath(__file__))
        self.cache_path = self.current_path + "/.cache/"
        self.class_data = self.cache_path + "class_data"
        self.server_uri = self.cache_path + "server.uri"
        self.server_pid = self.cache_path + "server.pid"
        self.limit = 5
