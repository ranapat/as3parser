import Pyro.core
import parser
import settings
import analyzer

class Wrapper(Pyro.core.ObjBase):
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)

        self.project = ""

        self.settings = settings.Settings()
        self.parser = parser.Parser()
        self.analyzer = analyzer.Analyzer(self.parser)

    def load_from_cache(self):
        self.parser.cache_from(self.settings.class_data + ("." + self.project if self.project != "" else ""))

    def save_to_cache(self):
        self.parser.cache_to(self.settings.class_data + ("." + self.project if self.project != "" else ""))

    def reset_cache(self):
        self.parser.collection = []

    def reset_class(self, name):
        self.parser.remove_from_cache(name)

    def load_from_content(self, content):
        self.parser.parse_content(content)
        
    def load_from_file(self, name):
        self.parser.parse_file(name)

    def load_from_directory(self, name):
        self.parser.parse_directory(name)

    def set_project(self, name):
        self.project = name
        
        self.reset_cache()
        self.load_from_cache()
        
    def complete(self, line, content):
        return self.analyzer.guess(line, content, False)

    def remind(self, line, content):
        return self.analyzer.guess(line, content, True)
