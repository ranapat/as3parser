#!/usr/local/bin/python

import os
import os.path
import sys
import signal
import wrapper
import settings
import Pyro.core

class Server:
    def __init__(self):
        _settings = settings.Settings()
        
        self.uri_file = _settings.server_uri
        self.pid_file = _settings.server_pid
        self.try_to_clean = False

    def main(self):
        if os.path.isfile(self.uri_file):
            if len(sys.argv) == 2 and sys.argv[1] == "--force":
                print "Server already running? Forcing new instance! Check for garbage!"
                current_pid = open(self.pid_file, "r").read()
                print "Killing pid is", current_pid
                try:
                    os.kill(int(current_pid), signal.SIGTERM)
                    os.remove(self.uri_file)
                    os.remove(self.pid_file)
                except:
                    pass
            else:
                print "Server already running?"
                return 1;

        Pyro.core.initServer()

        daemon = Pyro.core.Daemon()
        uri = daemon.connect(wrapper.Wrapper(), "wrapper")

        print "The daemon runs on port:", daemon.port
        print "The object's uri is:", uri
        print "URI stored to file:", self.uri_file

        uri_file = open(self.uri_file, "w")
        uri_file.write(str(uri))
        uri_file.close()

        pid_file = open(self.pid_file, "w")
        pid_file.write(str(os.getpid()))
        pid_file.close()
        
        self.try_to_clean = True

        daemon.requestLoop()

        return 0

    def cleanup(self):
        if self.try_to_clean is True:
            if os.path.isfile(self.uri_file):
                os.remove(self.uri_file)
            if os.path.isfile(self.pid_file):
                os.remove(self.pid_file)

if __name__ == "__main__":
    server = Server()
    
    try:
        sys.exit(server.main())
    except:
        pass
    finally:
        server.cleanup()

