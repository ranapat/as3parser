#!/usr/local/bin/python

import os
import os.path
import sys
import wrapper
import settings
import Pyro.core

class Server:
    def __init__(self):
        self.uri_file = settings.Settings().server_uri
        self.try_to_clean = False

    def main(self):
        if os.path.isfile(self.uri_file):
            print "Server already running?"
            return 1;

        Pyro.core.initServer()

        daemon = Pyro.core.Daemon()
        uri = daemon.connect(wrapper.Wrapper(), "wrapper")

        print "The daemon runs on port:", daemon.port
        print "The object's uri is:", uri
        print "URI stored to file:", self.uri_file

        text_file = open(self.uri_file, "w")
        text_file.write(str(uri))
        text_file.close()
        self.try_to_clean = True

        daemon.requestLoop()

        return 0

    def cleanup(self):
        if self.try_to_clean is True and os.path.isfile(self.uri_file):
            os.remove(self.uri_file)

if __name__ == "__main__":
    server = Server()
    
    try:
        sys.exit(server.main())
    except:
        pass
    finally:
        server.cleanup()

