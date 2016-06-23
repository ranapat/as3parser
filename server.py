import os
import sys
import wrapper
import settings
import Pyro.core

uri_file = settings.Settings().server_uri

def main():
    Pyro.core.initServer()

    daemon = Pyro.core.Daemon()
    uri = daemon.connect(wrapper.Wrapper(), "wrapper")

    print "The daemon runs on port:", daemon.port
    print "The object's uri is:", uri
    print "URI stored to file:", uri_file

    text_file = open(uri_file, "w")
    text_file.write(str(uri))
    text_file.close()

    daemon.requestLoop()

    return 0

def cleanup():
    pass
    os.remove(uri_file)

if __name__ == "__main__":
    try:
        sys.exit(main())
    except:
        pass
    finally:
        cleanup()

