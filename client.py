import os
import sys
import settings
import Pyro.core

def print_help_line(command, description):
    print 4 * " " + "/**" + " " + description + " " "*/"
    print 1 * " " + " - " + command

def main():
    file = open(settings.Settings().server_uri, "r")

    wrapper = Pyro.core.getProxyForURI(file.read())

    if len(sys.argv) == 1:
        print "Usage:"
        print_help_line("load-from-cache", "Loads class definition from the cache")
        print_help_line("save-to-cache", "Saves class definition to the cache")
        print_help_line("reset-cache", "Resets the session cache")
        print_help_line("reset-class <name>", "Removes class definition from the cache")
        print_help_line("load-from-file <file>", "Loads class definition from a source file")
        print_help_line("load-from-directory <directory>", "Loads class definition form a source directory")
        print_help_line("load-from-content <content>", "Loads class definition from a content")
        print_help_line("<line> <content>", "Tries to autocomplete")

        return 1

    if len(sys.argv) == 2:
        first_parameter = sys.argv[1]
        second_parameter = None
    elif len(sys.argv) == 3:
        first_parameter = sys.argv[1]
        second_parameter = sys.argv[2]

    if first_parameter == "load-from-cache":
        wrapper.load_from_cache()
    elif first_parameter == "save-to-cache":
        wrapper.save_to_cache()
    elif first_parameter == "reset-cache":
        wrapper.reset_cache()
    elif first_parameter == "reset-class" and second_parameter is not None:
        wrapper.reset_class(second_parameter)
    elif first_parameter == "load-from-file" and second_parameter is not None:
        wrapper.load_from_file(second_parameter)
    elif first_parameter == "load-from-directory" and second_parameter is not None:
        wrapper.load_from_directory(second_parameter)
    elif first_parameter == "load-from-content" and second_parameter is not None:
        wrapper.load_from_content(second_parameter)
    else:
        print wrapper.complete(first_parameter, second_parameter)

    return 0

if __name__ == "__main__":
    try:
        sys.exit(main())
    except:
        pass
