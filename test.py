import os
current_path = os.path.dirname(os.path.realpath(__file__))
cache_path = current_path + "/.cache/"
class_data = cache_path + "class_data"

print class_data

from parser import Parser

parser = Parser()


print "len before is", len(parser.collection)
parser.cache_from(class_data)
#parser.parse_directory("/Users/ivo/Projects/libs/")
#parser.parse_directory("/Users/ivo/Projects/Ginrummyplus-Client/src")
print "len after is", len(parser.collection)

print parser.look_for("Bundle")["methods"]

#parser.cache_to(class_data)
