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
#parser.parse_directory("./tests")
print "len after is", len(parser.collection)

#print 30 * "#"
#print parser.look_for("Bundle")["methods"]
#print 30 * "#"
#print parser.complete_class("Bundle", True)
#print 30 * "#"
print [ ("+" if data["visibility"] == "public" else "-") + data["name"] + ("(" + data["parameters"] + ")" if data["node"] == "method" else "<get>" if data["node"] == "getter" else "<set>" if data["node"] == "setter" else "") + ":" + data["type"] for data in parser.complete_member("control", "ControlLobbyView", False)]

#parser.cache_to(class_data)
